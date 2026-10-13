"""
Fit the optical transfer function (a.k.a. the XCAL emissivity) together with the
emissivities of the other emitters to the calibration data.

Model, for one frequency bin nu and one interferogram j:

    Y_j(nu) / (ETF * Bol)  =  sum_i E_i(nu) * P(nu, T_i^j)

with i = 0 the XCAL (E_0 = OTF), i = 1..5 the ICAL, dihedral, refhorn, skyhorn and
collimator/structure, and i = 6..9 the four bolometer assemblies (RH, RL, LH, LL),
each at its own measured temperature.  This is the same model as

    S_XCAL = 1/OTF * 1/ETF * 1/Bol * Y - R,    R = 1/OTF * sum_{i>0} E_i * P(T_i)

multiplied through by the OTF, which is how it used to be written here.

The right-hand side is *linear* in the emissivities, so every frequency bin is an
ordinary complex linear least-squares problem and is solved directly with
np.linalg.lstsq.  There is no iterative optimiser and no numerical gradient: the
measured spectrum on the left-hand side does not depend on any fitted parameter, so
the interferograms are Fourier transformed exactly once.

Every bin from the low-frequency cutoff up to Nyquist is fitted, not only the 43 bins
where the published model is defined; `report()` prints where the model stops
explaining the data so the usable range can be read off.  (Bins below the cutoff --
0-4 for the short-slow modes -- are zeroed by the pipeline in `ifg_to_spec`, since the
DC block leaves no signal there.)

All nine physical bolometer parameters (R0, T0, G1, beta, rho, C1, C3, Jo, Jg) are
fitted jointly, once per channel and shared by its modes.  At every trial response the
emissivities are still solved exactly by linear least squares (variable projection), so
the non-linear optimiser only sees the nine detector parameters.

Output: `fitted_emissivities_{channel}_{mode}.npy`, shape (SPEC_SIZE, 10), complex, one
file per mode.  Column i is E_i in the ordering used by `R()` below, and
`fit_diagnostics_{channel}_{mode}.npz` carries the fitted and published bolometer
parameter vectors they go with.
"""

import argparse
import os
import time

import matplotlib.pyplot as plt
import numpy as np
from astropy.io import fits
from scipy.optimize import minimize, minimize_scalar

import globals as g
from calibration import bolometer
from pipeline import ifg_spec
from utils import my_utils as utils
from utils.config import gen_nyquistl

# The four bolometer assemblies, in the order R() documents for indices 6-9.  Each one
# has its own temperature, measured in its own channel.
# BOLOMETER_CHANNELS = ["rh", "rl", "lh", "ll"]
BOLOMETER_CHANNELS = ["ll"]  # only fit the LL bolometer for now TODO: PUT THIS BACK
# assert [g.CHANNELS[c] for c in BOLOMETER_CHANNELS] == [0, 1, 2, 3]


# Rows of `temps`, and the columns of the saved solution.
EMITTERS = [
    "xcal (OTF)",
    "ical",
    "dihedral",
    "refhorn",
    "skyhorn",
    "collimator",
] + [f"bolometer {c}" for c in BOLOMETER_CHANNELS]
N_EMISSIVITIES = len(EMITTERS)

# Where the four bolometer assemblies start in EMITTERS.
FIRST_BOLOMETER = EMITTERS.index("bolometer ll") # TODO: PUT THIS BACK TO RH

# The published emissivity columns for emitters 0-5 (the XCAL one is TRANSFE).  The
# published model has a single bolometer column, BOLOMET, which is compared against
# the fitted channel's own bolometer.
FITS_COLUMNS = ["TRANSFE", "ICAL", "DIHEDRA", "REFHORN", "SKYHORN", "STRUCTU"]

# bol_cmd_bias is stored as raw counts; the rest of the pipeline divides by this to
# get volts before handing it to the bolometer model (see fsl.py, cal_ifgs.py).
BOL_CMD_BIAS_TO_VOLTS = 25.5
BOLOMETER_PARAMETER_NAMES = ("R0", "T0", "G1", "beta", "rho", "C1", "C3", "Jo", "Jg")


def get_memory_usage():
    """Get current memory usage in GB."""
    try:
        import psutil
        process = psutil.Process(os.getpid())
        return process.memory_info().rss / 1e9
    except ImportError:
        return None


def bolometer_row(temps, channel):
    """
    Row of `temps` holding the temperature of this channel's own bolometer.

    The bolometer response function needs the bolometer that is actually reading this
    channel out, not the other three.  Callers that pass the older seven-row array,
    whose single bolometer row is already the right one, get that row back.
    """
    row = FIRST_BOLOMETER + g.CHANNELS[channel]
    return min(row, len(temps) - 1)


def D(Ei, nui, ifg, channel, mode, gain, sweeps, bol_cmd_bias, bol_volt, temps, adds_per_group,
      fnyq_icm, apod=False):
    """Measured spectrum divided by the OTF, i.e. 1/OTF * 1/ETF * 1/Bol * Y."""
    spec = ifg_spec.ifg_to_spec(ifg, channel, mode, adds_per_group, bol_cmd_bias, bol_volt,
                                fnyq_icm, otf=Ei[0],
                                Tbol=temps[bolometer_row(temps, channel)],
                                apod=apod, gain=gain, sweeps=sweeps, nui=nui)

    return spec


def R(Ei, nui, temps, frequencies):
    """
    R is the function that weights each of the emitters.

    Parameters
    ----------
    Ei : array_like
        The ten emissivities at this frequency.  Ei[0] is the optical transfer
        function a.k.a. the emissivity of the XCAL; the rest are
        1: ICAL, 2: dihedral, 3: refhorn, 4: skyhorn, 5: collimator, 6: bolometer_rh,
        7: bolometer_rl, 8: bolometer_lh, 9: bolometer_ll
    """
    total = np.zeros_like(temps[0], dtype=complex)
    for i in range(1, Ei.shape[0]):
        # `temps` normally carries one row per emitter, including a separate temperature
        # for each of the four bolometers.  Callers that pass the older seven-row array
        # fall back to its single bolometer temperature for all four.
        temp_idx = min(i, len(temps) - 1)
        total += Ei[i] * utils.planck(frequencies[nui], temps[temp_idx])

    H = Ei[0]

    return total / H


def S(nui, frequencies, temps):
    return utils.planck(frequencies[nui], temps[0])


def full_function(Ei, nui, ifg, channel, mode, gain, sweeps, bol_cmd_bias, bol_volt, temps,
                  adds_per_group, fnyq_icm, frequencies):
    """Sum of squared residuals of the model, for diagnostics.

    The residual is D - R - S (the model is S = D - R); it is multiplied by the OTF so
    that it does not blow up where the OTF is small.
    """
    Di = D(Ei, nui, ifg, channel, mode, gain, sweeps, bol_cmd_bias, bol_volt, temps,
           adds_per_group, fnyq_icm)
    Ri = R(Ei, nui, temps, frequencies)
    Si = S(nui, frequencies, temps)

    return np.sum(np.abs(Ei[0] * (Di - Ri - Si)) ** 2)


def low_frequency_cutoff(mode):
    """First bin `ifg_to_spec` keeps by default; below it the DC block cuts the signal."""
    return 5 if mode[1] == "s" else 7


def channel_modes(channels, modes):
    """
    The channel/mode combinations that can be fitted, out of those asked for.

    The high-frequency channels have no long-slow mode, and `generate_frequencies` and
    the published models only cover ss and lf.
    """
    for channel in channels:
        for mode in modes:
            if mode == "lf" and channel[1] == "h":
                print(f"skipping {channel}_{mode}: the high channels have no long-slow mode")
                continue
            if mode not in g.MODES:
                print(f"skipping {channel}_{mode}: mode not in g.MODES")
                continue
            model = f"{g.PUB_MODEL}FIRAS_CALIBRATION_MODEL_{channel.upper()}{mode.upper()}.FITS"
            if not os.path.exists(model):
                print(f"skipping {channel}_{mode}: no published model at {model}")
                continue
            yield channel, mode


def published_emissivities(channel, mode):
    """
    Read the published calibration model.

    Returns
    -------
    published : (SPEC_SIZE, N_EMISSIVITIES) complex array
        The published emissivities in EMITTERS order, zero outside the published band.
        Its single bolometer column is placed in this channel's own bolometer slot.
    band : slice
        The frequency bins where the published model is non-zero.
    """
    cutoff = low_frequency_cutoff(mode)

    with fits.open(
        f"{g.PUB_MODEL}FIRAS_CALIBRATION_MODEL_{channel.upper()}{mode.upper()}.FITS"
    ) as fits_data:
        record = fits_data[1].data
        otf = record["RTRANSFE"][0] + 1j * record["ITRANSFE"][0]
        n_band = int(np.count_nonzero(otf))

        published = np.zeros((g.SPEC_SIZE, N_EMISSIVITIES), dtype=complex)
        columns = FITS_COLUMNS + ["BOLOMET"]
        rows = list(range(len(FITS_COLUMNS))) + [FIRST_BOLOMETER +
                                                 BOLOMETER_CHANNELS.index(channel)]
        for row, name in zip(rows, columns):
            column = record[f"R{name}"][0] + 1j * record[f"I{name}"][0]
            published[cutoff:cutoff + n_band, row] = column[:n_band]

    return published, slice(cutoff, cutoff + n_band)


def raw_spectra(ifg, channel, mode, adds_per_group, bol_cmd_bias, bol_volt, Tbol, fnyq_icm,
                gain, sweeps):
    """
    The left-hand side of the model: Y / (ETF * Bol), in MJy/sr.

    This is `ifg_to_spec` with the division by the OTF disabled, i.e. everything that
    does *not* depend on the fitted emissivities.  It is computed once for all
    frequencies instead of once per objective evaluation.  The unit OTF spans the whole
    spectrum and `cutoff=0` turns off the zeroing of the low bins, so every frequency
    reaches the fit; whether the low bins carry anything is then a result rather than
    an assumption.
    """
    return ifg_spec.ifg_to_spec(ifg, channel, mode, adds_per_group, bol_cmd_bias, bol_volt,
                                fnyq_icm, otf=np.ones(g.SPEC_SIZE), Tbol=Tbol, apod=False,
                                gain=gain, sweeps=sweeps, cutoff=0)


def audio_frequencies(channel, mode):
    """The audio frequency of each spectral bin, as the bolometer response function uses it."""
    return utils.get_afreq(0 if mode[1] == "s" else 1, channel, g.SPEC_SIZE)


def design_matrix(temps, frequency):
    """
    Planck function of every emitter temperature at one frequency.

    Parameters
    ----------
    temps : (7, nifg) array
        Emitter temperatures in EMITTERS order.
    frequency : float
        Frequency in GHz.

    Returns
    -------
    (nifg, 7) complex array, one column per emissivity to fit.
    """
    return np.array([utils.planck(frequency, temp) for temp in temps]).T.astype(complex)


def noise_weights(spec, published_band):
    """
    Relative inverse-noise weight per interferogram, from the out-of-band bins.

    The bins above the published band carry no signal, so their scatter measures the
    noise of each interferogram.  The electronics transfer function suppresses them, so
    this is only meaningful as a *relative* weight between interferograms, which is all
    the least-squares solution needs.
    """
    first_noise_bin = published_band.stop + 5
    if first_noise_bin >= g.SPEC_SIZE - 10:
        print("  not enough out-of-band bins to estimate noise; using uniform weights")
        return np.ones(spec.shape[0])

    sigma = np.std(spec[:, first_noise_bin:], axis=1)
    good = np.isfinite(sigma) & (sigma > 0)
    weights = np.zeros(spec.shape[0])
    weights[good] = 1.0 / sigma[good]
    # normalise so the chi^2 stays on a readable scale
    weights /= np.median(weights[good])
    return weights


def solve_band(spec, temps, frequencies, band, tau, omega, weights=None, rcond=None,
               sum_zero=True):
    """
    Solve for the emissivities as an exact linear function of the time constant scale.

    With `sum_zero` the emissivities are constrained to sum to zero, by fitting
    E_i for i > 0 against the temperature-differenced columns P(T_i) - P(T_xcal) and
    setting E_0 = -sum_{i>0} E_i.  A differential instrument sees nothing when every
    emitter sits at the same temperature, so this has to hold; the published model
    satisfies it to 3e-9.  Without it the fit runs away along that near-null direction
    at high frequency, where the Planck columns become nearly proportional.

    The published time constant is scaled by a factor `alpha`, fitted jointly with the
    emissivities.  `spec` arrives already divided by the published response
    S0 / (1 + i w tau), so undoing that factor gives

        U = spec / (1 + i w tau),      model:  A @ E  =  U + alpha * (i w tau U)

    which is linear in alpha as well as in E, and alpha does not multiply E.  So per
    bin one least-squares solve with the two right-hand sides U and i w tau U gives the
    exact dependence, E(alpha) = E0 + alpha E1 and residual r(alpha) = r0 + alpha r1.
    This function stops there and returns those two pieces, so that the alpha
    minimising the residual can be chosen over more than one mode at a time; see
    `best_tau_scale` and `apply_tau_scale`.

    Returns
    -------
    fit : dict
        `base` and `slope` are E0 and E1 above, `quad` and `quad_null` are the
        coefficients of |r(alpha)|^2 as a quadratic in alpha, and `fitted` marks the
        bins that were solved at all.
    """
    n_free = N_EMISSIVITIES - 1 if sum_zero else N_EMISSIVITIES
    base = np.zeros((g.SPEC_SIZE, n_free), dtype=complex)
    slope = np.zeros((g.SPEC_SIZE, n_free), dtype=complex)
    fitted = np.zeros(g.SPEC_SIZE, dtype=bool)
    # sum |r0 + a r1|^2 = quad[0] + 2a quad[1] + a^2 quad[2], and likewise for the data
    quad = np.zeros((g.SPEC_SIZE, 3))
    quad_null = np.zeros((g.SPEC_SIZE, 3))
    diagnostics = {key: np.full(g.SPEC_SIZE, np.nan)
                   for key in ("cond", "rank", "chi2", "chi2_null", "rms_resid", "rms_data",
                               "alpha")}

    for nui in range(band.start, band.stop):
        A = design_matrix(temps, frequencies[nui])
        if sum_zero:
            A = A[:, 1:] - A[:, [0]]

        y = spec[:, nui] / (1 + 1j * omega[nui] * tau)
        x = 1j * omega[nui] * tau * y

        if weights is not None:
            A = A * weights[:, np.newaxis]
            y, x = y * weights, x * weights

        # High up the band the Planck columns are ~1e-40 and the data is pure noise;
        # the solve is still well defined, but guard against a column underflowing away
        # entirely.
        if not (np.isfinite(A).all() and np.isfinite(y).all() and np.isfinite(x).all()
                and np.any(A)):
            continue

        coefficients, _, rank, _ = np.linalg.lstsq(A, np.stack([y, x], axis=1), rcond=rcond)
        base[nui], slope[nui] = coefficients[:, 0], coefficients[:, 1]
        r0 = A @ base[nui] - y
        r1 = A @ slope[nui] - x

        fitted[nui] = True
        quad[nui] = [np.sum(np.abs(r0) ** 2), np.real(np.sum(r0 * np.conj(r1))),
                     np.sum(np.abs(r1) ** 2)]
        quad_null[nui] = [np.sum(np.abs(y) ** 2), np.real(np.sum(y * np.conj(x))),
                          np.sum(np.abs(x) ** 2)]
        try:
            diagnostics["cond"][nui] = np.linalg.cond(A)
        except np.linalg.LinAlgError:
            diagnostics["cond"][nui] = np.inf
        diagnostics["rank"][nui] = rank
        # the scale this bin alone would choose, to show whether one global tau fits
        if quad[nui, 2] > 0:
            diagnostics["alpha"][nui] = -quad[nui, 1] / quad[nui, 2]

    return {"base": base, "slope": slope, "fitted": fitted, "quad": quad,
            "quad_null": quad_null, "diagnostics": diagnostics, "sum_zero": sum_zero,
            "n_points": spec.shape[0]}


def best_tau_scale(fits):
    """
    The single time constant scale that minimises the residual over all of `fits`.

    Each fit contributes sum |r0 + alpha r1|^2 = q0 + 2 alpha q1 + alpha^2 q2 summed
    over its fitted bins, so the minimum over any collection of them is at
    -sum q1 / sum q2.  Passing every mode of one channel gives that channel a single
    tau: the four bolometers are physical detectors, one per channel, and their thermal
    time constant cannot depend on how the mirror happens to be scanning.  Only tau is
    shared this way -- each mode keeps its own emissivities, which `apply_tau_scale`
    then evaluates at the common alpha.  alpha = 1 is the published time constant.
    """
    total = sum(fit["quad"][fit["fitted"]].sum(axis=0) for fit in fits)
    return float(-total[1] / total[2]) if total[2] > 0 else 1.0


def apply_tau_scale(fit, alpha):
    """
    Evaluate the emissivities and the chi^2 at a given time constant scale.

    Returns
    -------
    solution : (SPEC_SIZE, N_EMISSIVITIES) complex array
    diagnostics : dict of per-bin arrays, indexed by bin number
    """
    fitted, quad, quad_null = fit["fitted"], fit["quad"], fit["quad_null"]
    diagnostics = dict(fit["diagnostics"])

    powers = np.array([1.0, 2 * alpha, alpha ** 2])
    solution = np.zeros((g.SPEC_SIZE, N_EMISSIVITIES), dtype=complex)
    coefficients = fit["base"] + alpha * fit["slope"]
    if fit["sum_zero"]:
        solution[:, 1:] = coefficients
        solution[:, 0] = -coefficients.sum(axis=1)
    else:
        solution[:] = coefficients
    solution[~fitted] = 0

    n_points = fit["n_points"]
    diagnostics["chi2"][fitted] = quad[fitted] @ powers
    diagnostics["chi2_null"][fitted] = quad_null[fitted] @ powers
    diagnostics["rms_resid"][fitted] = np.sqrt(diagnostics["chi2"][fitted] / n_points)
    diagnostics["rms_data"][fitted] = np.sqrt(diagnostics["chi2_null"][fitted] / n_points)

    return solution, diagnostics


def bolometer_response(channel, mode, fields, Tbol, parameters):
    """Response on the complete spectral grid for explicit physical parameters."""
    return bolometer.get_bolometer_response_function(
        channel, mode, fields["bol_cmd_bias"], fields["bol_volt"], Tbol,
        parameters=parameters,
    )


def solve_with_bolometer_parameters(spec, temps, frequencies, band, channel, mode, fields,
                                    published_parameters, parameters, weights=None, rcond=None,
                                    sum_zero=True, keep_solution=True):
    """Profile out emissivities for a trial set of physical bolometer parameters.

    ``spec`` is divided by the published response by :func:`raw_spectra`.  Multiplying
    it by ``B_published / B_trial`` gives the spectrum that would have resulted had it
    been divided by the trial response instead.  The remaining fit is linear.
    """
    Tbol = temps[bolometer_row(temps, channel)]
    try:
        B_published = bolometer_response(channel, mode, fields, Tbol, published_parameters)
        B_trial = bolometer_response(channel, mode, fields, Tbol, parameters)
        with np.errstate(divide="ignore", invalid="ignore", over="ignore"):
            corrected = spec * B_published / B_trial
    except (FloatingPointError, ValueError, np.linalg.LinAlgError):
        return None

    solution = np.zeros((g.SPEC_SIZE, N_EMISSIVITIES), dtype=complex)
    diagnostics = {key: np.full(g.SPEC_SIZE, np.nan)
                   for key in ("cond", "rank", "chi2", "chi2_null", "rms_resid", "rms_data")}
    chi2 = 0.0
    for nui in range(band.start, band.stop):
        A = design_matrix(temps, frequencies[nui])
        if sum_zero:
            A = A[:, 1:] - A[:, [0]]
        y = corrected[:, nui]
        if weights is not None:
            A, y = A * weights[:, np.newaxis], y * weights
        if not (np.isfinite(A).all() and np.isfinite(y).all() and np.any(A)):
            return None
        coefficients, _, rank, _ = np.linalg.lstsq(A, y, rcond=rcond)
        residual = A @ coefficients - y
        value = float(np.sum(np.abs(residual) ** 2))
        null_value = float(np.sum(np.abs(y) ** 2))
        if not np.isfinite(value):
            return None
        chi2 += value
        diagnostics["chi2"][nui] = value
        diagnostics["chi2_null"][nui] = null_value
        diagnostics["rms_resid"][nui] = np.sqrt(value / y.size)
        diagnostics["rms_data"][nui] = np.sqrt(null_value / y.size)
        diagnostics["rank"][nui] = rank
        try:
            diagnostics["cond"][nui] = np.linalg.cond(A)
        except np.linalg.LinAlgError:
            diagnostics["cond"][nui] = np.inf
        if keep_solution:
            if sum_zero:
                solution[nui, 1:] = coefficients
                solution[nui, 0] = -coefficients.sum()
            else:
                solution[nui] = coefficients
    return chi2, solution, diagnostics


def fit_bolometer_parameters(entries, bound_factor, maxiter):
    """Fit one shared nine-parameter detector model using variable projection."""
    reference = np.asarray(entries[0]["published_parameters"], dtype=float)
    if np.any(~np.isfinite(reference)) or np.any(reference == 0):
        raise ValueError("published bolometer parameters must be finite and non-zero")
    log_bound = np.log(bound_factor)
    evaluations = 0

    def objective(log_ratio):
        nonlocal evaluations
        evaluations += 1
        parameters = reference * np.exp(log_ratio)
        total = 0.0
        for entry in entries:
            result = solve_with_bolometer_parameters(
                entry["spec"], entry["temps"], entry["frequencies"], entry["band"],
                entry["channel"], entry["mode"], entry["fields"],
                entry["published_parameters"], parameters, entry["weights"], entry["rcond"],
                entry["sum_zero"], keep_solution=False,
            )
            if result is None:
                return np.inf
            total += result[0]
        return total

    result = minimize(objective, np.zeros(reference.size), method="L-BFGS-B",
                      bounds=[(-log_bound, log_bound)] * reference.size,
                      options={"maxiter": maxiter, "ftol": 1e-10})
    parameters = reference * np.exp(result.x)
    return parameters, result, evaluations

def fit_Jo(entries, bound_factor, maxiter):
    """Fit only Jo while keeping the other bolometer parameters published."""
    reference = np.asarray(entries[0]["published_parameters"], dtype=float)
    jo_index = BOLOMETER_PARAMETER_NAMES.index("Jo")
    if not np.isfinite(reference[jo_index]) or reference[jo_index] == 0:
        raise ValueError("published Jo must be finite and non-zero")

    log_bound = np.log(bound_factor)
    evaluations = 0

    def objective(log_ratio):
        nonlocal evaluations
        evaluations += 1
        parameters = reference.copy()
        parameters[jo_index] *= np.exp(log_ratio)
        total = 0.0
        for entry in entries:
            result = solve_with_bolometer_parameters(
                entry["spec"], entry["temps"], entry["frequencies"], entry["band"],
                entry["channel"], entry["mode"], entry["fields"],
                entry["published_parameters"], parameters, entry["weights"], entry["rcond"],
                entry["sum_zero"], keep_solution=False,
            )
            if result is None:
                return np.inf
            total += result[0]
        return total

    result = minimize_scalar(objective, bounds=(-log_bound, log_bound), method="bounded",
                             options={"maxiter": maxiter, "xatol": 1e-10})
    parameters = reference.copy()
    parameters[jo_index] *= np.exp(result.x)
    return parameters, result, evaluations


def plot_solution(solution, published, band, published_band, frequencies, channel, mode, out_dir):
    """
    Fitted emissivities against the published ones, one panel per emitter.

    The left column zooms on the published band, where the emissivities are physical.
    The right column is |E| over everything that was fitted, on a log scale, which is
    where to look for whether anything survives above the published band.
    """
    bins = np.arange(band.start, band.stop)
    pub_bins = np.arange(published_band.start, published_band.stop)
    fig, axes = plt.subplots(len(EMITTERS), 2, figsize=(12, 1.7 * len(EMITTERS)), sharex="col")

    for i, name in enumerate(EMITTERS):
        zoom, full = axes[i]

        if np.any(published[pub_bins, i]):
            zoom.plot(frequencies[pub_bins], published[pub_bins, i].real, color="k",
                      label="published (real)")
            zoom.plot(frequencies[pub_bins], published[pub_bins, i].imag, color="k",
                      linestyle="dashed", label="published (imag)")
            full.plot(frequencies[pub_bins], np.abs(published[pub_bins, i]), color="k",
                      label="|published|")
        zoom.plot(frequencies[pub_bins], solution[pub_bins, i].real, color="crimson",
                  label="fitted (real)")
        zoom.plot(frequencies[pub_bins], solution[pub_bins, i].imag, color="crimson",
                  linestyle="dashed", label="fitted (imag)")
        full.plot(frequencies[bins], np.abs(solution[bins, i]), color="crimson", label="|fitted|")

        scale = np.abs(solution[pub_bins, i]).max()
        if np.isfinite(scale) and scale > 0:
            zoom.set_ylim(-1.5 * scale, 1.5 * scale)
        zoom.axhline(0, color="grey", linewidth=0.5)
        zoom.set_ylabel(name, fontsize="small")

        full.set_yscale("log")
        full.axvline(frequencies[published_band.stop - 1], color="steelblue", linewidth=0.8)

    axes[0, 0].legend(fontsize="x-small", ncol=2)
    axes[0, 1].legend(fontsize="x-small")
    axes[0, 0].set_title("published band", fontsize="small")
    axes[0, 1].set_title("everything fitted, |E| (blue line: top of the published band)",
                         fontsize="small")
    for ax in axes[-1]:
        ax.set_xlabel("frequency [GHz]")
    fig.suptitle(f"{channel}_{mode} emissivities")
    fig.tight_layout()
    path = f"{out_dir}/fitted_vs_published_{channel}_{mode}.png"
    fig.savefig(path, dpi=120)
    plt.close(fig)
    print(f"  wrote {path}")


def load_channel_mode(data, channel, mode, max_ifgs=None):
    """Select the calibration records for one channel and mode, dropping bad ones."""
    length_filter = data[f"mtm_length_{channel}"] == (0 if mode[0] == "s" else 1)
    speed_filter = data[f"mtm_speed_{channel}"] == (0 if mode[1] == "s" else 1)
    mode_filter = length_filter & speed_filter

    fields = {}
    for name in ("ifg", "adds_per_group", "sweeps", "bol_cmd_bias", "bol_volt", "gain"):
        fields[name] = data[f"{name}_{channel}"][mode_filter]

    temps = np.vstack([
        [data[f"{name}_{channel}"][mode_filter] for name in
         ("xcal_cone", "ical", "dihedral", "refhorn", "skyhorn", "collimator")],
        data[f"bolometer_{channel}"][mode_filter][np.newaxis, :],
    ])

    # Every emitter temperature has to be finite: the fit sums over all interferograms,
    # so a single NaN makes the whole normal equation NaN.
    finite = np.isfinite(temps).all(axis=0)
    housekeeping = (np.isfinite(fields["bol_volt"]) & np.isfinite(fields["bol_cmd_bias"])
                    & (fields["gain"] > 0) & (fields["sweeps"] > 0))
    good = finite & housekeeping
    print(f"  {good.sum()} of {good.size} interferograms usable "
          f"({(~good).sum()} dropped for NaN temperatures or bad housekeeping)")

    if max_ifgs is not None and good.sum() > max_ifgs:
        keep = np.flatnonzero(good)[:max_ifgs]
        good = np.zeros_like(good)
        good[keep] = True
        print(f"  --max-ifgs: using the first {max_ifgs} of them")

    fields = {name: value[good] for name, value in fields.items()}
    # the bolometer model wants volts, not raw counts
    fields["bol_cmd_bias"] = fields["bol_cmd_bias"] / BOL_CMD_BIAS_TO_VOLTS

    return fields, temps[:, good]


def report(solution, published, diagnostics, band, published_band, frequencies):
    """
    Print a per-bin summary of the fit and where it stops being meaningful.

    Returns the headline numbers, for the summary table across channels and modes.
    """
    bins = np.arange(band.start, band.stop)
    ratio = diagnostics["chi2"][bins] / diagnostics["chi2_null"][bins]
    largest = np.abs(solution[bins]).max(axis=1)

    # Every bin while there are few of them, otherwise every bin of the published band
    # and a sample of the rest.
    step = 1 if bins.size <= 60 else 4
    shown = [nui for nui in bins if nui < published_band.stop or (nui - band.start) % step == 0]

    print(f"\n  {'bin':>4} {'GHz':>8} {'cond(A)':>9} {'rank':>5} {'chi2/null':>10} "
          f"{'|OTF|fit':>9} {'|OTF|pub':>9} {'max|E|':>9}")
    for nui in shown:
        marker = " " if nui < published_band.stop else "*"
        print(f" {marker}{nui:>4} {frequencies[nui]:>8.1f} {diagnostics['cond'][nui]:>9.2e} "
              f"{int(diagnostics['rank'][nui]):>5} "
              f"{diagnostics['chi2'][nui] / diagnostics['chi2_null'][nui]:>10.4f} "
              f"{np.abs(solution[nui, 0]):>9.2e} {np.abs(published[nui, 0]):>9.2e} "
              f"{np.abs(solution[nui]).max():>9.2e}")
    if step > 1:
        print(f"  (* beyond the published band; every {step}th bin shown)")

    print(f"\n  chi2 / chi2(no model) over all fitted bins: median {np.median(ratio):.4f}, "
          f"worst {ratio.max():.4f}")

    pub_bins = np.arange(published_band.start, published_band.stop)
    print(f"  condition number: median {np.median(diagnostics['cond'][bins]):.2e}, "
          f"worst {diagnostics['cond'][bins].max():.2e}")

    # How far up the spectrum the fit is still saying something: the model has to
    # explain most of the variance and the emissivities have to stay physical.
    usable = (ratio < 0.5) & (largest <= 1)
    print(f"  bins with chi2/null < 0.5 and |E| <= 1: {usable.sum()} of {bins.size}")
    # The contiguous run containing the published band is what a user can actually take,
    # rather than isolated bins that pass the test by accident.
    if usable.any():
        first = int(np.argmax(usable))
        length = usable.size - first if usable[first:].all() else int(np.argmin(usable[first:]))
        low, high = bins[first], bins[first + length - 1]
        print(f"  contiguous from bin {low} ({frequencies[low]:.1f} GHz) to bin {high} "
              f"({frequencies[high]:.1f} GHz); the published model runs from bin "
              f"{published_band.start} ({frequencies[published_band.start]:.1f} GHz) to bin "
              f"{published_band.stop - 1} ({frequencies[published_band.stop - 1]:.1f} GHz)")
    else:
        low = high = band.start
        print("  no bin passes that test")

    # A differential instrument sees nothing when every emitter is at the same
    # temperature, so the emissivities should sum to ~0 (the published ones do).
    print(f"  |sum_i E_i|: fitted median {np.median(np.abs(solution[bins].sum(axis=1))):.2e}, "
          f"published {np.median(np.abs(published[bins].sum(axis=1))):.2e}")

    in_band = np.isin(bins, pub_bins)
    otf_error = (np.abs(solution[pub_bins, 0] - published[pub_bins, 0])
                 / np.abs(published[pub_bins, 0]))
    low_frequency = frequencies[pub_bins] < 250
    print(f"  over the published band, |OTF_fit - OTF_pub| / |OTF_pub|: median "
          f"{np.median(otf_error):.3f}, below 250 GHz "
          f"{np.median(otf_error[low_frequency]):.3f}")

    return {
        "usable_low_bin": int(low),
        "usable_top_bin": int(high),
        "usable_top_ghz": float(frequencies[high]),
        "published_top_bin": int(published_band.stop - 1),
        "published_top_ghz": float(frequencies[published_band.stop - 1]),
        "chi2_ratio": float(np.median(ratio[in_band])) if in_band.any() else float("nan"),
        "otf_error": float(np.median(otf_error)),
    }


def main():
    parser = argparse.ArgumentParser(description="Fit optical transfer function.")
    parser.add_argument("--channels", nargs="+", default=list(g.CHANNELS),
                        help="Channels to fit (default: all four).")
    parser.add_argument("--modes", nargs="+", default=list(g.MODES),
                        help="Modes to fit (default: all of g.MODES).")
    parser.add_argument("--max-ifgs", type=int, default=None,
                        help="Fit only the first N usable interferograms (for quick tests).")
    parser.add_argument("--weight", action="store_true",
                        help="Weight interferograms by their out-of-band noise.")
    parser.add_argument("--rcond", type=float, default=1e-4,
                        help="lstsq cutoff for singular values, relative to the largest. Several "
                             "emitter temperatures track each other closely -- the four "
                             "bolometers above all -- so some directions of the fit are "
                             "unconstrained; discarding them costs nothing in chi^2 and stops the "
                             "emissivities running away. Pass 0 to keep every direction.")
    parser.add_argument("--no-sum-zero", dest="sum_zero", action="store_false",
                        help="Do not constrain the emissivities to sum to zero.")
    parser.add_argument("--bolometer-bound-factor", type=float, default=2.0,
                        help="Constrain each fitted bolometer parameter to this multiplicative "
                             "factor of its published value (default: 2).")
    parser.add_argument("--bolometer-maxiter", type=int, default=100,
                        help="Maximum L-BFGS-B iterations for the nine-parameter fit (default: 100).")
    parser.add_argument("--published-band", action="store_true",
                        help="Fit only the bins the published model covers, instead of every bin "
                             "up to Nyquist.")
    parser.add_argument("--out-dir", default="calibration/output", help="Where to write results.")
    args = parser.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)

    data = np.load(f"{g.PREPROCESSED_DATA_PATH}cal.npz")
    print(f"Data loaded from {g.PREPROCESSED_DATA_PATH}cal.npz")

    fnyq = gen_nyquistl("../reference/fex_samprate.txt", "../reference/fex_nyquist.txt", "int")

    if args.bolometer_bound_factor <= 1:
        parser.error("--bolometer-bound-factor must be greater than one")

    # One bolometer reads out each channel, so its physical parameters are fitted once
    # per channel over all requested modes.
    by_channel = {}
    for channel, mode in channel_modes(args.channels, args.modes):
        by_channel.setdefault(channel, []).append(mode)

    summary = {}
    for channel, modes in by_channel.items():
        # Transform every mode once.  The nonlinear fit below only changes the
        # bolometer-response ratio, never the Fourier transform.
        solved = []
        for mode in modes:
            print(f"\n{'=' * 70}\n{channel}_{mode}\n{'=' * 70}")
            start = time.time()

            fields, temps = load_channel_mode(data, channel, mode, args.max_ifgs)
            if temps.shape[1] < len(EMITTERS):
                print("  not enough usable interferograms to fit; skipping")
                continue

            frequencies = utils.generate_frequencies(channel, mode, g.SPEC_SIZE)
            frec = 4 * (g.CHANNELS[channel] % 2) + g.MODES[mode]
            fnyq_icm = fnyq["icm"][frec]

            published, published_band = published_emissivities(channel, mode)
            # Bin 0 is the interferogram mean and has no Planck function to fit against
            # (P(0, T) is 0/0), so the fit starts at bin 1.
            band = published_band if args.published_band else slice(1, g.SPEC_SIZE)
            print(f"  fitting bins {band.start}-{band.stop - 1} "
                  f"({frequencies[band.start]:.1f}-{frequencies[band.stop - 1]:.1f} GHz); the "
                  f"published model covers bins {published_band.start}-{published_band.stop - 1}")
            print("  emitter temperature correlations (a pair at 1.000 cannot be separated):")
            for row, name in zip(np.corrcoef(temps), EMITTERS):
                print(f"    {name:>14}  " + " ".join(f"{value:6.3f}" for value in row))

            print("  transforming interferograms...")
            spec = raw_spectra(fields["ifg"], channel, mode, fields["adds_per_group"],
                               fields["bol_cmd_bias"], fields["bol_volt"],
                               temps[bolometer_row(temps, channel)], fnyq_icm,
                               fields["gain"], fields["sweeps"])

            finite = np.isfinite(spec[:, band]).all(axis=1)
            if not finite.all():
                print(f"  dropping {(~finite).sum()} interferograms with non-finite spectra")
                spec, temps = spec[finite], temps[:, finite]
                fields = {name: value[finite] for name, value in fields.items()}

            weights = noise_weights(spec, published_band) if args.weight else None
            if weights is not None:
                usable = weights > 0
                if not usable.all():
                    print(f"  dropping {(~usable).sum()} interferograms with no noise estimate")
                    spec, temps, weights = spec[usable], temps[:, usable], weights[usable]
                    fields = {name: value[usable] for name, value in fields.items()}

            memory = get_memory_usage()
            if memory:
                print(f"  memory in use: {memory:.2f} GB")

            published_parameters = np.asarray(bolometer.get_bolometer_parameters(channel, mode),
                                              dtype=float)
            print(f"  profiling {band.stop - band.start} frequency-bin emissivity fits "
                  f"against 9 bolometer parameters ({spec.shape[0]} interferograms)")
            solved.append({"channel": channel, "mode": mode, "spec": spec, "temps": temps,
                           "fields": fields, "weights": weights, "rcond": args.rcond,
                           "sum_zero": args.sum_zero, "published": published, "band": band,
                           "published_band": published_band, "frequencies": frequencies,
                           "published_parameters": published_parameters,
                           "interferograms": spec.shape[0]})

        if not solved:
            continue

        print(f"\n  {channel}: fitting shared bolometer parameters across {len(solved)} mode(s)...")
        # for the first iteration, let's fit Jo alone first, which seems to have a big effect on
        # the fit. afterwards we can fit all together
        Jo_parameters, Jo_optimizer, Jo_evaluations = fit_Jo(solved, args.bolometer_bound_factor, args.bolometer_maxiter)
        print(f"  Jo optimiser: {Jo_optimizer.message} after {Jo_optimizer.nit} iterations / {Jo_evaluations} evaluations")
        print(f"  fitted Jo parameter: {Jo_parameters[BOLOMETER_PARAMETER_NAMES.index('Jo')]:.7g}")

        # put new Jo parameter into the published parameters for the next fit
        for entry in solved:
            entry["published_parameters"][BOLOMETER_PARAMETER_NAMES.index('Jo')] = Jo_parameters[BOLOMETER_PARAMETER_NAMES.index('Jo')]

        parameters, optimizer, evaluations = fit_bolometer_parameters(
            solved, args.bolometer_bound_factor, args.bolometer_maxiter)
        print(f"  optimiser: {optimizer.message} after {optimizer.nit} iterations / "
              f"{evaluations} evaluations")
        print("  fitted bolometer parameters: " + ", ".join(
            f"{name}={value:.7g}" for name, value in zip(BOLOMETER_PARAMETER_NAMES, parameters)))

        for entry in solved:
            mode = entry["mode"]
            band, published_band = entry["band"], entry["published_band"]
            published, frequencies = entry["published"], entry["frequencies"]
            print(f"\n{'-' * 70}\n{channel}_{mode}: results with fitted bolometer parameters\n{'-' * 70}")
            profiled = solve_with_bolometer_parameters(
                entry["spec"], entry["temps"], frequencies, band, channel, mode, entry["fields"],
                entry["published_parameters"], parameters, entry["weights"], entry["rcond"],
                entry["sum_zero"], keep_solution=True)
            if profiled is None:
                raise RuntimeError("best-fit bolometer response is not finite")
            _, solution, diagnostics = profiled

            summary[f"{channel}_{mode}"] = report(solution, published, diagnostics, band,
                                                  published_band, frequencies)
            summary[f"{channel}_{mode}"]["interferograms"] = entry["interferograms"]
            summary[f"{channel}_{mode}"]["parameters"] = parameters
            plot_solution(solution, published, band, published_band, frequencies, channel, mode,
                          args.out_dir)

            path = f"{args.out_dir}/fitted_emissivities_{channel}_{mode}.npy"
            np.save(path, solution)
            print(f"  wrote {path}")

            path = f"{args.out_dir}/fit_bolometer_{channel}_{mode}.npz"
            np.savez(path, bolometer_parameter_names=BOLOMETER_PARAMETER_NAMES,
                     bolometer_parameters=parameters,
                     published_bolometer_parameters=entry["published_parameters"],
                     band=[band.start, band.stop],
                     published_band=[published_band.start, published_band.stop], **diagnostics)
            print(f"  wrote {path}")

    if len(summary) > 1:
        print(f"\n{'=' * 70}\nAll channels and modes\n{'=' * 70}")
        print(f"  {'':>8} {'ifgs':>7} {'published band':>16} {'usable to':>16} "
              f"{'chi2/null':>10} {'|dOTF|/OTF':>11}")
        for name, row in summary.items():
            print(f"  {name:>8} {row['interferograms']:>7} "
                  f"{row['published_top_bin']:>5} {row['published_top_ghz']:>9.1f} GHz "
                  f"{row['usable_top_bin']:>5} {row['usable_top_ghz']:>9.1f} GHz "
                  f"{row['chi2_ratio']:>10.4f} {row['otf_error']:>11.3f}")
        print("\n  'usable to' is the top of the contiguous run of bins where the model explains\n"
              "  more than half the variance and every |E| <= 1; chi2/null and |dOTF|/OTF are\n"
              "  medians over the published band.")


if __name__ == "__main__":
    main()
