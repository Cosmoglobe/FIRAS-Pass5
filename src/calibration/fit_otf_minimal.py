"""Minimal emissivity and bolometer fit based on the original fit_otf.py."""

import argparse
import os

import numpy as np
from astropy.io import fits
from scipy.optimize import minimize

import globals as g
from pipeline import ifg_spec
from utils import my_utils as utils
from utils.config import gen_nyquistl

PARAMETER_NAMES = ("R0", "T0", "G1", "beta", "rho", "C1", "C3", "Jo", "Jg")
N_EMISSIVITIES = 7

def measured_spectrum(ifg, channel, mode, adds_per_group, bol_cmd_bias, bol_volt, gain,
                      sweeps, Tbol, fnyq_icm, bol_params=None, nui=None):
    """Return the measured spectrum with the detector response removed."""
    return ifg_spec.ifg_to_spec(
        ifg, channel, mode, adds_per_group, bol_cmd_bias, bol_volt, fnyq_icm,
        otf=np.ones(g.SPEC_SIZE), Tbol=Tbol, apod=np.ones(512), gain=gain,
        sweeps=sweeps, nui=nui, bol_params=bol_params,
    )


def design_matrix(nui, frequencies, temps):
    """Planck spectrum for XCAL followed by this channel's six emitters."""
    emitter_temperatures = temps[:N_EMISSIVITIES]
    return np.column_stack([
        utils.planck(frequencies[nui], temperature) for temperature in emitter_temperatures
    ])


def solve_emissivities(measured, nui, frequencies, temps, weights=None):
    """Solve the seven complex emissivities for one frequency."""
    matrix = design_matrix(nui, frequencies, temps)
    if weights is not None:
        matrix = matrix * weights[:, np.newaxis]
        measured = measured * weights
    solution, _, rank, singular_values = np.linalg.lstsq(matrix, measured, rcond=None)
    return solution, rank, singular_values


def fit_emissivities(ifg, channel, mode, gain, sweeps, bol_cmd_bias, bol_volt, temps,
                     adds_per_group, fnyq_icm, frequencies, bol_params=None):
    """Solve the original ten-emitter model independently at every frequency."""
    measured = measured_spectrum(ifg, channel, mode, adds_per_group, bol_cmd_bias,
                                 bol_volt, gain, sweeps, temps[6], fnyq_icm,
                                 bol_params=bol_params)
    solution = np.zeros((g.SPEC_SIZE, N_EMISSIVITIES), dtype=complex)
    residual_sum = np.zeros(g.SPEC_SIZE)
    ranks = np.zeros(g.SPEC_SIZE, dtype=int)
    condition_numbers = np.full(g.SPEC_SIZE, np.inf)
    for nui in range(g.SPEC_SIZE):
        solution[nui], ranks[nui], singular_values = solve_emissivities(
            measured[:, nui], nui, frequencies, temps
        )
        residual = measured[:, nui] - design_matrix(nui, frequencies, temps) @ solution[nui]
        residual_sum[nui] = np.sum(np.abs(residual) ** 2)
        if singular_values[-1] != 0:
            condition_numbers[nui] = singular_values[0] / singular_values[-1]
    return solution, {"residual_sum": residual_sum, "rank": ranks,
                      "condition_number": condition_numbers}


def bolometer_objective(log_scales, data, published_parameters):
    """Profile emissivities out and return the detector-corrected residual sum."""
    parameters = np.asarray(published_parameters) * np.exp(log_scales)
    _, diagnostics = fit_emissivities(*data, bol_params=parameters)
    return np.sum(diagnostics["residual_sum"])


def fit_bolometer_parameters(data, published_parameters):
    """Fit multiplicative changes to the nine published bolometer parameters."""
    result = minimize(
        bolometer_objective, np.zeros(len(PARAMETER_NAMES)),
        args=(data, published_parameters), method="Powell",
        options={"maxiter": 100, "xtol": 1e-4, "ftol": 1e-4},
    )
    parameters = np.asarray(published_parameters) * np.exp(result.x)
    return parameters, result


def load_published_parameters(channel, mode):
    """Read the published bolometer parameters for one channel and mode."""
    with fits.open(f"{g.PUB_MODEL}FIRAS_CALIBRATION_MODEL_{channel.upper()}"
                   f"{mode.upper()}.FITS") as fits_data:
        row = fits_data[1].data
        return np.array([row[f"BOLPARM{i if i > 1 else '_'}"][0]
                         if i == 1 else row[f"BOLPARM{i}"][0]
                         for i in range(1, 10)])


def fit_channel_mode(channel="ll", mode="ss", output_dir="calibration/output"):
    """Fit one channel/mode and save emissivities plus numerical diagnostics."""
    data = np.load(f"{g.PREPROCESSED_DATA_PATH}cal.npz")
    mode_filter = ((data[f"mtm_length_{channel}"] == (0 if mode[0] == "s" else 1)) &
                   (data[f"mtm_speed_{channel}"] == (0 if mode[1] == "s" else 1)))
    ifg = data[f"ifg_{channel}"][mode_filter]
    names = ["xcal_cone", "ical", "dihedral", "refhorn", "skyhorn", "collimator",
             "bolometer"]
    temps = np.vstack([data[f"{name}_{channel}"][mode_filter] for name in names])
    fit_data = (ifg, channel, mode, data[f"gain_{channel}"][mode_filter],
                data[f"sweeps_{channel}"][mode_filter],
                data[f"bol_cmd_bias_{channel}"][mode_filter],
                data[f"bol_volt_{channel}"][mode_filter], temps,
                data[f"adds_per_group_{channel}"][mode_filter],
                gen_nyquistl("reference/fex_samprate.txt", "reference/fex_nyquist.txt", "int")
                ["icm"][4 * (g.CHANNELS[channel] % 2) + g.MODES[mode]],
                utils.generate_frequencies(channel, mode, g.SPEC_SIZE))
    published = load_published_parameters(channel, mode)
    parameters, result = fit_bolometer_parameters(fit_data, published)
    emissivities, diagnostics = fit_emissivities(*fit_data, bol_params=parameters)
    os.makedirs(output_dir, exist_ok=True)
    np.save(f"{output_dir}/fitted_emissivities_{channel}_{mode}.npy", emissivities)
    np.savez(f"{output_dir}/fit_minimal_{channel}_{mode}.npz",
             bolometer_parameters=parameters, published_parameters=published,
             objective=result.fun, success=result.success, **diagnostics)
    return emissivities, parameters, diagnostics


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Fit FIRAS emissivities and bolometer parameters.")
    parser.add_argument("--channel", default="ll")
    parser.add_argument("--mode", default="ss")
    parser.add_argument("--output-dir", default="calibration/output")
    args = parser.parse_args()
    fit_channel_mode(args.channel, args.mode, args.output_dir)