"""
Script to fit an optical transfer function to the calibration data. For now, it follows the model:
S_sky/XCAL = 1/OTF * 1/ETF * 1/Bol * Y - R,
where Y is the Fourier transformed interferogram and R is defined by
R = 1/OTF * sum over all emitters i (excluding XCAL) of E_i * P(T_i).
"""

import argparse
import gc
import os
import time
from multiprocessing import Pool, cpu_count

import matplotlib.pyplot as plt
import numpy as np
from astropy.io import fits
from scipy.optimize import fmin_l_bfgs_b, minimize

import globals as g
from pipeline import ifg_spec
from utils import my_utils as utils
from utils.config import gen_nyquistl


def get_memory_usage():
    """Get current memory usage in GB."""
    try:
        import psutil
        process = psutil.Process(os.getpid())
        return process.memory_info().rss / 1e9
    except ImportError:
        return None


def D(Ei, nui, ifg, channel, mode, gain, sweeps, bol_cmd_bias, bol_volt, temps, adds_per_group,
      fnyq_icm, apod=False):

    spec = ifg_spec.ifg_to_spec(ifg, channel, mode, adds_per_group, bol_cmd_bias, bol_volt,
                                fnyq_icm, otf=Ei[0],
                                Tbol=temps[6],  # TODO: generalize for other channels
                                apod=apod, gain=gain, sweeps=sweeps, nui=nui)

    return spec


def R(Ei, nui, temps, frequencies):
    """
    R is the function that weights each of the emitters.

    Parameters
    ----------
    H : array_like
        Optical transfer function a.k.a. emissivity of the XCAL.
    Ei : array_like
        2D array of the emissivities over frequency for each of the other nine emitters.
        1: ICAL, 2: dihedral, 3: refhorn, 4: skyhorn, 5: collimator, 6: bolometer_rh,
        7: bolometer_rl, 8: bolometer_lh, 9: bolometer_ll
    """
    sum = np.zeros_like(temps[0], dtype=complex)
    for i in range(1, Ei.shape[0]):
        # All bolometers (indices 6-9) use the same temperature temps[6]
        temp_idx = min(i, 6)
        sum += Ei[i] * utils.planck(frequencies[nui], temps[temp_idx])

    H = Ei[0]

    return sum / H


def S(nui, frequencies, temps):
    return utils.planck(frequencies[nui], temps[0])


def full_function(Ei, nui, ifg, channel, mode, gain, sweeps, bol_cmd_bias, bol_volt, temps,
                  adds_per_group, fnyq_icm, frequencies):
    result = (D(Ei, nui, ifg, channel, mode, gain, sweeps, bol_cmd_bias, bol_volt, temps,
                adds_per_group, fnyq_icm) - R(Ei, nui, temps, frequencies) - S(nui, frequencies,
                                                                               temps))

    return np.sum(np.abs(result) ** 2)  # Use abs to handle complex numbers properly


def real_to_complex(z):
    """
    Real vector of length 2n -> complex of length n.
    Taken from https://stackoverflow.com/questions/51211055/can-scipy-optimize-minimize-functions-of-complex-variables-at-all-and-how.
    """
    return z[: len(z) // 2] + 1j * z[len(z) // 2 :]


def complex_to_real(z):
    """
    Complex vector of length n -> real of length 2n
    Taken from https://stackoverflow.com/questions/51211055/can-scipy-optimize-minimize-functions-of-complex-variables-at-all-and-how.
    """
    return np.concatenate((np.real(z), np.imag(z)))


def full_function_real(Ei_real, nui, ifg, channel, mode, gain, sweeps, bol_cmd_bias, bol_volt,
                       temps, adds_per_group, fnyq_icm, frequencies):
    """Wrapper that converts real parameters to complex for optimization."""
    Ei = real_to_complex(Ei_real)
    return full_function(Ei, nui, ifg, channel, mode, gain, sweeps, bol_cmd_bias, bol_volt, temps,
                         adds_per_group, fnyq_icm, frequencies)


# Global variables for multiprocessing to avoid copying large arrays
_global_data = {}

def _init_worker(ifg, channel, mode, gain, sweeps, bol_cmd_bias, bol_volt, temps, 
                 adds_per_group, fnyq_icm, frequencies, cutoff):
    """Initialize worker process with shared data."""
    _global_data['ifg'] = ifg
    _global_data['channel'] = channel
    _global_data['mode'] = mode
    _global_data['gain'] = gain
    _global_data['sweeps'] = sweeps
    _global_data['bol_cmd_bias'] = bol_cmd_bias
    _global_data['bol_volt'] = bol_volt
    _global_data['temps'] = temps
    _global_data['adds_per_group'] = adds_per_group
    _global_data['fnyq_icm'] = fnyq_icm
    _global_data['frequencies'] = frequencies
    _global_data['cutoff'] = cutoff
    
    # Load FITS data ONCE per worker to avoid repeated file I/O
    fits_data = fits.open(
        f"{g.PUB_MODEL}FIRAS_CALIBRATION_MODEL_{channel.upper()}{mode.upper()}.FITS"
    )
    otf = fits_data[1].data["RTRANSFE"][0] + 1j * fits_data[1].data["ITRANSFE"][0]
    otf = otf[np.abs(otf) > 0]
    
    # Pre-extract all emissivity data
    _global_data['fits_otf'] = otf
    _global_data['fits_ical'] = (fits_data[1].data["RICAL"][0] + 
                                  1j * fits_data[1].data["IICAL"][0])[:len(otf)]
    _global_data['fits_dihedra'] = (fits_data[1].data["RDIHEDRA"][0] + 
                                     1j * fits_data[1].data["IDIHEDRA"][0])[:len(otf)]
    _global_data['fits_refhorn'] = (fits_data[1].data["RREFHORN"][0] + 
                                     1j * fits_data[1].data["IREFHORN"][0])[:len(otf)]
    _global_data['fits_skyhorn'] = (fits_data[1].data["RSKYHORN"][0] + 
                                     1j * fits_data[1].data["ISKYHORN"][0])[:len(otf)]
    _global_data['fits_structu'] = (fits_data[1].data["RSTRUCTU"][0] + 
                                     1j * fits_data[1].data["ISTRUCTU"][0])[:len(otf)]
    _global_data['fits_bolomet'] = (fits_data[1].data["RBOLOMET"][0] + 
                                     1j * fits_data[1].data["IBOLOMET"][0])[:len(otf)]
    fits_data.close()

def fit_single_frequency(nui):
    """
    Fit emissivities for a single frequency index.
    This function is designed to be called in parallel.
    Uses global data initialized by _init_worker.
    1: ICAL, 2: dihedral, 3: refhorn, 4: skyhorn, 5: collimator, 6: bolometer_rh, 7: bolometer_rl,
    8: bolometer_lh, 9: bolometer_ll
    """
    # Get data from global variables
    ifg = _global_data['ifg']
    channel = _global_data['channel']
    mode = _global_data['mode']
    gain = _global_data['gain']
    sweeps = _global_data['sweeps']
    bol_cmd_bias = _global_data['bol_cmd_bias']
    bol_volt = _global_data['bol_volt']
    temps = _global_data['temps']
    adds_per_group = _global_data['adds_per_group']
    fnyq_icm = _global_data['fnyq_icm']
    frequencies = _global_data['frequencies']
    cutoff = _global_data['cutoff']
    
    # Get pre-loaded FITS data
    otf = _global_data['fits_otf']
    fits_ical = _global_data['fits_ical']
    fits_dihedra = _global_data['fits_dihedra']
    fits_refhorn = _global_data['fits_refhorn']
    fits_skyhorn = _global_data['fits_skyhorn']
    fits_structu = _global_data['fits_structu']
    fits_bolomet = _global_data['fits_bolomet']

    # Set initial guess as the published emissivities (only for this frequency)
    Ei0 = np.zeros((10, 257), dtype=complex)
    Ei0[0][cutoff:cutoff+len(otf)] = otf
    Ei0[1][cutoff:cutoff+len(otf)] = fits_ical
    Ei0[2][cutoff:cutoff+len(otf)] = fits_dihedra
    Ei0[3][cutoff:cutoff+len(otf)] = fits_refhorn
    Ei0[4][cutoff:cutoff+len(otf)] = fits_skyhorn
    Ei0[5][cutoff:cutoff+len(otf)] = fits_structu
    Ei0[6][cutoff:cutoff+len(otf)] = fits_bolomet
    Ei0[7] = Ei0[6]
    Ei0[8] = Ei0[6]
    Ei0[9] = Ei0[6]

    Ei0_real = complex_to_real(Ei0)


    x, f, d = fmin_l_bfgs_b(
        full_function_real,
        Ei0_real,
        args=(
            nui,
            ifg,
            channel,
            mode,
            gain,
            sweeps,
            bol_cmd_bias,
            bol_volt,
            temps,
            adds_per_group,
            fnyq_icm,
            frequencies,
        ),
        approx_grad=True,
        epsilon=1e-8,  # Larger epsilon for more stable gradient approximation
        maxiter=500,
        factr=1e7,  # Moderate tolerance
        pgtol=1e-5,
    )

    # If L-BFGS-B fails with line search error, try Powell method (derivative-free)
    if d.get("warnflag") == 2:
        result = minimize(
            full_function_real,
            Ei0_real,
            args=(
                nui,
                ifg,
                channel,
                mode,
                gain,
                sweeps,
                bol_cmd_bias,
                bol_volt,
                temps,
                adds_per_group,
                fnyq_icm,
                frequencies,
            ),
            method="Powell",
            options={"maxiter": 1000, "ftol": 1e-6},
        )
        x = result.x
        success = result.success
        if not success and nui % 10 == 0:
            print(f"Warning: Frequency {nui} - Powell method failed to converge")
    elif d.get("warnflag") != 0 and nui % 10 == 0:
        print(
            f"Warning: Frequency {nui} with code {d.get('warnflag')} - {d.get('task', '')}"
        )

    # Convert result back to complex
    Ei_solution = real_to_complex(x)

    return nui, Ei_solution


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Fit optical transfer function.")
    parser.add_argument(
        "--n",
        type=int,
        default=None,
        help="IFG index to fit (if not provided, fit all frequencies).",
    )
    args = parser.parse_args()
    n = args.n

    channels = ["ll"]
    modes = ["ss"]

    data = np.load(f"{g.PREPROCESSED_DATA_PATH}cal.npz", "r")
    print(f"Data loaded from {g.PREPROCESSED_DATA_PATH}cal.npz")
    print("Preparing data for fitting...")
    for channel in channels:
        mtm_length = data[f"mtm_length_{channel}"]
        mtm_speed = data[f"mtm_speed_{channel}"]

        for mode in modes:
            if mode == "lf" and (channel[1] == "h"):
                continue
            if mode[0] == "s":
                length_filter = mtm_length == 0
            else:
                length_filter = mtm_length == 1
            if mode[1] == "s":
                speed_filter = mtm_speed == 0
            else:
                speed_filter = mtm_speed == 1

            mode_filter = length_filter & speed_filter

            # if n is chosen, load only the needed data
            if n is None:
                ifg = data[f"ifg_{channel}"][mode_filter]
                xcal = data[f"xcal_cone_{channel}"][mode_filter]  # TODO: update this
                ical = data[f"ical_{channel}"][mode_filter]
                dihedral = data[f"dihedral_{channel}"][mode_filter]
                refhorn = data[f"refhorn_{channel}"][mode_filter]
                skyhorn = data[f"skyhorn_{channel}"][mode_filter]
                collimator = data[f"collimator_{channel}"][mode_filter]
                # TODO: fit for all bolometers
                bol = data[f"bolometer_{channel}"][mode_filter]

                adds_per_group = data[f"adds_per_group_{channel}"][mode_filter]
                sweeps = data[f"sweeps_{channel}"][mode_filter]

                bol_cmd_bias = data[f"bol_cmd_bias_{channel}"][mode_filter]
                bol_volt = data[f"bol_volt_{channel}"][mode_filter]
                gain = data[f"gain_{channel}"][mode_filter]
            else:
                ifg = data[f"ifg_{channel}"][mode_filter][n]
                xcal = data[f"xcal_cone_{channel}"][mode_filter][n]  # TODO: update this
                ical = data[f"ical_{channel}"][mode_filter][n]
                dihedral = data[f"dihedral_{channel}"][mode_filter][n]
                refhorn = data[f"refhorn_{channel}"][mode_filter][n]
                skyhorn = data[f"skyhorn_{channel}"][mode_filter][n]
                collimator = data[f"collimator_{channel}"][mode_filter][n]
                bol = data[f"bolometer_{channel}"][mode_filter][n]

            # plot a random ifg to check
            n = np.random.randint(ifg.shape[0]) if ifg.ndim == 2 else n

            plt.plot(ifg[n] if ifg.ndim == 2 else ifg)
            plt.title(f"Random IFG at index {n}")
            plt.savefig(f"calibration/output/random_ifgs/{n}.png")
            print(f"Random IFG plotted and saved for index {n} in calibration/output/random_ifgs/"
                  f"{n}.png")

            # print(f"data keys: {list(data.keys())}")

            # TODO: fit for temp weight coefficients too
            # xcal = (data["a_xcal"][:] + data["b_xcal"][:]) / 2
            # ical = (data["a_ical"][:] + data["b_ical"][:]) / 2
            # dihedral = (data["a_dihedral"][:] + data["b_dihedral"][:]) / 2
            # refhorn = (data["a_refhorn"][:] + data["b_refhorn"][:]) / 2
            # skyhorn = (data["a_skyhorn"][:] + data["b_skyhorn"][:]) / 2
            # collimator = (data["a_collimator"][:] + data["b_collimator"][:]) / 2
            # bolometer_ll = (data["a_bol_assem_ll"][:] + data["b_bol_assem_ll"][:]) / 2
            # bolometer_lh = (data["a_bol_assem_lh"][:] + data["b_bol_assem_lh"][:]) / 2
            # bolometer_rl = (data["a_bol_assem_rl"][:] + data["b_bol_assem_rl"][:]) / 2
            # bolometer_rh = (data["a_bol_assem_rh"][:] + data["b_bol_assem_rh"][:]) / 2

            temps = np.array([xcal, ical, dihedral, refhorn, skyhorn, collimator, bol])

            frequencies = utils.generate_frequencies(channel, mode, 257)

            fnyq = gen_nyquistl("../reference/fex_samprate.txt", "../reference/fex_nyquist.txt",
                                "int")
            frec = 4 * (g.CHANNELS[channel] % 2) + g.MODES[mode]
            fnyq_icm = fnyq["icm"][frec]

            # 10 emissivities to fit
            solution = np.zeros((g.SPEC_SIZE, temps.shape[0]), dtype=complex)  

            # Determine number of processes to use
            n_processes = cpu_count() // 10
            print(f"Using {n_processes} CPU cores for parallel processing")
            
            mem = get_memory_usage()
            if mem:
                print(f"Memory before parallel processing: {mem:.2f} GB")

            cutoff = 5 if mode[1] == "s" else 7

            start0 = time.time()
            print(f"Starting parallel optimization for {g.SPEC_SIZE} frequencies...")

            # Parallel processing with initializer to avoid copying large arrays
            with Pool(processes=n_processes, initializer=_init_worker,
                      initargs=(ifg, channel, mode, gain, sweeps, bol_cmd_bias, bol_volt, temps,
                                adds_per_group, fnyq_icm, frequencies, cutoff)) as pool:
                # Use imap to maintain order and track progress
                results = []
                for i, result in enumerate(pool.imap(fit_single_frequency, range(g.SPEC_SIZE)), 1):
                    results.append(result)
                    # Print progress every 10 frequencies or at the end
                    if i % 10 == 0 or i == g.SPEC_SIZE:
                        elapsed = time.time() - start0
                        rate = i / elapsed
                        remaining = (g.SPEC_SIZE - i) / rate if rate > 0 else 0
                        print(f"Progress: {i}/{g.SPEC_SIZE} frequencies complete"
                              f" ({i/g.SPEC_SIZE*100:.1f}%) | Elapsed: {elapsed:.1f}s | ETA: "
                              f"{remaining:.1f}s")

            # Collect results
            failed_fits = 0
            for nui, emissivities in results:
                solution[nui] = emissivities

            end_time = time.time()
            total_seconds = end_time - start0

            print(f"\n{'='*60}")
            print(f"Optimization Complete!")
            print(f"{'='*60}")
            print(f"Total time taken: {total_seconds:.2f} seconds ({total_seconds/60:.2f} minutes)")
            print(f"Average time per frequency: {total_seconds/g.SPEC_SIZE:.2f} seconds")
            print(f"Speed improvement: {149.78/(total_seconds/g.SPEC_SIZE):.1f}x faster than before")

            # save solution
            np.save(f"calibration/output/fitted_emissivities_{channel}_{mode}.npy", solution)
            
            # Explicit cleanup to free memory before next iteration
            del ifg, xcal, ical, dihedral, refhorn, skyhorn, collimator, bol
            del temps, frequencies, solution, results
            if 'adds_per_group' in locals():
                del adds_per_group, sweeps, bol_cmd_bias, bol_volt, gain
            gc.collect()
            
            mem = get_memory_usage()
            if mem:
                print(f"Memory after cleanup for {channel}_{mode}: {mem:.2f} GB")
            
            n = None
