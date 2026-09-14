"""Small numerical residual report for the minimal calibration fit."""

import argparse
import os

import matplotlib.pyplot as plt
import numpy as np
from astropy.io import fits

import globals as g
from calibration.fit_otf_minimal import load_published_parameters
from simulations.main import generate_ifg
from utils import my_utils as utils


def rms(values):
    """Root-mean-square magnitude, ignoring non-finite values."""
    values = np.asarray(values)
    values = values[np.isfinite(values)]
    return np.sqrt(np.mean(np.abs(values) ** 2))


def residual_statistics(measured, model):
    """Return scale-independent summary statistics for complex residuals."""
    residual = np.asarray(measured) - np.asarray(model)
    return {
        "data_rms": rms(measured),
        "residual_rms": rms(residual),
        "relative_rms": rms(residual) / rms(measured),
        "sum_squared_residual": np.sum(np.abs(residual) ** 2),
    }


def published_emissivities(channel, mode):
    """Load the seven published emissivities used by the one-bolometer model."""
    cutoff = 5 if mode[1] == "s" else 7
    with fits.open(f"{g.PUB_MODEL}FIRAS_CALIBRATION_MODEL_{channel.upper()}"
                   f"{mode.upper()}.FITS") as fits_data:
        record = fits_data[1].data
        names = ["TRANSFE", "ICAL", "DIHEDRA", "REFHORN", "SKYHORN", "STRUCTU", "BOLOMET"]
        result = np.zeros((g.SPEC_SIZE, len(names)), dtype=complex)
        length = len(record["RTRANSFE"][0])
        for column, name in enumerate(names):
            values = record[f"R{name}"][0] + 1j * record[f"I{name}"][0]
            result[cutoff:cutoff + length, column] = values
    return result


def plot_ifg_debug(channel="ll", mode="ss", index=0, fit_dir="calibration/output",
                   output_path=None):
    """Plot original, simulated, overlaid, and residual IFGs for both models."""
    data = np.load(f"{g.PREPROCESSED_DATA_PATH}cal.npz")
    mode_filter = ((data[f"mtm_length_{channel}"] == (0 if mode[0] == "s" else 1)) &
                   (data[f"mtm_speed_{channel}"] == (0 if mode[1] == "s" else 1)))
    names = ["xcal_cone", "ical", "dihedral", "refhorn", "skyhorn", "collimator",
             "bolometer"]
    temps = np.vstack([data[f"{name}_{channel}"][mode_filter] for name in names])
    original = data[f"ifg_{channel}"][mode_filter]
    if not 0 <= index < len(original):
        raise IndexError(f"index {index} is outside 0:{len(original)}")
    apod = np.ones(512, dtype=float)
    common = dict(
        channel=channel, mode=mode, temps=temps, apod=apod,
        adds_per_group=data[f"adds_per_group_{channel}"][mode_filter],
        sweeps=data[f"sweeps_{channel}"][mode_filter],
        bol_cmd_bias=data[f"bol_cmd_bias_{channel}"][mode_filter],
        bol_volt=data[f"bol_volt_{channel}"][mode_filter],
        gain=data[f"gain_{channel}"][mode_filter],
    )
    fitted_emissivities = np.load(f"{fit_dir}/fitted_emissivities_{channel}_{mode}.npy")
    with np.load(f"{fit_dir}/fit_minimal_{channel}_{mode}.npz") as diagnostics:
        fitted_parameters = diagnostics["bolometer_parameters"]
    models = {
        "published": (published_emissivities(channel, mode),
                      load_published_parameters(channel, mode)),
        "fitted": (fitted_emissivities, fitted_parameters),
    }
    simulated = {}
    for name, (emissivities, parameters) in models.items():
        simulated[name], _ = generate_ifg(emissivities=emissivities,
                                           bol_params=parameters, **common)

    original = original[index] - np.median(original[index])
    fig, axes = plt.subplots(4, 1, figsize=(10, 15), sharex=True, sharey=True)
    axes[0].plot(original, color="black")
    axes[0].set_title("Original IFG")
    for name, values in simulated.items():
        values = values[index]
        axes[1].plot(values, label=name)
        axes[2].plot(values, label=name)
        axes[3].plot(original - values, label=name)
    axes[1].set_title("Simulated IFG")
    axes[2].plot(original, color="black", label="original")
    axes[2].set_title("Original vs simulated")
    axes[3].set_title("Residuals (original - simulated)")
    for axis in axes[1:]:
        axis.legend(fontsize="small")
    for axis in axes:
        axis.set_ylabel("Amplitude")
    axes[-1].set_xlabel("Sample")
    fig.suptitle(f"{channel.upper()} {mode.upper()} IFG {index}")
    fig.tight_layout(rect=[0, 0, 1, 0.97])
    if output_path is None:
        output_path = f"{fit_dir}/{channel}_{mode}_{index}_ifg_debug.png"
    fig.savefig(output_path, bbox_inches="tight")
    plt.close(fig)
    return output_path


def report(path):
    """Print the fit diagnostics saved by fit_otf_minimal.py."""
    with np.load(path) as diagnostics:
        residual_sum = diagnostics["residual_sum"]
        condition_number = diagnostics["condition_number"]
        print(f"objective: {diagnostics['objective']:.6e}")
        print(f"median residual sum: {np.median(residual_sum):.6e}")
        print(f"maximum condition number: {np.nanmax(condition_number):.6e}")
        print(f"successful optimizer: {bool(diagnostics['success'])}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Report minimal fit diagnostics.")
    parser.add_argument("path", help="Path to fit_minimal_*.npz")
    parser.add_argument("--plot", action="store_true")
    parser.add_argument("--channel", default="ll")
    parser.add_argument("--mode", default="ss")
    parser.add_argument("--index", type=int, default=0)
    parser.add_argument("--fit-dir", default="calibration/output")
    args = parser.parse_args()
    if not os.path.exists(args.path):
        raise FileNotFoundError(args.path)
    report(args.path)
    if args.plot:
        print(plot_ifg_debug(args.channel, args.mode, args.index, args.fit_dir))