"""
Forward model the calibration interferograms from the measured temperatures and compare
against the real ones, for the published calibration model and for the emissivities
fitted by `fit_otf` side by side.

The model is the one `fit_otf` fits:

    Y = ETF * Bol * FFT[ OTF * P(T_xcal) + sum_{i>0} E_i * P(T_i) ]

over XCAL, ICAL, dihedral, refhorn, skyhorn, collimator and the bolometer.  Every
emitter carries its own emissivity and none are dropped: FIRAS is differential and the
emissivities sum to zero, so XCAL and ICAL cancel to well under a percent and the terms
that look negligible are a few to ten per cent of what is left.

Each model is used with its own bolometer time constant.  The published emissivities go
with the published tau; the fitted ones only make sense with the tau scale they were
fitted against, which `fit_otf` writes alongside them, because the emissivities absorb
whatever the response function got wrong.
"""

import argparse
import os
import textwrap

import matplotlib.pyplot as plt
import numpy as np
from astropy.io import fits

import globals as g
import utils.my_utils as utils
from pipeline import ifg_spec
from simulations.main import generate_ifg, safe_divide
from utils.config import gen_nyquistl

# The emitters, in the order the emissivity columns are built below.
LABELS = ["XCAL", "ICAL", "Dihedral", "Refhorn", "Skyhorn", "Collimator", "Bolometer"]

# How each model is drawn, so the two are told apart on every panel.
STYLE = {"published": "tab:blue", "fitted": "tab:red"}

# argparse add-on to filenames and titles
parser = argparse.ArgumentParser(description="Process some calibration data.")
parser.add_argument("--suffix", type=str, default="", help="Suffix to add to filenames and titles")
parser.add_argument("--fit-dir", default="calibration/output", help="Where fit_otf wrote its "
                    "results. If they are missing, only the published model is used.")
parser.add_argument("--index", type=int, default=1704, help="Which interferogram to plot.")
args = parser.parse_args()

if args.suffix != "":
    suffix = f"_{args.suffix}"
else:
    suffix = ""

# suffix = "_otf_3rddeg_ical_2nddeg"

fnyq = gen_nyquistl("../reference/fex_samprate.txt", "../reference/fex_nyquist.txt", "int")

# set all text in figures bigger
plt.rcParams.update({"font.size": 16, "axes.titlesize": 18, "axes.labelsize": 16,
                     "xtick.labelsize": 14, "ytick.labelsize": 14, "legend.fontsize": 14,
                     "figure.titlesize": 20})

ghz_to_icm = g.C / 1e7  # GHz to cm/s


def add_wavenumber_axis(ax):
    secax = ax.secondary_xaxis("top", functions=(lambda x: x / ghz_to_icm, lambda x: x *
                                                 ghz_to_icm))
    secax.set_xlabel("Wavenumber (cm⁻¹)")


print("Loading calibration data...")
for channel in g.CHANNELS:
    data = np.load(f"{g.PREPROCESSED_DATA_PATH}cal.npz")

    # cal.npz stores one column per channel, suffixed with the channel name.
    mtm_length = data[f"mtm_length_{channel}"][:]
    mtm_speed = data[f"mtm_speed_{channel}"][:]

    for mode in g.MODES:
        if channel[1] == "h" and mode == "lf":
            continue
        print(f"Simulating IFGs for {channel.upper()} {mode.upper()}...")
        out_dir = f"calibration/output/checks/{channel}_{mode}"
        os.makedirs(out_dir, exist_ok=True)

        fits_data = fits.open(f"{g.PUB_MODEL}FIRAS_CALIBRATION_MODEL_{channel.upper()}"
                              f"{mode.upper()}.FITS")
        # apod = fits_data[1].data["APODIZAT"][0]
        apod = np.ones(512, dtype=np.float64)  # No apodization for now

        if mode[0] == "s":
            length_filter = mtm_length == 0
        else:
            length_filter = mtm_length == 1
        if mode[1] == "s":
            speed_filter = mtm_speed == 0
        else:
            speed_filter = mtm_speed == 1

        mode_filter = length_filter & speed_filter

        xcal = data[f"xcal_cone_{channel}"][mode_filter]  # TODO: update this
        ical = data[f"ical_{channel}"][mode_filter]  # - 1.5e-1  # -150 mK offset
        dihedral = data[f"dihedral_{channel}"][mode_filter]
        refhorn = data[f"refhorn_{channel}"][mode_filter]
        skyhorn = data[f"skyhorn_{channel}"][mode_filter]
        collimator = data[f"collimator_{channel}"][mode_filter]
        # one bolometer per channel: this channel's own detector, not the other three
        bolometer = data[f"bolometer_{channel}"][mode_filter]
        temps = np.vstack([xcal, ical, dihedral, refhorn, skyhorn, collimator, bolometer])

        adds_per_group = data[f"adds_per_group_{channel}"][mode_filter]
        sweeps = data[f"sweeps_{channel}"][mode_filter]
        bol_cmd_bias = data[f"bol_cmd_bias_{channel}"][mode_filter]
        bol_volt = data[f"bol_volt_{channel}"][mode_filter]
        gain = data[f"gain_{channel}"][mode_filter]

        original_ifgs = data[f"ifg_{channel}"][mode_filter]

        # stuff to process the original IFGs into spectra
        frec = 4 * (g.CHANNELS[channel] % 2) + g.MODES[mode]

        fnyq_icm = fnyq["icm"][frec]

        cutoff = 5 if mode[1] == "s" else 7

        f_ghz = utils.generate_frequencies(channel, mode, 257)

        length = len(fits_data[1].data["RTRANSFE"][0])

        bb = [utils.planck(f_ghz, temps[i]) for i in range(len(LABELS))]

        def published_column(name, fits_data=fits_data, cutoff=cutoff, length=length):
            """One published emissivity column, zero-padded to the full spectrum."""
            column = np.zeros(257, dtype=np.complex128)
            column[cutoff : cutoff + length] = (fits_data[1].data[f"R{name}"][0] + 1j *
                                                fits_data[1].data[f"I{name}"][0])
            return column

        # Every emitter carries its own emissivity. None of these may be dropped: FIRAS
        # is differential and the emissivities sum to zero, so the XCAL and ICAL terms
        # cancel to well under a percent and what is left -- the horns, the collimator --
        # is a few to ten per cent of the surviving signal, not a negligible correction.
        emissivities = {
            "published": np.column_stack([
                published_column(name) for name in
                ["TRANSFE", "ICAL", "DIHEDRA", "REFHORN", "SKYHORN", "STRUCTU", "BOLOMET"]
            ])
        }
        # The published emissivities go with the published time constant.
        tau_scales = {"published": 1.0}

        fitted_path = f"{args.fit_dir}/fitted_emissivities_{channel}_{mode}.npy"
        diagnostics_path = f"{args.fit_dir}/fit_diagnostics_{channel}_{mode}.npz"
        if os.path.exists(fitted_path) and os.path.exists(diagnostics_path):
            fitted = np.load(fitted_path)
            # fit_otf solves for the four bolometer assemblies separately. Their
            # temperatures track each other to a few mK and only their sum enters the
            # model here, so fold the four columns into the one bolometer term. Taking
            # just this channel's column would drop three emitters and break the
            # sum-to-zero the fit imposes, which is what makes the model differential.
            emissivities["fitted"] = np.column_stack(
                [fitted[:, i] for i in range(6)] + [fitted[:, 6:].sum(axis=1)]
            )
            # The fitted emissivities are only consistent with a bolometer response
            # built from tau_scale * tau, so the two have to travel together.
            tau_scales["fitted"] = float(np.load(diagnostics_path)["tau_scale"])
            print(f"  fitted emissivities from {fitted_path}, "
                  f"tau_scale = {tau_scales['fitted']:.4f}")
        else:
            print(f"  no fitted emissivities at {fitted_path}; published model only")

        # n = args.index
        n = np.random.randint(0, original_ifgs.shape[0])
        if not 0 <= n < original_ifgs.shape[0]:
            print(f"  --index {n} outside the {original_ifgs.shape[0]} records; skipping")
            continue

        # Everything each model predicts, keyed by model name.
        results = {}
        for name, E in emissivities.items():
            otf = E[:, 0]
            tau_scale = tau_scales[name]

            processed_spectra = ifg_spec.ifg_to_spec(original_ifgs, channel=channel, mode=mode,
                                                     adds_per_group=adds_per_group, sweeps=sweeps,
                                                     bol_cmd_bias=bol_cmd_bias / 25.5,
                                                     bol_volt=bol_volt, gain=gain,
                                                     fnyq_icm=fnyq_icm, otf=otf, Tbol=temps[6],
                                                     apod=apod, tau_scale=tau_scale)

            # Solve the model for each emitter in turn, given all the others. For the
            # XCAL this is the pipeline's own calibration step,
            #     S_xcal = D - sum_{i>0} E_i P(T_i) / OTF,
            # and for any other emitter it is the same equation rearranged for P(T_i),
            #     P(T_i) = (D - P(T_xcal)) OTF / E_i - sum_{j>0, j!=i} E_j P(T_j) / E_i,
            # where D is the OTF-divided measured spectrum. Emitters with a zero
            # emissivity cannot be solved for and come back NaN, not as a blow-up.
            decomposed = {}
            for i in range(len(LABELS)):
                if i == 0:
                    others = sum(E[:, j] * bb[j] for j in range(1, len(LABELS)))
                    decomposed[i] = processed_spectra - safe_divide(others, otf)
                else:
                    others = sum(E[:, j] * bb[j] for j in range(1, len(LABELS)) if j != i)
                    numerator = (processed_spectra - bb[0]) * otf - others
                    with np.errstate(divide="ignore", invalid="ignore"):
                        decomposed[i] = np.where(E[:, i] != 0, numerator / E[:, i], np.nan)

            simulated_ifgs, simulated_spectra = generate_ifg(
                channel=channel, mode=mode, temps=temps, apod=apod,
                adds_per_group=adds_per_group, sweeps=sweeps, bol_cmd_bias=bol_cmd_bias,
                bol_volt=bol_volt, gain=gain, emissivities=E, tau_scale=tau_scale)

            # Move the XCAL residual from one side of the comparison to the other.
            xcal_residuals = decomposed[0][n] - bb[0][n]
            simulated_plus_noise = simulated_spectra[n] + xcal_residuals

            # Only wanted for record n, so forward model that one record rather than
            # tiling its spectrum across the whole selection.
            one = slice(n, n + 1)
            simulated_ifgs_corrected, _ = generate_ifg(
                channel=channel, mode=mode, temps=temps[:, one], apod=apod,
                adds_per_group=adds_per_group[one], sweeps=sweeps[one],
                bol_cmd_bias=bol_cmd_bias[one], bol_volt=bol_volt[one], gain=gain[one],
                emiss_xcal=otf, tau_scale=tau_scale,
                total_spectra=np.nan_to_num(simulated_plus_noise, nan=0.0)[np.newaxis, :])

            if channel[0] == "r":
                simulated_ifgs = -simulated_ifgs
                # simulated_ifgs_corrected = -simulated_ifgs_corrected

            results[name] = {
                "E": E, "otf": otf, "processed": processed_spectra,
                "decomposed": decomposed, "simulated": simulated_spectra,
                "ifgs": simulated_ifgs, "ifgs_corrected": simulated_ifgs_corrected,
                "corrected": processed_spectra[n] - xcal_residuals,
                "plus_noise": simulated_plus_noise,
            }

        caption = (f"Temps: XCAL={xcal[n]:.2f}, ICAL={ical[n]:.2f}, dihed={dihedral[n]:.2f}, "
                   f"refhorn={refhorn[n]:.2f}, skyhorn={skyhorn[n]:.2f}, "
                   f"collimator={collimator[n]:.2f}, bolometer={bolometer[n]:.2f}")
        caption = textwrap.fill(caption, width=65)

        # plot emissivities
        fig, ax = plt.subplots(4, 2, figsize=(15, 20), sharex=True, sharey=True)
        fig.suptitle(f"{channel.upper()} {mode.upper()} Emissivities")
        for i, label in enumerate(LABELS):
            axis = ax.flatten()[i + 1]
            for name, result in results.items():
                axis.plot(f_ghz, result["E"][:, i].real, color=STYLE[name],
                          label=f"{name} (real)")
                axis.plot(f_ghz, result["E"][:, i].imag, color=STYLE[name], linestyle="--",
                          label=f"{name} (imag)")
            axis.set_title("OTF" if i == 0 else f"{label} Emissivity")
            axis.legend(fontsize="x-small")
        fig.tight_layout(rect=[0, 0, 1, 0.88])
        plt.savefig(f"{out_dir}/emissivities{suffix}.png", bbox_inches="tight")
        plt.close()

        # same but all divided by otf
        fig, ax = plt.subplots(3, 2, figsize=(15, 20), sharex=True, sharey=True)
        fig.suptitle(f"{channel.upper()} {mode.upper()} Emissivities / OTF")
        for i, label in enumerate(LABELS[1:], start=1):
            axis = ax.flatten()[i - 1]
            for name, result in results.items():
                ratio = safe_divide(result["E"][:, i], result["otf"])
                axis.plot(f_ghz, ratio.real, color=STYLE[name], label=f"{name} (real)")
                axis.plot(f_ghz, ratio.imag, color=STYLE[name], linestyle="--",
                          label=f"{name} (imag)")
            axis.set_title(label)
            axis.legend(fontsize="x-small")
        fig.tight_layout(rect=[0, 0, 1, 0.88])
        plt.savefig(f"{out_dir}/emissivities_div_otf{suffix}.png", bbox_inches="tight")
        plt.close()

        fig, ax = plt.subplots(4, 2, figsize=(15, 20), sharex=True, sharey=False)
        fig.suptitle(f"{channel.upper()} {mode.upper()} Black Body Spectra {n}\n{caption}")
        add_wavenumber_axis(ax.flatten()[0])
        add_wavenumber_axis(ax.flatten()[1])

        for name, result in results.items():
            ax.flatten()[0].plot(f_ghz, result["processed"][n], color=STYLE[name],
                                 label=f"{name} processed")
            ax.flatten()[0].plot(f_ghz, result["simulated"][n], color=STYLE[name],
                                 linestyle="--", label=f"{name} sum of BBs")
        ax.flatten()[0].set_title("Processed Spectra")
        ax.flatten()[0].set_ylabel("MJy/sr")
        ax.flatten()[0].legend(fontsize="x-small")

        for i, label in enumerate(LABELS):
            axis = ax.flatten()[i + 1]
            for name, result in results.items():
                axis.plot(f_ghz, result["decomposed"][i][n], color=STYLE[name],
                          label=f"{name} processed")
            axis.plot(f_ghz, bb[i][n], color="black", linestyle="--", label="Black Body")
            axis.set_title(f"{label} Spectra")
            axis.legend(fontsize="x-small")
            if i % 2 == 1:
                axis.set_ylabel("MJy/sr")
        ax.flatten()[6].set_xlabel("Frequency (GHz)")
        ax.flatten()[7].set_xlabel("Frequency (GHz)")
        fig.tight_layout(rect=[0, 0, 1, 0.88])
        fig.savefig(f"{out_dir}/{n}_01_bb_spectra{suffix}.png", bbox_inches="tight")
        plt.close()

        # same plot but with bb * emissivity / otf
        fig, ax = plt.subplots(4, 2, figsize=(15, 20), sharex=True, sharey=False)
        fig.suptitle(f"{channel.upper()} {mode.upper()} Black Body Spectra with "
                     f"Emissivities {n}\n{caption}")
        add_wavenumber_axis(ax.flatten()[0])
        add_wavenumber_axis(ax.flatten()[1])

        for name, result in results.items():
            ax.flatten()[0].plot(f_ghz, result["processed"][n], color=STYLE[name],
                                 label=f"{name} processed")
            ax.flatten()[0].plot(f_ghz, result["simulated"][n], color=STYLE[name],
                                 linestyle="--", label=f"{name} sum of BBs")
        ax.flatten()[0].set_title("Processed Spectra")
        ax.flatten()[0].set_ylabel("MJy/sr")
        ax.flatten()[0].legend(fontsize="x-small")

        for i, label in enumerate(LABELS):
            axis = ax.flatten()[i + 1]
            for name, result in results.items():
                weight = 1.0 if i == 0 else safe_divide(result["E"][:, i], result["otf"])
                axis.plot(f_ghz, result["decomposed"][i][n] * weight, color=STYLE[name],
                          label=f"{name} processed")
                axis.plot(f_ghz, bb[i][n] * weight, color=STYLE[name], linestyle="--",
                          label=f"{name} BB with emissivity")
            axis.set_title(f"{label} Spectra")
            axis.legend(fontsize="x-small")
            if i % 2 == 1:
                axis.set_ylabel("MJy/sr")
        ax.flatten()[6].set_xlabel("Frequency (GHz)")
        ax.flatten()[7].set_xlabel("Frequency (GHz)")
        fig.tight_layout(rect=[0, 0, 1, 0.88])
        plt.savefig(f"{out_dir}/{n}_01b_bb_spectra_emissivities{suffix}.png",
                    bbox_inches="tight")
        plt.close()

        # plot the bb curves all together, one panel per model
        fig, ax = plt.subplots(1, len(results), figsize=(9 * len(results), 8),
                               squeeze=False, sharex=True)
        fig.suptitle(f"{channel.upper()} {mode.upper()} Black Body Spectra Components {n}\n"
                     f"{caption}")
        for axis, (name, result) in zip(ax.flatten(), results.items()):
            for i, label in enumerate(LABELS):
                weight = 1.0 if i == 0 else safe_divide(result["E"][:, i], result["otf"])
                axis.plot(f_ghz, bb[i][n] * weight, label=label)
            axis.plot(f_ghz, result["simulated"][n], label="Sum of BBs", linestyle="--",
                      color="black")
            axis.set_title(name)
            axis.set_xlabel("Frequency (GHz)")
            axis.set_ylabel("MJy/sr")
            axis.legend(fontsize="x-small")
        fig.tight_layout(rect=[0, 0, 1, 0.88])
        plt.savefig(f"{out_dir}/{n}_01c_bb_spectra_components{suffix}.png",
                    bbox_inches="tight")
        plt.close()

        # plot residuals
        fig, ax = plt.subplots(4, 2, figsize=(15, 20), sharex=True, sharey=False)
        fig.suptitle(f"{channel.upper()} {mode.upper()} Residuals {n}\n{caption}")
        add_wavenumber_axis(ax.flatten()[0])
        add_wavenumber_axis(ax.flatten()[1])

        for name, result in results.items():
            ax.flatten()[0].plot(f_ghz, result["processed"][n] - result["simulated"][n],
                                 color=STYLE[name], label=name)
        ax.flatten()[0].set_title("Processed Spectra")
        ax.flatten()[0].set_ylabel("MJy/sr")
        ax.flatten()[0].legend(fontsize="x-small")

        for i, label in enumerate(LABELS):
            axis = ax.flatten()[i + 1]
            for name, result in results.items():
                axis.plot(f_ghz, result["decomposed"][i][n] - bb[i][n], color=STYLE[name],
                          label=name)
            axis.set_title(f"{label} Spectra")
            axis.legend(fontsize="x-small")
            if i % 2 == 1:
                axis.set_ylabel("MJy/sr")
        ax.flatten()[6].set_xlabel("Frequency (GHz)")
        ax.flatten()[7].set_xlabel("Frequency (GHz)")
        fig.tight_layout(rect=[0, 0, 1, 0.88])
        plt.savefig(f"{out_dir}/{n}_02_residuals{suffix}.png", bbox_inches="tight")
        plt.close()

        # the spectrum with the XCAL residual moved from one side to the other
        fig, ax = plt.subplots(2, 1, figsize=(10, 8), sharex=True)
        fig.suptitle(f"{channel.upper()} {mode.upper()} Spectra {n}\n{caption}")
        add_wavenumber_axis(ax[0])

        for name, result in results.items():
            ax[0].plot(f_ghz, result["corrected"], color=STYLE[name],
                       label=f"{name} processed, XCAL noise removed")
            ax[0].plot(f_ghz, result["simulated"][n], color=STYLE[name], linestyle="--",
                       label=f"{name} simulated")
            ax[1].plot(f_ghz, result["plus_noise"], color=STYLE[name],
                       label=f"{name} simulated plus XCAL noise")
            ax[1].plot(f_ghz, result["processed"][n], color=STYLE[name], linestyle="--",
                       label=f"{name} processed")
        ax[0].set_ylabel("MJy/sr")
        ax[0].legend(fontsize="x-small")
        ax[1].set_xlabel("Frequency (GHz)")
        ax[1].set_ylabel("MJy/sr")
        ax[1].legend(fontsize="x-small")
        fig.tight_layout(rect=[0, 0, 1, 0.88])
        plt.savefig(f"{out_dir}/{n}_03_spectra_comparison{suffix}.png", bbox_inches="tight")
        plt.close()

        # interferogram space: original against each model, and the residuals
        if mode == "lf":
            scan_length = 7.07  # cm
        else:
            scan_length = 1.76  # cm
        peak = g.PEAK_POSITIONS[f"{channel}_{mode}"]
        original = original_ifgs[n, :] - np.median(original_ifgs[n, :])

        for tag, key, index, name in (
            ("04_ifg_residuals", "ifgs", n, "Simulated IFG"),
            ("05_ifg_residuals_corrected", "ifgs_corrected", 0, "Simulated Corrected IFG"),
        ):
            print(f"Plotting IFG {n} for {channel.upper()} {mode.upper()} ({tag})...")
            fig, ax = plt.subplots(4, 1, figsize=(10, 15), sharex=True, sharey=True)
            fig.suptitle(f"{channel.upper()} {mode.upper()} IFG {n}\n{caption}")

            secax = ax[0].secondary_xaxis(
                "top",
                functions=(lambda x: x * scan_length / 512, lambda x: x * 512 / scan_length),
            )
            secax.set_xlabel("Length (cm)")

            ax[0].plot(original, color="black")
            ax[0].set_title("Original IFG")

            for model, result in results.items():
                simulated = result[key][index, :]
                ax[1].plot(simulated, color=STYLE[model], label=model)
                ax[2].plot(simulated, color=STYLE[model], label=f"{model} {name.lower()}")
                ax[3].plot(original - simulated, color=STYLE[model], label=model)
            ax[1].set_title(name)
            ax[1].legend(fontsize="x-small")

            ax[2].plot(original, color="black", label="Original IFG")
            ax[2].set_title(f"Original vs {name}")
            ax[2].legend(fontsize="x-small")

            ax[3].set_title(f"Residuals (original - {name.lower()})")
            ax[3].legend(fontsize="x-small")

            for axis in ax:
                axis.axvline(x=peak, color="red", linestyle=":")

            fig.tight_layout(rect=[0, 0, 1, 0.88])
            plt.savefig(f"{out_dir}/{n}_{tag}{suffix}.png", bbox_inches="tight")
            if tag == "04_ifg_residuals":
                plt.savefig("/mn/stornext/d5/data/aimartin/firas-reanalysis/FIRAS-Pass5/src/"
                            f"calibration/output/llss_checks/{n}.png")
            plt.close()

        # A number to go with the pictures: how much of the interferogram each model
        # actually explains.
        print(f"  IFG {n}: rms(original) = {np.sqrt(np.mean(original ** 2)):.4g}")
        for name, result in results.items():
            residual = original - result["ifgs"][n, :]
            print(f"    {name:>10}: rms(original - simulated) = "
                  f"{np.sqrt(np.mean(residual ** 2)):.4g}")
