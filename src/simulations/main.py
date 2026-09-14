"""
Script to simulate the sky as seen by FIRAS, based on given XCAL, ICAL, dihedral, reference and sky horns, bolometer (4 channels) temperatures.
"""

import os
import sys

import matplotlib.pyplot as plt
import numpy as np
from astropy.io import fits

import globals as g
from calibration import bolometer
from pipeline import ifg_spec
from utils import my_utils as utils
from utils.config import gen_nyquistl

modes = {"ss": 0, "lf": 3}
# modes = {"ss": 0}
channels = {"rh": 0, "rl": 1, "lh": 2, "ll": 3}
# channels = {"ll": 3}

# temps = {
#     "xcal": np.array([2.70413828]),
#     "ical": np.array([2.71052694]),
#     "dihedral": np.array([5.0066607]),
#     "refhorn": np.array([2.6955471]),
#     "skyhorn": np.array([2.69618714]),
#     "collimator": np.array([2.69618714]),
#     "bolometer_ll": np.array([1.55804986]),
#     "bolometer_lh": np.array([1.55815428]),
#     "bolometer_rl": np.array([1.54744506]),
#     "bolometer_rh": np.array([1.54760718]),
# }
temps = [
    2.70413828,
    2.71052694,
    5.0066607,
    2.6955471,
    2.69618714,
    2.69618714,
    1.55804986,
]


def safe_divide(numerator, denominator):
    """
    numerator / denominator, and zero wherever the denominator vanishes.

    The plain division would give inf there, and `np.nan_to_num` turns inf into 1.8e308
    rather than into zero, which then poisons everything downstream.
    """
    out = np.zeros(np.broadcast(numerator, denominator).shape, dtype=complex)
    good = denominator != 0
    return np.divide(numerator, denominator, out=out, where=good)


def published_emissivity(fits_data, name, cutoff, length):
    """One emissivity column of the published model, zero-padded to SPEC_SIZE."""
    column = np.zeros(257, dtype=np.complex128)
    column[cutoff : cutoff + length] = (
        fits_data[1].data[f"R{name}"][0] + 1j * fits_data[1].data[f"I{name}"][0]
    )
    return column


def generate_ifg(
    channel,
    mode,
    temps,
    apod,
    adds_per_group=np.array([3]),
    sweeps=np.array([16]),
    bol_cmd_bias=np.array([35]),
    bol_volt=np.array([2.048020362854004]),
    gain=np.array([300]),
    total_spectra=None,
    emiss_xcal=None,
    emiss_ical=None,
    emiss_dihedral=None,
    emiss_refhorn=None,
    emiss_skyhorn=None,
    emiss_collimator=None,
    emiss_bolometer=None,
    emissivities=None,
    Tbol=None,
    bol_params=None,
):
    """
    Forward model one interferogram per set of emitter temperatures.

    The model is  Y = ETF * Bol * FFT[ OTF * P(T_xcal) + sum_{i>0} E_i P(T_i) ],  which
    is built here in exactly that form.  Writing it instead as OTF * (P(T_xcal) + R/OTF)
    is the same thing analytically but not numerically: the OTF is small at the band
    edges and zero outside the band, so dividing by it and multiplying it back blows the
    sum up and then loses it to the nan handling.  The spectrum that is *returned* is
    still the OTF-divided one, since that is what `ifg_to_spec` produces and what
    callers compare against.

    Emissivities can be given either as the individual `emiss_*` arrays or as
    `emissivities` of shape (257, n) whose columns line up with the rows of `temps`.
    """

    fits_data = fits.open(
        f"{g.PUB_MODEL}FIRAS_CALIBRATION_MODEL_{channel.upper()}{mode.upper()}.FITS"
    )

    mtm_speed = 0 if mode[1] == "s" else 1
    if mtm_speed == 0:
        cutoff = 5
    else:
        cutoff = 7
    length = len(fits_data[1].data["RTRANSFE"][0])

    temps = np.asarray(temps)
    if Tbol is None:
        # The channel's own bolometer: the last row of the seven-row form, or this
        # channel's slot in the ten-row form the fit uses.
        Tbol = temps[6 + channels[channel]] if len(temps) > 7 else temps[6]

    if emissivities is not None:
        emissivities = np.asarray(emissivities)
        if emiss_xcal is None:
            emiss_xcal = emissivities[:, 0]
    elif emiss_xcal is None:
        emiss_xcal = published_emissivity(fits_data, "TRANSFE", cutoff, length)

    if total_spectra is None:
        frequency = utils.generate_frequencies(channel, mode, 257)

        print(f"Processing {channel.upper()}{mode.upper()}...")

        if emissivities is not None:
            # sum_{i>0} E_i P(T_i), over as many emitters as were given
            emitted = sum(
                emissivities[:, i] * utils.planck(frequency, np.asarray(temps[i]))
                for i in range(1, emissivities.shape[1])
            )
        else:
            if emiss_ical is None:
                emiss_ical = published_emissivity(fits_data, "ICAL", cutoff, length)
            if emiss_dihedral is None:
                emiss_dihedral = published_emissivity(fits_data, "DIHEDRA", cutoff, length)
            if emiss_refhorn is None:
                emiss_refhorn = published_emissivity(fits_data, "REFHORN", cutoff, length)
            if emiss_skyhorn is None:
                emiss_skyhorn = published_emissivity(fits_data, "SKYHORN", cutoff, length)
            if emiss_collimator is None:
                emiss_collimator = published_emissivity(fits_data, "STRUCTU", cutoff, length)
            if emiss_bolometer is None:
                emiss_bolometer = published_emissivity(fits_data, "BOLOMET", cutoff, length)

            emitted = (
                utils.planck(frequency, np.asarray(temps[1])) * emiss_ical
                + utils.planck(frequency, np.asarray(temps[2])) * emiss_dihedral
                + utils.planck(frequency, np.asarray(temps[3])) * emiss_refhorn
                + utils.planck(frequency, np.asarray(temps[4])) * emiss_skyhorn
                + utils.planck(frequency, np.asarray(temps[5])) * emiss_collimator
                + utils.planck(frequency, np.asarray(temps[6])) * emiss_bolometer
            )

        bb_xcal = utils.planck(frequency, np.asarray(temps[0]))
        # Bin 0 has no Planck function (P(0, T) is 0/0); it carries the interferogram
        # mean, which the model does not predict.
        emitted = np.nan_to_num(emitted, nan=0.0)
        bb_xcal = np.nan_to_num(bb_xcal, nan=0.0)

        # What actually goes into the transform, with no division by the OTF anywhere.
        spec_for_ifg = bb_xcal * emiss_xcal + emitted
        # What callers compare against `ifg_to_spec` output, which is OTF-divided.
        total_spectra = bb_xcal + safe_divide(emitted, emiss_xcal)
    else:
        # A spectrum handed in from outside is in the OTF-divided convention, so put the
        # OTF back to get the same quantity the branch above builds directly.
        total_spectra = np.nan_to_num(total_spectra, nan=0.0)
        spec_for_ifg = total_spectra * emiss_xcal

    fnyq = gen_nyquistl(
        "../reference/fex_samprate.txt", "../reference/fex_nyquist.txt", "int"
    )
    frec = 4 * (channels[channel] % 2) + modes[mode]

    # check for nans in the spectrum that is about to be transformed
    printed_nans_totalspec = np.isnan(spec_for_ifg).sum()
    if printed_nans_totalspec > 0:
        print(f"main: Warning, {printed_nans_totalspec} NaNs in spec_for_ifg")

    ifg = ifg_spec.spec_to_ifg(
        spec=spec_for_ifg,
        channel=channel,
        mode=mode,
        adds_per_group=adds_per_group,
        bol_cmd_bias=bol_cmd_bias / 25.5,  # convert to volts
        bol_volt=bol_volt,
        Tbol=Tbol,
        gain=gain,
        sweeps=sweeps,
        apod=apod,
        # the OTF is already in spec_for_ifg; multiplying again would double-count it
        otf=np.ones(257, dtype=np.complex128),
        fnyq_icm=fnyq["icm"][frec],
        bol_params=bol_params,
    )

    # plt.plot(ifg[0], label=f"{channel.upper()}{mode.upper()} IFG")
    # plt.title(f"{channel.upper()}{mode.upper()} IFG")
    # plt.xlabel("Sample")
    # plt.ylabel("Amplitude")
    # plt.legend()
    # plt.grid()
    # plt.show()
    print(f"IFG for {channel.upper()}{mode.upper()} generated.")

    return ifg, total_spectra
    # return ifg, noised_spec, bb_xcal


if __name__ == "__main__":
    for channel in channels.keys():
        for mode in modes.keys():
            if not (mode == "lf" and (channel == "lh" or channel == "rh")):
                ifg, total_spectra, xcal_spec = generate_ifg(channel, mode, temps)

                # save this ifg to a file
                np.save("/simulations/output/ifgsim.npy", ifg)

                plt.plot((ifg[0]), label=f"{channel.upper()}{mode.upper()} IFG")
                plt.title(f"{channel.upper()}{mode.upper()} IFG")
                plt.xlabel("Sample")
                plt.ylabel("Amplitude")
                plt.legend()
                plt.grid()
                plt.show()

                plt.plot(
                    np.abs(total_spectra[0]),
                    label=f"{channel.upper()}{mode.upper()} Total Spectra",
                )
                plt.title(f"{channel.upper()}{mode.upper()} Total Spectra")
                plt.xlabel("Frequency (GHz)")
                plt.ylabel("Brightness Temperature (K)")
                plt.legend()
                plt.grid()
                plt.show()
