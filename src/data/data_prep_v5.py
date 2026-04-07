"""
So for this version we will keep each channel separate and use the midpoint_time to interpolate to get the temperatures.
"""

import astropy.units as u
import h5py
import matplotlib

import data.utils.my_utils as data_utils
import globals as g

matplotlib.use('Agg')  # Use non-interactive backend
import os
import time
from functools import partial
from multiprocessing import Pool

import matplotlib.pyplot as plt
import numpy as np

from data import stats
from engineering_timing.get_interpolated_times import get_data, get_interp

start_time = time.time()

# Pre-load plateau divides to avoid repeated file reads
plateau_divides_cache = {}
with open(f"data/plateau_divides.txt", "r") as f:
    lines = f.readlines()
    for line in lines:
        parts = line.strip().split(" ")
        if len(parts) >= 2:
            name = parts[0]
            plateau_divides_cache[name] = np.array(parts[1].split(",")).astype(float)

# OPENING ORIGINAL DATA FILES
fdq_sdf = h5py.File("/mn/stornext/d16/cmbco/ola/firas/initial_data/fdq_sdf_new.h5", "r")
fdq_eng = h5py.File("/mn/stornext/d16/cmbco/ola/firas/initial_data/fdq_eng_new.h5", "r")

# Longitudes and latitudes are stored in radians*1e4
fact = 180.0 / np.pi / 1e4

ct_head = fdq_eng["ct_head"]
eng_time = np.sort(ct_head["time"][:])
eng_time_gmt = data_utils.binary_to_gmt(eng_time)
eng_time_s = (eng_time * (100 * u.ns)).to("s")

en_stat = fdq_eng["en_stat"]
# side a
stat_word_1 = en_stat["stat_word_1"][:]
stat_word_4 = en_stat["stat_word_4"][:]
stat_word_5 = en_stat["stat_word_5"][:]
stat_word_8 = en_stat["stat_word_8"][:]
lvdt_stat_a = en_stat["lvdt_stat"][:, 0]
# side b
stat_word_9 = en_stat["stat_word_9"][:]
stat_word_12 = en_stat["stat_word_12"][:]
stat_word_13 = en_stat["stat_word_13"][:]
stat_word_16 = en_stat["stat_word_16"][:]
lvdt_stat_b = en_stat["lvdt_stat"][:, 1]

en_analog = fdq_eng["en_analog"]
grt = en_analog["grt"]
grts, grt_times = get_data()
interpolators = get_interp(grts, grt_times)

group1 = en_analog["group1"]

chan = fdq_eng["chan"]
all_data = {}
cal_data = {}
sky_data = {}

mu = {}
mu_err = {}
avg_temp = {}
temp_err = {}
temps = {}
elements = ["ical", "xcal_cone", "refhorn", "skyhorn"]
sides = ["a", "b"]

cal_mask = {}
sky_mask = {}
midpoint_time_s = {}

for channel, channel_i in g.CHANNELS.items():
    science_data = fdq_sdf[f"fdq_sdf_{channel}"]

    sci_head = science_data["sci_head"]
    all_data[f"mtm_speed_{channel}"] = sci_head["mtm_speed"][:]
    all_data[f"mtm_length_{channel}"] = sci_head["mtm_length"][:]
    all_data[f"upmode_{channel}"] = sci_head["sc_head1a"][:]
    all_data[f"adds_per_group_{channel}"] = sci_head["sc_head9"][:]
    all_data[f"sweeps_{channel}"] = sci_head["sc_head11"][:]
    all_data[f"saturated_{channel}"] = sci_head["sc_head20"][:]

    ifg_data = science_data["ifg_data"]
    all_data[f"ifg_{channel}"] = ifg_data["ifg"][:]

    dq_data = science_data["dq_data"]
    all_data[f"fake_{channel}"] = dq_data["fake"][:]
    all_data[f"xcal_pos_{channel}"] = dq_data["xcal_pos"][:]
    all_data[f"data_quality_{channel}"] = dq_data["data_quality"][:]
    all_data[f"eng_time_{channel}"] = np.sort(dq_data["eng_time"][:])

    collect_time = science_data["collect_time"]
    all_data[f"midpoint_time_{channel}"] = np.sort(collect_time["midpoint_time"][:])
    all_data[f"midpoint_time_s_{channel}"] = (collect_time["midpoint_time"][:] * (100 * u.ns)).to("s")
    all_data[f"midpoint_time_gmt_{channel}"] = data_utils.binary_to_gmt(collect_time["midpoint_time"][:])

    attitude = science_data["attitude"]
    all_data[f"cel_lon_{channel}"] = attitude["ra"][:] * fact
    all_data[f"cel_lat_{channel}"] = attitude["dec"][:] * fact
    all_data[f"terr_lat_{channel}"] = attitude["terr_latitude"][:] * fact
    all_data[f"terr_lon_{channel}"] = attitude["terr_longitude"][:] * fact
    all_data[f"earth_limb_{channel}"] = attitude["earth_limb"][:] * fact
    all_data[f"sun_angle_{channel}"] = attitude["sun_angle"][:] * fact
    all_data[f"moon_angle_{channel}"] = attitude["moon_angle"][:] * fact
    all_data[f"gal_lon_{channel}"] = attitude["galactic_longitude"][:] * fact
    all_data[f"gal_lat_{channel}"] = attitude["galactic_latitude"][:] * fact
    all_data[f"ecl_lon_{channel}"] = attitude["ecliptic_longitude"][:] * fact
    all_data[f"ecl_lat_{channel}"] = attitude["ecliptic_latitude"][:] * fact
    all_data[f"solution_{channel}"] = attitude["solution"][:]

    print(f"\n\nChannel: {channel.upper()}")

    stats.table3_4(all_data[f"xcal_pos_{channel}"], all_data[f"mtm_length_{channel}"], all_data[f"mtm_speed_{channel}"])

    fpp_fail, fakeit, xcal_transit = stats.table4_1(
        all_data[f"data_quality_{channel}"], all_data[f"fake_{channel}"], all_data[f"xcal_pos_{channel}"]
    )

    for key in all_data:
        if key.endswith(f"_{channel}"):
            all_data[key] = all_data[key][fpp_fail == False]
            all_data[key] = all_data[key][fakeit == False]
            all_data[key] = all_data[key][xcal_transit == False]

    
    cal_mask[channel] = all_data[f"xcal_pos_{channel}"] == 1
    sky_mask[channel] = all_data[f"xcal_pos_{channel}"] == 2
    for key in all_data:
        if key.endswith(f"_{channel}"):
            cal_data[key] = all_data[key][cal_mask[channel]]
            sky_data[key] = all_data[key][sky_mask[channel]]

    (
        cal_saturated,
        cal_sw,
        cal_glitch_rate,
        sky_saturated,
        sky_sw,
        sky_glitch_rate,
        limb,
        no_solution,
    ) = stats.table4_2(
        cal_data[f"saturated_{channel}"],
        cal_data[f"data_quality_{channel}"],
        sky_data[f"saturated_{channel}"],
        sky_data[f"data_quality_{channel}"],
        sky_data[f"solution_{channel}"],
    )

    # Combine cuts before applying
    cal_cuts = ~(cal_saturated | cal_sw | cal_glitch_rate)
    sky_cuts = ~(sky_saturated | sky_sw | sky_glitch_rate | limb | no_solution)

    for key in all_data:
        if key.endswith(f"_{channel}"):
            cal_data[key] = cal_data[key][cal_cuts]
            sky_data[key] = sky_data[key][sky_cuts]

    # the next table to reproduce needs the ICAL temperatures so we need to match them now
    # Interpolate temperatures for sky data
    # the dihdral operates at different temperatures and thus we use a different method for it
    # same for the collimator

    print(f"Testing new way for de-biasing temperatures for ICAL and using the previous one for the rest")
    print(f"Taking the average of both sides")

    # join calibration and sky times for interpolation (optimized)
    print("Joining and sorting calibration and sky times for interpolation")
    midpoint_time_s[channel] = np.concatenate([cal_data[f"midpoint_time_s_{channel}"],
                                     sky_data[f"midpoint_time_s_{channel}"]])
    xcal_pos = np.concatenate([cal_data[f"xcal_pos_{channel}"], sky_data[f"xcal_pos_{channel}"]])
    
    sorted_indices = np.argsort(midpoint_time_s[channel])
    midpoint_time_s[channel] = midpoint_time_s[channel][sorted_indices]
    xcal_pos = xcal_pos[sorted_indices]
    
    # Batch interpolatetemperatures for all elements and sides
    print("Batch interpolating temperatures...")
    
    fig, ax = plt.subplots(figsize=(10, 6))
    
    for element in elements:
        for side in sides:
            thermometerid = elements.index(element) + sides.index(side) * 10
            print(f"Interpolating {element} temperatures for side {side.upper()} -----------------")
            temps[f"{side}_hi_{element}_{channel}"] = interpolators[f"{side}_hi_{element}"](midpoint_time_s[channel])
            temps[f"{side}_lo_{element}_{channel}"] = interpolators[f"{side}_lo_{element}"](midpoint_time_s[channel])

            if element == "xcal_cone":
                cal_mask[channel] = xcal_pos == 1
                sky_mask[channel] = xcal_pos == 2
                temps[f"{side}_hi_{element}_{channel}"] = temps[f"{side}_hi_{element}_{channel}"][cal_mask[channel]]
                temps[f"{side}_lo_{element}_{channel}"] = temps[f"{side}_lo_{element}_{channel}"][cal_mask[channel]]

            print(f"Dividing {side.upper()} side ICAL temperatures into plateaus to try to de-bias them")
            plateau_masks = stats.divide_plateaus(temps[f"{side}_lo_{element}_{channel}"],
                                                    channel, element, side, plateau_divides_cache)

            n_plateaus = len(plateau_masks)
            keyword = f"{side}_{element}_{channel}"
            mu[keyword] = np.zeros(n_plateaus)
            mu_err[keyword] = np.zeros(n_plateaus)
            avg_temp[keyword] = np.zeros(n_plateaus)
            temp_err[keyword] = np.zeros(n_plateaus)

            # Fit gaussians for each plateau
            for i, mask in enumerate(plateau_masks):
                output = stats.fit_gaussian(temps[f"{side}_hi_{element}_{channel}"][mask],
                                            temps[f"{side}_lo_{element}_{channel}"][mask], channel,
                                            element, side, sigma=1, plateau=i+1)

                mu[keyword][i] = output[0]
                mu_err[keyword][i] = output[1]
                avg_temp[keyword][i] = output[2]
                temp_err[keyword][i] = output[3]
                
                # Only add label for the first point of each element/side combination
                label = f"{element} ({side.upper()})" if i == 0 else None
                ax.errorbar(avg_temp[keyword][i], mu[keyword][i], xerr=temp_err[keyword][i],
                            yerr=mu_err[keyword][i], fmt='o', color=f'C{thermometerid}',
                            label=label, markersize=6, capsize=3)

# join the results for the same thermometer in all of the channels
beta = {}
for element in elements:
    for side in sides:
        mu_all = np.concatenate([mu[f"{side}_{element}_{channel}"] for channel in g.CHANNELS])
        mu_err_all = np.concatenate([mu_err[f"{side}_{element}_{channel}"] for channel in g.CHANNELS])
        avg_temp_all = np.concatenate([avg_temp[f"{side}_{element}_{channel}"] for channel in g.CHANNELS])
        temp_err_all = np.concatenate([temp_err[f"{side}_{element}_{channel}"] for channel in g.CHANNELS])
        beta[f"{element}_{side}"] = stats.selfheat_vs_temp(mu_all, mu_err_all, avg_temp_all, temp_err_all, element, side)
        # print(f"Fitted self-heating for {element} {side.upper()}: beta={beta:.6f}")

for channel, channel_i in g.CHANNELS.items():
    for element in elements:
        for side in sides:
            temps[f"{side}_{element}_{channel}"] = stats.debiase_hi(beta[f"{element}_{side}"],
                                                   temps[f"{side}_hi_{element}_{channel}"],
                                                   temps[f"{side}_lo_{element}_{channel}"], element,
                                                   side, channel)

        all_data[f"{element}_{channel}"] = (temps[f"a_{element}_{channel}"] +
                                            temps[f"b_{element}_{channel}"]) / 2.0

        if element == "xcal_cone":
            cal_data[f"{element}_{channel}"] = all_data[f"{element}_{channel}"]
        else:
            # split back into cal and sky data
            cal_data[f"{element}_{channel}"] = all_data[f"{element}_{channel}"][cal_mask[channel]]
            sky_data[f"{element}_{channel}"] = all_data[f"{element}_{channel}"][sky_mask[channel]]

    temps[f"a_lo_dihedral"] = interpolators[f"a_lo_dihedral"](midpoint_time_s[channel])
    temps[f"b_lo_dihedral"] = interpolators[f"b_lo_dihedral"](midpoint_time_s[channel])
    all_data[f"dihedral_{channel}"] = (temps[f"a_lo_dihedral"] + temps[f"b_lo_dihedral"]) / 2.0

    temps[f"a_lo_collimator"] = interpolators[f"a_lo_collimator"](midpoint_time_s[channel])
    all_data[f"collimator_{channel}"] = temps[f"a_lo_collimator"]

    for element in ["dihedral", "collimator"]:
        cal_data[f"{element}_{channel}"] = all_data[f"{element}_{channel}"][cal_mask[channel]]
        sky_data[f"{element}_{channel}"] = all_data[f"{element}_{channel}"][sky_mask[channel]]

    # Finalize and save the plot with all points
    ax.set_xlabel("Average High Current Temperature (K)")
    ax.set_ylabel("Fitted Gaussian Mean of High-Low Difference (K)")
    ax.set_title(f"Self-Heating vs Temperature for All Elements - Channel {channel.upper()}")
    ax.legend()
    fig.tight_layout()
    fig.savefig(f'data/output/all_points_{channel}.png', dpi=150)
    plt.close(fig)

    (
        earth_limb,
        sun_angle,
        wrong_sci_mode,
    ) = stats.table4_5(
        sky_data[f"earth_limb_{channel}"],
        cal_data[f"midpoint_time_gmt_{channel}"],
        sky_data[f"midpoint_time_gmt_{channel}"],
        cal_data[f"ical_{channel}"],
        sky_data[f"ical_{channel}"],
        sky_data[f"sun_angle_{channel}"],
        sky_data[f"upmode_{channel}"],
        cal_data[f"dihedral_{channel}"],
        sky_data[f"dihedral_{channel}"],
    )

    print("Ignoring official temperature cuts...")

    sky_cuts = (
        earth_limb
        | sun_angle
        | wrong_sci_mode
    )
    for key in sky_data:
        if key.endswith(f"_{channel}"):
            sky_data[key] = sky_data[key][~sky_cuts]

    # engineering data based on channels
    bol_cmd_bias = en_stat["bol_cmd_bias"][:, channel_i]
    bol_volt = group1["bol_volt"][:, channel_i]
    eng_upmode = chan["up_sci_mode"][:, channel_i]
    eng_fake = chan["fakeit"][:, channel_i]
    eng_adds_per_group = chan["up_adds_per_group"][:, channel_i]
    eng_sweeps = chan["up_swps_per_ifg"][:, channel_i]
    eng_mtm_speed = chan["xmit_mtm_speed"][:, channel_i]
    eng_mtm_length = chan["xmit_mtm_len"][:, channel_i]
    eng_gain = chan["sci_gain"][:, channel_i].astype(int)

    # Helper function to find nearest neighbors - optimized
    def find_nearest_neighbors(times_gmt, eng_time_gmt):
        """Find two nearest engineering time neighbors for each science time."""
        indices = np.searchsorted(eng_time_gmt, times_gmt)
        indices = np.clip(indices, 1, len(eng_time_gmt) - 1)
        
        idx_before = indices - 1
        idx_after = indices
        
        dist_before = np.abs(times_gmt - eng_time_gmt[idx_before])
        dist_after = np.abs(times_gmt - eng_time_gmt[idx_after])
        
        idx0 = np.where(dist_before <= dist_after, idx_before, idx_after)
        idx1 = np.where(dist_before <= dist_after, idx_after, idx_before)
        return idx0, idx1

    # Find nearest neighbors for both cal and sky data
    idx0_cal, idx1_cal = find_nearest_neighbors(cal_data[f"midpoint_time_gmt_{channel}"], eng_time_gmt)
    idx0_sky, idx1_sky = find_nearest_neighbors(sky_data[f"midpoint_time_gmt_{channel}"], eng_time_gmt)

    print("\nOther data cuts")

    # cal data
    bol_cmd_bias_mismatch_cal = (bol_cmd_bias[idx0_cal] != bol_cmd_bias[idx1_cal]) | (
        bol_cmd_bias[idx0_cal] <= 0
    )
    cal_data[f"bol_cmd_bias_{channel}"] = np.where(
        ~bol_cmd_bias_mismatch_cal,
        bol_cmd_bias[idx0_cal],
        np.nan
    )
    upmode_mismatch_cal = (eng_upmode[idx0_cal] != eng_upmode[idx1_cal]) | (
        eng_upmode[idx0_cal] != cal_data[f"upmode_{channel}"]
    )
    fakeit_mismatch_cal = (eng_fake[idx0_cal] != eng_fake[idx1_cal]) | (
        eng_fake[idx0_cal] != cal_data[f"fake_{channel}"]
    )
    adds_per_group_mismatch_cal = (
        eng_adds_per_group[idx0_cal] != eng_adds_per_group[idx1_cal]
    ) | (eng_adds_per_group[idx0_cal] != cal_data[f"adds_per_group_{channel}"])
    sweeps_mismatch_cal = (
        (eng_sweeps[idx0_cal] != eng_sweeps[idx1_cal])
        | (eng_sweeps[idx0_cal] != cal_data[f"sweeps_{channel}"])
        | (cal_data[f"sweeps_{channel}"] == 1)
    )
    mtm_length_mismatch_cal = (eng_mtm_length[idx0_cal] != eng_mtm_length[idx1_cal]) | (
        eng_mtm_length[idx0_cal] != cal_data[f"mtm_length_{channel}"]
    )
    mtm_speed_mismatch_cal = (eng_mtm_speed[idx0_cal] != eng_mtm_speed[idx1_cal]) | (
        eng_mtm_speed[idx0_cal] != cal_data[f"mtm_speed_{channel}"]
    )
    gain_mismatch_cal = (eng_gain[idx0_cal] != eng_gain[idx1_cal]) | (
        eng_gain[idx0_cal] == 0
    )
    cal_data[f"gain_{channel}"] = np.where(~gain_mismatch_cal, eng_gain[idx0_cal], np.nan)

    # Vectorized status word processing for calibration data
    stat_words_cal = [
        (stat_word_1, "stat_word_1"),
        (stat_word_4, "stat_word_4"),
        (stat_word_5, "stat_word_5"),
        (stat_word_8, "stat_word_8"),
        (stat_word_9, "stat_word_9"),
        (stat_word_12, "stat_word_12"),
        (stat_word_13, "stat_word_13"),
        (stat_word_16, "stat_word_16"),
        (lvdt_stat_a, "lvdt_stat_a"),
        (lvdt_stat_b, "lvdt_stat_b"),
    ]
    
    sw_mismatches_cal = np.zeros(len(idx0_cal), dtype=bool)
    for stat_array, key in stat_words_cal:
        mismatch = stat_array[idx0_cal] != stat_array[idx1_cal]
        cal_data[f"{key}_{channel}"] = np.where(~mismatch, stat_array[idx0_cal], np.nan)
        sw_mismatches_cal |= mismatch
    
    other_cuts_cal = ~(
        bol_cmd_bias_mismatch_cal
        | upmode_mismatch_cal
        | fakeit_mismatch_cal
        | adds_per_group_mismatch_cal
        | sweeps_mismatch_cal
        | mtm_length_mismatch_cal
        | mtm_speed_mismatch_cal
        | gain_mismatch_cal
        | sw_mismatches_cal
    )
    for key in cal_data:
        if key.endswith(f"_{channel}"):
            cal_data[key] = cal_data[key][other_cuts_cal]

    # sky data
    # Check if the two closest values match
    bol_cmd_bias_mismatch = (bol_cmd_bias[idx0_sky] != bol_cmd_bias[idx1_sky]) | (
        bol_cmd_bias[idx0_sky] <= 0
    )
    print(f"    bol_cmd_bias mismatch: {(bol_cmd_bias_mismatch).sum()}")
    # Assign values where they match
    sky_data[f"bol_cmd_bias_{channel}"] = np.where(
        ~bol_cmd_bias_mismatch, bol_cmd_bias[idx0_sky], np.nan
    )

    # compare between id1 and idx2 but also with the one from the science data
    upmode_mismatch = (eng_upmode[idx0_sky] != eng_upmode[idx1_sky]) | (
        eng_upmode[idx0_sky] != sky_data[f"upmode_{channel}"]
    )
    print(f"    upmode mismatch: {(upmode_mismatch).sum()}")

    fakeit_mismatch = (eng_fake[idx0_sky] != eng_fake[idx1_sky]) | (
        eng_fake[idx0_sky] != sky_data[f"fake_{channel}"]
    )
    print(f"    fakeit mismatch: {(fakeit_mismatch).sum()}")

    adds_per_group_mismatch = (
        eng_adds_per_group[idx0_sky] != eng_adds_per_group[idx1_sky]
    ) | (eng_adds_per_group[idx0_sky] != sky_data[f"adds_per_group_{channel}"])
    print(f"    adds_per_group mismatch: {(adds_per_group_mismatch).sum()}")

    sweeps_mismatch = (
        (eng_sweeps[idx0_sky] != eng_sweeps[idx1_sky])
        | (eng_sweeps[idx0_sky] != sky_data[f"sweeps_{channel}"])
        | (sky_data[f"sweeps_{channel}"] == 1)
    )
    print(f"    sweeps mismatch: {(sweeps_mismatch).sum()}")

    mtm_length_mismatch = (eng_mtm_length[idx0_sky] != eng_mtm_length[idx1_sky]) | (
        eng_mtm_length[idx0_sky] != sky_data[f"mtm_length_{channel}"]
    )
    print(f"    mtm_length mismatch: {(mtm_length_mismatch).sum()}")

    mtm_speed_mismatch = (eng_mtm_speed[idx0_sky] != eng_mtm_speed[idx1_sky]) | (
        eng_mtm_speed[idx0_sky] != sky_data[f"mtm_speed_{channel}"]
    )
    print(f"    mtm_speed mismatch: {(mtm_speed_mismatch).sum()}")

    # let's try to simply use the values from engineering for the gain
    # bad_gain = (sky_data["gain"] == np.nan) != (eng_gain[idx0] == eng_gain[idx1]) | (
    #     eng_gain[idx0] != sky_data["gain"]
    # )
    gain_mismatch = (eng_gain[idx0_sky] != eng_gain[idx1_sky]) | (
        eng_gain[idx0_sky] == 0
    )
    print(f"    Gain Mismatch: {gain_mismatch.sum()}")
    sky_data[f"gain_{channel}"] = np.where(~gain_mismatch, eng_gain[idx0_sky], np.nan)

    moon_contamination = sky_data[f"moon_angle_{channel}"] <= 22.0
    print(f"    Moon Angle <= 22.0: {moon_contamination.sum()}")

    # Vectorized status word processing for sky data
    sw_mismatches = np.zeros(len(idx0_sky), dtype=bool)
    for stat_array, key in stat_words_cal:  # Reuse the same list
        mismatch = stat_array[idx0_sky] != stat_array[idx1_sky]
        sky_data[f"{key}_{channel}"] = np.where(~mismatch, stat_array[idx0_sky], np.nan)
        sw_mismatches |= mismatch
    print(f"    Status Word Mismatches: {sw_mismatches.sum()}")

    other_cuts = ~(
        bol_cmd_bias_mismatch
        | upmode_mismatch
        | fakeit_mismatch
        | adds_per_group_mismatch
        | sweeps_mismatch
        | mtm_length_mismatch
        | mtm_speed_mismatch
        | gain_mismatch
        | moon_contamination
        | sw_mismatches
    )
    print(f"    Total Sky Records Failed Other Cuts: {(~other_cuts).sum()}")
    print(f"    Remaining Sky Records: {other_cuts.sum()}")
    for key in sky_data:
        if key.endswith(f"_{channel}"):
            sky_data[key] = sky_data[key][other_cuts]

    cal_data[f"bol_volt_{channel}"] = np.interp(cal_data[f"midpoint_time_s_{channel}"], eng_time_s,
                                                bol_volt)
    sky_data[f"bol_volt_{channel}"] = np.interp(sky_data[f"midpoint_time_s_{channel}"], eng_time_s,
                                                bol_volt)

    # no high current readings for bolometers - vectorized
    a_lo_bol_assem = grt["a_lo_bol_assem"][:, channel_i]
    b_lo_bol_assem = grt["b_lo_bol_assem"][:, channel_i]
    bol_assem = (a_lo_bol_assem + b_lo_bol_assem) * 0.5  # Slightly faster than /2.0
    
    cal_data[f"bolometer_{channel}"] = np.interp(cal_data[f"midpoint_time_s_{channel}"], eng_time_s,
                                                 bol_assem)
    good_bolometer_cal = cal_data[f"bolometer_{channel}"] > 0.0
    for key in cal_data:
        if key.endswith(f"_{channel}"):
            cal_data[key] = cal_data[key][good_bolometer_cal]
    
    sky_data[f"bolometer_{channel}"] = np.interp(sky_data[f"midpoint_time_s_{channel}"], eng_time_s,
                                                 bol_assem)
    good_bolometer_sky = sky_data[f"bolometer_{channel}"] > 0.0
    print(f"    Bad Bolometer Temperature Readings: {(~good_bolometer_sky).sum()}")
    for key in sky_data:
        if key.endswith(f"_{channel}"):
            sky_data[key] = sky_data[key][good_bolometer_sky]
    
    channel_time = time.time() - start_time
    print(f"Channel {channel.upper()} completed in {channel_time:.2f} seconds")

# save with compression
np.savez_compressed(
    f"{g.PREPROCESSED_DATA_PATH}/sky.npz",
    **sky_data,
)
np.savez_compressed(
    f"{g.PREPROCESSED_DATA_PATH}/cal.npz",
    **cal_data,
)

elapsed_time = time.time() - start_time
print(f"\n\nAll channels processed successfully!")
print(f"Total time: {elapsed_time:.2f} seconds ({elapsed_time/60:.2f} minutes)")
print(f"\n\nAll channels processed successfully!")
print(f"Total time: {elapsed_time:.2f} seconds ({elapsed_time/60:.2f} minutes)")
