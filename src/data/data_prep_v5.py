"""
So for this version we will keep each channel separate and use the midpoint_time to interpolate to get the temperatures.
"""

import astropy.units as u
import data.utils.my_utils as data_utils
import globals as g
import h5py
import matplotlib

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
# remove nans from grts and corresponding times
# valid_indices = ~np.isnan(grts).any(axis=1)
# grts = grts[valid_indices]
# grt_times = grt_times[valid_indices]

group1 = en_analog["group1"]

chan = fdq_eng["chan"]

for channel, channel_i in g.CHANNELS.items():
    science_data = fdq_sdf[f"fdq_sdf_{channel}"]

    all_data = {}

    sci_head = science_data["sci_head"]
    all_data["mtm_speed"] = sci_head["mtm_speed"][:]
    all_data["mtm_length"] = sci_head["mtm_length"][:]
    all_data["upmode"] = sci_head["sc_head1a"][:]
    all_data["adds_per_group"] = sci_head["sc_head9"][:]
    all_data["sweeps"] = sci_head["sc_head11"][:]
    all_data["saturated"] = sci_head["sc_head20"][:]

    ifg_data = science_data["ifg_data"]
    all_data["ifg"] = ifg_data["ifg"][:]

    dq_data = science_data["dq_data"]
    all_data["fake"] = dq_data["fake"][:]
    all_data["xcal_pos"] = dq_data["xcal_pos"][:]
    all_data["data_quality"] = dq_data["data_quality"][:]
    all_data["eng_time"] = np.sort(dq_data["eng_time"][:])

    collect_time = science_data["collect_time"]
    all_data["midpoint_time"] = np.sort(collect_time["midpoint_time"][:])
    all_data["midpoint_time_s"] = (collect_time["midpoint_time"][:] * (100 * u.ns)).to("s")
    all_data["midpoint_time_gmt"] = data_utils.binary_to_gmt(collect_time["midpoint_time"][:])

    attitude = science_data["attitude"]
    all_data["cel_lon"] = attitude["ra"][:] * fact
    all_data["cel_lat"] = attitude["dec"][:] * fact
    all_data["terr_lat"] = attitude["terr_latitude"][:] * fact
    all_data["terr_lon"] = attitude["terr_longitude"][:] * fact
    all_data["earth_limb"] = attitude["earth_limb"][:] * fact
    all_data["sun_angle"] = attitude["sun_angle"][:] * fact
    all_data["moon_angle"] = attitude["moon_angle"][:] * fact
    all_data["gal_lon"] = attitude["galactic_longitude"][:] * fact
    all_data["gal_lat"] = attitude["galactic_latitude"][:] * fact
    all_data["ecl_lon"] = attitude["ecliptic_longitude"][:] * fact
    all_data["ecl_lat"] = attitude["ecliptic_latitude"][:] * fact
    all_data["solution"] = attitude["solution"][:]

    print(f"\n\nChannel: {channel.upper()}")

    stats.table3_4(all_data["xcal_pos"], all_data["mtm_length"], all_data["mtm_speed"])

    fpp_fail, fakeit, xcal_transit = stats.table4_1(
        all_data["data_quality"], all_data["fake"], all_data["xcal_pos"]
    )

    for key in all_data:
        all_data[key] = all_data[key][fpp_fail == False]
        all_data[key] = all_data[key][fakeit == False]
        all_data[key] = all_data[key][xcal_transit == False]

    cal_data = {}
    sky_data = {}
    # Optimized: split data using vectorized indexing
    cal_mask = all_data["xcal_pos"] == 1
    sky_mask = all_data["xcal_pos"] == 2
    for key in all_data:
        cal_data[key] = all_data[key][cal_mask]
        sky_data[key] = all_data[key][sky_mask]

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
        cal_data["saturated"],
        cal_data["data_quality"],
        sky_data["saturated"],
        sky_data["data_quality"],
        sky_data["solution"],
    )

    # Combine cuts before applying
    cal_cuts = ~(cal_saturated | cal_sw | cal_glitch_rate)
    sky_cuts = ~(sky_saturated | sky_sw | sky_glitch_rate | limb | no_solution)

    for key in all_data:
        cal_data[key] = cal_data[key][cal_cuts]
        sky_data[key] = sky_data[key][sky_cuts]

    # the next table to reproduce needs the ICAL temperatures so we need to match them now
    # Interpolate temperatures for sky data
    elements = ["ical", "xcal_cone", "refhorn", "skyhorn", "dihedral", "collimator"]
    sides = ["a", "b"]

    print(f"Testing new way for de-biasing temperatures for ICAL and using the previous one for the rest")
    print(f"Taking the average of both sides")
    temps = {}

    # join calibration and sky times for interpolation (optimized)
    print("Joining and sorting calibration and sky times for interpolation")
    midpoint_time_s = np.concatenate([cal_data["midpoint_time_s"], sky_data["midpoint_time_s"]])
    xcal_pos = np.concatenate([cal_data["xcal_pos"], sky_data["xcal_pos"]])
    
    sorted_indices = np.argsort(midpoint_time_s)
    midpoint_time_s = midpoint_time_s[sorted_indices]
    xcal_pos = xcal_pos[sorted_indices]
    
    # Batch interpolatetemperatures for all elements and sides
    print("Batch interpolating temperatures...")
    for element in elements:
        for side in sides:
            if element == "collimator" and side == "b":
                continue
            print(f"Interpolating {element} temperatures for side {side.upper()} -----------------")
            temps[f"{side}_hi_{element}"] = interpolators[f"{side}_hi_{element}"](midpoint_time_s)
            temps[f"{side}_lo_{element}"] = interpolators[f"{side}_lo_{element}"](midpoint_time_s)

            # TODO: working on changing this to new de-biased temps
            # if element == "ical" or element == "xcal_cone":
            if element == "xcal_cone":
                xcal_mask = xcal_pos == 1
                temps[f"{side}_hi_{element}"] = temps[f"{side}_hi_{element}"][xcal_mask]
                temps[f"{side}_lo_{element}"] = temps[f"{side}_lo_{element}"][xcal_mask]
            
            print(f"Dividing {side.upper()} side ICAL temperatures into plateaus to try to de-bias them")
            plateau_masks = stats.divide_plateaus(temps[f"{side}_lo_{element}"],
                                                    channel, element, side, plateau_divides_cache)

            n_plateaus = len(plateau_masks)
            mu = np.zeros(n_plateaus)
            mu_err = np.zeros(n_plateaus)
            avg_temp = np.zeros(n_plateaus)
            temp_err = np.zeros(n_plateaus)

            # Fit gaussians for each plateau
            for i, mask in enumerate(plateau_masks):
                mu[i], mu_err[i], avg_temp[i], temp_err[i] = stats.fit_gaussian(
                    temps[f"{side}_hi_{element}"][mask],
                    temps[f"{side}_lo_{element}"][mask],
                    channel, element, side,
                    sigma=1, plateau=i+1)
                plt.errorbar(avg_temp[i], mu[i], xerr=temp_err[i], yerr=mu_err[i], fmt='o')
                
            beta = stats.selfheat_vs_temp(mu, mu_err, avg_temp, temp_err, element, side)
            temps[f"{side}_{element}"] = stats.debiase_hi(beta,
                                                            temps[f"{side}_hi_{element}"],
                                                            temps[f"{side}_lo_{element}"],
                                                            element, side, channel)

            # else:
            #     # Vectorized temperature selection using the temps from the original pipeline       
            #     temps[f"{side}_{element}"] = data_utils.get_temperature_hl_vectorized(
            #         temps[f"{side}_lo_{element}"],
            #         temps[f"{side}_hi_{element}"],
            #         element,
            #         side,
            #     )

        if element == "collimator":
            all_data[element] = temps[f"a_{element}"]
        else:
            all_data[element] = (temps[f"a_{element}"] + temps[f"b_{element}"]) / 2.0

        if element == "xcal_cone":
            cal_data[element] = all_data[element]
        else:
            # split back into cal and sky data
            cal_data[element] = all_data[element][xcal_pos == 1]
            sky_data[element] = all_data[element][xcal_pos == 2]

    # plot all of the points
    plt.savefig('data/output/all_points.png')
    plt.close()

    # collimator_hi = interpolators["a_hi_collimator"](midpoint_time_s)
    # collimator_lo = interpolators["a_lo_collimator"](midpoint_time_s)
    # all_data = ((collimator_hi + collimator_lo) / 2.0)

    # cal_data["collimator"] = all_data[xcal_pos == 1]
    # sky_data["collimator"] = all_data[xcal_pos == 2]

    # collimator_hi = interpolators["a_hi_collimator"](sky_data["midpoint_time_s"])
    # collimator_lo = interpolators["a_lo_collimator"](sky_data["midpoint_time_s"])
    # sky_data["collimator"] = ((collimator_hi + collimator_lo) / 2.0)[temp_mask_sky["a"] & temp_mask_sky["b"]]
    # apply temp masks to cal and sky data except for elements and collimator
    # for key in cal_data:
    #     if key not in elements and key != "collimator":
    #         # print(f"DEBUG key: {key}. Shape before masking: cal_data {cal_data[key].shape}, sky_data {sky_data[key].shape}")
    #         # Use concatenate with axis=0 for 2D arrays, append for 1D
    #         if cal_data[key].ndim > 1:
    #             all_data = np.concatenate([cal_data[key], sky_data[key]], axis=0)
    #         else:
    #             all_data = np.append(cal_data[key], sky_data[key])
    #         all_data = all_data[sorted_indices]
    #         all_data = all_data
    #         cal_data[key] = all_data[xcal_pos == 1]
    #         sky_data[key] = all_data[xcal_pos == 2]
            # print(f"DEBUG key: {key}. Shape after masking: cal_data {cal_data[key].shape}, sky_data {sky_data[key].shape}")
    (
        earth_limb,
        # wrong_ical_temp_cal,
        # wrong_ical_temp_sky,
        sun_angle,
        wrong_sci_mode,
        # dihedral_temp_cal,
        # dihedral_temp_sky,
    ) = stats.table4_5(
        sky_data["earth_limb"],
        cal_data["midpoint_time_gmt"],
        sky_data["midpoint_time_gmt"],
        cal_data["ical"],
        sky_data["ical"],
        sky_data["sun_angle"],
        sky_data["upmode"],
        cal_data["dihedral"],
        sky_data["dihedral"],
    )

    print("Ignoring official temperature cuts...")

    # cal_cuts = wrong_ical_temp_cal | dihedral_temp_cal
    # for key in cal_data:
    #     cal_data[key] = cal_data[key][cal_cuts == False]
    sky_cuts = (
        earth_limb
        # | wrong_ical_temp_sky
        | sun_angle
        | wrong_sci_mode
        # | dihedral_temp_sky
    )
    for key in sky_data:
        sky_data[key] = sky_data[key][sky_cuts]

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
    idx0_cal, idx1_cal = find_nearest_neighbors(cal_data["midpoint_time_gmt"], eng_time_gmt)
    idx0_sky, idx1_sky = find_nearest_neighbors(sky_data["midpoint_time_gmt"], eng_time_gmt)

    print("\nOther data cuts")

    # cal data
    bol_cmd_bias_mismatch_cal = (bol_cmd_bias[idx0_cal] != bol_cmd_bias[idx1_cal]) | (
        bol_cmd_bias[idx0_cal] <= 0
    )
    cal_data["bol_cmd_bias"] = np.where(
        ~bol_cmd_bias_mismatch_cal,
        bol_cmd_bias[idx0_cal],
        0.0,  # or np.nan if you prefer
    )
    upmode_mismatch_cal = (eng_upmode[idx0_cal] != eng_upmode[idx1_cal]) | (
        eng_upmode[idx0_cal] != cal_data["upmode"]
    )
    fakeit_mismatch_cal = (eng_fake[idx0_cal] != eng_fake[idx1_cal]) | (
        eng_fake[idx0_cal] != cal_data["fake"]
    )
    adds_per_group_mismatch_cal = (
        eng_adds_per_group[idx0_cal] != eng_adds_per_group[idx1_cal]
    ) | (eng_adds_per_group[idx0_cal] != cal_data["adds_per_group"])
    sweeps_mismatch_cal = (
        (eng_sweeps[idx0_cal] != eng_sweeps[idx1_cal])
        | (eng_sweeps[idx0_cal] != cal_data["sweeps"])
        | (cal_data["sweeps"] == 1)
    )
    mtm_length_mismatch_cal = (eng_mtm_length[idx0_cal] != eng_mtm_length[idx1_cal]) | (
        eng_mtm_length[idx0_cal] != cal_data["mtm_length"]
    )
    mtm_speed_mismatch_cal = (eng_mtm_speed[idx0_cal] != eng_mtm_speed[idx1_cal]) | (
        eng_mtm_speed[idx0_cal] != cal_data["mtm_speed"]
    )
    gain_mismatch_cal = (eng_gain[idx0_cal] != eng_gain[idx1_cal]) | (
        eng_gain[idx0_cal] == 0
    )
    cal_data["gain"] = np.where(~gain_mismatch_cal, eng_gain[idx0_cal], np.nan)

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
        cal_data[key] = np.where(~mismatch, stat_array[idx0_cal], np.nan)
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
        cal_data[key] = cal_data[key][other_cuts_cal]

    # sky data
    # Check if the two closest values match
    bol_cmd_bias_mismatch = (bol_cmd_bias[idx0_sky] != bol_cmd_bias[idx1_sky]) | (
        bol_cmd_bias[idx0_sky] <= 0
    )
    print(f"    bol_cmd_bias mismatch: {(bol_cmd_bias_mismatch).sum()}")
    # Assign values where they match
    sky_data["bol_cmd_bias"] = np.where(
        ~bol_cmd_bias_mismatch, bol_cmd_bias[idx0_sky], 0.0  # or np.nan if you prefer
    )

    # compare between id1 and idx2 but also with the one from the science data
    upmode_mismatch = (eng_upmode[idx0_sky] != eng_upmode[idx1_sky]) | (
        eng_upmode[idx0_sky] != sky_data["upmode"]
    )
    print(f"    upmode mismatch: {(upmode_mismatch).sum()}")

    fakeit_mismatch = (eng_fake[idx0_sky] != eng_fake[idx1_sky]) | (
        eng_fake[idx0_sky] != sky_data["fake"]
    )
    print(f"    fakeit mismatch: {(fakeit_mismatch).sum()}")

    adds_per_group_mismatch = (
        eng_adds_per_group[idx0_sky] != eng_adds_per_group[idx1_sky]
    ) | (eng_adds_per_group[idx0_sky] != sky_data["adds_per_group"])
    print(f"    adds_per_group mismatch: {(adds_per_group_mismatch).sum()}")

    sweeps_mismatch = (
        (eng_sweeps[idx0_sky] != eng_sweeps[idx1_sky])
        | (eng_sweeps[idx0_sky] != sky_data["sweeps"])
        | (sky_data["sweeps"] == 1)
    )
    print(f"    sweeps mismatch: {(sweeps_mismatch).sum()}")

    mtm_length_mismatch = (eng_mtm_length[idx0_sky] != eng_mtm_length[idx1_sky]) | (
        eng_mtm_length[idx0_sky] != sky_data["mtm_length"]
    )
    print(f"    mtm_length mismatch: {(mtm_length_mismatch).sum()}")

    mtm_speed_mismatch = (eng_mtm_speed[idx0_sky] != eng_mtm_speed[idx1_sky]) | (
        eng_mtm_speed[idx0_sky] != sky_data["mtm_speed"]
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
    sky_data["gain"] = np.where(~gain_mismatch, eng_gain[idx0_sky], np.nan)

    moon_contamination = sky_data["moon_angle"] <= 22.0
    print(f"    Moon Angle <= 22.0: {moon_contamination.sum()}")

    # Vectorized status word processing for sky data
    sw_mismatches = np.zeros(len(idx0_sky), dtype=bool)
    for stat_array, key in stat_words_cal:  # Reuse the same list
        mismatch = stat_array[idx0_sky] != stat_array[idx1_sky]
        sky_data[key] = np.where(~mismatch, stat_array[idx0_sky], np.nan)
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
        sky_data[key] = sky_data[key][other_cuts]

    # Using numpy interp is faster than scipy interp1d for single evaluations
    cal_data["bol_volt"] = np.interp(cal_data["midpoint_time_s"], eng_time_s, bol_volt)
    sky_data["bol_volt"] = np.interp(sky_data["midpoint_time_s"], eng_time_s, bol_volt)

    # no high current readings for bolometers - vectorized
    a_lo_bol_assem = grt["a_lo_bol_assem"][:, channel_i]
    b_lo_bol_assem = grt["b_lo_bol_assem"][:, channel_i]
    bol_assem = (a_lo_bol_assem + b_lo_bol_assem) * 0.5  # Slightly faster than /2.0
    
    cal_data["bolometer"] = np.interp(cal_data["midpoint_time_s"], eng_time_s, bol_assem)
    good_bolometer_cal = cal_data["bolometer"] > 0.0
    for key in cal_data:
        cal_data[key] = cal_data[key][good_bolometer_cal]
    
    sky_data["bolometer"] = np.interp(sky_data["midpoint_time_s"], eng_time_s, bol_assem)
    good_bolometer_sky = sky_data["bolometer"] > 0.0
    print(f"    Bad Bolometer Temperature Readings: {(~good_bolometer_sky).sum()}")
    for key in sky_data:
        sky_data[key] = sky_data[key][good_bolometer_sky]

    # save with compression
    print(f"Saving results for channel {channel.upper()}...")
    np.savez_compressed(
        f"{g.PREPROCESSED_DATA_PATH}/sky_{channel}.npz",
        **sky_data,
    )
    np.savez_compressed(
        f"{g.PREPROCESSED_DATA_PATH}/cal_{channel}.npz",
        **cal_data,
    )
    
    channel_time = time.time() - start_time
    print(f"Channel {channel.upper()} completed in {channel_time:.2f} seconds")

elapsed_time = time.time() - start_time
print(f"\n\nAll channels processed successfully!")
print(f"Total time: {elapsed_time:.2f} seconds ({elapsed_time/60:.2f} minutes)")
print(f"\n\nAll channels processed successfully!")
print(f"Total time: {elapsed_time:.2f} seconds ({elapsed_time/60:.2f} minutes)")
