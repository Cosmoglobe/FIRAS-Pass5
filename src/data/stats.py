import matplotlib
import scipy.odr as odr

import globals as g

matplotlib.use('Agg')  # Use non-interactive backend for multiprocessing
import os
from functools import partial
from multiprocessing import Pool

import matplotlib.pyplot as plt
import numpy as np
from matplotlib import markers
from scipy.stats import norm

element_long_names = {
    "ical": "Internal Calibrator",
    "xcal_cone": "External Calibrator Cone",
    "refhorn": "Reference Horn",
    "skyhorn": "Sky Horn",
    "dihedral": "Dihedral Mirror",
    "collimator": "Collimating Mirror"
    }

def table3_4(xcal_pos, mtm_length, mtm_speed):
    """Compute and print scan mode statistics as in table 3.4 of the FIRAS Explanatory Supplement."""

    science_data = xcal_pos == 2
    mtm_length = mtm_length[science_data]
    mtm_speed = mtm_speed[science_data]

    ss = (mtm_length == 0) & (mtm_speed == 0)
    sf = (mtm_length == 0) & (mtm_speed == 1)
    ls = (mtm_length == 1) & (mtm_speed == 0)
    lf = (mtm_length == 1) & (mtm_speed == 1)
    print("\nTable 3.4")
    print(
        f"    SS: {ss.sum()/len(mtm_length)*100:.1f}, SF: {sf.sum()/len(mtm_length)*100:.1f}, LS: {ls.sum()/len(mtm_length)*100:.1f}, LF: {lf.sum()/len(mtm_length)*100:.1f}"
    )


def table4_1(data_quality, fake, xcal_pos):
    print("\nTable 4.1")
    print(f"    Total Science Records: {len(data_quality)}")

    # so it seems that the FPP fails correspond to data quality flag 109 being 0 or flag 110 being 32 (always the same)
    fpp_fail = data_quality[:, -1] == 32
    print(f"    Records Failed by FPP: {fpp_fail.sum()}")

    fake = fake[fpp_fail == False]
    xcal_pos = xcal_pos[fpp_fail == False]
    fakeit = fake == 1
    print(f"    Fake-it Records: {fakeit.sum()}")

    xcal_pos = xcal_pos[fakeit == False]
    xcal_transit = xcal_pos == 3
    print(f"    XCAL Transit Records: {xcal_transit.sum()}")

    print(
        f"    Records Eliminated Before FDQ: {(fpp_fail.sum() + fakeit.sum() + xcal_transit.sum())}"
    )

    xcal_pos = xcal_pos[xcal_transit == False]
    cal = xcal_pos == 1
    sci = xcal_pos == 2
    print(f"    Cal Records Passed by FPP: {cal.sum()}")
    print(f"    Sky Records Passed by FPP: {sci.sum()}")

    return fpp_fail, fakeit, xcal_transit


def table4_2(
    cal_saturated, cal_data_quality, sky_saturated, sky_data_quality, solution
):
    # TODO: this is the most questionable data selection
    print("\nTable 4.2")

    # calibration data
    # TODO: there are some negative values, which they took as non-saturated, should we do the same? here we are doing the same as them
    cal_saturated = cal_saturated > 0
    print(f"    Saturated Sample Count: {cal_saturated.sum()}")
    cal_sw = cal_data_quality[:, 3] != 0
    print(f"    Microprocessor Status Word: {(cal_sw).sum()}")
    cal_glitch_rate = cal_data_quality[:, 7] > 0
    print(f"    Glitch Rate: {cal_glitch_rate.sum()}")
    # TODO: these totals don't correspond to the tables in the explanatory supplement
    print(
        f"    Cal Records Failed by FDQ: {(cal_saturated | cal_sw | cal_glitch_rate).sum()}"
    )
    print(
        f"    Cal records Passed by FDQ: {(~(cal_saturated | cal_sw | cal_glitch_rate)).sum()}"
    )

    # sky data
    sky_saturated = sky_saturated > 0
    print(f"    Saturated Sample Count: {sky_saturated.sum()}")
    sky_sw = sky_data_quality[:, 3] != 0
    print(f"    Microprocessor Status Word: {(sky_sw).sum()}")
    sky_glitch_rate = sky_data_quality[:, 7] > 0
    print(f"    Glitch Rate: {sky_glitch_rate.sum()}")
    limb = sky_data_quality[:, 5] != 0
    print(f"    Sun, Moon, or Earth Limb: {limb.sum()}")
    no_solution = solution == 0
    print(f"    No Attitude Solution: {no_solution.sum()}")
    print(
        f"    Sky Records Failed by FDQ: {(sky_saturated | sky_sw | sky_glitch_rate | limb | no_solution).sum()}"
    )
    print(
        f"    Sky records Passed by FDQ: {(~(sky_saturated | sky_sw | sky_glitch_rate | limb | no_solution)).sum()}"
    )

    return (
        cal_saturated,
        cal_sw,
        cal_glitch_rate,
        sky_saturated,
        sky_sw,
        sky_glitch_rate,
        limb,
        no_solution,
    )


def table4_5(
    earth_limb,
    midpoint_time_cal,
    midpoint_time_sky,
    ical_cal,
    ical_sky,
    sun_angle,
    upmode,
    dihedral_cal,
    dihedral_sky,
):
    print("\nTable 4.5")

    earth_limb = earth_limb < 87
    print(f"    Earth Limb Angle < 87.0: {earth_limb.sum()}")

    sun_angle = sun_angle < 91.2
    print(f"    Sun Angle < 91.2: {sun_angle.sum()}")

    wrong_sci_mode = upmode != 4
    print(f"    Wrong Science Mode: {wrong_sci_mode.sum()}")

    print(f"    Sky Records Failed by FSS: {(earth_limb | sun_angle | wrong_sci_mode).sum()}")
    print(
        f"    Sky Records Passed by FSS: {len(midpoint_time_sky) - (earth_limb | sun_angle | wrong_sci_mode).sum()}"
    )
    
    return (
        earth_limb,
        sun_angle,
        wrong_sci_mode,
    )

def hilo_stats(hi, lo, channel, element, side):
    diff = hi - lo
    plt.hist(diff, bins=np.linspace(-0.05, 0.05, 1000))
    plt.xlabel("High - Low Detector Temperature (K)")
    plt.ylabel("Counts")
    plt.title(f"High - Low Detector Temperature Difference ({channel}) for " + element + " (" + side.upper() + " side)")
    plt.yscale('log')
    plt.savefig(f"data/output/hilo_temp_difference_histogram/{element}_{side}_{channel}.png")
    plt.close()

def _plot_timeseries_chunk(args):
    """Helper function for parallel plotting of time series chunks."""
    i, lo, plateau_masks, channel, element, side = args
    if element == "xcal_cone":
        xsize = 1000
    else:
        xsize = 10000

    x = np.arange(i, min(i + xsize, len(lo)))
    fig_ts, ax_ts = plt.subplots(figsize=(12, 6))
    ax_ts.set_xlabel("Record Index")
    ax_ts.set_ylabel("Temperature (K)")
    ax_ts.set_title(f"Detector Temperatures Over Time for {element} ({side.upper()}) - {channel.upper()}")
    for j, mask in enumerate(plateau_masks):
        mask_slice = mask[x]
        if np.any(mask_slice):
            ax_ts.plot(x[mask_slice], lo[x][mask_slice], label=f'Plateau {j+1}', ls='None', marker='.',
                     ms=4, color=f"C{j}")
    ax_ts.legend()
    fig_ts.savefig(f"data/output/divide_plateaus/01_chunks_over_time/{channel}/{element}/{side}/{i}.png")
    plt.close(fig_ts)


def divide_plateaus(lo, channel, element, side, plateau_divides_cache=None, n_cores=None):
    """
    Divide each of the temperatures into their different plateaus so we can find only one Gaussian
    for each.
    """
    if n_cores is None:
        n_cores = max(1, os.cpu_count() - 2)  # Leave 2 cores free

    print(f"Minimum and maximum low current {channel} ({side.upper()} side) for {element}: "
          f"{np.nanmin(lo):.3f}, {np.nanmax(lo):.3f}")

    # Use cached plateau divides if available, otherwise read from file
    if plateau_divides_cache is not None and f"{element}_{side}" in plateau_divides_cache:
        plateau_divides = plateau_divides_cache[f"{element}_{side}"]
    else:
        with open(f"data/plateau_divides.txt", "r") as f:
            lines = f.readlines()
            for line in lines:
                name = line.split(" ")[0]
                if name == f"{element}_{side}":
                    plateau_divides = np.array((line.split(" ")[1].split(","))).astype(float)
                    break

    plateau_masks = []
    for i in range(1, len(plateau_divides)):
        plateau = (lo >= plateau_divides[i-1]) & (lo < plateau_divides[i])
        plateau_masks.append(plateau)

        if g.VERBOSE > 2:
            min_val = plateau_divides[i-1]
            divide = plateau_divides[i]
            print(f"Plateau {i} ({min_val} to {divide}): {np.sum(plateau) / len(lo) * 100:.2f}% of the data")

    # plot the whole temperature over time and color the points by plateau
    # plot for each 100000 points to see better - parallelized
    if element == "xcal_cone":
        xsize = 1000
    else:
        xsize = 10000
    chunk_indices = list(range(0, len(lo), xsize))
    args_list = [(i, lo, plateau_masks, channel, element, side) for i in chunk_indices]
    
    if g.VERBOSE > 2:
        with Pool(processes=n_cores) as pool:
            pool.map(_plot_timeseries_chunk, args_list)
        print("Plotted check 1 -----------------------------------------------------------------------")

    # plot timeseries of only the points in the first plateau to see if there are more to split
    if g.VERBOSE > 2:
        for i in range(len(plateau_masks)):
            plt.figure()  # Create a new figure instead of using the current one
            plt.plot(lo[plateau_masks[i]])
            plt.xlabel("Record Index")
            plt.ylabel("Temperature (K)")
            plt.title(f"Low Current Over Time for {element} ({side.upper()}) - {channel.upper()}")
            plt.savefig(f"data/output/divide_plateaus/02_plateaus_over_time/{channel}/{element}/{side}_{i+1}.png")
            plt.close()
        print("Plotted check 2 -----------------------------------------------------------------------")

    return plateau_masks


def fit_gaussian(hi, lo, channel, element, side, plateau=0):
    """Fit a Gaussian to the difference between hi and lo temperatures."""
    if g.VERBOSE > 1:
        print("Starting fit_gaussian -------------------------------------------------------------")

    # Vectorized statistics
    n = len(hi)
    avg_temp = np.mean(hi)
    std_temp = np.std(hi, ddof=1)  # Use ddof=1 for sample std
    temp_err = std_temp / np.sqrt(n)

    diff = hi - lo

    # fit the normal distribution to the data
    mu, std = norm.fit(diff, loc=0.015, scale=0.005)
    mu_err = std / np.sqrt(n)  # Standard error of the mean

    # set up plot
    if g.VERBOSE > 2:
        fig_gauss, ax_gauss = plt.subplots(figsize=(10, 6))
        ax_gauss.set_yscale('log')
        ax_gauss.set_title(f"{channel.upper()} for " + element + " (" + side.upper() + " side)")
        ax_gauss.set_xlabel("High - Low Detector Temperature (K)")
        ax_gauss.set_ylabel("Density")

        # plot the full distribution
        ax_gauss.hist(diff, bins=np.linspace(-abs(diff).max(), abs(diff).max(), 1000), density=True)
        ax_gauss.set_ylim(1, None)

        # set axis from the full distribution
        xmin, xmax = ax_gauss.get_xlim()
        x = np.linspace(xmin, xmax, 100)

        # plot the histogram and the fitted Gaussian
        p = norm.pdf(x, mu, std)
        ax_gauss.plot(x, p, ls='dashed', label=f'Gaussian for Plateau {plateau}')
        print(f"Fitted gaussian for plateau {plateau} for {element} {side} {channel}: mu={mu:.04f}, std={std:.04f}")

        ax_gauss.legend()
        fig_gauss.savefig(f"data/output/fit_gaussian/01_hilo_temp_difference/{channel}/{element}/{side}_plateau_{plateau}.png")
        plt.close(fig_gauss)
        print("Plotted check 1 -------------------------------------------------------------------")
    return mu, mu_err, avg_temp, temp_err

def oneoverT2(beta, x):
    level = beta[0]
    T_knee = beta[1]
    A = beta[2]
    return level + A * (T_knee / x) ** 2

def selfheat_vs_temp(mu, mu_err, avg_temp, temp_err, element, side):
    print(f"Number of points being fit: {len(mu)}")

    if g.VERBOSE > 1:
        print("Starting selfheat_vs_temp ---------------------------------------------------------")
    if g.VERBOSE > 2:
        plt.figure()  # Create a new figure instead of using the current one
        plt.errorbar(avg_temp, mu, yerr=mu_err, xerr=temp_err, fmt='o')
        # plt.ylim(-0.2, 0.2)
        plt.xlabel("Average High Current Temperature (K)")
        plt.ylabel("Fitted Gaussian Mean of High-Low Difference (K)")
        plt.title("Self-Heating vs Average High Current Temperature")
        plt.savefig(f"data/output/selfheat_vs_temp/01_points/{element}_{side}.png")
        plt.close()

    sx = np.abs(temp_err)  # Uncertainty in x (sigma_x)
    sy = np.abs(mu_err)  # Uncertainty in y (sigma_y)
    data = odr.Data(avg_temp, mu, wd=1/sx**2, we=1/sy**2)  # wd: x weights, we: y weights

    model = odr.Model(oneoverT2)
    odr_obj = odr.ODR(data, model, beta0=[0, 3, 1])
    odr_result_oneoverT2 = odr_obj.run()
    print(f"Residual variance for 1/T^2 model: {odr_result_oneoverT2.res_var:.3f}")

    if g.VERBOSE > 2:
        plt.figure()  # Create a new figure for the fit plots
        plt.errorbar(avg_temp, mu, yerr=mu_err, xerr=temp_err, fmt='o', label='Data')
        x = np.linspace(avg_temp.min() - 0.5, avg_temp.max() + 0.5, 100)
        plt.plot(x, oneoverT2(odr_result_oneoverT2.beta, x), label="1/T^2 model")
        plt.xlabel("Average High Current Temperature (K)")
        plt.ylabel("Fitted Gaussian Mean of High-Low Difference (K)")
        plt.title("Self-Heating vs Average High Current Temperature - Model Fits")
        plt.legend()
        plt.savefig(f"data/output/selfheat_vs_temp/02_odr_fit/{element}_{side}.png")
        plt.close()
        print("Plotted check 1 -------------------------------------------------------------------")

    return odr_result_oneoverT2.beta
    
def moving_average(a, n=3):
    result = np.convolve(a, np.ones(n)/n, mode='same')
    for i in range(0, n):
        result[i] = np.average(a[0:i*2+1])
    for i in range(len(a)-n+1, len(a)):
        result[i] = np.average(a[i*2:])
    return result

def moving_std(a, n=3):
    result = np.zeros_like(a)
    for i in range(n, len(a)-n+1):
        start = max(0, i - n//2)
        end = min(len(a), i + n//2 + 1)
        result[i] = np.std(a[start:end])
    for i in range(0, n):
        result[i] = np.std(a[0:i*2+1])
    for i in range(len(a)-n+1, len(a)):
        result[i] = np.std(a[i*2:])
    return result

def estimate_noise(lo, hi, element):
    n = 15
    lo_avg = moving_average(lo, n=n)
    lo_std = moving_std(lo, n=n)

    # these are gotten by eye checking all the data
    low_cut = 0.005
    med_cut = 0.08
    high_cut = 0.6
    
    jumps = []
    for i in range(len(lo)):
        if (lo_avg[i] < 4) and (lo_std[i] > low_cut):
            jumps.append(i)
        elif (lo_avg[i] >= 4) and (lo_avg[i] < 10) and (lo_std[i] > med_cut):
            jumps.append(i)
        elif (lo_avg[i] >= 10) and (lo_std[i] > high_cut):
            jumps.append(i)

    if g.VERBOSE > 2:
        xsize = 1000 if element == "xcal_cone" else 10000
        x = np.arange(len(lo), dtype=float)
        for i in range(0, len(lo), xsize):
            if i + xsize > len(lo):
                break
            fig_noise, ax_noise = plt.subplots(figsize=(12, 6))

            curr_jumps = [np.array(jumps)[(np.array(jumps) >= i) & (np.array(jumps) < i + xsize)]]

            ax_noise.vlines(curr_jumps, np.nanmin(lo[i:i+xsize]), np.nanmax(lo[i:i+xsize]),
                            label='Detected Jumps', color='C4')
            ax_noise.plot(x[i:i+xsize], lo[i:i+xsize], label='Low Current')
            ax_noise.plot(x[i:i+xsize], hi[i:i+xsize], label='High Current')
            ax_noise.plot(x[i:i+xsize], lo_avg[i:i+xsize], label='Moving Average')
            ax_noise.plot(x[i:i+xsize], lo_std[i:i+xsize]+2.7, label='Moving Std Dev+2.7',
                        ls='dashed')
            
            ax_noise.set_xlabel("Record Index")
            ax_noise.set_ylabel("Low Current Temperature (K)")
            ax_noise.set_title("Low Current Temperature and Moving Average")
            ax_noise.legend()
            fig_noise.savefig(f"data/output/estimate_noise/01_moving_average/{element}/{i}.png")
            plt.close(fig_noise)

    hi_std = moving_std(hi, n=n)

    return jumps, lo_std, hi_std

def debiase_hi(beta, hi, lo, low_temps, high_temps, jumps, lo_std, hi_std, element, side, channel):
    """
    Debiase the high temperature using the fitted Gaussian parameters.
    
    Parameters:
    - beta: Parameters of the electronics model fitted to the self-heating vs temperature data.
    - hi: Original high current temperatures.
    - lo: Original low current temperatures.
    - low_temps: Boolean array indicating which temperatures are below a threashold and only the
    low current should be used for them.
    - high_temps: Boolean array indicating which temperatures are above a threshold and only the
    high current should be used for them.
    - element: The element being analyzed (e.g., "ical", "xcal_cone", etc.).
    - side: The side being analyzed ("a" or "b").
    - channel: The channel being analyzed ("short", "medium", or "long").
    """

    if g.VERBOSE > 1:
        print("Starting debiase_hi ---------------------------------------------------------------")
    
    # Vectorized debiasing - no copying needed
    debiased_hi = np.copy(lo)
    debiased_hi[~low_temps & ~high_temps] = (hi[~low_temps & ~high_temps] -
                                             oneoverT2(beta, hi[~low_temps & ~high_temps]))
    debiased_hi[low_temps or high_temps] = np.nan

    xsize = 1000 if element == "xcal_cone" else 10000

    if g.VERBOSE > 2:
        x = np.arange(len(hi))
        for i in range(0, len(debiased_hi), xsize):
            fig_debias, ax_debias = plt.subplots(figsize=(12, 6))
            ax_debias.set_xlabel("Record Index")
            ax_debias.set_ylabel("Temperature (K)")
            ax_debias.set_title(f"Debiasing High Current Temperatures for {element} "
                                f"({side.upper()}) - {channel.upper()}")
            ax_debias.plot(x[i:i+xsize], lo[i:i+xsize], label='Low Current')
            ax_debias.plot(x[i:i+xsize], hi[i:i+xsize],
                    label='High Current', ls='None', marker='.', ms=4)
            
            ax_debias.set_ylabel("Temperature (K)")
            ax_debias.set_xlabel("Record Index")
            ax_debias.set_title(f"Temperatures measured for the {element_long_names[element]} "
                                f"{side.upper()} side")
            ax_debias.legend()
            fig_debias.savefig(f"data/output/debiase_hi/01_timeseries/{element}/{side}/{channel}/"
                               f"{i}_0.png")
            plt.close(fig_debias)

            fig_debias, ax_debias = plt.subplots(figsize=(12, 6))
            ax_debias.plot(x[i:i+xsize], lo[i:i+xsize], label='Low Current')
            ax_debias.plot(x[i:i+xsize], debiased_hi[i:i+xsize],
                    label='Debiased High Current', ls='None', marker='.', ms=4)
            
            ax_debias.set_ylabel("Temperature (K)")
            ax_debias.set_xlabel("Record Index")
            ax_debias.set_title(f"Temperatures measured for the {element_long_names[element]} "
                                f"{side.upper()} side after debiasing")
            ax_debias.legend()
            fig_debias.savefig(f"data/output/debiase_hi/01_timeseries/{element}/{side}/{channel}/{i}_1.png")
            plt.close(fig_debias)
        print("Plotted check 1 -------------------------------------------------------------------")

    return debiased_hi

if __name__ == "__main__":
    array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    print(moving_average(array, n=5))