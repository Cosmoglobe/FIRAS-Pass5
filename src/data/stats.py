import globals as g
import matplotlib
import scipy.odr as odr

matplotlib.use('Agg')  # Use non-interactive backend for multiprocessing
from functools import partial
from multiprocessing import Pool

import matplotlib.pyplot as plt
import numpy as np
from matplotlib import markers
from scipy.stats import norm


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

    # start_times = [
    #     "89-326-1130",
    #     "89-328-0000",
    #     "89-343-0152",
    #     "90-019-0205",
    #     "90-080-0115",
    #     "90-129-0000",
    #     "90-139-1535",
    #     "90-193-1850",
    #     "90-207-1104",
    #     "90-208-1120",
    #     "90-220-0500",
    # ]
    # # Vectorized date parsing using list comprehension (faster than loop)
    # start_times = [
    #     data_utils.parse_date_string(t.replace("-", ""), include_s=False)
    #     for t in start_times
    # ]

    # end_times = [
    #     "89-327-2359",
    #     "89-343-0151",
    #     "90-019-0204",
    #     "90-080-0114",
    #     "90-128-2359",
    #     "90-139-1534",
    #     "90-193-1849",
    #     "90-207-1103",
    #     "90-208-1119",
    #     "90-220-0459",
    #     "90-264-0936",
    # ]
    # end_times = [
    #     data_utils.parse_date_string(t.replace("-", ""), include_s=False)
    #     for t in end_times
    # ]

    # allowed_ical_temps = [
    #     [2.789],
    #     [2.758, 2.763, 2.789],
    #     [2.759, 2.771],
    #     [2.758, 2.771],
    #     [2.758, 2.771],
    #     [2.758, 2.770],
    #     [2.7455, 2.755, 2.768],
    #     [2.746, 2.757, 2.769],
    #     [2.757, 2.769],
    #     [2.758, 2.770],
    #     [2.758, 2.771],
    # ]

    # wrong_ical_temp_cal = np.zeros(len(midpoint_time_cal), dtype=bool)
    # for i, (start, end, temps) in enumerate(
    #     zip(start_times, end_times, allowed_ical_temps)
    # ):
    #     time_mask = (midpoint_time_cal > start) & (midpoint_time_cal < end)
    #     temp_mask = np.ones(len(midpoint_time_cal), dtype=bool)
    #     for temp in temps:
    #         temp_mask &= (ical_cal > temp + 0.002) | (ical_cal < temp - 0.002)
    #     wrong_ical_temp_cal |= time_mask & temp_mask

    # wrong_ical_temp_sky = np.zeros(len(midpoint_time_sky), dtype=bool)
    # for i, (start, end, temps) in enumerate(
    #     zip(start_times, end_times, allowed_ical_temps)
    # ):
    #     time_mask = (midpoint_time_sky > start) & (midpoint_time_sky < end)
    #     temp_mask = np.ones(len(midpoint_time_sky), dtype=bool)
    #     for temp in temps:
    #         temp_mask &= (ical_sky > temp + 0.002) | (ical_sky < temp - 0.002)
    #     wrong_ical_temp_sky |= time_mask & temp_mask
    # print(f"    Wrong ICAL Temperature: {sum(wrong_ical_temp_sky)}")

    sun_angle = sun_angle < 91.2
    print(f"    Sun Angle < 91.2: {sun_angle.sum()}")

    wrong_sci_mode = upmode != 4
    print(f"    Wrong Science Mode: {wrong_sci_mode.sum()}")

    # dihedral_temp_cal = dihedral_cal > 5.5
    # dihedral_temp_sky = dihedral_sky > 5.5
    # print(f"    Dihedral Temperature > 5.5: {dihedral_temp_sky.sum()}")

    # print(
    #     f"    Sky Records Failed by FSS: {(earth_limb | wrong_ical_temp_sky | sun_angle | wrong_sci_mode | dihedral_temp_sky).sum()}"
    # )
    print(f"    Sky Records Failed by FSS: {(earth_limb | sun_angle | wrong_sci_mode).sum()}")
    # print(
    #     f"Sky Records Passed by FSS: {len(midpoint_time_sky) - (earth_limb | wrong_ical_temp_sky | sun_angle | wrong_sci_mode | dihedral_temp_sky).sum()}"
    # )
    print(
        f"    Sky Records Passed by FSS: {len(midpoint_time_sky) - (earth_limb | sun_angle | wrong_sci_mode).sum()}"
    )
    
    return (
        earth_limb,
        # wrong_ical_temp_cal,
        # wrong_ical_temp_sky,
        sun_angle,
        wrong_sci_mode,
        # dihedral_temp_cal,
        # dihedral_temp_sky,
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
    plt.figure(figsize=(12, 6))
    plt.xlabel("Record Index")
    plt.ylabel("Temperature (K)")
    plt.title(f"Detector Temperatures Over Time for {element} ({side.upper()}) - {channel.upper()}")
    for j, mask in enumerate(plateau_masks):
        if lo[x][mask[x]].size > 0:
            plt.plot(x[mask[x]], lo[x][mask[x]], label=f'Plateau {j+1}', ls='None', marker='.',
                     ms=4, color=f"C{j}")
    plt.legend()
    plt.savefig(f"data/output/divide_plateaus/01_chunks_over_time_{channel}/{element}/{side}/{i}.png")
    plt.close()


def divide_plateaus(lo, channel, element, side, plateau_divides_cache=None, n_cores=12):
    """
    Divide each of the temperatures into their different plateaus so we can find only one Gaussian
    for each.
    """

    print(f"Minimum and maximum low current {channel} ({side.upper()} side) for {element}: {lo.min():.3f}, {lo.max():.3f}")

    # plt.hist(lo, bins=np.arange(1.5, 25, 0.001))
    # plt.xlabel(f"Low Current {channel.upper()} ({side.upper()} side) for {element}")
    # plt.savefig(f"data/output/divide_plateaus/01_lo_{element}_{channel}/{side}.png")
    # plt.close()

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
    for i, divide in enumerate(plateau_divides):
        if i == 0:
            continue
        else:
            plateau = (lo >= plateau_divides[i-1]) & (lo < divide)
            min = plateau_divides[i-1]
        plateau_masks.append(plateau)

        if g.VERBOSE > 2:
            print(f"Plateau {i} ({min} to {divide}): {len(lo[plateau]) / len(lo) * 100:.2f}% of the data")

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
            plt.plot(lo[plateau_masks[i]])
            plt.xlabel("Record Index")
            plt.ylabel("Temperature (K)")
            plt.title(f"Low Current Over Time for {element} ({side.upper()}) - {channel.upper()}")
            plt.savefig(f"data/output/divide_plateaus/02_plateaus_over_time_{channel}/{element}/{side}_{i+1}.png")
            plt.close()
        print("Plotted check 2 -----------------------------------------------------------------------")

    return plateau_masks


def fit_gaussian(hi, lo, channel, element, side, sigma=3, plateau=0):
    """Fit a Gaussian to the difference between hi and lo temperatures."""
    # fit a gaussian to the input temperatures?
    if g.VERBOSE > 1:
        print("Starting fit_gaussian -------------------------------------------------------------")

    avg_temp = np.mean(hi)
    std_temp = np.std(hi)
    temp_err = std_temp / np.sqrt(len(hi))

    diff = hi - lo
    # print(f"Minimum and maximum difference: {diff.min():.3f}, {diff.max():.3f}")

    # fit the normal distribution to the data
    mu, std = norm.fit(diff, loc=0.015, scale=0.005)
    mu_err = std / np.sqrt(len(diff))  # Standard error of the mean

    # set up plot
    if g.VERBOSE > 2:
        plt.yscale('log')
        plt.title(f"{channel.upper()} for " + element + " (" + side.upper() + " side)")
        plt.xlabel("High - Low Detector Temperature (K)")
        plt.ylabel("Density")

        # plot the full distribution
        plt.hist(diff, bins=np.linspace(-abs(diff).max(), abs(diff).max(), 1000), density=True)
        plt.ylim(1, None)

        # set axis from the full distribution
        xmin, xmax = plt.xlim()
        x = np.linspace(xmin, xmax, 100)

        # plot the histogram and the fitted Gaussian
        p = norm.pdf(x, mu, std)
        plt.plot(x, p, ls='dashed', label=f'Gaussian for Plateau {plateau}')
        print(f"Fitted gaussian for plateau {plateau} for {element} {side} {channel}: mu={mu:.04f}, std={std:.04f}")
        explained = len(diff[(diff > mu - sigma*std) & (diff < mu + sigma*std)]) / len(diff)
        print(f"Explained fraction by the gaussian at {sigma} std: {explained*100:.02f}%")

        plt.legend()
        plt.savefig(f"data/output/fit_gaussian/01_hilo_temp_difference_{channel}/{element}/{side}_plateau_{plateau}.png")
        plt.close()
        print("Plotted check 1 -------------------------------------------------------------------")
    return mu, mu_err, avg_temp, temp_err

    # plot all temperatures over time for visual inspection
    plt.figure(figsize=(12, 6))
    plt.xlabel("Record Index")
    plt.ylabel("Temperature (K)")
    plt.title(f"Detector Temperatures Over Time for {element} ({side.upper()}) - {channel.upper()}")

    x = np.arange(len(hi))

    # plot only a subset to see better
    idx = x[(x > 90000) & (x < 95000)]
    x = x[idx]
    hi = hi[idx]
    lo = lo[idx]
    diff = diff[idx]
    
    plt.plot(x, hi, label='High Temp')
    plt.plot(x, lo, label='Low Temp')
    plt.savefig(f"data/output/fit_gaussian/hilo_temp_timeseries/{element}_{side}_{channel}_plateau{plateau}_g0.png")
    plt.close()
    if ngaussians > 0:
        plt.plot(x[(diff > mu - sigma*std) & (diff < mu + sigma*std)],
                 hi[(diff > mu - sigma*std) & (diff < mu + sigma*std)], label='Explained by Gaussian 1',
                    linestyle='None', marker='.')
        plt.plot(x[(diff > mu - sigma*std) & (diff < mu + sigma*std)],
                 lo[(diff > mu - sigma*std) & (diff < mu + sigma*std)], label='Explained by Gaussian 1',
                    linestyle='None', marker='.')
        plt.legend()
        plt.savefig(f"data/output/fit_gaussian/hilo_temp_timeseries/{element}_{side}_{channel}_g1.png")
        plt.close()
    if ngaussians > 1:
        plt.plot(x[(diff > mu2 - sigma*std2) & (diff < mu2 + sigma*std2)],
                 hi[(diff > mu2 - sigma*std2) & (diff < mu2 + sigma*std2)], label='Explained by Gaussian 2',
                    linestyle='None', marker='.')
        plt.plot(x[(diff > mu2 - sigma*std2) & (diff < mu2 + sigma*std2)],
                 lo[(diff > mu2 - sigma*std2) & (diff < mu2 + sigma*std2)], label='Explained by Gaussian 2',
                    linestyle='None', marker='.')
        plt.legend()
        plt.savefig(f"data/output/fit_gaussian/hilo_temp_timeseries/{element}_{side}_{channel}_g2.png")
        plt.close()
    
    # plt.savefig(f"data/output/hilo_temp_timeseries/{element}_{side}_{channel}.png")
    plt.close()
    if ngaussians == 1:
        return mu, std
    elif ngaussians == 2:
        return mu, std, mu2, std2

def negative_exponential(beta, x):
    A = beta[0]
    lamb = beta[1]
    return A * np.exp(lamb * x)

def linear_model(beta, x):
    m = beta[0]
    b = beta[1]
    return m * x + b

def power_law(beta, x):
    A = beta[0]
    alpha = beta[1]
    return A * x**(alpha)

def double_power_law(beta, x):
    A1 = beta[0]
    alpha1 = beta[1]
    A2 = beta[2]
    alpha2 = beta[3]
    return A1 * x**(alpha1) + A2 * x**(alpha2)

def electronics_model(beta, x):
    A1 = beta[0]
    T_knee = beta[1]
    A2 = beta[2]
    alpha = beta[3]
    level = beta[4]

    return A1 * T_knee / x ** 2 + A2 * np.exp(x * alpha) + level


def selfheat_vs_temp(mu, mu_err, avg_temp, temp_err, element, side):
    print(f"Number of points being fit: {len(mu)}")

    if g.VERBOSE > 1:
        print("Starting selfheat_vs_temp ---------------------------------------------------------")
    if g.VERBOSE > 2:
        plt.errorbar(avg_temp, mu, yerr=mu_err, xerr=temp_err, fmt='o')
        # plt.ylim(-0.2, 0.2)
        plt.xlabel("Average High Current Temperature (K)")
        plt.ylabel("Fitted Gaussian Mean of High-Low Difference (K)")
        plt.title("Self-Heating vs Average High Current Temperature")
        plt.savefig(f"data/output/selfheat_vs_temp/01_points/{element}_{side}.png")

    sx = np.abs(temp_err)  # Uncertainty in x (sigma_x)
    sy = np.abs(mu_err)  # Uncertainty in y (sigma_y)
    data = odr.Data(avg_temp, mu, wd=1/sx**2, we=1/sy**2)  # wd: x weights, we: y weights

    model = odr.Model(negative_exponential)
    # Initialize ODR with model, data, and initial parameter guess
    odr_obj = odr.ODR(data, model, beta0=[1.0, -1.0])
    # Run the fit
    odr_result_exp = odr_obj.run()
    print(f"Residual variance for negative exponential: {odr_result_exp.res_var:.3f}")

    model = odr.Model(linear_model)
    odr_obj = odr.ODR(data, model, beta0=[0.0, 0.0])
    odr_result_linear = odr_obj.run()
    print(f"Residual variance for linear model: {odr_result_linear.res_var:.3f}")

    model = odr.Model(power_law)
    odr_obj = odr.ODR(data, model, beta0=[1.0, -1.0])
    odr_result_power = odr_obj.run()
    print(f"Residual variance for power law: {odr_result_power.res_var:.3f}")

    model = odr.Model(double_power_law)
    odr_obj = odr.ODR(data, model, beta0=[1.0, 1.0, 1.0, -1.0])
    odr_result_double_power = odr_obj.run()
    print(f"Residual variance for double power law: {odr_result_double_power.res_var:.3f}")

    model = odr.Model(electronics_model)
    odr_obj = odr.ODR(data, model, beta0=[1.0, 6.0, 1.0, -1.0, 0.0])
    odr_result_electronics = odr_obj.run()
    print(f"Residual variance for electronics model: {odr_result_electronics.res_var:.3f}")

    if g.VERBOSE > 2:
        x = np.linspace(avg_temp.min() - 0.5, avg_temp.max() + 0.5, 100)
        plt.plot(x, negative_exponential(odr_result_exp.beta, x), "r-", label="Negative exponential")
        plt.plot(x, linear_model(odr_result_linear.beta, x), "g-", label="Linear")
        plt.plot(x, power_law(odr_result_power.beta, x), "b-", label="Power law")
        plt.plot(x, double_power_law(odr_result_double_power.beta, x), "m-", label="Double power law")
        plt.plot(x, electronics_model(odr_result_electronics.beta, x), "c-", label="Electronics model")
        # plt.ylim(min(mu-mu_err), max(mu+mu_err))
        plt.legend()
        plt.savefig(f"data/output/selfheat_vs_temp/02_odr_fit/{element}_{side}.png")
        plt.close()
        print("Plotted check 1 -------------------------------------------------------------------")

    return odr_result_electronics.beta
    
def debiase_hi(beta, hi, lo, element, side, channel):
    """Debiase the high temperature using the fitted Gaussian parameters."""
    if g.VERBOSE > 1:
        print("Starting debiase_hi ---------------------------------------------------------------")
    # diff = hi - lo
    debiased_hi = np.copy(hi)

    # Debias using the fitted negative exponential model
    # debiased_hi = hi - negative_exponential(beta, hi)
    debiased_hi = hi - electronics_model(beta, hi)

    # mask1 = (diff > mu - sigma*std) & (diff < mu + sigma*std)
    # debiased_hi[mask1] = hi[mask1] - mu

    # # Debias using the second Gaussian
    # mask2 = (diff > mu2 - sigma*std2) & (diff < mu2 + sigma*std2)
    # debiased_hi[mask2] = hi[mask2] - mu2

    if element == "xcal_cone":
        xsize = 1000
    else:
        xsize = 10000

    if g.VERBOSE > 2:
        x = np.arange(len(hi))
        for i in range(0, len(debiased_hi), xsize):
            plt.plot(x[i:i+xsize], lo[i:i+xsize], label='Low Current')
            plt.plot(x[i:i+xsize], hi[i:i+xsize],
                    label='High Current', ls='None', marker='.', ms=4)
            plt.legend()
            plt.savefig(f"data/output/debiase_hi/01_timeseries/{element}/{side}/{channel}_{i}_0.png")
            plt.close()

            plt.plot(x[i:i+xsize], lo[i:i+xsize], label='Low Current')
            plt.plot(x[i:i+xsize], debiased_hi[i:i+xsize],
                    label='Debiased High Current', ls='None', marker='.', ms=4)
            plt.legend()
            plt.savefig(f"data/output/debiase_hi/01_timeseries/{element}/{side}/{channel}_{i}_1.png")
            plt.close()
        print("Plotted check 1 -------------------------------------------------------------------")

    return debiased_hi