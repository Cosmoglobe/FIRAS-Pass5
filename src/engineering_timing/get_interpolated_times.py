import astropy.units as u
import h5py
import matplotlib.pyplot as plt
import numpy as np

#DATA_DIR = '/home/dwatts/Commander/commander3/todscripts/firas/src/engineering_timing'
DATA_DIR = "/mn/stornext/d16/cmbco/ola/firas/initial_data/"

plot = True

lens_grt = np.ones(64, dtype=int)

names_en_analog_grt = [
    # grts
    # 'a_lo_grt', 'a_hi_grt', 'b_lo_grt', 'b_hi_grt',
    "a_lo_xcal_tip",
    "a_lo_skyhorn",
    "a_lo_refhorn",
    "a_lo_ical",
    "a_lo_dihedral",
    "a_lo_bol_assem_1",
    "a_lo_bol_assem_2",
    "a_lo_bol_assem_3",
    "a_lo_bol_assem_4",
    "a_lo_mirror",
    "a_lo_cal_resistors_1",
    "a_lo_cal_resistors_2",
    "a_lo_cal_resistors_3",
    "a_lo_cal_resistors_4",
    "a_lo_xcal_cone",
    "a_lo_collimator",
    "a_hi_xcal_tip",
    "a_hi_skyhorn",
    "a_hi_refhorn",
    "a_hi_ical",
    "a_hi_dihedral",
    "a_hi_bol_assem_1",
    "a_hi_bol_assem_2",
    "a_hi_bol_assem_3",
    "a_hi_bol_assem_4",
    "a_hi_mirror",
    "a_hi_cal_resistors_1",
    "a_hi_cal_resistors_2",
    "a_hi_cal_resistors_3",
    "a_hi_cal_resistors_4",
    "a_hi_xcal_cone",
    "a_hi_collimator",
    "b_lo_xcal_tip",
    "b_lo_skyhorn",
    "b_lo_refhorn",
    "b_lo_ical",
    "b_lo_dihedral",
    "b_lo_bol_assem_1",
    "b_lo_bol_assem_2",
    "b_lo_bol_assem_3",
    "b_lo_bol_assem_4",
    "b_lo_mirror",
    "b_lo_cal_resistors_1",
    "b_lo_cal_resistors_2",
    "b_lo_cal_resistors_3",
    "b_lo_cal_resistors_4",
    "b_lo_xcal_cone",
    "b_lo_collimator",
    "b_hi_xcal_tip",
    "b_hi_skyhorn",
    "b_hi_refhorn",
    "b_hi_ical",
    "b_hi_dihedral",
    "b_hi_bol_assem_1",
    "b_hi_bol_assem_2",
    "b_hi_bol_assem_3",
    "b_hi_bol_assem_4",
    "b_hi_mirror",
    "b_hi_cal_resistors_1",
    "b_hi_cal_resistors_2",
    "b_hi_cal_resistors_3",
    "b_hi_cal_resistors_4",
    "b_hi_xcal_cone",
    "b_hi_collimator",
]


def get_data():
    data = h5py.File(f"{DATA_DIR}/fdq_eng.h5")
    eng = h5py.File(f"{DATA_DIR}/fdq_eng_new.h5")
    sdf = h5py.File(f"{DATA_DIR}/fdq_sdf_new.h5")

    grt_arr = data["fdq_eng"]["en_analog"]["grt"]

    grts = {}
    offsets = {}

    # "Binary time"
    # ADT Time, in units of 100ns since 1858-11-17
    bin_time = data["fdq_eng"]["ct_head"]["time"] * (100 * u.ns)

    reord = np.argsort(bin_time)

    stat_word_9 = eng["en_stat/stat_word_9"][()]
    filters = stat_word_9 == 16185

    filters = filters[reord]

    ind0 = 0
    for i in range(len(names_en_analog_grt)):
        grts[names_en_analog_grt[i]] = grt_arr[:, ind0 : ind0 + lens_grt[i]].flatten()
        grts[names_en_analog_grt[i]] = grts[names_en_analog_grt[i]][reord]
        offsets[names_en_analog_grt[i]] = ind0
        ind0 += lens_grt[i]
    time = bin_time[reord]

    time = time.to("s")

    grt_times = {}

    grt_names = [
        "xcal_tip",
        "skyhorn",
        "refhorn",
        "ical",
        "dihedral",
        "bol_assem_1",
        "bol_assem_2",
        "bol_assem_3",
        "bol_assem_4",
        "mirror",
        "cal_resistors_1",
        "cal_resistors_2",
        "cal_resistors_3",
        "cal_resistors_4",
        "xcal_cone",
        "collimator",
    ]

    # Recall that the full list from the original python port is
    #  'xcal_tip',  'skyhorn', 'refhorn',       'ical',      'dihedral',
    #  'bol_assem', 'mirror',  'cal_resistors', 'xcal_cone', 'collimator',
    # and bol_assem and cal_resistors are both blocks of four.


    # 5--8 are bol_assem
    # 10--13 are calibration resistors

    offset_ind = np.arange(16)

    ifg_time_offset = 36 * u.s
    for side in ["a", "b"]:
        for i, grt in enumerate(grt_names):
            if (grt == "xcal_tip" or grt == "collimator") and side == "b":
                continue
            not_ok = grts[f"{side}_lo_{grt}"] == -9999
            grts[f"{side}_lo_{grt}"][not_ok] = np.nan
            not_ok = grts[f"{side}_hi_{grt}"] == -9999
            grts[f"{side}_hi_{grt}"][not_ok] = np.nan

            not_ok = grts[f"{side}_lo_{grt}"] == -9999
            grts[f"{side}_lo_{grt}"][not_ok] = np.nan
            not_ok = grts[f"{side}_hi_{grt}"] == -9999
            grts[f"{side}_hi_{grt}"][not_ok] = np.nan

            grts[f"{side}_lo_{grt}"][filters] = np.nan
            grts[f"{side}_hi_{grt}"][filters] = np.nan

            # This ordering is an artifact of the time-domain multiplexing, likely wires being plugged in a non-ideal order.
            if (grt == "collimator") or (grt == "xcal_cone"):
                grt_times[f"{side}_lo_{grt}"] = time + 16 * u.s + ifg_time_offset
                grt_times[f"{side}_hi_{grt}"] = time + ifg_time_offset
            else:
                grt_times[f"{side}_lo_{grt}"] = time + ifg_time_offset
                grt_times[f"{side}_hi_{grt}"] = time + 16 * u.s + ifg_time_offset

            grt_times[f"{side}_lo_{grt}"] += offset_ind[i] * u.s
            grt_times[f"{side}_hi_{grt}"] += offset_ind[i] * u.s

    return grts, grt_times


def test_plot():
    import matplotlib.pyplot as plt




    grt = "cal_resistors_1"
    grt = "bol_assem_2"
    grt = "ical"
    current = "lo"
    side = "a"

    grts, grt_times = get_data()
    interpolators = get_interp(grts, grt_times)

    time = grt_times[f"{side}_{current}_{grt}"]

    inds = (np.arange(len(time)) > 108_000) & (np.arange(len(time)) < 110_000)
    #inds = (np.arange(len(time)) > 100_000) & (np.arange(len(time)) < 102_000)

    plt.plot(
        grt_times[f"{side}_{current}_{grt}"][inds],
        grts[f"{side}_{current}_{grt}"][inds],
        label=current,
    )

    current = "hi"
    plt.plot(
        grt_times[f"{side}_{current}_{grt}"][inds],
        grts[f"{side}_{current}_{grt}"][inds],
        label=current,
    )
    plt.legend()

    current = "lo"
    side = "a"

    plt.figure()

    plt.plot(
        grt_times[f"{side}_{current}_{grt}"][inds],
        grts[f"{side}_{current}_{grt}"][inds],
        "o",
        label=current,
    )

    t = grt_times[f"{side}_{current}_{grt}"][inds]
    t_lin = np.linspace(t.min(), t.max(), 10 * len(t))
    plt.plot(t, interpolators[f"{side}_{current}_{grt}"](t), label='Interpolator')
    plt.legend()
    plt.savefig(f"engineering_timing/output/{grt}_{side}_{current}.png")


def get_interp(grts, grt_times):
    # Get interpolator dictionary
    from scipy.interpolate import interp1d

    interpolators = {}
    for key in grt_times.keys():
        t = grt_times[key]
        T = grts[key]
        f = interp1d(t, T)
        interpolators[key] = f
    return interpolators

def test_offsets():
    grts, grt_times = get_data()
    # inds = [109_500, 109_600]
    inds = [527_240, 527_340]

    grt = "bol_assem_4"
    grt2 = "bol_assem_1"
    plt.plot(grt_times[f'a_lo_{grt}'][inds[0]:inds[1]], grts[f'a_lo_{grt}'][inds[0]:inds[1]]/np.nanmax(grts[f'a_lo_{grt}'][inds[0]:inds[1]]), label=f'a_lo_{grt}')
    plt.plot(grt_times[f'b_lo_{grt}'][inds[0]:inds[1]], grts[f'b_lo_{grt}'][inds[0]:inds[1]]/np.nanmax(grts[f'b_lo_{grt}'][inds[0]:inds[1]]), label=f'b_lo_{grt}')

    plt.plot(grt_times[f'a_lo_{grt2}'][inds[0]:inds[1]], grts[f'a_lo_{grt2}'][inds[0]:inds[1]]/np.nanmax(grts[f'a_lo_{grt2}'][inds[0]:inds[1]]), label=f'a_lo_{grt2}')
    plt.plot(grt_times[f'b_lo_{grt2}'][inds[0]:inds[1]], grts[f'b_lo_{grt2}'][inds[0]:inds[1]]/np.nanmax(grts[f'b_lo_{grt2}'][inds[0]:inds[1]]), label=f'b_lo_{grt2}')

    plt.legend()
    plt.savefig(f"engineering_timing/output/{grt}_offsets_vs_{grt2}.png")

if __name__ == "__main__":
    # test_plot()
    test_offsets()
