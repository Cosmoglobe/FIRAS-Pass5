"""
Data processing script has to be re-written again, not matching eng_time/time now,
and instead interpolating over gmt to get the engineering data corresponding to the science data.
"""

import h5py
import numpy as np
import pandas as pd
import tables as tb
from scipy import interpolate
import time

from utils.my_utils import clean_xcal_pos, parse_date_string

# check how much time the script takes to run
start_time = time.time()

# OPENING ORIGINAL DATA FILES
fdq_sdf = h5py.File("/mn/stornext/d16/cmbco/ola/firas/initial_data/fdq_sdf_new.h5")
fdq_eng = h5py.File("/mn/stornext/d16/cmbco/ola/firas/initial_data/fdq_eng_new.h5")

# PARSING THE H5 DATA INTO A PANDAS DATAFRAME TO MANIPULATE DATA EASIER
# decoding the gmt data
gmt_lh = np.array(fdq_sdf["fdq_sdf_lh/ct_head/gmt"]).astype(str)  # .astype(int)
gmt_ll = np.array(fdq_sdf["fdq_sdf_ll/ct_head/gmt"]).astype(str)  # .astype(int)
gmt_rh = np.array(fdq_sdf["fdq_sdf_rh/ct_head/gmt"]).astype(str)  # .astype(int)
gmt_rl = np.array(fdq_sdf["fdq_sdf_rl/ct_head/gmt"]).astype(str)  # .astype(int)

gmt_lh_parsed = []
gmt_ll_parsed = []
gmt_rh_parsed = []
gmt_rl_parsed = []

for gmt_nb in gmt_lh:
    gmt_lh_parsed.append(parse_date_string(gmt_nb))
for gmt_nb in gmt_ll:
    gmt_ll_parsed.append(parse_date_string(gmt_nb))
for gmt_nb in gmt_rh:
    gmt_rh_parsed.append(parse_date_string(gmt_nb))
for gmt_nb in gmt_rl:
    gmt_rl_parsed.append(parse_date_string(gmt_nb))

# getting the ifg data
ifg_lh = fdq_sdf["fdq_sdf_lh/ifg_data/ifg"]
ifg_ll = fdq_sdf["fdq_sdf_ll/ifg_data/ifg"]
ifg_rh = fdq_sdf["fdq_sdf_rh/ifg_data/ifg"]
ifg_rl = fdq_sdf["fdq_sdf_rl/ifg_data/ifg"]

# getting the xcal_pos data
xcal_pos_lh = fdq_sdf["fdq_sdf_lh/dq_data/xcal_pos"]
xcal_pos_ll = fdq_sdf["fdq_sdf_ll/dq_data/xcal_pos"]
xcal_pos_rh = fdq_sdf["fdq_sdf_rh/dq_data/xcal_pos"]
xcal_pos_rl = fdq_sdf["fdq_sdf_rl/dq_data/xcal_pos"]

# getting each channel into its own df
df_lh = pd.DataFrame(
    {
        "gmt": gmt_lh_parsed,
        "ifg": list(ifg_lh),
        "xcal_pos": list(xcal_pos_lh),
    }
)
df_ll = pd.DataFrame(
    {
        "gmt": gmt_ll_parsed,
        "ifg": list(ifg_ll),
        "xcal_pos": list(xcal_pos_ll),
    }
)
df_rh = pd.DataFrame(
    {"gmt": gmt_rh_parsed, "ifg": list(ifg_rh), "xcal_pos": list(xcal_pos_rh)}
)
df_rl = pd.DataFrame(
    {"gmt": gmt_rl_parsed, "ifg": list(ifg_rl), "xcal_pos": list(xcal_pos_rl)}
)

tolerance = pd.Timedelta(seconds=2)
# getting all possible gmts so that we can do an outer join using merge_asof
unified_timestamps = (
    pd.DataFrame(pd.concat([df_lh["gmt"], df_ll["gmt"], df_rh["gmt"], df_rl["gmt"]]))
    .drop_duplicates()
    .sort_values("gmt")
    .reset_index(drop=True)
)

# outer-join the dataframes on gmt
merged_df = pd.merge_asof(
    unified_timestamps, df_lh, on="gmt", direction="nearest", tolerance=tolerance
)
merged_df = pd.merge_asof(
    merged_df,
    df_ll,
    on="gmt",
    direction="nearest",
    suffixes=("_lh", "_ll"),
    tolerance=tolerance,
)
merged_df = pd.merge_asof(
    merged_df,
    df_rh,
    on="gmt",
    direction="nearest",
    suffixes=("_ll", "_rh"),
    tolerance=tolerance,
)
merged_df = pd.merge_asof(
    merged_df,
    df_rl,
    on="gmt",
    direction="nearest",
    suffixes=("_rh", "_rl"),
    tolerance=tolerance,
)


# CLEANING XCAL_POS AND CONSTRAINING FOR ONLY 1 AND 2
# making sure xcal_pos is the same within each record
merged_df["xcal_pos"] = merged_df.apply(clean_xcal_pos, axis=1)
merged_df = merged_df.drop(
    columns=["xcal_pos_lh", "xcal_pos_ll", "xcal_pos_rh", "xcal_pos_rl"]
)
merged_df = merged_df[(merged_df["xcal_pos"] == 1) | (merged_df["xcal_pos"] == 2)]

# initializing the xcal temp column so then we only add the relevant xcal values
# merged_df["xcal"] = np.nan

# ENGINEERING DATA
a_hi_ical = fdq_eng["en_analog/grt/a_hi_ical"]
a_lo_ical = fdq_eng["en_analog/grt/a_lo_ical"]
b_hi_ical = fdq_eng["en_analog/grt/b_hi_ical"]
b_lo_ical = fdq_eng["en_analog/grt/b_lo_ical"]
ical = np.mean([a_hi_ical, a_lo_ical, b_hi_ical, b_lo_ical], axis=0)

a_hi_xcal_cone = np.array(fdq_eng["en_analog/grt/a_hi_xcal_cone"])
a_hi_xcal_tip = np.array(fdq_eng["en_analog/grt/a_hi_xcal_tip"])
a_lo_xcal_cone = np.array(fdq_eng["en_analog/grt/a_lo_xcal_cone"])
a_lo_xcal_tip = np.array(fdq_eng["en_analog/grt/a_lo_xcal_tip"])
b_hi_xcal_cone = np.array(fdq_eng["en_analog/grt/b_hi_xcal_cone"])
b_lo_xcal_cone = np.array(fdq_eng["en_analog/grt/b_lo_xcal_cone"])
xcal = np.mean(
    [
        a_hi_xcal_cone * 0.9 + a_hi_xcal_tip * 0.1,
        a_lo_xcal_cone * 0.9 + a_lo_xcal_tip * 0.1,
        b_hi_xcal_cone,
        b_lo_xcal_cone,
    ],
    axis=0,
)

gmt_eng = np.array(fdq_eng["ct_head/gmt"]).astype(str)
gmt_eng_parsed = []
for gmt in gmt_eng:
    gmt_eng_parsed.append(parse_date_string(gmt))

# make engineeering data df
df_eng = pd.DataFrame(
    {
        "gmt": gmt_eng_parsed,
        "ical": list(ical),
        "xcal": list(xcal),
    }
)

# precompute timestamps for merged_df and df_eng
science_times = merged_df["gmt"].apply(lambda x: x.timestamp()).values
engineering_times = df_eng["gmt"].apply(lambda x: x.timestamp()).values

# initialize list for interpolated values
ical_new = []
xcal_new = []

# iterate over science times and find engineering indices within 1 minute
for target_time in science_times:
    # find engineering times within 1 minute of the current science time - TODO: change the constraint?
    lower_bound = target_time - 60
    upper_bound = target_time + 60

    # get indices of engineering times within 1-minute bounds using searchsorted
    start_idx = np.searchsorted(engineering_times, lower_bound, side="left")
    end_idx = np.searchsorted(engineering_times, upper_bound, side="right")
    indices = range(start_idx, end_idx)

    # check if indices are available
    if len(indices) > 1:
        # interpolate only if there are multiple points in the range
        f = interpolate.interp1d(
            engineering_times[indices],
            df_eng["ical"][indices],
            fill_value="extrapolate",
        )
        ical_new.append(f(target_time))
        # if int(merged_df.loc[science_times == target_time]["xcal_pos"]) == 1: # TODO: fix this
        f = interpolate.interp1d(
            engineering_times[indices],
            df_eng["xcal"][indices],
            fill_value="extrapolate",
        )
        xcal_new.append(f(target_time))
    else:
        ical_new.append(np.nan)
        xcal_new.append(np.nan)

merged_df["ical"] = ical_new
merged_df["xcal"] = xcal_new

# drop rows without ical data
merged_df = merged_df[merged_df["ical"].notna()]

# set xcal to nan if xcal_pos is 2
merged_df[merged_df["xcal_pos"] == 2]["xcal"] = np.nan

zero_list = [0] * 512
# filling out ifg nans with zeros
for column in ["ifg_lh", "ifg_ll", "ifg_rh", "ifg_rl"]:
    merged_df[column] = merged_df[column].apply(
        lambda x: zero_list if (isinstance(x, float) and np.isnan(x)) else x
    )

# converting gmt to string so we can save
gmt_str = merged_df["gmt"].dt.strftime("%Y-%m-%d %H:%M:%S").to_numpy(dtype="S")

print(merged_df.tail())
print(merged_df[merged_df["xcal_pos"] == 1].tail())

# saving to a h5 file
with tb.open_file("./data/df_v6.h5", mode="w") as h5file:
    group = h5file.create_group("/", "df_data", "Merged Data")

    h5file.create_array(group, "gmt", gmt_str)
    h5file.create_array(group, "ifg_lh", np.stack(merged_df["ifg_lh"].values))
    h5file.create_array(group, "ifg_ll", np.stack(merged_df["ifg_ll"].values))
    h5file.create_array(group, "ifg_rh", np.stack(merged_df["ifg_rh"].values))
    h5file.create_array(group, "ifg_rl", np.stack(merged_df["ifg_rl"].values))
    h5file.create_array(group, "xcal_pos", merged_df["xcal_pos"].values)
    h5file.create_array(group, "ical", merged_df["ical"].tolist())

end_time = time.time()
print(f"Time taken: {(end_time - start_time)/60} minutes")
