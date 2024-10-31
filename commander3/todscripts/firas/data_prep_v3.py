"""
Data processing script has to be re-written again, not matching eng_time/time now,
and instead interpolating over gmt to get the engineering data corresponding to the science data.
"""

import h5py
import numpy as np
import pandas as pd
import tables as tb
from datetime import datetime, timedelta

from utils.my_utils import clean_xcal_pos, parse_date_string

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

for i in range(len(gmt_lh)):
    gmt_lh_parsed.append(parse_date_string(gmt_lh[i]))
for i in range(len(gmt_ll)):
    gmt_ll_parsed.append(parse_date_string(gmt_ll[i]))
for i in range(len(gmt_rh)):
    gmt_rh_parsed.append(parse_date_string(gmt_rh[i]))
for i in range(len(gmt_rl)):
    gmt_rl_parsed.append(parse_date_string(gmt_rl[i]))

gmt_lh_parsed = np.array(gmt_lh_parsed)
gmt_ll_parsed = np.array(gmt_ll_parsed)
gmt_rh_parsed = np.array(gmt_rh_parsed)
gmt_rl_parsed = np.array(gmt_rl_parsed)

# getting the ifg data
ifg_lh = fdq_sdf["fdq_sdf_lh/ifg_data/ifg"]
ifg_ll = fdq_sdf["fdq_sdf_ll/ifg_data/ifg"]
ifg_rh = fdq_sdf["fdq_sdf_rh/ifg_data/ifg"]
ifg_rl = fdq_sdf["fdq_sdf_rl/ifg_data/ifg"]

xcal_pos_lh = fdq_sdf["fdq_sdf_lh/dq_data/xcal_pos"]
xcal_pos_ll = fdq_sdf["fdq_sdf_ll/dq_data/xcal_pos"]
xcal_pos_rh = fdq_sdf["fdq_sdf_rh/dq_data/xcal_pos"]
xcal_pos_rl = fdq_sdf["fdq_sdf_rl/dq_data/xcal_pos"]

# getting each channel into its own df
# correcting for the 250 offset in the left channel data
df_lh = pd.DataFrame(
    {
        "gmt": gmt_lh_parsed - timedelta(microseconds=250),
        "ifg": list(ifg_lh),
        "xcal_pos": list(xcal_pos_lh),
    }
)
df_ll = pd.DataFrame(
    {
        "gmt": gmt_ll_parsed - timedelta(microseconds=250),
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

# outer-join the two dataframes on gmt
merged_df = df_lh.merge(df_ll, on="gmt", how="outer", suffixes=("_lh", "_ll"))
merged_df = merged_df.merge(df_rh, on="gmt", how="outer", suffixes=("_ll", "_rh"))
merged_df = merged_df.merge(df_rl, on="gmt", how="outer", suffixes=("_rh", "_rl"))


# CLEANING XCAL_POS AND CONSTRAINING FOR ONLY 1 AND 2
merged_df["xcal_pos"] = merged_df.apply(clean_xcal_pos, axis=1)
merged_df = merged_df.drop(
    columns=["xcal_pos_lh", "xcal_pos_ll", "xcal_pos_rh", "xcal_pos_rl"]
)
merged_df = merged_df[(merged_df["xcal_pos"] == 1) | (merged_df["xcal_pos"] == 2)]


# # ENGINEERING DATA
# a_hi_ical = fdq_eng["en_analog/grt/a_hi_ical"]
# a_lo_ical = fdq_eng["en_analog/grt/a_lo_ical"]
# b_hi_ical = fdq_eng["en_analog/grt/b_hi_ical"]
# b_lo_ical = fdq_eng["en_analog/grt/b_lo_ical"]
# ical = np.mean([a_hi_ical, a_lo_ical, b_hi_ical, b_lo_ical], axis=0)

# gmt_eng = np.array(fdq_eng["ct_head/gmt"]).astype(str).astype(int)

# # make engineeering data df
# df_eng = pd.DataFrame(
#     {
#         "gmt": list(gmt_eng),
#         "ical": list(ical),
#     }
# )

# # # make gmt data into datetime format
# # merged_df["gmt"], merged_df["gmt_rest"] = merged_df["gmt"].apply(parse_date_string)
# # df_eng["gmt"], df_eng["gmt_rest"] = df_eng["gmt"].apply(parse_date_string)
# df_eng["gmt"] = df_eng["gmt"].apply(parse_date_string)

# print(merged_df["gmt"].tail())
# print(merged_df["gmt_rest"].tail())

# quit()


# def get_times(row):
#     print(merged_df["gmt"][0])
#     if abs(merged_df["gmt"][0] - row["gmt"]).total_minutes() <= 1:
#         print(row["gmt"])


# df_eng.apply(get_times, axis=1)

# quit()

zero_list = [0] * 512
# filling out nans with zeros
for column in ["ifg_lh", "ifg_ll", "ifg_rh", "ifg_rl"]:
    merged_df[column] = merged_df[column].apply(
        lambda x: zero_list if (isinstance(x, float) and np.isnan(x)) else x
    )

print(merged_df.tail())

gmt_str = merged_df["gmt"].dt.strftime("%Y-%m-%d %H:%M:%S:%f").to_numpy(dtype="S")

# saving to a h5 file
with tb.open_file("./data/df_v4.h5", mode="w") as h5file:
    group = h5file.create_group("/", "df_data", "Merged Data")

    h5file.create_array(group, "gmt", gmt_str)
    h5file.create_array(group, "ifg_lh", np.stack(merged_df["ifg_lh"].values))
    h5file.create_array(group, "ifg_ll", np.stack(merged_df["ifg_ll"].values))
    h5file.create_array(group, "ifg_rh", np.stack(merged_df["ifg_rh"].values))
    h5file.create_array(group, "ifg_rl", np.stack(merged_df["ifg_rl"].values))
    h5file.create_array(group, "xcal_pos", merged_df["xcal_pos"].values)
