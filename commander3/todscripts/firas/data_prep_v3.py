"""
Data processing script has to be re-written again, not matching eng_time/time now,
and instead interpolating over gmt to get the engineering data corresponding to the science data.
"""

import h5py
import numpy as np
import pandas as pd
import tables as tb
import math

# OPENING ORIGINAL DATA FILES
fdq_sdf = h5py.File("/mn/stornext/d16/cmbco/ola/firas/initial_data/fdq_sdf_new.h5")
fdq_eng = h5py.File("/mn/stornext/d16/cmbco/ola/firas/initial_data/fdq_eng_new.h5")

# PARSING THE H5 DATA INTO A PANDAS DATAFRAME TO MANIPULATE DATA EASIER
# decoding the gmt data
gmt_lh = np.array(fdq_sdf["fdq_sdf_lh/ct_head/gmt"]).astype(str).astype(int)
gmt_ll = np.array(fdq_sdf["fdq_sdf_ll/ct_head/gmt"]).astype(str).astype(int)
gmt_rh = np.array(fdq_sdf["fdq_sdf_rh/ct_head/gmt"]).astype(str).astype(int)
gmt_rl = np.array(fdq_sdf["fdq_sdf_rl/ct_head/gmt"]).astype(str).astype(int)

# getting the ifg data
ifg_lh = fdq_sdf["fdq_sdf_lh/ifg_data/ifg"]
ifg_ll = fdq_sdf["fdq_sdf_ll/ifg_data/ifg"]
ifg_rh = fdq_sdf["fdq_sdf_rh/ifg_data/ifg"]
ifg_rl = fdq_sdf["fdq_sdf_rl/ifg_data/ifg"]

xcal_pos_lh = fdq_sdf["fdq_sdf_lh/dq_data/xcal_pos"]
xcal_pos_ll = fdq_sdf["fdq_sdf_ll/dq_data/xcal_pos"]
xcal_pos_rh = fdq_sdf["fdq_sdf_rh/dq_data/xcal_pos"]
xcal_pos_rl = fdq_sdf["fdq_sdf_rl/dq_data/xcal_pos"]

a_hi_ical = fdq_eng["en_analog/grt/a_hi_ical"]

# getting each channel into its own df
df_lh = pd.DataFrame(
    {"gmt": gmt_lh, "ifg": list(ifg_lh), "xcal_pos": list(xcal_pos_lh)}
)
df_ll = pd.DataFrame(
    {"gmt": gmt_ll, "ifg": list(ifg_ll), "xcal_pos": list(xcal_pos_ll)}
)
df_rh = pd.DataFrame(
    {"gmt": gmt_rh, "ifg": list(ifg_rh), "xcal_pos": list(xcal_pos_rh)}
)
df_rl = pd.DataFrame(
    {"gmt": gmt_rl, "ifg": list(ifg_rl), "xcal_pos": list(xcal_pos_rl)}
)

# outer-join the two dataframes on gmt
merged_df = df_lh.merge(df_ll, on="gmt", how="outer", suffixes=("_lh", "_ll"))
merged_df = merged_df.merge(df_rh, on="gmt", how="outer", suffixes=("_ll", "_rh"))
merged_df = merged_df.merge(df_rl, on="gmt", how="outer", suffixes=("_rh", "_rl"))


# def clean_xcal_pos(row):
#     channel_exists = {}
#     channels = ["lh", "ll", "rh", "rl"]
#     for channel in channels:
#         if math.isnan(row[f"xcal_pos_{channel}"]):
#             channel_exists[channel] = False
#         else:
#             channel_exists[channel] = True

#     checking = False  # did it find an existing channel to check against?
#     same = True  # did it find one instance where they're not the same?

#     for i in range(len(channels)):
#         if channel_exists[channels[i]]:
#             checking = True
#             for j in range(i + 1, len(channels)):
#                 if channel_exists[channels[j]]:
#                     if row[f"xcal_pos_{channels[i]}"] != row[f"xcal_pos_{channels[j]}"]:
#                         same = False
#         if checking and same:
#             return row[f"xcal_pos_{channels[i]}"]

#     if not same:
#         return None

import math


def clean_xcal_pos(row):
    channels = ["lh", "ll", "rh", "rl"]

    # Filter out channels with non-NaN values
    valid_values = [
        row[f"xcal_pos_{channel}"]
        for channel in channels
        if not math.isnan(row[f"xcal_pos_{channel}"])
    ]

    # If there are no valid channels, return None
    if not valid_values:
        return None

    # Check if all valid values are the same
    if all(value == valid_values[0] for value in valid_values):
        return valid_values[0]
    else:
        return None


merged_df["xcal_pos"] = merged_df.apply(clean_xcal_pos, axis=1)
merged_df = merged_df.drop(
    columns=["xcal_pos_lh", "xcal_pos_ll", "xcal_pos_rh", "xcal_pos_rl"]
)
merged_df = merged_df[(merged_df["xcal_pos"] == 1) | (merged_df["xcal_pos"] == 2)]

print(
    merged_df["xcal_pos"][:10],
)
exit()

zero_list = [0] * 512
# filling out nans with zeros
for column in ["ifg_lh", "ifg_ll", "ifg_rh", "ifg_rl"]:
    merged_df[column] = merged_df[column].apply(
        lambda x: zero_list if (isinstance(x, float) and np.isnan(x)) else x
    )


# saving to a h5 file
with tb.open_file("./data/df_v2.h5", mode="w") as h5file:
    group = h5file.create_group("/", "df_data", "Merged Data")

    h5file.create_array(group, "gmt", merged_df["gmt"].values)
    h5file.create_array(group, "ifg_lh", np.stack(merged_df["ifg_lh"].values))
    h5file.create_array(group, "ifg_ll", np.stack(merged_df["ifg_ll"].values))
    h5file.create_array(group, "ifg_rh", np.stack(merged_df["ifg_rh"].values))
    h5file.create_array(group, "ifg_rl", np.stack(merged_df["ifg_rl"].values))
