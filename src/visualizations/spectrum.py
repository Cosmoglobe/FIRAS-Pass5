"""
This script makes an image of the spectrum with imshow to serve as a background.
"""
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LinearSegmentedColormap

import utils.my_utils as utils

bb_temp = np.load("./../output/data/bb_temp/ll_ss.npy")
freqs = np.linspace(60, 630, 1000)
bb_curve = utils.planck(freqs, bb_temp)

# colors = [(231, 126, 21), (245, 220, 101), (203, 223, 229), (67, 179, 222), (4, 75, 151)]
orange_colors = [(245, 220, 101), (231, 126, 21)]
# normalize colors
colors = [(r / 255, g / 255, b / 255) for r, g, b in orange_colors]

n_bin = 1000
cmap_name = 'my_list'
fig, ax = plt.subplots()
fig.subplots_adjust(left=0.02, bottom=0.06, right=0.95, top=0.94, wspace=0.05)
cmap = LinearSegmentedColormap.from_list(cmap_name, colors, N=n_bin)

im = ax.imshow(bb_curve[:, np.newaxis], origin='lower', cmap=cmap, aspect='auto')
fig.colorbar(im, ax=ax)

plt.savefig("./../output/plots/spectrum_background.png", dpi=300, bbox_inches="tight")
plt.show()