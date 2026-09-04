"""
This script makes an image of the spectrum with imshow to serve as a background.
"""
import matplotlib.pyplot as plt
import numpy as np

import utils

bb_temp = np.load("./../output/data/bb_curve/ll_ss.npy")
freqs = np.linspace(60, 630, 100)
bb_curve = utils.planck(freqs, bb_temp)

plt.imshow(bb_curve[:, np.newaxis], aspect="auto", cmap="viridis")
plt.show()