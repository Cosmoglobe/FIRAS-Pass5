"""
This is meant as the "main" calibration script, where we iterate over all of the different things
that need fitting for the calibration data. These will also need to be fit for the sky data, but 
that will be later. The data model is given by (more in detail in each of the scripts):
d = G * ΔG * N_sweeps * F[E_f * B_f * (S_k,XCAL * a_f,XCAL * e^(-ifφ) + sum_k a_f,k P_f(T_k + ΔT_k) + H_f + G_f)] + n

There are three main "things" that need to be fit.
- The emissivities. These need to be fit per detector and per mode. There is one emissivity curve
per element in the optical path of the instrument and each should have size 512 (if fit in IFG
space) or 257 * 2 (if fit in spectrum space; real and imaginary parts). In total, this is
4 channels * 4 modes * 7 emitters per channel (this includes the bolometer for that channel and the
"XCAL", which is actually the spectral response of the dihedral) * 512 or 257 * 2 points.

- Corrections to the thermometer readings. These need to be fit to each of the elements in the
optical path and should be fit (I believe) for each 4 IFGs (i.e. each stroke of the mirror, for the
4 detectors). These should be weighed down by adding a term to the chi2 that penalizes large
deviations from the expected thermometer readings. 

- Constants and things that have less effect on the overall shape. These are
MULTIPLICATIVE:
- The phase. Needs to be fit for each 4 IFGs to start with, but it is likely it is constant until
there is a big change in the system (i.e. SAA crossings or mission split events).
- The gain. Needs to be fit once for the whole mission for each of the gain values.
- 9 bolometer parameters, R0, T0, G1, beta, rho, C1, C3, Jo, Jg, based on the published model. These
need to be fit per channel and per mode.
ADDITIVE:
- 5 vibration parameters, Δ1, Δ2, q, ɑ, β, based on the published model.
These need to be fit for the whole mission per mode.
- 2 harmonic amplitudes, ε1, ε2. These need to be fit for the whole mission per mode.

The exact equations for each etc are in their own scripts.
"""

# first we start with fitting the emissivities
