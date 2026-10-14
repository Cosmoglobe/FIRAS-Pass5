"""
The emissivities need to be fit per channel and per mode. The main equation is
d = G * ΔG * N_sweeps * F[E_f * B_f * (S_k,XCAL * a_f,XCAL * e^(-ifφ) + sum_k a_f,k P_f(T_k + ΔT_k) + H_f + G_f)] + n
where the emissivities are the a_f,XCAL and a_f,k terms. The other terms are either known or fit in other scripts.
The emissivities are fit independently at each frequency, so the main thing to do is to set up the
design matrix and solve for the emissivities at each frequency. The design matrix is the Planck
spectrum for the XCAL followed by the Planck spectra for each of the six emitters.
"""

