import calibration.electronics as electronics
import numpy as np
import calibration.bolometer as bolometer
import utils

def harmonics(Y, h2, h3):
    H2 = h2 * Y[::2]  # Harmonic 2 is applied to every other point
    H3 = h3 * Y[::3]  # Harmonic 3 is applied to every third point
    return H2 + H3

def vibrations(delta1, delta2, alpha, beta, a_k, channel, mode):
    """
    We follow the following equation:
    G_f = Δ (ξ - f) a_(ξ - f),k P_(ξ - f)(T_k)
    where Δ = Δ1 * cos(ɑ) + Δ2 * cos(β)
    eps is the frequency of the vibration. eps - f can be negative in the main term, but it is done
    in absolute value for the emissivities and the Planck function.
    """
    xi = 57.57 # Hz

    f = utils.generate_frequencies(channel, mode, 257)

    delta = np.pi * (delta1 * np.cos(alpha) + delta2 * np.cos(beta))
    G = delta * (xi - f) * a_k * planck(T_k + dT_k)

def data_model(gain, dgain, sweeps, channel, adds_per_group, mode, bol_cmd_bias, bol_volt,
temperatures, R0, T0, G1, beta, rho, C1, C3, Jo, Jg, a, dT, phi, h1, h2):
    Txcal = temperatures[0]
    # Tbol comes from the temperature array
    Tk = temperatures[1:7]  # the six emitter temperatures are the next six elements in the temperatures array
    Tbol = temperatures[6]  # the bolometer temperature is the last element in the temperatures array
    bolparams = (R0, T0, G1, beta, rho, C1, C3, Jo, Jg)
    a_xcal = a[0]
    a_k = a[1:7]
    dT_xcal = dT[0]
    dT_k = dT[1:7]

    f = utils.generate_frequencies(channel, mode, 257)

    # Y is the data after it has been divided by the gain and the sweeps and Fourier transformed
    Y = (electronics.etfunction(channel, adds_per_group) *
         bolometer.btfunction(channel, mode, bol_cmd_bias, bol_volt, Tbol, parameters=bolparams) *
         (a_xcal * utils.planck(f, (Txcal + dT_xcal)) * np.exp(-1j * f * phi) +
          np.sum(a_k * utils.planck(f, Tk + dT_k)))) #+vibrations()))

    Y = Y + harmonics(Y, h1, h2)  # Add harmonics to the signal
    
    d = gain * dgain * sweeps * np.fft.fft(Y, axis=0)

    return d