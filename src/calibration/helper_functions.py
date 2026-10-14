import calibration.electronics as electronics
import numpy as np
import calibration.bolometer as bolometer
import utils

def harmonics():


def vibrations():

def data_model(gain, dgain, sweeps, channel, adds_per_group, mode, bol_cmd_bias, bol_volt,
temperatures, R0, T0, G1, beta, rho, C1, C3, Jo, Jg, a, dT, phi):
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
    Y = electronics.etfunction(channel, adds_per_group) * 
    bolometer.btfunction(channel, mode, bol_cmd_bias, bol_volt, Tbol, parameters=bolparams) *
    (a_xcal * utils.planck(f, (Txcal + dT_xcal)) * np.exp(-1j * f * phi) +
    np.sum(a_k * utils.planck(f, Tk + dT_k)) + harmonics() + vibrations())
    
    d = gain * dgain * sweeps * np.fft.fft(Y, axis=0)

    return d