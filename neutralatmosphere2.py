
from simpleneutratmos import SimpleNeutralAtmos as SNA
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import quad
from skaero.atmosphere import coesa
import math

#if __name__ == '__main__':
class NeutralAtmosphere2:

    heights = np.arange(1, 30.5, 0.5)
    def __init__(self,heights):
        self.heights = heights


    def getwaterparpsr(self, temp_k):
        e1_mmhg = math.exp(20.386 - (5132/temp_k))
        e1_pa = e1_mmhg*133.322
        return e1_pa

    def getbuckwaterparpsr(self, temp_k):
        if temp_k >= 273.15:
            e2_kpa = 0.61121 * math.exp((18.678 - (temp_k / 234.5)) * (temp_k / (257.14 + temp_k)))
        else:
            e2_kpa = 0.61121 * math.exp((23.036 - (temp_k / 333.7)) * (temp_k / (279.82 + temp_k)))
        e2_pa = e2_kpa * 1000
        return e2_pa


    def gethTprho(self, height_km):
        """
        US Standard atmosphere, 1976
        :param height_km: height in km
        :return: height in m, temperature in K, p in Pa, rho in kg/m^3
        """
        h, T, p, rho = coesa.table(height_km * 1000)
        return h,T,p,rho

    def getrefractivity(self, t, rho, e):
        """

        :param t: temperature in K
        :param p: pressure in Pa
        :param rho: density in kg/m^3
        :param e: water vapor partial pressure in Pa
        :return: Refractivity based on Thayer, Smith, Weintraub model
        """

        k1 = 77.60  # [K/hPa]
        k2_dash = 22.1 # [K/hPa]
        k3 = 370100 # [K^2/hPa]
        R0 = 8.31434 # universal gas constant [J mol^-1 K^-1 ]
        M_dry = 28.9644 # molecular mass of dry air [kg/kmol]
        Zw = 1 # compressibility factor of wet air, considered to be =1

        N_hyd = (k1 * R0 * rho) / M_dry
        N_wet = ((k2_dash * 0.01*e / t) + (k3 * 0.01*e / t**2)) * Zw**-1
        N_total = N_hyd + N_wet
        return N_total, N_hyd, N_wet

    def getSaastdelay(self, T, p, e, z=0):
        """

        :param T: surface temperature [K]
        :param p: Total surface pressure [Pa]
        :param e: partial water vapor pressure [Pa]
        :param z: zenith angle [rad], default = 0
        :return: tropospheric delay as per Saastamoinen model [m]
        """

        p_mbar = p / 100  # convert pressure from Pascal to mbar
        e_mbar = e / 100  # convert pressure from Pascal to mbar
        delrho_saast = (2277e-6/math.cos(z))*(p_mbar + ((1255/T) + 0.005)*e_mbar - 1.16*(math.tan(z))**2) # Tropospheric delay based on Saastamoinen model
        return  delrho_saast

    def getResponse(self):
        ht_m = []
        T_k = []
        p_pa = []
        e_pa = []
        #pd_pa = []
        rho_kgm3 = []
        deltrop_m = []
        N = []
        for ht in np.nditer(self.heights):
            height, temp, psr, density = self.gethTprho(ht)
            e1 = self.getwaterparpsr(temp)
            refractivities = self.getrefractivity(temp, density, e1)[0]
            delays = self.getSaastdelay(temp, psr, e1, 0)
            ht_m.append(height)
            T_k.append(temp)
            p_pa.append(psr)
            rho_kgm3.append(density)
            e_pa.append(e1)
            N.append(refractivities)
            deltrop_m.append(delays)

        ht_m = np.array(ht_m)
        T_k = np.array(T_k)
        p_pa = np.array(p_pa)
        rho_kgm3 = np.array(rho_kgm3)
        e_pa = np.array(e_pa)
        N = np.array(N)
        deltrop_m = np.array(deltrop_m)

        self.responsedf = pd.DataFrame({'height_m'     : ht_m,
                                   'temperature_K'  : T_k,
                                   'refractivity'  : N,
                                   'pressure_Pa'  : p_pa,
                                   'watparpsr_Pa': e_pa,
                                   'density': rho_kgm3,
                                   'tropdelay' : deltrop_m
                              })
        #
        # plt.plot(ht_m, N, 'r--')
        # plt.ylabel("Refractivities (N units)")
        # plt.xlabel("Heights (m)")
        # plt.grid()
        # plt.show()
        #
        # plt.plot(heights, T_k, 'r--')  # heights, p_pa,'b--', heights, rho_kgm3, 'g--'
        # plt.grid()
        # plt.show()

        return self.responsedf
