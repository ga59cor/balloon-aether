from collections import namedtuple
import math


class SimpleNeutralAtmos:
    """
    Class for simulating simple neutral atmosphere using an exponential model

    """

    def __init__(self):
        """
        Initialize class with constants to be used for calculations
        """
        Constants = namedtuple('Constants', ['H_0',     # scale height = 7.35km
                                             'N_0',     # avg val of N extrapolated to sea level = 315 refr. units
                                             'H_TOP'])  # height of the topmost shell of the neutral atmosphere = 40km
        self.constants = Constants(7.35, 315, 40)

    def getrefractivity(self, height):
        """
        Get refractivity based on a simple shell-based exponential model with topmost shell at H_TOP height.
        The refractivity is considered to be constant within shell boundaries. Shell-width=1km.
        :param height: height of a receiver from MSL
        :return: neutral atmosphere refractivity at the receiver
        """
        refractivity = 0
        if height <= self.constants.H_TOP:
            height = math.ceil(height)  # consider shell ht to be const = 1km, const N within shell
            refractivity = self.constants.N_0 * math.exp(-1 * height / self.constants.H_0)

        return refractivity

    def getslantdelay(self, elev, ztdelay):
        """
        Get slant delay based on elevation of a receiver from a GNSS satellite
        :param elev: elevation angle between a receiver and a GNSS satellite
        :param ztdelay: zenith tropospheric delay
        :return: slant tropospheric delay
        """
        cutoff = 0
        weight = self.mappingfn(elev, cutoff)
        stdelay = weight*ztdelay
        return stdelay

    @staticmethod
    def mappingfn(elevangle, cutoffangle=0):
        """
        Define a simple sine-based mapping function based on satellite elevation- and receiver cutoff angles
        :param elevangle: elevation angle between a receiver and a GNSS satellite = pi/2 - zenith angle [rad]
        :param cutoffangle: angle below which satellite signals are ignored, default 0
        :return: weight for slant delay
        """
        _mappingweight = 0
        if elevangle >= cutoffangle:
            _mappingweight = 1 / math.sin(elevangle)

        return _mappingweight
