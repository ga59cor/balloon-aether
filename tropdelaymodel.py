from collections import namedtuple
import numpy as np
import math
import pandas as pd
import matplotlib.pyplot as plt
from scipy.integrate import quad


class TropDelayModel:
    """
    Class for defining tropospheric delay model using different functions
    """
    def __init__(self):
        Constants = namedtuple('Constants', ['H_0',  # scale height = 7.35km
                                             'N_0',  # avg val of N extrapolated to sea level = 315 refr. units
                                             'H_TOP'])  # height of the topmost shell of the neutral atmosphere = 40km
        self.constants = Constants(7.35, 315, 40)

    def gettropdelay(self):
        """
        Tropospheric propagation delay of GPS signals
        :return: dtrop
        """
        dtrop_wet = self.getwetdelay()
        dtrop_dry = self.gethystatdelay()
        dtrop = dtrop_wet + dtrop_dry
        return dtrop

    # def gethystatdelay(self):
    #     mapfnwt =
    #
    #
    # def getwetdelay(self):
    #     return wetdelay


