
from simpleneutratmos import SimpleNeutralAtmos as SNA
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import quad
import math

class NeutralAtmosphere1:
  # if __name__ == '__main__':

    def getData(self):
        heights = np.arange(1, 40.5, 0.5)  # height of shells in km
        H_TOP = 40
        snaobj = SNA()
        refractivities = []
        shell_nums = []

        for ht in np.nditer(heights):
            shell_nums.append(math.ceil(ht))  # shell number based on 1km cell widths
            n_ht = snaobj.getrefractivity(ht)  # calculate a refractivity for each shell
            refractivities.append(n_ht)

        shell_nums = np.array(shell_nums)
        refractivities = np.array(refractivities)

        ztds = []
        for ht in np.nditer(heights):
            ztd_ht = quad(snaobj.getrefractivity, ht, H_TOP)[0]
            ztds.append(ztd_ht)
        ztds = np.array(ztds)

        elev_angle = 45
        slt_45 = []
        for zd in np.nditer(ztds):
            std_zd = snaobj.getslantdelay(elev_angle, zd)
            slt_45.append(std_zd)
        slt_45 = np.array(slt_45)

        self.snadf = pd.DataFrame({'shell_num'     : shell_nums,
                           'point_height'  : heights,
                           'refractivity'  : refractivities,
                           'zenith_delay'  : ztds,
                           'slant_delay_45': slt_45})  # save data in a dataframe
        self.snadf.head()

        plt.plot(refractivities, heights, 'r--')
        plt.xlabel("Refractivities (N units)")
        plt.ylabel("Heights (km)")
        plt.grid()
        plt.show()

        plt.plot(ztds, heights, 'r--', slt_45, heights, 'b--' )
        plt.grid()
        plt.show()

        return self.snadf


