from simpleneutratmos import SimpleNeutralAtmos as sna
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import quad
import math

if __name__ == '__main__':

    heights = np.arange(1,40.5,0.5)   #height of shells in km
    H_TOP = 40
    snaobj = sna()
    refractivities = []
    shell_nums = []
    for ht in np.nditer(heights):
        shell_nums.append(math.ceil(ht))    # shell number based on 1km cell widths
        n_ht = snaobj.getrefractivity(ht)   # calculate a refractivity for each shell
        refractivities.append(n_ht)
    shell_nums = np.array(shell_nums)
    refractivities = np.array(refractivities)

    ztds = []
    for ht in np.nditer(heights):
        ztd_ht = quad(snaobj.getrefractivity,ht,H_TOP)[0]
        ztds.append(ztd_ht)
    ztds = np.array(ztds)

    df = pd.DataFrame ({'shell_num' : shell_nums,
                        'point_height': heights,
                        'refractivity': refractivities,
                        'zenith_delay': ztds})    # save data in a dataframe
    df.head()

    plt.plot (refractivities, heights, 'r--')
    plt.xlabel("Refractivities (N units)")
    plt.ylabel("Heights (km)")
    plt.grid ()
    plt.show ()



