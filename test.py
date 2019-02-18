import numpy as np

import scipy.io as sio


filename = '/Users/voyager/Aether/balloon-aether/stnKinDataWTCO1to150.mat'
data = sio.loadmat(filename)
a = sio.whosmat(filename)
data = data['stnKinData']
print("Got the data!")
