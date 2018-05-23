# balloonNap
Use a weather balloon to profile atmosphere using GNSS receivers

The idea is to compare signals from low elevation GNSS satellites and compare them with no-troposphere model signals to compute atmospheric profile at a given location. The GNSS receiver, however, is placed on a weather balloon.

I. The first part of the project is to simulate the flight trajectory of a weather balloon and generate the RINEX file for this trajectory. 

==SIGOG==
We start with a FORTRAN based software SIGOG (Simulated GPS Observation Generator) by by Elsa Mohino et al to simulate unbiased or error-controlled Global Positioning System (GPS) observations at a static location. The runSigog.py script is intended to make system calls to the SIGOGbcst.for code with given x,y,z ECEF-coordinates in the following sequence:

1. balloonTraj.py generates the simulated trajectory of the balloon. We use the ==pnuu/pyBalloon.py== code from GitHub for this.
2. runSigog.py reads the simulated trajectory and generates input.txt files for every epoch of the flight at a user-defined time interval.
3. runSigog.py then calls the SIGOGbcst.for FORTRAN code through system calls for each input file generated, and corresponding RINEX files are generated; one for each set of epochs with same (x,y,z) coordinates.
4. cleanUp.py combines all the RINEX files into a single file, and cleans up all the intermediate input.txt files.

More stuff to come. Feel free to contribute! Contact me at ga59cor@gmail.com


