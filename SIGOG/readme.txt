This folder contains the SiGOG package, a software that simulates code and 
carrier phase observations. The folder includes the following files:

-readme.txt: this file contains general specifications about SiGOG

-SiGOGprcs.f: source file of SiGOG for precise ephemeris. Please note that 
when using a precise ephemeris, SiGOG needs orbits for a day before and one 
day after the actual simulating date.

-SiGOGbcst.f: source file of SiGOG for broadcast orbits.

-input.txt: input file example

-LICENSE: GNU General Public License. For more information on this license, 
please go to http://www.gnu.org/

It also contains example input/output data for SiGOGbcst.f (brdc1330.03n and
vbca1330.03o) and example input/output data for SiGOGprcs.f (igs12181.sp3,
igs12182.sp3, igs12183.sp3, vcba1330.03o).

Special Notes:
SiGOG also contains a function called seeber2, that accounts for the 
tropospheric delay, and can be used if so specified in the input file. The 
reason for the inclusion of this function is related to the software we have 
being using for processing the simulated observations: GPSpace point 
positioning software corrects for this delay always, so, in order to recover 
the original hypothetical receiver location with GPSpace, one must account 
for this delay.

Special note when using GPSpace software with precise orbits: GPSpace 
software has a bug and shows problems with satellite 13, regardless of its 
actual prn. Solution: generate the observation RINEX file without 
satellite 13.

SiGOG package generates GPS observations for antena reference point, this is, 
adding the antenna offsets specified in input file (dh,de,dn) to the monument 
position. It doesn't take into account the L1 and L2 phase center offsets of 
the GPS antenna nor the effect of the antenna phase pattern (phase center 
variations).

As SiGOG doesn't simulate any ionospheric effects, P1 and P2 observations 
are equal, as well as the L1 and L2 pair.

The souce code for SiGOG can also be found at the authors' website:
http://seneca.fis.ucm.es/emohino/SiGOG


