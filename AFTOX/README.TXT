     The following report describes the AFTOX model.  Version 4.1 is
similar to 4.0 except for some minor corrections.

    Kunkel, B.A. (1991) AFTOX 4.0 - The Air Force Toxic Chemical
    Dispersion Model - A User's Guide, PL-TR-91-2119, ADA246726.

     This report may be obtained from NTIS (703)487-4650 or from DTIC
(703)274-7633.  It can also be obtained from the Phillips Laboratory
while supplies last.  Call (617)377-2982.

     Any questions concerning the AFTOX program can be addressed to:
        
           Bruce Kunkel
           17 Galloway Rd
           Chelmsford, MA 01824
           (508)256-2793


1.  AFTOX COMPUTER PROGRAM OPERATION

     This file provides the program structure and execution instructions
for the AFTOX computer program.  The code is written in the GW-BASIC
programming language.  Although AFTOX was initially designed for Air Force
use, it is applicable to all spills at all locations.  Before operating the
model, the spill location must be entered into SD.DAT (see Section 1.1.1).

1.1  File Structure

     The AFTOX program contains five component program files and six data
files, four for input data and two for output.  Three ancillary files are
used for editing the data in three of the input data files.  CHAIN statements
are used to connect the various program files.  A list of the files is shown
in Table 1.
 
    Table 1. List of files in AFTOX
__________________________________________________________________
PROGRAM FILES

     AFTOX.EXE    Introduction
     DSP1.EXE     Defines chemical properties and the
                     meteorological conditions 
     DSP2.EXE     Defines source conditions (that is, emission
                     rate, spill duration, and spill area)
     DSPHP.EXE    Defines source conditions for buoyant plume from 
                     a stack
     DSP3.EXE     Computes 1) the hazard area, 2) the maximum
                     concentration, and 3) the concentration at a
                     given point and time

DATA FILES          

     SD.DAT      Station data
     CH.DAT      Chemical name and data, and toxic limits
     EVAP.DAT    Chemical data for Vossler evaporation model
     AFT.DAT     File for storing all input and output data when 
                 printer is off
     CONCXY.DAT  File for storing x, y positions of concentration
                 contour
     DEVICE.DAT  Contains information on computer and screen type

ANCILLARY FILES          

     SETUP.EXE   Establishes computer and screen type
     SDFIL.EXE   Edits station data file (SD.DAT)
     CHFIL.EXE   Edits chemical data file (CH.DAT)
_____________________________________________________________________

1.1.1 ANCILLARY FILES

        SETUP.EXE

     Before running AFTOX for the first time, SETUP.EXE should be run to
establish the type of computer and screen.  When running SETUP.EXE, a screen
as shown in Figure 1 will appear.  Enter the appropriate code for the computer
and monitor types.  The appropriate information will be stored in DEVICE.DAT.




__________________________________________________________________
                                 A F T O X

                 AIR FORCE TOXIC CHEMICAL DISPERSION MODEL

                        SET UP FOR GRAPHICS PROGRAM

        Enter code for type of graphics card/monitor.

         E = EGA/VGA    C = CGA     N = Other or no graphics   ? E

        Enter code for monitor type                               
                                        
         C = COLOR   M = MONOCHROME     ? C
__________________________________________________________________                 
 
Figure 1.  Screen display for SETUP routine.

        SDFIL.EXE 
  
   The SDFIL.EXE file is used to set up the station data file, SD.DAT.
SDFIL allows one to view, delete, add, and edit data in SD.DAT.  The file
may contain one or more stations.

     To run SDFIL, type SDFIL and press <ENTER>.  The computer will
display a screen with several options as shown in Figure 2.  Choose
the appropriate option and proceed as instructed.

____________________________________
1--display or print station data
2--set up new SD.DAT file
3--edit data
4--add station
5--delete station
6--quit
----------------------------
Choose one of the above
____________________________________            
Figure 2. List of options in SDFIL.


     The following is a list of data required for each station stored in
SD.DAT.

           Station name
           1-Metric  2-English Units
           Standard Deviation of Wind Direction (Y/N)
           Standard Deviation Averaging Time (min)
           Latitude (deg)
           Longitude (deg)
           Surface Roughness (cm)
           Height of Wind Measurement (m or ft)
           Time Difference (Greenwich-local standard)
           Station Elevation (m or ft)

     Stations that are located reasonably close to each other (<50 km) do
not have to be listed separately because the latitude, longitude, and elevation
are not highly sensitive parameters.  However, if the stations have different
surface roughnesses or wind measurement heights then they must be listed
separately.

     The units refer to the meteorological and distance parameters.  The user
will have the option of entering either metric or English units for the spill
rate or quantity.

     As a guide for inputting the proper surface roughness, a table, as shown
below, is automatically displayed on the screen.  The surface roughness need 
not be restricted to one of the values shown in Figure 3, but it is recommended
that the value fall between 0.5 and 100 cm. 

___________________________________________________________________
                    TERRAIN DESCRIPTION       SURFACE ROUGHNESS(cm) 
   
        SNOW, NO VEGETATION, MUD FLATS, NO OBSTACLES            0.5
        RUNWAY, OPEN FLAT TERRAIN, GRASS, FEW ISOLATED OBSTACLES 3
        LOW CROPS, OCCASIONAL LARGE OBSTACLES                    10
        HIGH CROPS, SCATTERED OBSTACLES                          25
        PARKLAND, BUSHES, NUMEROUS OBSTACLES                     50
        REGULAR LARGE OBSTACLE COVERAGE (SUBURB, FOREST)        100
___________________________________________________________________
Figure 3.  Roughness length as a function of terrain type.

        CHFIL.EXE

     The CHFIL.EXE file is used to view, make changes, deletions or additions
to the chemical data file, CH.DAT.  The current CH.DAT file contains 130
chemicals, listed in alphabetical order.  

     To run CHFIL, type CHFIL and press <ENTER>.  The computer will display a 
screen with several options as shown in Figure 4.  Choose the appropriate 
option and proceed as instructed.

_______________________________________
1--display or print data
2--edit exposure limits for specific chemical
3--edit data for specific chemical
4--add chemical
5--delete chemical
6--quit
-----------------------
Choose one of the above
_______________________________________
Figure 4.  List of options in CHFIL.

     In viewing the chemical data, several options exist: 1) data may be 
displayed on the screen or sent to the printer; 2) either the chemical numbers
and names or all the chemical data may be displayed or printed; and 3) chemical
data for a particular chemical or for all the chemicals may be displayed or
printed.

     The exposure limits are used only as a guide for the user.  The user must
input the exposure limits when running AFTOX, but may default to the STEL
values.  The values given in the file are, for the most part, the 1990-91
threshold limit values as defined by the American Conference of Governmental 
Industrial Hygienists (ACGIH).      

     When editing the chemical data under option 3, the user must re-enter all
the data for a particular chemical, even though he may be changing only one
value.  The old data is displayed on the screen to make it easy for the user to
re-enter the data.  One word of caution--the names hydrazine,
monomethylhydrazine(MMH), dimethylhydrazine(UDMH), and nitrogen tetroxide must
be spelled exactly because the model must recognize the name, and not the
number, in order for it to use the Vossler evaporation model. 

     The data for each chemical consist of the following:

          Chemical number
          Chemical name
          Time weighed average (TWA) exposure limits (PPM and mg/m3)
          Short term exposure limits (STEL) (PPM and mg/m3)
          Molecular weight
          Boiling temperature (K)
          Critical temperature (K)
          Critical pressure (atm)
          Critical volume (cm3/g-mole)
          Vapor pressure constants (3)
          Liquid density constants (2)
          Molecular diffusivity constants (2)

     If the chemical is a gas at all expected ambient temperatures, then 
only information up through the boiling temperature is needed.

     If the chemical is a liquid, vapor pressure information must be in 
the chemical file.  The exception is for the five chemicals, hydrazine, 
monomethylhydrazine(MMH), dimethylhydrazine(UDMH), aerozine-50, and nitrogen
tetroxide, that use the Vossler evaporation model (1989).  The chemical data
required for the Vossler model, including the vapor pressure data, are included
in EVAP.DAT.  All other chemicals use either the Shell's SPILLS evaporation 
model (Fleischer, 1980) or the Clewell model (1983).

     The vapor pressure may be defined in one of three ways: 1) by the 
Antoine equation, in which case three constants are required, 2) by the 
Frost-Kalkwarf equation in which two constants are required, or 3) by 
directly inputting the vapor pressure in atm.  The model determines which 
method to use by the number of constants in the file.  To use the 
Frost-Kalkwarf equation, the critical temperature and critical pressure 
must be known. 
 
     The liquid density is required for all chemicals in the liquid state, 
including those using the Vossler evaporation model.  The liquid density is 
defined in one of two ways: 1) by the Guggenheim equation, in which case two 
constants are required, or 2) by directly inputting the liquid density in 
g/cm3.  As with the vapor pressure, the model determines which method of 
defining the liquid density by the number of constants.  The Guggenheim 
equation requires both the critical temperature and critical volume.  If 
no density is entered, the model will assume a density of 1 g/cm3.

     The molecular diffusivity constants are the effective diameter of the 
molecule(A) and the energy of molecular interaction(J).  The diffusivity 
is required for the Shell evaporation model.  If either of these two constants
is not available, the model defaults to the Clewell evaporation model.

1.1.2  DATA FILES

     There are six data files, three of which have been discussed in
Section 1.1.1.  The remaining three are EVAP.DAT, CONCXY.DAT, and AFT.DAT.

        EVAP.DAT

     EVAP.DAT is the chemical data file for the Vossler evaporation
model.  The chemicals listed in this file are hydrazine,
monomethylhydrazine(MMH), dimethylhydrazine(UDMH), and nitrogen
tetroxide.  The chemical data include the following:

         Molecular weight
         Boiling temperature
         Freezing temperature
         Molecular diffusion volume
         Constants and applicable temperature ranges for:
            Vapor viscosity
            Vapor heat capacity
            Vapor thermal conductivity
            Heat of vaporization
            Saturation vapor pressure
            Liquid thermal conductivity

     Data may be changed using an edit program.  However, if a new
chemical is added to the file, the code in DSP2.BAS must be changed
so that the new chemical will be recognized.            

        CONCXY.DAT

     CONCXY.DAT is an output data file containing information on
the X, Y coordinates of up to three requested concentrations.  Data
stored in the file include the concentration of interest, either
mg/m3 or ppm, the time since release for an instantaneous or finite
continuous release, and the contour half width for various
distances downwind in either m or ft.  The half width has 20 m
resolution.  The actual half width will be within 20 m but always
less than the calculated.   

1.2  Setting up AFTOX

     AFTOX may be run directly from the diskette, but if your
computer has a hard disk it is recommended that the program be
transferred to the hard disk.  Create a directory on your hard disk
where you would like the program to reside (e.g., C:>MD AFTOX41). 
Then copy the files from the diskette to the hard disk (e.g.,
A:>COPY *.* C:>AFTOX41). 

     To tailor the program to your particular computer, run
SETUP.EXE.  Specify whether your machine is VGA/EGA, CGA, or no
graphics.  If your machine has Hercules graphics, you will have to
choose no graphics.  Also, specify whether your monitor is color or
monochrome.  Once SETUP has been run, there will be no need to run
it again unless you transfer the program to a different type of
computer.

     Information on your location must be stored in the station
data file, SD.DAT, by running SDFIL.EXE.  Once you input the
station data into SD.DAT, there will be no need to run SDFIL.EXE
again unless you change your location.

     You are now ready to run AFTOX.

1.3  Running AFTOX

     1.3.1  GENERAL

     AFTOX is a very user friendly program.  Simply proceed through
the program, answering the questions as you go along.  Default
values are frequently given <in brackets>.  If you wish to go with
the default value, simply press <ENTER>.  The program has the
unique feature of being able to back up by entering <999> <ENTER>,
thus eliminating the need to start over if you accidently entered
the wrong data.  The exception is that you can not back up into the
previous file (e.g., you can not back up from DSP2 into DSP1). 



     1.3.2  PRINTING AND STORING DATA

     Type AFTOX at the DOS prompt and press <ENTER>.  This will start the 
program execution.  If you are using a printer, and wish the plume plot to be 
sent to the printer, you must load the appropriate PSC utility program before 
you start AFTOX.  If you haven't, the program will remind you, and you will 
have to start over.  If the printer is off, then the input and output data are
stored in AFT.DAT for viewing or printing out at a later time.  Each time AFTOX 
is run, the data in AFT.DAT is erased.  If you wish to save the data in 
AFT.DAT, the file can be saved by renaming the file (e.g., C:>AFTOX41:ren 
aft.dat spill#1.dat). 

     1.3.3  STATION DATA

     If there is more than one station in the station data file (SD.DAT), 
you will be asked to enter the appropriate station.  The data stored in 
SD.DAT has been discussed in Section 1.1.1.  

     1.3.4  DATE AND TIME

     The computer date and time are displayed.  If the spill is for a 
different date or time, a new date and time may be entered.  The program 
converts the date to a Julian date which it uses inconjunction with the 
time, latitude and longitude to determine the solar elevation, and 
subsequently, the solar insolation and surface heat flux.

     1.3.5  TYPE OF RELEASE

     AFTOX handles five types of releases:

        Continuous gas
        Continuous liquid
        Instantaneous gas
        Instantaneous liquid
        Continuous buoyant stack

The user has a choice of a continuous, instantaneous, or buoyant release.  
An instantaneous release is defined as occuring over a 15 sec period.  A 
continuous release is any release occuring over a period greater than 15 sec. 
For the continuous and instantaneous releases, the model determines whether 
it is a gas or liquid, based on whether the air temperature is above or below 
the boiling point of the chemical.

     1.3.6  CHEMICAL DATA

     A list of 130 chemicals are displayed on the screen.  The user enters
the appropriate number.  If the chemical of interest is not listed, the 
user may press <ENTER> at the end of the list of chemicals.  He will then 
be asked the name of the chemical and its molecular weight.  If the
molecular weight is entered, the model asks for the vapor pressure
in mm Hg.  If the vapor pressure is known, the model uses Clewell's
formula for determining the evaporation rate.  If either the molecular 
weight or vapor pressure is not known, the model assumes the worst case; 
that is, the evaporation rate is equal to the spill rate.  Also, if the 
molecular weight is not entered, the concentrations must be in mg/m3 since 
conversion to ppm is not possible without knowing the molecular weight.

     For a buoyant plume release, the model bypasses the chemical list 
and data file and asks only for the molecular weight.  Again, if the 
molecular weight is not known, concentrations must be in mg/m3.

     1.3.7  METEOROLOGICAL DATA

     The meteorological data consist of the following:

        Air temperature
        Wind direction
        Wind speed
        Standard deviation of wind direction and time over which it
          is determined (optional)
        Cloud amount
        Predominant cloud category 
        Ground condition - dry, wet, snow covered (daytime only)
        Inversion base height
 
    The air temperature is a necessary input parameter but does not have a 
large influence on the results.  If a temperature reading is not available, 
a reasonable guess would be sufficient. 

     Calm winds are not allowed.  If a zero wind speed is entered, the model 
will adjust the speed to either 0.5 m/sec or 1 kt, depending on which units 
the user chooses.  The model converts the wind speed measurement, whose height 
is specified in the station file SD.DAT, to a 10-m height wind speed.  The 10-m 
wind speed is used in all of the calculations, and therefore the user should be 
aware that the plume may move downwind at a faster rate than the measured wind 
speed would indicate.

     If the standard deviation of wind direction is normally available, 
it is so indicated in SD.DAT along with the time over which it is determined.
If the standard deviation is not available at the time that the model is 
being run, simply press <ENTER> and the model will default to using the wind 
speed and solar conditions for computing stability and the corridor width.

     The cloud amount is entered in eighths.  There are three cloud
categories to choose from - high, middle, and low.  If there are two 
cloud layers present, the operators should use the layer with the larger 
cloud amount.  If there are two layers with equal cloud amount, the operator 
should choose the lower cloud layer.  The cloud type is not a factor at night.

     There are three ground types to choose from - wet, dry, snow covered.
Ifin doubt as to whether the ground is wet or dry, the user should choose wet,
which will result in a more conservative hazard distance.  If the air 
temperature is 20C (68F) or greater, the model assumes no snow cover.

     If the base of an inversion is below 500 m (2000 ft), the height of its 
base is entered.  Inversion heights greater than 500 m (2000 ft) may be entered 
but will have no effect on the plume.  If the base of the inversion is below 
50 m (164 ft), the model assumes no inversion but does assume a stability 
parameter of 6.  When there is an inversion, the model assumes that the 
pollutants are trapped below its base.  In the rare case that the release is
above the inversion, the pollutants remain above the inversion.

     1.3.8  ROUGHNESS LENGTH AT SPILL SITE

     For a non-buoyant release, the surface roughness length at the
spill site must be entered.  For a buoyant plume, it is assumed
that the elevated plume is minimally affected by the surface
roughness, which is set at 3 cm.  The user has the option to call
up the table of roughness lengths (Figure 3) for different types of
terrain.  When uncertain as to the appropriate roughness length for
the spill site, the user should choose a lower value, which will
produce longer hazard distances.  Input roughness lengths below 0.5
cm or greater than 100 cm are set at 0.5 or 100 cm, respectively. 

     1.3.9  SOURCE INFORMATION

     The source information required varies depending on the type
of spill - continuous, instantaneous, gas, or liquid.

    Continuous Gas Release

     For a continuous gas release, the following information is required:

          Height of leak above ground (m, ft)
          Emission rate through rupture (kg/min, lb/min)
          Total time of release (min)

     The emission rate is assumed constant for the total time of
the spill.  If the duration of the release is finite then the total
amount released is displayed.

     Continuous liquid release

     For a continuous liquid release, the following information is required:

          Spill rate through rupture (kg/min, m3/min, lb/min, gal/min)
          Total time of spill (min)
          Spill area (m2, ft2) - otherwise default value is used
          Pool temperature - for those chemicals using the Clewell
                     evaporation model (default = air temperature)

     The height of the leak is not entered since it is assumed that
the liquid spills to the ground.  The default spill area is based
on the volume spilled and the assumption of a 1-cm deep pool.  For
an ongoing continuous spill, the volume spilled is based on a 10-min spill.
The default spill area is displayed but if the user has information on the 
size of the spill area, he may over-ride the default value.  Based on the 
input data and the chemical properties, the model computes the evaporation 
(emission) rate into the atmosphere and the total evaporation (release) time 
for a finite continuous release.

     Instantaneous gas release

     For an instantaneous gas release, the following information is required:

          Release height (m, ft)
          Amount released (kg, lb)

     The model assumes a cylindrical volume source in which the height is 
equal to the radius.  The initial volume of the spill is a function of the 
amount released.

     Instantaneous liquid release

     For an instantaneous liquid release, the following information is 
required:

          Amount spilled (kg, m3, lb, gal)
          Spill area (m2, ft2) -  otherwise default value is used
          Pool temperature -  for those chemicals using the Clewell
                      evaporation model (default = air temperature)

     The default spill area is based on the volume spilled and the assumption 
of a 1-cm deep pool.  Based on the input data and the chemical properties, the
model computes the emission rate into the atmosphere and the total time of 
release.  

     1.3.10  WORST CASE SCENERIO

     When the initial spill alert is sounded, quite often very little source 
information is available.  In this case, AFTOX 4.1 has the option of computing
a worst case scenerio.  The toxic corridor length calculation is based on 
Air Force Regulation 355-1/AWSSUP1, Attachment 1, dated 24 September 1990.  
This regulation states that the corridor length is equal to the current wind 
speed in knots x 6,000.  This represents the distance in feet that the plume 
will travel in one hour.

     The corridor width is a function of the wind speed, or standard deviation 
of wind direction if available.  The computation of the width is described in 
Section 1.3.15.  The exception is that their is no width adjustment for the 
duration of the spill since the duration may be unknown.

     Having computed the worst case scenerio, the user can then proceed and 
enter the source information when it becomes available without re-entering 
the meteorological information.  However, if you need to change the name of 
the chemical or the source type (continuous, instantaneous), then you must 
start over.

     1.3.11 CONTINUOUS BUOYANT PLUME RELEASE

     For a continuous buoyant plume release from a stack, the following 
information is required:

     Molecular weight (if available)
     Emission rate (kg/min or lbs/min)
     Elapsed time of emissions (min)
     Stack height (m or ft)
     Gas stack temperature (C or F)
     Volume flow rate (m3/min or ft3/min)
     
     With these data, the model calculates the equilibrium plume
height and the downwind distance at which the plume reaches the
equilibrium height.  If the equilibrium plume height is higher than
the inversion, the plume height is adjusted down to the inversion
height.  If the stack height is higher than the inversion the
program terminates because the input meteorological conditions most
likely do not apply above the inversion.  If molecular weight is
not available, concentrations will be in mg/m3.

     1.3.12  CONCENTRATION AVERAGING TIME

     The user must specify the concentration averaging time.  For
continuous releases, the default value is 15 min, which corresponds
to the short term exposure limit (STEL) which is defined as a 15
minute time-weighted average exposure.  For releases of less than
15 min duration, the default averaging time is equal to the release
time.  For instantanteous gas releases, the averaging time is 1
min.  The user can not enter averaging times less than 1 min. 
Also, the averaging time cannot be greater than the release time
since changes in the concentrations during the averaging period are
not taken into account.  The averaging time affects the dispersion
coefficients such that the longer the averaging time the greater
the dispersion coefficients, thus resulting in shorter and wider
plumes.  The averaging time and dispersion coefficents are related
by the 1/5 power law. 

     1.3.13  ELAPSED TIME SINCE START OF SPILL

     For instantaneous and finite continuous releases, the user
must specify the elapsed time since the start of the spill.  For an
ongoing continuous release, the model defaults to a sufficiently
large elapsed time to assure a steady state condition (maximum
hazard distance).  For an instantaneous liquid release or a finite
continuous release, the default elapsed time is equal to the
release time.  Except for short duration spills (a few minutes),
the default time would normally give the greatest hazard distance. 
For instantaneous gas releases, the default elapsed time is
arbitrarily set at 10 min.  For small releases, the plume may
dispersed within 10 min, in which case the user may wish to enter
a shorter elapsed time.
 
    1.3.14  TYPES OF OUTPUT

     The user can specify one of three types of output.

        1. Toxic corridor plot
        2. Concentration at specified location and time
        3. Maximum concentration at given height and time

     The one exception is when the no graphics option is chosen in
SETUP, the corridor plot is replaced with a printout of the hazard
distance for a given concentration(s).

     Toxic corridor plot

     If the user chooses not to use the default concentration, he
may specify up to three contours in any order.  The concentrations
can be specified in either mg/m3 or ppm.  The exception is when the
molecular weight is not known the concentration is in mg/m3.  The
default concentration is the short term exposure limit (STEL) in
ppm, if available, otherwise the time weighted average (TWA)
exposure limit.  If neither is available then the user must enter
one or more concentrations.  The user must also input the height of
interest or choose the default height of 2 m (6 ft).

     AFTOX computes the Y position of the concentration of interest for 
increasing values of X.  X varies in increments of 100 to 400 m depending 
on how rapidly Y is varying in the X direction.  For slowly varying Y, X 
increments approach 400 m.  The model starts the Y computation at 100 m 
from the source.  However, if the centerline concentration at 100 m is less 
than the specified concentration of interest, calculations start at 10 m 
and progress outward in 10 m increments.  Plumes < 10 m in length will not 
be plotted.  The model first computes the concentration at the centerline 
and then moves outward in the Y direction in 20 m increments when X>100 m, 
and 2 m increments when X<100 m.  The point at which the computed concentration
is less than the concentration of interest defines the Y position.  The X, Y
positions are stored in CONCXY.DAT along with the centerline concentrations. 

     The model first computes the appropriate scale by calculating centerline 
concentrations for the specified time and height.  The concentration contours 
are then plotted on the screen as computations are taking place.  The contours 
are plotted in order of increasing concentrations no matter in what order they 
are entered.  These contours represent average distances, which means that 50 
percent of the time the actual distance may be greater than shown and 50 
percent of the time may be less than shown.  The 90 percent hazard area for 
the lowest concentration is outlined on the contour plot and represents the 
area within which the plume is confined 90 percent of the time.  This 90 
percent area represents the toxic corridor.

     The plume plot represents the position and size of a plume at the 
specified time after the start of the release.  Depending on the time after 
release, this may not be the maximum distance that the plume will extend 
downwind, expecially if it is an instantaneous release.  The 90 percent hazard
area, however, represents the area at the time when the plume reaches its 
maximum distance downwind.  It may or may not be at the specified time after 
release.  For more discussion on the confidence limits, the reader should refer 
to Section 1.3.15.
 
     Once the contour plot is completed, the user may proceed to make any of 
the changes listed below, obtain a printer plot, run another case, or terminate 
the program.

        1.  time and meterological conditions
        2.  source conditions
        3.  concentration averaging time
        4.  elapsed time since start of spill
        5.  concentration contours
        6.  height of interest
        7.  scale

     Concentration at a specified location and time

     If this option is chosen, the user must enter the downwind and
crosswind distances, the height, and the time after start of the release.
The model then computes the concentration for that particular point and 
time.  The user may then change the location and/or time, choose another 
option, run another case, or terminate the program.

     Maximum concentration at a specified height and time

     This option will give the user the maximum concentration and its location
for a specified height and time.  The user enters the height of interest and 
the time from start of release.  If it is a continuous spill that is still 
taking place and the specified height is the release height, the maximum 
concentration will be at the source.  In this case, AFTOX computes the 
concentration at 30 m from the source.  If the specified height is different 
than the release height, or if the time after start of release is greater
than the duration of the release, the maximum concentration will most likely 
occur at a distance greater than 30 m from the source.  The location of the 
maximum concentration is determined within an accuracy of +/- 5 m along the 
X axis, and of course would be located on the centerline of the plume at the 
specified height.  When completed, the user may change the height and/or time, 
choose another option, run another case, or terminate the program.

     1.3.15  CONFIDENCE LIMITS
     
     The model predicts the mean hazard distance.  However,
operationally, one would like to be at least 90 percent confident
that the actual hazard distance will not exceed the predicted
distance.  The model's concentration contour plot also shows the 90
percent confidence level hazard area, or toxic corridor.  The
evaluation study described in Kunkel (1988) concluded that the
predicted hazard distance must be multiplied by 2.1 to be 90
percent certain that the actual  will not exceed the predicted
distance.  Earlier versions of AFTOX showed the 90 percent hazard
area only for continuous releases.  In AFTOX 4.0, the 90 percent
hazard area is shown for all spills even though the 2.1 factor was
derived from continuous release data.  It should be pointed out
that in deriving the 90 percent hazard area, it is assumed that the
input data is correct.  Errors in the input data, such as the wind
speed and source strength will enlarge this area.  Zettlemoyer
(1990) examined the effect that data input uncertainties have on
the concentration uncertainties using a Monte Carlo simulation
technique. He look at wind speed, emission rate, spill height, and
the horizontal and vertical dispersion coefficients.  He concluded
that errors in the wind speed had greater effect on the
concentration uncertainty than the other parameters.  The greatest
uncertainties occurred within one kilometer of the source.

     The method used to determine the width of the hazard area for
the 90 percent confidence level remains similar to the method used
by the Air Weather Service (see Kahler et al (1980)).  If the
measured wind is less than 1.8 m/sec, (<3.5 kt), the hazard area is
a circle of radius equal to 2.1 times the predicted hazard
distance.  If the standard deviation of wind direction (SD) is
known and the wind speed is greater than 1.8 m/sec, then the toxic
corridor width (W) is equal to:

     W = 6 SD         

The measured SD is adjusted by the one-fifth power law to the time
duration of the spill, up to a maximum of one hour.  If the width
is calculated to be less than 30 degrees, it is set at 30 degrees.

     If SD is not known, then the following rules apply:

     1) For neutral or stable conditions, that is, a stability
parameter of 3.5 or greater, the width is equal to 90 degrees for measured
winds of 1.8 to 5.15 m/sec (3.5 to 10 kt).  For winds greater than 5.15
m/sec (10 kt), the width is equal to 45 degrees. 

     2) For unstable conditions, the width is a function of the stability, 
parameter (STB) as shown in the following equation.
    
     W = 165 - 30 STB                                      

The width will vary from 60 degrees for neutral conditions to 150 degrees
for very unstable conditions.

REFERENCES

Clewell, H.J. (1983) A Simple Formula for Estimating Source Strengths from 
Spills of Toxic Liquids, ESL-TR-83-03.

Fleischer, M.T. (1980 SPILLS - An Evaporation/Air Dispersion Model for Chemical 
Spills on Land, Shell Development Company, PB 83109470.

Kahler, J.P., Curry, R.G., and Kandler, R.A. (1980) Calculating Toxic 
Corridors, AWS/TR-80/003, ADA101267.

Kunkel B.A. (1988) User's Guide for the Air Force Toxic Chemical Dispersion 
Model (AFTOX), AFGL-TR-88-0009, ADA199096.

Vossler, T.L. (1989) Comparison of Steady State Evaporation Models for Toxic 
Chemical Spills: Development of a New Evaporation Model, GL-TR-89-0319, 
ADA221752.

Zettlemoyer, M.D. (1990)  An Attempt to Estimate Measurement Uncertainty 
in the Air Force Toxic Chemical Dispersion (AFTOX) Model, Master of Science 
Thesis, The Florida State University. 

