# CountyFinal: ISCST3 GRIDDED AREA/MOBILE Source Processing - Final Format 
#      Produces ISC -formatted text files

# Provide Model name
setenv MODEL ISC
# Provide 1-character model-run identifier.  This ensures that ISCST3
# contains unique source ID's when all EMS-HAP output are fed into it.
# A = Area (non-point) with NO airports -airports are processed separately
#  -see COPAX-nonroad batch file
setenv RUN_ID B

# Provide directory paths:

#   path for the SAS input dataset
setenv IN_DATA /olympia/tox/ISC_TRACT/OUTP/
 
#   path for the SAS output dataset
setenv OUTDATA /olympia/tox/ISC_TRACT/OUTP/

#   path for the reference text files
setenv REFFILES /vail2aspen/dyntel/EMSHAP/ANCILLARY_NIF/
 
#   path for the output files for input into ISC
setenv OUTFILES /olympia/tox/ISC_TRACT/OUTP/

# Provide input and output SAS data set names

#   input SAS data set name
setenv INSAS countyproc_ncsc

#   output SAS dataset name
setenv OUTSAS countyfinal_ncsc

# Provide ancillary file that contains default particle distributions: 
# SAROAD, # of sizes, list of size distributions, list of mass fractions,
# list of densities, and liquid scavenging 
# put "NONE" or leave blank if it does not exist is not desired
setenv DEFPART defpartPHILLY

# Provide ancillary file that contains default gas deposition parameters:
# SAROAD, diffusivity, alphas, Rx, Rsubm, Henry's coefficient
# put "NONE" or leave blank if it does not exist is not desired
setenv DEFGAS defgasPHILLY

# Set to yes (1) if you want to use scavenging coefficients that may be
# included in DEFPART and DEFGAS files
setenv SCAVENG 1

# grid cell elevation data
# put "NONE" or leave blank if it does not exist is not desired
setenv ELEVDAT philly-elev                                       

# default elevation in meters (used only if ELEVDAT does not exist)
setenv DEFELEV    

# Provide Southwest corner UTM coordinates (X_ORIG,YORIG) and number of 1x1
# km columns and rows
# X_ORIG AND Y_ORIG are for both ISC and ISCTRACT.  all others only for ISC
setenv X_ORIG 432000                  
setenv Y_ORIG 4369000                     
setenv CELLSIZE 1000            
setenv MAXCOL 107      
setenv MAXROW 107      

# name of tract file with vertices for ISCTRACT
setenv TRACTFILE                                         

# Reference UTM zone
setenv REF_ZONE 

# area source release heights in meters
setenv ARELHGT 2

# initial vertical dimension of the area source plume in meters
setenv AINPLUM 1

#cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3_MAY2004/CountyFinal.sas CountyFinal.sas
sas CountyFinal -work /olympia/tox/
