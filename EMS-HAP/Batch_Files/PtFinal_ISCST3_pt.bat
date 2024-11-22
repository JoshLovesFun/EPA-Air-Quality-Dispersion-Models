#Point Source Processing - Final Format - For ISCST3 data Processing 
#      Assigns source groups for ISCST3
#      Produces ISCST3 - formatted text files

# Provide the model for which the data is being processed (must be ISC)
setenv MODEL ISC

# Provide 1-character model-run identifier.  This ensures that ISCST3 
# contains unique source ID's when all EMS-HAP output are fed into it.
# "M"OBIL6.2 2003 RUN
# "P"oint sources    
setenv RUN_ID X

# Provide directory paths:

#   path for the SAS input dataset
setenv IN_DATA /data/work17/ISC_tract_test/pt/
 
#   path for the SAS output dataset
setenv OUTDATA /data/work17/ISC_tract_test/pt/

#   path for the reference SAS datafiles
setenv REFSAS /vail2aspen/dyntel/EMSHAP/ANCILLARY_NIF/

#   path for the reference text files
setenv REFFILES /vail2aspen/dyntel/EMSHAP/ANCILLARY_NIF/
 
#   path for the output files for input into ASPEN or ISC
setenv OUTFILES /data/work17/ISC_tract_test/pt/OUTP/

# Provide input and output SAS data set names

#   input SAS data set name
setenv INSAS temporal_test

#   output SAS dataset name
setenv OUTSAS isc_phl99pt_b

# Select the procedure to be used to assign source groups
# Set value to 1 for yes (or true) and 0 for no (or false)

# Assign source groups by source type (major or area): set value of DoSource
# to 1 for yes (or true) and 0 for no (or false)
setenv DOSOURCE 1

# Assign source groups by MACT categories: set value of DoMACT to 1 for yes
# (or true) and 0 for no (or false)
setenv DOMACT 0

# If using MACT categories, provide name of the text file containing the
# group assignments
setenv MACTGRP MACT_grp

# Assign source groups by SCCs: set value of DoSCC to 1 for yes (or true)
# and 0 for no (or false)
setenv DOSCC 0

# If using SCCs, provide the name of the text file containing the group
# assignments
setenv SCCGRP SCC6_grp

# Assign source groups by SIC: set value of DoSIC to 1 for yes (or true) and
# 0 for no (or false)
setenv DOSIC 0

# If using SICs, provide the name of the text file containing the group
# assignments
setenv SICGRP SIC_grp

# Provide a default group assignment (value between 00 and 99) for those
# sources not assigned by your selected procedure
setenv DFLTGRP 01

# Provide ancillary file that contains default particle distributions: 
# SAROAD, # of sizes, list of size distributions, list of mass fractions,
# list of densities, and liquid scaveng.
setenv DEFPART defpartPHILLY

# SCC-specific particle distribution file -put "NONE" if it doesn't exist
setenv SCCPART NONE

# Provide ancillary file that contains default gas deposition parameters: 
# SAROAD, diffusivity, alphas, Reac, Rsubm, Henry's coefficient
setenv DEFGAS defgasPHILLY

# Set to yes(1) if you want to use scavenging coefficients that may be
# included in DEFPART and DEFGAS files
setenv SCAVENG 1

# grid cell or census tract elevation data
setenv ELEVDAT philly-elev

# default elevation in meters (used only if ELEVDAT does not exist)
setenv DEFELEV 100

# Set to yes (1) to call the macro that writes the building dimension
# include files
setenv USEBLDG  1 

# Provide Southwest corner UTM coordinates (X_ORIG,YORIG) and number of 1x1
# km columns and rows
# SEE G:/USER/SHARE/PAL/PHILLY/Philly_ISCST3prep_JUL29.doc
setenv X_ORIG 432000
setenv Y_ORIG 4369000
setenv CELLSIZE 1000    
setenv MAXCOL 107   
setenv MAXROW 107   
# Give file name of tract info file and UTM zone
setenv TRCTINF
setenv REF_ZONE 

# Part of run-stream for ISC input
setenv OUTNAME PHL_pt_c

#  Write particle distrubution include files: 1 = particle data by source,
#  2 = particle data by pollutant
setenv PARTMETH 2

#cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/PtFinal_ISCST3_V3.sas PtFinal_ISCST3_PHL99_all.sas
sas PtFinal_test -work /data/work17
