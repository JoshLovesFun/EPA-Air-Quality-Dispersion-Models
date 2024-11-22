#   AirportProc program of EMSHAP

# Provide the model for which this data is being processed(ASPEN or ISC)
setenv MODEL ASPEN

# Emissions type (AR for area, MV for mobile)
setenv EMISTYPE MV

################## FOLLOWING 4 KEYWORDS ARE ONLY FOR NON-POINT INVENTORIES
# Name of Temporal ALlocation Factor File
setenv TAFFILE   

# Name of Spatial Surrogate reference file
setenv SURRXREF  

# Name of SIC to SCC cross-reference file
setenv SIC2SCC   

# Name of MACT to SCC cross-reference file
setenv MACT2SCC  
###########################################################################
# Define all directories

# Point Source Input/Output
setenv POINT  /data/work17/DPM_2004/

# County-level (Mobile or NON-point Source) Input/Output
setenv COUNTY /data/work17/DPM_2004/

# Reference files
setenv REFDIR /data/work17/DPM_ANCILLARY/

# Define all input files

# Point source inventory
setenv INPOINT 

# Input County-level (Mobile or Non-point source) inventory
setenv INCOUNTY dpm_2004

# Airport allocation cross-reference file
setenv AIRPXREF airportxref99_OCT03

# Airport allocation factor file prefix
setenv AP_AF efm_apaf

# Define output files

# Output Point source inventory
setenv OUTPOINT pt_dpm99

# Output County-level (Mobile or Non-point source) inventory
setenv OUTCNTY county_dpm99

# Set add2pt flag to 1 in order to add allocated airport emission to input point source inventory
# Set flag to 0 to create output file containing airport emissions only
setenv ADD2PT 0

cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/COPAX.sas COPAX_DPM.sas
time sas COPAX_DPM -work /data/work17
