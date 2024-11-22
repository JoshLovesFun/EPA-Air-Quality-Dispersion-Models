#   COPAX: COunty Point and Airport eXtraction program of EMSHAP

# Provide the model for which this data is being processed (ASPEN or ISC)
setenv MODEL ASPEN

# Emissions type (AR for area, MV for mobile)
setenv EMISTYPE MV

############################################################################
### FOLLOWING 5 KEYWORDS ARE ONLY FOR NON-POINT INVENTORIES ################
############################################################################
# Name of Temporal Allocation Factor File
setenv TAFFILE   

# Name of Spatial Surrogate reference file
setenv SURRXREF

############################################################################
### FOLLOWING 3 KEYWORDS ARE ONLY USED FOR 1996 NON-POINT INVENTORY ########
############################################################################
# Name of SIC to SCC cross-reference file
setenv SIC2SCC

# Name of MACT to SCC cross-reference file
setenv MACT2SCC  NONE

# Name of SCC to AMS cross-reference file: used in NTI only
setenv SCC2AMS
############################################################################
### FOLLOWING 6 KEYWORDS ARE USED ONLY WHEN MODEL = ISC ####################
############################################################################
# Airport parameters file for modeling as ISC area source
setenv ISCAREA 

# Define deflt ISCST3 Airport release parameters for airports not in ISCAREA
# Length (meters) of X side of rectangle for ISCST3 area sources
setenv DEFXLEN 
# Length (meters) of Y side of rectangle for ISCST3 area sources
setenv DEFYLEN 
# Orientation angle (deg from north) of rectangle for ISCST3 area sources
setenv DEFANGLE 
# Release Height (meters) above ground for ISCST3 area sources
setenv DEFRELHT 
# Initial vertical dimension (meters) of plume for ISCST3 area sources
setenv DEFINPLM 
############################################################################
# Define all directories

# Point source 
setenv POINT  /data/work16/EMSHAP/V3TEST/

# County-level (Mobile or NON-point Source) Input/Output
setenv COUNTY /data/work16/EMSHAP/INVENTORY_NIF99/NONROAD/

# Reference file
setenv REFDIR /vail2aspen/dyntel/EMSHAP/ANCILLARY_V3/

# Define all input files

# Point source inventory
setenv INPOINT 

# Input County-level (Mobile or Non-point source) inventory
setenv INCOUNTY non99_oct21

# Airport allocation cross-reference TEXT file
setenv AIRPXREF airportxref

# Airport allocation factor file SAS prefix
setenv AP_AF ap_af

# Define output files

# Point source inventory
setenv OUTPOINT pt_nonrd99_aspen_ap

# Output County-level (Mobile or Non-point source) inventory
setenv OUTCNTY nonrd99_aspen_ap

# Set add2pt flag to 1 in to add allocated (airport) emissions to input
# point source inventory.  Set flag to 0 to create output file containing 
# allocated (airport) emissions only
setenv ADD2PT 0

cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/COPAX.sas COPAX_nif99nonASPEN.sas
sas COPAX_nif99nonASPEN -work .
