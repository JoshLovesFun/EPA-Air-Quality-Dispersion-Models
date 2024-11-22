# CountyProc (former AMProc) batch file --- ISCST3 NON-POINT PHILLY, no GC

# Set DIET to yes (1) if you want to minimize CPU and disk space resources
setenv DIET 0

#Provide model for which the data is being processed (ASPEN or ISC)
setenv MODEL  ISC

# For ISCST3, conservation of annual emissions depends on number of days.
#  Leap year: 1=yes, 0= no
setenv LEAPYEAR 0

# AMProc RUN IDENTIFICATION INFORMATION
# Run identification for titles
setenv RUNID  'EMS-HAP 1999 NEI PHILLY NON-POINT'

# Description of emissions file
setenv EMISLABL '1999 PHILLY NON-POINT'

# Date identifying this run
setenv RUNDATE  032404

# Emissions type (AR for area, MV for mobile)
setenv EMISTYPE AR

# Label for output files
setenv USRLABEL PHL

# FILE DIRECTORIES
# Ancillary files directory
setenv INPFILES  /vail2aspen/dyntel/EMSHAP/ANCILLARY_NIF/

# Input emissions file directory
setenv INPEMISS  /data/work17/ISC_tract_test/

# Output files directory
setenv OUTFILES  /data/work17/ISC_tract_test/outp_test_isc/

# INPUT FILES
# Input emissions file name prefix
setenv EMISFILE testdat_phl

# SAF file name prefix: UPDATE w/ PHILLY surrogate info
setenv SAFFILE  psaf

# Default SAF:
# For NON-POINT processing: All Surrogates not assigned in COPAX are defaulted to this 
# For Mobile processing: All surrogates not assigned in SURRXREF are defaulted to this
setenv DEFLTSAF 100

# TAF file name prefix
setenv TAFFILE  taff-ISCfactorsV3_mob6_99

#######################################################################
######### 2 HAP TABLE FILES
#######################################################################
#   HAP-General File
#######################################################################
###  General HAP Table
setenv GENHAP haptabl_stationary_toxwtPHL
#######################################################################
###  Specific HAP Table. May be "NONE"
#######################################################################
setenv SPECHAP haptabl_SPEC
#######################################################################

# Spatial surrogate xref file name prefix: UPDATE w/ PHILLY surrogate info
setenv SURRXREF surrxref99

# Optional CSV file: Description of spatial surrogates -helpful for QA:
#   We used surrogate_def_and_codes_2ram.csv for NIF2.0 DRAFT
setenv SURRDESC surrogate_codes_and_definitions

# Emissions bins file name prefix
setenv EMISBINS  am_grp99PRE

# County urban/rural flag xref file name prefix :different format but same data as popflag99 for PHL domain
setenv CNTYUR   popflg99

# GROWTH and CONTROL: name of CSV file containing projection parameters -see V3 documentation
setenv PROJECT NONE

# QA and OUTPUT FILES
# Lsubsetp = 1 to subset to a pollutant
setenv LSUBSETP 0

# The pollutant code for subsetting to
setenv SUBSETP  98

# Lsubsetg = 1 to subset to a state
setenv LSUBSETG 0

# The 2-character state abbreviation for subsetting to
setenv SUBSETG  US

# Ldbg = 1 to turn on debugging prints
setenv LDBG  0

# The cell for debug prints (state|county|tract)
setenv ONECELL 41019010098

# Assign temporary work space directory
setenv WORK2  /data/work17/temp

# Assign UTM-X origin of the modeling grid in meters
setenv XORIG         432000

# Assign UTM-Y origin of the modeling grid in meters
setenv YORIG         4369000

# Assign the size (length) of each gridcell in meters
setenv CELLSIZE      1000

# Assign the prefix of the output SAS dataset
setenv ISCOUT   countyproc_nonpt99_outp

#cp /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/CountyProc.sas CountyProc_PHLnonpt991.sas
time sas CountyProc_test -work /data/work17
