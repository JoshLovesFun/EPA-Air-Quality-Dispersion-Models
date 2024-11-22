# CountyProc: NIF2.0 Diesel PM -includes PR/VI from 1996

#Provide model for which the data is being processed (ASPEN or ISC)
setenv MODEL  ASPEN

# Indicate whether you want a 'diet' run or not (Y=1, N=0)
setenv DIET 0

# AMProc RUN IDENTIFICATION INFORMATION
# Run identification for titles
setenv RUNID  'NIF 2.0 1999 Diesel Emissions';

# Description of emissions file
setenv EMISLABL 'NIF 2.0 1999 Diesel Emissions';

# Date identifying this run
setenv RUNDATE  012004

# Emissions type (AR for area, MV for mobile)
setenv EMISTYPE MV

# Label for output files
setenv USRLABEL dpm

# FILE DIRECTORIES
# Ancillary files directory
setenv INPFILES  /data/work17/DPM_ANCILLARY/

# Input emissions file directory
setenv INPEMISS  /data/work17/DPM_2004/

# Output files directory
setenv OUTFILES  /data/work17/DPM_2004/ASPENemis/

# INPUT FILES
# Input emissions file name prefix
setenv EMISFILE county_dpm99

# SAF file name prefix
setenv SAFFILE  SAFe

# Default SAF to be applied when SAF information is missing for a county with emissions
setenv DEFLTSAF 100

# TAF file name prefix
setenv TAFFILE  taff_hourlyV3dpmPRE

# Decay rates file name prefix
setenv INDECAY  indecay

#######################################################################
######### 2 HAP TABLE FILES
#######################################################################
#   HAP-General File
#######################################################################
###  General HAP Table
setenv GENHAP haptabl_nonroadGEN_toxwt
#######################################################################
###  Specific HAP Table. May be "NONE"
#######################################################################
setenv SPECHAP NONE
#######################################################################

# Spatial surrogate xref file name prefix
setenv SURRXREF surrxref99

# Spatial surrogate to surrogate description xref prefix (CSV -comma delimited): OPTIONAL input
setenv SURRDESC surrogate_codes_and_definitions

# Emissions bins file name prefix: Format depends on inventory
setenv EMISBINS  am_grp99PRE

# County urban/rural flag xref file name prefix
setenv CNTYUR   popflg99

# GROWTH and CONTROL: set to NONE or leave blank if you are not projecting
setenv PROJECT NONE
 
# QA and OUTPUT FILES
# SaveFile = 1 to save large SAS emissions file
setenv SAVEFILE 1

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
setenv WORK2  /data/work17/test2

cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/CountyProc.sas CountyProc_DPM.sas
time sas CountyProc_DPM -work  /data/work17
