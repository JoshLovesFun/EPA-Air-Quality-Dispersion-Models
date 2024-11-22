# CountyProc: NIF3.0  Non-point

#Provide model for which the data is being processed (ASPEN or ISC)
setenv MODEL  ASPEN

# Indicate whether you want a 'diet' run or not (1=YES, 0=NO)
# extended SAS data will NOT be created if DIET option is chosen
setenv DIET 1

# CountyProc RUN IDENTIFICATION INFORMATION
# Run identification for titles
setenv RUNID  'NIF 3.0 1999 Non-Point Emissions';

# Description of emissions file
setenv EMISLABL 'NIF 3.0 1999 Non-Point Emissions';

# Date identifying this run
setenv RUNDATE  082903

# Emissions type (AR for area, MV for mobile)
setenv EMISTYPE AR

# Label for output files
setenv USRLABEL nonpt 

# FILE DIRECTORIES
# Ancillary files directory
setenv INPFILES  /vail2aspen/dyntel/EMSHAP/ANCILLARY_NIF/

# Input emissions file directory
setenv INPEMISS  /data/work16/EMSHAP/INVENTORY_NIF99/np2003/

# Output files directory
setenv OUTFILES  /data/work16/EMSHAP/INVENTORY_NIF99/np2003/ASPENemis/

# INPUT FILES
# Input emissions file name prefix
setenv EMISFILE nonpt99_aspen_ap

# SAF file name prefix
setenv SAFFILE  SAFe

# Default SAF applied when SAF information missing for a county w/ emissions
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
setenv GENHAP haptabl_stationary_188
#######################################################################
###  Specific HAP Table. May be "NONE"
#######################################################################
setenv SPECHAP haptabl_SPEC
#######################################################################

# Spatial surrogate xref file name prefix
setenv SURRXREF surrxref99

# Spatial surrogate to surrogate description xref prefix
# (CSV -comma delimited): OPTIONAL input
setenv SURRDESC surrogate_codes_and_definitions

# Emissions bins file name prefix: Format depends on inventory
setenv EMISBINS  am_grp99PRE

# County urban/rural flag xref file name prefix
setenv CNTYUR   popflg99

# GROWTH and CONTROL: set to NONE or leave blank if you are not projecting
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
setenv WORK2  /data/work17/ 

 cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/CountyProc.sas CountyProc_nonpt.sas
time sas CountyProc_nonpt -work /data/work17
