# Point Source Processing - DataProc for ISCST3 -V3
# Defaults Location Data and Stack Parameters

# Provide the Model for which the data is being processed
setenv MODEL ISC

#   Choose UTMZ for ISC
setenv REF_ZONE 18

# Provide directory paths:

#   path for the SAS input data set
setenv IN_DATA /data/work16/PHILLY/

#   path for the SAS output data set
setenv OUTDATA /data/work16/PHILLY/point/

#   path for reference text files
setenv REFTEXT /vail2aspen/dyntel/EMSHAP/ANCILLARY_PHILLY/

#   path for included program to determine the UTM from lat/lon
setenv INC_DIR /vail2aspen/dyntel/EMSHAP/PROGRAMS/

#   path for output text file of records without latitude/longitude data
setenv OUTTEXT /data/work16/PHILLY/point/

# Provide input and output SAS data set names

#   input SAS data set name
setenv INSAS pt_w_landfills_ap

#   output SAS data set name
setenv OUTSAS dataset

#   output SAS data set created from Windowing portion of data processing
setenv FINAL dataproc

# Select the procedures to be included in data processing
# Set value to 1 for yes (or true) and 0 for no (or false)
# Provide name of necessary reference files and other information

# Default invalid or missing location data: set value of DoLocate to 1 for
# yes (or true) and 0 for no (or false)
### ISCST3 -simply convert x/y to UTM
setenv DOLOCATE 1

#   Default stack parameters: set value of DoStack to 1 for yes
#   (or true) and 0 for no (or false)
setenv DOSTACK 1

# To default stack parameters by SCC: set value of DoSCC to 1 for yes (or
# true) and 0 for no (or false)
setenv DOSCCDEF 1

# If defaulting stack parameters by SCC, provide the name of the SCC
# correspondence file
setenv SCCDEFLT def_scc

# To default stack parameters by SIC: set value of DoSIC to 1 for yes (or
# true) and 0 for no (or false)
setenv DOSICDEF 1

# If defaulting stack parameters by SIC, provide the name of the SIC
# correspondence file
setenv SICDEFLT def_sic

# If defaulting stack parameters, provide valid ranges and global defaults
# for each parameter

#      Stack Height range
setenv DLOWHT 0.003
setenv DHIHT 381

#      Stack Diameter range
setenv DLOWVEL 0.003
setenv DHIVEL 198

#      Stack Temperature
setenv DLOWTEMP 273
setenv DHITEMP  1505

#      Stack Diameter
setenv DLOWDIA 0.0762
setenv DHIDIA 15.24

#      Global Defaults
setenv DFLTHT 10
setenv DFLTVEL 1
setenv DFLTTEMP 295
setenv DFLTDIA 1

# Window inventory data set by selecting variables and removing records with
# zero emissions

# To select variables: set value of DoSetVar to 1 for yes (or true) and 0 for
# no (or false)
setenv DOSETVAR 1

# To select variables in addition to the required variables: set value of 
# UseList to 1 for yes (or true) and 0 for no (or false) and provide the name
# of the file
setenv USELIST 1
setenv VARLIST varlist_philly

# To window by zero emissions and valid locations: set value of DoWindow to
# 1 for yes (or true) and 0 for no (or false)
setenv DOWINDOW 1

# If windowing inventory, provide names of data sets to store the records
# with zero emissions  and the records without lat/lon values. 
setenv NOLOCATE nolatlon
setenv ZEROEMIS zeroemis

cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/PtDataProc_V3.sas  PtDataProc_PHILLY.sas
sas PtDataProc_PHILLY -work .
