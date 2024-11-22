# Point Source Processing - DataProc
# Defaults Location Data and Stack Parameters

# Provide the Model for which the data is being processed
setenv MODEL ASPEN

# Provide directory paths:

#   path for the SAS input data set
setenv IN_DATA /data/work16/EMSHAP/INVENTORY_NIF99/pt2003/

#   path for the SAS output data set
setenv OUTDATA /data/work16/EMSHAP/INVENTORY_NIF99/pt2003/

#   path for reference SAS data sets: MAPPING DATASETS MUST ALSO BE HERE 
setenv REFFILE /vail2aspen/dyntel/EMSHAP/ANCILLARY_NIF

#   path for reference text files
setenv REFTEXT /vail2aspen/dyntel/EMSHAP/ANCILLARY_NIF

#   path for output text file of records without latitude/longitude data
setenv OUTTEXT /data/work16/EMSHAP/INVENTORY_NIF99/pt2003/

# Provide input and output SAS data set names

#   input SAS data set name
setenv INSAS pt99_aug04

#   output SAS data set name
setenv OUTSAS dataset

#   output SAS data set created from Windowing portion of data processing
setenv FINAL dataproc


# Select the procedures to be included in data processing
# Set value to 1 for yes (or true) and 0 for no (or false)
# Provide name of necessary reference files and other information

# Default invalid or missing location data: set value of DoLocate to 1 for
# yes (or true) and 0 for no (or false)
setenv DOLOCATE 1

# Also provide names of the text files containing the 
# county centroids by zip code, county FIPS, and state FIPS and postal abbr.
setenv ZIP zipcodes99
setenv CNTYCENT cty_cntr99
setenv MAP_INDX bound6_99
setenv POLYGONS counties99

# Also provide name of sas dataset containing random array of tracts, with
# radius greater than 0.5 miles, for each county to be used to assign
# default locations
setenv TRACTS trctarry99

# Also provide name of sas dataset containing tract information,
# specifically the location of the tract centroid
setenv TRCTINFO tractinf99

#   Default stack parameters: set value of DoStack to 1 for yes
#   (or true) and 0 for no (or false)
setenv DOSTACK 1

# To default stack parameters by SCC: set value of DoSCC to 1 for yes
# (or true) and 0 for no (or false)
setenv DOSCCDEF 1

# If defaulting stack parameters by SCC, provide the name of the SCC
# correspondence file
setenv SCCDEFLT def_scc

# To default stack parameters by SIC: set value of DoSIC to 1 for yes
# (or true) and 0 for no (or false)
setenv DOSICDEF 1

# If defaulting stack parameters by SIC, provide the name of the SIC
# correspondence file
setenv SICDEFLT def_sic

# If defaulting stack parameters, provide valid ranges and global
# defaults for each parameter
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
setenv VARLIST varlist_pt99

# To window by zero emissions and valid locations: set value of DoWindow to
# 1 for yes (or true) and 0 for no (or false)
setenv DOWINDOW 1

# If windowing inventory, provide names of data sets to store the records
# with zero emissions and the records without lat/lon values. 
setenv NOLOCATE nolatlon
setenv ZEROEMIS zeroemis

cp /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/PtDataProc_V3.sas PtDataProc_jul03.sas
sas PtDataProc_jul03 -work /data/work17/
