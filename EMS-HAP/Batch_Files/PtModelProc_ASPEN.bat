# Point Source Processing - Model Specific Processing

# Provide the Model for which the data is being processed
setenv MODEL   ASPEN

# Provide directory paths:

#   path for the SAS input data set
setenv IN_DATA /data/work16/EMSHAP/INVENTORY_NIF99/pt2003/

#   path for the SAS output data set
setenv OUTDATA /data/work16/EMSHAP/INVENTORY_NIF99/pt2003/

#   path for the reference SAS data sets
setenv REFSAS /vail2aspen/dyntel/EMSHAP/ANCILLARY_NIF

#   path for the reference text files
setenv REFTEXT /vail2aspen/dyntel/EMSHAP/ANCILLARY_NIF

# Provide input and output SAS data set names

#   input SAS data set name
setenv INSAS dataproc

#   output SAS data set name
setenv OUTSAS PtAspen

# Provide name of reference files containing pollutant information and
# census tract information

# name of the text files containing the correspondence between the pollutant
# code used in the inventory and CAS number, SAROAD code, NTI HAP code,
# pollutant description and a flag indicating whether the pollutant should
# be retained for processing

#######################################################################
######### 3 HAP TABLE FILES
#######################################################################
#   2 HAP-General Files: point-area & non-road
#######################################################################
###  General Non-Road HAP Table
setenv G_MOBHAP haptabl_nonroadGEN_toxwt

###  General Point-Area HAP Table
setenv G_PTHAP haptabl_stationary_188
#######################################################################
###  Specific HAP Table.  Applies to all inventories: May be "NONE"
setenv SPECHAP haptabl_SPEC

# name of the sas data set containing the urban/rural flags by county
# (value is 1 or 0 if  all tracts within the county of the same and value is
# 9 for non-uniform counties)
setenv CTYFLAG ctyflag99

#   name of the SAS data set containing the census tract information,
# including urban/rural flags, state and county FIP codes, tract location,
# and tract radius
setenv TRCTINF  tractinf99

cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/PtModelProc_V3.sas PtModelProc_jul03.sas 
sas PtModelProc_jul03 -work /data/work17/
