# Point Source Processing - Model Specific Processing for ISCST3

# Provide the Model for which the data is being processed
setenv MODEL   ISC

# Provide directory paths:

#   path for the SAS input data set
setenv IN_DATA /data/work16/PHILLY/point/

#   path for the SAS output data set
setenv OUTDATA /data/work16/PHILLY/point/

#   path for the reference text files
setenv REFTEXT /vail2aspen/dyntel/EMSHAP/ANCILLARY_PHILLY/

# Provide input and output SAS data set names

#   input SAS data set name
setenv INSAS dataproc

#   output SAS data set name
setenv OUTSAS Ptmodel

# Provide name of reference SAS data sets containing pollutant information

# name of the text files containing the correspondence between the pollutant
# code used in the inventory and CAS number, SAROAD code, NTI HAP code,
# pollutant description and a flag indicating whether the pollutant should
# be retained for processing
#######################################################################
######### 3 HAP TABLE FILES
#######################################################################
#   2 HAP-General Files: point-area & non-road
#######################################################################
###  General Non-Road HAP Table:  V3 HAP table because we are using 1999 NEI
# (V3) nonroad emissions
setenv G_MOBHAP haptabl_nonroadGEN2_PHL

###  General Point-Area HAP Table:  V2 HAP table because we are using 1996
# NTI (V2) point/area emissions
setenv G_PTHAP haptabl_point_area_PHILLY
#######################################################################
###  Specific HAP Table.  Applies to all inventories: May be "NONE"
setenv SPECHAP NONE

cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/PtModelProc_V3.sas PtModelProc_PHILLY.sas
sas PtModelProc_PHILLY -work .
