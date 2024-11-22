# Point Source Processing - Temporal V3 1999 NEI

# Provide the Model for which the data is being processed (ASPEN or ISC)
setenv MODEL ASPEN

# Provide directory paths:

#   path for the SAS input data set
setenv IN_DATA /data/work16/EMSHAP/INVENTORY_NIF99/pt2003

#   path for the SAS output data set
setenv OUTDATA /data/work16/EMSHAP/INVENTORY_NIF99/pt2003

#   path for the reference text files
setenv REFFILE /vail2aspen/dyntel/EMSHAP/ANCILLARY_NIF

# Provide input and output SAS data set names

#   input SAS data set name
setenv INSAS PtAspen

#   output SAS data set name
setenv OUTSAS Temporal

# Provide name of Temporal Allocation File (TAF)
setenv TAF taff_hourlyV3dpmPRE

# Provide name of the SCC_AMS correspondence texts:

#   name of SCC to SCC_AMS correspondence file
setenv SCCLINK scc2amsV3

#   name of SIC to SCC_AMS correspondence file
setenv SICLINK sic2scc

#   name of MACT category code to SCC_AMS correspondence file
setenv MACTLINK mact2scc

cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/PtTemporal_V3.sas PtTemporal_jul03.sas
sas PtTemporal_jul03 -work /data/work17
