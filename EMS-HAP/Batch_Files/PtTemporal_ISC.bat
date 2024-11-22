# ONROAD MOBILE6 ROAD SEGMENTS -USES ARRAY OF EMISSIONS for SUMMER AND WINTER

# Provide the Model for which the data is being processed (ASPEN or ISC)
setenv MODEL ISC

# For ISCST3, conservation of annual emissions depends on number of days.
#  Leap year: 1=yes, 0= no
setenv LEAPYEAR 1

# Provide directory paths:

#   path for the SAS input data set
setenv IN_DATA /data/work16/PHILLY/ONROAD_SEGMENTS/MARCH2003/

#   path for the SAS output data set
setenv OUTDATA /data/work16/PHILLY/ONROAD_SEGMENTS/MARCH2003/

#   path for the reference text files
setenv REFFILE /vail2aspen/dyntel/EMSHAP/ANCILLARY_PHILLY/

# Provide input and output SAS data set names

#   input SAS data set name
setenv INSAS mob6_temporal_inpnew

#   output SAS data set name
setenv OUTSAS Temporal_mob6new

# Provide name of Temporal Allocation File (TAF)
setenv TAF taff-ISCfactorsV3_mob6

# Provide name of the SCC_AMS correspondence texts:

#   name of SCC to SCC_AMS correspondence file
setenv SCCLINK scc2amsV3

#   name of SIC to SCC_AMS correspondence file
setenv SICLINK sic2scc_philly

#   name of MACT category code to SCC_AMS correspondence file
setenv MACTLINK mact2scc

cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/PtTemporal_V3.sas PtTemporal_MOBIL62new.sas
sas PtTemporal_MOBIL62new -work .
