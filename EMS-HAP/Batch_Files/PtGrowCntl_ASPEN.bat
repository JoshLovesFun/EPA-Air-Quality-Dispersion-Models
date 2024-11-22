#Point Source Processing - The Growth and Control Program (PtGrowCntl)

# Model for which EMS-HAP is being run: ASPEN or ISC
setenv MODEL ASPEN

#Provide directory paths:

# path for the SAS input datasets
setenv IN_DATA /data/work16/EMSHAP/INVENTORY_NIF99/PROJ/pt/

# path for the SAS output datasets
setenv OUTDATA /data/work16/EMSHAP/INVENTORY_NIF99/PROJ/pt/OUTP/

# path for the reference text files
setenv REFTEXT /vail2aspen/dyntel/EMSHAP/ANCILLARY_V3/

#Provide input and output SAS data set names:

# input SAS data set name
setenv INSAS temporal188_fixsic

# GROWTH and CONTROL: set to NONE or leave blank if you are not projecting
setenv PROJECT proj_NIF99_ptV3

# output SAS data set prefix:
# remainder of the filename appended w/ column K text in PROJECT spreadsheet
setenv OUTSAS pt188_

cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/PtGrowCntl_V3.sas PtGrowCntl_NIF99.sas
sas PtGrowCntl_NIF99 -work .
