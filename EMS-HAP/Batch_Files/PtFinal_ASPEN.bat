#Point Source Processing - Final Format 
#      Assigns source groups for ASPEN       
#      Produces ASPEN-formatted text files

# Provide the Model for which data is being processed (ASPEN only)
setenv MODEL ASPEN

# Provide directory paths:

#   path for the SAS input dataset
setenv IN_DATA /data/work16/EMSHAP/INVENTORY_NIF99/pt2003
 
#   path for the SAS output dataset
setenv OUTDATA /data/work16/EMSHAP/INVENTORY_NIF99/pt2003/ASPENemis/

#   path for the reference text files
setenv REFFILES /vail2aspen/dyntel/EMSHAP/ANCILLARY_NIF
 
#   path for the output files for input into ASPEN
setenv OUTFILES /data/work16/EMSHAP/INVENTORY_NIF99/pt2003/ASPENemis/

#   path for the single ASCII output file
setenv ASCIIFILE /data/work16/EMSHAP/INVENTORY_NIF99/pt2003/ASPENemis/

# Provide input and output SAS data set names
#   input SAS data set name
setenv INSAS temporal

#   output SAS dataset name
setenv OUTSAS pt99_aug05

# Select the procedure to be used to assign source groups
# Set value to 1 for yes (or true) and 0 for no (or false)

# Assign source groups by source type (major or area): set value of DoSource
# to 1 for yes (or true) and 0 for no (or false)
setenv DOSOURCE 1

#   Assign source groups by MACT categories: set value of DoMACT to 1 for yes
#   (or true) and 0 for no (or false)
setenv DOMACT 0

# If using MACT categories, provide name of the text file containing the
# group assignments
setenv MACTGRP MACT_grp

# Assign source groups by SCCs: set value of DoSCC to 1 for yes (or true)
# and 0 for no (or false)
setenv DOSCC 0

# If using SCCs, provide the name of the text file containing the group
# assignments
setenv SCCGRP SCC_grp

# Assign source groups by SIC: set value of DoSIC to 1 for yes (or true)
# and 0 for no (or false)
setenv DOSIC 0

# If using SICs, provide the name of the text file containing the group
# assignments
setenv SICGRP SIC_grp

# Provide a default group assignment (value between 0 and 9) for those
# sources not assigned by your selected procedure
setenv DFLTGRP 1

# Select the creation of ASPEN-formatted text files
# Set value of DoWrite to 1 for yes (or true) and 0 for no (or false)
setenv DOWRITE 1

# Provide the file name of the text file containing the decay rates for each
#     reactivity class, extension must be .txt
setenv DECAY indecay

# Provide a file identifier (maximum of 10 character) to be included in the
# name of the ASPEN-formatted text files
setenv OUTCODE PT99_aug05

# Specify the source type, set value of Itype to 0 for point sources and 3
# for pseudo point sources
setenv ITYPE 0

# Provide an identifying run name to be included in the file header (maximum
# of 25 characters)
setenv RUNID 'NIF POINT JUL03'

# Select the creation of the single ASCII-formatted file
# Set value of DoASCII to 1 for yes (or true) and 0 for no (or false)
setenv DOASCII 1

# Provide the file name of the output ASCII file 
setenv ASCII PT99_aug05

cp -p /vail2aspen/dyntel/EMSHAP/PROGRAMS/V3/PtFinal_ASPEN_V3.sas PtFinal_ASPEN_jul03.sas

sas PtFinal_ASPEN_jul03 -work /data/work17/
