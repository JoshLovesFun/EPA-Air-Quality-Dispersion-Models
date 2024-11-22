C     Include File:  SF1.INC
C

      INTEGER   SFMH,SFMV,SFHLY
      INTEGER   SFOBS,SFBLKF                                            DTB94243
      INTEGER   IDPPT,PPTAMT,IDVAR
      CHARACTER SAMFMT*180
      CHARACTER VNAMES*4,SFFMT*8,SFLON*8,SFLAT*8,SFLOC*8                JOP
      CHARACTER PPTLOC*8,PPTFMT*8
C
      PARAMETER (SFMH=24,SFMV=60,SFHLY=23,MAXVAR=26)                              JOP
      COMMON /SFBK01/ SFOBS(SFMH,SFMV)
      COMMON /SFBK02/ SFLON,SFLAT,SFFMT,SFLOC,SFBLKF                    DTB94243
      COMMON /PPT001/ PPTFMT,PPTLOC,IDPPT,PPTAMT(SFMH)
      COMMON /SFBK04/ VNAMES(SFMV)
      COMMON /SAMSON/ SAMFMT,IDVAR(MAXVAR)
C
C       SFMH       Maximum number of hours that can be merged
C       SFMV       Maximum number of weather observation variables
C                  (for NWS hourly and onsite data: VNAMES and SFQA)
C       SFHLY      Number of NWS hourly surface weather variables
C       SFLOC      Hourly surface station identifier
C       PPTLOC     Station identifier for precipitation data
C       PPTFMT     Precipitation data format (3240FB, 3240VB)
C       PPTAMT     Array of hourly precipitaiton amounts (integer)
C       IDPPT      Station ID in precip file
C       IPPTYP     Precipitation type reported for ISCSTDFT (Stage 3)
C       SFOBS      Master data array for NWS surface data
C       SFLAT      Station latitude*100 (degrees) (e.g. 3034n)
C       SFFMT      Surface weather data format (e.g, 144FB)
C       SFBLKF     Blocking factor for NWS data file                    DTB94243
C       VNAMES     4-letter names for the weather variables
C       SAMFMT     Format constructed to read SAMSON data
C       IDVAR      The variable ID number for processing SAMSON data
