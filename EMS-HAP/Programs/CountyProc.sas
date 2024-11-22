/*
            ****************************************************
            *                                                  *
            *      EMS-HAP COUNTY LEVEL EMISSIONS PROCESSOR    *
            *                    Version 3                     *
            *                                                  *
            *                    CountyProc.sas                *
            *                                                  *
            *                     MAR 2003                     *
            *                                                  *
            ****************************************************


   The County Processor reads in county level mobile or non-point emission rates.  If the user is 
looking for ISCST3 or ASPEN model output, spatial allocation factors, temporal allocation factors, and
cross-reference files are also read in and the county level emissions are apportioned to census tracts
(ASPEN) or grid cells (ISCST3) within the counties, according to the factors.  The User also has the
option of applying multiple projection (growth and control) scenarios to the temporally allocated
emissions.  If the user wants only county-level projected emissions, the user may by-pass the spatial
and temporal allocation application, thereby saving considerable disk space and run time.
   For ASPEN model processing, the emission rates are allocated from annual emission rates to 3-hour
emission rates (eight 3-hour blocks for an average day).  For ISCST3 model processing, the emission
rates are allocated from annual emission rates to hourly emissions (by day-of-week and season).  For the
ASPEN model, spatially and temporally allocated emission rates are then written out to a file in the
format required for input to the ASPEN model.  For the ISCST3 model, a SAS dataset is output for use by
CountyFinal.  Some emissions summaries are also provided, the level of which is determined by the value
of the DIET option in the batch file.  This program was developed by EC/R Inc and modified by DynCorp.
   Added March 2004, a new model option, ISCTRACT, that uses the census tract surrogates to create ISC
sources.  Instead of gridded 1x1 km sources in ISC, the census tract polygons become the ISC area sources.
In this module, the surrogate files used for ASPEN are used to spatially distribute county level emissions 
to the census tracts as ISC sources.  New model option added in batch file.

                             LIST OF INPUT FILES

       Run options -on UNIX platfrom, specify options in the batch file
       County level emission inventory file (area or mobile)
       Spatial allocation factor files
       Pollutant decay rates file (ASPEN only)
       Temporal allocation factor file
       Spatial allocation surrogate-to-SCC cross-reference file
       HAP Table
      *Growth Factor Files by SCC, and/or SIC and/or MACT code
      *MACT general, specific, and user control files
       Emission source group assignment file
       County urban/rural flag cross-reference file
      *User-defined County flag cross-reference file

* - these files, referenced in the PROJECTion spreadsheet, are optional and used only for growth and control.

                          DESCRIPTIONS OF INPUT FILES


       COUNTY LEVEL EMISSIONS INVENTORY (EMISFILE) FILE
          Contains emission rates for each species and each county, covering the region to be modeled.

          Typical Variables:
             SCC       - SCC code (character, length: 10 for NIF2.0, 8 for NATA 1996).
             AMS**     - AMS/SCC code (character, length 10).
             CAS       - The unique pollutant code (character, length 10).
             CAT_NAME  - Area/Mobile source emissions category name (character, length 50(mobile) or 90
                         (area)).
             EMIS      - Emission rates (tons/year) for the pollutant, source category, and county.
             MACT      - MACT code (character, length 7).
             MATCH*    - Information on how spatial surrogates (SPATSURR) and AMS or SCC codes were assigned in
                         CoPAX for non-point emissions (character, length 4).
             SIC       - SIC code (character, length 4).
             SPATSURR* - The assigned spatial surrogate.
             FIPS      - State and county FIPS codes, concatenated to give a 5-character variable, with
                         zeros in place of leading blanks (character, length 5).

       * - these variables are present when processing non-point emissions only.
       ** - this variable is present when processing 1996 NATA NTI non-point emissions only.

       CAS-SAROAD-REACT GENERAL HAP TABLE (GENHAP) FILE
          Contains cas to saroad link, groups and partitions HAPs, assigns reactivity class (ASPEN), and
          assigns NTI_HAP (Growth and Control).  Keeps only those pollutants where keep variable = "Y".

          Variables:
             POLLDESC  - Pollutant Description (character, length 45).
             SAROADDC  - SAROAD Description (character, length 50).
             CAS       - The unique pollutant code (character, length 10).
             REACT     - Reactivity Class (1-9), used by ASPEN only (character, length 1).
             KEEP      - Drops emissions in inventory where it equals "N" (character, length 1).
             SAROAD    - SAROAD Code (character, length 5).
             FACTOR    - Speciates metals and other HAPs. 
             NTI_HAP   - Used for Growth and Control, more broad than SAROAD (character, length 3).

       SPECIFIC HAP TABLE (SPECHAP) FILE
          Contains CAS to SAROAD link and additional factors for splitting CAS into even more specific
          profiles: MACT, SCC, SIC or CAS-level splits (up to three).

          Variables:
             POLLDESC  - Pollutant Description (character, length 45).
             CAS       - The unique pollutant code (character, length 10).
             OLDS1     - original SAROAD code from GENHAP. If metal, generally the fine SAROAD code (character, length 5).
             NEWS1     - new SAROAD code to replace OLDS1 (character, length 5).
             OLDS2     - original SAROAD code from GENHAP. If metal, generally the coarse SAROAD code (character, length 5).
             NEWS2     - new SAROAD code to replace OLDS2 (character, length 5).
             OLDS3     - original SAROAD code from GENHAP. Could be POM-type SAROAD (character, length 5).
             NEWS3     - new SAROAD code to replace OLDS3 (character, length 5).
             SPEC_FAC  - Speciates OLDS# to NEWS# based on MACT, SCC, SIC, or CAS-level information.
             MACT      - MACT code (character, length 7).
             SCC       - SCC code (character, length 10).
             SIC       - SIC code (character, length 4).


       SPATIAL ALLOCATION FACTOR (SAFFILE) FILES
          These files contain factors which allocate the emission rates in a county to all census tracts
          within the county, for all tract-county combinations.  There is a file for each spatial
          allocation surrogate.

          Variables:
             CELL  - FIPS concatenated with the tract id (character, length 11).
             SAF#  - The county-to-tract spatial allocation factor.
             LAT   - Latitude of the centroid of the tract.
             LON   - Longitude of the centroid of the tract.
             UFLAG - Urban/rural land type flag (1=urban, 2=rural).


       POLLUTANT DECAY RATES (INDECAY) FILE (ASPEN only)
          This file contains decay rates for the different reactivity classes, in an array 
          dimensioned by reactivity class, time block and stability.  One record for each
          reactivity class / time block combination.

          Variables:
             RCT       - Reactivity class (integer, 1 digit, column 1)
             TIME      - Time block (integer, 1 to 8, column 3)
             DECAY     - The decay rate for the specified reactivity class
                         and time (numeric, columns 5-64)

       TEMPORAL ALLOCATION FACTOR (TAFFILE) FILE
          This file contains temporal allocation factors allocating annual emission rates to hourly
          emission rates (ASPEN) and hourly/day-of-week/seasonal emission rates (ISCST3), by SCC.
          ASPEN:  One record for each SCC.
          ISCST3: Twelve records for each SCC -one record per 4 seasons, per 3 day-of-week types.

          Variables:
             SCC          - AMS/SCC code (character, length 10).
             TAF (ASPEN)  - 24 hourly temporal allocation factors for each SCC.
             TAF (ISCST3) - 288 hourly temporal allocation factors for each SCC.


       SCC TO SPATIAL ALLOCATION SURROGATE CROSS-REFERENCE (SURRXREF) FILE
          MOBILE inventories only:  This file gives the assignments of spatial surrogates to SCCs.

          Variables:
             SCC       - SCC code (character, length 10).
             SURR      - The spatial allocation surrogate assigned to the SCC.


       COUNTY URBAN/RURAL FLAG (CNTYUR) **SOURCE-GROUPING** FILE
          This file contains county urban/rural flags.

          Variables:
             FIPS      - State and county FIPS codes, concatenated to give a 5-character variable, with
                         zeros in place of leading blanks (character, length 5).
             CNTY_UR   - Urban/rural flag (character, length 1).

**NOTE** Each projection scenario with user-defined reductions MAY opt for a unique version of this file in
  which counties are assigned projection-scenario-specific codes (1-5 character variable that follows CNTY_UR).


       EMISSIONS GROUPS DEFINITION (EMISBINS) FILE
          This file adds the emissions source category group (bin) to the file.

          Variables:
             CAT_NAME  - In 1996 NATA (NTI) non-point input only: emissions category name (character, length 90).
             SCC       - For 1996 NATA (NTI) mobile and in 1999 NIF2.0 and above: SCC (character, length 90).
             CATCODE   - Source category identification code (character, length 4).
             URBANBIN  - Bin to be used for urban census tracts/grid cells (CNTY_UR=U).
             RURALBIN  - Bin to be used for rural census tracts/grid cells (CNTY_UR=R).
             SRC_TYPE  - Source category used for emission reduction scenarios (character, length 1).


       GROWTH FACTOR BY STATE/MAC (GFMACT) FILE
          This file contains growth factors by state/county/MACT code.

          Variables:
             STATE     - State FIPS (character, length 2).
             COUNTY    - County FIPS (character, length 3).
             MACT      - MACT code (character, length 7).
             GF        - Growth factor for future year.


       GROWTH FACTOR BY STATE/SIC (GFSIC) FILE
          This file contains growth factors by state/county/SIC code.

          Variables:
             STATE     - State FIPS (character, length 2).
             COUNTY    - County FIPS (character, length 3).
             SICKEY    - SIC code (character, length 4).
             GF        - Growth factor for future year.


       GROWTH FACTOR BY STATE/MAC (GFSCC) FILE
          This file contains growth factors by state/county/SCC code via an EGAS REMI indicator in the header.

          Variables:
             STATE     - State FIPS (character, length 2).
             COUNTY    - County FIPS (character, length 3).
             SCC       - SCC code (character, length 10).
             INDICATOR - EGAS REMI indicator used to cross-reference SCCs to Growth Factors (character, length 72).
             GF        - Growth factor for future year.


       GENERAL MACT CONTROLS SPECIFICATION (MACTGEN) FILE
          The general MACT controls specification file.

          Variables:
             MACT      - MACT code (character, length 7).
             MACTXEFF  - Control efficiency to be applied to existing emission sources.
             MACTNEFF  - Control efficiency to be applied to new, modified, or reconstructed
                         emission sources.
             MACTRATE  - Percentage of future emissions attributed to new sources.
             BIN       - MACT standard bin, can have 4 possible values: 2, 4, 7, or 10
                         (character, length 2).
             COMPLYR   - Expected deadline for affected emission sources to comply with
                         standards.  Used with MACTbin to determine if MACT controls are
                         applied (character, length 4).
             APPLY     - Application control flag: set to 1 if controls are to be applied,
                         set to 0 if controls are not to be applied (character, length 1).
             MACT_SRC  - Source control flag: set to M to apply controls only to major
                         point sources, set to B to apply to all point sources (character,
                         length 1). 

       POLLUTANT SPECIFIC MACT CONTROLS SPECIFICATION (SPECFILE) FILE
          The pollutant specific MACT controls specification file.

          Variables:
             MACT      - MACT code (character, length 7).
             NTI_HAP   - HAP identification code (character, length 3).
             SCC8      - 8-digit SCC (character, length 8).
             SCC6      - 6-digit SCC (character, length 6).
             EFFXSPEC  - Control efficiency to be applied to existing emission sources.
             EFFNSPEC  - Control efficiency to be applied to new, modified, or reconstructed
                         emission sources.
             SNEWRATE  - Percentage of future emissions attributed to new sources.
             APPLY     - Application control flag: set to 1 if controls are to be applied,
                         set to 0 if controls are not to be applied (character, length 1).
             SAPP_SRC  - Source control flag: set to M to apply controls only to major
                         point sources, set to B to apply to all point sources (character,
                         length 1). 


       USER-DEFINED CONTROLS SPECIFICATION (USERFILE) FILE
          The user-defined controls specification file.

          Variables:
             CAT_NAME  - In 1996 NATA (NTI) non-point input only: emissions category name (character, length 90).
             SCC       - For 1996 NATA (NTI) mobile and in 1999 NIF2.0 and above: SCC (character, length 90).
             MACT      - MACT code (character, length 7).
             NTI_HAP   - HAP identification code (character, length 3).
             E_EFF     - Control efficiency to be applied to existing emission sources.
             N_EFF     - Control efficiency to be applied to new, modified, or reconstructed
                         emission sources.
             N_RATE    - Percentage of future emissions attributed to new sources.
             CNTYCODE  - County code (character, length 5).
             R_CODE    - MACT replacement code (character, length 1).
             APPLY     - Application control flag: set to 1 if controls are to be applied,
                         set to 0 if controls are not to be applied (character, length 1).


   OUTPUT FILES
      ASPEN:
        There are two types of output files, a set of files (one for each reactivity class) with
      spatially/temporally allocated emission rates in the format required for input directly to ASPEN,
      and 2 SAS-formatted files and an ASCII file containing more detailed information from which
      emissions summaries can be produced and from which the ASPEN- formatted files can be written out.
   
        The ASPEN-formatted ascii output files are given names of the form
      EMISTYPE.USRLABEL.dRUNDATE.rREACT.inp.  An example file name is MV.Base96.3.r9.d020499.inp, where
      Base96 is the user-specified label, MV is the emissions type, 3 is the emissions category, r9 
      identifies the reactivity class, and d020499 is the date of the run.  The file names of the
      SAS-formatted output files are the first 8 characters of EMISTYPE.USRLABEL (without the period)
      with a SAS suffix.  One of the SAS files has a leading C.  For example, MVBase96.sas7bdat and
      CMVBase96.sas7bdat. The ASCII file has the suffix ".txt", for example, MVBase96.txt.

        The directory name for the output files are specified by the user in the control options
      specfications.  The format of the ASPEN input files is specified in the ASPEN user's manual.

        If the DIET option is chosen, the extended SAS-formatted file and ASCII file are not output.

      ISCST3:
        If the DIET option is chosen, there is one SAS output file (ISCOUT).  This file is used as input
      for CountyFinal.  If the DIET option is NOT chosen, an extended SAS file, containing SCC-level data,
      is also produced.  It has the same name as the CountyFinal input file, but is preceeded with an "X".

***************************************************************************/


***  SET OPTIONS AND BASIC PARAMETERS;
options ls=92 ps=54 compress=NO mprint symbolgen mlogic pageno=1;


*==================*;
%MACRO COF;
*==================*;

***  VERSION FOR RUNNING ON UNIX PLATFORMS;

********************************************************************;
* Read batch file specifying the names of                           ;
* the directories and file names to be used.                        ;
********************************************************************;

%GLOBAL PROJECT amsvar model diet ;

/*** CONTAINS MACRO VARIABLES THAT DETERMINE FLOW OF EMISSIONS PROCESSING WHEN PROJECTING EMISSIONS ***/
%let PROJECT  = %sysget(PROJECT);

%let Model = %sysget(MODEL);
%let diet  = %sysget(DIET );

%IF (%upcase("&model") = "ASPEN") %THEN %DO;

   %GLOBAL NUMSURR POLLIST NUMPOLL;
   %GLOBAL RunID    emislabl RunDate  emistype numtaf
           usrlabel inpfiles inpemiss outfiles emisfile
           SAFfile  DEFLTSAF TAFfile  indecay  GENHAP SPECHAP
           SURRxref SURRDESC EMISBINS CNTYUR_
           Lsubsetp subsetp  Lsubsetg subsetg
           Ldbg     onecell OUTNAME8;

   %let numtaf = 8;

   %let RunID    = %sysget(RUNID   );
   %let emislabl = %sysget(EMISLABL);
   %let RunDate  = %sysget(RUNDATE );
   %let emistype = %sysget(EMISTYPE);
   %let usrlabel = %sysget(USRLABEL);

   %let inpfiles = %sysget(INPFILES);
   %let INPEMISS = %sysget(INPEMISS);
   %let outfiles = %sysget(OUTFILES);
   %let emisfile = %sysget(EMISFILE);
   %let SAFfile  = %sysget(SAFFILE );
   %let DEFLTSAF = %sysget(DEFLTSAF);
   %let TAFfile  = %sysget(TAFFILE );
   %let indecay  = %sysget(INDECAY );
   %let GENHAP   = %sysget(GENHAP  );
   %let SPECHAP  = %sysget(SPECHAP );
   %let SURRxref = %sysget(SURRXREF);
   %let SURRDESC = %sysget(SURRDESC);

   %let EMISBINS = %sysget(EMISBINS);
   %let CNTYUR_  = %sysget(CNTYUR  );

   %let Lsubsetp = %sysget(LSUBSETP);
   %let subsetp  = %sysget(SUBSETP );
   %let Lsubsetg = %sysget(LSUBSETG);
   %let subsetg  = %sysget(SUBSETG );
   %let Ldbg     = %sysget(LDBG    );
   %let onecell  = %sysget(ONECELL );
   %let work2    = %sysget(WORK2   );

   %put _user_;

   title1 "&RUNID";

   * ----------------------------------------- *;
   ***  ASSIGN DIRECTORIES AND FILENAMES;
      libname inpfiles  "&INPFILES";
      libname inpemiss  "&INPEMISS";
      libname outfiles  "&OUTFILES";
      libname work2     "&WORK2";  * Additional work space for work2.emistemp,
                               which can be > 2GB, and for temporary QA files;

      %let OUTNAME8 = &EMISTYPE.&USRLABEL ;
   * ----------------------------------------- *;

%END;

%ELSE %IF (%upcase("&model") = "ISCTRACT") %THEN %DO; *new model option JAT 3/04;

   %GLOBAL NUMSURR POLLIST NUMPOLL;
   %GLOBAL RunID    emislabl RunDate emistype numtaf
           usrlabel inpfiles inpemiss outfiles emisfile
           SAFfile  DEFLTSAF TAFfile  GENHAP   SPECHAP
           SURRxref SURRDESC EMISBINS CNTYUR_  Lsubsetp subsetp
           Lsubsetg subsetg  Ldbg     onecell  ISCOUT
           OUTNAME8 ndays;

   %let numtaf = 288;
   %let RunID    = %sysget(RUNID   );
   %let emislabl = %sysget(EMISLABL);
   %let RunDate  = %sysget(RUNDATE );
   %let emistype = %sysget(EMISTYPE);
   %let usrlabel = %sysget(USRLABEL);

   %let inpfiles = %sysget(INPFILES);
   %let INPEMISS = %sysget(INPEMISS);
   %let outfiles = %sysget(OUTFILES);
   %let emisfile = %sysget(EMISFILE);
   %let SAFfile  = %sysget(SAFFILE );
   %let DEFLTSAF = %sysget(DEFLTSAF);
   %let TAFfile  = %sysget(TAFFILE );
   %let GENHAP   = %sysget(GENHAP  );
   %let SPECHAP  = %sysget(SPECHAP );
   %let SURRxref = %sysget(SURRXREF);
   %let SURRDESC = %sysget(SURRDESC);

   %let EMISBINS = %sysget(EMISBINS);
   %let CNTYUR_  = %sysget(CNTYUR  );

   %let Lsubsetp = %sysget(LSUBSETP);
   %let subsetp  = %sysget(SUBSETP );
   %let Lsubsetg = %sysget(LSUBSETG);
   %let subsetg  = %sysget(SUBSETG );
   %let Ldbg     = %sysget(LDBG    );
   %let onecell  = %sysget(ONECELL );
   %let work2    = %sysget(WORK2   );

   %let ISCOUT   = %sysget(ISCOUT  );
   %let leapyear =  %sysget(LEAPYEAR);

   %IF &leapyear %THEN %DO;
      %let ndays = 366;
   %END;
   %ELSE %DO;
      %let ndays = 365;
   %END;

   %put _user_;

   title1 "&RUNID";

   * ----------------------------------------- *;
   ***  ASSIGN DIRECTORIES AND FILENAMES;
      libname inpfiles  "&INPFILES";
      libname inpemiss  "&INPEMISS";
      libname outfiles  "&OUTFILES";
      libname work2     "&WORK2";  * Additional work space for work2.emistemp,
                                  which can be > 2GB, and for temporary QA files;

      %let OUTNAME8 = &EMISTYPE.&USRLABEL ;
   * ----------------------------------------- *;

%END;

%ELSE %IF (%upcase("&model") = "ISC") %THEN %DO;

   %GLOBAL NUMSURR POLLIST NUMPOLL;
   %GLOBAL RunID    emislabl emistype numtaf
           usrlabel inpfiles inpemiss outfiles emisfile
           SAFfile  DEFLTSAF TAFfile  GENHAP   SPECHAP
           SURRxref SURRDESC EMISBINS CNTYUR_  Lsubsetp subsetp
           Lsubsetg subsetg  Ldbg     onecell  ISCOUT 
           XORIG    YORIG    CELLSIZE OUTNAME8 ndays;

   %let numtaf = 288;
   %let RunID    = %sysget(RUNID   );
   %let emislabl = %sysget(EMISLABL);
   %let RunDate  = %sysget(RUNDATE );
   %let emistype = %sysget(EMISTYPE);
   %let usrlabel = %sysget(USRLABEL);

   %let inpfiles = %sysget(INPFILES);
   %let INPEMISS = %sysget(INPEMISS);
   %let outfiles = %sysget(OUTFILES);
   %let emisfile = %sysget(EMISFILE);
   %let SAFfile  = %sysget(SAFFILE );
   %let DEFLTSAF = %sysget(DEFLTSAF);
   %let TAFfile  = %sysget(TAFFILE );
   %let GENHAP   = %sysget(GENHAP  );
   %let SPECHAP  = %sysget(SPECHAP );
   %let SURRxref = %sysget(SURRXREF);
   %let SURRDESC = %sysget(SURRDESC);

   %let EMISBINS = %sysget(EMISBINS);
   %let CNTYUR_  = %sysget(CNTYUR  );

   %let Lsubsetp = %sysget(LSUBSETP);
   %let subsetp  = %sysget(SUBSETP );
   %let Lsubsetg = %sysget(LSUBSETG);
   %let subsetg  = %sysget(SUBSETG );
   %let Ldbg     = %sysget(LDBG    );
   %let onecell  = %sysget(ONECELL );
   %let work2    = %sysget(WORK2   );

   %let xorig    = %sysget(XORIG   );
   %let yorig    = %sysget(YORIG   );
   %let cellsize = %sysget(CELLSIZE);
   %let ISCOUT   = %sysget(ISCOUT  );
   %let leapyear =  %sysget(LEAPYEAR);

   %IF &leapyear %THEN %DO;
      %let ndays = 366;
   %END;
   %ELSE %DO;
      %let ndays = 365;
   %END;

   %put _user_;

   title1 "&RUNID";

   * ----------------------------------------- *;
   ***  ASSIGN DIRECTORIES AND FILENAMES;
      libname inpfiles  "&INPFILES";
      libname inpemiss  "&INPEMISS";
      libname outfiles  "&OUTFILES";
      libname work2     "&WORK2";  * Additional work space for work2.emistemp,
                                  which can be > 2GB, and for temporary QA files;

      %let OUTNAME8 = &EMISTYPE.&USRLABEL ;
   * ----------------------------------------- *;

%END;

%ELSE %DO;
   put @1 'Error!!! Model designation is incorrect.';
   STOP;
%END;

***** DETERMINE IF INVENTORY IS 1996 NATA (NTI) non-point ***;
data _null_;
   %let amsvar = 0;
run;
proc contents data=inpemiss.&EMISFILE out=chkams(keep=name) noprint; run;
data _null_;
   set chkams;
   if upcase(trim(name)) = "AMS" then call symput('amsvar',1);
run;
%put &amsvar;

%MEND COF;


*==================*;
%MACRO SETPOLL;
*==================*;

***  SET POLLNUM, POLLIST;
data _NULL_;
   if 0 then set POLLStmp nobs=howmany;
   call symput('NUMPOLL',howmany);
   stop;
run;

proc sql noprint;
   select distinct cas
      into :POLLIST separated by " "
      from POLLStmp;
quit;
%put POLLIST = &POLLIST;
%MEND SETPOLL;


*==================*;
%MACRO READEMIS;
*==================*;
***  READING COUNTY LEVEL EMISSIONS BY POLLUTANT AND SOURCE CATEGORY;
***  SELECT EMISSIONS FILE, READ IT IN, AND SUBSET TO ONE STATE AND/OR
***  POLLUTANT IF DESIRED;

data emission;
   length SCC $10 mact $7 sic $4 %IF "&EMISTYPE" = "MV" %THEN %DO; cat_name $10 %END;;

   ***  READ EMISSIONS DATA FILE;
   set inpemiss.&EMISFILE;

   ***  SET LABELS FOR VARIABLES;
   label emis="Emissions*(tpy)" cat_name="Source*category"
   %IF "&EMISTYPE" = "AR" %THEN %DO;
       spatsurr="Spatial*surrogate"
   %END;;

   ***  SUBSET TO A POLLUTANT, IF REQUESTED;
   IF &LSUBSETP THEN DO;
      if cas = "&SUBSETP";
   END;

   ***  SUBSET TO A STATE, IF REQUESTED;
   %IF &LSUBSETG %THEN %DO;
      if fipstate(substr(fips,1,2)) = compress("&SUBSETG");
   %END;
   %ELSE %DO;
      %let SUBSETG = US ;
   %END;

   ***  KEEP ONLY NEEDED VARIABLES - DYL - add keeping mact;
%IF "&EMISTYPE" = "AR" %THEN %DO;
   keep fips SCC cas mact sic emis cat_name spatsurr %if &amsvar %then %do; AMS %end;;
%END;

%IF "&EMISTYPE" = "MV" %THEN %DO;
   mact = '';                                        *added 5/11/01 by RAM;
   sic  = '';                                        *added 6/15/01 by RAM;
   cat_name = SCC;                                   ** added FEB 2003 by RAM;
   keep FIPS SCC CAS mact sic emis cat_name;
%END;

   cat_name = UPCASE(cat_name);
run;

options ls=128;

title2 "The Input Emissions File &EMISFILE (20 records)";
proc print data=emission(obs=20) split="*";
run;
title2 "&EMISLABL";
proc contents data=emission; run;

***  PRINT EMISSIONS TRACKING SUMMARY  ***;
proc sort data=emission;
   by cas;
run;
title2 "Summary of Input Emission Rates by Pollutant (tpy)";
title3 "QA: Tracking Emission Rate Totals";
proc means data=emission N SUM noprint;
   by cas;
   var emis;
   output out=POLLStmp n=nrecords sum=emis;
run;
data haplist_;
   label polldesc="CAS description";
   length polldesc $45 cas $10;
   infile "&INPFILES./&GENHAP..txt" firstobs=2 lrecl=110 stopover;
   input @1 polldesc $45.  @100 cas $10.;
run;

proc sort data=haplist_ nodupkey; by cas; run;

data POLLStmp_w_CAS_desc;
   merge POLLStmp(in=a) haplist_;
   by cas;
   if a;
run;

proc print split="*";
   var cas polldesc emis nrecords;
   sum emis nrecords;
   label emis="Emissions*(tpy)" nrecords="# of*records";
run;
title2;

***  PRINT STATE SUMS  ***;
data temp1;
   set emission;
   state=substr(fips,1,2);
   keep state emis;
run;
proc sort data=temp1;
   by state;
run;
title2 "Summary of Input Emission Rates by State (tpy)";
proc means data=temp1 N SUM noprint;
   by state;
   var emis;
   output out=Atemp1 n=nrecords sum=emis;
run;
proc print split="*";
   var state emis nrecords;
   sum emis nrecords;
   label state="State" emis="Emissions*(tpy)" nrecords="# of*records";
run;
proc datasets;
   delete temp1 Atemp1;
run;
title2;

***  CREATE CASNUM FILE FOR READHAPS;
proc sort data=emission(keep=cas) out=casnum nodupkey;
   by cas;
run;

%SETPOLL;

%MEND READEMIS;


*==================*;
%MACRO READHAPS;
*==================*;

***  READ IN CAS-SAROAD-REACT HAP TABLE;
***  SET POLLCODE;

data haplist1;

   length polldesc $45 saroaddc $50 pollcode $10 keep $1 SAROAD $5 NTI_HAP $3;
   infile "&INPFILES./&GENHAP..txt" firstobs=2 lrecl=147 stopover;
   input
     @1 polldesc $45.
     @47 saroaddc $50.
     @100 pollcode $10.
     @113 react 1.
     @121 keep $1.
     @128 SAROAD $5.
     @135 factor 7.
     @144 NTI_HAP $3.
   ;

   label polldesc="CAS description" saroaddc="SAROAD description"
         react="Reactivity*class" pollcode="CAS*number"
         saroad="SAROAD*code" nti_hap="NTI_HAP*Code";
run;
*  Doing IF KEEP = "Y" below to get correct non-matches printed;

title2 "General HAP Table Pollutant Code List";
title3 "RETAINED POLLUTANTS IN  &GENHAP";
data temp2;
   set haplist1;
   polldesc=substr(polldesc,1,30);
   saroaddc=substr(saroaddc,1,30);
   label polldesc="CAS description" saroaddc="SAROAD description";
run;
proc print data=temp2 uniform label;
   where KEEP = "Y";
   var pollcode polldesc SAROAD saroaddc react factor;
   label polldesc="CASDESC";
run;
title2;

*** NEW QA: SUM POLLUTANTS BY SAROADDC ***;
proc sort data=haplist1(keep=pollcode polldesc saroaddc) out=cas_name nodupkey; by pollcode polldesc; run;
data y;
   set cas_name;
   by pollcode polldesc; 
   if sum(first.polldesc,last.polldesc) ne 2;
run;
proc print data=y;
  title "Non-Unique Pollutant Descriptions for the SAME CAS numbers";
  title2 "Need to fix: &GENHAP";
  var pollcode polldesc saroaddc;
run;
data b4_HAPGEN(keep=cas polldesc inpGENemis);
   merge POLLStmp(in=a rename=(emis=inpGENemis)) cas_name(in=b rename=(pollcode=cas));
   by cas;
   if a and b;
run; 
title;
*** ;

data haplist;
   set haplist1(keep=pollcode saroad react keep factor nti_hap);
run;

title2 "CAS numbers of pollutants";  * for the 1st pollcode list;

***  PRINT CAS NUMBERS IN EMISSIONS FILE NOT MATCHED IN HAPTABLE FILE;
title2 "*** Warning - POLLUTANTS IN EMISSIONS FILE NOT MATCHED TO HAP TABLE ***";
proc sort data=haplist1;
   by pollcode;
run;
proc sort data=casnum;
   by cas;
run;
data temp1;
   merge haplist1(in=in1) casnum(in=in2 rename=(cas=pollcode));
   by pollcode;
   if in2 and not in1;
run;
proc print data=temp1 split="*";
run;
title2;

proc datasets;
   delete temp1 temp2 y;
run;

%MEND READHAPS;


*==================*;
%MACRO COLLAPSE;
*==================*;

*** COLLAPSE (FROM CAS) TO SAROAD LEVEL ***;

/* Vars in emission are: SCC FIPS cas nti_hap mact SIC cat_name emis react (and spatsurr if AR).
   Need to save cat_name for MERGEBIN.
   DYL - Need to keep mact and nti_hap for MACT controls and pollutant specific MACT controls
   RAM - Need to keep SIC for SIC part of GROWTH and CONTROL MACROS */

***  RESET POLLNUM, POLLIST;
proc sort data=emission;
   by SAROAD;
run;
proc means data=emission N SUM noprint;
   by SAROAD;
   var emis;
   output out=Atemp n=n;
run;
data _NULL_;
   if 0 then set Atemp nobs=howmany;
   call symput('NUMPOLL',howmany);
   stop;
run;
proc sql noprint; **noprint option added OCT 2002 *;
   select distinct SAROAD
      into :POLLIST separated by " "
      from Atemp;
quit;

proc datasets;
   delete Atemp merged;
run;

%MEND COLLAPSE;


*==================*;
%MACRO SPECREAC;
*==================*;

***  SELECT AND SPECIATE HAPS, MERGE REACTIVITY CODES INTO EMISSIONS FILE;

***  READ HAPLIST FILE;
%READHAPS;

%IF &LDBG %THEN %DO;
   title2 "debug: emission and haplist before SQL in SPECREAC";
   proc contents data=emission; run;
   proc print data=emission(obs=6); run;
   proc contents data=haplist; run;
   proc print data=haplist(obs=6); run;
%END;

***  MERGE HAPTABLE WITH EMISSIONS FILE;
***  DYL - CAS variable in haplist has been renamed cas_hap to avoid warning message ;
***        Variable will be dropped in the next data step                            ;

Proc SQL noprint;
   Create table merged
     as Select *
     from emission a left join haplist b
     on lowcase(trim(a.cas)) = lowcase(trim(b.pollcode))
   ;
quit;

*** WINDOW TO HAPS DESIGNATED TO BE KEPT ***;
data emission(drop=keep pollcode) work2.noreact;
   set merged;
   if keep = 'Y' then do;
      if react>0 then output emission;
      else output work2.noreact;
   end;
run;

*** MARCH 2003:  Remove work2.noreect if it has zero-obs ***;
data _null_;
  set sashelp.vtable(keep=nobs libname memname);
  where libname = 'WORK2' and memname eq "NOREACT";
  call symput('keepw2',nobs);
run;

%IF &keepw2 %THEN %DO;
   ***  PRINT Warning MESSAGES IF THERE ARE NON-MATCHES;
   title2 "Warning- When merging reactivity codes with emissions,";
   title3 "Records with no matching reactivity code were encountered and dropped.";
   title4 "These are the first 10 non-matched records.";
   proc print data=work2.noreact(obs=10) split="*";
   run;
   title4 "SUMMARY OF NON-MATCHED EMISSIONS BY POLLUTANT.";
   proc means data=work2.noreact N SUM MAX noprint;
      by cas;
      var emis;
      output out=tmpsum1 n=nrecords sum=emis max=maxemis;
   run;
   proc print split="*" label;
      label maxemis="Maximum*Emissions" nrecords="# of*Records";
      var cas emis maxemis nrecords;
      sum emis nrecords;
   run;
   proc datasets library=work;
      delete tmpsum1;
   quit;
%END;
%ELSE %DO;
   proc datasets library=work2;
      delete noreact;
   quit;
%END;
title2;

%IF &LDBG %THEN %DO;
   ***  PRINT EMISSIONS SUMMARY;
   title2 "Pollutant Sums by CAS Before Collapse over SAROAD, After haptable/emis sql merge";
   proc sort data=emission;
      by cas;
   run;
   proc means data=emission noprint;
      var emis;
      by cas;
      output out=temp1 n=n sum=emis;
   run;
   proc print data=temp1;
      var cas emis n;
      sum emis n;
   run;
   title2;
   proc datasets;
      delete temp1;
   quit;
%END;

********** NEW ****************************************************************************;
%if "&spechap" ne "NONE" and %quote(&spechap) ne %then %do;
data b4_spec;
   set emission;
   emis=emis*factor;

proc sort data=b4_spec; by cas saroad; run;
proc summary data=b4_spec sum;
   id factor;
   by cas saroad;
   var emis;
   output out=qa_b4(drop=_type_ _freq_) sum=inpemis n=inpcount;
run;
data HAPspec;
   infile "&INPFILES./&SPECHAP..txt" firstobs=5 missover pad lrecl=212;
   input @1 polldesc $45. @47 cas $10. @58 olds1 $5. @64 news1 $5. @70 olds2 $5.
         @76 news2 $5. @82 olds3 $5. @88 news3 $5. @94 spec_fac 7. @101 MACTcode $7.
         @109 SCCcode $10. @120 SICcode $4.;
run;
proc sort data=emission; by cas; run;
proc sort data=HAPspec(keep=cas) out=SPECcode nodupkey; by cas; run;
*** Extracts data with CAS numbers in the HAPspec file;
data nospec spec;
   merge emission(in=A) SPECcode(in=B);
   by cas;
   if A and B then output spec;
   if A and not B then output nospec;
run;

data specmact(drop=SICcode SCCcode) specsic(drop=MACTcode SCCcode) specscc(drop=MACTcode SICcode) def_spec(drop=MACTcode SICcode SCCcode);
   set HAPspec;
   if MACTcode ne '' then output specmact;
   else if SICcode ne '' then output specsic;
   else if SCCcode ne '' then output specscc;
   else output def_spec;
run;
proc sql;
   create table mactsql
   as select *, 1 as specflag
   from spec a,
        specmact (rename=(cas=pollcode)) b
   where (a.cas=b.pollcode) and (a.MACT=b.MACTcode)
;
   create table sccsql
   as select *, 2 as specflag
   from spec a,
        specscc (rename=(cas=pollcode)) b
   where (a.cas=b.pollcode) and (a.SCC=b.SCCcode)
;
   create table sicsql
   as select *, 3 as specflag
   from spec a,
        specsic (rename=(cas=pollcode)) b
   where (a.cas=b.pollcode) and (a.SIC=b.SICcode)
;
   create table defsql
   as select *, 4 as specflag
   from spec a,
        def_spec (rename=(cas=pollcode)) b
   where (a.cas=b.pollcode)
;
quit;
** Obtain real saroad, sort and retain only lowest specflag (MACT > SIC > SCC > Default) **;
***** MAY 2002 -Retain OLD SAROAD emissions for QA purposes ******;
data allspec(drop=olds1-olds3 news1-news3 pollcode);
   set mactsql(drop=MACTcode) sicsql(drop=SICcode) sccsql(drop=SCCcode) defsql; *bug MACT should be MACTcode;
   if saroad = olds1 then saroad = news1;
   else if saroad = olds2 then saroad = news2;
   else saroad = news3;
run;
proc sort data = allspec;
   by FIPS SCC cas saroad specflag;
run;
data SPEC_fin;
   length spec_hap $26;
   set allspec;
   by FIPS SCC cas saroad specflag;
   if first.saroad;
   select (specflag);
      when (1) spec_hap = 'Obtained from HAP/MACT';
      when (2) spec_hap = 'Obtained from HAP/SIC';
      when (3) spec_hap = 'Obtained from HAP/SCC';
      otherwise spec_hap = 'Pollcode Default';
   end;
run;

data emission(drop=specflag);
   set nospec SPEC_fin;
   if spec_fac = . then spec_fac = 1.0;
run;
%end;
***** END NEW ***;

***********************************************************;
***  Apply HAP-GEN/HAP-SPEC Table factor to emissions   ***;
***********************************************************;

%if "&spechap" ne "NONE" and %quote(&spechap) ne %then %do;
   data emission;
      set emission(drop=polldesc);
      factor = factor * spec_fac;
      emis = emis * factor;
   run;
%end;
%else %do;
   data emission;
      set emission;
      emis = emis * factor;
   run;
%end;

*** QA of Application of Factor ***;

proc sort data=emission;
   by cas saroad;

proc summary sum data = emission;
   var emis;
   by  cas saroad;
   id factor;
   output out=PollSum(drop=_type_ _freq_) sum=emis n=count;
run;

%if "&spechap" ne "NONE" and %quote(&spechap) ne %then %do;
   ****** PRINT Before and After (HAP tables) Emission splits ***;
   data b4_after; ** Pollcode/SAROAD emission sums before and after HAP-SPEC **;
      merge qa_b4(rename=(factor=inpfac)) PollSum;
      by cas saroad;
   run;
   data b4_after_;
      merge b4_HAPGEN b4_after;
      by cas;
      if not first.cas then inpGENemis = .;
   run;
   proc sort data=haplist1(keep=pollcode saroad polldesc saroaddc) out=hapgen_(rename=(pollcode=cas));
      by pollcode saroad;
   run;
   proc sort data=b4_after_; by cas saroad; run;
   data finalQA;
      merge b4_after_(in=a) hapgen_;
      by cas saroad;
      if a;
   run;
   proc print data=finalQA noobs split='*';
      format polldesc $20. saroaddc $30.;
      label cas = 'CAS'
            saroad = 'SAROAD*Code'
            polldesc="CAS description"
            saroaddc="SAROAD description"
            inpGENemis = 'BEFORE*HAP-GEN*Emissions'
            inpemis = 'BEFORE*HAP-SPEC*Emissions'
            emis = 'AFTER*HAP-SPEC*Emissions'
            inpfac = 'HAP-GEN* Table*Factor'
            factor = 'Combined*HAP-GEN/*HAP-SPEC*Table*Factor';
       sum inpGENemis inpemis emis;
       var cas saroad polldesc saroaddc inpGENemis inpemis emis inpfac factor;
       title "Pollutant Level Summary of Emissions Retained for &MODEL Modeling After Application of Factor";
       title2 'Obtained BEFORE and AFTER HAP-GEN and HAP-SPEC factors applied';
       title3 'HAP-GEN/SPEC factor is product of HAP-GEN and HAP-SPEC factors';
   run;
   title3 ;
   proc datasets;
      delete nospec spec spec_fin allspec defsql sccsql sicsql mactsql b4_spec 
         specscc specmact specsic speccode hapspec haplist b4_after def_spec casnum;
   quit;
%end;
%else %do;
   proc print data=PollSum noobs split='*';
      label cas = 'CAS'
            saroad = 'SAROAD Code'
            emis = 'HAP*Emissions'
            factor = 'HAP Table*Factor';
       sum emis;
       var cas saroad emis factor;
       title2 'HAP-SPEC TABLE NOT USED: set to NONE in Batch file';
   run;
%end;
title;
****** END PRINT Before and After (HAP tables) Emission splits ***;

***********************************************************************************;
***  Accumulate emission by site ID, emission release point and SAROAD code ***;
***********************************************************************************;

proc sort data=emission(drop=factor) ;
   by FIPS SCC cat_name SAROAD %IF "&EMISTYPE" = "AR" %THEN %DO; spatsurr %END;;

proc summary sum data=emission;
   by FIPS SCC cat_name SAROAD react %IF "&EMISTYPE" = "AR" %THEN %DO; spatsurr %END;;
   id nti_hap mact sic %IF &amsvar %THEN %DO; AMS %END;;
  var emis;
  output out=emission(drop=_type_ _freq_) sum=;
run;

*** COLLAPSE (FROM CAS) TO POLLCODE (SAROAD) LEVEL ***;
%COLLAPSE

***  PRINT EMISSIONS TRACKING SUMMARY  ***;
proc sort data=emission;
   by saroad;
run;
title2 "Summary of Emission Rates by Pollutant (tpy)";
title3 "After Collapsing from CAS to SAROAD";
title4 "QA: Tracking Emission Rate Totals";
proc means data=emission N SUM noprint;
   by saroad;
   var emis;
   output out=tmpsum1 n=nrecords sum=emis;
run;
proc print split="*";
   var saroad emis nrecords;
   sum emis nrecords;
   label nrecords="# of*records";
run;
title2;

%MEND SPECREAC;

*==================*;
%MACRO MERGEBIN;
*==================*;

***  ADD THE EMISSIONS SOURCE CATEGORY GROUP (BIN) TO THE FILE.
***  ALSO ADD CATCODE AND THE COUNTY URBAN/RURAL FLAG;

***  READ THE COUNTY URBAN/RURAL FLAG FILE;
data cnty_ur;
   infile "&INPFILES./&CNTYUR_..txt" stopover firstobs=3;
   input fips $ 1-5 cnty_ur $ 53;
   if cnty_ur="U" then cnty_ur="1";
   if cnty_ur="R" then cnty_ur="2";
run;

***  MERGE COUNTY URBAN/RURAL FLAG INTO EMISSIONS;
proc sort data=emission out=aremis;
   by fips;
run;
proc sort data=cnty_ur;
   by fips;
run;
data emission(%if &amsvar %then %do; rename=(ams=scc scc=scc_inv) %end;) nomatch;
   %if &amsvar %then %do;
      label ams='SCC: inventory AMS' scc='SCC_INV: inventory SCC';
   %end;
   merge aremis(in=in1) cnty_ur(in=in2);
   by fips;
   if in1 and not in2 then do;
      output nomatch;
      cnty_ur = "2";  * rural is the default;
   end;
   if in1 then output emission;
run;

***  PRINT Warning MESSAGES IF THERE ARE NON-MATCHES;
title2 "*** Warning *** THERE ARE NON-MATCHED COUNTY URBAN/RURAL CODES ***";
proc print data=nomatch(obs=30) split="*"; run;
proc freq data=nomatch;
   table fips;
run;

***  READ THE EMISSION SOURCE GROUP DEFINITION FILE;
%if &amsvar %then %do;
   data emisbins;
      length Cat_Name $90 CatCode $4;
      infile "&INPFILES./&EMISBINS..txt" firstobs=8 missover;
      input Cat_Name $ 1-90 CatCode $ 91-94 UrbanBin 96-97 RuralBin 99-100 src_type $102;
      Cat_Name = UPCASE(Cat_Name);
   run;
   proc sort data=emission; by cat_name; where emis ne 0; run;
   ***  MERGE INTO THE EMISSIONS FILE;
   proc sort data=emisbins; by cat_name; run;
%end;
%else %do;
   data emisbins;
      length SCC $10 CatCode $4;
      infile "&INPFILES./&EMISBINS..txt" firstobs=3 missover;
      input SCC $ 1-10 CatCode $ 12-15 UrbanBin 17-18 RuralBin 20-21 src_type $23;
   run;
   proc sort data=emission; by SCC; where emis ne 0; run;
   ***  MERGE INTO THE EMISSIONS FILE;
   proc sort data=emisbins; by SCC; run;
%end;

data emission emisonly binsonly;
   length group $2;
   merge emission(in=in1) emisbins(in=in2);
   %if &amsvar %then %do;
      by cat_name
   %end;
   %else %do;
      by SCC
   %end;;
   %IF (%upcase("&MODEL") eq "ISC") %THEN %DO;
      if cnty_ur = "1" then group= left(put(UrbanBin,z2.));
      else group=left(put(RuralBin,z2.));
   %END;
   %ELSE %DO;
      if cnty_ur = "1" then group= left(put(UrbanBin,2.));
      else group=left(put(RuralBin,2.));
   %END;
   drop UrbanBin RuralBin;
   if in1 and not in2 then output emisonly;
   else if in2 and not in1 then output binsonly;
   else output emission;
run;

***  PRINT Warning MESSAGES IF THERE ARE NON-MATCHES;
title2 "*** Warning *** EMISSIONS NOT MATCHED TO CATEGORY GROUPS FOR THESE CATEGORIES ***";
%if &amsvar %then %do;
   proc sort data=emisonly(keep=cat_name) out=temp1 nodupkey;
      by cat_name;
   run;
%end;
%else %do;
   proc sort data=emisonly(keep=SCC) out=temp1 nodupkey;
      by SCC;
   run;
%end;
proc print data=temp1 split="*"; run;

proc datasets;
   delete cnty_ur aremis temp1 emisonly binsonly nomatch;
quit;

%MEND MERGEBIN;

*==================*;
%MACRO MERGESAF;
*==================*;

* ----------------------------------------- *;
***  READ THE SPATIAL SURROGATE-TO-SCC CROSS-REFERENCE FILE;
data surrxref;
   length sccdesc $200;
   infile "&INPFILES./&SURRXREF..txt" firstobs=2 pad;
   input SCC $ 1-10 spatsurr 14-16 sccdesc $18-207;
   SCC = left(SCC);
run;
proc sort data=surrxref nodupkey;
   by SCC;
run;

***  MERGE THE SPATIAL SURROGATE CODES INTO EMISSIONS FILE;

%IF "&EMISTYPE" = "MV" %THEN %DO;
   proc sort data=emission;
      by SCC;
   run;
   data emission;
      merge emission(in=in1) surrxref;
      by SCC;
      if in1;
   run;
%END;
%ELSE %DO;
   proc sort data=emission;
      by SCC;
   run;
   data emission;
      merge emission(in=in1) surrxref(keep=SCC sccdesc);
      by SCC;
      if in1;
   run;
%END;

%IF "&SURRDESC" ne "NONE" and %quote(&SURRDESC) ne %THEN %DO;
   data surrdesc;
      length surrdesc $100;
      infile "&INPFILES./&SURRDESC..csv" dlm=',' dsd firstobs=2;
      input spatsurr surrdesc $;
   run;
   proc sort data=surrdesc nodupkey; by spatsurr; run;
   proc sort data=emission; by spatsurr; run;
   data emission;
      merge emission(in=a) surrdesc;
      by spatsurr;
      if a;
   run;
%END;
data tmpemis1 work2.nosurr;
   set emission(rename=(spatsurr=surr));
   if surr LE 0 then do;
      surr = &DEFLTSAF; *Assign non-matches to Default surrogate;
      output work2.nosurr;
   end;
   output tmpemis1;
run;

title2 "Assignment of Spatial Surrogates to Source Categories";
proc sort data=tmpemis1;
   by SCC surr;
run;
proc summary data=tmpemis1 N SUM;
   by SCC surr;
   id sccdesc %IF &SURRDESC ne NONE and &SURRDESC ne %THEN %DO; surrdesc %END;;
   var emis;
   output out=tmpsum1 n=nrecords sum=emis;
run;
proc print split="*";
   var SCC sccdesc surr %IF &SURRDESC ne NONE and &SURRDESC ne %THEN %DO; surrdesc %END; nrecords emis;
   sum emis nrecords;
   label emis="Emissions*(tpy)" nrecords="# of*records" surr="Spatial*surrogate" sccdesc="SCC*Description"
      %IF &SURRDESC ne NONE and &SURRDESC ne %THEN %DO; surrdesc="Surrogate*Description" %END;;
run;

title2 "Surrogate/HAP-level summary of emissions matched to spatial surrogates";
proc sort data=tmpemis1;
   by surr saroad;
run;
proc summary data=tmpemis1 N SUM;
   by surr saroad;
   %IF &SURRDESC ne NONE and &SURRDESC ne %THEN %DO; id surrdesc %END;;
   var emis;
   output out=tmpsum1 n=nrecords sum=emis;
run;
proc print split="*";
   var surr %IF &SURRDESC ne NONE and &SURRDESC ne %THEN %DO; surrdesc %END; saroad emis nrecords;
   sum emis nrecords;
   label emis="Emissions*(tpy)" nrecords="# of*records" surr="Spatial*surrogate"
     %IF &SURRDESC ne NONE and &SURRDESC ne %THEN %DO; surrdesc="Surrogate*Description" %END;;
run;

data _null_;
  set sashelp.vtable(keep=nobs libname memname);
  where libname = 'WORK2' and memname eq "NOSURR";
  call symput('keepw2',nobs);
run;

%IF &keepw2 %THEN %DO;
   ***  PRINT Warning MESSAGES IF THERE ARE NON-MATCHES;
   title2 "Warning- When merging spatial surrogate codes with emissions,";
   title3 "Records with no matching surrogate code were encountered and assigned to SAF= &DEFLTSAF.";
   title4 "These are the SCC codes which did not match to spatial surrogates.";
   proc sort data=work2.nosurr out=tmp1 nodupkey;
      by SCC;
   run;
   proc print data=tmp1;
      var SCC;
   run;

   title4 "These are the first 40 non-matched records.";
   proc print data=work2.nosurr(obs=40) split="*";
      var SCC group fips cat_name saroad surr mact sic emis;
   run;

   title4 "SUMMARY OF NON-MATCHED EMISSIONS BY POLLUTANT.";
   proc sort data=work2.nosurr;
      by saroad;
   run;
   proc summary data=work2.nosurr N SUM MAX;
      by saroad;
      var emis;
      output out=tmpsum1 n=nrecords sum=emis max=maxemis;
   run;
   proc print split="*" label;
      label maxemis = "Maximum*emissions" nrecords="# of*Records";
      var saroad emis maxemis nrecords;
      sum emis nrecords;
   run;

   title4 "SUMMARY OF NON-MATCHED EMISSIONS BY SOURCE CATEGORY.";
   proc sort data=work2.nosurr;
      by SCC saroad;
   run;
   proc means data=work2.nosurr N SUM MAX noprint;
      by SCC saroad;
      var emis;
      output out=tmpsum1 n=nrecords sum=emis max=maxemis;
   run;
   proc print split="*" label;
      label maxemis = "Maximum*emissions" nrecords="# of*Records";
      var SCC saroad emis maxemis nrecords;
      sum emis nrecords;
   run;
   proc datasets library=work;
      delete tmp1;
   quit;
%END;
%ELSE %DO;
   proc datasets library=work2;
      delete nosurr;
   quit;
%END;
title2;

***  PRINT EMISSIONS TRACKING SUMMARY  ***;
proc sort data=tmpemis1 out=emission;
   by saroad;
run;
title2 "Summary of Emission Rates by Pollutant (tpy) - After Spatial Surrogates Assignment";
title3 "QA: Tracking Emission Rate Totals";
proc means data=emission N SUM noprint;
   by saroad;
   var emis;
   output out=tmpsum1 n=nrecords sum=emis;
run;
proc print split="*";
   var saroad emis nrecords;
   sum emis nrecords;
   label nrecords="# of*records";
run;
title2;

proc datasets;
   delete tmpemis1 tmpsum1;
run;

* ----------------------------------------- *;
*   PROCESS EMISSIONS DATA FOR EACH SPATIAL SURROGATE SEPARATELY;
* ----------------------------------------- *;
***  SPLIT EMISSIONS FILE INTO SEPARATE FILES FOR EACH SPATIAL SURROGATE;

***  Count the number of surrogates used;
title2 "Spatial Surrogates Frequency Table";
proc freq data=emission;
   table surr / out=numsurr;
run;

data _NULL_;
   set numsurr; * end=final;
   call symput('SURR'||trim(left(_N_)),surr);
run;

data _NULL_;
   if 0 then set numsurr nobs=howmany;
   call symput('NUMSURR',howmany);
   stop;
run;
***** SUMMING WILL REMOVE ALL SCC, SIC, MACT, and CAT_NAME INFORMATION : AUG2003: RETAIN SCC-level data until after CSV QA files*;
proc sort data=emission out=sccqa nodupkey;  **AUG2003;
   by fips surr scc;
run;
%IF &DIET %THEN %DO;
   proc sort data=emission;
      by surr fips saroad group;
   run;
   proc summary data=emission sum;
      by surr fips saroad group;
      id react nti_hap %if &SURRDESC ne NONE and &SURRDESC ne %then %do; surrdesc %end;;
      var temis1-temis&numtaf emis;
      output out=emission(drop=_type_ _freq_) sum=;
   run;
%END;

data nosurr2 %do IS=1 %to &NUMSURR; SE&IS %end;
     ;
   set emission;
   if surr = -999 then surr = -999;
   %do IS=1 %to &NUMSURR;
      else if surr = &&SURR&IS then output SE&IS;
   %end;
   else output nosurr2;
run;

***  PRINT Warning MESSAGES IF THERE ARE NON-MATCHES;
title2 "Problem splitting emissions by spatial surrogate.  Check NUMSURR.";
proc print data=nosurr2(obs=100) split="*"; run;

proc datasets;
   delete emission numsurr nosurr2;
run;

* ----------------------------------------- *;
*      CREATE ASPEN DEFAULT SECONDARY SAF -assigned in batch file *;
*      ISCST3:  SURROGATE MUST EXIST FOR ALL COUNTIES, ENTIRE COUNTIES *;
* FOR EACH COUNTY, if sum of primary surrogates = 0, AND county emissions > 0,
* THEN  secondary (default) surrogate is applied *;
* ----------------------------------------- *;

%if (%upcase("&Model") = "ASPEN") %then %do;
   data defltsaf;
      length fips $5;
      set inpfiles.&SAFFILE&DEFLTSAF(rename=(saf&DEFLTSAF=ratio));
      fips=substr(cell,1,5);
      keep fips cell ratio lat lon uflag;
   run;
%end;

%else %if (%upcase("&Model") = "ISCTRACT") %then %do; *use ASPEN ready surrogates;
   data defltsaf;
      length fips $5;
      set inpfiles.&SAFFILE&DEFLTSAF(rename=(saf&DEFLTSAF=ratio));
      fips=substr(cell,1,5);
      keep fips cell ratio ;
   run;
%end;

%else %do;
   data defltsaf;
      set inpfiles.&SAFFILE&DEFLTSAF(rename=(hsaf&DEFLTSAF=ratio));

      cell = PUT(col, z3.) || PUT(row, z3.);

*..... compute the SW corner (in UTM) of each gridcell;
      utmx = &xorig + (col - 1) * &cellsize;
      utmy = &yorig + (row - 1) * &cellsize;

      keep fips cell ratio utmx utmy ;
   run;
%end;
* ----------------------------------------- *;
*      NOW FOR EACH SPATIAL SURROGATE ;
* ----------------------------------------- *;
%let counter1 = 0;
%do IS=1 %to &NUMSURR;
   %let SIS = %scan(&&SURR&IS,1);

   ***  READ IN THE SAF FILE;

   %if (%upcase("&Model") = "ASPEN") %then %do;
      *+++++++++++ SAFs for ASPEN model ++++++++++++++;
      data tempsaf;
         length fips $5;
         set inpfiles.&SAFFILE&SIS(rename=(saf&SIS=ratio));
         fips=substr(cell,1,5);
         keep fips cell ratio lat lon uflag;
      run;
   %end;

   %else %if (%upcase("&Model") = "ISCTRACT") %then %do;
      *+++++++++++ SAFs for ISC model using ASPEN tract surrogates ++++++++++++++;
      data tempsaf;
         length fips $5;
         set inpfiles.&SAFFILE&SIS(rename=(saf&SIS=ratio));
         fips=substr(cell,1,5);
         keep fips cell ratio ;
      run;
   %end;

   %else %do;
      *+++++++++++ SAFs for ISC model ++++++++++++++;
      data tempsaf;
         set inpfiles.&SAFFILE&SIS(rename=(hsaf&SIS=ratio));
         cell = PUT(col, z3.) || PUT(row, z3.);
      *..... compute the SW corner (in UTM) of each gridcell;
         utmx = &xorig + (col - 1) * &cellsize;
         utmy = &yorig + (row - 1) * &cellsize;
         keep fips cell ratio utmx utmy;
      run;
   %end;

   %IF &LDBG %THEN %DO;
      title2 "saf";
      proc print data=tempsaf(obs=40);
      run;
   %END;
   * ----------------------------------------- *;
   ***  MERGE SAFs INTO EMISSIONS FILE;
   *    Create a table that relates each STATE, COUNTY, SURR, CAS,
        emissions combination to a STATE, COUNTY, SURR, tract+, SAF;
   *    DYL - keep mact and nti_hap variable from emissions file ;
   **   RAM - For ASPEN & ISCST3, if county-level emissions exist and SUM of all ;
   **         county-level (H)SAFs = 0 then use DEFLTSAF surrogate data. ;

   proc sort data=SE&IS;
      by fips;
   run;
   proc sort data=tempsaf;
      by fips;
   run;
   **********************************************************************************;
   proc summary data=SE&IS sum;
      by fips;
      var emis %IF &DIET %THEN %DO; temis1-temis&numtaf %END;;
      output out=cty_sum&IS(drop=_type_ _freq_) sum=;
   run;
   proc summary data=tempsaf sum;
      by fips;
      var ratio;
      output out=primarysaf_sum&IS(drop=_type_ _freq_) sum=;
   run;
   ***** Flag counties where SAF totals are zero and emissions exist ***************;
   data useDEFLT(keep=fips use2saf);
      length use2saf $1;
      merge cty_sum&IS(in=a) primarysaf_sum&IS(in=b);
      by fips;
      if a ;
      if ratio le 0 then use2saf = 'Y'; 
   run;
   %if not &amsvar %then %do;
      title "Warning: Counties have emissions but primary saf&SIS is zero for all tracts";
      title2 "BATCH FILE DEFAULT SURROGATE: &DEFLTSAF IS APPLIED TO THESE COUNTIES";
      title3 "First 5 counties only";
      title4 "A complete list, with SCC-level data, is provided in output file: FIPS_nonmatch_SCC_surr&SIS..csv";
      proc print data=useDEFLT(obs=5);
         where use2saf = 'Y';
         var fips;
      run;
   %end;
   title;
   **** Merge in flag to ensure SQL merges in DEFAULT SAF for counties with missing surrogate data;
   data SE&IS;
      merge SE&IS useDEFLT;
      by fips;
   run; 
   **********************************************************************************;
   %if (%upcase("&Model") = "ASPEN") %then %do;
   *...... ASPEN merge;
   proc sql noprint;
      create table work2.p_SEtab&IS as
         select 
           %IF &DIET %THEN %DO I=1 %TO &numtaf;
              temis&I,
           %END;
           %ELSE %DO;
              %IF &amsvar %THEN %DO;
                scc_inv,
              %END;
              mact, sic, SCC, CatCode, src_type,
           %END;
         E.fips, E.surr, saroad, nti_hap, emis, cell, ratio, lat, lon, uflag, react, group ,  use2saf
         from SE&IS E left join tempsaf F
         on (E.fips = F.fips) and (use2saf ne 'Y')
         where use2saf ne 'Y';
      create table work2.s_SEtab&IS as
         select 
           %IF &DIET %THEN %DO I=1 %TO &numtaf;
              temis&I,
           %END;
           %ELSE %DO;
              %IF &amsvar %THEN %DO;
                scc_inv,
              %END;
              mact, sic, SCC, SCCdesc, CatCode, src_type,
           %END;
           %IF &SURRDESC ne NONE and &SURRDESC ne %THEN %DO;
              surrdesc,
           %END;
         E.fips, E.surr, saroad, nti_hap, emis, cell, ratio, lat, lon, uflag, react, group ,  use2saf
         from SE&IS E left join defltsaf F
         on (E.fips = F.fips) and (use2saf eq 'Y')
         where use2saf eq 'Y';
   quit;
   %end;
   %else %do;
   *...... ISC or ISCTRACT merge;
   proc sql noprint;
      create table work2.p_SEtab&IS as
         select 
           %IF &DIET %THEN %DO I=1 %TO &numtaf;
              temis&I,
           %END;
           %ELSE %DO;
              %IF &amsvar %THEN %DO;
                scc_inv,
              %END;
              mact, sic, SCC, CatCode, src_type,
           %END;
           %IF (%UPCASE("&Model") = "ISC") %THEN %DO;
         E.fips, E.surr, saroad, nti_hap, emis, cell, ratio, utmx, utmy, group ,  use2saf
           %END;
           %ELSE %DO;
             E.fips, E.surr, saroad, nti_hap, emis, cell, ratio, group ,  use2saf
           %END;
         from SE&IS E left join tempsaf F
         on (E.fips = F.fips) and (use2saf ne 'Y')
         where use2saf ne 'Y';
      create table work2.s_SEtab&IS as
         select 
           %IF &DIET %THEN %DO I=1 %TO &numtaf;
              temis&I,
           %END;
           %ELSE %DO;
              %IF &amsvar %THEN %DO;
                scc_inv,
              %END;
              mact, sic, SCC, SCCdesc, CatCode, src_type,
           %END;
           %IF &SURRDESC ne NONE and &SURRDESC ne %THEN %DO;
              surrdesc,
           %END;
           %IF (%UPCASE("&Model") = "ISC") %THEN %DO;
             E.fips, E.surr, saroad, nti_hap, emis, cell, ratio, utmx, utmy, group ,  use2saf
           %END;
           %ELSE %DO;
             E.fips, E.surr, saroad, nti_hap, emis, cell, ratio, group ,  use2saf
           %END;
         from SE&IS E left join defltsaf F
         on (E.fips = F.fips) and (use2saf eq 'Y')
         where use2saf eq 'Y';
   quit;
   %end;
   %let nonmatch = 0;
   data _null_;
     set sashelp.vtable(keep=nobs libname memname);
     where libname = 'WORK2' and memname eq "S_SETAB&IS";
     call symput('nonmatch',nobs);
   run;
   %IF not &amsvar and &nonmatch %THEN %DO;
      %let counter1 = %eval(&counter1 +1);
      proc sort data = work2.s_SEtab&IS(keep=fips surr) out=qasurr nodupkey;
          by fips surr;
      run;
      data qasurr;
         merge qasurr(in=a) sccqa(in=b keep=fips scc surr sccdesc %IF &SURRDESC ne NONE and &SURRDESC ne %THEN %DO; 
                 surrdesc
              %END;);
         by fips surr; 
         if a and b;
      run;
      data _null_;
          length fips $5 SCC $10 sccdesc $200 %if &SURRDESC ne NONE and &SURRDESC ne %then %do; surrdesc $90  %end;;
          set qasurr;
          file "&OUTFILES./FIPS_nonmatch_SCC_SURR.csv" dlm=',' dsd %if &counter1 > 1 %then %do; mod %end;;
          %if &counter1 = 1 %then %do;
            if _n_ = 1 then do;
               put "These are the Counties where assigned surrogate has no data";
               put "Default surrogate # &DEFLTSAF is used instead. Data are sorted by Surrogate, FIPS, then SCC";
               put "FIPS" "," "SCC" "," "SCC DESCRIPTION" "," "SURROGATE" "," "SURROGATE DESCRIPTION";
            end;
          %end;
          put fips $ scc $ sccdesc $ surr %if &SURRDESC ne NONE and &SURRDESC ne %then %do; surrdesc %end;;
      run;
      proc datasets library=work;
         delete qasurr;
      quit;
   %END;
   data work2.SEtab&IS;
      set work2.p_SEtab&IS work2.s_SEtab&IS;
   run;

   proc datasets library=work2;
      delete p_SEtab&IS s_SEtab&IS;
   quit;
   * ----------------------------------------- *;
   %IF &LDBG %THEN %DO;
      title2 "Table AREATAB, after creating table  (MERGESAF)";
      proc contents data=work2.areatab;
      run;
      proc print data=work2.areatab(obs=40);
      run;
      title3 "one cell";
      proc print;
         where cell="&ONECELL";
      run;
      title2;
   %END;

   proc datasets;
      delete tempsaf SE&IS useDEFLT primarysaf_sum&IS cty_sum&IS;
   quit;
%END;  * of loop over spatial surrogates;

***  COMBINE THE BY-SURROGATE EMISSIONS DATASETS;
data work2.areatab;
   set %do IS=1 %to &NUMSURR; work2.SEtab&IS %end; ;
run;

proc datasets library=work2;
   delete %do IS=1 %to &NUMSURR; SEtab&IS %end; ;
quit;

%IF &LDBG %THEN %DO;
   ***  Print emissions totals by spatial surrogate;
   title2 "Emissions totals by spatial surrogate, after sql";
   proc sort data=work2.areatab;
      by surr saroad;
   run;
   proc means data=work2.areatab n sum noprint;
      var emis;
      by surr saroad;
      output out=tmpsum1 n=nrecords sum=emis max=maxemis;
   run;
   proc print split='*' label;
      label maxemis = "Maximum*emissions" nrecords="# of*Records";
      var surr saroad emis maxemis nrecords;
      sum emis nrecords;
   run;

   proc datasets lib=work;
      delete tmpsum1;
   quit;
%END;

proc datasets lib=work;
   delete defltsaf;
quit;

%MEND MERGESAF;


*==================*;
%MACRO APPLYSAF;
*==================*;

***  APPLY THE SPATIAL ALLOCATION FACTORS TO THE EMISSIONS;
***  DYL - keep nti_hap variable ;

%if (%upcase("&Model") = "ASPEN") %then %do;

data outfiles.&OUTNAME8(keep = fips saroad nti_hap cell emis lat lon
      %IF &DIET %THEN %DO I=1 %TO &numtaf; 
          temis&I 
      %END;
      %ELSE %DO;
         %IF &amsvar %THEN %DO;
            scc_inv
         %END;
         SIC SCC MACT CatCode src_type
      %END;
                               uflag react surr group use2saf)
%end;

%else %if (%upcase("&Model") = "ISCTRACT") %then %do;

data outfiles.&OUTNAME8(keep = fips saroad nti_hap cell emis 
      %IF &DIET %THEN %DO I=1 %TO &numtaf;
          temis&I
      %END;
      %ELSE %DO;
         %IF &amsvar %THEN %DO;
            scc_inv
         %END;
         SIC SCC MACT CatCode src_type
      %END;
                            surr group use2saf)
%end;

%else %do;

data outfiles.&OUTNAME8(keep = fips saroad nti_hap cell emis utmx utmy
      %IF &DIET %THEN %DO I=1 %TO &numtaf; 
          temis&I 
      %END;
      %ELSE %DO;
         %IF &amsvar %THEN %DO;
            scc_inv
         %END;
         SIC SCC MACT CatCode src_type
      %END;
                               surr group use2saf)
%end;

    work2.snoratio work2.snocell work2.snoemis;
    set work2.areatab;

   if ratio = . then output work2.snoratio;  * records with no surr match;
   else if cell = ""  then output work2.snocell;
   else if emis = .   then output work2.snoemis;
   else do;   * We have a match with SAF (ratio);
         *oldemis = emis;
         emis = emis * ratio;
         %IF &DIET %THEN %DO I=1 %TO &numtaf; 
             temis&I = temis&I * ratio; 
         %END;
         if emis > 0 then output outfiles.&OUTNAME8;
   end;
run;

*** DONE, REST OF MACRO IS SUMMARIES;
*** MARCH 2003:  Remove all zero-ob permanent SAS outputs ***;

proc datasets lib=work2;
   delete areatab;
quit;

data _null_;
  set sashelp.vtable(keep=nobs libname memname);
  where libname = 'WORK2' and memname in("SNOCELL","SNOEMIS");
  if memname = "SNOCELL" then call symput('keepw2a',nobs);
  if memname = "SNOEMIS" then call symput('keepw2b',nobs);
run;

%IF not &keepw2a or not &keepw2b %THEN %DO;
   proc datasets library=work2;
      delete 
        %IF not &keepw2a %THEN %DO; snocell %END;
        %IF not &keepw2b %THEN %DO; snoemis %END;
      ;
   quit;
%END;


%IF &LDBG %THEN %DO;
   ***  Print emissions totals by spatial surrogate;
   title2 "Emission rate totals by spatial surrogate, after applysaf";
   proc sort data=outfiles.&OUTNAME8;
      by surr saroad;
   run;
   proc means data=outfiles.&OUTNAME8 n sum noprint;
      var emis;
      by surr saroad;
      output out=tmpsum1 n=nrecords sum=emis max=maxemis;
   run;
   proc print split='*' label;
      label maxemis = "Maximum*emissions" nrecords="# of*Records";
      var surr saroad emis maxemis nrecords;
      sum emis nrecords;
   run;
   title2 "outfiles.&OUTNAME8 - before collapsing over SCC  -  APPLYSAF";
   proc contents data=outfiles.&OUTNAME8;
   run;
   proc print data=outfiles.&OUTNAME8(obs=40);
   run;
   title3 "one cell";
   proc print;
      where cell="&ONECELL";
   run;
   title2;
%END;

data _null_;
  set sashelp.vtable(keep=nobs libname memname);
  where libname = 'WORK2' and memname eq "SNORATIO";
  call symput('keepw2',nobs);
run;

%IF &keepw2 %THEN %DO;
   ***  PRINT Warning MESSAGES IF THERE ARE NON-MATCHES;
   title2 "Warning-  When matching spatial surrogates with emissions,";
   title3 "Records with no matching surrogate ratios were encountered and dropped.";
   title4 "These are the first 36 no-ratio records. EMIS=county-level tpy"; **DEC2002;
   proc print data=work2.snoratio(obs=36) split="*";
      %if (%upcase("&Model") = "ASPEN") %then %do;
         var emis saroad nti_hap surr fips cell ratio group lon lat;
      %end;
      %else %if (%upcase("&Model") = "ISCTRACT") %then %do;
         var emis saroad nti_hap surr fips cell ratio group ;
      %end;
      %else %do;
         var emis saroad nti_hap surr fips cell ratio group utmx utmy;
      %end;
   run;

   title4 "SUMMARY OF NO-RATIO EMISSIONS BY POLLUTANT";
   proc sort data=work2.snoratio;
      by saroad;
   run;
   *** DEC 2002: snoratio dataset is split amongst all cells.  DO NOT DOUBLE COUNT *;
   data tmpsum1;
      retain ct;
      set work2.snoratio;
      by saroad;
      if first.saroad then ct=0;
      ct+1;
      if last.saroad then output;
   run;
   proc means data=tmpsum1 SUM noprint;
      by saroad;
      var emis ct;
      output out=tmpsum1 sum=;
   run;
   proc print split="*" label;
      label emis='Emissions*[tpy]' ct='# of*cells';
      var saroad emis ct;
      sum emis ct;
   run;

   title4 "SUMMARY OF NO MATCHING SURROGATE RATIOS BY COUNTY (1st 30)";
   proc sort data=work2.snoratio;
      by fips;
   run;
   *** DEC 2002: snoratio dataset is split amongst all cells.  DO NOT DOUBLE COUNT *;
   data tmpsum1;
      retain ct;
      set work2.snoratio;
      by fips;
      if first.fips then ct=0;
      ct+1;
      if last.fips then output;
   run;
   proc means data=tmpsum1 SUM noprint;
      by fips;
      var emis ct;
      output out=tmpsum1 sum=;
   run;
   proc print split="*" label;
      label emis='Emissions*[tpy]' ct='# of*cells';
      var fips emis ct;
      sum emis ct;
   run;

   %IF not &DIET %THEN %DO;
      title4 "SUMMARY OF NO-RATIO EMISSIONS BY SOURCE CATEGORY";
      proc sort data=work2.snoratio;
         by SCC saroad;
      run;
      proc means data=work2.snoratio N SUM MAX noprint;
         by SCC saroad;
         var emis;
         output out=tmpsum1 n=nrecords sum=emis max=maxemis;
      run;
      proc print split="*" label;
         label maxemis = "Maximum*emissions" nrecords="# of*Records";
         var SCC saroad emis maxemis nrecords;
         sum emis nrecords;
      run;
   %END;

   title4 "SUMMARY OF NO-RATIO EMISSIONS BY SPATIAL SURROGATE and SAROAD";
   proc sort data=work2.snoratio;
      by surr saroad;
   run;
   *** DEC 2002: snoratio dataset is split amongst all cells.  DO NOT DOUBLE COUNT *;
   data tmpsum1;
      retain ct;
      set work2.snoratio;
      by surr saroad;
      if first.saroad then ct=0;
      ct+1;
      if last.saroad then output;
   run;
   proc means data=tmpsum1  N SUM noprint;
      by surr saroad;
      var emis ct;
      output out=tmpsum1 sum=;
   run;
   proc print split="*" label;
      label emis='Emissions*[tpy]' ct='# of*cells';
      var surr saroad emis ct;
      sum emis ct;
   run;
%END;
%ELSE %DO;
   proc datasets library=work2;
      delete snoratio;
   quit;
%END;
title2;

%IF &LDBG %THEN %DO;
   title2 "outfiles.&OUTNAME8  -  APPLYSAF";
   proc contents;
   run;
   proc print data=outfiles.&OUTNAME8(obs=40);
   run;
   title3 "one cell";
   proc print;
      where cell="&ONECELL";
   run;
%END;

   * ----------------------------------------- *;
   ***  PRINT EMISSIONS TRACKING SUMMARY  ***;
   title2 "Summary of Emission Rates by Pollutant (tpy) - After Spatial Allocation";
   title3 "QA: Tracking Emission Rate Totals";
   proc sort data=outfiles.&OUTNAME8(keep=saroad emis) out=Atemp;
      by saroad;
   run;
   proc summary data=Atemp N sum;
      by saroad;
      var emis;
      output out=Ctemp(drop=_type_ _freq_) n=nrecords sum=emis;
   run;
   proc print data=Ctemp split="*";
      var saroad emis nrecords;
      sum emis nrecords;
      label nrecords="# of*records";
   run;
   title2;

   proc datasets;
      delete Atemp Ctemp;
   run;
%MEND APPLYSAF;


*==================*;
%MACRO ISCTAF;
*==================*;

********************************************************************;
*   Macro: ISCTAF                                                  ;
*  This macro reads the ISC temporal allocation factor file,        ;
*  and checks that the factors for each observation are normalized
*  (add to 1).                                                      ;
********************************************************************;
;

***  Read ISC Temporal Allocation Factors;

data INTAF;
   length SCC $10;
   array spwk{24} spwk1-spwk24;
   array suwk{24} suwk1-suwk24;
   array fawk{24} fawk1-fawk24;
   array wiwk{24} wiwk1-wiwk24;
   array spsa{24} spsa1-spsa24;
   array susa{24} susa1-susa24;
   array fasa{24} fasa1-fasa24;
   array wisa{24} wisa1-wisa24;
   array spsu{24} spsu1-spsu24;
   array susu{24} susu1-susu24;
   array fasu{24} fasu1-fasu24;
   array wisu{24} wisu1-wisu24;

   array hr{24} hr1-hr24;

   array tafs{288} tafs1-tafs288;


   infile "&INPFILES./&TAFFILE..txt" lrecl=210 firstobs=3 EOF=LASTREC missover; *lrecl=250 before 9/18;


*..... read the Spring weekday fractions;
   input @1 SCC $10.
            daytype 3.
            seatype 3. @;
      irec + 1;
      if (daytype = 1 AND seatype = 1) then do;
         input dayfrac 7.5
               seafrac 7.5
               junk $12.
               (hr1-hr24) (7.5);
      end;
      else do;
         put @1 'Error!!!  TAF spring weekday not properly ordered at record ' irec;
         put @1 'daytype = ' daytype ' seatype = ' seatype ' SCC = ' SCC;
         stop;
      end;

   do I = 1 to 24;
      spwk(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Summer weekday fractions;
   input @1 SCC $10.
            daytype 3.
            seatype 3. @;
         irec + 1;
      if (daytype = 1 AND seatype = 2) then do;
         input dayfrac 7.5
               seafrac 7.5
               junk $12.
               (hr1-hr24) (7.5);
      end;
      else do;
         put @1 'Error!!!  TAF Summer weekday not properly ordered at record ' irec;
         put @1 'daytype = ' daytype ' seatype = ' seatype ' SCC = ' SCC;
         stop;
      end;

   do I = 1 to 24;
      suwk(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Fall weekday fractions;
   input @1 SCC $10.
            daytype 3.
            seatype 3. @;
         irec + 1;
      if (daytype = 1 AND seatype = 3) then do;
         input dayfrac 7.5
               seafrac 7.5
               junk $12.
               (hr1-hr24) (7.5);
      end;
      else do;
         put @1 'Error!!!  TAF Fall weekday is not properly ordered at record ' irec;
         put @1 'daytype = ' daytype ' seatype = ' seatype ' SCC = ' SCC;
         stop;
      end;

   do I = 1 to 24;
      fawk(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Winter weekday fractions;
   input @1 SCC $10.
            daytype 3.
            seatype 3. @;
         irec + 1;
      if (daytype = 1 AND seatype = 4) then do;
         input dayfrac 7.5
               seafrac 7.5
               junk $12.
               (hr1-hr24) (7.5);
      end;
      else do;
         put @1 'Error!!!  TAF Winter weekday is not properly ordered at record ' irec;
         put @1 'daytype = ' daytype ' seatype = ' seatype ' SCC = ' SCC;
         stop;
      end;

   do I = 1 to 24;
      wiwk(I) = dayfrac * seafrac * hr(I);
   end;



*..... read the Spring Saturday fractions;
   input @1 SCC $10.
            daytype 3.
            seatype 3. @;
         irec + 1;
      if (daytype = 2 AND seatype = 1) then do;
         input dayfrac 7.5
               seafrac 7.5
               junk $12.
               (hr1-hr24) (7.5);
      end;
      else do;
         put @1 'Error!!!  TAF Spring Saturday is not properly ordered at record ' irec;
         put @1 'daytype = ' daytype ' seatype = ' seatype ' SCC = ' SCC;
         stop;
      end;

   do I = 1 to 24;
      spsa(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Summer Saturday fractions;
   input @1 SCC $10.
            daytype 3.
            seatype 3. @;
         irec + 1;
      if (daytype = 2 AND seatype = 2) then do;
         input dayfrac 7.5
               seafrac 7.5
               junk $12.
               (hr1-hr24) (7.5);
      end;
      else do;
         put @1 'Error!!!  TAF Summer Saturday is not properly ordered at record ' irec;
         put @1 'daytype = ' daytype ' seatype = ' seatype ' SCC = ' SCC;
         stop;
      end;

   do I = 1 to 24;
      susa(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Fall Saturday fractions;
   input @1 SCC $10.
            daytype 3.
            seatype 3. @;
         irec + 1;
      if (daytype = 2 AND seatype = 3) then do;
         input dayfrac 7.5
               seafrac 7.5
               junk $12.
               (hr1-hr24) (7.5);
      end;
      else do;
         put @1 'Error!!!  TAF Fall Saturday is not properly ordered at record ' irec;
         put @1 'daytype = ' daytype ' seatype = ' seatype ' SCC = ' SCC;
         stop;
      end;

   do I = 1 to 24;
      fasa(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Winter Saturday fractions;
   input @1 SCC $10.
            daytype 3.
            seatype 3. @;
         irec + 1;
      if (daytype = 2 AND seatype = 4) then do;
         input dayfrac 7.5
               seafrac 7.5
               junk $12.
               (hr1-hr24) (7.5);
      end;
      else do;
         put @1 'Error!!!  TAF Winter Saturday is not properly ordered at record ' irec;
         put @1 'daytype = ' daytype ' seatype = ' seatype ' SCC = ' SCC;
         stop;
      end;

   do I = 1 to 24;
      wisa(I) = dayfrac * seafrac * hr(I);
   end;



*..... read the Spring Sunday fractions;
   input @1 SCC $10.
            daytype 3.
            seatype 3. @;
         irec + 1;
      if (daytype = 3 AND seatype = 1) then do;
         input dayfrac 7.5
               seafrac 7.5
               junk $12.
               (hr1-hr24) (7.5);
      end;
      else do;
         put @1 'Error!!!  TAF Spring Sunday is not properly ordered at record ' irec;
         put @1 'daytype = ' daytype ' seatype = ' seatype ' SCC = ' SCC;
         stop;
      end;

   do I = 1 to 24;
      spsu(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Summer Sunday fractions;
   input @1 SCC $10.
            daytype 3.
            seatype 3. @;
         irec + 1;
      if (daytype = 3 AND seatype = 2) then do;
         input dayfrac 7.5
               seafrac 7.5
               junk $12.
               (hr1-hr24) (7.5);
      end;
      else do;
         put @1 'Error!!!  TAF Summer Sunday is not properly ordered at record ' irec;
         put @1 'daytype = ' daytype ' seatype = ' seatype ' SCC = ' SCC;
         stop;
      end;

   do I = 1 to 24;
      susu(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Fall Sunday fractions;
   input @1 SCC $10.
            daytype 3.
            seatype 3. @;
         irec + 1;
      if (daytype = 3 AND seatype = 3) then do;
         input dayfrac 7.5
               seafrac 7.5
               junk $12.
               (hr1-hr24) (7.5);
      end;
      else do;
         put @1 'Error!!!  TAF Fall Sunday is not properly ordered at record ' irec;
         put @1 'daytype = ' daytype ' seatype = ' seatype ' SCC = ' SCC;
         stop;
      end;

   do I = 1 to 24;
      fasu(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Winter Sunday fractions;
   input @1 SCC $10.
            daytype 3.
            seatype 3. @;
         irec + 1;
      if (daytype = 3 AND seatype = 4) then do;
         input dayfrac 7.5
               seafrac 7.5
               junk $12.
               (hr1-hr24) (7.5);
      end;
      else do;
         put @1 'Error!!!  TAF Winter Sunday is not properly ordered at record ' irec;
         put @1 'daytype = ' daytype ' seatype = ' seatype ' SCC = ' SCC;
         stop;
      end;

   do I = 1 to 24;
      wisu(I) = dayfrac * seafrac * hr(I);
   end;


*.... Fill the TAFS array with the seasonal/daytype, hourly fractions;
   do I = 1 to 24;
      tafs(I) = wiwk(I);
      J = I + 24;
      tafs(J) = spwk(I);
      K = I + 48;
      tafs(K) = suwk(I);
      L = I + 72;
      tafs(L) = fawk(I);

      M = I + 96;
      tafs(M) = wisa(I);
      N = I + 120;
      tafs(N) = spsa(I);
      P = I + 144;
      tafs(P) = susa(I);
      Q = I + 168;
      tafs(Q) = fasa(I);

      R = I + 192;
      tafs(R) = wisu(I);
      S = I + 216;
      tafs(S) = spsu(I);
      T = I + 240;
      tafs(T) = susu(I);
      U = I + 264;
      tafs(U) = fasu(I);
   end;


LASTREC: SCC = left(SCC);

   keep SCC tafs1-tafs288;
   if SCC ne '';

run;


*** Ensure that all TAFs are normalized ***;

data normTAF;
   array tafs{288} tafs1-tafs288;
   set INTAF;

/* Start with tons per hour for each season and daytype
/* Yields tons per daytype per season --------------------*/

   winwk   = sum(of tafs1-tafs24);
   sprwk   = sum(of tafs25-tafs48);
   sumwk   = sum(of tafs49-tafs72);
   falwk   = sum(of tafs73-tafs96);

   winsa   = sum(of tafs97-tafs120);
   sprsa   = sum(of tafs121-tafs144);
   sumsa   = sum(of tafs145-tafs168);
   falsa   = sum(of tafs169-tafs192);

   winsu   = sum(of tafs193-tafs216);
   sprsu   = sum(of tafs217-tafs240);
   sumsu   = sum(of tafs241-tafs264);
   falsu   = sum(of tafs265-tafs288);

/* Yields tons per season ---------------------------*/

   winter  = sum(winwk * 5,winsa,winsu) * 13 ;
   spring  = sum(sprwk * 5,sprsa,sprsu) * 13;
   summer  = sum(sumwk * 5,sumsa,sumsu) * 13;
   fall    = sum(falwk * 5,falsa,falsu) * 13;

/* Yields tons per year ---------------------------*/

   alltafs = sum(winter,spring,summer,fall);

   %do J = 1 %to 288;
       tafs{&J} = tafs{&J} / alltafs * (364/&ndays); ** JAN 2003: factor added to conserve mass;
   %end;

run;

title2 "Temporal Allocation Factor Sums < 0.9 or > 1.1  (ISCTAF)";
proc print data=normTAF;
   var SCC alltafs tafs1-tafs288;
   where alltafs not between 0.9 and 1.1;


*** Remove normalization variables ***;

data TAF;
  set normTAF;
  drop alltafs winwk sprwk sumwk falwk winsa sprsa sumsa falsa winsu sprsu sumsu falsu;
run;

*** Delete temporay files ***;

proc datasets;
   delete INTAF normTAF;
quit;


%MEND ISCTAF;


*==================*;
%MACRO READTAF;
*==================*;
 ***  READ TEMPORAL ALLOCATION FACTORS for ASPEN;
 ***  These factors, when multiplied by emissions rate, do not change
      the units of emissions rate (this is done in macro write2).
      Thus, the MEAN of a set of temporal factors is 1.0, as opposed to
      the SUM being 1.0;

data TAF;
   length SCC $10;
   array tafA{24} taf1-taf24;
   array TF3HRA{8} TF3HR1-TF3HR8;
   retain TF3HRA;

   infile "&INPFILES./&TAFFILE..txt" lrecl=205 firstobs=3;
   input SCC $ 1-10
   %do J = 1 %to 24;
       tafA{&J}
   %end;
   ;
   SCC = left(SCC);

   ***  Calculate 3-hourly TAFs from hourly TAFs;
   %do I = 1 %to 8;
      J = 3*(&I-1) + 1;
      TF3HRA{&I} = ( tafA{J} + tafA{J+1} + tafA{J+2} ) * 8;
   %end;

   drop taf1-taf24 J;
run;
proc sort data=TAF nodupkey;
   by SCC;
run;

***  ENSURE THAT THE TAFs ARE NORMALIZED;
data TAF(drop=avetaf);
   array TF3HRA{8} TF3HR1-TF3HR8;
   set TAF;
   avetaf = sum(of TF3HR1-TF3HR8) / 8;
   %do J = 1 %to 8;
       TF3HRA{&J} = TF3HRA{&J} / avetaf;
   %end;
run;

%MEND READTAF;


*==================*;
%MACRO APPLYTAF;
*==================*;
***  MERGE THE TAFs INTO THE EMISSIONS FILE AND APPLY THEM;
proc sort data=TAF;
   by SCC;
run;

%IF &DIET %THEN %DO;
   proc sort data=emission out=temp12;
      by SCC;
   run;

%END;
%ELSE %DO;
   proc sort data=outfiles.&OUTNAME8 out=temp12;
      by SCC;
   run;
%END;

%if (%upcase("&Model") = "ASPEN") %then %do;

*+++++++ Apply TAFs for the ASPEN model +++++++++++;

data %IF &DIET %THEN %DO;
         emission(drop=TF3HR1-TF3HR8) 
     %END;
     %ELSE %DO;
         outfiles.&OUTNAME8
     %END;
     work2.notaf;
   array TF3HRA{8} TF3HR1-TF3HR8;
   array tEmisA{8}  tEmis1-tEmis8;
   merge temp12(in=in1) TAF(in=in2);
   by SCC;

   ***  Assign uniform profile to non-matches;
   if in1 and not in2 then do;
      %do I = 1 %to 8;
         TF3HRA{&I} = 1;
      %end;
   end;

   ***  TEMPORALLY ALLOCATE THE EMISSIONS FOR THIS RECORD;
   ***  Input emissions have units of annual average emissions rate;
   %do I = 1 %to 8;
      tEmisA{&I} = emis * TF3HRA{&I};
   %end;

   if abs(sum(of TF3HR1-TF3HR8)-8) > 0.01 then do;
      put " TAF sum NE 1 " TF3HR1-TF3HR8 SCC= %IF not &DIET %THEN %DO; cell= %END;;
      ABORT ABEND;
   end;

   ***  LABEL VARIABLES;
   label temis1="Emissions [tpy]*Hrs 0-2"    temis2="Emissions [tpy]*Hrs 3-5"
         temis3="Emissions [tpy]*Hrs 6-8"    temis4="Emissions [tpy]*Hrs 9-11"
         temis5="Emissions [tpy]*Hrs 12-14"  temis6="Emissions [tpy]*Hrs 15-17"
         temis7="Emissions [tpy]*Hrs 18-20"  temis8="Emissions [tpy]*Hrs 21-23"
         emis = "Base Emis [tpy]";

   if in1 then output %IF &DIET %THEN %DO;
         emission 
     %END;
     %ELSE %DO;
         outfiles.&OUTNAME8
     %END;
   ;
   if in1 and not in2 then output work2.notaf;
run;

%end;

%else %do;

*++++++ Apply TAFs for the ISC or ISCTRACT model +++++++++++++;
data %IF &DIET %THEN %DO;
         emission(drop=tafs1-tafs288) 
     %END;
     %ELSE %DO;
         outfiles.&OUTNAME8
     %END;
     work2.notaf;
   label temis1="Base Emissions*Hr1 Winter Weekday [t/hr]"
         temis25="Base Emissions*Hr1 Spring Weekday [t/hr]"
         temis49="Base Emissions*Hr1 Summer Weekday [t/hr]"
         temis73="Base Emissions*Hr1 Autumn Weekday [t/hr]"
         temis97="Base Emissions*Hr1 Winter Saturday [t/hr]"
         temis121="Base Emissions*Hr1 Spring Saturday [t/hr]"
         temis145="Base Emissions*Hr1 Summer Saturday [t/hr]"
         temis169="Base Emissions*Hr1 Autumn Saturday [t/hr]"
         temis193="Base Emissions*Hr1 Winter Sunday [t/hr]"
         temis217="Base Emissions*Hr1 Spring Sunday [t/hr]"
         temis241="Base Emissions*Hr1 Summer Sunday [t/hr]"
         temis265="Base Emissions*Hr1 Autumn Sunday [t/hr]"
         emis = "Base Emis [tpy]";
   array tafs{288} tafs1-tafs288;
   array tEmisA{288}  tEmis1-tEmis288;
   merge temp12(in=in1) TAF(in=in2);
   by SCC;

   ***  Assign uniform profile to non-matches;
   if in1 and not in2 then do;
      %do I = 1 %to 288;
      ***Prior to JAN2003:  8760 was used (# hours in 365 days). &ndays*24 must be used for consistency;
         tafs{&I} = 1 / (&ndays*24); *JAN2003 -ensures conservation of mass;
      %end;
   end;

   ***  TEMPORALLY ALLOCATE THE EMISSIONS FOR THIS RECORD;
   ***  Input emissions have units of annual average emissions rate;
   %do I = 1 %to 288;
      tEmisA{&I} = emis * tafs{&I};
   %end;

   if in1 then output %IF &DIET %THEN %DO;
         emission 
     %END;
     %ELSE %DO;
         outfiles.&OUTNAME8
     %END;
   ;
   if in1 and not in2 then output work2.notaf;
run;

%end;


proc datasets;
   delete temp12;
run;

%IF not &DIET %THEN %DO; ** Do not perform this if you have HUGE inventory*;
   %if (%upcase("&Model") = "ASPEN") %then %do;

      ***  PRINT SUMMARY OF PROFILES USED;
      data temp121;
         set outfiles.&OUTNAME8;
         array TF3HRA{8} TF3HR1-TF3HR8;
         keep SCC TF3HR1-TF3HR8;
      run;
      proc sort data=temp121 nodupkey;
         by SCC TF3HR1-TF3HR8;
      run;
      data temp121;
         set temp121;
         mean = sum(of TF3HR1-TF3HR8)/8;
      run;
      title2 "Temporal Factors Used to Allocate Emission Rates to 3-Hour Periods";
      proc print data=temp121;
         var SCC TF3HR1-TF3HR8 mean;
         sum TF3HR1-TF3HR8 mean;
         format TF3HR1-TF3HR8 6.3;
      run;
      title2;
      proc datasets;
         delete temp121;
      run;

   %end;


   ***  PRINT EMISSIONS TRACKING SUMMARY  ***;
   title2 "Summary of Emission Rates by Pollutant (tpy)";
   title3 "After Temporal Factor Merge, Before Collapsing";
   title4 "QA: Tracking Emission Rate Totals";

   proc sort data=outfiles.&OUTNAME8(keep=saroad emis) out=Atemp;
      by saroad;
   run;
   proc summary data=Atemp N sum;
      by saroad;
      var emis;
      output out=Ctemp(drop=_type_ _freq_) n=nrecords sum=emis;
   run;
   proc print data=Ctemp split="*";
      var saroad emis nrecords;
      sum emis nrecords;
      label nrecords="# of*records";
   run;
   title2;

   proc datasets;
      delete Atemp Ctemp;
   run;

   title2 "Warning-  When merging temporal allocation factors with emissions,";
   title3 "Records with no matching TAFs were encountered and assigned uniform profiles.";

   title4 "These are the SCC codes which did not match to temporal factors.";
   proc sort data=work2.notaf out=tmp1 nodupkey;
      by SCC;
   run;
   proc print data=tmp1 split="*";
      var SCC;
   run;
   proc datasets;
      delete tmp1;
   run;

%END; **** NO DIET ONLY;

title2 "Warning-  When merging temporal allocation factors with emissions,";
title3 "Records with no matching TAFs were encountered and assigned uniform profiles.";
title4 "These are the first 50 non-matched records (not all variables listed here).";
proc print data=work2.notaf(obs=50) split="*";
   var emis fips scc saroad nti_hap mact sic catcode src_type group; **DEC2002: other variables are so unneccessary *;
run;

title4 "SUMMARY OF NON-MATCHED EMISSIONS BY POLLUTANT";
proc sort data=work2.notaf;
   by saroad;
run;
proc means data=work2.notaf N SUM MAX noprint;
   by saroad;
   var emis;
   output out=tmpsum1 n=nrecords sum=emis max=maxemis;
run;
proc print split="*" label;
   label maxemis = "Maximum*emissions" nrecords="# of*Records";
   var saroad emis maxemis nrecords;
   sum emis nrecords;
run;

title4 "SUMMARY OF NON-MATCHED EMISSIONS BY SOURCE CATEGORY and SAROAD";
proc sort data=work2.notaf;
   by SCC saroad;
run;
proc means data=work2.notaf N SUM MAX noprint;
   by SCC saroad;
   var emis;
   output out=tmpsum1 n=nrecords sum=emis max=maxemis;
run;
proc print split="*" label;
   label maxemis = "Maximum*emissions" nrecords="# of*Records";
   var SCC saroad emis maxemis nrecords;
   sum emis nrecords;
run;
title2;

proc datasets;
   delete tmpsum1 TAF;
run;
proc datasets lib=work2;
   delete notaf;
run;

%IF &LDBG %THEN %DO;
   title2 "outfiles.+OUTNAME8, after applytaf";
   proc contents data=outfiles.&OUTNAME8; run;
%END;

%MEND APPLYTAF;


*==================*;
%MACRO ISCWRITE(GCFLAG,projname);
*==================*;

*==============================================*;
*   This macro prepares the output SAS
*   dataset for the CountyFinal (ISCST3 model);
*==============================================*;

%IF not &DIET and &GCFLAG ne 1 %THEN %DO;
   %let keepvar = gf gfcode CntlCode Replace ExistEff New_Eff New_Rate AddXEff AddNEff Add_Rate;
%END;
%ELSE %DO;
   %let keepvar = ;
%END;

%if &projname %then %do;
   %let ISCOUTorig = &ISCOUT;
   %let ISCOUT = &ISCOUTorig._&&pname&P;
%end;

***FEB 2003: KEEP SCC (and SCC_INV for NATA);
***MARCH 2004: add ISCTRACT option *JAT ;
%if (%upcase("&Model") = "ISCTRACT") %then %do;
proc sort data= outfiles.&OUTNAME8
   out=outfiles.X&ISCOUT(keep= emis tEmis1-tEmis288 saroad group cell fips nti_hap
        %IF not &DIET %THEN %DO;
           %if &amsvar %then %do;
              SCC_inv
           %end;
           catcode SCC sic mact src_type
        %END;
        &keepvar );

   by saroad group cell;
run;
%end;
%else %do;
proc sort data= outfiles.&OUTNAME8
   out=outfiles.X&ISCOUT(keep= emis tEmis1-tEmis288 saroad group cell utmx utmy fips nti_hap
        %IF not &DIET %THEN %DO;
           %if &amsvar %then %do;
              SCC_inv
           %end;
           catcode SCC sic mact src_type
        %END;
        &keepvar );

   by saroad group cell;
run;
%end;
proc datasets library=outfiles;
   delete &OUTNAME8;
run;
/* COLLAPSE OVER SOURCE CATEGORIES TO GROUP LEVEL */

proc summary data=outfiles.X&ISCOUT sum;
   var emis tEmis1-tEmis288;
   by saroad group cell;
   %if (%upcase("&Model") = "ISC") %then %do;
     id utmx utmy ;
   %end;
   output out=outfiles.&ISCOUT(drop=_type_ _freq_) sum = emis tEmis1-tEmis288;
run;

%IF &DIET %THEN %DO;
   proc datasets library=outfiles;
      delete X&ISCOUT;
   run;
%END;

%if &projname %then %do;
   %let ISCOUT = &ISCOUTorig;
%end;

%MEND ISCWRITE;


*==================*;
%MACRO WRITE1;
*==================*;

***  WRITE FIRST PART OF OUTPUT FILE  ***;

data _NULL_;
   length IDRun $40. IDFile $20.;

****** READ DECAY RATES FOR DIFFERENT REACTIVITY CLASSES *****;
* Array Decay dimensioned by reactivity class, time block, stability;
   infile "&INPFILES./&INDECAY..txt";   * 72 lines;
   Array Decay(10,8) $60 Decay1-Decay80;
   Do j = 1 to 72;
      Input @1 Rct 1. @3 Time 1. @5 Decay(Rct,Time) $60.;
   End;

***  Set run ID and emissions file ID  ***;
   IDrun = 'RunID'; * max length 40;

***  LOOP OVER REACTIVITY CLASSES  ***;

   %Do React = 1 %to 9;
      Pol_type = '0';    * gas;
      Dry_dep  = '1';    * no dry dep except for fine PM;
      Wet_dep  = '1';    * no wet dep except for fine PM;

      If &React = 2 OR &React = 3 then do;     * fine PM;
         Pol_type = '1'; * fine PM;
         Dry_dep  = '0'; * use dry dep;
         Wet_dep  = '0'; * use wet dep;
         if &react = 3 then Pol_type = 2;      * coarse particles;
      end;

***  SPECIFY OUTPUT FILE NAME;
     file "&OUTFILES./&OUTNAME8..&SUBSETG..D&RUNDATE..r&React..inp";

***  SET AND WRITE HEADER ***;
     IDFile = 'Group '||'3'||', React'||" &React "; * max length 20;
      Put @1 IDRun $40. @41 IDFile $20.
          @61 Pol_type $1. @62 Dry_dep $1. @63 Wet_dep $1.;

      Do j = 1 to 8;
         Put @1 Decay(&React,j) $60.;
      End;
   %End;
Run;

%MEND WRITE1;


*==================*;
%MACRO WRITE2(GCFLAG);
*==================*;
***  COLLAPSE OVER SOURCE CATEGORIES TO EMISBIN LEVEL;
***  DYL 11/20/00 - Change to keep only variables necessary in emistemp ***;

proc sort data=outfiles.&OUTNAME8(keep=emis tEmis1-tEmis8 cell saroad group react lat lon fips uflag)
    out=work2.emistemp;
    by cell saroad group;
run;

%IF &DIET %THEN %DO;
   proc datasets library=outfiles;
      delete &OUTNAME8;
   run;
%END;
proc means sum data=work2.emistemp noprint;
   var emis tEmis1-tEmis8;
   by cell saroad group;
   id react lat lon fips uflag;
   output out=&OUTNAME8(drop=_type_ _freq_) sum = emis tEmis1-tEmis8;
run;

proc datasets library=work2;
   delete emistemp;
run;

%IF not &DIET %THEN %DO;
   ***  PRINT EMISSIONS TRACKING SUMMARY  ***;
   title2 "Summary of Emission Rates by Pollutant (tpy)";
   title3 "After Collapsing Source Categories to Emissions Groups";
   title4 "QA: Tracking Emission Rate Totals";

   proc sort data=&OUTNAME8(keep=saroad emis) out= Atemp; by saroad; run;
   proc summary data=Atemp N SUM;
      by saroad;
      var emis;
      output out=Btemp n=nrecords sum=emis;
   run;
   proc print data=Btemp split="*";
      var saroad emis nrecords;
      sum emis nrecords;
      label nrecords="# of*records";
   run;
   title2;
   proc datasets;
      delete Atemp Btemp;
   run;
%END;
***  WRITE THE REST OF THE ASPEN INPUT EMISSIONS FILE  ***;

proc sort data=&OUTNAME8;
   by react group cell saroad;
run;

data outfiles.C&OUTNAME8;
   array tEmisA{8} tEmis1-tEmis8;
   %IF &PROJECT ne NONE and &PROJECT ne and &GCFLAG eq 3 %then %DO;
   label temis1="GC Emissions [g/s]*Hrs 0-2"    temis2="GC Emissions [g/s]*Hrs 3-5"
         temis3="GC Emissions [g/s]*Hrs 6-8"    temis4="GC Emissions [g/s]*Hrs 9-11"
         temis5="GC Emissions [g/s]*Hrs 12-14"  temis6="GC Emissions [g/s]*Hrs 15-17"
         temis7="GC Emissions [g/s]*Hrs 18-20"  temis8="GC Emissions [g/s]*Hrs 21-23";
   %END;
   %ELSE %DO;
   label temis1="Emissions [g/s]*Hrs 0-2"    temis2="Emissions [g/s]*Hrs 3-5"
         temis3="Emissions [g/s]*Hrs 6-8"    temis4="Emissions [g/s]*Hrs 9-11"
         temis5="Emissions [g/s]*Hrs 12-14"  temis6="Emissions [g/s]*Hrs 15-17"
         temis7="Emissions [g/s]*Hrs 18-20"  temis8="Emissions [g/s]*Hrs 21-23";
   %END;

   label emis = "Base emis [g/s]"
         emis_tpy = "Base emis [tpy]";
   length tract1 8 tractc $6 tractr $6 StackID $5;
   length ivent $1 ibldg $1 srcetype $1 uflag $1 nosc $6 nows $6 nowd $6;
   set &OUTNAME8 end=last;
   by react group cell saroad;

   srcetype = "3"; * pseudo-source -used for area/mobile;
   ivent = "1";
   ibldg = "0";
   nosc = "      "; nows = "      "; nowd = "      ";
   wbanid = "     ";

    ***  Set output file name for this react class;
    %do rr = 1 %to 9;
       if react = &rr then do;
             file "&OUTFILES./&OUTNAME8..&SUBSETG..D&RUNDATE..r&rr..inp" mod;
       end;
    %end;

      StackID = substr(cell,1,5);
      tract1 = substr(cell,6,6);
      tractc = put(tract1,6.);
      tractr = right(tractc);
***** WRITE PLANT AND STACK INPUTS *****;
   if first.cell then do;
      Put @1 StackID $5.
          @10 tractr $6.
          @16 lon 10.5
          @26 lat 10.5
          @36 srcetype $1.
          @37 uflag $1.;  * 1=urban, 2=rural;
      Put @1 wbanid $5. Nosc $6. Nows $6. Nowd $6.;
      Put @1; * polar grid distances, blank line is default;
      Put @1 StackID $5.
          @6 lon 10.5
          @16 lat 10.5
          @66 ivent $1. @67 ibldg $1.;
   End;

***  CONVERT EMISSIONS RATE FROM TPY TO GRAMS/SEC;
***  DYL - factor = [2000.0lbs/ton] * [453.592 g/lb] / ( [8760 hrs/yr] * [3600 sec/hr])***;

   emis_tpy = emis;
   emis = emis * 0.0287666;
   %do I = 1 %to 8;
      tEmisA{&I} = tEmisA{&I} * 0.0287666;
   %end;

***** WRITE POLLUTANT EMISSIONS *****;
**NOTE!!!**FEB 2003: GROUP VARIABLE MAY BE EXPANDED TO 2-CHARACTERS LATER IN 2003 ****;
** If this happens, emissions MAY need to shift 1 space to the right **;
   Put @1 saroad $5. @6 group $2. @7
      tEmisA{1} e10.4
      tEmisA{2} e10.4
      tEmisA{3} e10.4
      tEmisA{4} e10.4
      tEmisA{5} e10.4
      tEmisA{6} e10.4
      tEmisA{7} e10.4
      tEmisA{8} e10.4;

***** MARK END OF STACK GROUPING *****;
   If last.cell then Do;
      Put;
      Put;
   End;

   drop tract1 tractc;
   output;
run;

***  PRINT EMISSIONS SUMMARY  ***;
data tmpsumm1;
   set outfiles.C&OUTNAME8;
   keep saroad emis emis_tpy tEmis1-tEmis8 tEmistot;
   tEmistot = sum(of tEmis1-tEmis8) / 8;
   label tEmistot="Daily*rate";
run;
proc sort data=tmpsumm1;
   by saroad;
run;
title2 "Summary of Emission Rates by Pollutant - Output Emission Rates (grams/sec)";
title3 "QA: Tracking Emission Rate Totals";
proc means data=tmpsumm1 N SUM noprint;
   by saroad;
   var emis emis_tpy tEmis1-tEmis8 tEmistot;
   output out=tmpsum1 n=nrecords sum=emis emis_tpy tEmis1-tEmis8 tEmistot;
run;
proc print split="*";
   var saroad emis emis_tpy nrecords;
   sum emis emis_tpy nrecords tEmis1-tEmis8; * tEmistot;
   label emis="Emissions*(g/s)"
         emis_tpy="Emissions*(tpy)" nrecords="# of*records";
   label temis1="Emissions*Hrs 0-2"    temis2="Emissions*Hrs 3-5"
         temis3="Emissions*Hrs 6-8"    temis4="Emissions*Hrs 9-11"
         temis5="Emissions*Hrs 12-14"  temis6="Emissions*Hrs 15-17"
         temis7="Emissions*Hrs 18-20"  temis8="Emissions*Hrs 21-23";
run;
title2;

proc datasets;
   delete tmpsumm1 tmpsum1;
run;

***** MARK END OF ASPEN INPUT FILES *****;
Data _null_;
%do react = 1 %to 9;
       file "&OUTFILES./&OUTNAME8..&SUBSETG..D&RUNDATE..r&React..inp" mod;
   Put;
   Put;
   Put;
%end;
Run;

%MEND WRITE2;



%MACRO RUNTIMES(cty_only);

      title2 "   ";
      title3 "*******  FINISHED  *******";
      title4 "   ";
      title5 "Run Times for Processing Components (hh:mm:ss)";

/* Only complete projection runs will be calculated when projecting emissions */
%IF &PROJECT ne NONE and &PROJECT ne %THEN %DO;

   data runtimes;
      array gc{&numproj} gc1-gc%eval(&numproj);
      array tgc{&numproj} tgc1-tgc%eval(&numproj);
      set runtimes;
      TEND = time();
      total = TEND - TSTART;
      treademi = reademis - tstart;
      tspecrea = specreac - reademis;
      tmergbin = mergebin - specreac;
      %IF not &cty_only %THEN %DO;
         %IF &DIET %THEN %DO;
            treadtaf = readtaf  - mergebin;
            taplytaf = applytaf - readtaf;
            tmergsaf = mergesaf - applytaf;
            taplysaf = applysaf - mergesaf;
            lastemshap = applysaf;
         %END;
         %ELSE %DO;
            tmergsaf = mergesaf - mergebin;
            taplysaf = applysaf - mergesaf;
            treadtaf = readtaf  - applysaf;
            taplytaf = applytaf - readtaf;
            lastemshap = applytaf;
         %END;
      %END;
      %ELSE %DO;
         lastemshap = mergebin;
      %END;

      do I=1 to &numproj;
         IF I = 1 THEN DO;
            tgc{I} = gc{I} - lastemshap;
         END;
         ELSE DO;
            tgc{I} = gc{I} - gc{I-1};
         END;
      end;
   run;

   proc print data=runtimes noobs split="*";
      var    total treademi tspecrea tmergbin %do I=1 %to &numproj; tgc%eval(&I) %end;;
      format total treademi tspecrea tmergbin %do I=1 %to &numproj; tgc%eval(&I) %end; time8.;

      label  total="Total*Time" treademi="Read*Emissions"
             tspecrea="HAP Table*Stuff" tmergbin="Merge*Bins"
             %do I= 1 %to &numproj;
                tgc%eval(&I) = "Growth,*control*for*&OUTNAMEorig._&&pname&I"
             %end;;
   run;
%END;
%ELSE %DO;
/*  PRINT RUNTIME STATS WHEN NO PROJECTIONS ARE MADE */
   %IF (%upcase("&model") = "ASPEN") %THEN %DO;
      data runtimes;
         set runtimes;
         TEND = time();
         total = TEND - TSTART;
         treademi = reademis - tstart;
         tspecrea = specreac - reademis;
         tmergbin = mergebin - specreac;
         %IF &DIET %THEN %DO;
            treadtaf = readtaf  - mergebin;
            taplytaf = applytaf - readtaf;
            tmergsaf = mergesaf - applytaf;
            taplysaf = applysaf - mergesaf;
            tgrwnctl = growcntl - applysaf;
         %END;
         %ELSE %DO;
            tmergsaf = mergesaf - mergebin;
            taplysaf = applysaf - mergesaf;
            treadtaf = readtaf  - applysaf;
            taplytaf = applytaf - readtaf;
            tgrwnctl = growcntl - applytaf;
         %END;
         twrite   = write12  - growcntl;
         tfinish  = finish   - write12;
      run;
   %END;
   %ELSE %DO;
      data runtimes;
         set runtimes;
         TEND = time();
         total = TEND - TSTART;
         treademi = reademis - tstart;
         tspecrea = specreac - reademis;
         tmergbin = mergebin - specreac;
         %IF &DIET %THEN %DO;
            treadtaf = readtaf  - mergebin;
            taplytaf = applytaf - readtaf;
            tmergsaf = mergesaf - applytaf;
            taplysaf = applysaf - mergesaf;
            tgrwnctl = growcntl - applysaf;
         %END;
         %ELSE %DO;
            tmergsaf = mergesaf - mergebin;
            taplysaf = applysaf - mergesaf;
            treadtaf = readtaf  - applysaf;
            taplytaf = applytaf - readtaf;
            tgrwnctl = growcntl - applytaf;
         %END;
         tfinish  = finish   - growcntl;
      run;
   %END;

   title2 "   ";
   title3 "*******  FINISHED  *******";
   title4 "   ";
   title5 "Run Times for Processing Components (hh:mm:ss)";

   %IF (%upcase("&model") = "ASPEN") %THEN %DO;
      proc print data=runtimes noobs split="*";
         var    total treademi tspecrea tmergbin 
             %IF &DIET %THEN %DO;
                 treadtaf taplytaf tmergsaf taplysaf 
             %END;
             %ELSE %DO;
                 tmergsaf taplysaf treadtaf taplytaf
             %END;
             tgrwnctl twrite tfinish;
         format total treademi tspecrea tmergbin tmergsaf taplysaf treadtaf
             taplytaf tgrwnctl twrite tfinish  time8.;
         label  total="Total*Time" treademi="Read*Emissions"
             tspecrea="HAP Table*Stuff" tmergbin="Merge*Bins"
             treadtaf="Read*TAFs" tmergsaf="Merge*SAFs"
             taplysaf="Apply*SAFs" taplytaf="Apply*TAFs"
             twrite="Write*outputs" 
             tgrwnctl="Growth,*control" tfinish="Finish";
      run;
   %END;
   %ELSE %DO;
      proc print data=runtimes noobs split="*";
         var    total treademi tspecrea tmergbin 
             %IF &DIET %THEN %DO;
                 treadtaf taplytaf tmergsaf taplysaf 
             %END;
             %ELSE %DO;
                 tmergsaf taplysaf treadtaf taplytaf
             %END;
             tgrwnctl tfinish;
         format total treademi tspecrea tmergbin tmergsaf taplysaf treadtaf
             taplytaf tgrwnctl tfinish  time8.;
         label  total="Total*Time" treademi="Read*Emissions"
             tspecrea="HAP Table*Stuff" tmergbin="Merge*Bins"
             treadtaf="Read*TAFs" tmergsaf="Merge*SAFs"
             taplysaf="Apply*SAFs" taplytaf="Apply*TAFs"
             tgrwnctl="Growth,*control" tfinish="Finish";
      run;
   %END;
%END;

title2;
%MEND RUNTIMES;


***  ------------  MACROS FOR WRITING OUT ASCII FILE  ------------  ***;
*==================*;
%MACRO WRITEA;
*==================*;

data _NULL_;
   set outfiles.c&OUTNAME8;
   array tEmisA{8} tEmis1-tEmis8;
   length tractr $6;  * tract1 8 tractc;
   length ivent $1 ibldg $1 srcetype $1 uflag $1 nosc $6 nows $6 nowd $6;
   length blank10 $10;

   file "&OUTFILES./&OUTNAME8..txt";

   emis_gps = emis;
   stackID = fips;
   tractr = substr(cell,6,6); * changed from '5' to '6' on 6/6/00 by RAM;
   blank10 = "          ";
   f999 = 999.0;

   put @1 fips $5.
       +1 blank10 $10.
       +1 lon 10.5
       +1 lat 10.5
       +1 srcetype $1.
       +1 uflag 1.
       +1 StackID $5.
       +1 f999 6. +1 f999 6. +1 f999 6. +1 f999 6.
       +1 saroad $5.
       +1 group $2.
       +1 tEmisA{1} e10.
       +1 tEmisA{2} e10.
       +1 tEmisA{3} e10.
       +1 tEmisA{4} e10.
       +1 tEmisA{5} e10.
       +1 tEmisA{6} e10.
       +1 tEmisA{7} e10.
       +1 tEmisA{8} e10.
       +1 tractr $6.
       +1 ivent $1.
       +1 ibldg $1.
       +1 emis_tpy e12.5
       +1 emis_gps e12.5;
run;

%MEND WRITEA;


*==================*;
%MACRO SUMERYS;
*==================*;
***  PRINT EMISSIONS SUMMARY BY GROUPS (BINS);
title2 "Pollutant Sums by Source Category Group";

proc sort data=outfiles.c&OUTNAME8(keep=emis emis_tpy saroad react group) out=b;
   by group saroad react;

proc summary data=b n sum;
   by group saroad react;
   var emis emis_tpy;
   output out=c(drop=_type_) n=nrecords sum=emis_gps emis_tpy;

proc print data=c split="*";;
   var saroad react nrecords emis_tpy emis_gps;
   by group;
   sum nrecords;
   label group="Emissions group" saroad="SAROAD*code" nrecords="# of*records"
         emis_tpy="Emissions*(tpy)" emis_gps="Emissions*(g/sec)"
         react="Reactivity*class";
run;

title2 "Emission rate summaries by reactivity class";
proc sort data=b;
   by react saroad;

proc summary data=b n sum noprint;
   var emis_tpy emis;
   by react saroad;
   output out=c(drop=_type_ _freq_) n=nrecords sum=emis_tpy emis_gps;

proc print data=c split="*";
   var saroad nrecords emis_tpy emis_gps;
   by react;
   sum nrecords;
   label react="Reactivity class" saroad="SAROAD*code" nrecords="# of*records"
         emis_tpy="Emissions*(tpy)" emis_gps="Emissions*(g/sec)";
run;

proc datasets;
   delete b c;
run;

%MEND SUMERYS;


*==================*;
%MACRO WASCII;
*==================*;

%IF not &DIET %THEN %DO;
   %WRITEA;
%END;
%SUMERYS;

%MEND WASCII;
***  --------  END OF MACROS FOR WRITING OUT ASCII FILE  ---------  ***;


*==================*;
%MACRO FINISH;
*==================*;
***  PRINT SUMMARIES OF EMISSIONS WRITTEN TO THE ASPEN FILES
***  AND WRITES OUT THE ASCII FILE;

title2 "Contents of the ASPEN emissions data set C&OUTNAME8";
proc contents data=outfiles.C&OUTNAME8;
run;
%IF not &DIET %THEN %DO;
   title2 "Contents of the full emissions data set &OUTNAME8";
   proc contents data=outfiles.&OUTNAME8;
   run;
%END;

***  ---------------  EMISSIONS SUMMARIES  ---------------  ***;
***     (SUMMARIES OF EMISSIONS WRITTEN TO ASPEN FILES)     ***;

title1 "&RUNID";
title2 "&EMISLABL";
title3 "EMISTYPE: &EMISTYPE, USRLABEL: &USRLABEL, SUBSETG: &SUBSETG, RUNDATE: &RUNDATE";

proc sort data=&OUTNAME8(keep=react cell saroad emis) out=emission;
   by saroad react;                                             *9/11-RAM;
run;

***  EMISSIONS TOTALS BY POLLUTANT;
title4 "Emission Rate Totals by Pollutant (tpy)";
proc summary data=emission;
   by saroad react;
   var emis;
   output out=pollemis n=nrecords sum=sumemis;
run;
proc print data=pollemis split="*";
   var saroad react sumemis Nrecords;
   sum Nrecords sumemis;
   label Nrecords="# of*records"
         react="Reactivity*Class"  sumemis="Total*Emissions";
run;

***  STATE SUMS  ***;
data temp1(keep=state emis);
   set emission;
   state=substr(cell,1,2);
run;
proc sort data=temp1;
   by state;
run;
title2 "Summary of Emission Rates by State (tpy)";
proc summary data=temp1 N SUM;
   by state;
   var emis;
   output out=temp1 n=nrecords sum=emis;
run;
proc print split="*";
   label nrecords="# of*Records";
   var state emis nrecords;
   sum emis nrecords;
run;
proc datasets;
   delete temp1;
run;
title2;

***  EMISSIONS FREQUENCIES AND TOTALS BY REACTIVITY CLASS;

title3 "Frequencies of Emissions Sources by Reactivity Class";
proc freq data=emission;
   table react;
run;

title3 "Emission Rate Totals by Reactivity Class (tpy)";
proc sort data=pollemis;
   by react;
run;
proc means data=pollemis noprint;
   by react;
   var sumemis Nrecords;
   output out=tempp3 sum=sumemis Nrecords N=numpolls;
run;
proc print data=tempp3 split="*";
   var react numpolls sumemis Nrecords;
   sum numpolls sumemis Nrecords;
   label Nrecords="# of*Records" react="Reactivity*Class"
         sumemis="Total*Emissions" numpolls="Number of*Pollutants";
run;

***  ------------  END OF EMISSIONS SUMMARIES  ------------  ***;

proc datasets library=work;
   delete emission pollemis tempp3;
quit;

***  ------------  WRITE OUT ASCII FILE  ------------  ***;
%WASCII;

%MEND FINISH;


*========================================================*;
%MACRO MERGCNTL(eff_file= ,mvar1= ,mvar2= ,mvar3= ,code= );
*========================================================*;
*** Determine if incoming control file contains any records ***;

proc contents data=&eff_file out=chkobs noprint;

data _null_;
   set chkobs (obs=1);
   call symput('notempty',nobs);
run;

%If &notempty %then %do;

*** Separate the control file into two, one with geographic-level controls, one without ***;

   %IF &cntycodes %THEN %DO;
      data withcnty no_cnty(drop=CntyCode);
         set &eff_file;
         if compress(CntyCode) = "" then output no_cnty;
         else output withcnty;
      run;

      *** Merge first the control without any geographic-level controls ***;
      proc contents data=no_cnty out=chkobs noprint;

      data _null_;
         set chkobs (obs=1);
         call symput('nocnty',nobs);
      run;
   %END;

   %if not &cntycodes or &nocnty %then %do;
      %IF &cntycodes %THEN %DO;
         proc sort data=no_cnty;
      %END;
      %ELSE %DO;
         proc sort data=&eff_file out=no_cnty;
      %END;
         by &mvar1 &mvar2 &mvar3;
      run;

      proc sort data=EMISSION;
         by &mvar1 &mvar2 &mvar3;
      run;

      %IF &LDBG %THEN %DO;
         title2 "MERGING SOURCE CATEGORY-LEVEL ONLY GROWTH AND CONTROL INFORMATION WITH EMISSIONS";
         title3 "Without County-level Controls";
         proc contents data=no_cnty; run;
         proc freq data=no_cnty;
            table &mvar1 &mvar2 &mvar3;
         run;
      %END;

      data EMISSION(drop=E_eff N_eff N_rate R_code)
           CNTLonly(keep=&mvar1 &mvar2 &mvar3 E_eff N_eff N_rate R_code);
         merge EMISSION(in=in1) no_cnty(in=in2);
         by &mvar1 &mvar2 &mvar3;
         if in1 and in2 then do;
            UserXEff = E_eff;
            UserNEff = N_eff;
            UserRate = N_rate;
            Replace = R_code;
            UserCntl = "&Code"||" for all counties";
         end;
         If in1 then output EMISSION;
         If in2 and not in1 then output CNTLonly;
      run;

      ***  CHECK FOR NON-MATCHES;
      title2 "Warning: Some non-county-specific controls not found in emissions file";
      title3 "&eff_file (first 5 records)";
      proc print data=CNTLonly(obs=5) split="*";
      run;

      proc datasets;
         delete CNTLonly no_cnty;
      quit;
   %end;
   %IF &cntycodes %THEN %DO;

      *** Merge second the control with geographic-level controls;
      proc contents data=withcnty out=chkobs noprint;

      data _null_;
         set chkobs (obs=1);
         call symput('w_cnty',nobs);
      run;

      %if &w_cnty %then %do;

         proc sort data=withcnty;
            by &mvar1 &mvar2 &mvar3 CntyCode;
         run;

         proc sort data=EMISSION;
            by &mvar1 &mvar2 &mvar3 CntyCode;
         run;

         %IF &LDBG %THEN %DO;
            title2 "MERGING SOURCE CATEGORY-LEVEL ONLY GROWTH AND CONTROL INFORMATION WITH EMISSIONS";
            title3 "With County-level Controls";
            proc contents data=withcnty; run;
            proc freq data=withcnty;
               table &mvar1 &mvar2 &mvar3 CntyCode;
            run;
         %END;

         data EMISSION(drop=E_eff N_eff N_rate R_code)
              CNTLonly(keep=&mvar1 &mvar2 &mvar3 E_eff N_eff N_rate CntyCode R_code);
            merge EMISSION(in=in1) withcnty(in=in2);
            by &mvar1 &mvar2 &mvar3 CntyCode;
            if in1 and in2 then do;
               UserXEff = E_eff;
               UserNEff = N_eff;
               UserRate = N_rate;
               Replace = R_code;
               UserCntl = "&Code"||" for county "||compress(CntyCode);
            end;
            If in1 then output EMISSION;
            If in2 and not in1 then output CNTLonly;
         run;

         ***  CHECK FOR NON-MATCHES;
         title2 "Warning: Some county-specific controls not found in emissions file";
         title3 "&eff_file (first 5 records)";
         proc print data=CNTLonly(obs=5) split="*";
         run;

         proc datasets;
            delete CNTLonly withcnty;
         quit;
      %end;
   %END;
%End;

proc datasets;
   delete chkobs;
quit;

%MEND MERGCNTL;


*=====================================================================================================*;
%MACRO GROW(resolution,inpfname);
*=====================================================================================================*;
***  PERFORMS GROWTH AND CONTROL CALCULATIONS TO PRODUCE A PROJECTED EMISSIONS INVENTORY;

*************************************;
***  READ Emission Inventory File ***;
*************************************;
*** MAY 2003:  SCC (when processing 1996 NTI, inventory AMS is renamed SCC and inventory SCC is renamed;
*** SCC_INV) variable is needed for SCC-level growth ***;
**********************************************************************************;
data EMISSION;
   %IF &&GFMACT&P ne or &&GFSIC&P ne or &&GFSCC&P ne %THEN %DO;
      length state $2. county $3. gfcode $7
   %END;;
   set &inpfname(keep=catcode MACT SIC SCC fips NTI_HAP src_type
                    %IF &resolution ne ctylvl %THEN %DO;
                        cell
                    %END;);
   %IF &&GFMACT&P ne or &&GFSIC&P ne or &&GFSCC&P ne %THEN %DO;
      state = substr(fips,1,2);
      county = substr(fips,3,3);
      gfcode = '';
   %END;
run;
****************************************************************************************;
*    ASSIGN GROWTH FACTORS TO EMISSION INVENTORY                                        ;
* If GFMACT, GFSIC, or GFSCC are not equal to NONE or left blank, assign growth factors ;
* FIRST, Merge in Growth Factors by MACT, if user has selected this                     ;
****************************************************************************************;

%IF &&GFMACT&P ne %THEN %DO;

***  READ GROWTH FACTOR BY STATE/county/MACT FILE  ***;
    data GrowMACT;
       infile "&inpfiles./&&GFMACT&P...txt" firstobs=3 stopover;
       input state $ 1-2 county $ 4-6 MACT $ 8-14 gf 16-24;
       MACT = left(MACT);
*** if MACT is only three digit, make four digit with preceding zero;
      if length(MACT) = 3 then MACT = '0' || substr(MACT,1,3);

   proc sort data=GrowMACT nodupkey;  * Warning: THERE SHOULD BE NO DUPLICATES IN THIS FILE;
      by state county MACT;

   data national(keep=MACT gf)
     state(keep=state MACT gf)
     county;
      set GrowMACT;
      if state = '00' then output national;
      else if county = '000' then output state;
      else output county;

*** Check MACT value in inventory emission file;

*** create state and county variable for merging growth factors;
   data EMISSION;
      set EMISSION;

      MACT = left(MACT);
*** if MACT is only three digit, make four digit with preceding zero;
      if length(MACT) = 3 then MACT = '0' || substr(MACT,1,3);

******************************************;
***  MERGE GFs INTO EMISSIONS BY MACT  ***;
******************************************;

*** First merge in national growth factors ***;
   proc sort data=EMISSION;
      by MACT;

   proc sort data=national;
      by MACT;

   data EMISSION;
      merge national (in=in1) EMISSION (in=in2) ;
      by MACT;
      if in2;
   run;

*** Second merge in state growth factors ***;
   proc sort data=EMISSION;
      by state MACT;

   proc sort data=state;
      by state MACT;

   data EMISSION(drop=gfst);
      merge STATE (in=in1 rename=(gf=gfst)) EMISSION (in=in2);
      by state MACT;
      if gfst ne . then gf = gfst;
      if in2;
   run;

*** Third merge in county growth factors ***;
   proc sort data=EMISSION;
      by state county MACT;

   proc sort data=county;
      by state county MACT;

   data EMISSION(drop=gfcty gf) MACTemis;
      merge county (in=in1 rename=(gf=gfcty)) EMISSION (in=in2) ;
      by state county MACT;
      if gfcty ne . then gf = gfcty;
      if in2;
      if gf = . then output EMISSION;
      else do;
         gfcode = 'MACT';
         output MACTemis;
      end;
   run;

   proc datasets;
      delete GrowMACT national state county;
   quit;
%END; /*** End assignment of growth factors by MACT ***/
%ELSE %DO;
   *** CREATE DUMMY DATASET OF MACT GROWTH ***;
   data MACTemis;
      set _null_;
   run;
%END;

*************************************************************************;
* IF selected, merge in Growth Factors by SIC (non-point inventory only) ;
*************************************************************************;
/* now use the 4 digit SIC code instead of 2 digit JAT 5/20/04 */
%IF &&GFSIC&P ne and &EMISTYPE eq AR %then %do;

   data GrowSIC;
      infile "&inpfiles./&&GFSIC&P...txt" firstobs=3 stopover;
      input state $ 1-2 county $ 4-6 sic $ 8-11 gf 13-21;

   proc sort data=GROWSIC nodupkey;  * Warning: THERE SHOULD BE NO DUPLICATES IN THIS FILE;
      by state county sic; **MAY2004;

   data national(keep=sic gf)
        state(keep=state sic gf)
        county;
      set GrowSIC;
      if state = '00' then output national;
      else if county = '000' then output state;
      else output county;

***  MERGE GFs INTO EMISSIONS BY SIC STATE  ***;
*** First merge in national growth factors ***;

   proc sort data=EMISSION;
      by sic;

   proc sort data=national;
      by sic;

   data EMISSION(drop=gfus);
      merge national (in=in1 rename=(gf=gfus)) EMISSION (in=in2);
      by sic;
      if gfus ne . then gf = gfus;
      if in2 ;
   run;

*** Second merge in state growth factors ***;
   proc sort data=EMISSION;
      by state sic;

   proc sort data=state;
      by state sic;

   data EMISSION(drop=gfst);
      merge state (in=in1 rename=(gf=gfst)) EMISSION (in=in2);
      by state sic;
      if gfst ne . then gf = gfst;
      if in2;
   run;

*** Third merge in county growth factors ***;
   proc sort data=EMISSION;
      by state county sic;

   proc sort data=county;
      by state county sic;

   data EMISSION(drop=gfcty gf) SICemis;
      merge county (in=in1 rename=(gf=gfcty)) EMISSION (in=in2);
      by state county sic;
      if gfcty ne . then gf = gfcty;
      if in2;
      if gf = . then output EMISSION;
      else do;
         gfcode = 'SIC';
         output SICemis;
      end;
   run;

*** Delete GrowSIC intermediate files  ***;
   proc datasets library=work;
      delete GrowSIC national state county;
   quit;
%END;
%ELSE %DO;
   *** CREATE DUMMY DATASET OF SIC GROWTH ***;
   data SICemis;
      set _null_;
   run;
%END; *** End assignment of growth factors by SIC;

%IF &&GFSCC&P ne %THEN %DO;

   data sccxref (keep=SCC remi) sccg(keep=state county gf remi);
      retain next;
      length remi $72 scc $10;
      infile "&inpfiles./&&GFSCC&P...txt" firstobs=3 lrecl=300 pad;
      if _n_ = 1 then next = 0;
      if next = 0 then do;
         input SCC $1-10 @;
         if substr(SCC,3,1) = '' then do;
            next=1;
         end;
         else do;
           input remi $12-83;
           output sccxref;
         end;
      end;
      if next = 1 then do;
         input state $1-2 county $4-6 gf remi $72.;
         output sccg;
      end;
   run;
   *** Save processing time by removing all SCCs in Growth Factor file NOT in inventory ***;
   proc sort data=EMISSION(keep=SCC) out=uniqSCC nodupkey;
      by SCC;
   run;
   data _null_;
      set uniqSCC;
      call symput('invSCC'||left(_n_),SCC);
      call symput('n_SCC',left(_n_));
   run;

   proc sql;
      create table GrowSCC
      as  select SCC, state, county, gf
      from sccg,sccxref
      where sccg.remi=sccxref.remi and SCC in(%do I = 1 %to &n_SCC; "&&invscc&I" %end;)
   ;
   quit;

   proc sort data=GROWSCC nodupkey;  * Warning: THERE SHOULD BE NO DUPLICATES IN THIS FILE;
      by state county scc;

   data national(keep=gf scc)
        state(keep=state gf scc)
        county;
      set GrowSCC;
      if state = '00' then output national;
      else if county = '000' then output state;
      else output county;

***  MERGE GFs INTO EMISSIONS BY SCC STATE  ***;
*** First merge in national growth factors ***;
   proc sort data=EMISSION;
      by scc;

   proc sort data=national;
      by scc;

   data EMISSION(drop=gfus);
      merge national (in=in1 rename=(gf=gfus)) EMISSION (in=in2);
      by scc;
      if gfus ne . then gf = gfus;
      if in2;
   run;

*** Second merge in state growth factors ***;
   proc sort data=EMISSION;
      by state scc;

   proc sort data=state;
      by state scc;

   data EMISSION(drop=gfst);
      merge state (in=in1 rename=(gf=gfst)) EMISSION (in=in2);
      by state scc;
      if gfst ne . then gf = gfst;
      if in2;
   run;

*** Third merge in county growth factors ***;
   proc sort data=EMISSION;
      by state county scc;

   proc sort data=county;
      by state county scc;

   data SCCemis(drop=gfcty);
      merge county (in=in1 rename=(gf=gfcty)) EMISSION (in=in2);
      by state county scc;
      if gfcty ne . then gf = gfcty;
      if in2 then do;
         gfcode = 'SCC';
         output;
      end;
   run;
   data EMISSION(keep=catcode src_type SCC SIC NTI_HAP FIPS MACT gf gfcode
                    %IF &resolution ne ctylvl %THEN %DO;
                        cell
                    %END;);
      set MACTemis SICemis SCCemis;
   run;

*** Delete GrowSCC intermediate files  ***;
   proc datasets library=work;
      delete SCCemis GrowSCC sccxref sccg uniqSCC national state county;
   Quit;

%END;
%ELSE %DO;
   *** CREATE FINAL DATASET CONTAINING ALL GROWTH RECORDS ***;
   data EMISSION(keep=catcode src_type SCC SIC NTI_HAP FIPS MACT gf gfcode
                    %IF &resolution ne ctylvl %THEN %DO;
                        cell
                    %END;);
      set MACTemis SICemis EMISSION; *where EMISSION contains the leftover records of
                                               missing GFs from MACT, and/or SIC growth *;
   run;
%END;

********************************************************;
proc datasets;
   delete MACTemis SICemis;
quit;

*** End of assigning Growth Factors from GF file(s) ***;

********************************************;
*******  PROCESS MACT CONTROLS FILE  *******;
*********************************************************************************;
* If MACTGEN is not blank or equal to NONE, General MACT controls are applied.  *;
* If SPECFILE is not blank or equal to NONE, Specific MACT controls are applied.*;
* If USERFILE is not blank or equal to NONE, User-defined controls are applied. *;
* NOTE: MACT-spec controls are applied only if MACT-gen controls are applied.   *;
*********************************************************************************;

%IF &&MACTGEN&P ne %THEN %DO;

******************************************;
***  READ NATIONAL MACT CONTROLS FILE  ***;
******************************************;
***  Modified 10/3/00 to use MACT_gen file used by point source processing ***;
***  Retain ALL RECORDS: Could be major-bin landfills or MOBILE ("O") reductions;

   data MACTCNTL;
      Infile "&INPFILES./&&MACTGEN&P...txt" firstobs=2 stopover;
      Input @1 MACT $7. @9 MACTXEff 6.2 @16 MACTNEff 6.2 @23 MACTrate 6.2
            @30 cdate mmddyy10.  @41 Apply $1. @43 MACT_src $1.;

***  Assume controls are not in place for entire year until year after compliance date ***;
         MACT = compress(MACT);

***  Keep only those records where Apply flag is set true (1) *;
***  Source type is considered, but not until ....... *;
         If Apply;
            keeprec = 1;

***  CHECK FOR MISSING VALUES IN THE MACT CONTROL FILE;
***  Drop records is information is missing;
         if MACT = "" or (cdate = .) or (MACTXEff = . and MACTNEff = .) then do;
            put " *** Error: one of MACT cdate MACTXEff MACTNEff is missing in the MACT controls file";
            put MACT= cdate= MACTXEff= MACTNEff=;
            put " This control will be dropped.";
            keeprec = 0;
         end;
         %if %upcase(&&yeartype&P) = CALENDAR %then %do;
            if cdate le mdy(12,31,&&growyear&P) then do;
               if cdate gt mdy(12,31,&&growyear&P -1) then do;
                  MACTXEff= MACTXEff * (mdy(12,31,&&growyear&P) - cdate) / (mdy(12,31,&&growyear&P) - mdy(12,31,&&growyear&P - 1));
                  MACTNEff= MACTNEff * (mdy(12,31,&&growyear&P) - cdate) / (mdy(12,31,&&growyear&P) - mdy(12,31,&&growyear&P - 1));
               end;
            end;
            else do; ***  Drop records that do not meet the projection year criteria;
                keeprec = 0;
            end;
         %end;
         %else %if %upcase(&&yeartype&P) = FISCAL %then %do;
            if cdate le mdy(9,30,&&growyear&P) then do;
               if cdate gt mdy(9,30,&&growyear&P -1) then do;
                  MACTXEff= MACTXEff * (mdy(9,30,&&growyear&P) - cdate) / (mdy(9,30,&&growyear&P) - mdy(9,30,&&growyear&P - 1));
                  MACTNEff= MACTNEff * (mdy(9,30,&&growyear&P) - cdate) / (mdy(9,30,&&growyear&P) - mdy(9,30,&&growyear&P - 1));
               end;
            end;
            else do; ***  Drop records that do not meet the projection year criteria;
                keeprec = 0;
            end;
         %end;
         %else %do;
             put "Error: **YEARTYPE** BATCH FILE VARIABLE MUST BE EQUAL TO FISCAL or CALENDAR";
             ABORT;
         %end;
      run;

      title2 "The Area Source MACT controls";
      title3 "(not applied if keeprec=0)";
      proc print data=MACTCNTL;
      run;
      title2;

      proc sort data=MACTCNTL(keep=MACT MACTXEff MACTNEff MACTRate MACT_src keeprec cdate);
         where keeprec = 1;
         by MACT;
      run;

********************************************************************;
*********  MERGE MACT CONTROL INFORMATION WITH EMISSIONS  **********;
********************************************************************;

      proc sort data=EMISSION;
         by MACT;
      run;

      data EMISSION
           MACTonly(keep=MACT MACTXEff MACTNEff MACTRate MACT_src);
         merge EMISSION(in=in1) MACTCNTL(in=in2);
         by MACT;
         length MACTCntl $60.;

         if in1 and in2 then MACTCntl = "General MACT";
         if in1 then output EMISSION;
         if in2 and not in1 then output MACTonly;
      run;

***  CHECK FOR NON-MATCHES;
      title2 "Warning: There are records in the MACT-based control file not matched to emission file";
      title3 "These controls cannot be applied (first 5 records) ";
      proc print data=MACTonly(obs=5) split="*";
      run;
      title2;

      proc datasets;
         delete MACTCNTL MACTonly;
      run;

*************************************************************************;
*** DYL - Add MACT and pollutant specific controls.  If SPECFILE exists *;
***  or os not equal to NONE then read SPECFILE file.  Use only those records  ***;
***    with a value for MACT and NTI_HAP and no value for SCC.  Apply ***;
***    to inventory by overwriting any control information assigned   ***;
***    by MACT only.                                                  ***;
*************************************************************************;

  %IF &&SPECFILE&P ne %THEN %DO;

*** File contains the SAROAD which is read, but not currently used ***;

       Data MACTspec;
          Infile "&INPFILES./&&SPECFILE&P...txt" firstobs=2 stopover;
          Input @1 MACT $7. @9  nti_hap $3. @13 saroad $5. @20 scc8 $8. @29 Scc6 $6.
                @37 EffXspec 6.2 @44 EffNspec 6.2 @51 snewrate 6.2 @58 Apply $1. @60 SApp_src $1.;

***  Keep only those records where Apply flag is set true (1);
***  Source type is considered, but not until .....;
***     and only MACT and NTI_HAP are specified;
         MACT = compress(MACT);
           if  Apply and
               (compress(MACT) ne "" and compress(NTI_HAP) ne "") and
               (compress(scc8) eq "" and compress(scc6) eq "") ;

               keeprec = 1;

***  Flag records for deletion if information is missing;
               if (EffXspec = . and EffNspec = . ) or (SApp_src = "") then do;
                  put " *** Error: ExistEff or New_Eff is missing in the MACT specific controls file";
                  put MACT= NTI_HAP= EffXspec= EffNspec= SApp_src=;
                  put " This control will be dropped.";
                  keeprec=0;
               end;
       run;

       proc sort data=MACTspec;
          by MACT NTI_HAP;

       data MACTspec dups;
          set MACTspec;
          by MACT NTI_HAP;
          if first.NTI_HAP then output MACTspec;
          else output dups;
       run;

       title2 "The Area Source MACT and Pollutant Specific controls";
       title3 "(not applied if keeprec=0)";
       proc print data=MACTspec;
       run;

       title2 "Warning: Duplicate records in MACT SPEC file when sorted by MACT and NTI_HAP";
       title3 ;
       proc print data=dups;
       run;
      title2;

       data MACTspec(drop=keeprec);
          set MACTspec(keep=MACT NTI_HAP EffXspec EffNspec snewrate SApp_src keeprec);
          where keeprec = 1;
       run;

       proc sort data=EMISSION;
          by MACT NTI_HAP;

       data EMISSION(drop=EffXspec EffNspec snewrate SApp_src keeprec)
            SPEConly(keep=MACT NTI_HAP EffXspec EffNspec snewrate SApp_src);
          merge EMISSION(in=A) MACTspec(in=B);
          By MACT NTI_HAP;
          If keeprec and A and B then do;

      %if %upcase(&&yeartype&P) = CALENDAR %then %do;
         if cdate gt mdy(12,31,&&growyear&P -1) then do;
            MACTXEff= EffXspec * (mdy(12,31,&&growyear&P) - cdate) / (mdy(12,31,&&growyear&P) - mdy(12,31,&&growyear&P - 1));
            MACTNEff= EffNspec * (mdy(12,31,&&growyear&P) - cdate) / (mdy(12,31,&&growyear&P) - mdy(12,31,&&growyear&P - 1));
         end;
         else MACTXEff= EffXspec; ** Compliance date is before beginning of projection year;
      %end;
      %else %if %upcase(&&yeartype&P) = FISCAL %then %do;
         if cdate gt mdy(9,30,&&growyear&P -1) then do;
            MACTXEff= EffXspec * (mdy(9,30,&&growyear&P) - cdate) / (mdy(9,30,&&growyear&P) - mdy(9,30,&&growyear&P - 1));
            MACTNEff= EffNspec * (mdy(9,30,&&growyear&P) - cdate) / (mdy(9,30,&&growyear&P) - mdy(9,30,&&growyear&P - 1));
         end;
         else MACTXEff= EffXspec; ** Compliance date is before beginning of projection year;
      %end;

             MACTRate = snewrate;
             MACT_src = SApp_src;
             MACTCntl = "MACT/HAP MACT";
          End;

          if A then output EMISSION;
          if B and not A then output SPEConly;
       run;

***  CHECK FOR NON-MATCHES;
       title2 "Warning: There are records in specific MACT-based control file not matched to emission file";
       title3 "These controls cannot be applied (first 5 records) ";
       proc print data=SPEConly(obs=5) split="*";
       run;
      title2;

       proc datasets;
          delete MACTspec SPEConly dups;
       run;

   %END; *** -end of applying specific MACT controls ***;


%END; ***  -end of applying MACT controls ***;

**************************************************;
*******  PROCESS USER-DEFINED CONTROLS  **********;
**************************************************;

%IF &&USERFILE&P ne %THEN %DO;

***  READ NATIONAL SOURCE CATEGORY CONTROLS FILE (by Cat_Name);
      data USERCNTL(drop=Apply);
         infile "&INPFILES./&&USERFILE&P...txt" lrecl=141 firstobs=11 stopover;
      /*** FOR NON-POINT NATA, USE CAT_NAME MATCH ***/
      %if &amsvar %then %do;
         input @1 Cat_Name $90. @92 MACT $7. @100 NTI_HAP $3. @104 E_eff 6.2 @111 N_Eff 6.2
               @118 N_Rate 6.2 @125 CntyCode $5. @131 R_Code $1. @133 Apply $1. ;

         Cat_Name = UPCASE(Cat_Name);
      %end;
      %else %do;
         length catcode $10; ***AUG2003;
         input @1 Cat_Name $10. @12 MACT $7. @20 NTI_HAP $3. @24 E_eff 6.2 @31 N_Eff 6.2
               @38 N_Rate 6.2 @45 CntyCode $5. @51 R_Code $1. @53 Apply $1. ;
         catcode = CAT_NAME; *** cat_name is actually SCC -AUG2003;
      %end;
         MACT = compress(MACT);
         CntyCode = COMPRESS(CntyCode);
         NTI_HAP = COMPRESS(NTI_HAP);

         label
            Cat_Name = "Source*Category"
            MACT = "MACT*Category*Code"
            NTI_HAP = "NTI*Pollutant*Code"
            E_EFF = "Existing*control*efficiency"
            N_Eff  = "New*control*efficiency"
            N_Rate  = "Percent*New*Emissions"
            CntyCode = "County*Control*Code"
            R_code = "MACT*Replacment*Code";

***  Only keep records that are to be applied for this control strategy, Apply is 1(true) - DYL***;

         if Apply;
            keeprec = 1;

***  Flag records for deletion if information is missing;
            if (compress(Cat_Name) eq "" and compress(MACT) eq "") or
               (E_Eff = . and N_Eff = . ) then do;
               put " *** Error: Cat_Name, MACT, E_Eff or N_Eff is missing in the User controls file";
               put Cat_Name= MACT= NTI_HAP= E_eff= N_Eff=;
               put " This control will be dropped.";
               keeprec=0;
            end;
      run;
      proc sort data=USERCNTL out=USERCNTL(drop=keeprec) nodupkey;  * Warning: THERE SHOULD BE NO DUPLICATES IN THIS FILE;
         by Cat_Name MACT NTI_HAP CntyCode;
         where keeprec = 1;
      run;

******************* CATCODE NEEDED ONLY FOR 1996 NTI **********************************************;
*** Recreate the EMISBINS FILE for 1996 NTI (MAY NOT EXIST YET) ***; *AUG2003;
      %if &amsvar %then %do;  ***AUG2003;
         /***  READ THE CAT_NAME/CATCODE XREF FILE (THE EMISSIONS GROUPS DEFINITION FILE) */
         data emisbins;                                         * am_grp_nata.txt;
            length Cat_Name $90 CatCode $4;
            infile "&INPFILES./&EMISBINS..txt" firstobs=8 missover;
            input Cat_Name $ 1-90 CatCode $ 91-94;
            Cat_Name = UPCASE(Cat_Name);
         run;
         proc sort data=emisbins nodupkey;  * Warning: THERE SHOULD BE NO DUPLICATES IN THIS FILE;
            by Cat_Name;
         run;

***  MERGE CATCODE INTO THE CONTROL FILE;
         data USERCNTL in1only in2only;
            merge USERCNTL(in=in1) emisbins(in=in2);
            by Cat_Name;
            if in1 then output USERCNTL;
            else if in1 and not in2 then output in1only;
            else if in2 and not in1 then output in2only;
         run;
      %end; ***AUG2003;
******************* CATCODE NEEDED ONLY FOR 1996 NTI **********************************************;

***   NOW WE HAVE ALL SOURCE CATEGORY CONTROL INFORMATION BY CATCODE  ***;
      title2 "The User-Defined Source Category Control Information";
      proc print data=USERCNTL split="*";
         var Cat_Name CatCode MACT NTI_HAP E_Eff N_Eff N_Rate CntyCode R_code;
      run;
      title2;

*** Split the control file into five parts, depending on the level of control information ***;

   %let cntycodes = 0; **FEB 2003: Do not make popflag file a requirement if no cntycodes exist in user file;
 
      data MACTONLY(keep=MACT E_Eff N_Eff N_Rate CntyCode R_Code)
           CATNAME(keep=CatCode E_Eff N_Eff N_Rate CntyCode R_Code)
           MACT_CAT(keep=MACT CatCode E_Eff N_Eff N_Rate CntyCode R_Code)
           MACT_HAP(keep=MACT NTI_HAP E_Eff N_Eff N_Rate CntyCode R_Code)
           CAT_HAP(keep=CatCode NTI_HAP E_Eff N_Eff N_Rate CntyCode R_Code)
           MCAT_HAP(keep=MACT CatCode NTI_HAP E_Eff N_Eff N_Rate CntyCode R_Code)
           ;
         set USERCNTL;
         If compress(MACT) ne "" then do;
            If compress(NTI_HAP) ne "" then do;
               IF compress(CatCode) ne "" then output MCAT_HAP; *1-2;
               Else output MACT_HAP; *5-6;
            end;
            Else do;
               IF compress(CatCode) ne "" then output MACT_CAT; *7-8;
               Else output MACTonly; *11-12;
            end;
         end;
         Else do;
            If compress(CatCode) ne "" then do;
               If compress(NTI_HAP) = "" then output CATNAME; *9-10;
               Else output CAT_HAP; *3-4;
            end;
            Else do;
               put " *** Error: both the Category Name and MACT code are missing in the User controls file";
               put NTI_HAP= E_Eff= N_Eff= N_Rate CntyCode= R_Code=;
               put " This control cannot be assigned and will be dropped.";
            end;
         end;
         drop Cat_Name;
         if cntycode ne '' then call symput('cntycodes',1);
      run;

***  Add a county control code to the emissions file IF USER-DEFINED FILE CONTAINS THEM -FEB 2003**;
***   - county control code is five characters to accommodate future geographic-level controls ***;

   %IF &cntycodes %THEN %DO;
***  READ THE COUNTY URBAN/RURAL FLAG FILE;
      data CNTYFILE;
         infile "&INPFILES./&&CNTYUR&P...txt" stopover firstobs=3;
         input fips $ 1-5 CntyCode $ 56-60;
         length cntycode $5.;
         CntyCode = COMPRESS(CntyCode);
      run;

***  MERGE COUNTY CONTROL CODE INTO EMISSIONS;
      proc sort data=EMISSION;
         by fips;
      run;

      proc sort data=CNTYFILE;
         by fips;
      run;

      data EMISSION nomatch;
         merge EMISSION(in=in1) CNTYFILE(in=in2);
         by fips;
         length UserCntl $60.;
         UserCntl = "";
         if in1 and not in2 then do;
            output nomatch;
         end;
         if in1 then output EMISSION;
      run;

***  PRINT Warning MESSAGES IF THERE ARE NON-MATCHES;
      title2 "*** Warning *** THERE ARE NON-MATCHED COUNTY CONTROL CODES ***";
      proc print data=nomatch(obs=30) split="*"; run;
      proc freq data=nomatch;
         table fips;
      run;
      title2;
   %END;

*******************************************************************************;
*********  MERGE SOURCE CATEGORY CONTROL INFORMATION WITH EMISSIONS  **********;
*******************************************************************************;

      %MERGCNTL(eff_file=MACTONLY,mvar1=,mvar2=,mvar3=MACT,code=User MACT Only)
      %MERGCNTL(eff_file=CATNAME,mvar1=,mvar2=,mvar3=CatCode,code=Category Only)
      %MERGCNTL(eff_file=MACT_CAT,mvar1=,mvar2=MACT,mvar3=CatCode,code=User MACT/Category)
      %MERGCNTL(eff_file=MACT_HAP,mvar1=,mvar2=MACT,mvar3=NTI_HAP,code=User MACT/HAP)
      %MERGCNTL(eff_file=CAT_HAP,mvar1=,mvar2=CatCode,mvar3=NTI_HAP,code=Category/HAP)
      %MERGCNTL(eff_file=MCAT_HAP,mvar1=MACT,mvar2=CatCode,mvar3=NTI_HAP,code=MACT/Category/HAP)

*** Delete temporary files;

      proc datasets;
         delete emisbins cntyfile in1only in2only mactonly catname mact_cat cat_hap mact_hap mcat_hap
                nomatch usercntl;
      run;

   %END;

***  DYL - end of Applying user-defined controls by source category ***;

********************************************************************************************;
***  RESOLVE APPLICATION OF CONTROL EFFICIENCIES BETWEEN MACT CONTROLS AND USER CONTROLS ***;
********************************************************************************************;

   %If &&MACTGEN&P ne and &&USERFILE&P eq %then %do;
      data PROJEMIS;
         set EMISSION;
         length CntlCode $60.;
*** Allow major bin controls -11/26/01 ***;
*** Allow bins to be ANY NUMBER!! Use NEW src_type VARIABLE  -02/21/02 ***;
         If (src_type = 'M' and upcase(MACT_src) in('M','B'))
            or (src_type = 'A' and upcase(MACT_src) in('A','B')) then do;
            ExistEff = MACTXEff;
            New_Eff = MACTNEff;
            New_Rate = MACTrate;
            AddXEff = 0;
            AddNEff = 0;
            Add_Rate = 0;
            Replace = '';   **added 09/28/01 *;
            CntyCode = '';   **added 09/28/01 *;
            CntlCode = MACTCntl;
         end;

         Else do;
            If compress(MACTcntl) ne "" then CntlCode = 'MACT not applied due to source type';
         end;

         drop MACTXEff MACTNEff MACTrate MACTCntl MACT_src;
      run;
   %END;

   %If &&MACTGEN&P eq and &&USERFILE&P ne %then %do;
      data PROJEMIS;
         set EMISSION;
         length CntlCode $60.;
         ExistEff = UserXEff;
         New_Eff = UserNEff;
         New_Rate = UserRate;
         AddXEff = 0;
         AddNEff = 0;
         Add_Rate = 0;
         CntlCode = UserCntl;
         drop UserXEff UserNEff UserRate UserCntl;
      run;
   %END;

   %If &&MACTGEN&P ne and &&USERFILE&P ne %then %do;
      data PROJEMIS;
         set EMISSION;
         length CntlCode $60.;
*** Allow major bin controls -11/26/01 ***;
*** Allow bins to be ANY NUMBER!! Use NEW src_type VARIABLE  -02/21/02 ***;
         If (src_type = 'M' and upcase(MACT_src) in('M','B'))
            or (src_type = 'A' and upcase(MACT_src) in('A','B')) then do;
            ExistEff = MACTXEff;
            New_Eff = MACTNEff;
            New_Rate = MACTrate;
            CntlCode = MACTCntl;
         end;
         Else do;
            If compress(MACTcntl) ne "" then CntlCode = 'MACT not applied due to source type';
         end;

         If compress(Replace) = "" then do;
            ExistEff = MACTXEff;
            New_Eff = MACTNEff;
            New_Rate = MACTrate;
            AddXEff = 0;
            AddNEff = 0;
            Add_Rate = 0;
            If compress(MACTCntl) = "" then CntlCode = "";
            Else CntlCode = trim(MACTCntl)||"- no User control";
         end;
         Else do;
            If (compress(upcase(Replace)) = "R") then do;
               ExistEff = UserXEff;
               New_Eff = UserNEff;
               New_Rate = UserRate;
               AddXEff = 0;
               AddNEff = 0;
               Add_Rate = 0;
               If compress(MACTCntl) = "" then
                CntlCode = trim(UserCntl)||"- no MACT control";
               Else CntlCode = trim(UserCntl)||" replaced "||trim(MACTCntl);
            end;
            Else do;
***  Replace must be equal to A and MACT control have been assigned ***;
               ExistEff = MACTXEff;
               New_Eff = MACTNEff;
               New_Rate = MACTrate;
               AddXEff = UserXEff;
               AddNEff = UserNEff;
               Add_Rate = UserRate;
               If compress(MACTCntl) = "" then
                CntlCode = trim(UserCntl)||"- no MACT control";
               Else CntlCode = trim(MACTCntl)||" with added "||trim(UserCntl);
            end;
         end;
         drop UserXEff UserNEff UserRate UserCntl MACTXEff MACTNEff MACTrate MACTCntl MACT_src;
      run;
   %END;

   %If &&MACTGEN&P eq and &&USERFILE&P eq %then %do;
      data PROJEMIS;
         set EMISSION;
         length CntlCode $60.;
         length Replace $1.;
         length CntyCode $5.;
         ExistEff = 0;
         New_Eff = 0;
         New_Rate = 0;
         AddXEff = 0;
         AddNEff = 0;
         Add_Rate = 0;
         Replace = '';
         CntyCode = '';
         CntlCode = 'Growth Only';
      run;
   %END;


*******************************************************************************;

************************************************;
***  DO THE GROWTH AND CONTROL CALCULATIONS  ***;
***     Merge control information into inventory - DYL 5/17/01 ***;
***  FEB 2002 -Use src_type instead of emisbin or group *;
***  APR 2002 -add MACT and SIC variables to ensure proper merge *;
************************************************;

*** Check of existence of Growth factor and control data. ***;
***  If data present, set appropriate flag to true.       ***;

   proc contents data=projemis out=checkvar noprint;

   data _null_;
      set checkvar end=endchk;
      if _N_ = 1 then do;
         CNTLset=0; GFset=0;
      end;
      Retain CNTLset GFset;
      if trim(lowcase(name)) = 'existeff' then CNTLset = 1;
      if trim(lowcase(name)) = 'gf' then GFset = 1;
      if endchk then do;
          call symput('CNTLset',trim(CNTLset));
          call symput('GFset',trim(GFset));
      end;
   run;

   data projemis;
      set projemis;
   *** Create and assign appropriate variables when controls have not been ***;
   ***   assigned by the user.  If growth factor is not present create     ***;
   ***   variable and set to 1.                                            ***;
         %If not(&CNTLset) %then %do;
            ExistEff = 0;
            New_Eff = 0;
            New_Rate = 0;
            AddXEff = 0;
            AddNEff = 0;
            Add_Rate = 0;
            Replace = "";
            CntlCode = "";
            CntyCode = "";
         %end;
         %If not(&GFset) %then %do;
            GF = 1;
         %end;
   *** Check for missing values for growth factor and control variables ***;
         if GF = . then GF = 1;
         if ExistEff = . then ExistEff = 0;
         if New_Eff = . then New_Eff = 0;
         if New_Rate = . then New_Rate = 0;
         if AddXEff = . then AddXEff = 0;
         if AddNEff = . then AddNEff = 0;
         if Add_Rate = . then Add_Rate = 0;
   run;

%IF &&GCFLAG&P %THEN %DO;
   proc sort data=projemis(drop=fips MACT SIC %if not &amsvar %then %do; SCC %end;) nodupkey;
      by src_type cell catcode nti_hap gf ExistEff;

   proc sort data=&inpfname;
      by src_type cell catcode nti_hap;

   data badprojemis;
      set projemis;
      by src_type cell catcode nti_hap;
      if first.nti_hap ne 1;
   run;
   proc print data=badprojemis;
      title2 'Warning: Duplicated Growth-Control records by src_type, cell, catcode, nti_hap, gf, ExistEff';
   run;
   title2;
   data outfiles.&OUTNAME8;
	  merge &inpfname(in=A) projemis(in=B);
	  by src_type cell catcode nti_hap;
	  if A;
   run;
%END;
%ELSE %DO;
   proc sort data=projemis(drop=MACT SIC %if not &amsvar %then %do; SCC %end;) nodupkey;
      by src_type fips catcode nti_hap gf ExistEff;

   proc sort data=&inpfname;
      by src_type fips catcode nti_hap;

   data badprojemis;
      set projemis;
      by src_type fips catcode nti_hap;
      if first.nti_hap ne 1;
   run;
   proc print data=badprojemis;
      title2 'Warning: Duplicated Growth-Control records by src_type, cell, catcode, nti_hap, gf, ExistEff';
   run;
   title2;
   data outfiles.&OUTNAME8;
	  merge &inpfname(in=A) projemis(in=B);
	  by src_type fips catcode nti_hap;
	  if A;
   run;
%END;


***  GROWTH CALCULATIONS  ***;
********************* NO ARRAYS WHEN GCFLAG = 0 ******************;
%IF &&GCFLAG&P NE 1 %THEN %DO;
      data outfiles.&OUTNAME8;
         set outfiles.&OUTNAME8;

            GrowEmis = Emis * GF;

   *DYL - assume no control in place in area source inventory *;
            Base_Eff = 0;

   ***  CONTROL CALCULATIONS  ***;
   *DYL - If controls have not been applied, then grow emissions only *;

           %If &&MACTGEN&P eq and &&USERFILE&P eq %then %do;
               EmisGC = GrowEmis;
            %end;

   /*DYL - Else apply controls to the grown emissions */
            %Else %do;

   *DYL - Determine if controls should be applied over baseline controls ***;
   *DYL - Apply primary control efficiency   ***;
              if ExistEff > base_eff then do;
                 ProjExst = GrowEmis * (1-New_Rate/100) * (1 - ExistEff/100)/(1 - base_eff/100);
              end;
              else
                 ProjExst = GrowEmis * (1-New_Rate/100);

   *DYL - Repeat process for emissions from new sources ***;
              if New_Eff > base_eff then do;
                  ProjNew = GrowEmis * (New_Rate/100) * (1 - New_Eff/100)/(1 - base_eff/100);
              end;
              else
                  ProjNew = GrowEmis * (New_Rate/100);
   *DYL - Add together the projected emissions from existing and new sources ;
              ProjEmis = ProjNew + ProjExst;

   *DYL - Apply added controls to projected emissions ***;
   **** FEB 2002 -User Controls are NOT SUBJECT TO BASELINE REDUCTIONS;
   *              (really only point-source inventory anyways) **;
              ProjExst = ProjEmis * (1-Add_Rate/100) * (1 - AddXEff/100);

   *DYL - Repeat process for emissions from new sources ***;

               ProjNew = ProjEmis * (Add_Rate/100) * (1 - AddNEff/100);

   *DYL - Add projected emissions from existing sources and projected emissions from new sources;

              EmisGC = ProjNew + ProjExst;

            %End;
         **Drop GrowEmis base_eff ProjEmis ProjNew ProjExst;
         keep fips saroad nti_hap group scc mact sic emis emisGC catcode src_type CntlCode ExistEff 
              New_Eff New_Rate AddXEff AddNEff Add_Rate Replace CntyCode gf gfcode cdate /*cdate added JULY 2003 */
              %if &amsvar %then %do; cat_name scc_inv %end;;
      run;

   %IF (%upcase("&MODEL") eq "ISC") %THEN %DO;
       proc datasets library=outfiles;
          change &OUTNAME8=X&OUTNAME8;
       quit;
   %END;
   %IF (%upcase("&MODEL") eq "ISCTRACT") %THEN %DO;
       proc datasets library=outfiles;
          change &OUTNAME8=X&OUTNAME8;
       quit;
   %END;


   ************************************************************************;
%END;
%ELSE %DO;
      data outfiles.&OUTNAME8;
         set outfiles.&OUTNAME8;

         array tEmisA{&numtaf} temis1-temis&numtaf;

   *** Calculate temporally allocated projected emissions ***;

         Do i = 1 to &numtaf;
            GrowEmis = tEmisA(i) * GF;

   *DYL - assume no control in place in area source inventory *;
            Base_Eff = 0;

   ***  CONTROL CALCULATIONS  ***;
   *DYL - If controls have not been applied, then grow emissions only *;

           %If &&MACTGEN&P eq and &&USERFILE&P eq %then %do;
               tEmisA(i) = GrowEmis;
            %end;

   /*DYL - Else apply controls to the grown emissions */
            %Else %do;

   *DYL - Determine if controls should be applied over baseline controls ***;
   *DYL - Apply primary control efficiency   ***;
              if ExistEff > base_eff then do;
                 ProjExst = GrowEmis * (1-New_Rate/100) * (1 - ExistEff/100)/(1 - base_eff/100);
              end;
              else
                 ProjExst = GrowEmis * (1-New_Rate/100);

   *DYL - Repeat process for emissions from new sources ***;
              if New_Eff > base_eff then do;
                  ProjNew = GrowEmis * (New_Rate/100) * (1 - New_Eff/100)/(1 - base_eff/100);
              end;
              else
                  ProjNew = GrowEmis * (New_Rate/100);
   *DYL - Add together the projected emissions from existing and new sources ;
              ProjEmis = ProjNew + ProjExst;

   *DYL - Apply added controls to projected emissions ***;
   **** FEB 2002 -User Controls are NOT SUBJECT TO BASELINE REDUCTIONS;
   *              (really only point-source inventory anyways) **;
              ProjExst = ProjEmis * (1-Add_Rate/100) * (1 - AddXEff/100);

   *DYL - Repeat process for emissions from new sources ***;

               ProjNew = ProjEmis * (Add_Rate/100) * (1 - AddNEff/100);

   *DYL - Add projected emissions from existing sources and projected emissions from new sources;

              tEmisA(i) = ProjNew + ProjExst;

            %End;

    *** End loop over temporally allocated emissions values ***;

         End;

         Drop i GrowEmis base_eff ProjEmis ProjNew ProjExst;

      run;

   ************************************************************************;
   ***                GROWTH AND CONTROL SUMMARY TABLES                 ***;
   ************************************************************************;
   %IF (%upcase("&MODEL") = "ASPEN") %THEN %DO;
      data CntlSum;
         set outfiles.&OUTNAME8;
         gemis2 = emis * tf3hr2 * 0.0287925 * GF;
         gemis4 = emis * tf3hr4 * 0.0287925 * GF;
         gemis6 = emis * tf3hr6 * 0.0287925 * GF;
         keep CatCode MACT NTI_HAP CntyCode Replace
              ExistEff New_Eff New_Rate AddXEff AddNEff Add_Rate CntlCode
              gemis2 gemis4 gemis6 temis2 temis4 temis6;
         label
            gemis2   = "Grown*Emissions*Hrs 3-5"
            gemis4   = "Grown*Emissions*Hrs 9-11"
            gemis6   = "Grown*Emissions*Hrs 15-17"
         ;

      proc sort data=CntlSum;
         by CatCode MACT NTI_HAP CntyCode Replace;

      proc means sum data=CntlSum noprint;
         by CatCode MACT NTI_HAP CntyCode Replace;
         var gemis2 gemis4 gemis6
             temis2 temis4 temis6;
         id ExistEff New_Eff New_Rate AddXEff AddNEff Add_Rate CntlCode;
         output out=CntlSum(drop=_freq_ _type_)
         sum = gemis2 gemis4 gemis6
               temis2 temis4 temis6;
      run;

      data CntlSum;
         set CntlSum;
         if compress(CntlCode) = "" then delete;

      proc print data=CntlSum noobs split='*';
         var CatCode MACT NTI_HAP CntyCode Replace
             ExistEff New_Eff New_Rate AddXEff AddNEff Add_Rate CntlCode
             gemis2 gemis4 gemis6 temis2 temis4 temis6;
        title2 'Summary of Assigned Controls and Projected Emissions';
      run;
      title2;

   %END;
   %ELSE %DO;

      data CntlSum;
         set outfiles.&OUTNAME8;
         gemis90 = emis * tafs90 * 0.0287925 * GF;
         gemis180 = emis * tafs180 * 0.0287925 * GF;
         gemis230 = emis * tafs230 * 0.0287925 * GF;
         keep CatCode MACT NTI_HAP CntyCode Replace
              ExistEff New_Eff New_Rate AddXEff AddNEff Add_Rate CntlCode
              gemis90 gemis180 gemis230 temis90 temis180 temis230;
         label
            gemis90   = "Grown*Emissions*Winter*Weekday*Hr 19"
            gemis180  = "Grown*Emissions*Fall*Saturday*Hr 11"
            gemis230  = "Grown*Emissions*Spring*Sunday*Hr 14"
         ;

      proc sort data=CntlSum;
         by CatCode MACT NTI_HAP CntyCode Replace;

      proc means sum data=CntlSum noprint;
         by CatCode MACT NTI_HAP CntyCode Replace;
         var gemis90 gemis180 gemis230
             temis90 temis180 temis230;
         id ExistEff New_Eff New_Rate AddXEff AddNEff Add_Rate CntlCode;
         output out=CntlSum(drop=_freq_ _type_)
         sum = gemis90 gemis180 gemis230
               temis90 temis180 temis230;
      run;

      data CntlSum;
         set CntlSum;
         if compress(CntlCode) = "" then delete;

      proc print data=CntlSum noobs split='*';
         var CatCode MACT NTI_HAP CntyCode Replace
             ExistEff New_Eff New_Rate AddXEff AddNEff Add_Rate CntlCode
             gemis90 gemis180 gemis230 temis90 temis180 temis230;
        title2 'Summary of Assigned Controls and Projected Emissions';
      run;
      title2;

   %END;

   proc datasets;
      delete CntlSum;
   run;

   ***  SET LABELS FOR PRINTING;
      data outfiles.&OUTNAME8;
         set outfiles.&OUTNAME8; *keep JULY 2003(drop=CntyCode);
         label
      emis     = "Base Emis (tpy)"
      GF       = "Growth*factor"
      CatCode  = "Category*code"
      MACT     = "MACT*code"
      NTI_HAP  = "NTI HAP*Code"
      Replace  = "User*Replacement*Code"
      ExistEff = "Existing*control*efficiency"
      New_Eff  = "New*control*efficiency"
      New_Rate = "Percent*emissions*from*new*sources"
      AddXEff  = "Added*Existing*control*efficiency"
      AddNEff  = "Added*New*control*efficiency"
      Add_Rate = "Added*Percent*emissions*from new*sources"
      CntlCode = "Control*Code"
      ;
   %IF (%upcase("&MODEL") eq "ISC") %THEN %DO;
      label temis1="GC Emissions*Hr1 Winter Weekday [t/hr]"
            temis25="GC Emissions*Hr1 Spring Weekday [t/hr]"
            temis49="GC Emissions*Hr1 Summer Weekday [t/hr]"
            temis73="GC Emissions*Hr1 Autumn Weekday [t/hr]"
            temis97="GC Emissions*Hr1 Winter Saturday [t/hr]"
            temis121="GC Emissions*Hr1 Spring Saturday [t/hr]"
            temis145="GC Emissions*Hr1 Summer Saturday [t/hr]"
            temis169="GC Emissions*Hr1 Autumn Saturday [t/hr]"
            temis193="GC Emissions*Hr1 Winter Sunday [t/hr]"
            temis217="GC Emissions*Hr1 Spring Sunday [t/hr]"
            temis241="GC Emissions*Hr1 Summer Sunday [t/hr]"
            temis265="GC Emissions*Hr1 Autumn Sunday [t/hr]"
            emis = "Base Emis [tpy]";
   %END;
   %IF (%upcase("&MODEL") eq "ISCTRACT") %THEN %DO;
      label temis1="GC Emissions*Hr1 Winter Weekday [t/hr]"
            temis25="GC Emissions*Hr1 Spring Weekday [t/hr]"
            temis49="GC Emissions*Hr1 Summer Weekday [t/hr]"
            temis73="GC Emissions*Hr1 Autumn Weekday [t/hr]"
            temis97="GC Emissions*Hr1 Winter Saturday [t/hr]"
            temis121="GC Emissions*Hr1 Spring Saturday [t/hr]"
            temis145="GC Emissions*Hr1 Summer Saturday [t/hr]"
            temis169="GC Emissions*Hr1 Autumn Saturday [t/hr]"
            temis193="GC Emissions*Hr1 Winter Sunday [t/hr]"
            temis217="GC Emissions*Hr1 Spring Sunday [t/hr]"
            temis241="GC Emissions*Hr1 Summer Sunday [t/hr]"
            temis265="GC Emissions*Hr1 Autumn Sunday [t/hr]"
            emis = "Base Emis [tpy]";
   %END;
   %IF (%upcase("&MODEL") = "ASPEN") %THEN %DO;
      label temis1="GC Emissions [tpy]*Hrs 0-2"    temis2="GC Emissions [tpy]*Hrs 3-5"
            temis3="GC Emissions [tpy]*Hrs 6-8"    temis4="GC Emissions [tpy]*Hrs 9-11"
            temis5="GC Emissions [tpy]*Hrs 12-14"  temis6="GC Emissions [tpy]*Hrs 15-17"
            temis7="GC Emissions [tpy]*Hrs 18-20"  temis8="GC Emissions [tpy]*Hrs 21-23"
            emis = "Base Emis [tpy]";
   %END;
      run;
%END;


proc datasets;
   delete checkvar;
run;

options ls=92 ps=54;

%EXIT: %MEND GROW;


* ++++++++++++++++++ END OF PROGRAM MACROS  ++++++++++++++++++;


* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
* ++++++++++++++++++++++  MAIN PROGRAM  ++++++++++++++++++++++;
* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
%MACRO MAIN;

data runtimes; TSTART=time(); put TSTART=time.; run;

***  EXECUTING MACRO COF;

%COF

* ----------------------------------------- *;

***  EXECUTING MACRO READEMIS;
%READEMIS;
data runtimes; set runtimes; READEMIS=time(); put READEMIS=time.; run;

***  EXECUTING MACRO SPECREAC;
%SPECREAC;
data runtimes; set runtimes; SPECREAC=time(); put SPECREAC=time.; run;

***  EXECUTING MACRO MERGEBIN;
%MERGEBIN;
data runtimes; set runtimes; MERGEBIN=time(); put MERGEBIN=time.; run;

%IF &PROJECT ne and &PROJECT ne NONE %THEN %DO;
   /* Initialize: base, pmod pcty */
   %let base=0; %let pmod=0; %let pcty=0;
   data projinfo;
      length GFMACT $32 GFSIC $32 GFSCC $32 MACTGEN $32 SPECFILE $32 USERFILE $32;
      length CNTYUR $32 YEARTYPE $8 PNAME $16 Comments $130;
      infile "&INPFILES./&PROJECT..csv" firstobs=4 dlm=',' dsd pad lrecl=400;
      input GCFLAG GFMACT $ GFSIC $ GFSCC $ MACTGEN $ SPECFILE $ @;
      input USERFILE $ CNTYUR $ GROWYEAR YEARTYPE $ PNAME $ Comments $;

      yeartype = upcase(yeartype);
      if upcase(GFMACT)    eq "NONE" then GFMACT   = "";
      if upcase(GFSIC)     eq "NONE" then GFSIC    = "";
      if upcase(GFSCC)     eq "NONE" then GFSCC    = "";
      if upcase(MACTGEN)   eq "NONE" then MACTGEN  = "";
      if upcase(SPECFILE)  eq "NONE" then SPECFILE = "";
      if upcase(USERFILE)  eq "NONE" then USERFILE = "";

      if  GCFLAG  = . then  GCFLAG  = 0;
  ** Base-year emissions processing is indicated by blanks in filenames for Growth/controls **; 
      if GFMACT = "" and GFSIC = "" and GFSCC = "" and MACTGEN = "" and USERFILE = "" then do;
         pp=1;
         call symput('base',1); *..........................................should be only 1 observation -if any!! *;
      end;
      else do;
         if GCFLAG then do; /*Projections for models -NOT County-level projections only*/
            pp=3;
            call symput('pmod',1);
         end;
         else do; /*County-level projections only*/
            pp=2;
            call symput('pcty',1);
         end;
      end;
      call symput('numproj',_n_);
   run;
   /** Sort projections so Base are first, then county-level, then model-level */
   proc sort data=projinfo; by pp; run; 
   
   data _null_;
      set projinfo;
      call symput('numproj',_n_);
      call symput('gfmact'||left(_n_),trim(GFMACT));
      call symput('gfsic'||left(_n_),trim(GFSIC));
      call symput('gfscc'||left(_n_),trim(GFSCC));
      call symput('mactgen'||left(_n_),trim(MACTGEN));
      call symput('specfile'||left(_n_),trim(SPECFILE));
      call symput('userfile'||left(_n_),trim(USERFILE));
      call symput('cntyur'||left(_n_),trim(CNTYUR));
      call symput('yeartype'||left(_n_),trim(YEARTYPE));
      call symput('pname'||left(_n_),trim(PNAME));
      call symput('gcflag'||left(_n_),gcflag);
      call symput('growyear'||left(_n_),growyear);
      call symput('ptype'||left(_n_),pp);
      call symput('comments'||left(_n_),trim(comments));
   run;
/**** CYCLE THROUGH EMS-HAP RETAINING BASE EMISSIONS (if applicable) and in EFFICIENT MANNER ***/
/***********************************************************************************************************/
/***********************************************************************************************************/
/** Following 2 macro variables are for when no base year emissions are created **/
%let savBINe = 0; /** Index used to tell EMS-HAP to save BIN emissions */
%let savTAFe = 0; /** Index used to tell EMS-HAP to save TAF emissions */
/***********************************************************************************************************/
   %do P=1 %to &numproj;
      %let county_run = 0; /* used for runtimes macro */
/***********************************************************************************************************/
      %let OUTNAMEorig = &OUTNAME8;  /* save filename provided in batch file */
/* filenames used for GC runs. If no name provided, filename will differ only in the extra underscore at the end */
      %let OUTNAME8 = &OUTNAMEorig._&&pname&P;
%put NOTE: &&comments&P;
title "&&comments&P";
/***********************************************************************************************************/
      %IF &base %THEN %DO;
         %IF not &pmod %THEN %DO;
            %IF &pcty %THEN %DO;
               %IF &&ptype&P=1 %THEN %DO; /* base year emissions: Save MergeBin output for county-level GC */
                  proc copy in=work out=work2;
                     select emission;
                  run;
               %END;
            %END;
            %IF &&ptype&P=1 %THEN %DO;
               %IF &DIET %THEN %DO;
                  ***  EXECUTING MACRO READTAF OR ISCTAF;
                  %IF (%upcase("&Model") = "ASPEN") %THEN %DO;
                     %READTAF;
                  %END;
                  %ELSE %DO;
                     %ISCTAF;
                  %END;
                  data runtimes; set runtimes; READTAF=time(); put READTAF=time.; run;
                  ***  EXECUTING MACRO APPLYTAF;
                  %APPLYTAF;
                  data runtimes; set runtimes; APPLYTAF=time(); put APPLYTAF=time.; run;
                  ***  EXECUTING MACRO MERGESAF;
                  %MERGESAF;
                  data runtimes; set runtimes; MERGESAF=time(); put MERGESAF=time.; run;
                  ***  EXECUTING MACRO APPLYSAF;
                  %APPLYSAF;
               %END;
               %ELSE %DO; /*no diet*/
                  ***  EXECUTING MACRO MERGESAF;
                  %MERGESAF;
                  data runtimes; set runtimes; MERGESAF=time(); put MERGESAF=time.; run;
                  ***  EXECUTING MACRO APPLYSAF;
                  %APPLYSAF;
                  data runtimes; set runtimes; APPLYSAF=time(); put APPLYSAF=time.; run;
                  ***  EXECUTING MACRO READTAF OR ISCTAF;
                  %IF (%upcase("&Model") = "ASPEN") %THEN %DO;
                     %READTAF;
                  %END;
                  %ELSE %DO;
                     %ISCTAF;
                  %END;
                  data runtimes; set runtimes; READTAF=time(); put READTAF=time.; run;
                  ***  EXECUTING MACRO APPLYTAF;
                  %APPLYTAF;
                  data runtimes; set runtimes; APPLYTAF=time(); put APPLYTAF=time.; run;
               %END;
            %END;
            %ELSE %DO; /* must be county-level GC: Use MergeBin Output */
               %GROW(ctylvl,work2.emission); /*SIGNIFY to GC that you are processing county-level data*/
            %END;
         %END;
         %ELSE %DO; /* have at least base and model-level emissions */
            %IF &pcty %THEN %DO; /*all 3 emissions*/
               %IF &&ptype&P=1 %THEN %DO; /* base year emissions: Save MergeBin output for county-level GC */
                                       /* save TAF output for model-level GC */
                  data work2.emission_bin;
                     set emission;
                  run;
               %END;
            %END;
            %IF &&ptype&P=1 %THEN %DO;
               %let batchDIET = &DIET;
               %let DIET = 0;
 /* DIET MUST NOT BE APPLIED HERE BECAUSE NEED EXTENDED SAS DATA FOR MODEL-LEVEL EMISSIONS */
/* EMS-HAP will delete extended SAS (after all projections are complete) and will omit many emission summaries if DIET is ON */
               ***  EXECUTING MACRO MERGESAF;
               %MERGESAF;
               data runtimes; set runtimes; MERGESAF=time(); put MERGESAF=time.; run;
               ***  EXECUTING MACRO APPLYSAF;
               %APPLYSAF;
               data runtimes; set runtimes; APPLYSAF=time(); put APPLYSAF=time.; run;
               ***  EXECUTING MACRO READTAF OR ISCTAF;
               %IF (%upcase("&Model") = "ASPEN") %THEN %DO;
                  %READTAF;
               %END;
               %ELSE %DO;
                  %ISCTAF;
               %END;
               data runtimes; set runtimes; READTAF=time(); put READTAF=time.; run;
               ***  EXECUTING MACRO APPLYTAF;
               %APPLYTAF;
               data runtimes; set runtimes; APPLYTAF=time(); put APPLYTAF=time.; run;
               proc copy in=outfiles out=work2; /* saves TAF output for model-level GC */
                  select &OUTNAME8;
               run;
            %END;
            %ELSE %IF &&ptype&P=2 %THEN %DO; /* use MergeBin output for county-level GC */
               %let DIET = &batchDIET;
               %GROW(ctylvl,work2.emission_bin); /*SIGNIFY to GC that you are processing county-level data*/
            %END;
            %ELSE %DO; /* must be model-level GC: Use Base-year TAF output*/
               %let DIET = &batchDIET;
/* output file name has been shifted */
               %GROW(modlvl,work2.&OUTNAMEorig._&pname1); /*SIGNIFY to GC that you are processing model-level data*/
            %END;
         %END;
      %END;
      %ELSE %DO; /* no base year emissions */
         %IF &pcty %THEN %DO;
            %IF &pmod %THEN %DO;
               %IF &&ptype&P=2 %then %let savBINe=%eval(&savBINe+1); /* Index when to execute SAF-TAF portion of EMS-HAP */
               %IF &&ptype&P=3 %then %let savTAFe=%eval(&savTAFe+1); /* Index when to execute SAF-TAF portion of EMS-HAP */
               %IF &&ptype&P=2 %THEN %DO; /* Run through county-level projections first */
                                       /* Save MergeBin output for county-level GC */
                  %IF &savBINe = 1 %THEN %DO; /* only need to copy once */
                     proc copy in=work out=work2;
                        select emission;
                     run;
                  %END;
                  %GROW(ctylvl,work2.emission); /*SIGNIFY to GC that you are processing county-level data*/
               %END;
               %ELSE %DO; /* model-level: first run EMS-HAP from bins to TAF */
 /* DIET MUST NOT BE APPLIED HERE BECAUSE NEED EXTENDED SAS DATA FOR MODEL-LEVEL EMISSIONS */
/* EMS-HAP will delete extended SAS (after all projections are complete) and will omit many emission summaries if DIET is ON */
                  %let batchDIET = &DIET;
                  %let DIET = 0;

                  %IF &savTAFe = 1 %THEN %DO; /* need to copy Mergebin output for SAF-TAF processing once */
                     proc copy in=work2 out=work;
                        select emission;
                     run;
                     ***  EXECUTING MACRO MERGESAF;
                     %MERGESAF;
                     data runtimes; set runtimes; MERGESAF=time(); put MERGESAF=time.; run;
                     ***  EXECUTING MACRO APPLYSAF;
                     %APPLYSAF;
                     data runtimes; set runtimes; APPLYSAF=time(); put APPLYSAF=time.; run;
                     ***  EXECUTING MACRO READTAF OR ISCTAF;
                     %IF (%upcase("&Model") = "ASPEN") %THEN %DO;
                        %READTAF;
                     %END;
                     %ELSE %DO;
                        %ISCTAF;
                     %END;
                     data runtimes; set runtimes; READTAF=time(); put READTAF=time.; run;
                     ***  EXECUTING MACRO APPLYTAF;
                     %APPLYTAF;
                     data runtimes; set runtimes; APPLYTAF=time(); put APPLYTAF=time.; run;
                     proc copy in=outfiles out=work2;
                        select &OUTNAME8;
                     run;
                     %let mod_finp = &OUTNAME8;
                  %END;
/* output file name has been shifted */
                  %GROW(modlvl,work2.&mod_finp); /*SIGNIFY to GC that you are processing model-level data*/
                  %let DIET = &batchDIET;
               %END;
            %END;
            %ELSE %DO; /* ONLY county-level projections */
               %let county_run = 1; /* used for runtimes macro */
               %IF &&ptype&P=2 %then %let savBINe=%eval(&savBINe+1); /* Index when to copy Mergebin output ONCE */
                %IF &savBINe = 1 %THEN %DO; /* only need to copy once */
                   proc copy in=work out=work2;
                      select emission;
                   run;
               %END;
               %GROW(ctylvl,work2.emission); /*SIGNIFY to GC that you are processing county-level data*/
            %END;
         %END;
         %ELSE %DO; /* model-level only: first run EMS-HAP from bins to TAF */
 /* DIET MUST NOT BE APPLIED HERE BECAUSE NEED EXTENDED SAS DATA FOR MODEL-LEVEL EMISSIONS */
/* EMS-HAP will delete extended SAS (after all projections are complete) and will omit many emission summaries if DIET is ON */
            %let batchDIET = &DIET;
            %let DIET = 0;

            %IF &&ptype&P=3 %then %let savTAFe=%eval(&savTAFe+1); /* Index when to execute SAF-TAF portion of EMS-HAP */
            %IF &savTAFe = 1 %THEN %DO; /* only need to copy once */
               ***  EXECUTING MACRO MERGESAF;
               %MERGESAF;
               data runtimes; set runtimes; MERGESAF=time(); put MERGESAF=time.; run;
               ***  EXECUTING MACRO APPLYSAF;
               %APPLYSAF;
               data runtimes; set runtimes; APPLYSAF=time(); put APPLYSAF=time.; run;
               ***  EXECUTING MACRO READTAF OR ISCTAF;
               %IF (%upcase("&Model") = "ASPEN") %THEN %DO;
                  %READTAF;
               %END;
               %ELSE %DO;
                  %ISCTAF;
               %END;
               data runtimes; set runtimes; READTAF=time(); put READTAF=time.; run;
               ***  EXECUTING MACRO APPLYTAF;
               %APPLYTAF;
               data runtimes; set runtimes; APPLYTAF=time(); put APPLYTAF=time.; run;
               proc copy in=outfiles out=work2;
                  select &OUTNAME8;
               run;
            %END;
/* output file name has been shifted */
            %GROW(modlvl,work2.&OUTNAMEorig._&pname1); /*SIGNIFY to GC that you are processing model-level data*/
            %let DIET = &batchDIET;
         %END;
      %END; /* THAT IS IT FOR PROJECTION FLOWCHART */
            
      /** If processing county-level projections, do not run through model-write modules **/
      %IF &&ptype&P ne 2 %THEN %DO;
         ***  EXECUTING WRITE MACROS - EITHER FOR ISC OR ASPEN;
         %IF (%upcase("&Model") = "ASPEN") %THEN %DO;
            ***  EXECUTING MACRO WRITE1;
            %WRITE1;
            ***  EXECUTING MACRO WRITE2;
            %WRITE2(&&ptype&P);
            data runtimes; set runtimes; WRITE12=time(); put WRITE12=time.; run;
            ***  EXECUTING MACRO FINISH;
            %FINISH;
            data runtimes; set runtimes; FINISH=time(); put FINISH=time.; run;
         %END;
         %IF (%upcase("&Model") = "ISCTRACT") %THEN %DO;
             %ISCWRITE(&&ptype&P,1);
            data runtimes; set runtimes; FINISH=time(); put FINISH=time.; run;
         %END;
         %IF (%upcase("&Model") = "ISC") %THEN %DO;
             %ISCWRITE(&&ptype&P,1);
            data runtimes; set runtimes; FINISH=time(); put FINISH=time.; run;
         %END;
      %END;
      %let OUTNAME8 = &OUTNAMEorig;  /* reset filename to that provided in batch file */
      ***  EXECUTING MACRO RUNTIMES;
      data runtimes; array GC{&numproj} gc1-gc%eval(&numproj); set runtimes; gc{&P}=time(); put gc{&P}=time.; run;
   %end; /*end of each projection iteration*/
   %RUNTIMES(&county_run);
   %IF &DIET %THEN %DO;
      proc datasets library=work2 kill;
      run;
   %END;
%END;
/* NO PROJECTIONS: IF YOU HAVE HUGE INVENTORY (and NOT projecting emissions), CHOOSE DIET=1 option*/
%ELSE %DO;
   %IF &DIET %THEN %DO;
       ***  EXECUTING MACRO READTAF OR ISCTAF;
         %IF (%upcase("&Model") = "ASPEN") %THEN %DO;
            %READTAF;
         %END;
         %ELSE %DO;
            %ISCTAF;
         %END;
         data runtimes; set runtimes; READTAF=time(); put READTAF=time.; run;
         ***  EXECUTING MACRO APPLYTAF;
         %APPLYTAF;
         data runtimes; set runtimes; APPLYTAF=time(); put APPLYTAF=time.; run;
         ***  EXECUTING MACRO MERGESAF;
         %MERGESAF;
         data runtimes; set runtimes; MERGESAF=time(); put MERGESAF=time.; run;
         ***  EXECUTING MACRO APPLYSAF;
         %APPLYSAF;
         data runtimes; set runtimes; APPLYSAF=time(); put APPLYSAF=time.; run;
   %END;
   %ELSE %DO;
      ***  EXECUTING MACRO MERGESAF;
      %MERGESAF;
      data runtimes; set runtimes; MERGESAF=time(); put MERGESAF=time.; run;
      ***  EXECUTING MACRO APPLYSAF;
      %APPLYSAF;
      data runtimes; set runtimes; APPLYSAF=time(); put APPLYSAF=time.; run;
      ***  EXECUTING MACRO READTAF OR ISCTAF;
      %IF (%upcase("&Model") = "ASPEN") %THEN %DO;
         %READTAF;
      %END;
      %ELSE %DO;
         %ISCTAF;
      %END;
      data runtimes; set runtimes; READTAF=time(); put READTAF=time.; run;
      ***  EXECUTING MACRO APPLYTAF;
      %APPLYTAF;
      data runtimes; set runtimes; APPLYTAF=time(); put APPLYTAF=time.; run;
   %END;

   data runtimes; set runtimes; GROWCNTL=time(); put GROWCNTL=time.; run;

   ***  EXECUTING WRITE MACROS - EITHER FOR ISC OR ASPEN;
   %IF (%upcase("&Model") = "ASPEN") %THEN %DO;
      ***  EXECUTING MACRO WRITE1;
      %WRITE1;
      ***  EXECUTING MACRO WRITE2;
      %WRITE2(1);
      data runtimes; set runtimes; WRITE12=time(); put WRITE12=time.; run;
      ***  EXECUTING MACRO FINISH;
      %FINISH;
      data runtimes; set runtimes; FINISH=time(); put FINISH=time.; run;
   %END;
   %IF (%upcase("&Model") = "ISCTRACT") %THEN %DO;
       %ISCWRITE(1,0);
      data runtimes; set runtimes; FINISH=time(); put FINISH=time.; run;
   %END;
   %IF (%upcase("&Model") = "ISC") %THEN %DO;
       %ISCWRITE(1,0);
      data runtimes; set runtimes; FINISH=time(); put FINISH=time.; run;
   %END;
   ***  EXECUTING MACRO RUNTIMES;
   %RUNTIMES(0);
%END;

%MEND MAIN;

%MAIN;
***  ---------------        END PROGRAM        ---------------  ***;
