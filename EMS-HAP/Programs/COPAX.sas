/********************************************************************************************
   Program: COunty Point and Aircraft eXtraction (COPAX): Formerly AirportProc (V3), AreaPrep
            (V2) and MobilePrep (V2)
   Date:    February 25, 2000 with modifications made up to FEB 2003
   Authors: Diane Linderman, EC/R and Richard Mason, DynCorp

   This program reads the non-point OR mobile emissions inventory and removes data from
   airport or other known-coordinate sources.  If these sites can be allocated to known
   airports or coordinates (i.e. ports), they are then added to the point source inventory
   for processing as point sources.  Sites that cannot be allocated are returned to the
   original (mobile or non-point) source inventory for processing as nonroad or area sources.

   The program is divided into three major macro programs.

   Macro programs:
      GetAirp - reads the mobile or non-point emissions inventory file, extracts the
                emissions from airports (or other point sources), allocates these county-
                level emissions to specific airport locations. Records that cannot be
                allocated are separated and will be appended back into the original -mobile
                or non-point inventory. All required point source inventory variables are
                created.
      AddToPt - appends airport emission data from the original (mobile or non-point
                emissions inventory file to the point source emissions data. 
      AddToOrg- appends unallocated airport emission data back into the original (mobile or
                non-point) emissions inventory file.

   Quality checks are performed within each macro.  In addition, pollutant code level and
   state level emission sums are calculated and listed for records going to the point source
   inventory and to the mobile or non-point source inventory.

   The user provides the names of directories, and input and output SAS dataset.

   Directory names:
      point - point emissions inventory SAS data set directory
      county - mobile or non-point emissions inventory SAS data set directory
      refdir - ancillary file directory

   SAS data set name:
      in_point - input point source inventory SAS data set name
      incounty - input county-level (mobile or non-point)  source inventory SAS data set name
      outpoint - output point source inventory SAS data set name
      outcnty  - output county-level (mobile or non-point) source inventory SAS data set name
   ***ap_af    - airport allocation factor file(s) prefix
               ***These files can contain any SCC-level data (ie. ports)

   Other inputs:
      EMISTYPE - "MV" or "AR" for mobile and non-point inventories.  Used to assign src_type
                 variable AND to determine whether all former AreaPrep processing/ancillary
                 files are needed.
    **TAFFILE  - Name of Temporal Allocation Factor file
    **SURRXREF - Name of Spatial Surrogate reference file
    **SIC2SCC  - Name of SIC to SCC cross-reference file
    **MACT2SCC - Name of MACT to SCC cross-reference file
    **SCC2AMS  - Name of SCC to AMS/SCC cross-reference file
      AIRPXREF - Airport allocation cross-reference file
      ISCAREA  - Ancillary file containing ISCST3 area source parameters for each airport
      DEFXLEN  - Default x-length for airports not in ISCAREA file
      DEFYLEN  - Default y-length for airports not in ISCAREA file
      DEFANGLE - Default angle of rotation, measured clockwise from north, for airports not
                 in ISCAREA file
      DEFRELHT - Default release height for airports not in ISCAREA file
      DEFINPLM - Default initial vertical plume dimension for airports not in ISCAREA file
      add2pt   - value of 1 directs program to append allocated airport emissions to the
                 existing input point source inventory.  Otherwise (if value of 0), output
                 point file will contain only the airport emissions.
  ** -USED ONLY WHEN &EMISTYPE=AR
*******************************************************************************************/

%MACRO GetInfo;
options mprint mlogic symbolgen;
********************************************************************************************;
* MACRO: GetInfo                                                                            ;
*     This macro reads a user designed batch file to get the names of the directories, file ;
*     names and include files, as well as variables to be used throughout the program.      ;
********************************************************************************************;

%global model sicvar amsvar catvar;
%global num_files;

** Initialize sicvar. Will be 1 if non-point inventory does NOT have SIC variable;
** FEB 2003:  NATA must have AMS,SIC, MACT, and cat_name;
** MAY 2003:  CAT_NAME is not required for NIF inventory but will retain if present;
data _null_;
   %let model  = %sysget(MODEL);
   %let sicvar = 0;
   %let amsvar = 0;
   %let catvar = 0;
run;
%put &amsvar;
%if (%upcase("&model") = "ASPEN") %then %do;

   data _null_;

      %global emistype in_point incounty outpoint outcnty airpxref ap_af add2pt;

      %let emistype = %sysget(EMISTYPE);
      %let in_point = %sysget(INPOINT);
      %let incounty = %sysget(INCOUNTY);

      %let outpoint = %sysget(OUTPOINT);
      %let outcnty  = %sysget(OUTCNTY);

      %let airpxref = %sysget(AIRPXREF);
      %let ap_af = %sysget(AP_AF);

      %let add2pt = %sysget(ADD2PT);

   run;
%end;


%else %if (%upcase("&model") = "ISC") %then %do;

   data _null_;
      %global emistype in_point incounty outpoint outcnty airpxref ap_af
              add2pt iscarea defxlen defylen defangle defrelht definplm;

      %let emistype = %sysget(EMISTYPE);
      %let in_point = %sysget(INPOINT);
      %let incounty = %sysget(INCOUNTY);

      %let outpoint = %sysget(OUTPOINT);
      %let outcnty  = %sysget(OUTCNTY);

      %let airpxref = %sysget(AIRPXREF);
      %let ap_af    = %sysget(AP_AF);
      %let iscarea  = %sysget(ISCAREA);

      %let defxlen  = %sysget(DEFXLEN);
      %let defylen  = %sysget(DEFYLEN);
      %let defangle = %sysget(DEFANGLE);
      %let defrelht = %sysget(DEFRELHT);
      %let definplm = %sysget(DEFINPLM);

      %let add2pt = %sysget(ADD2PT);

   run;

%end;

%else %do;
   put @1 'Error!!! Model designation is incorrect.';
   STOP;
%end;

%IF &emistype = AR %THEN %DO;
   data _null_;
      %global taffile surrxref sic2scc mact2scc scc2ams;

      %let taffile  = %sysget(TAFFILE);
      %let surrxref = %sysget(SURRXREF);
      %let sic2scc  = %sysget(SIC2SCC);
      %let mact2scc = %sysget(MACT2SCC);
      %let scc2ams  = %sysget(SCC2AMS);
   run;

%END;

%put _user_;

*** Set Directories ***;

Libname point "$POINT";
Libname county "$COUNTY";
Libname refdir "$REFDIR";
Filename reffile "$REFDIR";

%MEND GetInfo;

********************************************************************************************;
********************************************************************************************;


%MACRO GetAirp;

********************************************************************************************;
* GetAirp                                                                                   ;
*    Extract airports from mobile or non-point sources file. Merges airport data set by     ;
*  state and county FIPS with allocation file containing the airports in each county and an ;
*  allocation factor for each airport,as well as the latitude and longitude. Separates out  ;
*  records that could not be allocated. Assigns required variables for those records to be  ;
*  processed as point sources.                                                              ;
*    When processing data for ISCST3, user can provide a file to supply ISC area parameters ;
*  for each airport by the airport ID used in the allocation file.                          ;
********************************************************************************************;

********* NON-POINT/MOBILE INVENTORY WITH SCC CODES IN ANCILLARY CROSS-REFERENCE FILE *;

*** Mobile inventory may not have SIC code **;
proc contents data=county.&incounty out=chksic(keep=name) noprint; run;
data _null_;
   set chksic;
   if upcase(trim(name)) = "SIC" then call symput('sicvar',1);
   if upcase(trim(name)) = "AMS" then call symput('amsvar',1);
   if upcase(trim(name)) = "CAT_NAME" then call symput('catvar',1);
run;
%IF &emistype = AR %THEN %DO;
**********************************************************************************************;
* MAY 2003 -If inventory is NTI 96 and non-point, preserve AMS name for surrogate assignment*;
**********************************************************************************************;
   %if &amsvar %then %do;
      data &incounty(rename=(AMS=SCC));
        length sic $4;
        set county.&incounty(rename=(SCC=SCC_INV));
         where emis > 0; *** NATA mobile inventory contains records with zero emissions *;
        %if not &sicvar %then %do;
           sic = '';
        %end;
      run;
   %end;
   %else %do;
      data &incounty;
        length sic $4;
        set county.&incounty;
         where emis > 0; *** NATA mobile inventory contains records with zero emissions *;
        %if not &sicvar %then %do;
           sic = '';
        %end;
      run;
   %end;
   proc sort data=&incounty; by SCC; run;
%END;
**********************************************************************************************;
******** MAY 2003 -If inventory is NTI 96 and mobile, AMS is SCC AND, DROP CAT_NAME variable *;
**********************************************************************************************;
%IF &emistype = MV  %THEN %DO;
   %if &amsvar %then %do;
      data &incounty(keep=fips SCC CAS emis);
        length scc $10;
        label SCC = 'SCC';
        set county.&incounty(rename=(AMS=SCC));
         where emis > 0; *** NATA mobile inventory contains records with zero emissions *;
      run;
      proc sort data=&incounty; by SCC; run;
   %end;
   %else %do;
      proc sort data=county.&incounty(keep=fips SCC CAS emis) out=&incounty; 
         where emis > 0; *** NATA mobile inventory contains records with zero emissions *;
         by SCC;
      run;
   %end;
%END;
**********************************************************************************************;
data airpxref;
   length SCC $10 cat_name $90;
   infile reffile("&AIRPXREF..txt") firstobs=3 pad;
   input SCC $ 1-10 code 12-13 cat_name $15-104;
   SCC = left(SCC);
   if code > 0;
run;
proc sort data=airpxref nodupkey;
   by SCC;
run;
proc print data=airpxref;
   title "SCC codes in the Allocation Cross-Reference File (&AIRPXREF..txt)";
run;
proc sort data=&incounty out=non_scc nodupkey;
   by SCC;
run;
data noSCC_emis;
   merge airpxref(in=a) non_scc(in=b);
   by SCC;
   if a and not b;
run;
proc print data=noSCC_emis;
   var SCC code cat_name;
   title "Warning: Following SCC codes are in the Allocation Cross-Reference File (&AIRPXREF..txt) but NOT in COUNTY-LEVEL inventory";
run;
title;
proc sort data=airpxref(keep=code) out=num_files nodupkey;
   by code;
run;

********* NON-POINT/MOBILE INVENTORY WITH SCC CODES NOT IN ANCILLARY CROSS-REFERENCE FILE *;

data county.&outcnty;
   length SCC $10;
   merge &incounty(in=a) airpxref(in=b drop=code);
   by SCC;
   if a and not b;
run;

data scc_ap_;
   length SCC $10;
   merge &incounty(in=a keep=fips cas emis scc 
            %IF &emistype = AR %THEN %DO;
                %IF &amsvar %THEN %DO; scc_inv cat_name %END; mact sic
            %END;)
         airpxref(in=b keep=scc code %IF &emistype = MV or not &amsvar %THEN %DO; cat_name %end;);
   by SCC;
   if a and b;
run;
/**Use CAT_NAME from ANCILLARY FILE RATHER THAN INVENTORY for NIF2.0 */
*** NTI (if inventory has AMS variable) must have CAT_NAME variable ***;
data scc_ap scc_noap(drop=code);
   set scc_ap_;
   if cat_name = '' then do;
      cat_name = SCC;
      output scc_noap;
   end;
   else output scc_ap;
run;
********************************************************************************************;
* build QA step to check for existence of records in scc_ap -if not, skip all allocations **;
********************************************************************************************;
proc sort data=scc_ap out=uniqcodes nodupkey;
   by code;
run;
%let num_files = 0;
data _null_;
   set uniqcodes end=last;
   call symput('ap'||left(_n_),code);
   if last then call symput('num_files',_n_);
run;
%if &num_files %then %do;

   ******** SPLIT EMISSIONS into SEPARATE AIRCRAFT CODE FILES ***;
   data %do I=1 %to &num_files; scc_ap&I %end;;
      set scc_ap;
      %do I=1 %to &num_files;
         if code = &&ap&I then output scc_ap&I;
      %end;
   run;
   ******** MERGE SEPARATE AIRCRAFT EMISSIONS FILES WITH AIRCRAFT FACTOR FILES ***;
   %do I = 1 %to &num_files;
      proc sort data=scc_ap&I; by fips; run;
      proc sort data= refdir.&ap_af&I out=apaf&I; by fips; run;
      proc sort data= refdir.&ap_af&I out=apaf_&I nodupkey; by fips; run;
      proc sql noprint;
         create table temp&I as
            select a.fips, cas, emis, SCC, locid, &ap_af&I as factor,
                   sitename,lat, lon,emis*&ap_af&I as emis_ 
  %IF &emistype = AR %THEN %DO;
     , a.sic, a.mact
     %IF &amsvar %then %do;
     , scc_inv
     %end;
   %END;
            from scc_ap&I a left join apaf&I b
            on (a.fips= b.fips)
      ;
      quit;
      data back2cty&I;
         merge scc_ap&I(in=ina) apaf_&I(in=inb);
         by fips;
         if ina and not inb then output back2cty&I;
      run;
      proc print data=back2cty&I; 
         var fips cas scc cat_name emis;
         title "Warning: problem with airport (or other source) allocation file number &ap_af&I";
         title2 "FIPS are missing in allocation factors file";
         title3 "These Emissions will be appended BACK into the county-level inventory";
      run;
*** QA merge and summary: if emissions inventory FIPS not present in ap_af file or county emission sums not equal **;
      proc sort data=temp&I; by fips; run;
      proc summary data = temp&I sum;
         by fips;
         var emis_;
         output out=qa_out(drop=_type_ _freq_) sum=;
      run;
      proc summary data = scc_ap&I sum;
         by fips;
         var emis;
         output out=qa_inp(drop=_type_ _freq_) sum=;
      run;
      data bad;
         merge qa_out(in=a) qa_inp(in=b);
         by fips;
         if abs(emis-emis_) > 0.0001 then output;
      run;
      proc print data=bad label;
         label emis='inventory emissions' emis_ = 'emissions after merging factor';
         var fips emis emis_;
         title "Warning: problem with airport (or other point) allocation file number &ap_af&I";
         title2 "Sum of County Allocation factors do not equal 1.0";
         title3 "GO BACK AND FIX (NORMALIZE THE FACTORS) YOUR ALLOCATION FILE";
      run;
      %IF &I = &num_files %THEN %DO;
         data back2cty(keep=fips cas cat_name emis scc %IF &emistype = AR %THEN %DO;  sic mact %END;);
            set %do I=1 %to &num_files; back2cty&I %end;;
         run;
         data airports(rename=(emis_=emis)
                keep=fips cas emis_ scc locid lat lon sitename
                      %IF &emistype = AR %THEN %DO;
                         sic mact
                         %if &amsvar %then %do;
                            scc_inv
                         %end;
                      %END;);
            set %do I=1 %to &num_files; temp&I(drop=emis) %end;;
         run;
      %END;
   %end;

   *****************************************************************************************;
   ** Output Airport file with necessary variables for PtDataProc                           ;
   *** Match to ISCST3 airports                                                             ;
   *** If processing data for ISCST3, user can supply ancillary file to add ISC area        ;
   ***   parameters for each airport                                                        ;

   %if %upcase("&model") = "ISC" %then %do;
      %if %upcase("&iscarea") ne "NONE" and %quote(&iscarea) ne %then %do;

         proc sort data=airports;
            by locid;

         data iscarea;
            infile reffile("&ISCAREA..txt") firstobs=3 missover;
            input @1 locid $4.  @6 axlen 8.  @15 aylen 8.  @24 aangle 4.  @29 arelhgt 4.  @34 ainplum 4.;

         proc sort data=iscarea;
            by locid;

         data airports nolocid;
            merge airports(IN=A) iscarea(IN=B);
            by locid;
            length ISCtype $9;

            if A then do;
               ISCtype = 'ISCarea';
               if not B then do;
                  axlen = &defxlen.; *2000;
                  aylen = &defylen.; *2000;
                  aangle = &defangle.; *0;
                  arelhgt = &defrelht.; *2;
                  ainplum = &definplm.; *2;
               end;
               output airports;
            end;

            if B and not A then output nolocid;

         *** Print out list of airport location IDs not found in inventory ***;

         proc print data=nolocid noobs;
            var locid;
            title 'Airport (or other point source) Location IDs NOT Found in Emission Inventory';
         run;
      %end;
   %end;

   proc sort data = airports; by fips sitename; run; **Added 05/03/01 by Rich;

   *** Create id fields and other point source fields for those sites going to the ***;
   *** point source inventory                                                      ***;

   data airports(drop=locid);
      set airports(rename=(lon=x lat=y));
      where emis > 0; ** added Oct 2002 *;
      length emrelpid $50 emrelpty $2 MACT $7 sic $4 site_id $25 src_type $15 xy_type $7 zip_code $12;

      if x > 0 then x = -(x) ;
      xy_type = 'LATLON';
      utm_z = . ;
      %IF &emistype = MV %THEN %DO;
         src_type = 'nonroad';
         sic = ' ';
         MACT = ' ';  **FEB 2003;
      %END;
      %ELSE %DO;
         src_type = 'area';
      %END;
      zip_code = ' ';
      stackdia = . ;
      stackht = . ;
      stackvel = . ;
      stktemp = . ;
      cntl_eff = . ;
      *site_id = 'AP'||compress(FIPS)||'-'||compress(locid);
      site_id = 'COPAX'||compress(FIPS)||'-'||compress(locid);
      emrelpid = compress(site_id) || compress(scc);
      emrelpty = 'AP'; *Ensures source is modeled as a fugitive by ASPEN *;
   run;
%end;
%else %do;
   data airports;
      set _null_;
   run;
   %put WARNING: No Airport SCC match between inventory and airport reference file(s);
%end;
%MEND GetAirp;

********************************************************************************************;
********************************************************************************************;

%MACRO AddToPt;

********************************************************************************************;
*  MACRO: AddToPt                                                                           ;
*     This macro appends the airport data set with the emissions inventory data set.        ;
********************************************************************************************;

%if &add2pt %then %do;
   data point.&outpoint;
      format SCC $10. src_type $15.;
      set point.&in_point airports(drop= %if &emistype = AR and &amsvar %then %do; scc_inv %end;);
   run;
%end;
%else %do;
   data point.&outpoint;
      set airports(drop= %if &emistype = AR and &amsvar %then %do; scc_inv %end;);
   run;
%end;

*** QA Check: Airport sites added to Point Source Inventory ***;

proc sort data=airports;
   by site_id;

data airpsite;
   set airports;
   by site_id;
   if first.site_id;

title ;
proc print data=airpsite(obs=10) noobs;
   var fips sitename;
   title 'First 10 Sites Contributed from Airport (or other point) Data';

*** Produce pollutant-level summary of Airports to be Modeled as Point Sources ***;

proc sort data = airports;
  by cas;

Proc means sum n data=airports noprint;
  by cas;
  var emis;
  output out=airplsum(drop=_freq_ _type_) sum=emis n=count;

proc print data=airplSum noobs split='*';
   label cas = 'Pollutant Code'
         emis = 'Emission*Total'
         count = 'Total Number*of Records';
   sum emis count;
   title 'Pollutant Code Summary of Airport (and-or other point sources) Emissions to be Modeled as Point Sources';

*** Produce state-level summary ***;
data qa_states(drop=fips);
   set airports(keep=fips emis);
   st_fips = substr(fips,1,2);
run;
proc sort data=qa_states;
  by st_fips;

proc means sum n data=qa_states noprint;
   by st_fips;
   var emis;
   output out=airstsum(drop=_type_ _freq_) sum=emis n=count;

proc print data=airstsum noobs split='*';
   label st_fips = 'State'
         emis = 'Emission*Total'
         count = 'Total Number*of Records';
   sum emis count;
   title 'State Summary of Airport (and-or other point sources) Emissions to be Modeled as Point Sources';
run;
title ;

%MEND AddToPt;

********************************************************************************************;
********************************************************************************************;

%MACRO AddToOrg;

********************************************************************************************;
*  MACRO: AddToOrg                                                                          ;
*  This macro appends the unallocated airport data set with the emissions inventory data set;
********************************************************************************************;

/** MAY 2003: Build output from near-input if no records were extracted from the county-level inventory */
%if &num_files %then %do;
   data county.&outcnty;
      set county.&outcnty back2cty;
   run;
%end;
%else %do;
   data county.&outcnty;
      set &incounty;
   run;
%end;
/** June 2003: If inventory is NTI non-point, switch variable names BACK to original for surrogate assignments **/
%IF  &emistype = AR  and &amsvar %THEN %DO;
   data county.&outcnty(rename=(SCC_INV=SCC));
      length SCC_INV $8 AMS $10;
      label SCC_INV='SCC' AMS='AMS';
      set county.&outcnty(rename=(SCC=AMS));
   run;
%END;

********************************************************************************************;
** THIS IS ESSENTIALLY AreaPrep from V2.0                                                   ;
********************************************************************************************;
%IF &emistype = AR %THEN %DO;
   %if &amsvar %then %do;
     proc sort data=county.&outcnty out=as_codes nodupkey;
        by ams scc sic mact cat_name;
     run;
     title2 "All Source Category Code Combinations in the Non-point Source Inventory";
     title3 "Sorted by AMS SCC SIC MACT CAT_NAME";
     proc print data=as_codes;
        var ams scc sic mact cat_name;
     run;
   %end;
   %else %do;
     proc sort data=county.&outcnty out=as_codes nodupkey;
       by scc sic mact;
     run;
     title2 "All Source Category Code Combinations in the Non-point Source Inventory";
     title3 "Sorted by SCC SIC MACT";
     proc print data=as_codes;
        var scc sic mact %if &amsvar %then %do; cat_name %end;;
     run;
   %end;
  proc datasets; delete as_codes; run;
**  READ THE XREF FILES: Former READXREF MACRO ****************************************;

   data spatsurr;
      length _scc $10;
      infile reffile("&SURRXREF..txt") FIRSTOBS=2; *V3 begins on line 2;
      input _scc $ 1-10 s_surr 14-16;
      _scc = compress(_scc);
   run;
   proc sort nodupkey; by _scc; run;

   %IF "&SIC2SCC" ne "NONE" and %quote(&SIC2SCC) ne %THEN %DO;
      data sic_scc;
         length _scc1 $10 sic $4;
         infile reffile("&SIC2SCC..txt") FIRSTOBS=2;
         input sic $ 1-4 _scc1 $ 7-16 s_sic;
         _scc1 = compress(_scc1);
         sic = compress(sic);
      run;
      proc sort nodupkey; by sic; run;
      title2 "The SIC-to-_SCC link file";
   %END;
   %ELSE %DO;
      data sic_scc;
         length _scc1 $10 sic $4 s_sic 3.;
         set _null_;
         _scc1 = '';
         sic   = '';
         s_sic = .;
      run;
   %END;

******** FEB 2003 ***********;
   %IF "&SCC2AMS" ne "NONE" and %quote(&SCC2AMS) ne %THEN %DO;
      data scc_ams;
         length _scc1 $10 scc $8;
         infile reffile("&SCC2AMS..txt") FIRSTOBS=2;
         input scc $ 1-8 _scc1 $ 11-20 s_scc;
         _scc1 = compress(_scc1);
         scc = compress(scc);
      run;
      proc sort nodupkey; by scc; run;
   %END;
   %ELSE %DO;
      data scc_ams;
         length _scc1 $10 scc $8 s_scc 3.;
         set _null_;
         _scc1 = '';
         scc   = '';
         s_scc = .;
      run;
   %END;

   %IF "&MACT2SCC" ne "NONE" and %quote(&MACT2SCC) ne %THEN %DO;
      data mact_scc;
         length _scc1 $10 mact $7;
         infile reffile("&MACT2SCC..txt") FIRSTOBS=2 missover;
         input mact $ 1-7 _scc1 $ 9-18 s_mact 20-22;
         _scc1 = compress(_scc1);
         mact = compress(mact);
         if s_mact ne .; *AUG 2003: Allows COPAX and PtTemporal to specify different TAFs (via SCC) for same MACT;
      run;
      proc sort nodupkey; by mact; run;
   %END;
   %ELSE %DO;
      data mact_scc;
         length _scc1 $10 mact $7 s_mact 3.;
         set _null_;
         _scc1  = ''; 
         mact   = ''; 
         s_mact = .;
      run;
   %END;
** END FORMER READXREF MACRO *****************************************************;
   ***  CREATE _SCC IN EMISSIONS FILE;
   ***  MERGE IN MACT+SIC TO SCC XREFS TO FILL OUT _SCC IN THE EMISSIONS FILE;

   ***  THE INITIAL PRE-MERGE DATA SET;
   data emis0;
      length match $4 _scc $10;
      set county.&outcnty;
      match="none";
      _scc="";
      mact = compress(mact);
   run;

   ***  MATCH BY MACT;
   proc sort data=emis0; by mact; run;
   proc sort data=mact_scc; by mact; run;
   data emis0 emismact;
      length _scc $10 match $4 mact $7;
      merge emis0(in=in1) mact_scc(in=in2);
      by mact;
      if NOT in1 then delete;
      if compress(_scc1)="" then output emis0;
      else do;
         match = "MACT";
         _scc = _scc1;
         drop _scc1;
         output emismact;
      end;
   run;


   ***  MATCH BY SIC;
   ***  The obs coming in all have missing _scc;
   proc sort data=emis0; by sic; run;
   proc sort data=sic_scc; by sic; run;
   data emis0 emis_sic siconly;
      length _scc $10 match $4 sic $4;
      merge emis0(in=in1) sic_scc(in=in2);
      by sic;
      if in2 AND NOT in1 then output siconly;
      if NOT in1 then delete;
      if compress(_scc1)="" then output emis0;
      else do;
         if compress(_scc1)=" " OR _scc1=" " then ABORT;
         match = "SIC";
         _scc = _scc1;
         drop _scc1;
         output emis_sic;
      end;
   run;

   %if &amsvar %then %do;
      ************ FEB 2003 ***************************************;
      ***  MATCH BY SCC;
      ***  The obs coming in all have missing _scc;
      proc sort data=emis0; by scc; run;
      proc sort data=scc_ams; by scc; run;
      data emis0 emis_scc scconly;
         length _scc $10 match $4 scc $10;
         merge emis0(in=in1) scc_ams(in=in2);
         by scc;
         if in2 AND NOT in1 then output scconly;
         if NOT in1 then delete;
         if compress(_scc1)="" then output emis0;
         else do;
            match = "SCC";
            _scc = _scc1;
            drop _scc1;
            output emis_scc;
         end;
      run;

      ***  Print SCC Codes In Emissions File Not In SCC Link File, If Any.
      ***  These Can To Be Added To The SCC Link File;
      data temp1;
         set emis0;
         if compress(scc) NE "";
      run;
      proc sort data=temp1 nodupkey; by scc; run;
         title2 "SCC Codes in Emissions File Not in SCC Link File";
         title3 "THESE CAN BE ADDED TO THE SCC-AMSSCC LINK FILE";
      proc print data=temp1;
         var match _scc sic scc ams cat_name;
      run;
      proc datasets; delete temp1; run;

      ***  MATCH BY AMS: IF NATA;
      ***  The obs coming in all have missing ams_scc;
      data emis0 emis_AMS amserr1 amserr2;
         length _scc $10 match $4 AMS $10;
         set emis0;
         if match NE "none" then output amserr1;
         if compress(AMS)="" then output emis0;
         else do;
            if compress(_scc)="" then do;
               _scc = AMS;
               match = "AMS";
               output emis_AMS;
            end;
            else output amserr2;
         end;
      run;
      title2 "Warning: PROBLEM 1 IN MATCH BY AMS";
      proc print data=amserr1(obs=1000);
         var _scc match AMS;
      run;
      title2 "Warning: PROBLEM 2 IN MATCH BY AMS";
      proc print data=amserr2(obs=1000);
         var _scc match AMS;
      run;
      ***  MERGE IN AMS-LEVEL SURROGATES;
      proc sort data=emis_AMS; by _scc; run;
      data emis_AMS;
         merge spatsurr emis_AMS(in=in2);
         by _scc;
         if in2;
      run;
   %end;
   %else %do;

      data emis0 emis_SCC sccerr1 sccerr2;
         length _scc $10 match $4 SCC $10;
         set emis0;
         if match NE "none" then output sccerr1;
         if compress(SCC)="" then output emis0;
         else do;
            if compress(_scc)="" then do;
               _scc = scc;
               match = "scc";
               output emis_scc;
            end;
            else output sccerr2;
         end;
      run;
      title2 "Warning: PROBLEM 1 IN MATCH BY SCC";
      proc print data=SCCerr1(obs=1000);
         var _scc match SCC;
      run;
      title2 "Warning: PROBLEM 2 IN MATCH BY SCC";
      proc print data=SCCerr2(obs=1000);
         var _scc match SCC;
      run;
      ***  MERGE IN SCC-LEVEL SURROGATES;
      proc sort data=emis_SCC; by _scc; run;
      data emis_SCC;
         merge spatsurr emis_SCC(in=in2);
         by _scc;
         if in2;
      run;
   %end;


   ** FORMER MACRO COMB1 ************************************************************;
   ***  COMBINE MATCHED (and unmatched) EMIS FILES TO GO FORWARD WITH;

   data AS_EMIS3;
      set emis_SCC emis_sic %if &amsvar %then %do; emis_ams %end; emismact emis0;
      format _scc $10.;
      format match $4.;
      if compress(_scc)="" then do;
         _scc="7777777777";   * missing source category code;
      end;
      keep _scc match sic scc mact %if &amsvar %then %do; ams s_scc %end; fips CAS emis s_sic s_mact s_surr
         %if &catvar %then %do; cat_name %end;;
   run;
** END FORMER MACRO COMB1 ********************************************************;

   proc datasets; delete emis_SCC emis_sic %if &amsvar %then %do; emis_ams %end; emismact; run;

   ***  CODE MATCHING FINISHED  ***;

   ***  MERGE IN SPATIAL SURROGATE CODE.
   ***  ORDER OF PREFERENCE FOR ASSIGNING SURROGATE:
   ***  1. THE SURROGATES IN THE MACT LINK FILE,   2. THE SURROGATES IN THE SIC LINK FILE,
   ***  3. THE SPATIAL SURROGATE-BY-SCC FILE spatsurr;

   data county.&outcnty;
      set AS_EMIS3;
      spatsurr = s_mact;
      if spatsurr < 0 then do;
         spatsurr = s_sic;
         if spatsurr < 0 then do;
            %if &amsvar %then %do;
               spatsurr = s_scc;
            %end;
            if spatsurr < 0 then spatsurr = s_surr;
         end;
      end;
   run;

   title2 "Spatial Surrogates Used";
   proc sort data=county.&outcnty out=temp1 nodupkey;
      by spatsurr;
   run;
   proc print data=temp1; var spatsurr; run;


   *======= PRINT CODE MATCHING AND SPATIAL SURROGATE ASSIGNMENT TABLES =======;
   options ps=999 ;
   data temp11;
      set county.&outcnty;
      surr = spatsurr;
      keep %if &catvar %then %do; cat_name %end;
        sic scc %if &amsvar %then %do; ams s_scc %end; mact _scc match surr s_sic s_mact s_surr emis;
   run;

   proc sort data=temp11;
      by %if &catvar %then %do; cat_name %end;
          sic scc %if &amsvar %then %do; ams %end; mact _scc match surr;
   run;

   proc means data=temp11 noprint;
      var emis;
      by %if &catvar %then %do; cat_name %end;
          sic scc %if &amsvar %then %do; ams %end; mact _scc match surr;
      id s_sic %if &amsvar %then %do; s_scc %end; s_mact s_surr;
      output out=temp22(drop = _type_ _freq_) n=n;
   run;

   data temp22 MissSurr;
      %if &catvar %then %do;      length catname $42; %end;
      set temp22;
      %if &catvar %then %do; catname = substr(cat_name,1,42); %end;
      output temp22;
      if surr < 1 then output MissSurr;
   run;

   title2 "Warning: Source Categories With No Spatial Surrogate Assignments";
   title3 "CountyProc will assign a default surrogate to these sources based on its batch file variable DEFLTSAF";
   proc print data=MissSurr uniform noobs;
      var match surr %if &catvar %then %do; catname %end; n sic scc %if &amsvar %then %do; ams %end; mact _scc s_sic s_mact s_surr;
   run;
   title;
   title2 "All Code Combinations, With Matched _SCC Code and Spatial Surrogates";
   proc print data=temp22 uniform noobs;
      var match surr %if &catvar %then %do; catname %end; n sic scc %if &amsvar %then %do; ams %end; mact _scc s_sic s_mact s_surr;
   run;

   %if &amsvar %then %do;
      title2 "Code Combinations, With Matched _SCC Code and Spatial Surrogates - AMS sort";
      proc sort data=temp22;
         by ams catname sic scc mact _scc;
      run;
      proc print data=temp22 uniform noobs;
         where ams NE "";
         var match surr catname n sic scc ams mact _scc s_sic s_scc s_mact s_surr;
      run;
   %end;

   title2 "Code Combinations, With Matched _SCC Code and Spatial Surrogates - SIC sort";
   proc sort data=temp22;
      by sic %if &catvar %then %do; catname %end; scc %if &amsvar %then %do; ams %end; mact _scc;
   run;
   proc print data=temp22 uniform noobs;
      where sic NE "";
      var match surr %if &catvar %then %do; catname %end; n sic scc %if &amsvar %then %do; ams %end; mact _scc s_sic s_mact s_surr;
   run;

   title2 "Code Combinations, With Matched _SCC Code and Spatial Surrogates - SCC sort";
   proc sort data=temp22;
      by scc %if &catvar %then %do; catname %end; sic %if &amsvar %then %do; ams %end; mact _scc;
   run;
   proc print data=temp22 uniform noobs;
      where scc NE "";
      var match surr %if &catvar %then %do; catname %end; n sic scc %if &amsvar %then %do; ams %end; mact _scc s_sic s_mact s_surr;
   run;

   title2 "Code Combinations, With Matched _SCC Code and Spatial Surrogates - MACT sort";
   proc sort data=temp22;
      by mact %if &catvar %then %do; catname %end; sic %if &amsvar %then %do; ams %end; scc _scc;
   run;
   proc print data=temp22 uniform noobs;
      where mact NE "";
      var match surr %if &catvar %then %do; catname %end; n sic scc %if &amsvar %then %do; ams %end; mact _scc s_sic s_mact s_surr;
   run;

   options ps=40;

   *===========================================================;
   ***  CHECK TEMPORAL PROFILE ASSIGNMENTS;

** BEGIN FORMER MACRO READTAF **************************************************************;
   data TAF;
      length _scc $10;
      infile reffile("&TAFFILE..txt") lrecl=205 firstobs=3;
      input _scc $ 1-10;
      _scc = left(_scc);
      keep _scc;
   run;
   proc sort data=TAF nodupkey; by _scc; run;

** END FORMER MACRO READTAF ****************************************************************;

   proc sort data=county.&outcnty out=temp2 nodupkey; by _scc; run;
   data temp1;
      merge temp2(in=in1) TAF(in=in2);
      by _scc;
      if in1 and not in2;
   run;
   %if &amsvar %then %do;
      title2 "Warning: The following AMS_SCC categories were not found in the Temporal Allocation Factor file";
   %end;
   %else %do;
      title2 "Warning: The following SCC categories were not found in the Temporal Allocation Factor file";
   %end;
   title3 "They will be assigned a uniform temporal profile in CountyProc";
   proc print data=temp1;
      var _SCC;
   run;

   *===========================================================;

   ***  DROP VARS, SET SCC=_SCC AND POLLCODE FOR CountyProc;

   data county.&outcnty;
      set county.&outcnty;
      drop s_surr s_sic %if &amsvar %then %do; s_scc %end; s_mact;

      /*** SET AMS = _SCC FOR CountyProc- NATA
      *** SET SCC = _SCC FOR CountyProc- NIF2.0 */
      %if &amsvar %then %do;
         AMS = _scc;
      %end;
      %else %do;
         SCC = _scc;
      %end;
      drop _scc;
   run;

   **  Summary of Emissions With Missing Source Category Codes;
   proc sort data=emis0; by CAS; run;
   proc univariate data=emis0 noprint;
      var emis;
      by CAS;
      output out=temp4 N=Nrecords sum=emis max=max;
   run;
   title2 "Warning: Summary of Emissions With Missing Source Category Codes";
   proc print data=temp4 uniform split="*";
      var CAS emis max Nrecords;
      sum Nrecords;
      label CAS="Pollutant" emis="Emissions*(tpy)" max="Maximum*emissions"
            Nrecords="Number of*records";
   run;

   ***  STATE SUMMARIES OF DROPPED EMISSIONS;

   **  Summary by State of Emissions Dropped Due to Missing Source Category Codes;
   data emis0;
      set emis0;
      staten = fipstate(substr(fips,1,2));
   run;
   proc sort data=emis0; by staten CAS; run;
   proc univariate data=emis0 noprint;
      var emis;
      by staten CAS;
      output out=temp4 N=Nrecords sum=emis max=max;
   run;
   title2 "Summary by State of Emissions Dropped Due to Missing Source Category Codes";
   proc print data=temp4 uniform split="*";
      var CAS emis max Nrecords;
      by staten;
      sum Nrecords;
      label CAS="Pollutant" emis="Emissions*(tpy)" max="Maximum*emissions"
            Nrecords="Number of*records" staten="State";
   run;

   ***  PRINT EMISSIONS SUMMARY;
   title2 "Preprocessed Area Source Pollutant Sums";
   proc sort data=county.&outcnty; by cas; run;
   proc means data=county.&outcnty noprint;
      var emis;
      by cas;
      output out=temp1 n=n sum=emis;
   run;
   proc print data=temp1 uniform;
      var cas n emis;
      sum emis n;
   run;

   title3 "Source Category Frequencies";
   proc freq data=county.&outcnty;
      table
         %if &amsvar %then %do;
           AMS
         %end;
         %else %do;
           SCC
         %end;;
   run;

   ***  PRINT STATE TOTALS;
   title3 "State Records and Emissions Totals";
   data temp1;
      set county.&outcnty;
      state=substr(fips,1,2);
      keep state emis;
   run;
   proc sort data=temp1; by state; run;
   proc univariate data=temp1 noprint;
      var emis;
      by state;
      output out=temp2 n=nrecords sum=emis;
   run;
   proc print data=temp2 uniform;
      var state nrecords emis;
      sum nrecords emis;
   run;

%END;
*****************************************************************************************;

*** QA new Point and Mobile Inventories ***;
%if &add2pt %then %do;
   data inp;
      format SCC $10.;
      set point.&in_point(keep=scc emis) &incounty(keep=scc emis);
   run;
   proc sort data=inp;
      by scc;
   run;
%end;
%else %do;
   proc sort data= &incounty(keep=scc emis) out=inp;
      by scc;
   run;
%end;
proc summary data=inp sum;
   by scc;
   var emis;
   output out=inp_(drop=_type_ _freq_) sum=inpemis;
run;
data ap_cty;
   format SCC $10.;
   set %if &num_files %then %do; point.&outpoint(keep=scc emis) %end;
      county.&outcnty(keep=emis  
       %if &emistype = AR and &amsvar %then %do;
          ams rename=(ams=scc)
       %end;
       %else %do;
          scc
       %end;);
run;
proc sort data=ap_cty;
   by scc;
run;
proc summary data=ap_cty sum;
   by scc;
   var emis;
   output out=out_(drop=_type_ _freq_) sum=;
run;
data merg;
   format SCC $10.;
   merge inp_ out_;
   by scc;
run;
proc print data=merg label split='*';
   label emis='Emissions*after*COPAX' inpemis='Emissions*before*COPAX';
   title "Point and County-level SCC emission summaries by SCC before and after COPAX";
%if &emistype = AR and &amsvar %then %do;
   title2 "Individual Non-point SCC/AMS summaries may not match due to AMS/SCC gap filling...";
   title3 "..HOWEVER, total emissions should be identical";
%end;
   sum emis inpemis;
run;

********************************************************************************************;
* If inventory is MOBILE, SPLIT into ONROAD and NONROAD if, and ONLY if, BOTH onroad and    ;
* nonroad emissions are present.  IF emissions are NATA, AMS variable was renamed earlier as SCC;
********************************************************************************************;

%IF &emistype eq MV %THEN %DO;
   data county.&outcnty county.&outcnty._on county.&outcnty._of;
      set county.&outcnty(keep=scc fips emis cas %if &emistype = AR and &amsvar %then %do; cat_name %end;);
      if (substr(scc,1,3) = '220') or (substr(scc,1,3) = '223') then  output county.&outcnty._on;
      else output county.&outcnty._of;
      output county.&outcnty;
   run;
   
   %let keepsplit = 1;
   data _null_;
      set sashelp.vtable(keep=nobs libname memname);
      where libname = 'COUNTY' and memname in(%upcase("&outcnty._of"),%upcase("&outcnty._on"));
      if nobs = 0 then call symput('keepsplit',nobs);
   run;
   %if not &keepsplit %then %do;
      proc datasets library = county;
         delete &outcnty._of &outcnty._on;
      quit;
   %end;
   %else %do;
      proc sort data=county.&outcnty; by  CAS SCC; run;
      proc sort data=county.&outcnty._on; by  CAS SCC; run;
      proc sort data=county.&outcnty._of; by  CAS SCC; run;
      proc summary data=county.&outcnty sum;
         by CAS;
         var emis;
         output out=mobcas(drop=_type_ _freq_) sum=;
      run;
      proc summary data=county.&outcnty._on sum;
         by CAS;
         var emis;
         output out=oncas(drop=_type_ _freq_) sum=onemis;
      run;
      proc summary data=county.&outcnty._of sum;
         by CAS;
         var emis;
         output out=noncas(drop=_type_ _freq_) sum=nonemis;
      run;
      data qamob;
         merge mobcas oncas noncas;
         by cas;
      run;
      proc print data=qamob label split='*';
         label emis= 'Total*Mobile' onemis='Onroad' nonemis='Nonroad';
         title "County-level SCC emission summaries AFTER extracting Airports (and/or other sources)";
         title2 "Mobile CAS-level Emissions [tpy] by Total, Onroad, and Nonroad";
         sum emis onemis nonemis;
      run;
   %end;
********************************************************************************************;
   
%END;

%MEND AddToOrg;
********************************************************************************************;
********************************************************************************************;

***    MAIN PROGRAM ***;

OPTIONS mprint symbolgen;


%GetInfo

%MACRO PROC;
   %GetAirp

   %IF &num_files %THEN %DO;
      %AddToPt
   %END;

   %AddToOrg
%MEND PROC;
%PROC

Run;
