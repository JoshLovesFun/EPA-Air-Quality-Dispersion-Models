********************************************************************************;
* Program: AMFinalFormat - UNIX Version                                         ;
* Date: June 5, 2000                                                            ;
* Authors: Steve Fudge                                                          ;
*                                                                               ;
* This program performs these basic functions:                                  ;
*    GetInfo  - Obtains macro variables from batch file and collapses inventory ;
*               down to each gridcell and emission bin for each pollutant.      ;
*    MergPart - This macro merges particle distribution data with the emissions ;
*               inventory.  If available, scc-specific data is merged, otherwise;
*               a pollutant level default is used.                              ;
*    MergGasD - This macro merges gas deposition/scavenging data with the       ;
*               emissions inventory.                                            ;
*    MergElev - This macro merges source elevation data with the emissions      ;
*               inventory.  The source elevation data is by gridcell, and is an ;
*               optional input.  The user also has the option of entering an    ;
*               elevation value that will be used for all sources.              ;
*    SoPath   - This macro writes the SO pathway section of the ISC runstreams  ;
*               and writes the following include files: gas/particle deposition,;
*               building parameters, hourly emissions, and gridded area sources.;
*                                                                               ;
*  The user provides the names of directories, input and output SAS datasets,   ;
*  and external reference text files.                                           ;
*                                                                               ;
*  Modified March 2004 to allow census tracts to become ISC area sources.       ;
*  new ancillary file called tract_vertices is used to list the census tract    ;
*  vertices in UTM coordinates as ISC area sources.                             ;
*  In ISC, this is the AREAPOLY SO card                                         ;
*                                                                               ;
*  Directory names:                                                             ;
*      in_data - input SAS data set directory                                   ;
*      outdata - output SAS data set directory                                  ;
*      reffiles - reference text file directory                                 ;
*  All or some of these directories can be the same.  All must be assigned even ;
*  if they will not be used.                                                    ;
*                                                                               ;
*  Input and Output SAS dataset names:                                          ;
*     insas - input SAS data set name                                           ;
*     outsas - output SAS data set name, created by SoPath module               ;
*                                                                               ;
********************************************************************************;
;

%MACRO GetInfo;

*****************************************************************************;
* MACRO: GetInfo                                                             ;
*    This macro reads a user control input file to get the directories, file ;
* names, and variables to be used throughout the program. Also collapses the ;
* inventory to gridcell and emission bin for each pollutant.                 ;
*****************************************************************************;

%GLOBAL model;
%let Model = %sysget(MODEL);
data _null_;
 %IF (%upcase("&model") = "ISCTRACT") %THEN %DO;

   %global scaveng insas outsas run_id outcode
           defpart defgas elevdat defelev OUTFILES 
           numpoll arelhgt aangle ainplum tractfile ref_zone maxvert;

   %let run_id = %sysget(RUN_ID);
   %let insas = %sysget(INSAS);
   %let outsas = %sysget(OUTSAS);

   %let defpart = %sysget(DEFPART);
   %let defgas  = %sysget(DEFGAS);
   %let scaveng = %sysget(SCAVENG);
   %let elevdat = %sysget(ELEVDAT);
   %let defelev = %sysget(DEFELEV);
   %let x_orig = %sysget(X_ORIG);
   %let y_orig = %sysget(Y_ORIG);
   %let outfiles = %sysget(OUTFILES);
   %let tractfile = %sysget(TRACTFILE);
   %let ref_zone = %sysget(REF_ZONE);
   %let arelhgt = %sysget(ARELHGT);
   %let aangle = 0;  *JUNE 2002: Hard-code angle of rotation *;
   %let ainplum = %sysget(AINPLUM);
 %END;

 %ELSE %DO;
   %global scaveng insas outsas run_id outcode 
           defpart defgas elevdat defelev OUTFILES x_orig y_orig cellsize
           maxcol maxrow numpoll arelhgt aangle ainplum;

   %let run_id = %sysget(RUN_ID);
   %let insas = %sysget(INSAS);
   %let outsas = %sysget(OUTSAS);

   %let defpart = %sysget(DEFPART);
   %let defgas  = %sysget(DEFGAS);
   %let scaveng = %sysget(SCAVENG);
   %let elevdat = %sysget(ELEVDAT);
   %let defelev = %sysget(DEFELEV);
   %let x_orig = %sysget(X_ORIG);
   %let y_orig = %sysget(Y_ORIG);
   %let cellsize = %sysget(CELLSIZE);
   %let maxrow = %sysget(MAXROW);
   %let maxcol = %sysget(MAXCOL);
   %let outfiles = %sysget(OUTFILES);
   %let arelhgt = %sysget(ARELHGT);
   %let aangle = 0;  *JUNE 2002: Hard-code angle of rotation *;
   %let ainplum = %sysget(AINPLUM);
 %END;

Run;

*** Set directories ***;

Libname in_data "$IN_DATA";
Libname outdata "$OUTDATA";
Filename reffiles "$REFFILES";

%put _USER_;

*.... Convert emissions from to grams/sec-m**2 and output;
%IF (%upcase("&model") = "ISCTRACT") %THEN %DO;
* get maximum number of vertices from tract file;
  proc summary data=reffiles.&tractfile.;
    var nvert;
    output out=maxvert(drop=_type_ _freq_)max=maxvert;
  run;

  data _null_;
    set maxvert;
    call symput('maxvert',left(trim(maxvert)));
  run;

  proc print data=maxvert;
    var maxvert;
    title "Maximum number of vertices in tract vertices file";
  run;

* read in tract vertices file;
* and convert lat/lons to UTM coordinates based on UTM zone from batch file;
  data vertices(keep=cell nvert cellsize x1-x&maxvert. y1-y&maxvert.);
    array lon{&maxvert.} lon1-lon&maxvert.;
    array lat{&maxvert.} lat1-lat&maxvert.;
    array x{&maxvert.} x1-x&maxvert.;
    array y{&maxvert.} y1-y&maxvert.;
    set reffiles.&tractfile.;
    k0=0.9996;
    a=6378206.4;
    e2=0.00676866;
    ep2=0.0068148;
    false_e=500000.0;
    dtr=3.141592654/180.0;
    utmz=&ref_zone.;
    do bb = 1 to &maxvert.;
      dl = dtr*(lon{bb} - (6.0*utmz-183.0));
      p = dtr*lat{bb};
      sinp = sin(p);
      N = a/sqrt(1.0-e2*sinp*sinp);
      tanp = tan(p);
      T = tanp*tanp;
      cosp = cos(p);
      C = ep2*cosp*cosp;
      A1 = dl*cosp;
      M = 111132.0894*lat{bb} - 16216.94*sin(2.0*p) + 17.21*sin(4.0*p)
        - 0.02*sin(6.0*p);
      A2 = A1*A1;
      A3 = A2*A1;
      A4 = A2*A2;
      A5 = A4*A1;
      A6 = A4*A2;
      T2 = T*T;
      utmx = 0.001*(k0*N*(A1+(1.0-T+C)*A3/6.0
        + (5.0-18.0*T+T2+72.0*C-58.0*ep2)*A5/120.0)
        + false_e);
      utmy = 0.001*k0*(M+N*tanp * (A2/2.0
        + (5.0-T+9.0*C+4.0*C*C)*A4/24.0
        + (61.0-58.0*T+T2+600.0*C-330.0*ep2)*A6/720.0));
      x{bb} = (utmx*1000)-&x_orig.;
      y{bb} = (utmy*1000)-&y_orig.;
   end;
  run;
 
  proc sort data=in_data.&insas out=inemis;
   by cell;
  run;

  data inventry;
    array tEmisA{288} tEmis1-tEmis288;
    array x{&maxvert.} x1-x&maxvert.;
    array y{&maxvert.} y1-y&maxvert.;
     merge inemis(in=a) vertices(in=b);
     by cell;
     if a and b then output;
  run;

  data inventry;
    array tEmisA{288} tEmis1-tEmis288;
    array x{&maxvert.} x1-x&maxvert.;
    array y{&maxvert.} y1-y&maxvert.;
     set inventry;
*** DYL - factor converting temporally allocation emissions from tons/hour to ***;
***       grams/sec-m**2                                                      ***;
*** Factor = [2000.0lbs/ton] * [453.592 g/lb] / [3600 sec/hr]) = 251.995      ***;
*** DYL - factor converting annual emissions from tons/year to grams/sec-m**2 ***;
*** Factor = [2000.0lbs/ton] * [453.592 g/lb] / [8760 hr/yr] * [3600 sec/hr]) = 0.287666 ***;
   label emis='Base Emis [g/s**m2]'
         emis_tpy='Base Emis [tpy]';
   emis_tpy = emis;
   emis = emis * 0.0287666 /cellsize;
   %do i = 1 %to 288;
      tEmisA{&i} = tEmisA{&i} * 251.995 /cellsize;
   %end;
  run;
%END;

%ELSE %DO;
    
data inventry;
   array tEmisA{288} tEmis1-tEmis288;
   label emis='Base Emis [g/s**m2]'
         emis_tpy='Base Emis [tpy]';
   set in_data.&insas;

*** DYL - factor converting temporally allocation emissions from tons/hour to ***;
***       grams/sec-m**2                                                      ***;
*** Factor = [2000.0lbs/ton] * [453.592 g/lb] / [3600 sec/hr]) = 251.995      ***;
*** DYL - factor converting annual emissions from tons/year to grams/sec-m**2 ***;
*** Factor = [2000.0lbs/ton] * [453.592 g/lb] / [8760 hr/yr] * [3600 sec/hr]) = 0.287666 ***;

   emis_tpy = emis;
   emis = emis * 0.0287666 / (&cellsize.) ** 2;
   %do i = 1 %to 288;
      tEmisA{&i} = tEmisA{&i} * 251.995 / (&cellsize.) ** 2;
   %end;
run;
%END;
%MEND GetInfo;

**************************************************************;
**************************************************************;


%MACRO MergPart;

************************************************************;
* MACRO: MergPart                                           ;
*   This macro merges particle distribution data with the   ;
* emissions inventory.  If available, scc-specific data is  ;
* merged, otherwise, a pollutant level default is used.     ;
************************************************************;
;


%global numcat;


%if %upcase("&DEFPART") ne "NONE" and %quote(&DEFPART) ne %then %do;

*... Read the default particle distribution file;
;
data part_def(drop = i);

   length saroad $5.;
   
   array pdia(10) pdia1-pdia10;
   array pfra(10) pfra1-pfra10;
   array pden(10) pden1-pden10;
   array pliq(10) pliq1-pliq10;
   array pice(10) pice1-pice10;

   Infile REFFILES("&defpart..txt") firstobs = 2 missover;
   input saroad $ numcat @;
   do i = 1 to numcat;
      input pdia(i) @;
   end;
   do i = 1 to numcat;
      input pfra(i) @;
   end;
   do i = 1 to numcat;
      input pden(i) @;
   end;
   %if &scaveng %then %do;
      do i = 1 to numcat;
         input pliq(i) @;
      end;
      do i = 1 to numcat;
         input pice(i) @;
      end;
   %end;
   input;

   call symput('numcat', left(put(numcat,3.)));
run;

proc sort data=part_def;
   by saroad;
run;

*... Merge the default data by pollutant;
;
proc sort data=inventry out=sortdat;
   by saroad;
run;

data inventry;
   merge sortdat(in=a) part_def(in=b);
   by saroad;
   if a;
run;

%end;


%MEND MergPart;


%MACRO MergGasD;
*************************************************************;
* MACRO: MergGasD                                            ;
*   This macro merges gas deposition/scavenging data with the;
* emissions inventory.                                       ;
*************************************************************;
;

%if %upcase("&DEFGAS") ne "NONE" and %quote(&DEFGAS) ne %then %do;

*... Read the default gas distribution file;
;
data gas_def;
   length saroad $5.;

   Infile REFFILES("&defgas..txt") firstobs = 2 missover;
   input saroad $ diff alpha rx rsubm henry %if &scaveng %then %do; liqscav %end;;
run;

proc sort data=gas_def;
   by saroad;
run;

*... First, merge the default data by pollutant;
;
proc sort data=inventry out=sortgas;
   by saroad;
run;

data inventry;
   length saroad $5.;
   merge sortgas(in=a) gas_def(in=b);
   by saroad;
   if a;
run;

%end;

%MEND MergGasD;

*************************************************************;
*************************************************************;

%MACRO MergElev;

**************************************************************;
* MACRO: MergElev                                             ;
*   This macro merges source elevation data with the emissions;
*   inventory.  The source elevation data is by gridcell, and ;
*   is an optional input.  The user also has the option of    ;
*   entering an elevation value that will be used for all     ;
*   sources.                                                  ;
**************************************************************;
;

*... If available, merge the gridcell elevation data;
;
%if %upcase("&elevdat") ne "NONE" and %quote(&elevdat) ne %then %do;

*....... Read the gridcell elevation file (text);
  %IF (%upcase("&model") = "ISCTRACT") %THEN %DO;
   data elev;
      length cell $11.;
      Infile REFFILES("&elevdat..txt") firstobs=2;
      input @1 cell $11. selev 4.;
   run;
  %END;
  %ELSE %DO;
   data elev;
      Infile REFFILES("&elevdat..txt") firstobs=2;
      input @1 col 3. row 3. selev 4.;
      cell = PUT(col, z3.) || PUT(row, z3.);
      drop col row;
   run;
  %END;

   proc sort data=elev;
      by cell;

   proc sort data=inventry out=sortinv;
      by cell;

   data inventry;
      merge sortinv(in=a) elev(in=b);
      by cell;
      if a;
   run;

%end;


%else %then %do;

*..... Otherwise, use the user supplied default;

   data inventry;
      set inventry;
      selev = &defelev; *change z to selev 3/30/04 JAT;
   run;

%end;

%MEND MergElev;

*************************************************************;
*************************************************************;


%MACRO Partpoll;

**************************************************************;
* MACRO: Partpoll                                             ;
*   This macro writes the include files for pollutant specific;
*   particle distribution data.                               ;
**************************************************************;
;
proc sort data=inventry out=sortpoll(keep=saroad srcid pdia1-pdia&numcat
       pfra1-pfra&numcat pden1-pden&numcat pliq1-pliq&numcat pice1-pice&numcat) nodupkey;
   by saroad srcid;
run;

data _null_;
   array pdia(10) pdia1-pdia10;
   array pfra(10) pfra1-pfra10;
   array pden(10) pden1-pden10;
   array pliq(10) pliq1-pliq10;
   array pice(10) pice1-pice10;

   set sortpoll;
   by saroad srcid;

   %do i = 1 %to &numpoll;
      if saroad = "&&saroad_&i" then do;
         file "&&partf&i";
      end;
   %end;

   if first.saroad then firstid = srcid;
   if last.saroad then do;
      lastid = srcid;
/* Following conditional loops added 9/29 by Rich for ISCST format */

     if pdia1 then do;
         put @1 'SO PARTDIAM ' firstid $8. '-' lastid $8. +1 @;
         do j = 1 to &numcat while (pdia{j} ne .);
            put pdia{j}  7.4 +1 @;
         end;
         put;
      end;
      if pfra1 then do;
         put @1 'SO MASSFRAX ' firstid $8. '-' lastid $8. +1 @;
         do j = 1 to &numcat while (pfra{j} ne .);
            put pfra{j}  7.4 +1 @;
         end;
         put;
      end;
      if pden1 then do;
         put @1 'SO PARTDENS ' firstid $8. '-' lastid $8. +1 @;
         do j = 1 to &numcat while (pden{j} ne .);
            put pden{j}  7.4 +1 @;
         end;
         put;
      end;
      if pliq1 then do;
         put @1 'SO PARTSLIQ ' firstid $8. '-' lastid $8. +1 @;
         do j = 1 to &numcat while (pliq{j} ne .);
            put pliq{j}  e10.4 +1 @;
         end;
         put;
      end;
      if pice1 then do;
         put @1 'SO PARTSICE ' firstid $8. '-' lastid $8. +1 @;
         do j = 1 to &numcat while (pice{j} ne .);
            put pice{j}  7.4 +1 @;
         end;
         put;
      end;
      retain firstid;
   end;
run;

%MEND Partpoll;

*************************************************************;
*************************************************************;

%MACRO GasDepos;

**************************************************************;
* MACRO: GasDepos                                             ;
*   This macro writes the include files for pollutant specific;
*   gas deposition and scavenging data.                       ;
**************************************************************;
;

proc sort data=inventry out=sortgasd(keep=saroad srcid diff alpha rx rsubm henry liqscav) nodupkey;
   by saroad srcid;
run;

data _null_;
   set sortgasd;
   by saroad srcid;

   %do i = 1 %to &numpoll;
      if saroad = "&&saroad_&i" then do;
         file "&&gasdf&i";
      end;
   %end;

   if first.saroad then firstid = srcid;
   if last.saroad then do;
      lastid = srcid;
      if diff then
      put @1 'SO GASDEPOS ' firstid $8. '-' lastid $8. +1 diff e10.4 +1 alpha 5.1 +1 rx 5.1 +1 rsubm e10.4 +1 henry e10.4;
      if liqscav then
      put @1 'SO GAS-SCAV ' firstid $8. '-' lastid $8. ' LIQ ' liqscav 7.4;
      retain firstid;
   end;
run;

*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete sortgasd;
   quit;

%MEND GasDepos;

%MACRO SOPath;

*************************************************************;
* MACRO: SOPath                                              ;
*   This macro writes the SO pathway section of the ISC      ;
*   runstreams.                                              ;
*************************************************************;
;
proc sort data = inventry; by saroad group; run;

*... Window inventory and assign ISC source IDs;
;
data inventry(drop=srccnt);
   set inventry;
   by saroad group;
  %IF (%upcase("&model") = "ISC") %THEN %DO;
*...... drop the observation if it is outside the modeling grid;
   where (substr(cell,1,3) between "001" and put(&maxcol,z3.)) and
         (substr(cell,4,3) between "001" and put(&maxrow,z3.));
   %END;
   if first.group then srccnt = 0;

   srccnt + 1;
   srcid = put(input(group,2.),z2.) || compress("&run_id") || PUT(srccnt, z5.);
run;

proc sort data=inventry out=pollist(keep=saroad) nodupkey;
   by saroad;
run;

*.... Create macro variables containing the file names of the particle
*.... distribution files, and the hourly emissions files.;
;
data _null_;
   set pollist;
   call symput('numpoll', _n_);
   partname = "&OUTFILES" || 'particle_'|| compress("&run_id") ||'.'|| saroad;
   call symput('partf'||left(_n_), partname);
   gasdname = "&OUTFILES" || 'gasdepo_'|| compress("&run_id") ||'.'|| saroad;
   call symput('gasdf'||left(_n_), gasdname);
   ename = "&OUTFILES" || 'hrlyemis_'|| compress("&run_id") ||'.'|| saroad;
   call symput('emisfl'||left(_n_), ename);
   srcgname = "&OUTFILES" || 'AMcats_'|| compress("&run_id") ||'.'|| saroad;
   call symput('srcgfl'||left(_n_), srcgname);
   call symput('saroad_'||left(_n_), saroad);
run;

*........ Write out source groups for use as ISC category names in the ISC include file *;
*++++++++ Write the hourly emissions include file ++++++++++++++++;

proc sort data=inventry out=tempemis(keep=temis1-temis288 saroad srcid group);
   by saroad group srcid;
run;

data _null_;
   set tempemis(keep=saroad group srcid);
   by saroad group;

   %do i = 1 %to &numpoll;
      if saroad = "&&saroad_&i" then do;
         file "&&srcgfl&i";
      end;
   %end;

   if first.group then firstsrc = srcid;
   if last.group then do;
      lastsrc = srcid;
      range = 'CAT'||put(input(group,2.),z2.) || '  ' || compress(firstsrc) || '-' || compress(lastsrc);
      put 'SO SRCGROUP ' range $25.;
   end;
   retain firstsrc;
run;

data _null_;
   set tempemis(drop=group);
   array temis(288) temis1-temis288;

   %do i = 1 %to &numpoll;
      if saroad = "&&saroad_&i" then do;
         file "&&emisfl&i";
      end;
   %end;

*........ Winter, Spring, Summer, Fall Weekdays; **Incorrect prior to JAN 2003;
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis1-temis8) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis9-temis16) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis17-temis24) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis25-temis32) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis33-temis40) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis41-temis48) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis49-temis56) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis57-temis64) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis65-temis72) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis73-temis80) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis81-temis88) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis89-temis96) (+1 e11.);

*........ Winter, Spring, Summer, Fall Saturdays;

   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis97-temis104) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis105-temis112) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis113-temis120) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis121-temis128) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis129-temis136) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis137-temis144) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis145-temis152) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis153-temis160) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis161-temis168) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis169-temis176) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis177-temis184) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis185-temis192) (+1 e11.);

*........ Winter, Spring, Summer, Fall Sundays;

   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis193-temis200) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis201-temis208) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis209-temis216) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis217-temis224) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis225-temis232) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis233-temis240) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis241-temis248) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis249-temis256) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis257-temis264) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis265-temis272) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis273-temis280) (+1 e11.);
   put @1 'SO EMISFACT ' srcid ' SHRDOW ' (temis281-temis288) (+1 e11.);

run;

*+++++++++++++++++++++++++++++++++++++++++++++++++++++;
* Process ISC gridded area sources
*+++++++++++++++++++++++++++++++++++++++++++++++++++++;

*.... create an array of macro variables to hold the file names of the
*.... area source include files;
;
proc sort data=inventry out=afile(keep=saroad group) nodupkey;
   by saroad group;
run;

data _null_;
   set afile;
   call symput('numafil', _n_);
   pollgrp = put(input(group,2.),z2.) || saroad;
   call symput('agrp'||left(_n_), pollgrp);
   fname = "&OUTFILES" || 'grid' || put(input(group,2.),z2.) || compress("&run_id") || '.' || saroad;
   call symput('afil'||left(_n_), fname);
run;

proc sort data=inventry out=sortarea;
   by saroad group;
run;

*... Write all of the emissions include files;
;
data _null_;
   %IF (%upcase("&model") = "ISCTRACT") %THEN %DO;
     array x{&maxvert.} x1-x&maxvert.;
     array y{&maxvert.} y1-y&maxvert.;
   %END; 
   set sortarea;
   by saroad group;

   %do i = 1 %to &numafil;
      if put(input(group,2.),z2.)||saroad = compress("&&agrp&i") then do;
         file "&&afil&i";
      end;
   %end;
   
   rel_hght = &arelhgt;
   rotate   = &aangle;
   sigma_z  = &ainplum;
   %IF (%upcase("&model") = "ISCTRACT") %THEN %DO; *3/29/04 - JAT. change for census tracts as sources;
    utmx = x1;  *5/24/01 -Rich, set SW corner to origin;
    utmy = y1;  *2/8/01 - per Roger Brode advice: ISCST3 uses only 6 significant digits;
    put @1 'SO LOCATION ' srcid $8. ' AREAPOLY ' utmx 10.2 +1 utmy 10.2 +1 selev 10.;
    put @1 'SO SRCPARAM ' srcid $8. ' 1.0 ' rel_hght 6.1 +1 nvert 4.0 @;
    if &ainplum ne . then put sigma_z 6.1;
    put @1 'SO AREAVERT ' srcid $8. x1 10.2 +1 y1 10.2 +1 x2 10.2 +1 y2 10.2 +1 x3 10.2 +1 y3 10.2 +1;
   /* the following block of code is used to streamline the ISC input file */
    yy=((nvert-3)/4.0)-int(((nvert-3)/4.0)); 
    if yy = 0.0 then do;
      do jj = 4 to (nvert-3) by 4;
        put @1 'SO AREAVERT ' srcid $8. x{jj} 10.2 +1 y{jj} 10.2 +1 x{jj+1} 10.2 +1 y{jj+1} 10.2+1 @;
        put x{jj+2} 10.2 +1 y{jj+2} 10.2+1 x{jj+3} 10.2 +1 y{jj+3} 10.2;
      end;
    end;
    else do;
     xx=int(((nvert-3)/4.0));
     do kk=1 to xx;
       put @1 'SO AREAVERT ' srcid $8. x{4*kk} 10.2 +1 y{4*kk} 10.2 +1 x{4*kk+1} 10.2 +1 y{4*kk+1} 10.2 +1 @;
       put x{4*kk+2} 10.2 +1 y{4*kk+2} 10.2 +1 x{4*kk+3} 10.2 +1 y{4*kk+3};
     end;
       put @1 @1 'SO AREAVERT ' srcid $8. @;
     if yy = 0.25 then
       put x{nvert} 10.2 +1 y{nvert} 10.2;
     else if yy = 0.5 then
       put x{nvert-1} 10.2 +1 y{nvert-1} 10.2 +1 x{nvert} 10.2 +1 y{nvert} 10.2;
     else 
       put x{nvert-2} 10.2 +1 y{nvert-2} 10.2 +1 x{nvert-1} 10.2 +1 y{nvert-1} 10.2 +1 x{nvert} 10.2 +1 y{nvert} 10.2;
    end;
    /* end streamlining block */
   %END;
   %ELSE %DO;
    utmx = utmx - &x_orig;  *5/24/01 -Rich, set SW corner to origin;
    utmy = utmy - &y_orig;  *2/8/01 - per Roger Brode advice: ISCST3 uses only 6 significant digits;
    delta_x = &cellsize;
    delta_y = &cellsize;
    put @1 'SO LOCATION ' srcid $8. ' AREA ' utmx 10. +1 utmy 10. +1 selev 10.;
    put @1 'SO SRCPARAM ' srcid $8. ' 1.0 ' rel_hght 6.1 +1 delta_x 6. +1 delta_y 6. +1 rotate 6.1 +1 @;
    if &ainplum ne . then put sigma_z 6.1 @;
    put;
   %END;
run;

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
* Output particle distribution include files
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
;

*... Write all of the particle distribution include files;
;
%IF %upcase("&DEFPART") ne "NONE" and %quote(&DEFPART) ne %THEN %DO;
   %Partpoll
%END;

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
* Output gas deposition/scavenging include files
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
;

%if %upcase("&DEFGAS") ne "NONE" and %quote(&DEFGAS) ne %then %do;
   %GasDepos
%end;

%if (%upcase("&model") = "ISCTRACT") %then %do;

data outdata.&outsas(drop=nvert cellsize x1-x&maxvert. y1-y&maxvert.);
  set inventry;
run;
%end;
%else %do;
data outdata.&outsas;
  set inventry;
run;
%end;

%MEND SOPath;

*************************************************************;
*************************************************************;


***   MAIN PROGRAM ***;

OPTIONS mprint symbolgen;

%GetInfo

*** Merge particle and Gas distribution data with the inventory ***;
;
%MergPart
%MergGasD

*** Merge source elevation data with the inventory ***;
;
%MergElev

*** Write SO pathway section of the ISC runstream ***;
;
%SOPath

run;
