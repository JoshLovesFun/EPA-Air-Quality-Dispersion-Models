********************************************************************************;
* Program: PtFinal_ISCST3 - UNIX Version                                        ;
* Date: March 19, 1999                                                          ;
* Authors: Bill Battye, Diane Linderman, and Steve Fudge                        ;
*                                                                               ;
* This program performs these basic functions:                                  ;
*    GroupSet - This portion of the program assigns sources into groups used    ;
*               in the SO pathway.  The groups can be assigned by source type   ;
*               (major, area, nonroad, or onroad), MACT category, or by either  ;
*               SCCs, SICs, or both used together in a hierarchical scheme.     ;
*               The user controls which assignment procedure is used as well as ;
*               which assignment file(s) is used.                               ;
*    FugDef   - Assigns each source as either a volume (fugitives and horizontal;
*               stacks), area (must be already assigned as such), or point (all ;
*               remaining sources in the input data set) for ISCST3.            ;
*    MergPart - Obtains particle data (diameter, density, etc...) by saroad and ;
*               SCC-level data if available.                                    ;
*    MergGasD - Obtains gas deposition/scavenging data by saroad.               ;
*    GridCell - Determines what modeling gridcell the each source is in, and it ;
*               only keeps sources within the modeling domain.                  ;
*    MergElev - Merges source elevation data with the emissions inventory.  The ;
*               source elevation data is by gridcell, and is an optional input. ;
*               The user also has the option of entering an elevation value that;
*               will be used for all sources.                                   ;
*    SOPath   - Writes the SO pathway section of the ISC runstreams and writes  ;
*               all ISCST3 include files for the SO pathway section.            ;
*                                                                               ;
*  The user provides the names of directories, input and output SAS datasets,   ;
*  and external reference text files.                                           ;
*                                                                               ;
*  Directory names:                                                             ;
*      in_data - input SAS data set directory                                   ;
*      outdata - output SAS data set directory                                  ;
*      reffiles - reference text file directory                                 ;
*  All or some fo these directories can be the same.  All must be asigned even  ;
*  if they will not be used.                                                    ;
*                                                                               ;
*  Input and Output SAS dataset names:                                          ;
*     insas - input SAS data set name                                           ;
*     outsas - output SAS data set name                                         ;
*                                                                               ;
*  GroupSet function controls and input:                                        ;
*     controls:                                                                 ;
*        DoSource - set to 1 to assign groups by source type, set to 0 to skip  ;
*        DoMACT - set to 1 to assign groups by MACT categories, set to 0 to skip;
*        DoSCC - set to 1 to assign groups by SCC, set to 0 to skip.            ;
*        DoSIC - set to 1 to assign groups by SIC, set to 0 to skip.            ;
*     Can not select DoMACT and either DoSCC or DoSIC, can select DoSCC and     ;
*     DoSIC.  If DoSCC and DoSIC are selected, the assignment of group is done  ;
*     by SCC first and then by SIC, but only for those records where the group  ;
*     has not already been set.                                                 ;
*                                                                               ;
*     input files:                                                              ;
*        MACTgrp - name of text file containing the groups by MACT categories.  ;
*        SCCgrp - name of text file containing the groups by SCCs.              ;
*        SICgrp - name of text file containing the groups by SICs.              ;
*                                                                               ;
*  MergPart function controls and input:                                        ;
*     control:                                                                  ;
*        SCAVENG  - Set to yes(1) if you want to use scavenging coefficients    ;
*                   that may be included in DEFPART and/or SCCPART files.       ;
*                                                                               ;
*     input files:                                                              ;
*        Defpart  - default particle distribution file, set to "NONE" to skip.  ;
*        SCCPART  - SCC-specific particle distribution file, set to "NONE" to   ;
*                   skip.                                                       ;
*                                                                               ;
*  MergGasD function input:                                                     ;
*     input file:                                                               ;
*        DEFGAS   - file that contains default gas deposition parameters, set to;
*                   "NONE" to skip.                                             ;
*                                                                               ;
*  GridCell function controls:                                                  ;
*        X_ORIG   - UTMX (meters) of southwest corner of domain.                ;
*        Y_ORIG   - UTMY (meters) of southwest corner of domain.                ;
*        CELLSIZE - Length in meters of each side of each grid cell.            ;
*        MAXCOL   - Number of columns in domain, eastern boundary of domain is  ;
*                   equal to X_ORIG + (CELLSIZE*MAXCOL) + CELLSIZE.             ;
*        MAXROW   - Number of rows in domain, northern boundary of domain is    ;
*                   equal to Y_ORIG + (CELLSIZE*MAXROW) + CELLSIZE.             ;
*                                                                               ;
*  MergElev function controls and input:                                        ; 
*  Function controls and input:                                                 ;
*     control:                                                                  ;
*        DEFELEV  - Default elevation in meters, used only if ELEVDAT equals    ;
*                   "NONE".                                                     ;
*     input file:                                                               ;
*        ELEVDAT  - Gridcell elevation data in meters.                          ;
*                                                                               ;
*  SOPath function controls:                                                    ;
*     controls:                                                                 ;
*        PARTMETH - Write particle distrubution include files: 1 = particle data;
*                   by source, 2 = particle data by pollutant.                  ;
*        USEBLDG  - Set to "yes" to call the macro that writes the building     ;
*                   dimension include files.                                    ;
*        RUN_ID   - EMS-HAP model run identifier, assigned to preserve unique   ;
*                   source ID's for multiple EMS-HAP model runs across similar  ;
*                   source categories.                                          ;
*        OUTNAME  - Prefix for the file containing the SO pathway for ISCST3.   ;
*                                                                               ;
********************************************************************************;
;

%MACRO GetInfo;

********************************************************************;
* MACRO: GetInfo                                                    ;
*    This macro reads a user control input file to get the names of ;
* the directories and file names to be used and variables to be used;
* throughout the program.                                           ;
*    First read the MODEL variable and based on this value, read the;
* cooresponding variables or stop processing if the value of MODEL  ;
* is not either ASPEN or ISC.                                       ;
********************************************************************;
%global model;

%let Model    = %sysget(MODEL);

%IF (%upcase("&model") = "ISC") %THEN %DO;

   %global insas outsas run_id
           DoSource DoMACT MACTgrp DoSCC SCCgrp DoSIC SICgrp DfltGrp
           partmeth usebldg defgas scaveng
           defpart sccpart elevdat defelev
           x_orig y_orig cellsize maxcol maxrow
           outfiles outname numpoll;

   %let run_id = %sysget(RUN_ID);

   %let insas = %sysget(INSAS);
   %let outsas = %sysget(OUTSAS);

   %let DoSource = %sysget(DOSOURCE);

   %let DoMACT = %sysget(DOMACT);
   %let MACTgrp = %sysget(MACTGRP);

   %let DoSCC = %sysget(DOSCC);
   %let SCCgrp = %sysget(SCCGRP);

   %let DoSIC = %sysget(DOSIC);
   %let SICgrp = %sysget(SICGRP);

   %let DfltGrp = %sysget(DFLTGRP);

   %let defpart = %sysget(DEFPART);
   %let sccpart = %sysget(SCCPART);
   %let scaveng = %sysget(SCAVENG);

   %let defgas  = %sysget(DEFGAS);

   %let elevdat = %sysget(ELEVDAT);
   %let defelev = %sysget(DEFELEV);
   %let usebldg = %sysget(USEBLDG);

   %let x_orig = %sysget(X_ORIG);
   %let y_orig = %sysget(Y_ORIG);
   %let cellsize = %sysget(CELLSIZE);
   %let maxcol = %sysget(MAXCOL);
   %let maxrow = %sysget(MAXROW);

   %let partmeth = %sysget(PARTMETH);

   %let outfiles = %sysget(OUTFILES);
   %let outname = %sysget(OUTNAME);

*** Set directories ***;

   Libname in_data "$IN_DATA";
   Libname outdata "$OUTDATA";

   Filename reffiles "$REFFILES";

%END;

%ELSE %DO;
   put @1 'Error!!! Model designation is incorrect.';
   STOP;
%END;


%MEND GetInfo;

**************************************************************;
**************************************************************;


%MACRO GroupSet;

*************************************************************************;
* MACRO: GroupSet                                                        ;
*    This macro establishes source groups in a point source emissions    ;
* inventory file.  The source groups are assigned by source type, MACT   ;
* category code, using an ASCII file defining groups by MACT category, or;
* by SCC, using an ASCII file defining groups by SCC.                    ;
*    The default value for the source category group is 000 if no match  ;
* is found.  ASPEN will be more efficient if only groups 000 through 009 ;
* are used                                                               ;
*************************************************************************;
;

data GetGroup(keep=site_id saroad emrelpid src_type MACT scc sic) inventry;
   set in_data.&insas;

data GetGroup;
   set GetGroup;
   length group $2.;
   group = '';
run;

*** If user chooses, group sources by Source (major or area) ***;
%If &DoSource %then %do;
   data GetGroup; *nogroup;
      set GetGroup;

      if compress(lowcase(src_type)) = 'major' then group = '00';
      else if compress(lowcase(src_type)) = 'area' then group = '01';
      else if compress(lowcase(src_type)) = 'onroad' then group = '02';
      else if compress(lowcase(src_type)) = 'nonroad' then group = '03';

%end;

*** User chooses to group sources by MACT category ***;
%If &DoMACT %then %do;
   proc sort data=GetGroup;
      by MACT;

*** Read MACT category to group correspondance file ***;
   Data MACTgrp;
      Infile reffiles("&MACTgrp..txt");
      Input @1 MACT $7. @9 MACT_grpM $2. @12 MACT_grpA $2.;

*** Assign group based on MACT category ***;
   proc sort data=MACTgrp;
      by MACT;

   data GetGroup(drop=MACT_grpM MACT_grpA);
      merge MACTgrp(in=A) GetGroup(in=B);
      by MACT;
      if B;
      if compress(MACT_grpM) ne '' then do;
         if compress(lowcase(src_type)) = 'area' then group = MACT_grpA;
         else group = MACT_grpM;  *ONroad segments will only have group in MACT_grpM field;
      end;
%end;

*** User chooses to group emissions by SCC and or SIC ***;
%if &DoSCC or &DoSIC %then %do;
   %If &DoSCC and &DoSIC %then %do;
      %let tmpfiles = SCCgroup SICgroup;
   %end;
   %Else %do;
      %If &DoSCC %then %do;
        %let tmpfiles = SCCgroup;
      %end;
      %Else %do;
         %If &DoSIC %then %do;
            %let tmpfiles = SICgroup;
         %end;
      %end;
   %end;

*** User chooses to use SCCs to assign group ***;
   %If &DoSCC %then %do;

*** Read SCC to group correspondance file ***;
      data SCCgroup;
         Infile reffiles("&SCCgrp..txt");
         %If &DoSIC %then %do;
            input @1 SCC $10. @12 SCC_grpM $2. @15 SCC_grpA $2. @18 SCCrank 2.;
         %end;
         %Else %do;
            input @1 SCC $10. @12 SCC_grpM $2. @15 SCC_grpA $2.;
         %end;

*** Assign group based on SCC ***;
      proc sort data=SCCgroup; by SCC; run;
      proc sort data=GetGroup; by SCC; run;
      data GetGroup;
         length SCC $10;
         merge SCCgroup(in=A) GetGroup(in=B);
         by SCC;
         if B;
         if compress(SCC_grpM) ne '' then do;
           if compress(lowcase(src_type)) = 'area' then group = SCC_grpA;
           else group = SCC_grpM;  *ONroad segments will only have group in SCC_grpM field;
         end;
      run;
   %end;

*** If user chooses, group emissions by SIC, this can be done after SCC assignment ***;
***         with or without replacing the SCC assignments                          ***;
   %If &DoSIC %then %do;

*** Read SIC to group correspondance file ***;
      data SICgroup;
         Infile reffiles("&SICgrp..txt");
         %If &DoSCC %then %do;
            input @1 SIC $4. @6 SIC_GrpM $2. @9 SIC_GrpA $2. @12 SICrank 2.;
         %end;
         %Else %do;
            input @1 SIC $4. @6 SIC_GrpM $2. @9 SIC_GrpA $2.;
         %end;

*** Assign group based on SIC, but do not override SCC group assignments ***;
***     unless the SICrank is lower than the SCCrank                     ***;
      proc sort data=SICgroup; by SIC;
      proc sort data=GetGroup; by SIC;

      data GetGroup;
         merge SICgroup(in=A) GetGroup(in=B);
         by SIC;
         if B ;
*** Changes group already assigned by SCC if SIC is ranked lower or assigned group is blank ***;
*** OR Assigns group by SIC because no group was assigned by SCC ***;
         %If &DoSCC %then %do;
            if ((compress(SIC_grpM) ne '' and SICrank < SCCrank) or compress(group) eq '') then do;
               if compress(lowcase(src_type)) = 'area' then group = SIC_grpA;
               else group = SIC_grpM;
         %end;
         %Else %do;
            if compress(SIC_grpM) ne '' then do;
              if compress(lowcase(src_type)) = 'area' then group = SIC_grpA;
              else group = SIC_grpM;  *ONroad segments will only have group in SIC_grpM field;
            end;
         %end;
      run;
   %end;
%end;

*** Assign ungrouped sources to the default value set by the user ***;
data GetGroup;
   set GetGroup;
   if compress(group) = '' then group = "&DfltGrp";
*   keep site_id pollcode emrelpid group;

*** merge temporary file containing group assignment into inventory ***;
proc sort data=GetGroup;
   by site_id saroad emrelpid;

proc sort data=inventry;
   by site_id saroad emrelpid;

data inventry; *outdata.&outsas;
   merge inventry(in=A) GetGroup(keep=site_id saroad emrelpid group);
   by site_id saroad emrelpid;
   if A;
run;

***  QA  ****;
data QA;
   set GetGroup;
   if compress(group) ne '';
   keep src_type MACT scc sic group;

proc sort data=QA nodups;
   by src_type MACT scc sic;

proc print data=QA;
  title 'Group Assignment QA';
run;

*** delete temporary files used in this macro ***;
proc datasets library=work;
   delete GetGroup
%if &DoSCC or &DoSIC %then %do;
   &tmpfiles
%end;;
quit;


%MEND GroupSet;

********************************************************************;

%MACRO NumObs(dsn);

********************************************************************;
* MACRO: NumObs
*   This macro will check to see if the dataset passed in as a
*   parameter is empty or not.
********************************************************************;
;
%global num;

data _null_;
   if 0 then set &dsn nobs=count;
   call symput('num', left(put(count,8.)));
   stop;
run;

%MEND NumObs;


*************************************************************;
*************************************************************;

%MACRO FugDef;

********************************************************************;
* MACRO: FugDef                                                    ;
*   This macro determines whether or not a variable called ISCTYPE
*   exists on the incoming dataset.  If not, then it is created and
*   given a value of ISCVOLUME for fugitive and horizontal stack
*   sources (release types of 01 and 03).  All other sources are labeled
*   as ISCPOINT.  If ISCTYPE already exists, then its contents are
*   overwritten only if it is blank.
*
*   This macro also assigns default parameters to volume sources
*   that were not already labeled as a volume source in the incoming
*   dataset.
********************************************************************;
;

*.... Determine if ISCTYPE exists as a variable on the incoming dataset;
;
data _null_;
   %global varexist;
   rc = OPEN("inventry");
   i = VARNUM(rc, "isctype");
   if i = 0 then call symput('varexist', '0');
   else call symput('varexist', '1');
run;


data inventry;
   set inventry;

*..... variable ISCTYPE does not exist;
   if &varexist = '0' then do;
      if emrelpty = '01' or emrelpty = '03' then do;
         isctype = 'ISCVOLUME';
         if stackht = 0 or stackht = . then volhgt = 2;
         else volhgt = stackht;
         sigmay = 1.5;
         sigmaz = 1.5;
      end;
      else isctype = 'ISCPOINT';
   end;

*...... variable ISCTYPE does exist.  If it is empty, then fill it with a value.;
   else do;
      if length(compress(isctype)) = 1 then do;
         if emrelpty = '01' or emrelpty = '03' then do;
            isctype = 'ISCVOLUME';
            if stackht = 0 or stackht = . then volhgt = 2;
            else volhgt = stackht;
            sigmay = 1.5;
            sigmaz = 1.5;
          end;
          else isctype = 'ISCPOINT';
      end;
   end;

run;


%MEND FugDef;

********************************************************;
********************************************************;

%MACRO MergPart;

*************************************************************;
* MACRO: MergPart
*   This macro merges particle distribution data with the
* emissions inventory.  If available, scc-specific data is
* merged, otherwise, a pollutant level default is used.
************************************************************;
;

%global numcat;
%if %upcase("&DEFPART") ne "NONE" and %quote(&DEFPART) ne  %then %do;

*... Read the default particle distribution file;
;
data part_def(drop=i);

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

*... First, merge the default data by pollutant;
;
proc sort data=inventry out=sortdat;
   by saroad;
run;

data inventry;
   length saroad $5.;
   merge sortdat(in=a) part_def(in=b);
   by saroad;
   if a;
run;

*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete sortdat part_def;
   quit;

%end;


*... If provided, read the SCC-specific particle distribution file;
;

%if %upcase("&SCCPART") ne "NONE" and %quote(&SCCPART) ne %then %do;

data part_scc(drop=i);

   array pdias(10) pdias1-pdias10;
   array pfras(10) pfras1-pfras10;
   array pdens(10) pdens1-pdens10;
   array pliqs(10) pliqs1-pliqs10;
   array pices(10) pices1-pices10;

   Infile REFFILES("&sccpart..txt");
   input saroad $ scc $ numcat @;
   do i = 1 to numcat;
      input pdias(i) @;
   end;
   do i = 1 to numcat;
      input pfras(i) @;
   end;
   do i = 1 to numcat;
      input pdens(i) @;
   end;
   %if &scaveng %then %do;
      do i = 1 to numcat;
         input pliqs(i) @;
      end;
      do i = 1 to numcat;
         input pices(i) @;
      end;
   %end;
    input;

   call symput('numcat', left(put(numcat,3.)));
run;

proc sort data=part_scc;
   by saroad scc;
run;


*... Second, merge scc-level data (if present) by pollutant and scc;
;
   proc sort data=inventry out=sortdat2;
      by saroad scc;
   run;

   data partmrg2;
      length saroad $5.;
      merge sortdat2(in=a) part_scc(in=b);
      by saroad scc;
      if a then output;
   run;

*....... Use the particle data from the scc-level file if it merged with a record;
;
   data inventry(drop=i);
      set partmrg2;
      array pdia(10) pdia1-pdia10;
      array pfra(10) pfra1-pfra10;
      array pden(10) pden1-pden10;
      array pdias(10) pdias1-pdias10;
      array pfras(10) pfras1-pfras10;
      array pdens(10) pdens1-pdens10;
      array pliq(10) pliq1-pliq10;
      array pice(10) pice1-pice10;
      array pliqs(10) pliqs1-pliqs10;
      array pices(10) pices1-pices10;

      do i = 1 to numcat;
         if (pdias(i) ne .) then pdia(i) = pdias(i);
         if (pfras(i) ne .) then pfra(i) = pfras(i);
         if (pdens(i) ne .) then pden(i) = pdens(i);
         if (pliqs(i) ne .) then pliq(i) = pliqs(i);
         if (pices(i) ne .) then pice(i) = pices(i);
      end;

   drop pdias1-pdias10 pfras1-pfras10 pdens1-pdens10 pliqs1-pliqs10 pices1-pices10;
   run;

*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete partmrg2 sortdat2 part_scc;
   quit;

%end;

%MEND MergPart;


*************************************************************;
*************************************************************;

%MACRO MergGasD;

*************************************************************;
* MACRO: MergGasD;
*   This macro merges gas deposition/scavenging data with the
* emissions inventory.
************************************************************;
;

%if %upcase("&DEFGAS") ne "NONE" and %quote(&DEFGAS) ne %then %do;

*... Read the default gas distribution file;
;
data gas_def;
   length saroad $5.;

   Infile REFFILES("&defgas..txt") firstobs = 2 missover;
   input saroad $ diff alpha rx rsubm henry liqscav;
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

*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete sortgas gas_def;
   quit;

%end;

%MEND MergGasD;


*************************************************************;
*************************************************************;

%MACRO GridCell;

*************************************************************;
* MACRO: GridCell
*   This macro determines what modeling gridcell the source
*   is in, and it only keeps sources within the modeling domain.
*   Information about the modeling grid is required.
*   This consists of the origin of the grid (in UTM meters),
*   size of each gridcell (in meters), and the number of
*   columns and rows in the modeling domain.
************************************************************;
;
data inventry;
   set inventry;

   col = INT( (utmx - &x_orig) / &cellsize ) + 1;
   row = INT( (utmy - &y_orig) / &cellsize ) + 1;
   cell = put(((col * 1000) + row),z6.);
*...... drop the observation if it is outside the modeling grid;
   if ( (utmx < &x_orig) or (col > &maxcol) or (utmy < &y_orig) or (row > &maxrow) ) then delete;

run;
*** JANUARY 2003: SUM over all unique sites if SCC-level particle distribution data is NOT used;
%if "&SCCPART" eq "NONE" or %quote(&SCCPART) eq %then %do;
   proc contents data=inventry out=x(keep=name) noprint;
   run;
   data _null_;
      set x end=last;
      where lowcase(name) not contains 'emis' and lowcase(name) not in('site_id','emrelpid','saroad','fips','group');
      call symput('idvar'||left(_n_),trim(name));
      if last then call symput('numids',_n_);
   run;
   proc sort data=inventry; by saroad group fips site_id emrelpid; run;
   proc summary data=inventry sum;
      id %do I=1 %to &numids; &&idvar&I %end;;
      by saroad group fips site_id emrelpid;
      var emis temis1-temis288;
      output out=inventry(drop=_type_ _freq_) sum=;
   run;
%end;

%MEND GridCell;


*************************************************************;
*************************************************************;

%MACRO MergElev;

*************************************************************;
* MACRO: MergElev
*   This macro merges source elevation data with the emissions
*   inventory.  The source elevation data is by gridcell, and
*   is an optional input.  The user also has the option of
*   entering an elevation value that will be used for all
*   sources.
************************************************************;
;

*... If available, merge the gridcell elevation data;
;
%if %upcase("&elevdat") ne "NONE" and %quote(&elevdat) ne %then %do;

*....... Read the gridcell elevation file (text);
   data elev;
      Infile REFFILES("&elevdat..txt") firstobs=2;
      input @1 col 3. row 3. selev 4.;
      cell = PUT(col, z3.) || PUT(row, z3.);
      drop col row;
   run;

   proc sort data=elev;
      by cell;
   run;

   proc sort data=inventry out=sortinv;
      by cell;
   run;

   data inventry;
      merge sortinv(in=a) elev(in=b);
      by cell;
      if a;
   run;

*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete elev sortinv;
   quit;

%end;


%else %then %do;

*..... Otherwise, use the user supplied default;
;

   data inventry;
      set inventry;
      selev = &defelev;
   run;

%end;


%MEND MergElev;


*************************************************************;
*************************************************************;

%MACRO Partsrc;

*************************************************************;
* MACRO: Partsrc                                              ;
*   This macro writes the include files for source specific
*   particle distribution data.
*************************************************************;
;

proc sort data=inventry out=sortsrc(keep=srcid saroad pdia1-pdia&numcat
       pfra1-pfra&numcat pden1-pden&numcat pliq1-pliq&numcat pice1-pice&numcat) nodupkey;
   by saroad srcid;
run;

data _null_;
   array pdia(10) pdia1-pdia10;
   array pfra(10) pfra1-pfra10;
   array pden(10) pden1-pden10;
   array pliq(10) pliq1-pliq10;
   array pice(10) pice1-pice10;

   set sortsrc;
   by saroad srcid;

   %do i = 1 %to &numpoll;
      if saroad = "&&saroad_&i" then do;
         file "&&partf&i";
      end;
   %end;

  if pdia1 then do;
      put @1 'SO PARTDIAM ' srcid $8. +1 @;
      do j = 1 to &numcat while (pdia{j} ne .);
         put pdia{j}  7.4 +1 @;
      end;
      put;
   end;
   if pfra1 then do;
      put @1 'SO MASSFRAX ' srcid $8. +1 @;
      do j = 1 to &numcat while (pfra{j} ne .);
         put pfra{j}  7.4 +1 @;
      end;
      put;
   end;
   if pden1 then do;
      put @1 'SO PARTDENS ' srcid $8. +1 @;
      do j = 1 to &numcat while (pden{j} ne .);
         put pden{j}  7.4 +1 @;
      end;
      put;
   end;
   if pliq1 then do;
      put @1 'SO PARTSLIQ ' srcid $8. +1 @;
      do j = 1 to &numcat while (pliq{j} ne .);
         put pliq{j}  e10.4 +1 @;
      end;
      put;
   end;
   if pice1 then do;
      put @1 'SO PARTSICE ' srcid $8. +1 @;
      do j = 1 to &numcat while (pice{j} ne .);
         put pice{j}  7.4 +1 @;
      end;
      put;
   end;
run;

*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete sortsrc;
   quit;

%MEND Partsrc;


*************************************************************;
*************************************************************;

%MACRO Partpoll;

*************************************************************;
* MACRO: Partpoll                                              ;
*   This macro writes the include files for pollutant specific
*   particle distribution data.
*************************************************************;
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
/* Following conditional loops added 9/27 by Rich for ISCST format */

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

*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete sortpoll;
   quit;

%MEND Partpoll;


*************************************************************;
*************************************************************;

%MACRO GasDepos;

*************************************************************;
* MACRO: GasDepos                                            ;
*   This macro writes the include files for pollutant specific
*   gas deposition and scavenging data.
*************************************************************;
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


*************************************************************;
*************************************************************;

%MACRO Bldgparm;

*************************************************************;
* MACRO: Bldgparm                                              ;
*   This macro writes the building dimension include files.
*************************************************************;
;
proc sort data=inventry out=sortbld(keep=saroad srcid bldh bldw isctype) nodupkey;
   by saroad srcid;
run;

data _null_;
   set sortbld;
   by saroad srcid;

   %do i = 1 %to &numpoll;
      if saroad = "&&saroad_&i" then do;
         file "&&bldgf&i";
      end;
   %end;

   if ISCTYPE = "ISCPOINT" then do;
      if bldh then do;
         bldhgt = compress(put(bldh,8.3));
         put @1 'SO BUILDHGT ' srcid $8. +1 '36*' bldhgt;
      end;
      if bldw then do;
         bldwidth = compress(put(bldw,8.3));
         put @1 'SO BUILDWID ' srcid $8. +1 '36*' bldwidth;
      end;
   end;

*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete sortbld;
   quit;

%MEND Bldgparm;


*************************************************************;
*************************************************************;

%MACRO SOPath;

*************************************************************;
* MACRO: SOPath                                              ;
*   This macro writes the SO pathway section of the ISCST3
*   runstreams.
*************************************************************;
;

*.... Assign ISC source ids;
;
proc sort data = inventry;
   by saroad group;

data inventry(drop=srccnt);
   set inventry;
   by saroad group;

   if first.group then srccnt = 0;

   srccnt + 1;
   srcid = put(int(group),z2.) || compress("&run_id") || PUT(srccnt, z5.);
run;

proc sort data=inventry out=pollist(keep=saroad) nodupkey;
   by saroad;

*** Change area locations from center to southwest corner.  By doing this
    operation AFTER the GridCell and MergElev macros, airports, landfills,
    are not dropped if the southwest corner is shifted out of the model
    domain, and elevation data is taken at the CENTER of the source, not
    the SW corner of the source.  Onroad Segments, have varying angles of
    rotation about their SW corners and therefore, their coordinates should
    be given AT the SW corner, NOT in the center.  ***;;

data inventry(drop=pi);
   retain pi 3.14159265;
   array temis(288) temis1 - temis288;
   set inventry;

   if upcase(isctype) = 'ISCAREA' and compress(lowcase(src_type)) ne 'onroad' then do;
      *aylen is not a required variable. Assign aylen = axlen if aylen = . ; 
      if aylen eq . then aylen = axlen;
      *** JUN2003: non-road segements are not already at SW corner -account for aangle!! **;
      if aangle not in(.,0) then do;
         utmx= utmx - ((sin(aangle*pi/180)*aylen/2)+cos(aangle*pi/180)*axlen/2);
         utmy= utmy - ((-sin(aangle*pi/180)*axlen/2)+cos(aangle*pi/180)*aylen/2);
      end;
      else do;
      *dyl - changed criteria so that it does not depend on the group designation;
         utmx = utmx - 0.5 * axlen;
         utmy = utmy - 0.5 * aylen;
      end;
   end;

*** DYL - moved conversion of emission values to grams/sec and created ***;
***       permanent output SAS dataset                                 ***;
*** Convert hourly emissions from tons/hour to grams/sec for ISC/ST3;
***   normalize area emissions to units per meter squared (10/19/00);
*** DYL - factor converting temporally allocation emissions from tons/hour to ***;
***       grams/sec-m**2                                                      ***;
***   [2000.0lbs/ton] * [453.592 g/lb] / [3600 sec/hr]) = 907184/3600         ***;

   do j = 1 to 288;
      if upcase(isctype) = 'ISCAREA' then
         temis(j) = temis(j) * (907184/3600) * (1/(axlen*aylen));
      else
         temis(j) = temis(j) * (907184/3600);
   end;

   drop j;
run;

*.... Create macro variables containing the file names of the runstream
*.... files, the particle distribution/scavenging files, the building dimensions
*.... files, the gas deposition/scavenging files, and the hourly emissions files.;
;
data _null_;
   set pollist;
   call symput('numpoll', _n_);
   fname = "&OUTFILES.&OUTNAME" || saroad || compress("&run_id") ||".inp";
   call symput('rfil'||left(_n_), fname);
   partname = "&OUTFILES" || 'particle_'|| compress("&run_id") ||'.'|| saroad;
   call symput('partf'||left(_n_), partname);
   bldgname = "&OUTFILES" || 'bldgdim_'|| compress("&run_id") ||'.'|| saroad;
   call symput('bldgf'||left(_n_), bldgname);
   gasdname = "&OUTFILES" || 'gasdepo_'|| compress("&run_id") ||'.'|| saroad;
   call symput('gasdf'||left(_n_), gasdname);
   ename = "&OUTFILES" || 'hrlyemis_'|| compress("&run_id") ||'.'|| saroad;
   call symput('emisfl'||left(_n_), ename);
   call symput('saroad_'||left(_n_), saroad);
run;

*.... Create an ISC runstream file for each pollutant;
;
proc sort data=inventry out=runinv(keep=saroad isctype group srcid);
   by saroad group srcid;

proc sort data=runinv out=run2inv(keep=saroad isctype group) nodupkey;
   by saroad group isctype;

data solist1;
   set runinv;
   by saroad group srcid;
   length buffer $70;

   if first.group then firstsrc = srcid;
   if last.group then do;
      lastsrc = srcid;
      keyword = 'SRCGROUP';
      buffer = 'CAT' || put(int(group),z2.) || '  ' || compress(firstsrc) || '-' || compress(lastsrc);
      output;
   end;

   retain firstsrc lastsrc;
   keep keyword buffer saroad;

data solist2;
   set run2inv;
   by saroad group isctype;
   length buffer $70 name1 $4;

   if upcase(isctype) = 'ISCAREA' then name1 = 'area';
   if upcase(isctype) = 'ISCPOINT' then name1 = 'pnt';
   if upcase(isctype) = 'ISCVOLUME' then name1 = 'vol';
   keyword = 'INCLUDED';
   buffer = "&OUTFILES" || compress(name1) || put(int(group),z2.) || compress("&run_id") || '.' || saroad; 
   keep keyword buffer saroad;

proc sql noprint;
   create table solist 
   as select * from solist1 
   outer union corr
   select * from solist2
   order by saroad,keyword,buffer
;
quit;

*.... write the runstream files;
;

data _null_;
   length partfile $70 bldgfile $70 gasdfile $70;
   set solist;
   by saroad keyword buffer;

   %do i = 1 %to &numpoll;
      if saroad = "&&saroad_&i" then do;
         file "&&rfil&i";
         partfile = "&&partf&i";
         bldgfile = "&&bldgf&i";
         emisfile = "&&emisfl&i";
         gasdfile = "&&gasdf&i";
      end;
   %end;

   if first.saroad then do;
      put @1 'SO STARTING';
      put @1 'SO ELEVUNIT  METERS';
   end;

   buffer1 = left(buffer);
   if keyword ne 'SRCGROUP' then
   put @1 'SO ' keyword +2 buffer1;

   if keyword eq 'SRCGROUP' then do;
      if first.keyword then do;
         %if "&defpart" ne "NONE" and %quote(&defpart) ne and (&partmeth = 1 or &partmeth = 2) %then %do;
            put @1 "SO INCLUDED   " partfile;
         %end;
         %if &usebldg %then %do;
            put @1 "SO INCLUDED   " bldgfile;
         %end;
         put @1 "SO INCLUDED   " emisfile;     *....Added 06/01/01 by Rich;
         %if "&defgas" ne "NONE" and %quote(&defgas) ne %then %do;
            put @1 "SO INCLUDED   " gasdfile;
         %end;
         put @1 'SO CONCUNIT  1.0E6   GRAMS/SEC   MICROGRAMS/M**3';
         put @1 'SO DEPOUNIT  3.6E3   GRAMS/SEC   GRAMS/M**2';
      end;
      put @1 'SO ' keyword +2 buffer1;
   end;

   if last.saroad then do;
      put @1 'SO SRCGROUP  ALL';
      put @1 'SO FINISHED';
   end;
run;

*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete pollist runinv run2inv solist solist1 solist2;
   quit;

*++++++++ Write the hourly emissions include file ++++++++++++++++;
;

proc sort data=inventry out=tempemis(keep=temis1-temis288 saroad srcid axlen aylen isctype);
   by saroad srcid;
run;


data _null_;
   set tempemis;
   array temis(288) temis1-temis288;

   %do i = 1 %to &numpoll;
      if saroad = "&&saroad_&i" then do;
         file "&&emisfl&i";
      end;
   %end;

*........ Winter, Spring, Summer, Fall Weekdays; **Incorrect prior to JAN 2002;
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

*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete tempemis;
   quit;

*++++++ Split the inventory into ISC point, area, and volume sources ++++++;
;
data iscpnt iscarea iscvol notype;
   set inventry;
   if upcase(isctype) = 'ISCPOINT' then output iscpnt;
      else if upcase(isctype) = 'ISCAREA' then output iscarea;
         else if upcase(isctype) = 'ISCVOLUME' then output iscvol;
            else output notype;
run;

title 'Warning!!! Observations with no ISCTYPE assigned';
proc print data=notype;
   var fips emrelpid site_id isctype;
run;
title ;


*.... Only process ISC point sources if the point dataset is nonempty;
;
%NumObs(iscpnt);
%let obscnt = &num;
%if &obscnt ne 0 %then %do;


*+++++++++++++++++++++++++++++++++++++++++++++++++++++;
* Process ISC point sources
*+++++++++++++++++++++++++++++++++++++++++++++++++++++;

*.... create an array of macro variables to hold the file names of the
*.... point source include files;
;
proc sort data=iscpnt out=pfile(keep=saroad group) nodupkey;
   by saroad group;
run;

data _null_;
   set pfile;
   call symput('numpfil', _n_);
   pollgrp = put(int(group),z2.) || saroad;
   call symput('pgrp'||left(_n_), pollgrp);
   fname = "&OUTFILES" || 'pnt' || put(int(group),z2.) || compress("&run_id") || '.' || saroad;
   call symput('pfil'||left(_n_), fname);
run;


proc sort data=iscpnt out=sortpnt;
   by saroad group;
run;

*... Write all of the emissions include files;
;
data _null_;
   set sortpnt;
   by saroad group;

   %do i = 1 %to &numpfil;
      if put(int(group),z2.)||saroad = "&&pgrp&i" then do;
         file "&&pfil&i";
      end;
   %end;
   utmx = utmx - &x_orig;  *5/24/01 -Rich, set SW corner to origin;
   utmy = utmy - &y_orig;  *2/8/01 - per Roger Brode advice: ISCST3 uses only 6 significant digits;

   put @1 'SO LOCATION ' srcid $8. ' POINT ' utmx 10. +1 utmy 10. +1 selev 10.;
   put @1 'SO SRCPARAM ' srcid $8. ' 1.0 ' stackht 10.3 +1 stktemp 10.3 +1 stackvel 10.3 +1 stackdia 10.3;
run;

*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete iscpnt pfile sortpnt;
   quit;

%end;

*.... Only process ISC area sources if the area dataset is nonempty;
;
%NumObs(iscarea);
%let obscnt = &num;
%if &obscnt ne 0 %then %do;


*+++++++++++++++++++++++++++++++++++++++++++++++++++++;
* Process ISC area sources
*+++++++++++++++++++++++++++++++++++++++++++++++++++++;

*.... create an array of macro variables to hold the file names of the
*.... area source include files;
;
proc sort data=iscarea out=afile(keep=saroad group) nodupkey;
   by saroad group;
run;

data _null_;
   set afile;
   call symput('numafil', _n_);
   pollgrp = put(int(group),z2.) || saroad;
   call symput('agrp'||left(_n_), pollgrp);
   fname = "&OUTFILES" || 'area' || put(int(group),z2.) || compress("&run_id") || '.' || saroad;
   call symput('afil'||left(_n_), fname);
run;

proc sort data=iscarea out=sortarea;
   by saroad group;
run;

*... Write all of the emissions include files;
;
data _null_;
   set sortarea;
   by saroad group;

   %do i = 1 %to &numafil;
      if put(int(group),z2.)||saroad = "&&agrp&i" then do;
         file "&&afil&i";
      end;
   %end;
   utmx = utmx - &x_orig;  *5/24/01 -Rich, set SW corner to origin;
   utmy = utmy - &y_orig;  *2/8/01 - per Roger Brode advice: ISCST3 uses only 6 significant digits;

   put @1 'SO LOCATION ' srcid $8. ' AREA ' utmx 10. +1 utmy 10. +1 selev 10.;
   put @1 'SO SRCPARAM ' srcid $8. ' 1.0 ' arelhgt 10.3 +1 axlen 10.3 +1 aylen 10.3 +1 @;
   ** ISCST3 will attempt to use ainplum if aangle is not present;
   if aangle ne . then do;
      put aangle 10.3 +1 ainplum 10.3 @;
   end;
   put;
run;

*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete iscarea afile sortarea;
   quit;

%end;

*.... Only process ISC volume sources if the volume dataset is nonempty;
;
%NumObs(iscvol);
%let obscnt = &num;
%if &obscnt ne 0 %then %do;


*+++++++++++++++++++++++++++++++++++++++++++++++++++++;
* Process ISC volume sources
*+++++++++++++++++++++++++++++++++++++++++++++++++++++;

*.... create an array of macro variables to hold the file names of the
*.... volume source include files;
;
proc sort data=iscvol out=vfile(keep=saroad group) nodupkey;
   by saroad group;
run;

data _null_;
   set vfile;
   call symput('numvfil', _n_);
   pollgrp = put(int(group),z2.) || saroad;
   call symput('vgrp'||left(_n_), pollgrp);
   fname = "&OUTFILES" || 'vol' || put(int(group),z2.) || compress("&run_id") || '.' || saroad;
   call symput('vfil'||left(_n_), fname);
run;

proc sort data=iscvol out=sortvol;
   by saroad group;
run;

*... Write all of the emissions include files;
;
data _null_;
   set sortvol;
   by saroad group;

   %do i = 1 %to &numvfil;
      if put(int(group),z2.)||saroad = "&&vgrp&i" then do;
         file "&&vfil&i";
      end;
   %end;
   utmx = utmx - &x_orig;  *5/24/01 -Rich, set SW corner to origin;
   utmy = utmy - &y_orig;  *2/8/01 - per Roger Brode advice: ISCST3 uses only 6 significant digits;

   put @1 'SO LOCATION ' srcid $8. ' VOLUME ' utmx 10. +1 utmy 10. +1 selev 10.;
   put @1 'SO SRCPARAM ' srcid $8. ' 1.0 ' volhgt 10.3 +1 sigmay 10.3 +1 sigmaz 10.3;
run;
*** delete temporary files used in this macro ***;

   proc datasets library=work;
      delete iscvol vfile sortvol;
   quit;

%end;


*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
* Output particle distribution include files
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
;

*... Write all of the particle distrubution/scavenging include files;
;

%IF "&defpart" ne "NONE" and %quote(&defpart) ne %THEN %DO;
   *... particle data by source;
   %if &partmeth = 1 %then %do;
      %Partsrc
   %end;

   *... particle data by pollutant;
   %if &partmeth = 2 %then %do;
      %Partpoll
   %end;
%END;


*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
* Output building dimension include files (if needed)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
;

%if &usebldg %then %do;
   %Bldgparm
%end;

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
* Output gas deposition/scavenging include files
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;
;

%if "&defgas" ne "NONE" and %quote(&defgas) ne %then %do;
   %GasDepos
%end;
title2 "Contents of Output Final SAS dataset &outsas.";
proc contents data = inventry out=checkvar; run;
title2;

data _null_;
   set checkvar end=endchk;
   if _N_ = 1 then do;
      GFset=0;
   end;
   Retain GFset;
   if trim(lowcase(name)) = 'gf' then GFset = 1;
   if endchk then do;
       call symput('GF',trim(GFset));
   end;
run;

data outdata.&outsas;
    %IF &GF %then %DO;
    label temis1="GC Emissions*Hr1 Winter Weekday [g/s*m2]"
         temis25="GC Emissions*Hr1 Spring Weekday [g/s*m2]"
         temis49="GC Emissions*Hr1 Summer Weekday [g/s*m2]"
         temis73="GC Emissions*Hr1 Autumn Weekday [g/s*m2]"
         temis97="GC Emissions*Hr1 Winter Saturday [g/s*m2]"
         temis121="GC Emissions*Hr1 Spring Saturday [g/s*m2]"
         temis145="GC Emissions*Hr1 Summer Saturday [g/s*m2]"
         temis169="GC Emissions*Hr1 Autumn Saturday [g/s*m2]"
         temis193="GC Emissions*Hr1 Winter Sunday [g/s*m2]"
         temis217="GC Emissions*Hr1 Spring Sunday [g/s*m2]"
         temis241="GC Emissions*Hr1 Summer Sunday [g/s*m2]"
         temis265="GC Emissions*Hr1 Autumn Sunday [g/s*m2]"
    ;
    %END;
    %ELSE %DO;
   label temis1="Base Emissions*Hr1 Winter Weekday [g/s*m2]"
         temis25="Base Emissions*Hr1 Spring Weekday [g/s*m2]"
         temis49="Base Emissions*Hr1 Summer Weekday [g/s*m2]"
         temis73="Base Emissions*Hr1 Autumn Weekday [g/s*m2]"
         temis97="Base Emissions*Hr1 Winter Saturday [g/s*m2]"
         temis121="Base Emissions*Hr1 Spring Saturday [g/s*m2]"
         temis145="Base Emissions*Hr1 Summer Saturday [g/s*m2]"
         temis169="Base Emissions*Hr1 Autumn Saturday [g/s*m2]"
         temis193="Base Emissions*Hr1 Winter Sunday [g/s*m2]"
         temis217="Base Emissions*Hr1 Spring Sunday [g/s*m2]"
         temis241="Base Emissions*Hr1 Summer Sunday [g/s*m2]"
         temis265="Base Emissions*Hr1 Autumn Sunday [g/s*m2]"
    ;
    %END;

  set inventry;
run;

%MEND SOPath;


*************************************************************;
*************************************************************;

%MACRO DoProc;

*************************************************************;
* MACRO: DoProc                                              ;
*   This macro determines which portions of the program are  ;
* run based on the user input.                               ;
*************************************************************;

*** Assign source groups (bins) ***;
;
   %If &DoSource or &DoMACT or &DoSCC or &DoSIC %then %do;

      %GroupSet

   %end;

*** Default fugitive sources to ISC volume sources ***;
;
   %FugDef;

*** Merge particle distribution data with the inventory ***;
;
   %MergPart

*** Merge gas deposition/scavenging data with the inventory ***;
;
   %if &scaveng %then %do;
      %MergGasD
   %end;

*** Compute gridcell of all sources ***;
;
   %GridCell

*** Merge source elevation data with the inventory ***;
;
   %MergElev

*** Write SO pathway section of the ISC runstream ***;
;
   %SOPath


%MEND DoProc;

*************************************************************;
*************************************************************;


***   MAIN PROGRAM ***;

OPTIONS mprint symbolgen;

%GetInfo

%DoProc

run;
