********************************************************************************;
* Program: PtFinalFormat - UNIX Version                                         ;
* Date: March 19, 1999                                                          ;
*       mod 7/99-dyl                                                            ;
*                assign ASPEN group (bin) on src_type (nonroad, onroad, area, or major);
*                simplify writing ASPEN input file                              ;
*                produce single ASCII file of ASPEN input emissions data        ;
* Authors: Bill Battye, Diane Linderman, and Steve Fudge                        ;
*                                                                               ;
* This program performs three basic functions:                                  ;
*    GroupSet - This portion of the program assigns emissions into groups or    ;
*               bins used in the ASPEN output.  The groups can be assgined by   ;
*               source type (major or area), MACT category, or by either SCCs,  ;
*               SICs, or both used together in a hierarchical scheme.  The user ;
*               controls which assignment procedure is used as well as which    ;
*               assginement file(s) is used.                                    ;
*                                                                               ;
*    DoWrite - This portion of the program produces the ASPEN input files which ;
*              is accomplished in three macros:                                 ;
*       1) FileHead - Reads the decay rates for the nine reactivity classes,    ;
*                     opens the nine ASPEN output files, and writes the file    ;
*                     headers on each file.                                     ;
*       2) FileBody - Writes the body of the ASPEN file.                        ;
*       3) EndMark - Writes the last lines of the ASPEN file and closes the file;
*       NOTE:  It is assumed that the incoming SAS data set contains temporally ;
*              allocated emissions in tons/year.                                ;
*                                                                               ;
*    DoASCII - This portion of the program produces a single ASCII file of the  ;
*              processed emissions inventory data used in the ASPEN input files.;
*       NOTE:  If the ASPEN input files are not created at the same time, it is ;
*              asssumed that the variable assignments and the conversion of the ;
*              emissions from tons/year to grams/sec has not been done.         ;
*                                                                               ;
*  The user provides the names of directories, input and output SAS datasets,   ;
*  and external reference text files.                                           ;
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
*     outsas - output SAS data set name, created by GroupSet module             ;
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
*  DoWrite function controls and input:                                         ;
*     controls:                                                                 ;
*        DoWrite - set to 1 to produce ASPEN input files, set to 0 to skip.     ;
*                                                                               ;
*     input/output:                                                             ;
*        decay - name of the text file containing the decay rates for each      ;
*                reactivity class.  This information is written to the file     ;
*                header.                                                        ;
*                                                                               ;
*   DoASCII function controls and input:                                        ;
*     controls:                                                                 ;
*        DoASCII - set to 1 to produce single ASCII file of ASPEN input file,   ;
*                  set to 0 to skip                                             ;
*                                                                               ;
*     input/output:                                                             ;
*        ascii - name of ASCII file to be created                               ;
*                                                                               ;
*  Additional Information: uer must specify other information                   ;
*     DfltGrp - default group assignment, used only if no match is found in     ;
*               either MACT categories or SCC or SIC files.                     ;
*     outcode - name of ASPEN text files to be concatenated with the reactivity ;
*               class in final file name.                                       ;
*     itype - source type, set to 0 for point and 3 for pseudo point.           ;
*     runid - string (maximum 25 characters) used to identify the emissions on  ;
*             ASPEN text files header.                                          ;
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

%IF (%upcase("&model") = "ASPEN") %THEN %DO;

   %global insas outsas
           DoSource DoMACT MACTgrp DoSCC SCCgrp DoSIC SICgrp DfltGrp
           DoWrite decay outcode itype runid DoASCII ascii;

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

   %let DoWrite = %sysget(DOWRITE);
   %let decay = %sysget(DECAY);
   %let outcode = %sysget(OUTCODE);
   %let itype = %sysget(ITYPE);
   %let runid = %sysget(RUNID);

   %let DoASCII = %sysget(DOASCII);
   %let ascii = %sysget(ASCII);

   %put _USER_;

*** Set directories ***;

   Libname in_data "$IN_DATA";
   Libname outdata "$OUTDATA";

   Filename reffiles "$REFFILES";
   Filename outfiles "$OUTFILES";
   Filename ascifile "$ASCIIFILE";

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
* inventory file.  The source groups are assigned by srouce type, MACT   ;
* category code, using an ASCII file defining groups by MACT category, or;
* by SCC, using an ASCII file defining groups by SCC.                    ;
*************************************************************************;
;
data GetGroup(keep=site_id saroad emrelpid src_type MACT scc sic) inventry;
   set in_data.&insas;

data GetGroup;
   set GetGroup;
   length group $1.;
   group = '';
run;

*** If user chooses, group sources by Source (major or area) ***;
%If &DoSource %then %do;
   data GetGroup;
      set GetGroup;

      if compress(lowcase(src_type)) = 'major' then group = '0';
      else if compress(lowcase(src_type)) = 'area' then group = '1';
      else if compress(lowcase(src_type)) = 'onroad' then group = '2';
      else if compress(lowcase(src_type)) = 'nonroad' then group = '3';

%end;

*** User chooses to group sources by MACT category ***;
%If &DoMACT %then %do;
   proc sort data=GetGroup;
      by MACT;

*** Read MACT category to group correspondance file ***;
   Data MACTgrp;
      Infile reffiles("&MACTgrp..txt");
      Input @1 MACT $7. @9 MACT_grpM $1. @12 MACTgrpA $1.;

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
   %If &DoSCC %then %do;
      data GetGroup;
         set GetGroup;
         length scc6 $6.;
         scc6 = substr(scc,1,6);
   %end;

*** User chooses to use SCCs to assign group ***;
   %If &DoSCC %then %do;

*** Read SCC to group correspondance file ***;
      data SCCgroup;
         infile reffiles("&SCCgrp..txt");
         %If &DoSIC %then %do;
            input @1 SCC $10. @12 SCC_grpM $1. @15 SCC_grpA $1. @18 SCCrank 2.;
         %end;
         %Else %do;
            input @1 SCC $10. @12 SCC_grpM $1. @15 SCC_grpA $1.;
         %end;

*** Assign group based on SCC ***;
      proc sort data=SCCgroup; by SCC;
      proc sort data=getgroup; by SCC;

      data GetGroup;
         length SCC $10;
         merge SCCgroup(in=A) GetGroup(in=B);
         by SCC;
         if B;
         if compress(SCC_grpM) ne '' then do;
           if compress(lowcase(src_type)) = 'area' then group = SCC_grpA;
           else group = SCC_grpM;  *ONroad segments will only have group in SCC_grpM field;
         end;
   %end;

*** If user chooses, group emissions by SIC, this can be done after SCC assignment ***;
***         with or without replacing the SCC assignments                          ***;
   %If &DoSIC %then %do;

*** Read SIC to group correspondance file ***;
      data SICgroup;
         infile reffiles("&SICgrp..txt");
         %If &DoSCC %then %do;
            input @1 SIC $4. @6 SIC_GrpM $1. @9 SIC_GrpA $1. @12 SICrank 2.;
         %end;
         %Else %do;
            input @1 SIC $4. @6 SIC_GrpM $1. @9 SIC_GrpA $1.;
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

*** merge temporary file containing group assignment into inventory ***;
proc sort data=GetGroup;
   by site_id saroad emrelpid;

proc sort data=inventry;
   by site_id saroad emrelpid;

data outdata.&outsas;
   merge inventry(in=A) GetGroup;
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
   delete GetGroup inventry
%if &DoSCC or &DoSIC %then %do;
   &tmpfiles
%end;;
quit;


%MEND GroupSet;

**************************************************************;
**************************************************************;


%MACRO FileHead;

***************************************************************************;
* MACRO: FileHead                                                          ;
*    This macro reads decay rates for different reactivity classes and     ;
*    opens the ASPEN input files.                                          ;
*    Array Decay dimensioned by stability glass, reactivity and time block ;
***************************************************************************;
;

data _null_;

   Array Decay(10,8) $60 Decay1-Decay80;
   Infile reffiles("&decay..txt");

   Do j = 1 to 72;
      Input @1 Rct 1. @3 Time 1. @5
         Decay(Rct,Time) $60.;
   End;

***** Name run *****;

   IDrun = "&runid";

*** Set pollutant type code                ***;
***   Reactivity class 2, fine particles   ***;
***   Reactivity Class 3, coarse particles ***;
***   All Other Reactivity Classes, gas    ***;

   %Do React = 1 %to 9;

      if &React = '2' then do;
         Pol_type = '1';
         Dry_dep = '0';
         Wet_dep = '0';
      end;
      else if &React = '3' then do;
         Pol_type = '2';
         Dry_dep = '0';
         Wet_dep = '0';
      end;
      else do;
         Pol_type = '0';
         Dry_dep = '1';
         Wet_dep = '1';
      end;

***** Open modeling input files *****;

      File outfiles(&outcode..r&React..inp);

***** Write header for ASPEN input file *****;

      IDFile = 'File:'||"&outcode..r&React..inp"||' React.Class='||"&React ";

      Put @1 IDRun $25. IDFile $35.
         Pol_type $1. Dry_dep $1. Wet_dep $1.;

      Do j = 1 to 8;
         Put @1 Decay(&React,j) $60.;
      End;

   %End;

Run;

%MEND FileHead;

*************************************************************;
*************************************************************;


%MACRO FileBody;

**********************************************************************************;
*                                                                                 ;
* MACRO: FileBody                                                                 ;                                              ;
*  This macro writes the data to the point source input file for the ASPEN model. ;
*  Creates output dataset with same data. - DYL 4/30/01                           ;
**********************************************************************************;
;

data inventry;
   %If &DoSource or &DoMACT or &DoSCC or &DoSIC %then %do;
      set outdata.&outsas;
   %end;
   %Else %do;
      set in_data.&insas;
   %end;

   length StackID $5.;
   length PlantID $10.;

   PlantID = site_id;

   if length(emrelpid) >= 5 then StackID = substr(emrelpid,(length(emrelpid)-4),5);
   else if emrelpid = ' ' then StackID = 'AA001';
   else StackID = emrelpid;

   Itype = "&itype";


proc sort data=inventry;
   by React FIPS site_id emrelpid emrelpty saroad;

data inventry;
   set inventry end=last;
   by React FIPS site_id emrelpid emrelpty saroad;

   array tEmisA{8}  tEmis1-tEmis8;

***** Identify output file *****;

      %Do rr = 1 %to 9;
          if React = &rr then do;
              file outfiles(&outcode..r&rr..inp) mod;
          end;
      %end;

***** Write plant and stack inputs *****;

   if first.emrelpty then do;
      put @1 FIPS $5. PlantID $10. Lon 10.5 Lat 10.6 Itype $1. Uflag 1.;

*      put @1 IWBan $5. Nosc $6. Nows $6. Nowd $6.;
      put @1;         /* blank IWban gives nearest default, excluded stability classes, wind direction and speed */

      put @1;         /* polar grid distances, blank line is default */

      put @1 StackID $5. Lon 10.5 Lat 10.6 StackHt 10.4 StackDia 10.4 StackVel 10.4 StkTemp 10.2 
            Ivent $1. Ibldg $1. Bldw 10.2 Bldh 10.2; /*ram-NOV 2002: allow higher precision for stack params*/
   end;

*** Convert temporally allocated emissions from tons/year to grams/second for each 3-hour period *****;
***    factor = [2000.0lbs/ton] * [453.592 g/lb] / ( [8760 hrs/yr] * [3600 sec/hr])***;

   %do I = 1 %to 8;
      tEmisA{&I} = tEmisA{&I} * 0.0287666;
   %end;

***** Write pollutant emissions *****;

   put @1 SAROAD $5. @6 group $1.
          tEmisA{1} e10.
          tEmisA{2} e10.
          tEmisA{3} e10.
          tEmisA{4} e10.
          tEmisA{5} e10.
          tEmisA{6} e10.
          tEmisA{7} e10.
          tEmisA{8} e10.;

***** Mark end of stack grouping *****;

   if last.emrelpty then Do;
      Put;
      Put;
   end;

run;

*** Output data to final SAS dataset - DYL 4/30/01 ***;
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
    label temis1="GC Emissions [g/s]*Hrs 0-2"    temis2="GC Emissions [g/s]*Hrs 3-5"
         temis3="GC Emissions [g/s]*Hrs 6-8"    temis4="GC Emissions [g/s]*Hrs 9-11"
         temis5="GC Emissions [g/s]*Hrs 12-14"  temis6="GC Emissions [g/s]*Hrs 15-17"
         temis7="GC Emissions [g/s]*Hrs 18-20"  temis8="GC Emissions [g/s]*Hrs 21-23"
    ;
    %END;
    %ELSE %DO;
    label temis1="Base Emissions [g/s]*Hrs 0-2"    temis2="Base Emissions [g/s]*Hrs 3-5"
         temis3="Base Emissions [g/s]*Hrs 6-8"    temis4="Base Emissions [g/s]*Hrs 9-11"
         temis5="Base Emissions [g/s]*Hrs 12-14"  temis6="Base Emissions [g/s]*Hrs 15-17"
         temis7="Base Emissions [g/s]*Hrs 18-20"  temis8="Base Emissions [g/s]*Hrs 21-23"
    ;
    %END;
   set inventry;
run;

proc datasets library=work;
   delete inventry;
quit;


%MEND FileBody;

*************************************************************;
*************************************************************;


%MACRO EndMark;

*************************************************************;
* MACRO: EndMark                                             ;
*    This macro writes the end mark of ASPEN input files     ;
*************************************************************;
;

Data _null_;

   %Do React = 1 %to 9;
        file outfiles(&outcode..r&React..inp) mod;
        Put;
        Put;
   %End;

Run;

%MEND EndMark;

*************************************************************;
*************************************************************;

%MACRO ASCII2;

*************************************************************;
* MACRO: ASCII2                                              ;
*   This macro write the secondary ASCCI file.               ;
*************************************************************;
;

%If not(&DoWrite) %then %do;

   data outdata.&outsas;
      %If &DoSource or &DoMACT or &DoSCC or &DoSIC %then %do;
         set outdata.&outsas;
      %end;
      %Else %do;
         set in_data.&insas;
      %end;

      array tEmisA{8}  tEmis1-tEmis8;

      length StackID $5.;
      length PlantID $10.;

      PlantID = site_id;

      if length(emrelpid) >= 5 then StackID = substr(emrelpid,(length(emrelpid)-4),5);
      else if emrelpid = ' ' then StackID = 'AA001';
      else StackID = emrelpid;

      Itype = "&itype";

*** Convert temporally allocated emissions from tons/year to grams/second for each 3-hour period *****;
***  factor = [2000 lbs/ton] * [453.592 g/lb] / ( [8760 hrs/yr] * [3600 sec/hr] );

      %do I = 1 %to 8;
         tEmisA{&I} = tEmisA{&I} * 0.0287666;
      %end;

   proc sort data=inventry;
      by React FIPS site_id emrelpid emrelpty saroad;

%end;

data _null_;
   set outdata.&outsas;
   array tEmisA{8}  tEmis1-tEmis8;

   file ASCIfile(&ascii..txt);

   if FIPS = ' ' then FIPS = '999';
   if PlantID = ' ' then PlantID = '999';
   if lon = . then lon = 999;
   if lat = . then lat = 999;
   if Itype = ' ' then Itype = '9';
   if Uflag = . then Uflag = 0;
   if StackID = ' ' then StackID = '999';
   if StackHt = . then StackHt = 999;
   if StackDia = . then StackDia = 999;
   if StackVel = . then StackVel = 999;
   if SAROAD = ' ' then SAROAD = '999';
   if group = ' ' then group = '9';
   if tEmisA{1} = . then tEmisA{1} = '999';
   if tEmisA{2} = . then tEmisA{2} = '999';
   if tEmisA{3} = . then tEmisA{3} = '999';
   if tEmisA{4} = . then tEmisA{4} = '999';
   if tEmisA{5} = . then tEmisA{5} = '999';
   if tEmisA{6} = . then tEmisA{6} = '999';
   if tEmisA{7} = . then tEmisA{7} = '999';
   if tEmisA{8} = . then tEmisA{8} = '999';

   put @1 FIPS $5.
       +1
       PlantID $10.
       +1
       lon 10.5
       +1
       lat 8.5
       +1
       Itype $1.
       +1
       Uflag 1.
       +1
       StackID $5.
       +1
       StackHt 10.4
       +1
       StackDia 10.4
       +1
       StackVel 10.4
       +1
       StkTemp 10.2
       +1
       SAROAD $5.
       +1
       group $1.
       tEmisA{1} e10.
       tEmisA{2} e10.
       tEmisA{3} e10.
       tEmisA{4} e10.
       tEmisA{5} e10.
       tEmisA{6} e10.
       tEmisA{7} e10.
       tEmisA{8} e10.
         +1
         site_id $25.
    ;

run;


%MEND ASCII2;


*************************************************************;
*************************************************************;

%MACRO DoProc;

*************************************************************;
* MACRO: DoProc                                              ;
*   This macro determines which portions of the program are  ;
* run based on the user input.                               ;
*************************************************************;

*** Assign source groups ***;

   %If &DoSource or &DoMACT or &DoSCC or &DoSIC %then %do;

      %GroupSet

   %end;

*** Write ASPEN output files ***;

   %If &DoWrite %then %do;

      %FileHead

      %FileBody

      %EndMark

   %end;

   %If &DoASCII %then %do;

      %ASCII2;

   %end;


%MEND DoProc;

*************************************************************;
*************************************************************;


***   MAIN PROGRAM ***;

OPTIONS mprint symbolgen;

%GetInfo

%DoProc

run;
