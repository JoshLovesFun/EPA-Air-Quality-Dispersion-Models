/*******************************************************************************************
   Program: PtGrowControl
   Date: February 24, 1999 - February 2003
   Authors: Bill Battye and Diane Linderman, EC/R.  Most recently modified May 2003 by
            Rich Mason, CSC

   This program can performs four major functions:
      GROW     - adds annual growth factors for a specified year to emission inventory by
               SITE_ID and/or state fips code and: MACT and/or SIC  and/or SCC codes.
      MACTCntl - applies control efficiency variables by 1 or more files: MACT-General,
               and/or MACT-specific.
      UserCntl - applies control efficiency variables by User control file.
      AppCntl  - this portion performs three operations:
               1) adds variables flag and complyr which control when MACT controls are
                  applied.
               2) adds default and general MACT control efficiencies by MACT category, adds
                  process specific and chemical specific controls by NTI_HAP, scc8, scc6,
                  combination of scc8 and NTI_HAP, and combination of scc6 and NTI_HAP.
               3) applies the grown factors and controls efficiencies and calculates the
                  grown and controled emission values.

   The user controls which functions the program will be perform and which external reference
   files will be used.  The user also provide directory names and input and output SAS
   dataset names.  The following is a description of the user inputs:

   Model Type:
       MODEL - ASPEN or ISC

   Directory names:
       in_data - input SAS data set directory
       outdata - output SAS data set directory
       reftext - reference text files directory
    All or some of these directories can be the same.  All must be assigned even if they will
    not be used

   Input and Output SAS dataset name:
       insas - input SAS data set name
       outsas - output SAS data set name PREFIX

*******************************************************************************************
   Projection Options:
       PROJECT - comma delimited file containing all projection information.
                 Each record in this file is a distinct projection, with the
                 first projection on line 4.
*******************************************************************************************
       Variables and columns in (), with valid inputs for this spreadsheet:

       GROW function controls and input:
          input files:
             GFSITE(A)   - name of file containing annual growth factors for one year by
                         SITE_ID (text file).
             GFMACT(B)   - name of file containing annual growth factors for one year by FIPS
                         and MACT (text file).
             GFSIC(C)    - name of file containing annual growth factors for one year by FIPS
                         and SIC (text file).
             GFSCC(D)    - name of file containing annual growth factors for one year by FIPS
                         and SIC (text file).

       MACTCntl function inputs:
          input/output:
             MACTgen(E)  - name of file containing general control efficiencies, control
                         flags, and compliance year (text file). To NOT apply, set equal to
                         NONE or leave blank.
             SpecFile(F) - name of file containing control efficiencies and control flags by
                         process or chemical using NTI HAP code, 6-digit SCC, 8-digit SCC
                         and/or combinations of NTI HAP code with 6-digit SCC and/or 8-digit
                         SCC (text file) .  To NOT apply, set equal to NONE or leave blank.
       UserCntl function inputs:
          input/output:
             UserFile(G) - name of file containing control efficiencies and control flags
                         by specific facility using NTI HAP and saroad code (text file).
             Cntyur(H)   - county code assignment SAS file.  Assigns controls by county.
       AppCntl function controls and inputs:  NONE

       User must specify the following additional information:
             GrowYear(I) - year corresponding to the growth factors.
             YEARTYPE(J) - "CALENDAR" or "FISCAL".  Determines how emission reductions are
                         applied.  Choosing FISCAL begins projections on October 1 in the
                         year prior to the projected year.
             PNAME(K)    - when appended to the outsas variable in the batch file, completes
                         the SAS output filename.
             COMMENTS(L) - Used as the first line in the title for each projection scenario.

********************************************************************************************
 REFER TO THE EMS-HAP USER GUIDE (Version 3) FOR INPUT REQUIRMENTS AND FORMATS             
********************************************************************************************

   The program also uses several macros to perform general functions.  These macros and their
   functions are listed below:
      GetInfo - This macro reads the UNIX batch file and creates macro variables from
                environment variables in the batch file.  Directories names are set.  Input
                emissions file is read and the required information is separated from the
                inventory and put into a SAS data set named growcntl.  This data set is
                prepared for the processing selected by the user.  The PROJECTION comma
                delimited file is read and the rest of the program iterates once for each
                projection scenario in this file.
      FnlMerg - combines the SAS data set growcntl with the rest of the inventory to create
                an output SAS data set.
      MergMACT- merges the process and chemical specific control efficiencies into the SAS
                data set growcntl.
      MergUSER- merges the user-defined control efficiencies into the SAS data set growcntl.

*/

%MACRO GetInfo;

***************************************************************;
*** MACRO: GetInfo;  *UNIX version                             ;
*** Read User UNIX Control File and creates macro variables ***;
***  First read the MODEL variable and based on this value, ***;
***  the number of TAFs are determined.  Stop processing if ***;
***  the value of MODEL is neither ASPEN nor ISC.           ***;
***************************************************************;

%global model insas outsas numtaf project numproj;

%let Model    = %sysget(MODEL);
%let insas    = %sysget(INSAS);
%let outsas   = %sysget(OUTSAS);
%let PROJECT  = %sysget(PROJECT);

%IF (%upcase("&model") = "ASPEN") %THEN %DO;
   %let numtaf = 8;
%END;
%ELSE %IF (%upcase("&model") = "ISC") %THEN %DO;
  %let numtaf = 288;
%END;
%ELSE %DO;
   put @1 'Error!!! Model designation is incorrect.';
   STOP;
%END;

%put _USER_;

*** Set necessary directories ***;

Libname in_data '$IN_DATA';
Libname outdata '$OUTDATA';

Filename reftext '$REFTEXT';

****************************************************************************************;
*   Reads point source inventory and creates dataset containing all necessary variables ;
****************************************************************************************;
**** EMS-HAP Uses variable MACT not MACTcode as of MARCH 2003 ***;
proc contents data=in_data.&insas out=chkmact(keep=name) noprint; run;
%let renameMACT = 0;
data _null_;
   set chkmact end=last;
   where lowcase(name) = 'mactcode';
   call symput('renameMACT',last);
run;

*** Reads input pt source inventory and creates subset dataset for processing ***;

data growcntl(keep=scc sic site_id NTI_HAP FIPS MACT) inventry;
   length site_id $25.;  * may not be this length to begin with -required for merging. *;
   set in_data.&insas;

%if &renameMACT %then %do;
   MACT = compress(MACTcode);
%end;
%else %do;
   MACT = compress(MACT);
%end;

proc sort data=growcntl;
   by FIPS site_id scc sic NTI_HAP;

data growcntl;
   set growcntl;
   by FIPS site_id scc sic NTI_HAP;
   if first.NTI_HAP;
Run;

   data projinfo;
      length GFSITE $32 GFMACT $32 GFSIC $32 GFSCC $32 MACTGEN $32;
      length SPECFILE $32 USERFILE $32 CNTYUR $32 YEARTYPE $8 PNAME $16 Comments $130;
      infile reftext("&PROJECT..csv") firstobs=4 dlm=',' dsd pad lrecl=400;
      input GFSITE $ GFMACT $ GFSIC $ GFSCC $ MACTGEN $ @;
      input SPECFILE $ USERFILE $ CNTYUR $ GROWYEAR YEARTYPE $ PNAME $ Comments $;

      yeartype = upcase(yeartype);
** MAY 2003;
      if upcase(GFSITE)    eq "NONE" then GFSITE   = "";
      if upcase(GFMACT)    eq "NONE" then GFMACT   = "";
      if upcase(GFSIC)     eq "NONE" then GFSIC    = "";
      if upcase(GFSCC)     eq "NONE" then GFSCC    = "";
      if upcase(MACTGEN)   eq "NONE" then MACTGEN  = "";
      if upcase(SPECFILE)  eq "NONE" then SPECFILE = "";
      if upcase(USERFILE)  eq "NONE" then USERFILE = "";
      call symput('numproj',_n_);
   run;

%do I=1 %to &numproj;
    %global gfsite&I gfmact&I gfsic&I gfscc&I mactgen&I userfile&I
      cntyur&I yeartype&I pname&I specfile&I growyear&I comments&I;  
%end;
   data _null_;
      set projinfo;
      call symput('gfsite'||left(_n_),trim(GFSITE));
      call symput('gfmact'||left(_n_),trim(GFMACT));
      call symput('gfsic'||left(_n_),trim(GFSIC));
      call symput('gfscc'||left(_n_),trim(GFSCC));
      call symput('mactgen'||left(_n_),trim(MACTGEN));
      call symput('specfile'||left(_n_),trim(SPECFILE));
      call symput('userfile'||left(_n_),trim(USERFILE));
      call symput('cntyur'||left(_n_),trim(CNTYUR));
      call symput('yeartype'||left(_n_),trim(YEARTYPE));
      call symput('pname'||left(_n_),trim(PNAME));
      call symput('growyear'||left(_n_),growyear);
      call symput('comments'||left(_n_),trim(comments));
   run;

%MEND GetInfo;

*************************************************************;
*************************************************************;


%MACRO FnlMerg;

************************************************************************;
* MACRO: FnlMerg;                                                       ;
* This macro merges the growcntl data set containing any added variable ;
* with the rest of the inventory and creates the final output file      ;
************************************************************************;
;
%let keepvar = FIPS site_id scc sic NTI_HAP;

%If &&GFSITE&P ne or &&GFMACT&P ne or &&GFSIC&P ne or &&GFSCC&P ne %then %do;
   %let keepvar = &keepvar gf gfcode;
%END;

%IF &&MACTGEN&P ne %then %do;
   %let keepvar = &keepvar MACTXEff MACTNEff MACTRate MACT_src MACTCntl;
%END;

%IF &&USERFILE&P ne %then %do;
   %let keepvar = &keepvar UserXEff UserNEff UserRate UserCntl Replace;
%END;

proc sort data=growcntl(keep=&keepvar);
   by FIPS site_id scc sic NTI_HAP;

proc sort data=inventry;
   by FIPS site_id scc sic NTI_HAP;

****************************************************************************************;
/* Because variable "Replace" already exists IF inventory has been previously run through
   PtGrowCntl, it must be dropped before merging inventories.  If "Replace" variable is
   not dropped, only first occurrence of FIPS,site_id,scc,sic,NTI_HAP in inventory will
   undergo control reduction. (This techincally occurs only if USERFILE is used). */

proc contents data=inventry out=checkvar noprint;

%let dropvar=;
data _null_;
   set checkvar;
   if trim(lowcase(name)) = 'replace' then call symput('dropvar','replace');
run;

data outdata.&outsas.&&pname&P checkrec;
   merge inventry(in=A drop=&dropvar) growcntl(in=B);
****************************************************************************************;
   by FIPS site_id scc sic NTI_HAP;
   length CntlCode $100.;

   ExistEff = 0;
   New_Eff = 0;
   New_Rate = 0;
   AddXEff = 0;
   AddNEff = 0;
   Add_Rate = 0;
   CntlCode = "";
   if cntl_eff = . then cntl_eff = 0;

   *** Initialize replace variable if USERFILE not used **;

   %IF &&USERFILE&P eq %then %do;
      Replace = "";
   %END;
   %If &&GFSITE&P eq and &&GFMACT&P eq and &&GFSIC&P eq and &&GFSCC&P eq %then %do;
      gf = 1;
   %END;

   if A then output outdata.&outsas.&&pname&P;
   if B and not A then output checkrec;

Run;
proc print data= checkrec;
  var site_id fips mact sic scc gf ExistEff CntlCode;
  title2 "Warning: These records do not merge back in the inventory for some reason.";
  title3 "Find out why because no growth or reductions will be applied for these sites.";
run;
title2;

********************************************************************************************;
***  RESOLVE APPLICATION OF CONTROL EFFICIENCIES BETWEEN MACT CONTROLS AND USER CONTROLS ***;
********************************************************************************************;

%IF &&MACTGEN&P ne and &&USERFILE&P eq %THEN %DO;

   data outdata.&outsas.&&pname&P ;
      set outdata.&outsas.&&pname&P ;
      If (lowcase(Src_type) = 'major' and upcase(MACT_src) = 'M')
         or (lowcase(Src_type) = 'area'  and upcase(MACT_src) = 'A')
         or (upcase(MACT_src) = 'B') then do;
         ExistEff = MACTXEff;
         New_Eff = MACTNEff;
         New_Rate = MACTrate;
         CntlCode = MACTcntl;
      end;

      Else do;
         If compress(MACTcntl) ne "" then CntlCode = 'MACT not applied due to source type';
      end;

      drop MACTXEff MACTNEff MACTrate MACTcntl MACT_src;
   run;

%END;

%IF &&MACTGEN&P eq and &&USERFILE&P ne %THEN %DO;

   data outdata.&outsas.&&pname&P ;
      set outdata.&outsas.&&pname&P ;

         ExistEff = UserXEff;
         New_Eff = UserNEff;
         New_Rate = UserRate;
         CntlCode = Usercntl;
      drop UserXEff UserNEff UserRate Usercntl ;
   run;

%END;

%IF &&MACTGEN&P ne and &&USERFILE&P ne %THEN %DO;

   data outdata.&outsas.&&pname&P ;
      set outdata.&outsas.&&pname&P ;

      If (lowcase(Src_type) = 'major' and upcase(MACT_src) = 'M') or
         (lowcase(Src_type) = 'area'  and upcase(MACT_src) = 'A') or
         (upcase(MACT_src) = 'B') then do;

         ExistEff = MACTXEff;
         New_Eff = MACTNEff;
         New_Rate = MACTrate;
         CntlCode = MACTcntl;
      end;
      Else do;
         If compress(MACTcntl) ne "" then CntlCode = 'MACT not applied due to source type ';
      end;

         If compress(upcase(Replace)) = "R" then do;
            ExistEff = UserXEff;
            New_Eff = UserNEff;
            New_Rate = UserRate;
            if compress(CntlCode) = "" then CntlCode = trim(UserCntl)||' replaced no MACT applied';
            else CntlCode = trim(UserCntl)||' replaced '||trim(CntlCode);
         end;
         If compress(upcase(Replace)) = "A" then do;
            AddXEff = UserXEff;
            AddNEff = UserNEff;
            Add_Rate = UserRate;
            if compress(CntlCode) = "" then CntlCode = trim(UserCntl)||' added to no MACT applied';
            else CntlCode = trim(CntlCode)||' with additional '||trim(UserCntl);
         end;

      drop UserXEff UserNEff UserRate UserCntl 
           MACTXEff MACTNEff MACTrate MACTcntl MACT_src;
   run;

%END;

*** Delete intermediate inventory files ***;

proc datasets library=work;
   delete growcntl inventry checkrec;
Quit;


%MEND FnlMerg;

*************************************************************;
*************************************************************;


%MACRO MergMACT(eff_file= , mergvar1= ,mergvar2 = ,mergvar3= ,code= );

***************************************************************************;
* MACRO: MergMACT;                                                         ;
* This macro merges control efficiencies and codes into growncntl          ;
* Input variables;                                                         ;
*   eff_file - name of the efficiency file to merge into growcntl data set ;
*   mergvar1 - name of first variable to be used to perform the merge,     ;
*   mergvar2 - name of second variable to be used to perform the merge,    ;
*   mergvar3 -name of MACT code variable used to perform merge             ;
*   code - control code used it identify the source of the control info    ;
***************************************************************************;
;
proc contents data=&eff_file out=chkobs noprint;

data _null_;
   set chkobs (obs=1);
   call symput('notempty',nobs);
run;

%If &notempty %then %do;

    proc sort data=&eff_file;
       by &mergvar1 &mergvar2 &mergvar3;

*** Ensure that no duplicate record exist in the control file - DYL 4/10/01;

    data &eff_file dups;
       set &eff_file;
       by &mergvar1 &mergvar2 &mergvar3;
       if first.&mergvar3 then output &eff_file;
       else output dups;

    title2 "Warning: Duplicate records in &eff_file when sorted by &mergvar1 &mergvar2 and &mergvar3";
    proc print data=dups;

    proc sort data=growcntl;
       by &mergvar1 &mergvar2 &mergvar3;

    data growcntl(drop=EffXspec EffNspec snewrate sapp_src)
         EMISonly
         CNTLonly(keep= &mergvar1 &mergvar2 &mergvar3 EffXspec EffNspec snewrate sapp_src);
        merge growcntl(in=A) &eff_file (in=B);
        by &mergvar1 &mergvar2 &mergvar3;

        If A and B then do;
          MACTXEff = EffXspec;
          MACTNEff = EffNspec;
          MACTRate = snewrate;
          MACT_src = sapp_src;
          MACTCntl = "&code" ;
        End;

        If A then output growcntl;
        If A and not B then output EMISonly;
        If B and not A then output CNTLonly;
     Run;

***  CHECK FOR NON-MATCHES;
      title2 "Warning: Some MACT specific controls not found in emissions file";
      title3 "&eff_file: Reductions specified by &mergvar1 &mergvar2 and &mergvar3";
      proc print data=CNTLonly split="*";

*** Delete intermediate files ***;

      proc datasets library=work;
          delete EMISonly CNTLonly dups;
      Quit;

%End;

*** Delete intermediate files ***;

proc datasets library=work;
   delete chkobs;
Quit;

%MEND MergMACT;

*************************************************************;
*************************************************************;


%MACRO MergUSER(eff_file= , mvar1= ,mvar2 = ,mvar3= ,mvar4= ,code= );

***************************************************************************;
* MACRO: MergUSER                                                          ;
* This macro merges control efficiencies and codes into growncntl          ;
* Input variables;                                                         ;
*   eff_file - name of the efficiency file to merge into growcntl data set ;
*   mvar1 - name of first variable to be used in merge, may not be used    ;
*   mvar2 - name of second variable to be used in merge, may not be used   ;
*   mvar3 - name of third variable to be used in merge, may not be used    ;
*   mvar4 - name of fourth variable to be used in merge, must be used      ;
*   code - control code used it identify the source of the control info    ;
***************************************************************************;
;
proc contents data=&eff_file out=chkobs noprint;

data _null_;
   set chkobs (obs=1);
   call symput('notempty',nobs);
run;

%If &notempty %then %do;

*** Separate eff_file into two files, depending on whether or not the efficiency ;
***     is geographically conrolled by looking at the cntycode variable          ;

    %IF &cntycodes %THEN %DO;
       data no_cnty(drop=CntyCode) withcnty;
         set &eff_file;
         if compress(CntyCode ) = "" then output no_cnty;
         else output withcnty;
       run;

       proc contents data=no_cnty out=chkobs noprint;

       data _null_;
          set chkobs (obs=1);
          call symput('nocnty',nobs);
       run;
    %END;

*** Merge non-geographically specific control with growcntl file first ;

    %If not &cntycodes or &nocnty %then %do;

       %IF &cntycodes %THEN %DO;
          proc sort data=no_cnty;
       %END;
       %ELSE %DO;
          proc sort data=&eff_file out=no_cnty;
       %END;

          by &mvar1 &mvar2 &mvar3 &mvar4;
       run;

*** Ensure that no duplicate record exist in the control file - DYL 4/10/01;

      data no_cnty dups;
         set no_cnty;
         by &mvar1 &mvar2 &mvar3 &mvar4;
         if first.&mvar4 then output no_cnty;
         else output dups;

      title2 "Warning: Duplicate records found in the non-county-specific controls file &eff_file";
      title3 "when controls sorted by &mvar1 &mvar2 &mvar3 and &mvar4";
      proc print data=dups;

      proc sort data=growcntl;
         by &mvar1 &mvar2 &mvar3 &mvar4;

      data growcntl(drop= E_eff N_eff N_rate u_replac) EMISonly
         CNTLonly(keep=&mvar1 &mvar2 &mvar3 &mvar4 E_eff N_eff N_rate u_replac);
         merge growcntl(in=A) no_cnty (in=B);
         by &mvar1 &mvar2 &mvar3 &mvar4;

         If A and B then do;
            UserXEff = E_eff;
            UserNEff = N_eff;
            UserRate = N_rate;
            Replace = u_replac;
            UserCntl = "&code"|| "without county controls";
         End;

         If A then output growcntl;
         If A and not B then output EMISonly;
         If B and not A then output CNTLonly;

      Run;

***  CHECK FOR NON-MATCHES;
      title2 "Warning: Some non-county-specific reductions not found in emissions file";
      title3 "&eff_file: Reductions specified by &mvar1 &mvar2 &mvar3 and &mvar4";
      proc print data=CNTLonly split="*";
      run;
      *** Delete intermediate files ***;
      proc datasets library=work;
         delete dups EMISonly CNTLonly no_cnty;
      Quit;

   %End;

   %IF &cntycodes %THEN %DO;
      *** Merge geographically specific control with growcntl file;

       proc contents data=withcnty out=chkobs noprint;

       data _null_;
          set chkobs (obs=1);
          call symput('w_cnty',nobs);
       run;

       %If &w_cnty %then %do;

           proc sort data=withcnty;
              by &mvar1 &mvar2 &mvar3 &mvar4 CntyCode ;

            *** Ensure that no duplicate record exist in the control file - DYL 4/10/01;

           data withcnty dups;
              set withcnty;
              by &mvar1 &mvar2 &mvar3 &mvar4 CntyCode;
              if first.CntyCode then output withcnty;
              else output dups;
   
           title2 "Warning: Duplicate records found in county-specific control file &eff_file";
           title3 "when controls sorted by &mvar1 &mvar2 &mvar3 &mvar4, and county code";
           proc print data=dups;

           proc sort data=growcntl;
              by &mvar1 &mvar2 &mvar3 &mvar4 CntyCode ;

           data growcntl(drop= E_eff N_eff N_rate u_replac) EMISonly
              CNTLonly(keep=&mvar1 &mvar2 &mvar3 &mvar4 CntyCode E_eff N_eff N_rate u_replac);
              merge growcntl(in=A) withcnty (in=B);
              by &mvar1 &mvar2 &mvar3 &mvar4 CntyCode ;

              If A and B then do;
                 UserXEff = E_eff;
                 UserNEff = N_eff;
                 UserRate = N_rate;
                 Replace = u_replac;
                 UserCntl = "&code"||" for county "||compress(CntyCode);
              End;

              If A then output growcntl;
              If A and not B then output EMISonly;
              If B and not A then output CNTLonly;

           Run;

           ***  CHECK FOR NON-MATCHES;
           title2 "Warning: Some county-specific reductions not found in emissions file";
           title3 "&eff_file: Reductions specified by &mvar1 &mvar2 &mvar3 and &mvar4";
           title4 "Check County-types for possible mis-matches";
           proc print data=CNTLonly(obs=5) split="*";
           run;
           title4 ;
           *** Delete intermediate files ***;
           proc datasets library=work;
              delete dups EMISonly CNTLonly withcnty;
           Quit;
       %End;
   %END;
%End;

*** Delete intermediate files ***;

proc datasets library=work;
   delete chkobs;
Quit;

%MEND MergUser;


********************************************************;
********************************************************;


%MACRO GROW;

******************************************************************************************;
* MACRO: GROW                                                                             ;
* This macro merges a set of growth factors into growcntl by MACT/state and/or SIC/state. ;
* Input files                                                                             ;
*   growcntl - subset of input Point Source Inventory                                     ;
*   gfsite   - Growth factor file by SITE_ID (variable is FIPS-specific)                  ;
*   gfmact   - Growth factor file by State-county/MACT                                    ;
*   gfsic    - Growth factor file by State-county/SIC                                     ;
*   gfscc    - Growth factor file by State-county/SCC                                     ;
******************************************************************************************;
;

*** create state and county variable for merging growth factors;
data growcntl;
   set growcntl;
   length state $2 county $3 gfcode $7;

   state = substr(FIPS,1,2);
   county = substr(FIPS,3,3);
   gfcode = '';

*** Merge in Growth Factors by SITE_ID, if user has selected this ***;

%IF &&GFSITE&P ne %THEN %DO;

***  READ GROWTH FACTOR BY SITE_ID FILE  ***;
   data GrowSITE;
      infile reftext("&&GFSITE&P...txt") firstobs=3 stopover;
      input site_id $1-25 gf 27-35;
      site_id = left(site_id);

   proc sort data=GrowSITE nodupkey;  * Warning: THERE SHOULD BE NO DUPLICATES IN THIS FILE;
      by SITE_ID;

*** Check SITE_ID value in inventory emission file;

   data growcntl;
      set growcntl;
      SITE_ID = left(SITE_ID);

***  MERGE GFs INTO EMISSIONS BY SITE_ID ***;

   proc sort data=growcntl;
      by SITE_ID;

   proc sort data=GrowSITE;
      by SITE_ID;

   data growcntl(drop=gf) SITEemis;
      merge GrowSITE(in=in1) growcntl(in=in2) ;
      by SITE_ID;
      if in2;
      if gf = . then output growcntl;
      else do;
        gfcode = 'SITE ID';
        output SITEemis;
      end;
   run;
*** Delete GrowSITE intermediate files  ***;
   proc datasets;
      delete GrowSITE;
   quit;
%END;
%ELSE %DO;
   *** CREATE DUMMY DATASET OF SITE_ID GROWTH ***;
   data SITEemis;
      set _null_;
   run;
%END;

*** Merge in Growth Factors by MACT, if user has selected this ***;

%IF &&GFMACT&P ne %THEN %DO;

***  READ GROWTH FACTOR BY STATE/county/MACT FILE  ***;
   data GrowMACT;
      infile reftext("&&GFMACT&P...txt") firstobs=3 stopover;
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

   data growcntl;
      set growcntl;
      MACT = left(MACT);
*** if MACT is only three digit, make four digit with preceding zero;
      if length(MACT) = 3 then MACT = '0' || substr(MACT,1,3);

***  MERGE GFs INTO EMISSIONS BY MACT  ***;

*** First merge in national growth factors ***;
   proc sort data=growcntl;
      by MACT;

   proc sort data=national;
      by MACT;

   data growcntl;
      merge national (in=in1) growcntl (in=in2) ;
      by MACT;
      if in2;
   run;

*** Second merge in state growth factors ***;
   proc sort data=growcntl;
      by state MACT;

   proc sort data=state;
      by state MACT;

   data growcntl(drop=gfst);
      merge STATE (in=in1 rename=(gf=gfst)) growcntl(in=in2);
      by state MACT;
      if gfst ne . then gf = gfst;
      if in2;
   run;

*** Third merge in county growth factors ***;
   proc sort data=growcntl;
      by state county MACT;

   proc sort data=county;
      by state county MACT;

   data growcntl(drop=gfcty gf) MACTemis;
      merge county (in=in1 rename=(gf=gfcty)) growcntl (in=in2) ;
      by state county MACT;
      if gfcty ne . then gf = gfcty;
      if in2;
      if gf = . then output growcntl;
      else do;
         gfcode = 'MACT';
         output MACTemis;
      end;
   run;

*** Delete GrowMACT intermediate files  ***;
   proc datasets;
      delete GrowMACT national state county;
   quit;
%END;
%ELSE %DO;
   *** CREATE DUMMY DATASET OF MACT GROWTH ***;
   data MACTemis;
      set _null_;
   run;
%END;

******************************************************************;
*** Merge in Growth Factors by SIC, if user has selected this ***;
******************************************************************;
/* use 4 digit SIC instead of 2 digit JAT 5/20/04 */

%IF &&GFSIC&P ne %then %do;

   data GrowSIC;
      infile reftext("&&GFSIC&P...txt") firstobs=3 stopover;
      input state $ 1-2 county $ 4-6 sic $ 8-11 gf 13-21;
      sic = left(sic);

   proc sort data=GROWSIC nodupkey;  * Warning: THERE SHOULD BE NO DUPLICATES IN THIS FILE;
      by state county sic; **MAY2003;

   data national(keep=sic gf)
        state(keep=state sic gf)
        county;
      set GrowSIC;
      if state = '00' then output national;
      else if county = '000' then output state;
      else output county;

***  MERGE GFs INTO EMISSIONS BY SIC STATE  ***;
*** First merge in national growth factors ***;

   proc sort data=growcntl;
      by sic;

   proc sort data=national;
      by sic;

   data growcntl(drop=gfus);
      merge national (in=in1 rename=(gf=gfus)) growcntl (in=in2);
      by sic;
      if gfus ne . then gf = gfus;
      if in2 ;
   run;

*** Second merge in state growth factors ***;
   proc sort data=growcntl;
      by state sic;

   proc sort data=state;
      by state sic;

   data growcntl(drop=gfst);
      merge state (in=in1 rename=(gf=gfst)) growcntl (in=in2);
      by state sic;
      if gfst ne . then gf = gfst;
      if in2;
   run;

*** Third merge in county growth factors ***;
   proc sort data=growcntl;
      by state county sic;

   proc sort data=county;
      by state county sic;

   data growcntl(drop=gfcty gf) SICemis;
      merge county (in=in1 rename=(gf=gfcty)) growcntl (in=in2);
      by state county sic;
      if gfcty ne . then gf = gfcty;
      if in2;
      if gf = . then output growcntl;
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
%END;

%IF &&GFSCC&P ne %THEN %DO;

   data sccxref (keep=SCC remi) sccg(keep=state county gf remi);
      retain next;
      length remi $72 scc $10;
      infile reftext("&&GFSCC&P...txt") firstobs=3 lrecl=300 pad ;
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
   proc sort data=growcntl(keep=SCC) out=uniqSCC nodupkey;
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
   proc sort data=growcntl;
      by scc;

   proc sort data=national;
      by scc;

   data growcntl(drop=gfus);
      merge national (in=in1 rename=(gf=gfus)) growcntl (in=in2);
      by scc;
      if gfus ne . then gf = gfus;
      if in2;
   run;

*** Second merge in state growth factors ***;
   proc sort data=growcntl;
      by state scc;

   proc sort data=state;
      by state scc;

   data growcntl(drop=gfst);
      merge state (in=in1 rename=(gf=gfst)) growcntl (in=in2);
      by state scc;
      if gfst ne . then gf = gfst;
      if in2;
   run;

*** Third merge in county growth factors ***;
   proc sort data=growcntl;
      by state county scc;

   proc sort data=county;
      by state county scc;

   data SCCemis(drop=gfcty);
      merge county (in=in1 rename=(gf=gfcty)) growcntl (in=in2);
      by state county scc;
      if gfcty ne . then gf = gfcty;
      if in2 then do;
         gfcode = 'SCC';
         output;
      end;
   run;
   data growcntl(keep=scc sic site_id NTI_HAP FIPS MACT gf gfcode);
      set SITEemis MACTemis SICemis SCCemis;
   run;

*** Delete GrowSCC intermediate files  ***;
   proc datasets library=work;
      delete SCCemis GrowSCC sccxref sccg uniqSCC national state county;
   Quit;

%END;
%ELSE %DO;
   *** CREATE FINAL DATASET CONTAINING ALL GROWTH RECORDS ***;
   data growcntl(keep=scc sic site_id NTI_HAP FIPS MACT gf gfcode);
      set SITEemis MACTemis SICemis growcntl; *where growcntl contains the leftover records of
                                               missing GFs from SITE, MACT, and/or SIC growth *;
   run;
%END;

********************************************************;
proc datasets;
   delete SITEemis MACTemis SICemis;
quit;

*** End of assigning Growth Factors from GF file(s) ***;

%MEND GROW;

********************************************************;
********************************************************;


%MACRO MACTcntl;

**********************************************************************************;
* MACRO MACTcntl                                                                  ;
* This macro adds MACT control efficiencies and MACT compliance date information  ;
*    to growcntl and applies the controls and growth factors to calculate         ;
*    projected controlled emissions.                                              ;
* Input files:                                                                    ;
*   growcntl - subset of input Point Source Inventory                             ;
*   MACTgen -  default and average efficiences by code, and MACTcat (7-digit      ;
*              code) used if specified by user- MACTGEN ne NONE and not left blank;
*   SpecFile - process- and/or chemical-specific efficiencies used if specified   ;
*              by user- SPECFILE ne NONE and not left blank.                      ;
* Output file:                                                                    ;
*   outsas - output SAS data set containing controlled and grown emission values. ;
**********************************************************************************;

************************************************************************;
* User has selected to use the general control efficiencies by MACT code;
* Will also add MACT compliance date information (cdate);
************************************************************************;

*** Read overall category-wide efficiencies ***;
   Data MACTgen;
      Infile reftext("&&MACTgen&P...txt") firstobs=2 stopover;
      Input @1 MACT $7. @9 MACTXEff 6.2 @16 MACTNEff 6.2 @23 MACTrate 6.2
            @30 cdate mmddyy10.  @41 Apply $1. @43 MACT_src $1.;

      MACT = compress(MACT);
*** Assume controls are not in place for the entire year until the year after the compliance date ***;

***  Keep records where Apply flag is set true (1);
***  Source type is considered, but not until Final Merge (FnlMerg) Macro *;
      if Apply;
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
          put "Error: **YEARTYPE** VARIABLE in Column L of &PROJECT..csv MUST BE EQUAL TO FISCAL or CALENDAR";
          ABORT;
      %end;
   run;

   proc print data=MACTGen;
      title2 "The General MACT Reductions (not applied if keeprec=0)";
   run;

***** MARCH 2002: Use same compliance year for MACTSPEC as MACTgen **;
   data MACTGen;
      set MACTGen;
      if keeprec;
      keep MACT MACTXEff MACTNEff MACTRate MACT_Src keeprec cdate;
   run;

*** Merge general category efficiencies into inventory data, growcntl ***;

   Proc Sort Data = growcntl;
      By MACT;

   Proc Sort Data = MACTgen;
      By MACT;

   Data growcntl;
      Merge growcntl(in=A) MACTgen(in=B);
      By MACT;
      length MACTCntl $50.;

      If A and B then MACTCntl = 'General MACT Reduction';
      If A then output;
   Run;


*** QA Check - Summary of inventory records assigned with a general MACT control efficiency ***;

   data qacheck
       (keep=MACT SCC NTI_HAP MACTXEff MACTNEff MACT_src MACTRate MACTCntl);
      set growcntl;
      if compress(MACTCntl) ne "";
    run;

   title2 "Frequency of Inventory Records Assigned General MACT Reductions by MACT";
   proc freq data=qacheck;
       table MACT;

*** Delete intermediate general control efficiency files ***;

   proc datasets library=work;
      delete MACTgen qacheck;
   Quit;

**********************************************************************************;
* User has selected to use the process and chemical specific control efficiencies ;
**********************************************************************************;

%If &&SPECFILE&P ne %then %do;

*** Create appropriate variables for merging process and chemical specific effienciences ***;

   Data growcntl;
      Set growcntl;
      length scc6 $6 scc8 $8;
      scc6 = substr(left(scc),1,6);
      scc8 = compress(scc);
   Run;

*** Read process and chemical specific efficiencies ***;

   Data SpecFile;

*** File contains the SAROAD which is read, but not currently used ***;

      Infile reftext("&&SpecFile&P...txt") firstobs=2 stopover;
      Input @1 MACT $7. @9  nti_hap $3. @13 saroad $5. @20 scc8 $8. @29 Scc6 $6.
            @37 EffXspec 6.2 @44 EffNspec 6.2 @51 snewrate 6.2 @58 apply $1. @60 sapp_src $1.;

      MACT = compress(MACT);
***  Keep records where Apply flag is set true (1);
***  Source type is considered, but not until Final Merge (FnlMerg) Macro *;
     If Apply;
        keeprec = 1;

***  CHECK FOR MISSING VALUES IN THE MACT CONTROL FILE;
***  Drop records is information is missing;
        if MACT = ""
            or (EffXspec = . and EffNspec = .)
            or (nti_hap = "" and scc8 = . and scc6 = .)
            or (sapp_src = "") then do;
            put " *** Error: one of MACT EffXspec EffNspec NTI_HAP SCC8 SCC6 or SAPP_SRC is missing in specific MACT control file";
            put MACT= EffXspec= EffNspec= nti_hap= scc8= scc6= sapp_src=;
            put " This control will be dropped.";
            keeprec = 0;
        end;
   run;

   proc print data=SpecFile;
      title2 "The Specific MACT Reductions (not applied if keeprec=0)";
   run;

   data Specfile;
      set Specfile;
      if keeprec;
      drop Apply keeprec;
   run;

******** MARCH 2002: Ensure MACTgen compliance year information is used by MACTspec file *;
proc sort data=Specfile; by MACT; run;
proc sort data=growcntl(keep=MACT keeprec cdate) out=keepyear nodupkey; by MACT; run;
data Specfile(drop=keeprec);
   merge keepyear(in=A) Specfile(in=B);
   by MACT;
   if A and B and keeprec;
*****************************;
      %if %upcase(&&yeartype&P) = CALENDAR %then %do;
         if cdate le mdy(12,31,&&growyear&P) then do;
            if cdate gt mdy(12,31,&&growyear&P -1) then do;
               EffXspec= EffXspec * (mdy(12,31,&&growyear&P) - cdate) / (mdy(12,31,&&growyear&P) - mdy(12,31,&&growyear&P - 1));
               EffNspec= EffNspec * (mdy(12,31,&&growyear&P) - cdate) / (mdy(12,31,&&growyear&P) - mdy(12,31,&&growyear&P - 1));
            end;
         end;
         else do; ***  Drop records that do not meet the projection year criteria;
             keeprec = 0;
         end;
      %end;
      %else %if %upcase(&&yeartype&P) = FISCAL %then %do;
         if cdate le mdy(9,30,&&growyear&P) then do;
            if cdate gt mdy(9,30,&&growyear&P -1) then do;
               EffXspec= EffXspec * (mdy(9,30,&&growyear&P) - cdate) / (mdy(9,30,&&growyear&P) - mdy(9,30,&&growyear&P - 1));
               EffNspec= EffNspec * (mdy(9,30,&&growyear&P) - cdate) / (mdy(9,30,&&growyear&P) - mdy(9,30,&&growyear&P - 1));
            end;
         end;
         else do; ***  Drop records that do not meet the projection year criteria;
             keeprec = 0;
         end;
      %end;
*****************************;
run;
*-------------------------------------------------------------------------------------*;
   Data HAP_MACT (keep = NTI_HAP MACT EffXspec EffNspec snewrate sapp_src)
        SCC6MACT (keep = scc6 MACT EffXspec EffNspec snewrate sapp_src)
        SCC8MACT (keep = scc8 MACT EffXspec EffNspec snewrate sapp_src)
        SCC6HAP (keep = scc6 NTI_HAP MACT EffXspec EffNspec snewrate sapp_src)
        SCC8HAP (keep = scc8 NTI_HAP MACT EffXspec EffNspec snewrate sapp_src);

        set SpecFile;

*** Save records keyed only by process ***;
        If compress(NTI_HAP) = "" then do;
           If compress(scc8) ne "" then Output scc8MACT;
           If compress(scc6) ne "" and compress(scc8) = "" then Output SCC6MACT;
        End;
*** Save records keyed by both chemical and/or process ***;
        Else do;
           If compress(scc8) ne "" then Output SCC8HAP;
           If compress(scc6) ne "" and compress(scc8) = "" then Output SCC6HAP;
*** Save records keyed only by chemical ***;
           If compress(scc6) = "" and compress(scc8) = "" then Output HAP_MACT;
        End;
     Run;

***** Merge chemical-specific efficiencies *****;

     %MergMACT(eff_file=HAP_MACT,mergvar1=,mergvar2=NTI_HAP,mergvar3=MACT,code=MACT/NTI_HAP)


***** Merge process-specific efficiencies *****;

     %MergMACT(eff_file=SCC6MACT,mergvar1=,mergvar2=scc6,mergvar3=MACT,code=MACT/SCC6)
     %MergMACT(eff_file=SCC8MACT,mergvar1=,mergvar2=scc8,mergvar3=MACT,code=MACT/SCC8)


***** Merge chemical and process specific efficiencies *****;

     %MergMACT(eff_file=SCC6HAP,mergvar1=scc6,mergvar2=NTI_HAP,mergvar3=MACT,code=MACT/SCC6/NTI_HAP)
     %MergMACT(eff_file=SCC8HAP,mergvar1=scc8,mergvar2=NTI_HAP,mergvar3=MACT,code=MACT/SCC8/NTI_HAP)


*** QA Check - Summary of inventory records assigned with a specific MACT control efficiency ***;

data qacheck
     (keep=MACT SCC NTI_HAP MACTXEff MACTNEff MACT_src MACTRate MACTCntl);
   set growcntl;
   if substr(MACTCntl,1,4) eq "MACT";
run;

*** Delete intermediate process and chemical specific control files;

   proc datasets library=work;
      delete specfile HAP_MACT SCC6MACT SCC8MACT SCC6HAP SCC8HAP qacheck;
   Quit;

%end;


%MEND MACTCntl;

*********************************************************************;
*********************************************************************;


%MACRO UserCntl;

*******************************************************************************************;
* User has selected to use the facility/process and chemical specific control efficiencies ;
*******************************************************************************************;
*** Read Facility/process and chemical specific efficiencies ***;
/* change firstobs to 11 */
   data userfile;
      Infile reftext("&&userfile&P...txt") firstobs=11 stopover;
      Input @1 site_id $25. @27 MACT $7. @35 SCC $8. @44 SIC $4. @49 NTI_HAP $3.
            @53 saroad $5. @59 E_eff 6.2 @66 N_eff 6.2 @73 N_rate 6.2
            @80 Apply $1. /* @82 u_src $1. */ @84 CntyCode $5. @90 u_replac $1.;

      MACT = compress(MACT);

***  Keep records where Apply flag is set true (1);
***  Source type is considered, but not until Final Merge (FnlMerg) Macro *;
      if Apply;
         keeprec = 1;

***  CHECK FOR MISSING VALUES IN THE USER CONTROL FILE;
***  Drop records if information is missing;
         if (E_eff = . and N_eff = .)
             or (compress(SITE_ID) eq "" and compress(MACT) eq "" and
                 compress(SCC) eq "" and compress(SIC) eq "" and compress(NTI_HAP) eq "") then do;
            put " *** Error: Insufficient control information in the User controls file";
            put E_eff= N_eff=  SITE_ID= MACT= SCC= SIC=;
            put " This control will be dropped.";
            keeprec = 0;
         end;
   run;

   proc print data=userfile;
      title2 "The User Specific Reductions (not applied if keeprec=0)";
   run;

   %let cntycodes = 0; **FEB 2003: Do not make popflag file a requirement if no cntycodes exist in user file;
   data userfile;
      set userfile;
      if keeprec;
         if E_eff = . then E_eff = 0;
         if N_eff = . then N_eff = 0;
         if N_rate = . then N_rate = 0;
      drop keeprec Apply;
      if cntycode ne '' then call symput('cntycodes',1);
   run;

  Data HAPonly (keep=NTI_HAP E_eff N_eff N_rate CntyCode u_replac)
       MACTonly (keep=MACT E_eff N_eff N_rate CntyCode u_replac)
       MACTHAP (keep=MACT NTI_HAP E_eff N_eff N_rate CntyCode u_replac)
       SCConly (keep=SCC E_eff N_eff N_rate CntyCode u_replac)
       SCCHAP (keep=SCC NTI_HAP E_eff N_eff N_rate CntyCode u_replac)
       MACTSCC (keep=MACT SCC E_eff N_eff N_rate CntyCode u_replac)
       MACTSCCH (keep=MACT SCC NTI_HAP E_eff N_eff N_rate CntyCode u_replac)
       SIConly (keep=SIC E_eff N_eff N_rate CntyCode u_replac)
       SICHAP (keep=SIC NTI_HAP E_eff N_eff N_rate CntyCode u_replac)
       MACTSIC (keep=MACT SIC E_eff N_eff N_rate CntyCode u_replac)
       MACTSICH (keep=MACT SIC NTI_HAP E_eff N_eff N_rate CntyCode u_replac)
       SCCSIC (keep=SCC SIC E_eff N_eff N_rate CntyCode u_replac)
       SCCSICH (keep=SCC SIC NTI_HAP E_eff N_eff N_rate CntyCode u_replac)
       MACTALL (keep=MACT SCC SIC E_eff N_eff N_rate CntyCode u_replac)
       MACTALLH (keep=MACT SCC SIC NTI_HAP E_eff N_eff N_rate CntyCode u_replac)

       SITEonly  (keep=SITE_ID E_eff N_eff N_rate CntyCode u_replac)
       SITEMACT  (keep=SITE_ID MACT E_eff N_eff N_rate CntyCode u_replac)
       SITESCC   (keep=SITE_ID SCC E_eff N_eff N_rate CntyCode u_replac)
       SITEHAP   (keep=SITE_ID NTI_HAP E_eff N_eff N_rate CntyCode u_replac)
       SITEMHAP (keep=SITE_ID MACT NTI_HAP E_eff N_eff N_rate CntyCode u_replac)
       SITESHAP (keep=SITE_ID SCC NTI_HAP E_eff N_eff N_rate CntyCode u_replac)
       SITEMSCC (keep=SITE_ID MACT SCC E_eff N_eff N_rate CntyCode u_replac)
       SITEMSHP (keep=SITE_ID MACT SCC NTI_HAP E_eff N_eff N_rate CntyCode u_replac)
       BADUSER  (keep=SITE_ID MACT SCC SIC NTI_HAP E_eff N_eff N_rate CntyCode u_replac)
       ;
       set userfile;

****** Number to the right in parenthesis denotes order of precedence: 1-least specific, 38=most.
       Each datafile without SITE_ID is actually a set of 2: 1 without county data, 1 with *******;

       If compress(SITE_ID) eq "" then do;
          If compress(SIC) eq "" then do;
             if compress(MACT) eq "" then do;
                if compress(SCC) eq "" then do; *............ SITE_ID,SIC,MACT,SCC = '' **NTI_HAP MUST NOT = '';
                   if compress(NTI_HAP) eq "" then output BADUSER; *...... SOME FIELD MUST BE FILLED IN!! *****;
                   else output HAPonly; *(1-2)*;
                end;
                else do; *........................................................................... SCC ne '';
                   if compress(NTI_HAP) eq "" then output SCConly; *(15-16)*;
                   else output SCCHAP; *(23-24)*;
                end;
             end;
             else do; *....................................................... SITE_ID,SIC = '' but MACT ne '';
                if compress(SCC) eq "" then do;
                   if compress(NTI_HAP) eq "" then output MACTonly; *(5-6)*;
                   else output MACTHAP; *(11-12)*;
                end;
                else do;
                   if compress(NTI_HAP) eq "" then output MACTSCC; *(19-20)*;
                   else output MACTSCCH; *(27-28)*;
                end;
             end;
          end;
          Else do; *** SIC ne '';
             if compress(SCC) eq "" then do;
                if compress(MACT) eq "" then do;
                   if compress(NTI_HAP) eq "" then output SIConly; *(3-4)*;
                   else output SICHAP; *(9-10)*;
                end;
                else do;
                   if compress(NTI_HAP) eq "" then output MACTSIC; *(7-8)*;
                   else output MACTSICH; *(13-14)*;
                end;
             end;
             else do;
                if compress(MACT) eq "" then do;
                   if compress(NTI_HAP) eq "" then output SCCSIC; *(17-18)*;
                   else output SCCSICH; *(25-26)*;
                end;
                else do;
                   if compress(NTI_HAP) eq "" then output MACTALL; *(21-22)*;
                   else output MACTALLH; *(29-30)*;
                end;
             end;
          end;
       end;

*** Save records keyed by SITE_ID with and without: MACT, SCC, and/or  pollutant ***;
       Else do;
          if compress(MACT) eq "" then do;
             if compress(SCC) eq "" then do;
                If compress(NTI_HAP) = "" then output SITEonly; *(31)*;
                Else output SITEHAP; *(33)*;
             end;
             else do;
                If compress(NTI_HAP) = "" then output SITESCC; *(35)*;
                Else output SITESHAP; *(37)*;
             end;
          end;
          else do;
             if compress(SCC) eq "" then do;
                If compress(NTI_HAP) = "" then output SITEMACT; *(32)*;
                Else output SITEMHAP; *(34)*;
             end;
             else do;
                If compress(NTI_HAP) = "" then output SITEMSCC; *(36)*;
                Else output SITEMSHP; *(38)-MOST SPECIFIC USER REDUCTION: SITE_ID,MACT,SCC, and NTI_HAP *;
             end;
          end;
       end;

   Run;

***  Add a county control code to the emissions file IF USER-DEFINED FILE CONTAINS THEM -FEB 2003**;
***   - county control code is five characters to accommodate future geographic-level controls ***;

   %IF &cntycodes %THEN %DO;
     data CNTYFILE;
        infile reftext("&&Cntyur&P...txt") stopover firstobs=3;
        input fips $ 1-5 cntyCode $ 56-60;

        length cntycode $5.;
        CntyCode = COMPRESS(CntyCode);
     run;

***  MERGE COUNTY CONTROL CODE INTO EMISSIONS;
     proc sort data=growcntl;
        by FIPS;
     run;

     proc sort data=CNTYFILE;
        by FIPS;
     run;

     data growcntl nomatch;
        merge growcntl(in=in1) CNTYFILE(in=in2);
        by FIPS;
        length UserCntl $50.;
        UserCntl = '';

        if in1 and not in2 then output nomatch;
        if in1 then output growcntl;
     run;

***  PRINT Warning MESSAGES IF THERE ARE NON-MATCHES;
     title2 "*** Warning *** THERE ARE NON-MATCHED COUNTY CONTROL CODES ***";
     proc print data=nomatch(obs=30) split="*"; run;
     proc freq data=nomatch;
        table FIPS;
     run;
   %END;

***** Merge user-specific efficiencies *****;

   %MergUSER(eff_file=HAPonly,mvar1=,mvar2=,mvar3=,mvar4=NTI_HAP,code=HAP only(User))
   %MergUSER(eff_file=SIConly,mvar1=,mvar2=,mvar3=,mvar4=SIC,code=SIC only(User))
   %MergUSER(eff_file=MACTonly,mvar1=,mvar2=,mvar3=,mvar4=MACT,code=MACT only(User))
   %MergUSER(eff_file=MACTSIC,mvar1=,mvar2=,mvar3=MACT,mvar4=SIC,code=MACT/SIC(User))
   %MergUSER(eff_file=SICHAP,mvar1=,mvar2=,mvar3=SIC,mvar4=NTI_HAP,code=SIC/NTI_HAP(User))
   %MergUSER(eff_file=MACTHAP,mvar1=,mvar2=,mvar3=MACT,mvar4=NTI_HAP,code=MACT/NTI_HAP(User))
   %MergUSER(eff_file=MACTSICH,mvar1=,mvar2=MACT,mvar3=SIC,mvar4=NTI_HAP,code=MACT/SIC/NTI_HAP(User))
   %MergUSER(eff_file=SCConly,mvar1=,mvar2=,mvar3=,mvar4=SCC,code=SCC only(User))
   %MergUSER(eff_file=SCCSIC,mvar1=,mvar2=,mvar3=SCC,mvar4=SIC,code=SCC/SIC(User))
   %MergUSER(eff_file=MACTSCC,mvar1=,mvar2=,mvar3=MACT,mvar4=SCC,code=MACT/SCC(User))
   %MergUSER(eff_file=MACTALL,mvar1=,mvar2=MACT,mvar3=SCC,mvar4=SIC,code=MACT/SCC/SIC(User))
   %MergUSER(eff_file=SCCHAP,mvar1=,mvar2=,mvar3=SCC,mvar4=NTI_HAP,code=SCC/NTI_HAP(User))
   %MergUSER(eff_file=SCCSICH,mvar1=,mvar2=SCC,mvar3=SIC,mvar4=NTI_HAP,code=SCC/SIC/NTI_HAP(User))
   %MergUSER(eff_file=MACTSCCH,mvar1=,mvar2=MACT,mvar3=SCC,mvar4=NTI_HAP,code=MACT/SCC/NTI_HAP(User))
   %MergUSER(eff_file=MACTALLH,mvar1=MACT,mvar2=SCC,mvar3=SIC,mvar4=NTI_HAP,code=MACT/SCC/SIC/NTI_HAP(User))

   %MergUSER(eff_file=SITEonly,mvar1=,mvar2=,mvar3=,mvar4=SITE_ID,code=SITE_ID only(User))
   %MergUSER(eff_file=SITEMACT,mvar1=,mvar2=,mvar3=SITE_ID,mvar4=MACT,code=SITE_ID/MACT(User))
   %MergUSER(eff_file=SITEHAP,mvar1=,mvar2=,mvar3=SITE_ID,mvar4=NTI_HAP,code=SITE_ID/NTI_HAP(User))
   %MergUSER(eff_file=SITEMHAP,mvar1=,mvar2=SITE_ID,mvar3=MACT,mvar4=NTI_HAP,code=SITE_ID/MACT/NTI_HAP(User))
   %MergUSER(eff_file=SITESCC,mvar1=,mvar2=,mvar3=SITE_ID,mvar4=SCC,code=SITE_ID/SCC(User))
   %MergUSER(eff_file=SITEMSCC,mvar1=,mvar2=SITE_ID,mvar3=MACT,mvar4=SCC,code=SITE_ID/MACT/SCC(User))
   %MergUSER(eff_file=SITESHAP,mvar1=,mvar2=SITE_ID,mvar3=SCC,mvar4=NTI_HAP,code=SITE_ID/SCC/NTI_HAP(User))
   %MergUSER(eff_file=SITEMSHP,mvar1=SITE_ID,mvar2=MACT,mvar3=SCC,mvar4=NTI_HAP,code=SITE_ID/MACT/SCC/NTI_HAP(User))


*** QA Check - Summary of inventory records assigned with User-specific control efficiencies ***;

   data qacheck
        (keep=MACT SCC SIC site_id NTI_HAP UserXEff UserNEff UserRate CntyCode
              UserCntl Replace);
      set growcntl;
      if UserXEff <> 0 or UserNEff <> 0;
   run;

*** Delete intermediate process and chemical specific control files;

   proc datasets library=work;
      delete HAPonly SIConly MACTonly MACTSIC MACTHAP SCConly SCCHAP MACTSCC MACTSCCH SICHAP
             MACTSICH SCCSIC SCCSICH MACTALL MACTALLH SITEonly SITEMACT
             SITESCC SITEHAP SITEMHAP SITESHAP SITEMSCC SITEMSHP
             qacheck CNTYFILE userfile nomatch;
   Quit;

%MEND UserCntl;


*********************************************************************;
*********************************************************************;


%MACRO AppCntl;

*********************************************************************;
*  Merge control efficiencies, codes and compliance year information ;
*  into point source inventory.                                      ;
*  Calculate projected controled emissions in final output data set. ;
*********************************************************************;

%FnlMerg

** Check of existance of Growth factor, MACT control data and  ***;
***  SITE control information. If data present, set appropriate flag to true. ***;

*** Evaluate control information and calculate the projected emissions ***;
Data outdata.&outsas.&&pname&P;
   Set outdata.&outsas.&&pname&P;

   array tEmisA{&numtaf}  tEmis1-tEmis&numtaf;

*** Create and assign appropriate variables when no control strategy has been used ***;

*** Assign growth factor to 1, if missing ***;
*** Assign baseline control efficiency to inventory control efficiency ***;

   If GF = . then GF = 1;
   Base_eff = cntl_eff;

****** Added 08/02/01 because inventory control (if any) may contain fractions, not percentages ******;
*** That is, it is assumed that any inventory control (not cheap) would be at least 1% ***;
   if base_eff < 1.0 then base_eff = base_eff * 100;
*******************************************************************************************;
*** Determine if zero controls have been assigned to emission record  ***;
***   If so, then only the growth factor is applied                   ***;

   If ExistEff = 0 and New_Eff = 0 and AddXEff = 0 and AddNEff = 0 then do;
      Do j = 1 to &numtaf;
         tEmisA(j) = tEmisA(j) * GF;
      end;
   End;

*** For records where controls are present apply controls to all temporally ***;
***   allocated emissions                                                   ***;

   Else do;
      Do i = 1 to &numtaf;

         GrowEmis = tEmisA(i) * GF;

*** Apply Primary Reductions ***;

         If ExistEff > base_eff then
            ProjExst = GrowEmis * (1-New_Rate/100) * (1 - ExistEff/100)/(1 - base_eff/100);
         Else
            ProjExst = GrowEmis * (1-New_Rate/100);

*** Repeat process for emissions from new sources ***;

         If New_Eff > base_eff then
            ProjNew = GrowEmis * (New_Rate/100) * (1 - New_Eff/100)/(1 - base_eff/100);
         Else
            ProjNew = GrowEmis * (New_Rate/100);

*** Add projected emissions from existing sources and projected emissions from new sources;

         ProjEmis = ProjNew + ProjExst;

*** Apply Secondary or Added Reductions to the Projected Emissions  ***;
**** These 'Additional' Reductions are not weighted by baseline reductions ** FEB 2002 ***;

         ProjExst = ProjEmis * (1-Add_Rate/100) * (1 -  AddXEff/100);

*** Repeat process for emissions from new sources ***;

         ProjNew = ProjEmis * (Add_Rate/100) * (1 - AddNEff/100);

*** Add projected emissions from existing sources and projected emissions from new sources;

         tEmisA(i) = ProjNew + ProjExst;

      end;

   End;

   Drop j i GrowEmis base_eff ProjExst ProjNew ProjEmis;

run;


************************************************************************;
***                GROWTH AND CONTROL SUMMARY TABLES                 ***;
************************************************************************;
***  SET LABELS FOR PRINTING;
data outdata.&outsas.&&pname&P;
   set outdata.&outsas.&&pname&P;
   label
      emis     = "Base Emis [tpy]"
      GF       = "Growth*factor"
      NTI_HAP  = "NTI HAP*Code"
      Replace  = "User*Replacement*Code"
      ExistEff = "Existing*control*efficiency"
      New_Eff  = "New*control*efficiency"
      New_Rate = "Percent*emissions*from*new*sources"
      AddXEff  = "Added*Existing*control*efficiency"
      AddNEff  = "Added*New*control*efficiency"
      Add_Rate = "Added*Percent*emissions*from new*sources"
      CntlCode = "Reduction*Code"
      ;
%IF (%upcase("&MODEL") = "ASPEN") %THEN %DO;
    label temis1="GC Emissions [tpy]*Hrs 0-2"    temis2="GC Emissions [tpy]*Hrs 3-5"
         temis3="GC Emissions [tpy]*Hrs 6-8"    temis4="GC Emissions [tpy]*Hrs 9-11"
         temis5="GC Emissions [tpy]*Hrs 12-14"  temis6="GC Emissions [tpy]*Hrs 15-17"
         temis7="GC Emissions [tpy]*Hrs 18-20"  temis8="GC Emissions [tpy]*Hrs 21-23"
    ;
%END;

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
   ;
%END;
run;

%IF (%upcase("&MODEL") = "ASPEN") %THEN %DO;
   data CntlSum;
      set outdata.&outsas.&&pname&P;

      if compress(CntlCode) ne "" and (ExistEff <> 0 or AddXEff <> 0);

      gemis2 = emis * tafs2 * GF;
      gemis4 = emis * tafs4 * GF;
      gemis6 = emis * tafs6 * GF;

      keep MACT NTI_HAP SCC SIC SITE_ID CntlCode
           gemis2 gemis4 gemis6 temis2 temis4 temis6
           ExistEff New_Eff New_Rate AddXEff AddNEff Add_Rate;

      label
         gemis2   = "Grown*Emissions*Hrs 3-5"
         gemis4   = "Grown*Emissions*Hrs 9-11"
         gemis6   = "Grown*Emissions*Hrs 15-17"
         temis2   = "Projected*Emissions*Hrs 3-5"
         temis4   = "Projected*Emissions*Hrs 9-11"
         temis6   = "Projected*Emissions*Hrs 15-17"
      ;

   proc sort data=CntlSum;
      by CntlCode ExistEff AddXEff ;

   proc means sum data=CntlSum noprint;
      by CntlCode ExistEff AddXEff;
      var gemis2 gemis4 gemis6
          temis2 temis4 temis6;
      id MACT NTI_HAP SCC SIC SITE_ID
         New_Eff New_Rate AddNEff Add_Rate;
      output out=CntlSum(drop=_freq_ _type_)
      sum = gemis2 gemis4 gemis6
            temis2 temis4 temis6;
   run;

   proc print data=CntlSum noobs split='*';
      var MACT NTI_HAP SCC SIC SITE_ID CntlCode
          gemis2 gemis4 gemis6 temis2 temis4 temis6
          ExistEff New_Eff New_Rate AddXEff AddNEff Add_Rate;
     title2 'Summary of Assigned Reductions and Projected Emissions';
   run;

%END;

%IF (%upcase("&MODEL") eq "ISC") %THEN %DO;

   data CntlSum;
      set outdata.&outsas.&&pname&P;

      if compress(CntlCode) ne "" and (ExistEff <> 0 or AddXEff <> 0);

*** Calculate annual grown emissions (in tons/year) ***;

      gemis = emis * GF;

*** Calculate projected annual emissions (in tons/year) from the temporally ***;
***   allocation projected emissions (in tons/hr)                           ***;

* Start with tons per hour for each season and daytype *;
* Yields tons per daytype per season *;

      winwk   = sum(of temis1-temis24);
      sprwk   = sum(of temis25-temis48);
      sumwk   = sum(of temis49-temis72);
      falwk   = sum(of temis73-temis96);

      winsa   = sum(of temis97-temis120);
      sprsa   = sum(of temis121-temis144);
      sumsa   = sum(of temis145-temis168);
      falsa   = sum(of temis169-temis192);

      winsu   = sum(of temis193-temis216);
      sprsu   = sum(of temis217-temis240);
      sumsu   = sum(of temis241-temis264);
      falsu   = sum(of temis265-temis288);

* Yields tons per season *;

      winter  = sum(winwk * 5,winsa,winsu) * 13;
      spring  = sum(sprwk * 5,sprsa,sprsu) * 13;
      summer  = sum(sumwk * 5,sumsa,sumsu) * 13;
      fall    = sum(falwk * 5,falsa,falsu) * 13;

* Yields tons per year *;

      pemis = sum(winter,spring,summer,fall);

      keep MACT NTI_HAP SCC SIC SITE_ID CntlCode gemis pemis
           ExistEff New_Eff New_Rate AddXEff AddNEff Add_Rate;

      label
         gemis   = "Annual*Grown*Emissions"
         pemis   = "Annual*Projected*Emissions" ;

   proc sort data=CntlSum;
      by CntlCode ExistEff AddXEff ;

   proc means sum data=CntlSum noprint;
      by CntlCode ExistEff AddXEff;
      var gemis pemis;
      id MACT NTI_HAP SCC SIC SITE_ID
         New_Eff New_Rate AddNEff Add_Rate;
      output out=CntlSum(drop=_freq_ _type_)
      sum = gemis pemis;
   run;

   proc print data=CntlSum noobs split='*';
      var MACT NTI_HAP SCC SIC SITE_ID CntlCode
          gemis pemis
          ExistEff New_Eff New_Rate AddXEff AddNEff Add_Rate;
     title2 'Summary of Assigned Reductions and Projected Emissions';
   run;

%END;


*** Delete intermediate files ***;

proc datasets library=work;
   delete checkvar CntlSum;
Quit;

%MEND AppCntl;

*********************************************************************;
*********************************************************************;


%MACRO GrowCntl;

*********************************************************************;
*                                                                    ;
*   Main program for Macro GrowCntl.  This code controls which of    ;
* the two major portions of the code are run:                        ;
*        GROW - to add growth factors by FIPS and MACT and/or SIC and/or SCC;
*        MACTCntl - to read and assign MACT controls                 ;
*        UserCntl - to read and assign User-specified controls       ;
*        AppCntl - to apply growth factors and control efficiencies  ;
*********************************************************************;

%do P=1 %to &numproj;
   *** Set Title1 for all information written to list file   2/23/01 DYL;
   *** FEB 2003: Set Title1 for all information written to list file   2/23/01 DYL;
   title1 "&Model.: &&comments&P";

   *** First, determine if you need to preserve the inventory ***;
   %IF &numproj > 1 %THEN %DO;
      %IF &P eq 1 %THEN %DO;
         proc copy in=work out=outdata;
            select growcntl inventry;
         run;
      %END;
      %ELSE %DO;
         %IF &P eq &numproj %THEN %DO;
            proc copy in=outdata out=work move;
               select growcntl inventry;
            run;
         %END;
         %ELSE %DO;
            proc copy in=outdata out=work;
               select growcntl inventry;
            run;
         %END;
      %END;
   %END;
   %If &&GFSITE&P ne or &&GFMACT&P ne or &&GFSIC&P ne or &&GFSCC&P ne %then %do;
        %GROW
   %end;

   %If &&MACTGEN&P ne %then %do;
         %MACTCntl
   %end;

   %If &&USERFILE&P ne %then %do;
         %UserCntl
   %end;

   %AppCntl
%end;

%MEND GrowCntl;

*********************************************************************;
*********************************************************************;


********************************************************;
***                                                  ***;
***                MAIN PROGRAM                      ***;
***                                                  ***;
********************************************************;

OPTIONS mprint symbolgen;

%GetInfo

%GrowCntl

run;
