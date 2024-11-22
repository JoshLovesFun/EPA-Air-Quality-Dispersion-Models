********************************************************************************;
* Program: PtModelProc - Unix Version                                           ;
* Date: March 5, 1999                                                           ;
*       mod 7/99 - modified HAPTABL and Default                                 ;
* Authors: Bill Battye and Diane Linderman                                      ;
*                                                                               ;
* This program performs three basic functions:                                  ;
*    SelHAPS - This portion performs three operations:                          ;
*              1) Adds SAROAD code, SAROAD code description, NTI_HAP code, and  ;
*                 reactivity to inventory by the inventory pollutant code       ;
*                 (inv_code).                                                   ;
*              2) Windows the inventory to include only those pollutants of     ;
*                 interest (keep='Y') and applies factor used to apportion      ;
*                 emissions between more than one SAROAD code.                  ;
*              3) Sums the emissions by emissions source (identified by site_id ;
*                 and emrelpid) and SAROAD code.                                ;
*    TrctFlag - For ASPEN model processing only. Assigns the urban/rural flag to;
*               the inventory based on either of two criteria:                  ;
*                 county location of point source if all tract within the county;
*                 are urban or rural.                                           ;
*                 census tract nearest to each point source.                    ;
*    Default - This portion performs three operations:                          ;
*              1) For ASPEN model processing, default zero or missing           ;
*                 reactivities to 1.                                            ;
*              2) For ASPEN model processing, assigns IVENT variable based on   ;
*                 the emission release point type (emrelpty) to 0 for all       ;
*                 stacked sources and 0 for all non-stacked sources (fugitives, ;
*                 horizontal stacks, and airports).                             ;
*              3a) For ASPEN model processing, sets building parameters to 5m   ;
*                 for horizontal stacks and to 0 for all other stacks.          ;
*              3b) For ISCST3 model processing: for stacks less than 65 meters  ;
*                 in height, sets building height equal to 0.625*stack-height,  ;
*                 so long as building height is at least 3.05 meters.  Building ;
*                 width is set to twice the building height.                    ;
*                                                                               ;
*  The user provides the names of directories, input and output SAS datasets,   ;
*  and external reference SAS datasets and text files. The user also specified  ;
*  the emissions variable used in the accumulation of emissions by point source ;
*  and SAROAD code.                                                             ;
*                                                                               ;
*  Directory names:                                                             ;
*      in_data - input SAS data set directory                                   ;
*      outdata - output SAS data set directory                                  ;
*      refsas - reference SAS data sets directory                               ;
*      reftext - reference text file directory                                  ;
*                                                                               ;
*  Input and Output SAS dataset names:                                          ;
*     insas - input SAS data set name                                           ;
*     outsas - output SAS data set name                                         ;
*                                                                               ;
*  SelHAPs function input SAS data set:                                         ;
*     haps - name of data set containing correspondance between the inventory   ;
*            pollutant code and the CAS number, pollutant code, NTI_HAP code,   ;
*            SAROAD code, and reactivity class.  This file also indicates which ;
*            pollutants are to be kept in the windowing step.                   ;
*                                                                               ;
*  TrctFlag function input SAS data sets (ASPEN ONLY):                          ;
*     ctyflag - name of data set containing the average county urban/rural flag ;
*               by FIPS (state and county FIP codes).  The flag is set to either;
*               1 or 2 if the county is all urban or all rural and is set to 9  ;
*               if the county is not uniform.                                   ;
*     trctinf - name of data set containing census tract information, including ;
*               the urban/rural flag, tract state and county FIP code, tract    ;
*               location by latitude and longitude and the tract radius.        ;
*                                                                               ;
********************************************************************************;
;

**********************************************************************;
**********************************************************************;


%MACRO GetInfo;

********************************************************************;
* MACRO: GetInfo                                                    ;
*    This macro reads a user control input file to get the names of ;
* the directories and file names to be used and variables to be used;
* throughout the program.                                           ;
*    First read the MODEL variable and based on this value,read the ;
* cooresponding variables or stop processing if the value of MODEL  ;
* is not either ASPEN or ISC.                                       ;
********************************************************************;

%global model renameMACT;
%let Model = %sysget(MODEL);

%IF (%upcase("&model") = "ASPEN") %THEN %DO;

  %global insas outsas g_pthap g_mobhap spechap ctyflag trctinf;

   %let insas = %sysget(INSAS);
   %let outsas = %sysget(OUTSAS);

   %let g_pthap = %sysget(G_PTHAP);
   %let g_mobhap = %sysget(G_MOBHAP);
   %let spechap = %sysget(SPECHAP);

   %let ctyflag = %sysget(CTYFLAG);
   %let trctinf = %sysget(TRCTINF);

   %put _user_;

*** Set directories ***;

   Libname in_data "$IN_DATA";
   Libname outdata "$OUTDATA";
   Libname refsas "$REFSAS";
   Filename reftext "$REFTEXT";

%END;

%ELSE %IF (%upcase("&model") = "ISC") %THEN %DO;

  %global insas outsas g_pthap g_mobhap spechap;

   %let insas = %sysget(INSAS);
   %let outsas = %sysget(OUTSAS);

   %let g_pthap = %sysget(G_PTHAP);
   %let g_mobhap = %sysget(G_MOBHAP);
   %let spechap = %sysget(SPECHAP);

   %put _user_;

*** Set directories ***;

   Libname in_data "$IN_DATA";
   Libname outdata "$OUTDATA";

   Filename reftext "$REFTEXT";

%END;

%ELSE %DO;
   put @1 'Error!!! Model designation is incorrect.';
   STOP;
%END;
proc contents data=in_data.&insas out=chkmact(keep=name) noprint; run;
%let renameMACT = 0;
data _null_;
   set chkmact end=last;
   where lowcase(name) = 'mactcode';
   call symput('renameMACT',last);
run;

%MEND GetInfo;

**************************************************************;
**************************************************************;


%MACRO GetHAPs(genhap= ,spechap= ,datain= ,dataout= ,sumfile= );

******************************************************************************;
* MACRO: GetHAPs                                                              ;
*    This macro assigns SAROAD code, reactivity, SAROAD code description, and ;
* NTI-HAP code by the inventory pollutant code in the emission inventory file.;
* This file also include a keep code used to window the inventory to include  ;
* only those pollutants of interest and a factor used to apportion the        ;
* emissions to more than one SAROAD code.                                     ;
* This macro uses the correspondance file HAPTABLE to assign the inventory    ;
* pollutant code to the other pollutant identifiers.                          ;
** FOR 1999 NIF:  Allow HAPs to be speciated by MACT, SIC, and SCC.       ;
******************************************************************************;
;

*** Read text file describing HAPS and sort ***;
*** Modified (6/21/99) to use new haps table and input file from preprocessing. ***;
*** Modified (3/14/02) to use new haps tables: HAPgen and HAPspec. ***;

data hapkeep;
  infile reftext("&genhap..txt") firstobs=2;
  input
     @1 polldesc $45.
     @47 saroaddc $50.
     @100 pollcode $10.
     @113 react 1.
     @121 keep $1.
     @128 saroad $5.
     @135 factor 7.
     @144 NTI_HAP $3.
   ;
run;

*** Sort inventory file and merge with HAP description file  ***;
*** Window to HAPS designated to be kept and apply factor    ***;

Proc SQL noprint;
   Create table merged
     as Select *
     from &datain a left join hapkeep b
     on lowcase(trim(a.cas)) = lowcase(trim(b.pollcode))
   ;
quit;

data &dataout NotKept(keep= keep cas site_id emrelpid emis saroad polldesc saroaddc)
     NewPoll(keep= cas site_id emrelpid emis);
   set merged;

   if lowcase(keep) = 'y' then output &dataout;
   else if lowcase(keep) = 'n' then output NotKept;
   else output NewPoll;

***** NEW **************************************************************;
%if "&spechap" ne "NONE" and %quote(&spechap) ne %then %do;
data b4_spec;
   set &dataout;
   emis=emis*factor;

proc sort data=b4_spec; by cas saroad; run;
proc summary data=b4_spec sum;
   id factor;
   by cas saroad;
   var emis;
   output out=qa_b4(drop=_type_ _freq_) sum=inpemis n=inpcount;
run;
data HAPspec;
   infile reftext("&spechap..txt") firstobs=5 missover pad lrecl=212;
   input @1 polldesc $45. @47 cas $10. @58 olds1 $5. @64 news1 $5. @70 olds2 $5.
         @76 news2 $5. @82 olds3 $5. @88 news3 $5. @94 spec_fac 7. @101 MACTcode $7.
         @109 SCCcode $10. @120 SICcode $4.;
run;
proc sort data=&dataout; by cas; run;
proc sort data=HAPspec(keep=cas) out=SPECcode nodupkey; by cas; run;
*** Extracts data with CAS numbers in the HAPspec file;
data nospec spec;
   merge &dataout(in=A) SPECcode(in=B);
   by cas;
   if A and B then output spec;
   if A and not B then output nospec;
run;

data specmact(drop=SICcode SCCcode) specsic(drop=MACTcode SCCcode) specscc(drop=MACTcode SICcode)
     def_spec(drop=MACTcode SICcode SCCcode);
   set HAPspec;
   if MACTcode ne '' then output specmact;
   else if SICcode ne '' then output specsic;
   else if SCCcode ne '' then output specscc;
   else output def_spec;
run; 
proc sql;
   create table mactsql
   as select *, 1 as specflag
   from spec (drop=pollcode polldesc) a, 
        specmact (rename=(cas=pollcode)) b
   where (a.cas=b.pollcode) and (a.MACT=b.MACTcode)
;
   create table sccsql
   as select *, 2 as specflag
   from spec (drop=pollcode polldesc) a, 
        specscc (rename=(cas=pollcode)) b
   where (a.cas=b.pollcode) and (a.SCC=b.SCCcode)
;
   create table sicsql
   as select *, 3 as specflag
   from spec (drop=pollcode polldesc) a, 
        specsic (rename=(cas=pollcode)) b
   where (a.cas=b.pollcode) and (a.SIC=b.SICcode)
;
   create table defsql
   as select *, 4 as specflag
   from spec (drop=pollcode polldesc) a, 
        def_spec (rename=(cas=pollcode)) b
   where (a.cas=b.pollcode)
;
quit;
** Obtain real saroad, sort and retain only lowest specflag (MACT > SIC > SCC > Default) **;
***** MAY 2002 -Retain OLD SAROAD emissions for QA purposes ******;
data allspec(drop=olds1-olds3 news1-news3);
   set mactsql(drop=MACTcode) sicsql(drop=SICcode) sccsql(drop=SCCcode) defsql;
   if saroad = olds1 then saroad = news1;
   else if saroad = olds2 then saroad = news2;
   else saroad = news3;
run;
proc sort data = allspec;
   by site_id emrelpid cas saroad specflag;
run;
data SPEC_fin;
   length spec_hap $26;
   set allspec;
   by site_id emrelpid cas saroad;
   if first.saroad;
   *factor =  factor * spec_fac;
   select (specflag);
      when (1) spec_hap = 'Obtained from HAP/MACT'; 
      when (2) spec_hap = 'Obtained from HAP/SIC'; 
      when (3) spec_hap = 'Obtained from HAP/SCC'; 
      otherwise spec_hap = 'Pollcode Default';
   end;
run;
  
data &dataout(drop=specflag);
   set nospec SPEC_fin;
   if spec_fac = . then spec_fac = 1.0;
run;
%end;
***** END NEW ***;
*** Print out list of Records with Pollutant Codes not Found in HAP Table ***;

proc sort data = NewPoll out=uniqNewPoll nodupkey;
   by cas;
run;
proc print data=uniqNewPoll noobs;
   var cas ;
   title 'Unique New Pollutant Codes in Inventory NOT Found in HAP Table';
run;

*** QA of Pollutants Kept for Modeling ***;

proc sort data=&dataout;
   by cas saroad;

proc means sum n data=&dataout noprint;
   by cas saroad;
   var emis;
   id factor saroaddc polldesc;
   output out=Keptlist(drop=_type_ _freq_) sum=emis n=count;

proc print data=Keptlist noobs split='*';
   label cas = 'CAS'
         saroad = 'SAROAD Code'
         factor = 'HAP Table*Factor'
         emis = 'Emissions'
         count = 'Number of*Records'
         saroaddc = 'SAROAD Description'
         polldesc = 'Pollutant Code Description';
    var cas saroad factor emis count saroaddc polldesc;
    title "&datain :List of Pollutants Retained for ASPEN Modeling With SAROAD Assignment and Factor";

proc sort data=&dataout;
   by site_id emrelpid cas;

data HAPsum;
   set &dataout;
   by site_id emrelpid cas;
   if first.cas;

proc sort data=HAPsum;
   by cas;

proc means sum data=HAPsum noprint;
   by cas;
   var emis;
   output out=KeptSum(drop=_type_ _freq_) sum=kept;
run;

*** QA of Pollutants Not Retained for Modeling ***;

proc sort data=NotKept;
   by cas saroad;

proc means sum data=NotKept noprint;
   by cas saroad;
   var emis;
   id saroaddc polldesc;
   output out=NotList(drop=_type_ _freq_) sum=emis n=count;

proc print data=NotList noobs split='*';
   label cas = 'CAS'
         saroad = 'SAROAD Code'
         emis = 'Emissions'
         count = 'Number of*Records'
         saroaddc = 'SAROAD Description'
         polldesc = 'Pollutant Code Description';
    var cas saroad emis count saroaddc polldesc;
    title "&datain :List of Pollutants NOT Retained for ASPEN Modeling With SAROAD Assignment";

proc sort data=NotKept;
   by site_id emrelpid cas;

data NotSum;
   set NotKept;
   by site_id emrelpid cas;
   if first.cas;

proc sort data=NotSum;
   by cas;

proc means sum data=NotSum noprint;
   by cas;
   var emis;
   output out=NotKpSum(drop=_type_ _freq_) sum=notkept;

run;

*** Comparison of Emissions Retained and Not Retained by Pollutant Code with Incoming Data ***;

data indata;
   set &datain;
   keep cas emis;

proc sort data=indata;
   by cas;

proc means sum data=indata noprint;
   var emis;
   by cas;
   output out=DataSum(drop=_type_ _freq_) sum=dataemis;

proc sort data=DataSum;
   by cas;

proc sort data=NotKpSum;
   by cas;

proc sort data=KeptSum;
   by cas;

data PollList;
   merge KeptSum NotKpSum DataSum;
   by cas;

proc print data=PollList noobs split='*';
   label cas = 'CAS'
         dataemis = 'Emissions from*Input Data Set'
         kept = 'Emissions*Retained'
         notkept = 'Emissions Not*Retained';
   sum dataemis kept notkept;
   var cas kept notkept dataemis;
   title "&datain :Comparison of Emissions Retained for Modeling, and Emissions not Retained, and Emissions in Inventory";
   title2 "Remember, if POM is KEPT but HAPs like 7-PAH or 16-PAH are NOT, all 3 Columns may have the same values";
run;

***********************************************************;
***  Apply HAP-GEN/HAP-SPEC Table factor to emissions   ***;
***********************************************************;

%if "&spechap" ne "NONE" and %quote(&spechap) ne %then %do;
   data &dataout;
      set &dataout;
      factor = factor * spec_fac;
      emis = emis * factor;
   run;
%end;
%else %do;
   data &dataout;
      set &dataout;
      emis = emis * factor;
   run;
%end;

*** QA of Application of Factor ***;

proc sort data=&dataout;
   by cas saroad;

proc means sum data = &dataout noprint;
   var emis;
   by  cas saroad;
   id factor;
   output out=PollSum(drop=_type_ _freq_) sum=emis n=count;

%if "&spechap" ne "NONE" and %quote(&spechap) ne %then %do;
   ****** PRINT Before and After (HAP tables) Emission splits ***;
   data b4_after; ** Pollcode/SAROAD emission sums before and after HAP-SPEC **;
      merge qa_b4(rename=(factor=inpfac)) PollSum;
      by cas saroad;
   run;
   proc print data=b4_after noobs split='*';
      label cas = 'CAS'
            saroad = 'SAROAD Code'
            inpemis = 'AFTER HAP-GEN/*B4 HAP-SPEC*Emissions'
            emis = 'AFTER*HAP-SPEC*Emissions'
            inpfac = 'HAP-GEN* Table*Factor'
            factor = 'HAP-GEN/SPEC*Table*Factor';
       sum inpemis emis;
       var cas saroad inpemis emis inpfac factor;
       title 'Pollutant Level Summary of Emissions Retained for ASPEN Modeling After Application of Factor';
       title2 'Obtained BEFORE and AFTER HAP-GEN and HAP-SPEC factors applied';
       title3 'HAP-GEN/SPEC factor is product of HAP-GEN and HAP-SPEC factors';
   run;
   title3 ;
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
****** END PRINT Before and After (HAP tables) Emission splits ***;
title ;

proc sort data=PollSum;
   by saroad cas;

proc print data=PollSum noobs split='*';
   label saroad =  'SAROAD Code'
         cas = 'CAS'
         emis = 'Emissions'
         factor = 'HAP Table*Factor';
   sum emis;
   var saroad CAS emis factor;
   title "&datain :SAROAD Level Summary of Emission Retained for ASPEN Modeling After Application of Factor";

run;

***********************************************************************************;
***  Accumulate emission by site ID, emission release point and SAROAD code ***;
***********************************************************************************;

proc sort data=&dataout(drop=polldesc keep factor) ;
  by site_id emrelpid saroad;

proc means sum data=&dataout noprint;
  by site_id emrelpid saroad;
  var emis;
  output out=emisdata(drop=_type_ _freq_) sum=;

data &dataout;
  set &dataout;
  by site_id emrelpid saroad;
  if first.saroad;

data &dataout;
   set emisdata;
   set &dataout(drop=emis);


*** QA GetHAPs Macro Output ***;

proc sort data=&dataout;
   by saroad;

proc means sum n data=&dataout noprint;
   by saroad;
   var emis;
   output out=&sumfile(drop=_type_ _freq_) sum=emis n=count;

proc print data=&sumfile noobs;
   label saroad = 'SAROAD Code'
         emis = 'Emissions Total';
   sum emis;
   title "&datain :Emission Total After Selection of Pollutants, Application of Factor, and Accumulation by Saroad";

run;


*** Remove temporary working files ***;

proc datasets library=work;
   delete hapkeep emisdata notkept notkpsum keptsum NewPoll notlist notsum hapsum &sumfile;
quit;


%MEND GetHAPs;

**************************************************************;
**************************************************************;


%MACRO TrctFlag;

****************************************************************************;
* MACRO: TrctFlag (ASPEN MODEL PROCESSING ONLY)                             ;
*    This macro assigns urban/rural flags for each location (by lat/lon).   ;
* Flags for those counties that are uniformly urban or rural are set by     ;
* the FIPS (state and county FIP codes).  For points located in non-uniform ;
* counties, the flag is set of the nearst census tract.                     ;
****************************************************************************;
;

*** Merge Urban/Rural Flag with point inventory for counties with uniform urban/rural flags ***;

*** window inventory to a temporary file containing all unique point locations ***;

**** JULY 2002: SKIP THE uflag merging if PtDataProc was skipped (inventory has no lat/lon info).;

proc contents data=outdata.&outsas out=x(keep=name) noprint; run;

%let getflag = 0;
   data _null_;
      set x;
      where lowcase(name) in ('lat','lon');
      call symput('getflag',1);
   run;
%put &getflag;

%IF &getflag %THEN %DO;

proc sort data=outdata.&outsas out=AddFlag(keep=FIPS site_id emrelpid lat lon);
  by FIPS site_id emrelpid;

data AddFlag;
  set AddFlag;
  by FIPS site_id emrelpid;
  if first.emrelpid;

*** Add county-level urban/rural flag ***;

proc sort data=refsas.&ctyflag out=ctyflag;
  by FIPS;

data GotFlag(keep=FIPS site_id emrelpid uflag) NeedTrct;
  merge AddFlag(in=A) ctyflag(in=B);
  by FIPS;

  if A then do;
     if C_flag = . or C_flag = 9 then Uflag = 2;
     else Uflag = C_flag;

*** Identify plants in non-uniform counties ***;

     if C_flag = 9 then output NeedTrct;
     output GotFlag;
  end;

  drop C_flag;

*** Merge Tract information for plants in non-uniform counties ***;

proc sort data=refsas.&trctinf out=trctinf;
   by FIPS;

proc sort data=NeedTrct;
  by FIPS;

*** Modified (6/21/99) to increase the radius criteria for selecting tracts) ***;

Proc SQL noprint;
   Create table GetTrct as
      Select A.TrLon, A.TrLat, A.Uflag, A.TrRad,
         B.FIPS, B.Site_id, B.emrelpid, B.Lon, B.Lat,
         ((cos(((A.TrLat+B.Lat)/2)*180/3.14159)
         * (A.TrLon-B.Lon))**2
         + (A.TrLat-B.Lat)**2)
         * ((60*2*36/39.37)**2) as d2
      From trctinf A, NeedTrct B
      Where A.FIPS = B.FIPS
      and (A.TrRad**2)*3 >
         ((cos(((A.TrLat+B.Lat)/2)*180/3.14159)
         * (A.TrLon-B.Lon))**2
         + (A.TrLat-B.Lat)**2)
         * ((60*2*36/39.37)**2);
Quit;

*** Drop all but nearest tract ***;

Proc Sort Data = GetTrct;
   By FIPS site_id emrelpid;

Data GotTrct(keep=FIPS site_id emrelpid Uflag);
   Set GetTrct;
   By FIPS site_id emrelpid;

*** Modified (6/21/99) to remove the arbitrary initial value for chkdist. ***;
***  Also removed reference to TrFIPS and retaining of T_FIPS - not needed. ***;

*** Modified (051000) to correct assignment of tract when correct tract is ***;
***   the first in the selection.                                          ***;

   If first.emrelpid then do;
      Chkdist = d2;
      T_Uflag = Uflag;
   end;

   Retain chkdist T_Uflag;

*** Find nearest tract centroid ***;
   If d2 < chkdist then Do;
      Chkdist = d2;
      T_Uflag = Uflag;
   End;

*** Output with closest applicable Uflag ***;
   If last.emrelpid then do;
      Uflag = T_Uflag;
      output;

   End;

*** Merge data with some county-level flags to data with Tract-level flags ***;

proc sort data=GotFlag;
   by FIPS site_id emrelpid;

proc sort data=GotTrct;
  by FIPS site_id emrelpid;

data HaveFlag;
   merge GotFlag(in=A) GotTrct(in=B);
   by FIPS site_id emrelpid;

*** Merge Urban/Rural Flag data into inventory ***;

proc sort data=HaveFlag;
  by FIPS site_id emrelpid;

proc sort data=outdata.&outsas;
  by FIPS site_id emrelpid;

Data outdata.&outsas;
   Merge outdata.&outsas(in=A) HaveFlag(in=B);
   By FIPS site_id emrelpid;
   If A ;

Run;

*** Remove temporary working files ***;

Proc datasets library=work;
   delete AddFlag GotFlag NeedTrct GetTrct GotTrct HaveFlag x;
quit;

%END;
%ELSE %DO;
    *** JULY 2002: USER CHOOSES TO BY-PASS PtDataProc. Must initialize lat,lon,uflag as .;
   data outdata.&outsas;
      set outdata.&outsas;
      lat = .; lon= . ; uflag = .;
   run;

%END;

%MEND TrctFlag;

********************************************************;
********************************************************;


%MACRO Default;

********************************************************************;
* MACRO: Default                                                    ;
*    For ASPEN, this macro check the value of reactivity and
*    defaults missing or zero values to one, assigned IVENT based on
*    the emission release point type, and sets the building
*    parameters (zero for all points except for horizontal stacks
*    [emrelpty=02] where the building height and width are set to 5 m.
*
*    For ISCST3, this macro will use an algorithm to compute building
*    dimensions if they are not present in the inventory.
*
*    MARCH 2002:  Drop Pollcode, and POLCODE VARIABLES.
********************************************************************;
;

%if (%upcase("&model") = "ASPEN") %then %do;

*.........ASPEN part of the macro......................;

   data outdata.&outsas;
      set outdata.&outsas(drop=pollcode cas);


*** Set missing or zero reactivities to default ***;

   if React = 0 or React = . then React = 1;


*** Set IVENT to 1 for non-stacked sources (fugitives(01), horizontal stacks(03), and airport sources) ***;
***     and to 0 for stacked sources                                                                   ***;

   if emrelpty = '01' or emrelpty = '03' or emrelpty = 'AP' then Ivent = '1';
   else Ivent = '0';


*** Set building parameters to 5m for horizontal stacks(03) and to 0m for all other sources ***;

   if emrelpty = '03' then do;
      Ibldg = '1';
      Bldw = 5;
      Bldh = 5;
   end;
   else do;
      Ibldg = '0';
      Bldw = 0;
      Bldh = 0;
   end;

   run;

%end;

%else %if (%upcase("&model") = "ISC") %then %do;

*.........ISC part of the macro.........................;

   data outdata.&outsas;
      set outdata.&outsas(drop=pollcode cas);


*** Assign building parameters if the inventory building parameters are missing ***;
*** DEC 2002:  These building parameters will be ignored for ISCtype=ISCarea sources *;

   if (Bldh = .) then do;
      if (stackht le 65) then do;
         Bldh = 0.625 * stackht;
         if (Bldh < 3.05) then Bldh = 3.05;
      end;
   end;

   if (Bldw = .) then do;
      if (stackht le 65) then do;
         Bldw = 2 * Bldh;
      end;
   end;

   run;

%end;

*** Determine saroad-level sums for QA checks ***;

proc sort data=outdata.&outsas;
   by saroad;

proc means sum n data=outdata.&outsas noprint;
   by saroad;
   var emis;
   output out=TotalSum(drop=_type_ _freq_) sum=emis n=count;

proc print data=TotalSum noobs;
   label saroad = 'SAROAD Code'
         emis = 'Emissions Total';
   sum emis;
   var saroad emis;
   title 'SAROAD level Emission Summary for Aspen-Specific Processing (PtModelProc)';
   title2 "INCLUDES EMISSIONS FROM ALL SOURCE TYPES";
run;


%MEND Default;

*************************************************************;
*************************************************************;


%MACRO SelHAPS;

data ptdata mobdata;
   set in_data.&insas(%if &renameMACT %then %do; rename=(MACTcode=MACT) %end;);
   if compress(src_type) = 'nonroad' then output mobdata;
   else output ptdata;

%GetHAPS(genhap=&g_pthap,spechap=&spechap,datain=ptdata,dataout=point,sumfile=pointsum);

%GetHAPS(genhap=&g_mobhap,spechap=&spechap,datain=mobdata,dataout=nonroad,sumfile=nroadsum);

data outdata.&outsas;
   set point nonroad;
run;


%MEND SelHAPS;

*************************************************************;
*************************************************************;

%MACRO MainProg;

%SelHAPS

%if (%upcase("&model") = "ASPEN") %then %do;
   %TrctFlag
%end;

%Default


%MEND MainProg;

*************************************************************;
*************************************************************;



***   MAIN PROGRAM ***;

OPTIONS mprint symbolgen;

%GetInfo

%MainProg

run;
