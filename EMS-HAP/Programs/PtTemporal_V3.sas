********************************************************************************;
* Program: PtTemporal - PC Version                                              ;
* Date: July 12, 1999                                                           ;
* Modified: June 28, 2000                                                       ;
* Author: Diane Linderman                                                       ;
* Revised by: Steve Fudge                                                       ;
*                                                                               ;
* Revisions:                                                                    ;
* June 28, 2000 - This program was modified to be used to support either the    ;
*                 ASPEN model or the ISCST3 model.  For the ISCST3 model,       ;
*                 temporal factors by season, day-type, and hour are needed.  A ;
*                 macro called ISCTAF was written to read the required temporal ;
*                 factors for ISCST3.                                           ;
*                                                                               ;
* October 2002  - SCC2AMS file no longer has surrogate information in it because;
*                 Non-point inventory will NEVER need it anymore.  Removing this;
*                 field has NO effect on the code because it was NEVER read here;
*                 anyways -it just seemed unnecessary to keep it when it will   ;
*                 show up in Version 3 Appendix A.                              ;
*                                                                               ;
* November 2002 - Updated to allow ISCST3 processing of MOBILE6 emissions.      ;
*                 Seasonal (summer, spring, autumn, and winter) 24-hourly       ;
*                 emissions will be converted to the 288 emission rates for 4   ;
*                 seasons and 3 day-of-week types.                              ;
*                                                                               ;
*               * MOBILE6 emissions must be in tons/hour and ONE INVENTORY      ;
*                 record must contain an array of spring{spring1-spring24},     ;
*                 summer{summer1-summer24}, autumn{autumn1-autumn24}, and       ;
*                 winter{winter1-winter24}.                                     ;
*                                                                               ;
*              ** If you have an inventory with an array of annual emissions,   ;
*                 you will need to SEASONALLY allocate these first BEFORE       ;
*                 submitting to this program.                                   ;
*                                                                               ;
*             *** THESE ARRAYS MUST CONTAIN NON-MISSING VALUES !!!!!!! *********;
*                 THEREFORE, TAF RECORDS MUST CONTAIN A 'PSEUDO-SCC' CODE WHERE ;
*                 TAFs and SEASON FRACTIONS WILL ALL BE EQUAL to 1.0.           ;
*                 DAYTYPE  FRACTIONS MUST SUM TO 7.0 IN THE FOLLOWING EQUATION: ;
*                 (weekday*5 + saturday + sunday)=7.0                           ;
*                 THIS IS IN CONTRAST TO "NORMAL" annual emissions, where:      ;
*                 (weekday*5 + saturday + sunday) * 13 =1.0                     ;
*                                                                               ;
* This program performs two basic functions:                                    ;
*    ReadTAF - This portion performs three operations:                          ;
*              1) reads the temporal allocation file                            ;
*              2a) calculates the 3-hour allocation factors from the annual     ;
*                  1-hour factors for ASPEN model, or,                          ;
*              2b) calculates the hourly allocation factors from the seasonal   ;
*                  and day-of-week varying 1-hour factors for ISCST3 model      ;
*              3) normalizes the 3-hour (ASPEN) or hourly (ISCST3) allocation   ;
*                 factors                                                       ;
*    MergeTaf - This portion assigns temporal allocation factors to each record ;
*               in the point source inventory by the following hierarchy of     ;
*               matching and correspondance files:                              ;
*              1) Match 8-digit scc codes in the inventory with the ams_scc     ;
*                 codes in the TAF file. Continue for records without assigned  ;
*                 tafs                                                          ;
*              2) Use a scc to scc_ams correspondance file to assign scc_ams    ;
*                 codes to remaining inventory records.  Match these scc_ams    ;
*                 codes to scc_ams codes in TAF file.  Continue for records     ;
*                 without assigned tafs.                                        ;
*              3) Use a sic to scc_ams correspondance file to assign scc_ams    ;
*                 codes to remaining inventory records.  Match these scc_ams    ;
*                 codes to scc_ams codes in TAF file.  Continue for records     ;
*                 without assigned tafs.                                        ;
*              4) Use a MACT code to scc_ams correspondance file to assign      ;
*                 scc_ams codes to remaining inventory records.  Match these    ;
*                 scc_ams codes to scc_ams codes in TAF file.  Continue for     ;
*                 records without assigned tafs.                                ;
*              5) Match 6-digits scc codes to first 6-digits of the scc_ams     ;
*                 codes in TAF file.  Continue for records without assigned tafs;
*              6) Assign default tafs to remaining records.                     ;
*                 ASPEN: After each record of the point source inventory has    ;
*                   been assigned a taf, the 3-hour allocated emissions values  ;
*                   are calculated from the annual emissions value and each     ;
*                   3-hour temporal allocation factor.                          ;
*                 ISCST3: After each record of the point source inventory has   ;
*                   been assigned a taf, the hourly allocated emissions values  ;
*                   are calculated from the annual emissions value and each     ;
*                   hourly temporal allocation factor.                          ;
*              7) Emissions are output as tons/year (ASPEN), or tons/hr (ISCST3);
*                                                                               ;
*  The user provides the names of directories, input and output SAS datasets,   ;
*  and external reference text files.                                           ;
*                                                                               ;
*  Directory names:                                                             ;
*      in_data - input SAS data set directory                                   ;
*      outdata - output SAS data set directory                                  ;
*      reftext - reference text file directory                                  ;
*                                                                               ;
*  Input and Output SAS dataset names:                                          ;
*     insas - input SAS data set name                                           ;
*     outsas - output SAS data set name                                         ;
*                                                                               ;
*  ASPENTAF and ISCTAF function input text files:                               ;
*     TAF -  name of the text file containing the temporal allocation factors   ;
*                                                                               ;
*  MergeTAF function                                                            ;
*     input text files:                                                         ;
*         SCClink - name of text file containing the correspondance between     ;
*                   scc_ams codes and scc codes.                                ;
*         SIClink - name of text file containing the correspondance between     ;
*                   scc_ams codes and sic codes.                                ;
*         MACTlink - name of text file containing the correspondance between    ;
*                    scc_ams codes and MACT category codes.                     ;
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
* is not either ASPEN or ISC(ST3).                                  ;
********************************************************************;

%global model;
%let Model    = %sysget(MODEL);

%IF (%upcase("&model") = "ASPEN") %THEN %DO;

   %global insas outsas numtaf TAF SCClink SIClink MACTlink mob6arry;

   %let mob6arry = 0;
   %let numtaf   =  8;
   %let insas    =  %sysget(INSAS);
   %let outsas   =  %sysget(OUTSAS);
   %let TAF      =  %sysget(TAF);
   %let SCClink  =  %sysget(SCCLINK);
   %let SIClink  =  %sysget(SICLINK);
   %let MACTlink =  %sysget(MACTLINK);

   %put _user_;

*** Set directories ***;

   Libname in_data "$IN_DATA";
   Libname outdata "$OUTDATA";
   Filename reffile "$REFFILE";

%END;

%ELSE %IF (%upcase("&model") = "ISC") %THEN %DO;

   %global insas outsas numtaf TAF SCClink SIClink MACTlink mob6arry n_ann ndays;

   %let mob6arry = 0;
   %let n_ann    = 0;
   %let numtaf   =  288;
   %let insas    =  %sysget(INSAS);
   %let outsas   =  %sysget(OUTSAS);
   %let TAF      =  %sysget(TAF);
   %let SCClink  =  %sysget(SCCLINK);
   %let SIClink  =  %sysget(SICLINK);
   %let MACTlink =  %sysget(MACTLINK);
   %let leapyear =  %sysget(LEAPYEAR);

   %IF &leapyear %THEN %DO;
      %let ndays = 366;
   %END;
   %ELSE %DO;
      %let ndays = 365;
   %END;
   %put _user_;

*** Set directories ***;

   Libname in_data "$IN_DATA";
   Libname outdata "$OUTDATA";
   Filename reffile "$REFFILE";

   proc contents data=in_data.&insas out=varnames noprint;
   run;
   data _null_;
      set varnames;
      where name= 'summer1';
      call symput('mob6arry',1);
   run;

   ** TEST WHETHER ENTIRE INVENTORY IS MOB6 ARRAY           *;
   ** IF NOT, MUST MAKE 2 SEPARATE PASSES THROUGH PtTemporal *;
   
   %IF &mob6arry %THEN %DO;
      proc sql noprint;
         select count(*)
            into :n_ann
            from in_data.&insas(where=(summer1 = .));
      quit;
      %if &n_ann %then %do; 
         data annual season;
            set in_data.&insas;
            if summer1 = . then output annual;
            else output season;
         run;
      %end;
   %END;

%END;

%ELSE %DO;
   put @1 'Error!!! Model designation is incorrect.';
   STOP;
%END;


%MEND GetInfo;

********************************************************************;
********************************************************************;


%MACRO ASPENTAF;

********************************************************************;
*   Macro: ASPENTAF                                                  ;
*  This macro reads the ASPEN temporal allocation factor file, calculates ;
*  3-hour factors from the 1-hour factors, and checks that the      ;
*  factors for each observation are normalized (add to 1).          ;
********************************************************************;
;

***  Read ASPEN Temporal Allocation Factors;

data TAF;
   length SCC_AMS $10;
   array tafA{24} taf1-taf24;
   array tafs{8} tafs1-tafs8;
   retain tafs;

   infile reffile("&TAF..txt") lrecl=210 firstobs=3;
   input SCC_AMS $ 1-10
   %do J = 1 %to 24;
       tafA{&J}
   %end;
   ;
   SCC_AMS = left(SCC_AMS);

***  Calculate 3-hourly TAFs from hourly TAFs ***;
   %do I = 1 %to 8;
      J = 3*(&I-1) + 1;
      tafs{&I} = ( tafA{J} + tafA{J+1} + tafA{J+2} ) * 8;
   %end;

   drop taf1-taf24 J;

proc sort data=TAF nodupkey; by SCC_AMS; run;


*** Ensure that all TAFs are normalized ***;

data normTAF;
   array tafs{8} tafs1-tafs8;
   set TAF;

   avetaf = sum(of tafs1-tafs8) / 8;

   %do J = 1 %to 8;
       tafs{&J} = tafs{&J} / avetaf;
   %end;

   if avetaf<0.9 or avetaf > 1.1 then Lavetaf=1;

title2 "Temporal Allocation Factor Sums < 0.9 or > 1.1  (READTAF)";
proc print data=normTAF;
   var SCC_AMS avetaf tafs1-tafs8;
   where Lavetaf=1;


*** Remove normalization variables ***;

data normTAF;
  set normTAF;
  drop avetaf lavetaf;

run;

*** Delete temporay files ***;

proc datasets;
   delete TAF;
quit;


%MEND ASPENTAF;

********************************************************************;
********************************************************************;

%MACRO ISCTAF(mob6);

********************************************************************;
*   Macro: iscTAF                                                   ;
*  This macro reads the ISC temporal allocation factor file,        ;
*  and checks that the factors for each observation are normalized  ;
*  (add to 1).                                                      ;
********************************************************************;
;

***  Read ISC Temporal Allocation Factors;

data TAF;
   length SCC_AMS $10;
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


   infile reffile("&TAF..txt") lrecl=210 firstobs=3 EOF=LASTREC missover; *lrecl=250 before 9/18;


*..... read the Spring weekday fractions;
   input @1 SCC_AMS $10.
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
         put @1 'daytype = ' daytype ' seatype = ' seatype ' scc_ams = ' scc_ams;
         stop;
      end;

   do I = 1 to 24;
      spwk(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Summer weekday fractions;
   input @1 SCC_AMS $10.
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
         put @1 'daytype = ' daytype ' seatype = ' seatype ' scc_ams = ' scc_ams;
         stop;
      end;

   do I = 1 to 24;
      suwk(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Fall weekday fractions;
   input @1 SCC_AMS $10.
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
         put @1 'daytype = ' daytype ' seatype = ' seatype ' scc_ams = ' scc_ams;
         stop;
      end;

   do I = 1 to 24;
      fawk(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Winter weekday fractions;
   input @1 SCC_AMS $10.
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
         put @1 'daytype = ' daytype ' seatype = ' seatype ' scc_ams = ' scc_ams;
         stop;
      end;

   do I = 1 to 24;
      wiwk(I) = dayfrac * seafrac * hr(I);
   end;



*..... read the Spring Saturday fractions;
   input @1 SCC_AMS $10.
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
         put @1 'daytype = ' daytype ' seatype = ' seatype ' scc_ams = ' scc_ams;
         stop;
      end;

   do I = 1 to 24;
      spsa(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Summer Saturday fractions;
   input @1 SCC_AMS $10.
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
         put @1 'daytype = ' daytype ' seatype = ' seatype ' scc_ams = ' scc_ams;
         stop;
      end;

   do I = 1 to 24;
      susa(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Fall Saturday fractions;
   input @1 SCC_AMS $10.
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
         put @1 'daytype = ' daytype ' seatype = ' seatype ' scc_ams = ' scc_ams;
         stop;
      end;

   do I = 1 to 24;
      fasa(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Winter Saturday fractions;
   input @1 SCC_AMS $10.
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
         put @1 'daytype = ' daytype ' seatype = ' seatype ' scc_ams = ' scc_ams;
         stop;
      end;

   do I = 1 to 24;
      wisa(I) = dayfrac * seafrac * hr(I);
   end;



*..... read the Spring Sunday fractions;
   input @1 SCC_AMS $10.
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
         put @1 'daytype = ' daytype ' seatype = ' seatype ' scc_ams = ' scc_ams;
         stop;
      end;

   do I = 1 to 24;
      spsu(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Summer Sunday fractions;
   input @1 SCC_AMS $10.
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
         put @1 'daytype = ' daytype ' seatype = ' seatype ' scc_ams = ' scc_ams;
         stop;
      end;

   do I = 1 to 24;
      susu(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Fall Sunday fractions;
   input @1 SCC_AMS $10.
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
         put @1 'daytype = ' daytype ' seatype = ' seatype ' scc_ams = ' scc_ams;
         stop;
      end;

   do I = 1 to 24;
      fasu(I) = dayfrac * seafrac * hr(I);
   end;


*..... read the Winter Sunday fractions;
   input @1 SCC_AMS $10.
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
         put @1 'daytype = ' daytype ' seatype = ' seatype ' scc_ams = ' scc_ams;
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


LASTREC: SCC_AMS = left(SCC_AMS);

   keep SCC_AMS tafs1-tafs288;
   if SCC_AMS ne '';

run;

proc sort data=TAF nodupkey; by SCC_AMS; run;

************************************************************************************************;
*** Ensure that all TAFs are normalized ***;
/*** THIS NORMALIZATION IS FOR AN ARRAY OF 24 Seasonal EMISSION VALUES ***/
%IF &mob6 %THEN %DO;
  
   data normTAF;
      array tafs{288} tafs1-tafs288;
      set TAF;

   /* Start with tons per hour for each season and daytype
      Yields tons per season --------------------*/

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

      winter  = sum(winwk * 5,winsa,winsu) * 13 / (24*7*13); **JAN2003: MOB6 emissions already EXACT for each season *(364/&ndays);
      spring  = sum(sprwk * 5,sprsa,sprsu) * 13 / (24*7*13);
      summer  = sum(sumwk * 5,sumsa,sumsu) * 13 / (24*7*13);
      fall    = sum(falwk * 5,falsa,falsu) * 13 / (24*7*13);

      if winter > 0 and summer > 0 and spring > 0 and fall > 0;

      do I=1 to 24;
         tafs(I)     = tafs(I)     / winter;     *winter weekdays;
         tafs(I+96)  = tafs(I+96)  / winter;  *winter saturdays;
         tafs(I+192) = tafs(I+192) / winter; *winter sundays;
         tafs(I+48)  = tafs(I+48)  / summer;  *summer weekdays;
         tafs(I+144) = tafs(I+144) / summer; *summer saturdays;
         tafs(I+240) = tafs(I+240) / summer; *summer sundays;
         tafs(I+24)  = tafs(I+24)  / spring;  *spring weekdays;
         tafs(I+120) = tafs(I+120) / spring; *spring saturdays;
         tafs(I+216) = tafs(I+216) / spring; *spring sundays;
         tafs(I+72)  = tafs(I+72)  / fall  ;  *autumn weekdays;
         tafs(I+168) = tafs(I+168) / fall  ; *autumn saturdays;
         tafs(I+264) = tafs(I+264) / fall  ; *autumn sundays;
      end;
   run;
   *** Remove normalization variables ***;

   data normTAF;
     set normTAF;
     drop winter spring summer fall winwk sprwk sumwk falwk winsa sprsa sumsa falsa winsu sprsu sumsu falsu;
   run;
%END;
/*** THIS NORMALIZATION IS FOR 1 EMISSION VALUE ***/
%ELSE %THEN %DO;
   data normTAF;
      array tafs{288} tafs1-tafs288;
      set TAF;

   /* Start with tons per hour for each season and daytype
      Yields tons per daytype per season --------------------*/

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
   %IF &ndays = 366 %THEN %DO;
      title3 "MOBILE6 TAFS SHOULD HAVE A SUM -variable alltafs- OF 8784 (leap year)";
      title4 "** where 8784=24(hrs/day)*7(days/week)*13(weeks/season)*4(seasons/year)*(&ndays/364)";
   %END;
   %ELSE %DO;
      title3 "MOBILE6 TAFS SHOULD HAVE A SUM -variable alltafs- OF 8760 (non-leap year)";
      title4 "** where 8760=24(hrs/day)*7(days/week)*13(weeks/season)*4(seasons/year)*(&ndays/364)";
   %END;
   proc print data=normTAF;
      var SCC_AMS alltafs tafs1-tafs288;
      where alltafs not between 0.9 and 1.1;
   run;
   title;
   *** Remove normalization variables ***;

   data normTAF;
     set normTAF;
     drop alltafs winter spring summer fall winwk sprwk sumwk falwk winsa sprsa sumsa falsa winsu sprsu sumsu falsu;
   run;
%END;

*** Delete temporay files ***;

proc datasets;
   delete TAF;
quit;

%MEND ISCTAF;

********************************************************************;
********************************************************************;

%MACRO MergeTAF(mob6,inpemis);

********************************************************************;
*   Macro MergeTAF                                                  ;
*  This macro will merge with TAF file by the SCC_AMS code created  ;
*  from the SCC on the inventory, then by the SCC_AMS code based on ;
*  the link file between SCC and SCC_AMS, then by the SCC_AMS code  ;
*  based on the file between SIC and SCC_AMS, then by the SCC_AMS   ;
*  code based on the file between MACTcode and SCC_AMS.             ;
********************************************************************;
;

***  Prepare emissions inventory data set for merging ***;

proc sort data=&inpemis out=inventry(keep=scc sic mact);
   by scc sic mact;

data inventry;
   set inventry;
    by scc sic mact;

   length scc_ams $10. ;
   if length(scc) >= 8 then scc_ams = scc;

   if first.mact;


***  Merge TAF by SCC_AMS based on the SCC in the inventory file. ***;

proc sort data=normTAF;
   by SCC_AMS;

proc sort data=inventry;
   by SCC_AMS;

data match1 nomatch1(keep=scc sic mact);

   merge inventry(in=in1) normTAF(in=in2);
   by SCC_AMS;

   if in1 and in2 then output match1;
   if in1 and not in2 then output nomatch1;


***  Merge TAF by SCC_AMS based on the SCC to SCC_AMS link file. ***;

data scclink;
   infile reffile("&SCClink..txt") firstobs=2;

   input @1 SCC $8.
         @11 SCC_AMS $10.
   ;

proc sort data=scclink;
   by scc;

proc sort data=nomatch1;
   by scc;

data sccmatch;
   merge nomatch1(in=A) scclink(in=B);
   by scc;

   if A;

proc sort data=sccmatch;
   by scc_ams;

data match2 nomatch2(keep=scc sic mact);

   merge sccmatch(in=in1) normTAF(in=in2);
   by SCC_AMS;

   if in1 and in2 then output match2;
   if in1 and not in2 then output nomatch2;


***  Merge TAF by SCC_AMS based on the SIC to SCC_AMS link file. ***;

data siclink;
   infile reffile("&SIClink..txt") firstobs=2;

   input @1 SIC $4.
         @7 SCC_AMS $10.
   ;

proc sort data=siclink;
   by sic;

proc sort data=nomatch2;
   by sic;

data sicmatch;
   merge nomatch2(in=A) siclink(in=B);
   by sic;

   if A;

proc sort data=sicmatch;
   by scc_ams;

data match3 nomatch3(keep=scc sic mact);

   merge sicmatch(in=in1) normTAF(in=in2);
   by SCC_AMS;

   if in1 and in2 then output match3;
   if in1 and not in2 then output nomatch3;


***  Merge TAF by SCC_AMS based on the MACT code to SCC_AMS file. ***;

data mactscc(keep=mactcat scc_ams);
   infile reffile("&MACTlink..txt") FIRSTOBS=2 missover;

   input mactcat $ 1-7 scc_ams $9-18 s_mact 20-22;
   mactcat = compress(mactcat);
   if s_mact eq .; *AUG 2003: Allows COPAX and PtTemporal to specify different TAFs (via SCC) for same MACT;

*** Multiple SCC_AMS codes can link to one MACT.  ***;
*** Use the first SCC_AMS linked to each MACT.    ***;

proc sort data=mactscc;
   by mactcat;

data mactscc;
   set mactscc;
   by mactcat;
   if first.mactcat;

data nomatch3;
   set nomatch3;
   length mactcat $7.;
   mactcat = substr(mact,1,7);
   mactcat = compress(mactcat);

proc sort data=nomatch3;
   by mactcat;

data mactmtch;
   merge nomatch3(in=A) mactscc(in=B);
   by mactcat;

   if A;

proc sort data=mactmtch(drop=mactcat);
   by scc_ams;

data match4 nomatch4(keep=scc sic mact);

   merge mactmtch(in=in1) normTAF(in=in2);
   by SCC_AMS;

   if in1 and in2 then output match4;
   if in1 and not in2 then output nomatch4;

run;


***  Merge TAF by SCC_AMS based on the 6-digit SCC from the inventory file. ***;

data scc6;
   set nomatch4;
   length scc6 $6.;
   scc6 = substr(scc,1,6);

proc sort data=scc6;
  by scc6;

data tafscc6;
   length scc6 $6.;
   set normTAF;
   scc6 = substr(scc_ams,1,6);

***  Many temporal allocation profiles could be associated with a given ***;
***   6-digit SCC.  Use the first profile                               ***;

proc sort data=tafscc6;
   by scc6;

data tafscc6;
   set tafscc6;
   by scc6;
   if first.scc6;

*** PROVIDE CATCH FOR MOBILE6 SCCs NOT IN TAF FILE;
*** MOBILE6 EMISSIONS ARE ALREADY IN tons/hour;

data match5(drop=SCC6)
     outdata.notaf(drop=tafs1-tafs&numtaf);
   array tafs{&numtaf} tafs1-tafs&numtaf;

   merge scc6(in=in1) TAFscc6(in=in2);
   by SCC6;

***  Assign uniform profile to non-matches and save for further investigation ***;
   if in1 and not in2 then do;
      %if (%upcase("&model") = "ASPEN") %then
      %do I = 1 %to &numtaf;
         tafs{&I} = 1;
      %end;
      %else %if (%upcase("&model") = "ISC") %then 
      %do I = 1 %to &numtaf;
         %IF &mob6 %THEN %DO;
            tafs{&I} = 1;
         %END;
         %ELSE %DO;
         ***Prior to JAN2003:  8760 was used (# hours in 365 days). &ndays*24 must be used for consistency;
            tafs{&I} = 1 / (&ndays*24); *JAN2003 -ensures conservation of mass;
         %END;
      %end;
      output outdata.notaf;
   end;

   if in1 then output match5;

*** Join together all file containing matched and defaulted temporal allocation factors ***;

data _null_;
   set sashelp.vtable(keep=nobs libname memname);
   where libname = 'OUTDATA' and memname eq "NOTAF";
   call symput('keeptaf',nobs);
run;
%IF not &keeptaf %THEN %DO;
   proc datasets library=outdata;
      delete notaf;
   quit;
%END;

data withTAF;
   set match1 match2 match3 match4 match5;

proc sort data=withTAF;
   by scc sic mact;

proc sort data=&inpemis out=inpsas;
   by scc sic mact;

* DYL - drop tafs for ISCST3 models, but not for ASPEN model ***;
*** NOVEMBER 2002:  MOBILE6 emissions are already in an array of hourly values by season (but not daytype) *;

%IF &mob6 %THEN %DO;
   data temp1(keep=saroad emis totemis) 
        %IF &n_ann %THEN %DO;
           out_season(drop=I spring1-spring24 summer1-summer24 autumn1-autumn24 winter1-winter24 tafs1-tafs&numtaf)
        %END;
        %ELSE %DO;
           outdata.&outsas(drop=I scc_ams totemis spring1-spring24 summer1-summer24 autumn1-autumn24 winter1-winter24 tafs1-tafs&numtaf)
        %END;
        outdata.taferror_mob6(drop=I);

      array tafs{&numtaf} tafs1-tafs&numtaf;
      array temis{&numtaf} temis1-temis&numtaf;
      array spring{24} spring1-spring24;
      array summer{24} summer1-summer24;
      array autumn{24} autumn1-autumn24;
      array winter{24} winter1-winter24;
      merge inpsas(in=A) withTAF(in=B);
      by scc sic mact;
   
      do I=1 to 24;
         temis(I)     = winter(I) * tafs(I);     *winter weekdays;
         temis(I+96)  = winter(I) * tafs(I+96);  *winter saturdays;
         temis(I+192) = winter(I) * tafs(I+192); *winter sundays;
         temis(I+48)  = summer(I) * tafs(I+48);  *summer weekdays;
         temis(I+144) = summer(I) * tafs(I+144); *summer saturdays;
         temis(I+240) = summer(I) * tafs(I+240); *summer sundays;
         temis(I+24)  = spring(I) * tafs(I+24);  *spring weekdays;
         temis(I+120) = spring(I) * tafs(I+120); *spring saturdays;
         temis(I+216) = spring(I) * tafs(I+216); *spring sundays;
         temis(I+72)  = autumn(I) * tafs(I+72);  *autumn weekdays;
         temis(I+168) = autumn(I) * tafs(I+168); *autumn saturdays;
         temis(I+264) = autumn(I) * tafs(I+264); *autumn sundays;
      end;

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

   /* Yields tons per season ---------------------------*/

      %IF &ndays = 366 %THEN %DO;
         winter_  = sum(winwk * 5,winsa,winsu) * 13;     * leap years;
      %END;
      %ELSE %DO;
      winter_  = sum(winwk * 5,winsa,winsu) * (90/7); * non-leap years;
      %END;
      spring_  = sum(sprwk * 5,sprsa,sprsu) * (92/7); *equals 92 days-same every year;
      summer_  = sum(sumwk * 5,sumsa,sumsu) * (92/7); *equals 92 days-same every year;
      fall_    = sum(falwk * 5,falsa,falsu) * 13; *equals 91 days-same every year;

   /* Yields tons per year ---------------------------*/

      totemis = sum(winter_,spring_,summer_,fall_);  * (&ndays/364) **JAN2003: DO NOT ADJUST for converting to annual emissions *;
      drop winwk sprwk sumwk falwk winsa sprsa sumsa falsa winsu sprsu sumsu falsu winter_ spring_ summer_ fall_;
      if A then do;
         %IF &n_ann %THEN %DO;
            output out_season;
         %END;
         %ELSE %DO;
           output outdata.&outsas;
         %END;
         output temp1;
      end;
      if A and not B then output outdata.taferror_mob6;
   run;
   data _null_;
      set sashelp.vtable(keep=nobs libname memname);
      where libname = 'OUTDATA' and memname eq "TAFERROR_MOB6";
      call symput('keeperr',nobs);
   run;
   %IF not &keeperr %THEN %DO;
      proc datasets library=outdata;
         delete taferror_mob6;
      quit;
   %END;

%END;
%ELSE %DO;
   %if (%upcase("&model") = "ASPEN") %then %do;
      data outdata.&outsas outdata.taferror;
   %end;
   %else %if (%upcase("&model") = "ISC") %then %do;
      %IF &inpemis ne annual %THEN %DO;
         data outdata.&outsas(drop=tafs1-tafs&numtaf) outdata.taferror;
      %END;
      %ELSE %DO;
         /* Processing annual portion of ISCST3 emissions separately from MOBILE6 array */
         data out_annual(drop=tafs1-tafs&numtaf) outdata.taferror;
      %END;
   %end;

      array tafs{&numtaf} tafs1-tafs&numtaf;
      array tEmisA{&numtaf}  tEmis1-tEmis&numtaf;

      merge inpsas(in=A) withTAF(in=B);
      by scc sic mact;

   ***  Calculate temporally allocate emissions                                 ***;
   ***  For ASPEN model, unit  of temporally allocated emission is tons/year    ***;
   ***  For ISCST3 model, units of temporally allocation emissions is tons/hour ***;

      %do I = 1 %to &numtaf;
         tEmisA{&I} = emis * tafs{&I};
      %end;

   %if (%upcase("&model") = "ASPEN") %then %do;
      if abs(sum(of tafs1-tafs8)-8) > 0.05 then do;
         put " TAF sum NE 1 " tafs1-tafs&numtaf SCC= SIC= MACT= ;
         STOP;
      end;
   %end;

      if A then output 
         %IF &inpemis ne annual %THEN %DO; 
            outdata.&outsas
         %END;
         %ELSE %DO;
            out_annual;
         %END;;
      if A and not B then output outdata.taferror;
   run;

   data _null_;
      set sashelp.vtable(keep=nobs libname memname);
      where libname = 'OUTDATA' and memname eq "TAFERROR";
      call symput('keeperr',nobs);
   run;
   %IF not &keeperr %THEN %DO;
      proc datasets library=outdata;
         delete taferror;
      quit;
   %END;

   *** QA Temporal allocation ***;

   %if (%upcase("&model") = "ASPEN") %then %do;
      data temp1(keep=saroad emis totemis);
         set outdata.&outsas;
         array temis{&numtaf} temis1-temis&numtaf;
         totemis = sum(of temis1-temis&numtaf)/&numtaf;
      run;
   %end;

   %else %if (%upcase("&model") = "ISC") %then %do;
      data temp1(keep=saroad emis totemis);

         %IF &inpemis ne annual %THEN %DO;
            set outdata.&outsas;
         %END;
         %ELSE %DO;
            /* Processing annual portion of ISCST3 emissions separately from MOBILE6 array */
            set out_annual;
         %END;

         array temis{&numtaf} temis1-temis&numtaf;
   
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

   /* Yields tons per season ---------------------------*/

        winter  = sum(winwk * 5,winsa,winsu) * 13;
        spring  = sum(sprwk * 5,sprsa,sprsu) * 13;
        summer  = sum(sumwk * 5,sumsa,sumsu) * 13;
        fall    = sum(falwk * 5,falsa,falsu) * 13;

   /* Yields tons per year ---------------------------*/

       totemis = sum(winter,spring,summer,fall)*(&ndays/364); **JAN2003;  
      run;
   %end;
%END;

   proc sort data=temp1;
     by saroad;

   proc means sum n data=temp1 noprint;
     by saroad;
     var emis totemis;
     output out=totalsum(drop=_type_ _freq_) sum=emis totemis n=count;

   title;

   %IF &mob6 %THEN %DO;
      title 'Summary of Temporally Allocated and Unallocated Emissions by SAROAD Code';
      title2 "***MOBILE6 EMISSIONS ONLY*** DOES NOT INCLUDE ANNUAL EMISSIONS";
   %END;
   %ELSE %DO;
      %IF &mob6arry %THEN %DO;
         title 'Summary of Temporally Allocated and Unallocated Emissions by SAROAD Code';
         title2 "***ANNUAL EMISSIONS ONLY*** DOES NOT INCLUDE MOBILE6 EMISSIONS";
      %END;
      %ELSE %DO;
         title 'Summary of Temporally Allocated and Unallocated Emissions by SAROAD Code';
      %END;
   %END;

   proc print data=TotalSum  noobs split='*';
      format emis totemis best12.;
      label saroad = 'SAROAD code'
            emis = 'Unallocated*Emissions*Before TAFs'
            totemis = 'Total Allocated*Emissions*After TAFs'
            count = 'Number of*Records';
       sum emis totemis;
   run;
   title;

   *** Delete temporary data sets ***;

   proc datasets;
      delete match1 nomatch1 match2 nomatch2 match3 nomatch3 match4 nomatch4 match5
             normTAF scc6 tafscc6 inventry withTAF sccmatch sicmatch mactmtch mactscc
             scclink siclink temp1;
   quit;


%MEND MergeTAF;

*****************************************************************;
*****************************************************************;

%MACRO CONCAT;
***************************************************************;
*  Macro CONCAT                                                ;
*  This macro is used when ISCST3 input emissions contain BOTH ;
*  annual and Mobile6 winter-summer arrays of hourly emissions ;
***************************************************************;

   data outdata.&outsas(drop=spring1-spring24 summer1-summer24 autumn1-autumn24 winter1-winter24);
      set out_season out_annual;

   proc sort data=outdata.&outsas out=temp1;
     by saroad;

   proc summary sum n data=temp1;
     by saroad;
     var emis;
     output out=totalsum(drop=_type_ _freq_) sum=emis n=count;

   proc print data=TotalSum  noobs split='*';
      format emis best12.;
      label saroad = 'SAROAD code'
            emis = 'Total Allocated*Emissions*After TAFs'
            count = 'Number of*Records';
       sum emis ;
       title 'Summary of Temporally Allocated and Unallocated Emissions by SAROAD Code';
       title2 "INCLUDES EMISSIONS FROM BOTH MOBILE6 AND ANNUAL SOURCES";
   run;

%MEND CONCAT;

*****************************************************************;
*****************************************************************;


%MACRO MainProg;

%if (%upcase("&model") = "ASPEN") %then %do;

   %ASPENTAF
   %MergeTAF(mob6=0,inpemis=in_data.&insas);

%end;
%else %if (%upcase("&model") = "ISC") %then %do;
   %IF &mob6arry %THEN %DO;
      %IF &n_ann %THEN %DO;

         /* Mix of 4-season AND annual emissions */

         %ISCTAF(mob6=0)
         %MergeTAF(mob6=0,inpemis=annual);
         %ISCTAF(mob6=1)
         %MergeTAF(mob6=1,inpemis=season);

         /** Need to sum output emissions **/

         %CONCAT

      %END;
      %ELSE %DO;

         /* Strictly 4-season emissions */

         %ISCTAF(mob6=1)
         %MergeTAF(mob6=1,inpemis=in_data.&insas)

      %END;
   %END;
   %ELSE %DO;

   /* Strictly annual emissions */

      %ISCTAF(mob6=0)
      %MergeTAF(mob6=0,inpemis=in_data.&insas);

   %END;
%end;


%MEND MainProg;

*****************************************************************;


*****************************************************************;
*                                                                ;
*                   MAIN PROGRAM                                 ;
*                                                                ;
*****************************************************************;
;

OPTIONS mprint symbolgen;

%GetInfo

%MainProg

run;
