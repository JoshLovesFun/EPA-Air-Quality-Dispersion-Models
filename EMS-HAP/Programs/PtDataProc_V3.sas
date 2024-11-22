*********************************************************************************;
*   Program:   PtDataProc - PC and Unix Version                                 ;
*   Author:    Diane Linderman, Bill Battye, and Steve Fudge, EC/R              ;
*   Date:      Original version June 7, 1999, updated through 9/2000            ;
*              mod July, 1999 - add location defaults by zip code, state FIPS,  ;
*                               and state postal code                           ;
*              mod August, 1999 - add include program to find missing FIPS by   ;
*                                 lat/lon                                       ;
*              mod July, 2000 - add ISC option to process emissions for ISCST3  ;
*              mod May, 2002  - Remove ACT_ID variable -use SITE_ID instead     ;
*              mod Oct, 2002  - Prevent permanent sort of input data and allow  ;
*                               lat=y and lon=x when dolocate = 0               ;
*              mod April, 2003 - Modified State-level FIPs check and merged in  ;
*                               all location QA include files.  Added batch file;
*                               variables for counties and bound6 SAS data.     ;
*                               Removed batch file variables for include and    ;
*                               mapping files directories.  Added include files ;
*                               -allows for easier packaging.  Made tract array ;
*                               file more dynamic in format.  Now removes all   ;
*                               empty diagnostic SAS and text data outputs.     ;
*              mod November, 2003 - fixed distance calculation for location QA  ;
*                                                                               ;
*   The program is divided into three primary macros: Locate                    ;
*                                                     Stack                     ;
*                                                     ProcFile                  ;
*                                                                               ;
*      Locate: This macro provides quality assurance of location data and is    ;
*              divided into 5 macros:                                           ;
*              Convert - ** For ASPEN model processing only.  This macro        ;
*                        calculates the latitude and longitude based on the     ;
*                        xy_type (UTM or LATLON) on all points in the following ;
*                        manner:                                                ;
*                        a) converts utm to lat/lon in decimal degrees          ;
*                        b) converts lat/lon in degrees, minutes, second to     ;
*                           decimal degrees                                     ;
*                        c) separates out data with missing x or y values       ;
*                        d) separates out data with incomplete or inconsistent  ;
*                           location data                                       ;
*              ISCcnvrt - ** For ISCST3 processing only.  This macro converts   ;
*                         all inventory locational data to UTM coordinates based;
*                         on the value of "REF_ZONE" provided in the batch file.;
*              ChLocate - ** For ASPEN model processing only.  This macro checks;
*                         the lat/lon of each point for reasonableness.  If     ;
*                         identifies all points outside of specific lat/lon     ;
*                         ranges.                                               ;
*              DflLocat - ** For ASPEN model processing only.  This macro       ;
*                         determines default locations for all points identified;
*                         in Convert and ChLocate to have invalid location data.;
*              GetFIPS  - ** For ASPEN model processing only.  This macro merges;
*                         all data sets with valid or defaulted location data   ;
*                         together.  It identifies the points with missing or   ;
*                         zero FIPS and, within the include program findFIPS,   ;
*                         determines the FIPS using the lat/lon                 ;
*              Merge - This macro merges the data set created in GetFIPS with   ;
*                      the inventory file                                       ;
*                                                                               ;
*      Stack: This macro provides quality assurance of stack parameter data and ;
*             is divided into two macros.                                       ;
*             ChStack - This macro creates a QA file of points with unreasonable;
*                       stack parameters                                        ;
*             DftStack - This macro allows the user to default missing or zero  ;
*                        stack parameters using SCC-based defaults or SIC-based ;
*                        defaults.  Any missing or unreasonable stack parameters;
*                        present after any defaulting procedure are set to      ;
*                        general default values provided by the user.           ;
*                        Fugitive and horizontal stacks still missing or zero   ;
*                        get different treatment (hard-coded defaults)          ;
*                                                                               ;
*     ProcFile: This macro allows the user to reduce the size of the point      ;
*               source inventory file used in subsequent processing through the ;
*               following 2 macros:                                             ;
*             SetVar - This macro allows to user to select specific variables to;
*                      be retained (in addition variables required for          ;
*                      the subsequent processing through the other EMS-HAP      ;
*                      point source programs)                                   ;
*             WindData - This macro allows the user the window the data to      ;
*                        include only those records with lat/lon data and       ;
*                        non-zero emissions data.  Note that the data may       ;
*                        already been windowed for lat/lon through              ;
*                        the macro Locate.                                      ;
*                        FOR ISCST3, records with missing or zero UTMX or UTMY  ;
*                        coordinates are filtered out.                          ;
*                                                                               ;
*   The user provides the names of directories, input and output SAS datasets,  ;
*   external reference files (SAS data sets and text files), and included       ;
*   programs.  The user also provides function controls and inputs for various  ;
*   individual macros.                                                          ;
*                                                                               ;
*   Directory names:                                                            ;
*      in_data - input SAS data set directory                                   ;
*      outdata - output SAS data set directory                                  ;
*      reffile - reference SAS data set directory                               ;
*      reftext - reference text file directory                                  ;
*                                                                               ;
*   Input and Output SAS data set names:                                        ;
*      insas - input SAS data set name                                          ;
*      outsas - output SAS data set name created from either Locate or Stack    ;
*      final - final output SAS data set created from ProcFile                  ;
*                                                                               ;
*   Locate function controls and inputs:                                        ;
*      controls:                                                                ;
*         DoLocate - set to 1 to use the Locate macro in the data processing,   ;
*                    set to 0 to skip                                           ;
*                                                                               ;
*      input files (***ASPEN processing only):                                  ;
*         zip - name of SAS data set containing zip codes and centroid locations;
*         cntycent - name of SAS data set containing FIPS, county centroids, and;
*                  county radii;
*         tracts - name of SAS data set containing a random array of tracts for ;
*                  for each county.  For 1999, file is ordered by tract size    ;
*         trctinfo - name of SAS data set containing tract information,         ;
*                    specifically the tract centroid                            ;
*                                                                               ;
*   Stack function controls and inputs:                                         ;
*      controls:                                                                ;
*         DoStack - set to 1 to use the Stack macro in the data processing, set ;
*                   to 0 to skip                                                ;
*         DoSCCDef - set to 1 to use SCC-based default stack parameters to      ;
*                    replace missing values.                                    ;
*         DoSICDef - set to 1 to use SIC-based default stack parameters to      ;
*                    replace missing vaues.                                     ;
*                                                                               ;
*      input files:                                                             ;
*         SCCDeflt - name of data set containing default stack parameters       ;
*                    (height, diameter, exit velocity, and temperature) by SCC  ;
*         SICDeflt - name of data set containing default stack parameters       ;
*                    (height, diameter, exit velocity, and temperature) by SIC  ;
*                                                                               ;
*      input data;                                                              ;
*         dfltHt - default stack height in meters (3 meters)                    ;
*         dfltVel - default stack gas exit velocity in meters/second (4 m/s)    ;
*         dfltTemp - default stack gas exit temperature in Kelvin (295 K)       ;
*         delftDia - default stack diameter in meters (0.2 m)                   ;
*                                                                               ;
*   ProcFile function controls and inputs/outputs:                              ;
*      controls:                                                                ;
*         DoSetVar -set to 1 to use the SetVar macro in the data processing, set;
*                   to 0 to skip                                                ;
*         UseList -set to 1 to use the user provided list of varaibles to be    ;
*                   kept for further processing in addition to the required     ;
*                   variable, set to 0 to not use this file                     ;
*         DoWindow -set to 1 to use the WindData macro in the data processing,  ;
*                   set to 0 to skip                                            ;
*                                                                               ;
*      input/output files:                                                      ;
*         varlist - name of the text file containing the point source inventory ;
*                   variable names and a keep code to determine if the variable ;
*                   will be kept for further processing                         ;
*         nolatlon - name of the output SAS data set containing all records with;
*                    missing lat/lon values                                     ;
*         zeroemis - name of the output SAS data set containing all records with;
*                    zero emissions data                                        ;
*                                                                               ;
********************************************************************************;
;

**********************************************************************;
**********************************************************************;


%MACRO GetInfo;

*********************************************************************;
* MACRO: GetInfo for UNIX run                                        ;
*     This macro reads a user control input file to get the names of ;
* the directories, file names and include files, as well as variables;
* to be used throughout the program.                                 ;
*     First read the MODEL variable and based on this value, read the;
* corresponding variables or stop processing if the value of MODEL is;
* neither ASPEN nor ISC.                                             ;
*********************************************************************;

%global model renameMACT;

%let Model = %sysget(MODEL);

%IF (%upcase("&model") = "ASPEN") %THEN %DO;

   %global insas outsas
           DoLocate zip cntycent counties bound6 tracts trctinfo
           DoStack DoSccDef SCCDeflt DoSICDef SICDeflt
           dlowHt dhiHt dlowVel dhiVel dlowTemp dhiTemp dlowDia dhiDia
           dfltHt dfltVel dfltTemp dfltDia
           DoSetVar UseList varlist
           DoWindow nolocate zeroemis final;

   %let insas = %sysget(INSAS);
   %let outsas = %sysget(OUTSAS);

   %let DoLocate = %sysget(DOLOCATE);
   %let zip = %sysget(ZIP);
   %let cntycent = %sysget(CNTYCENT);
   %let bound6   = %sysget(MAP_INDX);
   %let counties = %sysget(POLYGONS);
   %let tracts = %sysget(TRACTS);
   %let trctinfo = %sysget(TRCTINFO);

   %let DoStack = %sysget(DOSTACK);
   %let DoSCCDef = %sysget(DOSCCDEF);
   %let SCCDeflt = %sysget(SCCDEFLT);
   %let DoSICDef = %sysget(DOSICDEF);
   %let SICDeflt = %sysget(SICDEFLT);

   %let dlowHt = %sysget(DLOWHT);
   %let dhiHt = %sysget(DHIHT);
   %let dlowVel = %sysget(DLOWVEL);
   %let dhiVel = %sysget(DHIVEL);
   %let dlowTemp = %sysget(DLOWTEMP);
   %let dhiTemp = %sysget(DHITEMP);
   %let dlowDia = %sysget(DLOWDIA);
   %let dhiDia = %sysget(DHIDIA);

   %let dfltHt = %sysget(DFLTHT);
   %let dfltVel = %sysget(DFLTVEL);
   %let dfltTemp = %sysget(DFLTTEMP);
   %let dfltDia = %sysget(DFLTDIA);

   %let DoSetVar = %sysget(DOSETVAR);
   %let UseList = %sysget(USELIST);
   %let varlist = %sysget(VARLIST);

   %let DoWindow = %sysget(DOWINDOW);
   %let nolocate = %sysget(NOLOCATE);
   %let zeroemis = %sysget(ZEROEMIS);
   %let final = %sysget(FINAL);

   %put _user_;

*** Set Directories ***;

   Libname in_data "$IN_DATA";
   Libname outdata "$OUTDATA";

   Libname reffile "$REFFILE";

   Filename reftext "$REFTEXT";

   Filename outtext "$OUTTEXT";

%END;

%ELSE %IF (%upcase("&model") = "ISC") %THEN %DO;

   %global insas outsas DoLocate ref_zone
           DoStack DoSccDef SCCDeflt DoSICDef SICDeflt
           dlowHt dhiHt dlowVel dhiVel dlowTemp dhiTemp dlowDia dhiDia
           dfltHt dfltVel dfltTemp dfltDia
           DoSetVar UseList varlist
           DoWindow nolocate zeroemis final;

   %let insas = %sysget(INSAS);
   %let outsas = %sysget(OUTSAS);

   %let DoLocate = %sysget(DOLOCATE);
   %let ref_zone = %sysget(REF_ZONE);
   %let DoStack = %sysget(DOSTACK);
   %let DoSCCDef = %sysget(DOSCCDEF);
   %let SCCDeflt = %sysget(SCCDEFLT);
   %let DoSICDef = %sysget(DOSICDEF);
   %let SICDeflt = %sysget(SICDEFLT);

   %let dlowHt = %sysget(DLOWHT);
   %let dhiHt = %sysget(DHIHT);
   %let dlowVel = %sysget(DLOWVEL);
   %let dhiVel = %sysget(DHIVEL);
   %let dlowTemp = %sysget(DLOWTEMP);
   %let dhiTemp = %sysget(DHITEMP);
   %let dlowDia = %sysget(DLOWDIA);
   %let dhiDia = %sysget(DHIDIA);

   %let dfltHt = %sysget(DFLTHT);
   %let dfltVel = %sysget(DFLTVEL);
   %let dfltTemp = %sysget(DFLTTEMP);
   %let dfltDia = %sysget(DFLTDIA);

   %let DoSetVar = %sysget(DOSETVAR);
   %let UseList = %sysget(USELIST);
   %let varlist = %sysget(VARLIST);

   %let DoWindow = %sysget(DOWINDOW);
   %let nolocate = %sysget(NOLOCATE);
   %let zeroemis = %sysget(ZEROEMIS);
   %let final = %sysget(FINAL);

   %put _user_;

*** Set Directories ***;

   Libname in_data "$IN_DATA";
   Libname outdata "$OUTDATA";

   Filename inc_dir "$INC_DIR";
   Filename reftext "$REFTEXT";

   Filename outtext "$OUTTEXT";

%END;

%ELSE %DO;
   put @1 'Error!!! Model designation is incorrect.';
   STOP;
%END;

**** EMS-HAP Uses variable MACT not MACTcode as of MARCH 2003 ***;
proc contents data=in_data.&insas out=chkmact(keep=name) noprint; run;
%let renameMACT = 0;
data _null_;
   set chkmact end=last;
   where lowcase(name) = 'mactcode';
   call symput('renameMACT',last);
run;

%MEND GetInfo;

**********************************************************************;
**********************************************************************;


%MACRO ISCcnvrt;

**********************************************************************;
*  MACRO: ISCcnvrt (***ISCST3 model processing only)                  ;
*      This macro converts lat/lon coordinates into utm coordinates   ;
*  in meters.  The algorithm requires the longitude to be positive.   ;
**********************************************************************;
;

data validate
   (keep=x y xy_type utm_z FIPS zip_code site_id cas emrelpid);
   set in_data.&insas;

data FinalLoc missing ;
   set validate;

   length llprob $7.;

*** Process only those records with nonzero and non-missing values for the x and y-coordinates ***;
***   and valid utm_z values for record identified to containing utm corrdinates.              ***;

   if (x = . or x = 0) or (y = . or y = 0) then do;
      llprob = 'missing';
      output missing;
   end;

   else if upcase(xy_type) = 'UTM' and (utm_z = . or utm_z eq 0) then do;
      llprob = 'missing';
      output missing;
   end;


*** identify records with coordinates assumed to be in lat/lon ***;
***   1) xy_type is 'LATLON'                                   ***;
***   2) xy_type is neither 'UTM' nor 'LATLON' and utm_z variable is less than zero or missing ***;


   else if (upcase(xy_type) = 'LATLON') or
           (upcase(xy_type) ne 'UTM' and upcase(xy_type) ne 'LATLON' and (utm_z = . or utm_z <= 0)) then do;

***  set flag if xy_type is neither LATLON or UTM, but asuming the x and y values represent lat/lon. ***;

      if upcase(xy_type) ne 'LATLON' then llprob = 'LATLON';

***  determine if either the x or y value is negative, if so, then make value positive ***;

      if x < 0 then do;
         x = -x;
         llprob = 'negative';
      end;
      if y < 0 then do;
         y = -y;
         llprob = 'negative';
      end;

***  determine if the x and y values are inverted ***;

      if y > x then do;
         temp = y;
         y = x;
         x = temp;
         llprob = 'flipll';
      end;

      lat = y;
      lon = x;


*** determine if the x and y values are in degrees, minutes, seconds ***;
***   if so, then convert to decimal degrees.                        ***;

      if x > 10000 then do;
         latchar = PUT(y, 6.);
         latdeg = SUBSTR(latchar, 1, 2);
         latmin = SUBSTR(latchar, 3, 2);
         latsec = SUBSTR(latchar, 5, 2);
         lat = latdeg + latmin/60.0 + latsec/3600.0;

         lonchar = PUT(x, 7.);
         londeg = SUBSTR(lonchar, 1, 3);
         lonmin = SUBSTR(lonchar, 4, 2);
         lonsec = SUBSTR(lonchar, 6, 2);
         lon = londeg + lonmin/60.0 + lonsec/3600.0;
      end;

*.... Begin the conversion of lat and lon to utm ........;
;
     ZONE = &ref_zone;

*...... the longitude must be negative for the conversion to work;
     lon = -lon;

/*** BEGIN former ll2utm include file ********/
   /*  %inc inc_dir("ll2utm.inc"); */
*     *** where                              ;
*           lon   E longitude in decimal degrees;
*           lat   N latitude in decimal degrees  ;
*           ZONE    UTM zone override if not zero         ;
*           utmz     returned UTM zone             ;
*           utme     returned UTM easting in kilometers;
*           utmn     returned UTM northing in kilometers;

  k0=0.9996;
  a=6378206.4;
  e2=0.00676866;
  ep2=0.0068148;
  false_e=500000.0;
  dtr=3.141592654/180.0;
  if (ZONE = 0) then
      utmz = int((180+lon)/6) + 1;
  else
      utmz = ZONE;

  dl = dtr*(lon - (6.0*utmz-183.0));
  p = dtr*lat;

  sinp = sin(p);
  N = a/sqrt(1.0-e2*sinp*sinp);
  tanp = tan(p);
  T = tanp*tanp;
  cosp = cos(p);
  C = ep2*cosp*cosp;
  A1 = dl*cosp;
  M = 111132.0894*lat - 16216.94*sin(2.0*p) + 17.21*sin(4.0*p)
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
  utmx = utmx*1000;
  utmy = utmy*1000;

  DROP k0 a e2 ep2 false_e dtr dl p sinp N tanp T cosp C A1 M A2 A3 A4 A5 A6
       T2;

/***** END former ll2utm include file ***/
     output FinalLoc;
   end;

*** Identify records with coordinates assumed to be in UTM coordinates ***;
*** If they are not in the reference UTM zone, convert to lat/lon, then back to UTM in the right zone ***;
***   1) xy_type is 'UTM' and utm_z variable is nonzero                ***;
***   2) xy_type is neither 'UTM' nor 'LATLON' and utm_z variable is nonzero ***;

   else if (upcase(xy_type) = 'UTM' and (utm_z ne . and utm_z ne 0)) or
           (upcase(xy_type) ne 'UTM' and upcase(xy_type) ne 'LATLON' and utm_z ne . and utm_z ne 0) then do;

***  set flag for condition 2 above ***;

      if upcase(xy_type) ne 'UTM' then llprob = 'UTM';

***  if the x value (UTM Easting) is  larger than the y value (UTM Northing), assume the data in inverted. ***;

      if x > y then do;
         temp = y;
         y = x;
         x = temp;
         llprob = 'flipxy';
      end;

*** If necessary, convert x and y coordinates from km to meters;

      if (y < 10000) then do;
         X1 = x * 1000;
         Y1 = y * 1000;
      end;
      else do;
         X1 = x;
         Y1 = y;
         llprob = 'meters';
      end;

      utmx = X1;
      utmy = Y1;
      utmz = utm_z;

*....... If the UTM zone is not the reference zone, convert to lat-lon,
*....... then convert back to UTM relative to the reference zone;
;
      if (utm_z ne &ref_zone) then do;

         ZONE = utm_z;
/** BEGIN FORMER utm2ll include file **/
/*  %inc inc_dir("utm2ll.inc"); */

      PI = 3.141592653589793;

      A = 6378206.4;  /* RADIUS OF THE EQUATOR IN METERS (CLARKE 1866 ELLIPSOID) */

      ESQ = 0.006768657997;  /* ECCENTRICITY SQUARED (CLARKE 1866 ELLIPSOID) */

      EM2 = ESQ/(1-ESQ);  /* MINOR ECCENTRICITY SQUARED */

      K0 = 0.9996;  /* SCALE FACTOR OF THE CENTRAL MERIDIAN */

      C=500000.0;  /* FALSE EASTING */


***  Rectifying Latitude ***;

      V = 0.0000001571128261;
      RL = V*Y1;
      CRL = COS(RL);


***  Calculate latitude of foot of perpendicular from point to central meridian (in radians) ***;

      G = 0.0000002457956882;
      D = 0.005078649674;
      F = 0.0000300245646;
      FP = RL+ ((D+ (F*CRL**2) + (G*CRL**4)) * (SQRT(1.0-CRL**2)) *CRL);

      COSFP = COS(FP);  /* COSINE OF LATITUDE OF FOOT OF PERPENDICULAR */

      CFP2 = COSFP**2;  /* COSINE OF LATITUDE OF FOOT OF PERPENDICULAR SQUARED */
      ETA = 1 + (EM2 * CFP2);

*** Compute difference between the input easting and the false easting ***;

      XMC = X1 - C;
      XAK = ((1 - ESQ)/(K0 * K0)) * (XMC/A) **2;

      RADDEG = 180.0/PI;  /* DEGREES IN 1 RADIAN */

*** First latitude calculation ***;

      LAT1 = ETA * ETA * TAN(FP)/2.0;

*** Second Latitude calculation ***;

      LAT2 = (2.0 - 6.0 * EM2 + 3.0/CFP2 + (12.0 * EM2 - 9.0 * EM2 **2) * CFP2 + 6.0 * EM2 **2 * CFP2)/12.0;

*** Third latitude calculation ***;

      LAT3 = ETA * (16.0 - 72.0 * EM2 + 45.0/CFP2 **2 - 45.0 * EM2/CFP2 + 244.0 * EM2 * CFP2)/360.;


***  Calculate the latitude in radians ***;

      LATRAD = FP - LAT1 * XAK * (1.0 - LAT2 * XAK + LAT3 * XAK **2);

***  First Longitude Calculation ***;

      LONG1 = SQRT(ETA)/COSFP;

***  Second Longtitude calculation ***;

      LONG2 = ETA * (-1.0 + 2.0/CFP2 + EM2 * CFP2)/6.0;

***  Third longitude calculation ***;

      LONG3 = ETA * (1.0 + 8.0 * EM2 + 24.0/CFP2 **2 - 20.0/CFP2 - 2.0 * EM2 * CFP2)/120.0;

***  Calculate the central meridian (in radians) from the grid zone ***;

      CM = (183.0 - 6.0 *ZONE)/RADDEG;

*************************************************************************;
*  Calculate the longitude in radians                                    ;
*                                                                        ;
*  NOTE:  Add difference in the easting is less than the false easting   ;
*  ====   and substract difference in the easting is greater than the    ;
*         false easting of the grid zone.                                ;
*************************************************************************;

      if XMC lt 1 then
         LONGRAD = CM + SQRT(XAK) * LONG1 * (1.0 - XAK * LONG2 + XAK **2 * LONG3);
      if XMC gt 1 then
         LONGRAD = CM - SQRT(XAK) * LONG1 * (1.0 - XAK * LONG2 + XAK **2 * LONG3);

***   Compute latitude and longitude in degrees ***;

      LAT = LATRAD * RADDEG;
      LON = -LONGRAD * RADDEG;

drop ZONE PI A ESQ EM2 K0 C V RL CRL G D F FP COSFP CFP2 ETA XMC
     XAK RADDEG LAT1 LAT2 LAT3 LONG1 LONG2 LONG3 LATRAD LONGRAD CM X1 Y1
     LATCHAR LONCHAR LATDEG LONDEG LATMIN LONMIN LATSEC LONSEC ;
/** END FORMER utm2ll **/
         ZONE = &ref_zone;

/** BEGIN FORMER ll2utm include file **/
/*         %inc inc_dir("ll2utm.inc"); */
*     *** where                              ;
*           lon   E longitude in decimal degrees;
*           lat   N latitude in decimal degrees  ;
*           ZONE    UTM zone override if not zero         ;
*           utmz     returned UTM zone             ;
*           utme     returned UTM easting in kilometers;
*           utmn     returned UTM northing in kilometers;


  k0=0.9996;
  a=6378206.4;
  e2=0.00676866;
  ep2=0.0068148;
  false_e=500000.0;
  dtr=3.141592654/180.0;
  if (ZONE = 0) then
      utmz = int((180+lon)/6) + 1;
  else
      utmz = ZONE;

  dl = dtr*(lon - (6.0*utmz-183.0));
  p = dtr*lat;

  sinp = sin(p);
  N = a/sqrt(1.0-e2*sinp*sinp);
  tanp = tan(p);
  T = tanp*tanp;
  cosp = cos(p);
  C = ep2*cosp*cosp;
  A1 = dl*cosp;
  M = 111132.0894*lat - 16216.94*sin(2.0*p) + 17.21*sin(4.0*p)
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
  utmx = utmx*1000;
  utmy = utmy*1000;

  DROP k0 a e2 ep2 false_e dtr dl p sinp N tanp T cosp C A1 M A2 A3 A4 A5 A6
       T2;

/** END FORMER ll2utm include file **/
         utmz = &ref_zone;

      end;

      output FinalLoc;

    end;

run;


%MEND ISCcnvrt;

*******************************************************************;
**********************************************************************;


%MACRO Convert;

**********************************************************************;
*  MACRO: Convert (***ASPEN model processing only)                    ;
*      This macro convert utm coordinates into latitude and longitude ;
*  coordinates and latitutde and longitude coordinates from degrees,  ;
*  minutes, second, to decimal degrees.                               ;
**********************************************************************;
;

data validate
   (keep=x y xy_type utm_z FIPS zip_code site_id cas emrelpid);
   set in_data.&insas;

/** BEGIN FORMER validFIP INCLUDE FILE **/
/** %inc inc_dir("validFIP.inc"); */

******************************************************;
*   ValidFIP                                          ;
*                                                     ;
*   This code assigns a flag, validFIP, depending on  ;
* the validity of the state/county fip code or the    ;
* state fip code.                                     ;
******************************************************;

*** Check the validity of the state/county FIPS ***;

proc sort data=reffile.&cntycent out=county(keep=FIPS);
   by FIPS;
   
proc sort data=validate;
   by FIPS;
   
data cntyFIP(drop=stfips) NotValid;
   merge validate(in=A) county(in=B);
   by FIPS;
   
   length validFIP $6.;
   length stfips $2.;
   
   if A and B then do;
      validFIP = 'county';
      output cntyFIP;
   end;
   else if A and not(B) then do;
      stfips = substr(FIPS,1,2);
      output NotValid;
   end;
   
**APRIL 2003: no need for extra file containing state fips **;
data state;
   length stfips $2;
   set county;
   stfips = substr(fips,1,2);
run;
proc sort data=state(keep=stfips) nodupkey;
   by stfips;
   
proc sort data=NotValid;
   by stfips;
   
data stFIP NotValid;
   merge NotValid(in=A) state(in=B);
   by stfips;         

   if A and B then do;
      validFIP = 'state';
      output stFIP;
   end;
   else if A and not(B) then do;
      validFIP = 'none';
      output NotValid;
   end;
   
   drop stfips;
   
data validate;
   set cntyFIP stFIP NotValid;
   
run;
 
 
proc datasets library=work;
   delete cntyFIP stFIP NotValid county state;    

/**END FORMER validFIP INCLUDE FILE **/

data latlon missing ;
   set validate;

   length zone 8;
   length llprob $7.;

*** DYL - 5/02/01 - initialize location default flags for entire inventory ***;

   length lflag $8.;
   length FIPflag $8.;
   lflag = '';
   FIPflag = '';

   drop x y xy_type utm_z temp;

*** Process only those records with nonzero and non-missing values for the x and y-coordinates ***;
***   and valid utm_z values for record identified to containing utm corrdinates.              ***;

   if (x = . or x = 0) or (y = . or y = 0) then do;
      llprob = 'missing';
      output missing;
   end;

   else if upcase(xy_type) = 'UTM' and (utm_z = . or utm_z eq 0) then do;
      llprob = 'missing';
      output missing;
   end;

*** identify records with coordinates assumed to be in UTM coordinates ***;
***   1) xy_type is 'UTM' and utm_z variable is nonzero                ***;
***   2) xy_type is neither 'UTM' nor 'LATLON' and utm_z variable is nonzero ***;

   else if (upcase(xy_type) = 'UTM' and (utm_z ne . and utm_z ne 0)) or
           (upcase(xy_type) ne 'UTM' and upcase(xy_type) ne 'LATLON' and utm_z ne . and utm_z ne 0) then do;


***  set flag for condition 2 above ***;

      if upcase(xy_type) ne 'UTM' then llprob = 'UTM';


***  if the x value (UTM Easting) is  larger than the y value (UTM Northing), assume the data in inverted. ***;

      if x > y then do;
         temp = y;
         y = x;
         x = temp;
         llprob = 'flipxy';
      end;


*** If necessary, convert x and y coordinates from km to meters;

      if (y < 10000) then do;
         X1 = x * 1000;
         Y1 = y * 1000;

      end;
      else do;
         X1 = x;
         Y1 = y;
         llprob = 'meters';
      end;


*** Begin UTM to lat-lon conversion ***;

      ZONE = utm_z;

/** BEGIN FORMER utm2ll include file **/
/*  %inc inc_dir("utm2ll.inc"); */

      PI = 3.141592653589793;

      A = 6378206.4;  /* RADIUS OF THE EQUATOR IN METERS (CLARKE 1866 ELLIPSOID) */

      ESQ = 0.006768657997;  /* ECCENTRICITY SQUARED (CLARKE 1866 ELLIPSOID) */

      EM2 = ESQ/(1-ESQ);  /* MINOR ECCENTRICITY SQUARED */

      K0 = 0.9996;  /* SCALE FACTOR OF THE CENTRAL MERIDIAN */

      C=500000.0;  /* FALSE EASTING */


***  Rectifying Latitude ***;

      V = 0.0000001571128261;
      RL = V*Y1;
      CRL = COS(RL);


***  Calculate latitude of foot of perpendicular from point to central meridian (in radians) ***;

      G = 0.0000002457956882;
      D = 0.005078649674;
      F = 0.0000300245646;
      FP = RL+ ((D+ (F*CRL**2) + (G*CRL**4)) * (SQRT(1.0-CRL**2)) *CRL);

      COSFP = COS(FP);  /* COSINE OF LATITUDE OF FOOT OF PERPENDICULAR */

      CFP2 = COSFP**2;  /* COSINE OF LATITUDE OF FOOT OF PERPENDICULAR SQUARED */
      ETA = 1 + (EM2 * CFP2);

*** Compute difference between the input easting and the false easting ***;

      XMC = X1 - C;
      XAK = ((1 - ESQ)/(K0 * K0)) * (XMC/A) **2;

      RADDEG = 180.0/PI;  /* DEGREES IN 1 RADIAN */

*** First latitude calculation ***;

      LAT1 = ETA * ETA * TAN(FP)/2.0;

*** Second Latitude calculation ***;

      LAT2 = (2.0 - 6.0 * EM2 + 3.0/CFP2 + (12.0 * EM2 - 9.0 * EM2 **2) * CFP2 + 6.0 * EM2 **2 * CFP2)/12.0;

*** Third latitude calculation ***;

      LAT3 = ETA * (16.0 - 72.0 * EM2 + 45.0/CFP2 **2 - 45.0 * EM2/CFP2 + 244.0 * EM2 * CFP2)/360.;


***  Calculate the latitude in radians ***;

      LATRAD = FP - LAT1 * XAK * (1.0 - LAT2 * XAK + LAT3 * XAK **2);

***  First Longitude Calculation ***;

      LONG1 = SQRT(ETA)/COSFP;

***  Second Longtitude calculation ***;

      LONG2 = ETA * (-1.0 + 2.0/CFP2 + EM2 * CFP2)/6.0;

***  Third longitude calculation ***;

      LONG3 = ETA * (1.0 + 8.0 * EM2 + 24.0/CFP2 **2 - 20.0/CFP2 - 2.0 * EM2 * CFP2)/120.0;

***  Calculate the central meridian (in radians) from the grid zone ***;

      CM = (183.0 - 6.0 *ZONE)/RADDEG;

*************************************************************************;
*  Calculate the longitude in radians                                    ;
*                                                                        ;
*  NOTE:  Add difference in the easting is less than the false easting   ;
*  ====   and substract difference in the easting is greater than the    ;
*         false easting of the grid zone.                                ;
*************************************************************************;

      if XMC lt 1 then
         LONGRAD = CM + SQRT(XAK) * LONG1 * (1.0 - XAK * LONG2 + XAK **2 * LONG3);
      if XMC gt 1 then
         LONGRAD = CM - SQRT(XAK) * LONG1 * (1.0 - XAK * LONG2 + XAK **2 * LONG3);

***   Compute latitude and longitude in degrees ***;

      LAT = LATRAD * RADDEG;
      LON = -LONGRAD * RADDEG;

drop ZONE PI A ESQ EM2 K0 C V RL CRL G D F FP COSFP CFP2 ETA XMC
     XAK RADDEG LAT1 LAT2 LAT3 LONG1 LONG2 LONG3 LATRAD LONGRAD CM X1 Y1
     LATCHAR LONCHAR LATDEG LONDEG LATMIN LONMIN LATSEC LONSEC ;
/** END FORMER utm2ll **/

*** Save all records with coordinates now in latitude and longitude (in decimal degrees) ***;

      output latlon;

   end;


*** identify records with coordinates assumed to be in lat/lon ***;
***   1) xy_type is 'LATLON'                                   ***;
***   2) xy_type is neither 'UTM' nor 'LATLON' and utm_z variable is less than zero or missing ***;


   else if (upcase(xy_type) = 'LATLON') or
           (upcase(xy_type) ne 'UTM' and upcase(xy_type) ne 'LATLON' and (utm_z = . or utm_z <= 0)) then do;

***  set flag if xy_type is neither LATLON or UTM, but asuming the x and y values represent lat/lon. ***;

      if upcase(xy_type) ne 'LATLON' then llprob = 'LATLON';

***  determine if either the x or y value is negative, if so, then make value positive ***;

      if x < 0 then do;
         x = -x;
         llprob = 'negative';
      end;
      if y < 0 then do;
         y = -y;
         llprob = 'negative';
      end;

***  determine if the x and y values are inverted ***;

      if y > x then do;
         temp = y;
         y = x;
         x = temp;
         llprob = 'flipll';
      end;

*** determine if the x and y values are in degrees, minutes, seconds ***;
***   if so, then convert to decimal degrees.                        ***;

      if x > 10000 then do;
         latchar = PUT(y, 6.);
         latdeg = SUBSTR(latchar, 1, 2);
         latmin = SUBSTR(latchar, 3, 2);
         latsec = SUBSTR(latchar, 5, 2);
         lat = latdeg + latmin/60.0 + latsec/3600.0;

         lonchar = PUT(x, 7.);
         londeg = SUBSTR(lonchar, 1, 3);
         lonmin = SUBSTR(lonchar, 4, 2);
         lonsec = SUBSTR(lonchar, 6, 2);
         lon = -(londeg + lonmin/60.0 + lonsec/3600.0);
      end;

***  if x and y values are in deciaml degrees, set flag ***;

      else do;
         lat = y;
         lon = -x;
         llprob = 'decimal';
      end;

*** Save all records with coordinates now in latitude and longitude (in decimal degrees) ***;

      output latlon;

   end;
run;

proc datasets library=work;
  delete validate;
quit;


%MEND Convert;

**********************************************************************;
**********************************************************************;


%MACRO ChLocate;

************************************************************************;
*  MACRO: ChLocate  (***ASPEN model processing only)                    ;
*      This macro checks the location of each point for reasonableness  ;
*  by using general US lat/lon range checks.                            ;
************************************************************************;
;

data goodloc badloc;
  set latlon;

  if ((lat > 17.5  and lat < 49.5)  and (lon > -126.0 and lon < -64.5)) or 
     ((lat > 51.0  and lat < 72.0)  and (lon > -179.0 and lon < -129.5)) /*ALASKA*/ or
     ((lat > 18.0  and lat < 23.0)  and (lon > -163.0 and lon < -153.0)) /*HAWAII*/ then output goodloc;
  else do;
     llprob = 'bad_loc';
     output badloc;
   end;

proc datasets library=work;
  delete latlon;
quit;


%MEND ChLocate;

**********************************************************************;
**********************************************************************;


%MACRO DftLocat;

*************************************************************************;
*  MACRO: DftLocat  (***ASPEN model processing only)                     ;
*      This macro defaults locations with missing or zero x or y values  ;
*  to the center of the zip code or the county.  Uses the missing, bad   ;
*  location, and data error data set created in the macro convert.       ;
*************************************************************************;
;

data ToDeflt;
   set missing badloc;

*************************************************************************;
*  Match records by zip code to the reference file containing lat/lon for;
*  the zip code centroid. Records without zip codes or where the zip code;
*  does not match the reference file are written to NoLocate data set.   ;
*  The records that do match are written to Defltzip.                    ;
*************************************************************************;
;

proc sort data=ToDeflt;
  by zip_code;

proc sort data=reffile.&zip(keep=fips zip_code cntlon cntlat) out=zipcode(rename=(FIPS=zipFIPS));
   by zip_code;

data DefltZip(drop=cntlat cntlon zipFIPS state)
     NoZipA(drop=cntlat cntlon zipFIPS state)
     NoZipB(drop=cntlat cntlon zipFIPS state);
   merge ToDeflt(in=A) zipcode(in=B);
   by zip_code;
   length state $2.;
   if A and B then do;

/*** If zipcodes match and inventory FIPs=zipcodes FIPS, default location to zipcode centroid **/

      if FIPS = zipFIPS then do;
         lat = cntlat;
         lon = cntlon;
         lflag = 'zipcode';
         output defltzip;
      end;

/*** If zipcodes match and inventory FIPs NOT =zipcodes FIPS, BUT inventory STATE FIPS match zipcode STATE FIPS:
        default location to zipcode centroid AND, reassign FIPS **/

      else if substr(FIPS,1,2) = substr(zipFIPS,1,2) and validFIP='state' then do;
         lat = cntlat;
         lon = cntlon;
         lflag = 'zipcode';
         FIPS = zipFIPS;
         FIPflag = 'assigned';
         output defltzip;
      end;

/*** If zipcodes match but FIPS and state FIPS do NOT, try matching first-2 characters of SITE_ID to zipcode STATE FIPS.
     For example, if site_id = 'NC32456-YADDA' and zipcodes FIPS='37001' (NC), site will STILL be defaulted to zipcode **/

      else if validFIP = 'none' then do;
         state = substr(site_id,1,2);
         if substr(state,1,1) not in('0','1','2','3','4','5','6','7','8','9','0','E') then do;
            if put(stfips(state),z2.) = substr(zipFIPS,1,2) then do;
               lat = cntlat;
               lon = cntlon;
               lflag = 'zipcode';
               FIPS = zipFIPS;
               FIPflag = 'assigned';
               output defltzip;
            end;
            else output NoZipB;
         end;
         else output NoZipB;
      end;
      else output NoZipA;
   end;
   if A and not(B) then output NoZipA;
run;

*************************************************************************;
*  Records without zip codes are separated based on whether or not the   ;
*  FIPS code is valid (if valid then validFIP is county).  Records with  ;
*  valid FIPS will have lat/lon defaulted to the county centroid.        ;
*  Records with invalid FIPS cannot be located and there will be dropped ;
*  from the inventory.                                                   ;
*************************************************************************;

data GetCtCnt NoLocate;
   set NoZipA NoZipB;
   if validFIP = 'county' then output GetCtCnt;
   else output NoLocate;

proc datasets library=work;
   delete NoZipA NoZipB;
quit;

*************************************************************************;
*  Lat/lons in the GetCtCnt data set are now defaulted to the county     ;
*  centroid by matching by FIPS to the county centroid file.  The output ;
*  data set if defltFIP.  No records should not match.                   ;
*************************************************************************;
;

proc sort data=reffile.&cntycent out=county(keep=FIPS avglat avglon);
   by FIPS;

proc sort data=GetCtCnt;
   by FIPS;

data defltFIP LocProb;
   merge GetCtCnt(in=A) county(in=B);
   by FIPS;

   if A and B then do;
      lat = avglat;
      lon = avglon;
      lflag = 'county';
      output defltFIP;
   end;

   if A and not(B) then output LocProb;

   drop avglat avglon ;

proc print data=LocProb noobs;
   title 'Records with valid FIPS that do not match to County Centroid File';
run;

*** Append together all data sets containing records with defaulted locations ***;

data Default;
  set defltzip defltFIP;

*** Prepare output data set containing all locations being defaulted for further investigation ***;

proc sort data=Default;
   by site_id cas emrelpid;

proc sort data=in_data.&insas out=inventory;
   by site_id cas emrelpid;

data outdata.DefltLoc;
  merge Default(in=A) inventory(in=B); **output data set added 10/1/2002 to preserve input dataset date;
  by site_id cas emrelpid;
  if A;
run;

data _null_;
   set sashelp.vtable(keep=nobs libname memname);
   where libname = 'OUTDATA' and memname eq "DEFLTLOC";
   call symput('keep_dfloc',nobs);
run;

%if &keep_dfloc %then %do;
   data _null_;

      put 'Data set DefltLoc contains all locations where the data does ';
      put 'not allow for the calculation of a latitude and longitiude or';
      put 'the latitude and longitude are outside of ranges encompassing the US';
      put 'and the latitude and longitude could not be defaulted to the county';
      put 'centroid.  All inventory data records for these location and all data';
      put 'fields are included in this output data set.';


   *** QA Check - List first 100 locations being defaulted ***;

   proc sort data=outdata.DefltLoc;
     by site_id;

   data listdef;
      set outdata.DefltLoc;
      by site_id;
      if first.site_id;
      keep sitename site_id FIPS lflag FIPflag utm_z y x lat lon;

   proc print data=listdef(obs=100) noobs split='*';
      label sitename = 'Site Name'
            site_id = 'Site ID'
            FIPS = 'FIPS Code'
            lflag = 'Lat/Lon*FLag'
            FIPflag = 'FIPS Flag'
            utm_z = 'Inventory*UTMZ'
            y   = 'Inventory*Latitude'
            x   = 'Inventory*Longitude'
            lat = 'Defaulted*Latitude'
            lon = 'Defaulted*Longitude';
      title 'List of First 100 Locations Requiring Defaulted Latitudes and Longitudes';
      title2 "***NOTE: Other Locations May Require Defaulted Locations or FIPS...";
      title3 "...if FIPS and County FIPS Coordinates Do Not Match.******************";
   run;
   title;
   proc datasets library=work;
      delete listdef;
   quit;
%end;
%else %do;
   proc datasets library=outdata;
      delete DefltLoc;
   quit;
%end;

**************************************************************************************************;
*** Prepare output data set containing all records without locations for further investigation ***;
***   These records will not be modeled.                                                       ***;
**************************************************************************************************;

proc sort data=NoLocate;
   by site_id cas emrelpid;

proc sort data=in_data.&insas out=inventory; **output data set added 10/1/2002 to preserve input dataset date;
   by site_id cas emrelpid;

data outdata.NoLocate;
  merge NoLocate(in=A) inventory(in=B);
  by site_id cas emrelpid;

  if A;
run;

data _null_;
   set sashelp.vtable(keep=nobs libname memname);
   where libname = 'OUTDATA' and memname eq "NOLOCATE";
   call symput('keep_noloc',nobs);
run;

%if &keep_noloc %then %do;
   data _null_;
      put 'Data set NoLocate contains all locations where the data does ';
      put 'not allow for the calculation of a latitude and longitiude or';
      put 'the latitude and longitude are outside of ranges encompassing the US';
      put 'and the latitude and longitude could not be defaulted to the county';
      put 'centroid.  These records have been removed from the inventory.';
      put 'A text version of this file will also be created.';

   *** QA Check - List first 100 locations begin defaulted and emission total ***;

   proc sort data=outdata.NoLocate;
      by site_id;

   data lstNoLoc;
      set outdata.NoLocate;
      by site_id;
      if first.site_id;
      keep sitename site_id FIPS validFIP lflag FIPflag;

   proc print data=lstNoLoc(obs=100) noobs split='*';
      label sitename = 'Site Name'
            site_id = 'Site ID'
            FIPS = 'FIPS Code'
            validFIP = 'Valid*FIPS flag'
            lflag = 'Lat/Lon*FLag'
            FIPflag = 'FIPS Flag';
       title 'List of First 100 Locations with no Latitudes and Longitudes';

   proc means sum data=outdata.NoLocate;
      var emis;
      output out=NoLocSum(drop=_type_ _freq_) sum=emis;

   proc print data=NoLocSum noobs;
      title 'Total Emissions Eliminated from Inventory - No Location Data for Point Source';
   run;

   ***   Create a text file version of the records where no lat/lon could be determined.    ;

   data _null_;
      set outdata.NoLocate;
      file outtext(NoLocate.txt);
      put site_id cas emis FIPS sitename llprob validFIP lflag FIPflag;
   run;
   proc datasets library=work;
      delete lstNoLoc NoLocSum;
   quit;
%end;
%else %do;
   proc datasets library=outdata;
      delete NoLocate;
   quit;
%end;

*** Delete temporary data sets ***;

proc datasets library=work;
   delete badloc todeflt defltzip defltFIP zipcode county
          getctcnt LocProb NoLocate;
quit;


%MEND DftLocat;

**********************************************************************;
**********************************************************************;


%MACRO GetFIPS;

********************************************************************;
*  MACRO: GetFIPS (***ASPEN model processing only)                  ;
*      This macro merges the locations with valid lat/lon with those;
*  with defaulted lat/lons.  Locations where the FIPS codes is not  ;
*  in agreement with the lat/lon are defaulted, either by changing  ;
*  the lat/lon or changing the FIPS.                                ;
********************************************************************;
;

data locate;
   set default goodloc;

proc sort data=locate;
   by site_id;

data noFIPS(keep=site_id lat lon);
  set locate;
  by site_id;
  if first.site_id;
  lon = -(lon);

/** BEGIN FORMER latlon2fip INCLUDE FILE: bound6 and counties are now assigned in batch file **/
/** %inc inc_dir("latlon2fip.inc"); **/

*---------------------------------------------------------------------------;
* part 1: based on findpoly_county.sas;

* find first pass guesses as far as which county polygons contain a
point.  the mapdsn corresponds to map.counties at highest density 6;

data locN;
  set NoFIPS;
  rcd = _n_;
run;

proc sql;
  create table loc as
  select unique lat,lon
  from NoFIPS;
quit;

data check;
  keep px py lat lon;
  retain rad2deg 57.29578;
  set loc;
  px = lon/rad2deg;
  py = lat/rad2deg;
run;

proc contents data=reffile.&bound6 out=a noprint;
data _null_;
  set a;
  call symput('npoly',nobs);
  stop;
run;

*  All polygons inside or outside of the county that could possibly
contain the point will be output.  A maximum of 5 guess polygons is
allowed per point.;

* if chknum = 0 then the point might be in the water, because no
possible polygons were found for the point.  These points will have to
be manually checked;

* If chknum >= 1 then the point is in a polygon.  Test further these
polygons in the guess list;

* also output will be the beginning and endding records for each polygon
to be tested;

*** retains first 5 state-county polygon matches **;
data pass1;
  keep   lat lon px py state1-state5 county1-county5 seg1-seg5
    begrcd1-begrcd5 endrcd1-endrcd5 chknum ok;
  retain lat lon px py state1-state5 county1-county5 seg1-seg5
    begrcd1-begrcd5 endrcd1-endrcd5 chknum ok;
  array   states(*)  state1- state5;
  array counties(*) county1-county5;
  array      seg(*)    seg1-   seg5;
  array   begrcd(*) begrcd1-begrcd5;
  array   endrcd(*) endrcd1-endrcd5;

  set check;

* initialize counters and other things;
  chknum = 0; * number of polygons to test;
  ok = 0;      * is it in the given county? ;
  do k = 1 to dim(states);
      states(k) = 0;         * states to check;
    counties(k) = 0;         * counties to check;
         seg(k) = 0;         * segment of the county to check;
      begrcd(k) = 0;         * first record of the mapfile to access;
      endrcd(k) = 0;         * last record of the mapfile to access;
  end;
  do j = 1 to &npoly;
    set reffile.&bound6(rename=(state=mapst county=mapcnty)) point=j;

    if px lt xmax and px gt xmin and py lt ymax and py gt ymin then do;
      chknum + 1;
        states(chknum) = mapst;
      counties(chknum) = mapcnty;
           seg(chknum) = segment;
        begrcd(chknum) = begseg;
        endrcd(chknum) = endseg;
    end;

  end;
  output;
  file print;
  title;
  if chknum eq 0 then put  'warning: no test polygons found for '
    lat= lon=;
run;

*--------------------------------------------------------------------------;

/* part 2: based on chkpoly_county.sas.  check the first pass guesses.

to check if a point lies in a polygon using the sas-created map
dataset map.counties.  pass1 is a dataset containing a state, county
and a lat and lon supposibly in that county, and the different county
polygons to check with this algorithm.

the input dataset pass1 was created by part 1, the program which
finds first guesses as to which polygons might contain the point.
pass1 contains these guesses, along with which records in the mapdsn2
to access to test that polygon.

$mis = /home/bkx/miscellaneous

 see $mis/pnpoly.f for the fortran version.  The fortran was written by

                   Wm. Randolph Franklin, Associate Professor
                                wrf@ecse.rpi.edu
                     http://www.ecse.rpi.edu/Homepages/wrf/
                         +1 (518) 276-6077; Fax: -6261
     ECSE Dept., 6026 JEC, Rensselaer Polytechnic Inst, Troy NY, 12180 USA
                              (PGP key available)

This was pulled in from the www.  see $mis/pnpoly.txt.

C        REMARKS
C           THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE.
C           THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY
C           OPTIONALLY BE INCREASED BY 1.
C           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
C           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
C           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
C           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
C           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
C           THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM
C           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70.

C        METHOD
C           A VERTICAL LINE IS DRAWN THRU THE POINT IN QUESTION. IF IT
C           CROSSES THE POLYGON AN ODD NUMBER OF TIMES, THEN THE
C           POINT IS INSIDE OF THE POLYGON.
*/

data chkpoly;
  keep truest truecnty realseg lat lon match;
  retain inout1-inout5;
  length inout1-inout5 mx my nx ny 3 match $10;

  array   states(*)  state1- state5; * states possibly containing the pt;
  array counties(*) county1-county5; * counties possibly containing the pt;
  array      seg(*)    seg1-   seg5; * the segment of that county;
  array   begrcd(*) begrcd1-begrcd5; * start record for each test in mapdsn;
  array   endrcd(*) endrcd1-endrcd5; * end record;
  array    inout(*)  inout1- inout5; * indicator of pt inside, outside, on-edge;

  array x(4000) _temporary_; * array of vertex xx minus px;
  array y(4000) _temporary_; * array of vertex yy minus py;

  set pass1;

  do t = 1 to chknum;  * test each polygon for this point;
    n = endrcd(t) - begrcd(t) + 1; * number of vertices;
    ii = 0;

* load polygon differences into array;
    do i = begrcd(t) to endrcd(t);
      set reffile.&counties(rename=(state=mapst county=mapcnty x=xx y=yy)) point=i;
      ii + 1;
      x(ii) = xx - px;
      y(ii) = yy - py;
    end;

    inout(t) = -1; * initialize flag to outside;

* start test;
    do i = 1 to n;
      j = 1 + mod(i,n);

      if x(i) ge 0 then mx = 1; else mx = 0;
      if x(j) ge 0 then nx = 1; else nx = 0;
      if y(i) ge 0 then my = 1; else my = 0;
      if y(j) ge 0 then ny = 1; else ny = 0;

      if ( not ((my or ny) and (mx or nx)) or (mx and nx)) then go to lab2;
      if( not (my and ny and (mx or nx) and  not (mx and nx))) then go to lab3;

      inout(t) = -1 * inout(t);

      go to lab2;

lab3: exp = ((y(i)*x(j)-x(i)*y(j))/(x(j)-x(i)));
      select;
        when(exp < 0)go to lab2;
        when(exp = 0)go to lab4;
        when(exp > 0)go to lab5;
      end;

lab4: inout(t) = 0;  * border;
      go to lab6;
lab5: inout(t) = -1 * inout(t);
lab2:
    end;

lab6:;  * inout = 1 means inside, no need to continue testing this point;
    if inout(t) eq 1 then goto lab7;
  end;

* test for the point being on a border, if it is not in any county;
  do t = 1 to chknum;
    if inout(t) eq 0 then do;
      truest = states(t);
      truecnty = counties(t);
      realseg = seg(t);
      return;
    end;
  end;

* if the flow gets here, this means that the point was not found to be in or
on the border of any county.  inout = -1;

  truest = .;
  truecnty = .;
  realseg = .;
  match = 'no county';
  return;

lab7:; * there is a county containing this point.  inout = 1;
  truest = states(t);
  truecnty = counties(t);
  realseg = seg(t);
run;

* find the names of these locations;

* get the names that came from the point-in-polygon check;

proc sort data=chkpoly;
  by truest truecnty;
run;
*****************************************************************************************TEST;

data cntycent(drop=fips rad_mi stname);
   length realcnty $25 realst $2;
   set reffile.&cntycent(rename=(avglat=ctrlat1 avglon=ctrlon1 cyname=realcnty));
   truest = input(substr(fips,1,2),2.);
   truecnty = input(substr(fips,3,3),3.);
   realst = fipstate(truest);
run;
proc sort data=cntycent; by truest truecnty; run;
data chk;
  merge
    chkpoly(in=inloc)
    cntycent;
  by truest truecnty;
  if inloc;
run;

proc sql;
  create table GotFIPS as
  select locN.site_id, chk.lat, chk.lon, chk.truest as stid,
  chk.truecnty as cyid, chk.realst as state, chk.realcnty as county,
  chk.ctrlat1, chk.ctrlon1
  from locN left join chk
  on locN.lat = chk.lat and locN.lon = chk.lon
  ;
quit;

***APR 2003: fixed inefficient code **;
data GotFIPS(keep=site_id newFIPS);
   length newFIPS $5;
   set GotFIPS;
   newFIPS = put(stid,z2.)||put(cyid,z3.);
   if substr(newFIPS,1,2) = ' .' then substr(newFIPS,1,2) = '99';
   if substr(newFIPS,3,3) = '  .' then substr(newFIPS,3,3) = '999';
run;

/** END FORMER latlon2fip INCLUDE FILE **/

proc sort data=gotFIPS;
  by site_id ;


data ChckFIPS;
  merge gotFIPS(in=A) locate(in=B);
  by site_id ;


data goodFIP(drop=newFIPS) stateloc chckloc;
   set ChckFIPS;

  if newFIPS = '99999' and FIPS = '99999' then output chckloc;
  else if fips = newfips then output goodFIP;
  else if substr(FIPS,1,2) = substr(newFIPS,1,2) then output stateloc;
  else output chckloc;

*** Delete temporary data sets ***;

proc datasets library=work;
   delete default goodloc noFIPS GotFIPS locate ChckFIPS Loc LocN A Pass1 Chk Check ChkPoly ;
quit;


***********************************************************************;
* Situation 1                                                          ;
*                                                                      ;
*     The NTI FIPS and the computed FIPS are not the same, but are in  ;
*  in the same state.  Determine if the point lies with 5.4 X radius of;
*  the NTI FIPS county.  If so, leave NTI FIPS and location unchanged. ;
*  If not, look at zip code FIPS, computed FIPS and validity of NTI    ;
*  FIPS county, and resolve situation depending on these factors.      ;
*                                                                      ;
*  Use stateloc dataset                                                ;
*                                                                      ;
***********************************************************************;

*** Check to see if the locations in stateloc are close to the centroid of the NTI FIPS county ***;

proc sort data=reffile.&cntycent out=cty_cntr(keep=avglat avglon rad_mi FIPS);
 by FIPS;
run;
proc sort data=stateloc;
  by FIPS;

data chckloc1 keepfip1(drop=newFIPS);
   merge stateloc(in=A) cty_cntr(in=B);
   by FIPS;

   if A and B then do;
      dist = (SQRT((cos(((avglat + lat)/2)*3.14159/180)
                 * (avglon - lon))**2
                 + (avglat - lat)**2))
             *(60*2000/1760);
      if 5.4*rad_mi => int(dist) then do;
         FIPflag = 'noch_ss';
         output keepfip1;
      end;
      else output chckloc1;
   end;

   else if A and not(B) then output chckloc1;

   drop avglat avglon rad_mi dist;

run;

*** Check to see if the locations in chckloc1 are can be resolved using the zip code FIPS ***;

proc sort data=chckloc1;
   by zip_code;

proc sort data=reffile.&zip(keep=fips zip_code cntlon cntlat) out=zipcodes(rename=(FIPS=zipFIPS));
   by zip_code;


data defzip1(drop=newFIPS) chckloc2;

   merge chckloc1(in=A) zipcodes(in=B);
   by zip_code;

   if A and B then do;
      if FIPS = zipFIPS then do;
         lat = cntlat;
         lon = cntlon;
         lflag = 'zipcode';
         FIPflag = 'noch_ss';
         output defzip1;
      end;
      else if newFIPS = zipFIPS then do;
         FIPS = newFIPS;
         FIPflag = 'ZIP_ss';
         output defzip1;
      end;
      else output chckloc2;
   end;

   else if A and not(B) then output chckloc2;

   drop cntlat cntlon zipFIPS;


data defcty1 chngLoc1;
   set chckloc2;

   if validFIP = 'county' then do;
      lflag = 'county';
      FIPflag = 'noch_ss';
      output defcty1;
   end;
   else do;
      FIPS = newFIPS;
      FIPflag = 'reloc_ss';
      output chngLoc1;
   end;

   drop newFIPS ;

run;


***********************************************************************;
* Situation 2                                                         *;
*                                                                     *;
*  The NTI FIPS and the computed FIPS are not the same and represent  *;
*  different States.  Determine if the point lies with 5.4 X radius of*;
*  the NTI FIPS county. If so, leave NTI FIPS and location unchanged. *;
*  If not, check the NTI FIPS with the FIPS from the zip code.        *;
*  If they match, relocate the point to centroid of the zip code.     *;
*  If not, check the FIPS from the zip code with the computed FIPS.   *;
*  If they match, then change the NTI FIPS to the computed FIPS.      *;
*  If not, do default location to county-based default based on NTI FIP;
*  If not possible (FIPS validity) series of other checks to resolve  *;
*  situation.                                                         *;
*                                                                     *;
*  Use chckloc datase                                                 *;
*                                                                     *;
***********************************************************************;

*** Check to see if the locations in chckloc are close to the centroid of the NTI FIPS county ***;

proc sort data=chckloc;
  by FIPS;

data keepfip2(drop=newFIPS) chckloc3;
  merge chckloc(in=A) cty_cntr(in=B);
  by FIPS;

  if A and B then do;
    dist = (SQRT((cos(((avglat + lat)/2)*3.14159/180)
                 * (avglon - lon))**2
                 + (avglat - lat)**2))
             *(60*2000/1760);
    if 5.4*rad_mi => int(dist) then do;
       FIPflag = 'noch_ds';
       output keepfip2;
    end;
    else output chckloc3;
  end;

  else if A and not(B) then output chckloc3;

   drop avglat avglon rad_mi dist;

run;


*** Check to see if the locations in chckloc3 can be defaulted by zip code ***;

proc sort data=reffile.&zip out=zipcodes(rename=(FIPS=zipFIPS));
   by zip_code;

proc sort data=chckloc3;
  by zip_code;


data defzip2(drop=newFIPS) chckloc4;
   merge chckloc3(in=A) zipcodes(in=B);
   by zip_code;

   if A and B then do;
      if FIPS = zipFIPS then do;
         lat = cntlat;
         lon = cntlon;
         lflag = 'zipcode';
         FIPflag = 'noch_ds';
         output defzip2;
      end;
      else if newFIPS = zipFIPS then do;
         FIPS = newFIPS;
         FIPflag = 'ZIP_ds';
         output defzip2;
      end;
      else output chckloc4;
   end;

   else if A and not(B) then output chckloc4;

   drop cntlat cntlon zipFIPS;


*** Check to see if the locations in chckloc4 can be defaulted by NTI county FIPS ***;

data defcty2(drop=state newFIPS) NoModel1(drop=state newFIPS) chckLoc5;
   set chckloc4;
   length state $2.;

   if validFIP = 'county' then do;
      lflag = 'county';
      FIPflag = 'noch_ds';
      output defcty2;
   end;
   else if validFIP = 'state' then do;
      FIPflag = 'no_model';
      output NoModel1;
   end;
   else do;
      state=substr(site_id,1,2); **looking for 2-character not 2-digit;
      output chckloc5;
   end;

run;

*** Check to see if the locations in chckloc5 can be defaulted by state postal code ***;
*** Or to the computed FIPS, if it lies in the same state as the postal code        ***;

data chngFIP2 NoModel2;
   set chckLoc5;
   if substr(state,1,1) not in('0','1','2','3','4','5','6','7','8','9','0','E') then do;
      if put(stfips(state),z2.) = substr(FIPS,1,2) then do;
         FIPS = newFIPS;
         FIPflag = 'reloc_ds';
         output chngFIP2;
      end;
      else do;
         if newFIPS ne '99999' then do;
            FIPS=newFIPS;
            FIPflag = 'reloc_ds';
            output chngFIP2;
         end;
         else do;
            FIPflag = 'no_model';
            output NoModel2;
         end;
      end;
   end;
   else do;
      if newFIPS ne '99999' then do;
         FIPS=newFIPS;
         FIPflag = 'reloc_ds';
         output chngFIP2;
      end;
      else do;
         FIPflag = 'no_model';
         output NoModel2;
      end;
   end;   
   drop state newFIPS;
run;

************************************************************************;
*                                                                      *;
*   Append all files together again.                                   *;
*                                                                      *;
************************************************************************;

data CorrFIP;
   set goodFIP keepfip1 defzip1 defcty1 chngLoc1 keepfip2 defzip2 defcty2 chngFIP2;
run;

data NoModel;
   set NoModel1 NoModel2;

******************************************************************************;
*** Prepare output data set containing all records that cannot be modeled  ***;
***    for further investigation                                           ***;
*******************************************************************************;

proc sort data=NoModel;
   by site_id cas emrelpid;

proc sort data=in_data.&insas out=inventory; **output data set added 10/1/2002 to preserve input dataset date;
   by site_id cas emrelpid;

data outdata.NoModel;
  merge NoModel(in=A) inventory(in=B);
  by site_id cas emrelpid;

  if A;
run;

data _null_;
   set sashelp.vtable(keep=nobs libname memname);
   where libname = 'OUTDATA' and memname eq "NOMODEL";
   call symput('keep_nomod',nobs);
run;

%if &keep_nomod %then %do;
   data _null_;
      put 'Data set NoModel contains all locations where the data does ';
      put 'not have adequate information about the location by lat/lon or ';
      put 'by state and county FIPS to model the emissions. ';
      put 'These records have been removed from the inventory.';
      put 'A text version of this file will also be created.';


   *** QA Check - List first 100 locations that can not be modeled ***;

   proc sort data=outdata.NoModel;
      by site_id;

   data lstNoMod;
      set outdata.NoModel;
      by site_id;
      if first.site_id;
      keep sitename site_id lat lon FIPS validFIP llprob lflag FIPflag;
   
   proc print data=lstNoMod(obs=100) noobs split='*';
      label sitename = 'Site Name'
            site_id = 'Site ID'
            lat = 'Latitude'
            lon = 'Longitude'
            FIPS = 'FIPS Code'
            validFIP = 'Valid*FIPS flag'
            lflag = 'Lat/Lon*FLag'
            FIPflag = 'FIPS Flag' ;
       title 'List of First 100 Locations that cannot be Modeled';

   proc means sum data=outdata.NoModel;
      var emis;
      output out=NoModSum(drop=_type_ _freq_) sum=emis;

   proc print data=NoModSum noobs;
      title 'Total Emissions Eliminated from Inventory - Insufficient Location Data for Point Source';
   run;

   ***   Create a text file version of the records that cannot be modeled.    ;

   data _null_;
      set outdata.NoModel;
      file outtext(NoModel.txt);
      put site_id cas emis FIPS sitename lat lon llprob validFIP lflag FIPflag;
   run;
   proc datasets library=work;
      delete lstNoMod NoModSum;
   quit;
%end;
%else %do;
   proc datasets library=outdata;
      delete NoModel;
   quit;
%end;

*** Delete temporary data sets ***;

proc datasets library=work;
   delete goodFIP keepfip1 defzip1 defcty1 chngloc1 keepfip2 defzip2 defcty2 chngFIP2
          stateloc cty_cntr chckloc zipcodes
          NoModel1 NoModel2 NoModel 
          chckloc1 chckloc2 chckloc3 chckloc4 chckloc5 ;
quit;


%MEND GetFIPS;

********************************************************************;
********************************************************************;


%MACRO CtyDeflt;

*******************************************************************************;
* MACRO:  CtyDeflt (***ASPEN model processing only)                            ;
*     This macro determines a default location for all records that have been  ;
* assigned a lflag of county.  This means that the location is to be assigned  ;
* the to centroid of a randomly assigned tract within the county as given by   ;
* the FIPS.                                                                    ;
*******************************************************************************;


data Default(keep=site_id cas emrelpid FIPS );
   set CorrFIP;
   if lflag = 'county';

proc sort data=Default out=defltcty(keep=FIPS site_id);
  by site_id;

data Facility;
   set defltcty;
   by site_id;
   if first.site_id;

proc sort data=Facility;
    by FIPS;

data Facility;
   set Facility;
   by FIPS;

   if first.FIPS then facilno = 1;
   output;

   facilno = facilno + 1;
   retain facilno;

proc sort data=Facility;
   by FIPS;

proc sort data=reffile.&tracts out=trctarry;
   by FIPS;

proc contents data=trctarry out=x(keep=name) noprint; run;

/** Overkill, unless there are 10000 or more unique sites in a given county with bad coordinates **/
data xx;
   set x;
   where name ge 't1' and 't9999';
   call symput('num_arry',_n_);
run;
%put &num_arry;

data gettract;
   merge Facility(in=A) trctarry(in=B);
   by FIPS;

   array trctno{%eval(&num_arry)} t1-t%eval(&num_arry);
   if A;

proc sort data=gettract;
   by FIPS;

data gottract;
   set gettract;
   by FIPS;
   length tract $8.;

   array trctno{%eval(&num_arry)} t1-t%eval(&num_arry);

   if first.FIPS then arrno = 0;

   if trctno{facilno-arrno} eq . then arrno = (facilno - 1);

   trct = left(trctno{facilno-arrno}) ;
  tract = put(input(trct,6.),z6.);

   retain arrno;
   drop t1-t%eval(&num_arry) arrno facilno trct;

data trctinfo;
  set reffile.&trctinfo(rename=(tract=trct trlat=lat trlon=lon));
  tract = put(input(trct,6.),z6.);
run; 
proc sort data=trctinfo;
  by FIPS tract;

proc sort data=gottract;
  by FIPS tract;

data assignll;
   merge gottract(in=A) trctinfo(in=B);
   by FIPS tract;

   if A;
      defltrct = tract;
    keep site_id lat lon defltrct;

proc sort data=assignll;
   by site_id;

proc sort data=default;
   by site_id;

data Cty_loc;
   merge assignll default;
   by site_id;

proc sort data=CorrFIP;
   by site_id cas emrelpid;

proc sort data=Cty_loc(drop=FIPS);
   by site_id cas emrelpid;

data FinalLoc;
   merge CorrFIP(in=A) Cty_loc(in=B);
   by site_id cas emrelpid;

   if A;
run;

*** Delete temporary data sets ***;

proc datasets library=work;
   delete CorrFIP defltcty facility gettract gottract assignll default Cty_loc;
quit;


%MEND CtyDeflt;

********************************************************************;
********************************************************************;


%MACRO Merge;

********************************************************************;
*  MACRO: Merge                                                     ;
*      This macro merges the data set with the converted lat/lon    ;
*  (ASPEN) or utmx/utmy (ISCST3) with the inventory file.           ;
********************************************************************;
;

proc sort data=FinalLoc;
   by site_id cas emrelpid;

proc sort data=in_data.&insas out=inventory; **output data set added 10/1/2002 to preserve input dataset date;
   by site_id cas emrelpid;

data outdata.&outsas;
  merge inventory(in=A) FinalLoc(in=B);
  by site_id cas emrelpid;

  if A and B;


%if (%upcase("&Model") = "ISC") %then %do;

   proc sort data=missing;
      by site_id cas emrelpid;

   data outdata.missing;
      merge inventory(in=a) missing(in=b);
      by site_id cas emrelpid;
     if a and b;
   run;

   proc datasets library=work;
      delete missing;
   quit;

%end;

*** QA check of pollutant level emissions totals ***;

proc sort data=outdata.&outsas;
  by cas;

proc means data=outdata.&outsas n noprint;
  by cas;
  var emis;
  output out=LocSum(drop=_type_ _freq_) sum=emis n=count;

proc print data=LocSum noobs split='*';
   label emis = 'Total Emissions'
         count = 'Total Number*of Records';
   sum emis count;
   title 'Pollutant Level Emission Totals after Location Calculations and Defaulting';

*** DYL - added creation of state variable and temp inventory file - 5/02/01 ***;

data ToSum;
   set outdata.&outsas;
   state = substr(FIPS,1,2);

proc sort data=ToSum ;
   by state;

proc means data=ToSum sum n noprint;
   var emis;
   by state;
   output out=StateSum(drop=_type_ _freq_) sum=emis n=count;

proc print data=StateSum noobs split='*';
   label emis = 'Total Emissions'
         count = 'Total Number*of Records'
         state = 'FIPS State*Code' ;
   sum emis count;
   title 'State Emission Totals after Location Calculations and Defaulting';

run;

*** Delete temporary data sets ***;

proc datasets library=work;
   delete FinalLoc LocSum ToSum StateSum;     **2/21/01 DYL;
quit;


%MEND Merge;

********************************************************************;
********************************************************************;


%MACRO ChStack;

************************************************************************;
*  MACRO: ChStack                                                       ;
*      This macro checks the reasonableness of the stack parameters     ;
*  Stack Parameter value is considered unreasonable if                  ;
*  it is greater than upper bound or less than lower bound value        ;
*      upper and lower bound stack parameter range values               ;
*      are input by the the user in the batch file                      ;
*                                                                       ;
************************************************************************;
;

*** Subset inventory data to only the variables needed for check stack parameters. ***;
*** Check for stack parameters outside of acceptable range ***;


data stack(keep=emrelpid emrelpty cas site_id stackht stackdia stackvel stktemp
                Htflag Diaflag Velflag Tempflag scc sic)
     outdata.stkcheck;
   %If &DoLocate %then %do;
      set outdata.&outsas;
   %end;
   %Else %do;
      set in_data.&insas;
   %end;

   length Htflag $10.;
   length Diaflag $10.;
   length Velflag $10.;
   length Tempflag $10.;

**********************************************************;
* Create data set StkCheck to investigate stack parameter ;
*  outside of user defined ranges.  Missing values are    ;
*  not included.                                          ;
*  11/01 - Initialize stack flags                         ;
**********************************************************;

   Htflag   = '';
   Diaflag  = '';
   Velflag  = '';
   Tempflag = '';

   if ( ((StackHt < &dlowHt or StackHt > &dhiHt) and StackHt <> .) or
        ((StackDia < &dlowDia or StackDia > &dhiDia) and StackDia <> .) or
        ((StackVel < &dlowVel or StackVel > &dhiVel) and StackVel <> .) or
        ((StkTemp < &dlowTemp or StkTemp > &dhiTemp) and StkTemp <> .) ) or
      (StackHt = 0 or StackHt = 0 or StackVel = 0 or StkTemp = 0)
      then output outdata.stkcheck;

   output stack;

******************************************************************************;
*** Prepare output data set containing all records with stack parameters   ***;
***   outside set limits for further investigation                         ***;
******************************************************************************;

data _null_;
   set sashelp.vtable(keep=nobs libname memname);
   where libname = 'OUTDATA' and memname eq "STKCHECK";
   call symput('keepstk',nobs);
run;

%IF &keepstk %THEN %DO;
   *** QA Check - List first 100 locations with out-of-range stack parameters ***;

   proc sort data=outdata.stkcheck;
      by site_id;

   data lststkch;
      set outdata.stkcheck;
      by site_id;
      if first.site_id;
      keep sitename site_id FIPS StackHt StackDia StackVel StkTemp;

   proc print data=lststkch(obs=100) noobs split='*';
      label sitename = 'Site Name'
            site_id = 'Site ID'
            FIPS = 'FIPS Code'
            StackHt = 'Stack*Height'
            StackDia = 'Stack*Diameter'
            StackVel = 'Stack*Velocity'
            StkTemp = 'Stack*Temperature';
       title 'List of First 100 Locations with Out-Of-Range Stack Parameters';

   proc means sum data=outdata.stkcheck noprint;
      var emis;
      output out=stkchSum(drop=_type_ _freq_) sum=emis;

   proc print data=stkchSum noobs;
      title 'Total Emissions from Locations with Out-Of-Range Stack Parameters';
   run;

   *** Delete temporary data sets ***;

   proc datasets library=work;
      delete lststkch stkchSum;
   quit;
%END;
%ELSE %DO;
   proc datasets library=outdata;
      delete stkcheck;
   quit;
%END;


%MEND ChStack;

**********************************************************************;
**********************************************************************;


%MACRO DftStack;

*************************************************************************;
* MACRO: DftStack                                                        ;
*    This macro replaces missing stack parameter with default stack      ;
* parameters based on SCC and/or SIC, depending on the users choice.     ;
* For stack parameters still missing after using SCC and/or SIC defaults,;
* user provided defaults are used.                                       ;
*************************************************************************;
;

************************************************************;
*  User has selected to use default stack parameters by SCC ;
************************************************************;

%If &DoSCCDef  %then %do;

   proc sort data=stack;
      by SCC;

   data sccdef;
      infile reftext("&SCCDeflt..txt");

      input @1 scc $10.
            @12 AvgHt 14.10
            @27 AvgDia 14.10
            @42 AvgVel 14.10
            @57 AvgTemp 16.10
            @74 defflag $6.;

   proc sort data=sccdef;
      by SCC;

   Data stack(drop=AvgHt AvgVel AvgDia AvgTemp defflag);

      merge stack(in=A) sccdef(in=B);
      by SCC;

      if A;
         if B and SCC ne '          ' then do;

            if StackHt = . then do;
               StackHt = AvgHt;
               Htflag = defflag||'miss';
            end;
            else if StackHt < &dlowHt or StackHt > &dhiHt then do;
               StackHt = AvgHt;
               Htflag = defflag||'out';
            end;

            if StackDia = . then do;
               StackDia = AvgDia;
               Diaflag = defflag||'miss';
            end;
            else if StackDia < &dlowDia or StackDia > &dhiDia then do;
               StackDia = AvgDia;
               Diaflag = defflag||'out';
            end;

            if StackVel= . then do;
               StackVel = AvgVel;
               Velflag = defflag||'miss';
            end;
            else if StackVel < &dlowVel or StackVel > &dhiVel then do;
               StackVel = AvgVel;
               Velflag = defflag||'out';
            end;

            if StkTemp = . then do;
               StkTemp = AvgTemp;
               Tempflag = defflag||'miss';
            end;
            else if StkTemp < &dlowTemp or StkTemp > &dhiTemp then do;
               StkTemp = AvgTemp;
               Tempflag = defflag||'out';
            end;

         end;

      output;

%end;

************************************************************;
*  User has selected to use default stack parameters by SIC ;
************************************************************;

%If &DoSICDef %then %do;

   proc sort data=stack;
      by SIC;

   data sicdef;
      infile reftext("&SICDeflt..txt");

      input @1 sic $4.
            @10 AvgHt 14.10
            @25 AvgDia 14.10
            @40 AvgVel 14.10
            @55 AvgTemp 16.10
            @72 defflag $6.;

   proc sort data=sicdef;
      by SIC;

   Data stack(drop=AvgHt AvgVel AvgDia AvgTemp defflag);

      merge stack(in=A) sicdef(in=B);
      by SIC;

      if A;
         if B then do;

            if StackHt = . then do;
               StackHt = AvgHt;
               Htflag = defflag||'miss';
            end;
            else if StackHt < &dlowHt or StackHt > &dhiHt then do;
               StackHt = AvgHt;
               Htflag = defflag||'out';
            end;

            if StackDia = . then do;
               StackDia = AvgDia;
               Diaflag = defflag||'miss';
            end;
            else if StackDia < &dlowDia or StackDia > &dhiDia then do;
               StackDia = AvgDia;
               Diaflag = defflag||'out';
            end;

            if StackVel= . then do;
               StackVel = AvgVel;
               Velflag = defflag||'miss';
            end;
            else if StackVel < &dlowVel or StackVel > &dhiVel then do;
               StackVel = AvgVel;
               Velflag = defflag||'out';
            end;

            if StkTemp = . then do;
               StkTemp = AvgTemp;
               Tempflag = defflag||'miss';
            end;
            else if StkTemp < &dlowTemp or StkTemp > &dhiTemp then do;
               StkTemp = AvgTemp;
               Tempflag = defflag||'out';
            end;

         end;
      output;

%end;


***  Stack parameters not already defaulted by SIC or SCC are taken care of ***;
***   now using global defaults based on user input values and the range   ***;
***   values where appropriate, with the following exception:              ***;
***   Stack parameters for  horizontal and fugitive sources are set to     ***;
***   height of 5 m, diameter of 1 m, temperature of 295 K, and velocity of 0.5 m/s   ***;

Data stack;
   set stack;

*** Set stack parameters for nonstacked sources if zero or missing, except for airport sources   ***;
***   fugitive sources (emrelpty=01) or horizontal sources (emrelpty=03)  ***;

   if emrelpty = '01' or emrelpty = '03' then do;
      if StackHt = 0 or StackHt = . then StackHt = 5;
      if StackDia = 0 or StackDia = . then StackDia = 1;
      if StkTemp = 0 or StkTemp = . then StkTemp = 295;
      if StackVel = 0 or StackVel = . then StackVel = 0.5;
   end;


*** Replace any missing stack parameters for stacked sources with the defaults provided by the user ***;
***  All airport sources do not have stack parameters and will thus be defaulted.                   ***;

   if StackHt = . then do;
      StackHt = &dfltHt;
      Htflag = 'default';
   end;
   else if StackHt < &dlowHt then do;
      StackHt = &dlowHt;
      Htflag = 'rangelow';
   end;
   else if StackHt > &dhiHt then do;
      StackHt = &dhiHt;
      Htflag = 'rangehi';
   end;
   if StackDia = . then do;
      StackDia = &dfltDia;
      Diaflag = 'default';
   end;
   else if StackDia < &dlowDia then do;
      StackDia = &dlowDia;
      Diaflag = 'rangelow';
   end;
   else if StackDia > &dhiDia then do;
      StackDia = &dhiDia;
      Diaflag = 'rangehi';
   end;

   if StackVel= . then do;
      StackVel = &dfltVel;
      Velflag = 'default';
   end;
   else if StackVel < &dlowVel then do;
      StackVel = &dlowVel;
      Velflag = 'rangelow';
   end;
   else if StackVel > &dhiVel then do;
      StackVel = &dhiVel;
      Velflag = 'rangehi';
   end;

   if StkTemp = . then do;
       StkTemp = &dfltTemp;
       Tempflag = 'default';
   end;
   else if StkTemp < &dlowTemp then do;
       StkTemp = &dlowTemp;
       Tempflag = 'rangelow';
   end;
   else if StkTemp > &dhiTemp then do;
       StkTemp = &dhiTemp;
       Tempflag = 'rangehi';
   end;

*** Merge checked and defaulted stack parameters back into the inventory data set. ***;

proc sort data=stack;
   by site_id cas emrelpid;

%If &DoLocate %then %do;
   proc sort data= outdata.&outsas tagsort;
%end;
%Else %do;
   proc sort data= in_data.&insas tagsort out=inventory; **output data set added 10/1/2002 to preserve input dataset date;
%end;
   by site_id cas emrelpid;

data outdata.&outsas;
   %If &DoLocate %then %do;
      merge outdata.&outsas stack;
   %end;
   %Else %do;
      merge inventory stack;
   %end;
   by site_id cas emrelpid;


*** Remove temporary working files ***;

proc datasets library=work;

   %let delfiles = stack;
   %let tempfile = &delfiles;

   %If &DoSCCDef or &DoSICDef %then %do;
      %If &DoSCCDef %then %do;
        %let delfiles = &tempfile sccdef;
        %let tempfile = &delfiles;
      %end;
      %If &DoSICDef %then %do;
        %let delfiles = &tempfile sicdef;
      %end;
   %end;

   delete &delfiles;
quit;


*** QA check of Pollutant Code emissions totals ***;

proc sort data=outdata.&outsas;
  by cas;

proc means data=outdata.&outsas sum n noprint;
  by cas;
  var emis;
  output out=PollSum(drop=_type_ _freq_) sum=emis n=count;

proc print data=PollSum noobs split='*';
   label cas = 'NTI Inventory*Pollutant Code'
         emis = 'Emissions'
         count = 'Number of*Records';
   sum emis;
   title 'Pollutant Code Level Summary of Emissions from PtDataProc';

*** QA Check of State Level Emission totals ***;

data ToSum;
   set outdata.&outsas;
   state = substr(FIPS,1,2);

proc sort data=ToSum;
  by state;

proc means data=ToSum sum n noprint;
  by state;
  var emis;
  output out=StateSum(drop=_type_ _freq_) sum=emis n=count;

proc print data=StateSum noobs split='*';
   label state = 'FIPS State*Code'
         emis = 'Emissions'
         count = 'Number of*Records';
   sum emis;
   title 'State Level Summary of Emissions from PtDataProc';
run;

*** Delete temporary data sets ***;

proc datasets library=work;
   delete PollSum ToSum StateSum;
quit;


%MEND DftStack;

**********************************************************************;
**********************************************************************;


%MACRO SetVar;

********************************************************************;
*  MACRO: SetVar                                                    ;
*      This macro retains only those variables necessary for further;
*  processing within EMS-HAP, including the growth and control      ;
*  module.  The user can provide a list of additional variable to be;
*  retained.                                                        ;
********************************************************************;

    %if (%upcase("&Model") = "ASPEN") %then %do;
       %let list = emis cas site_id FIPS src_type emrelpid emrelpty
                   StackHt StackDia StackVel StkTemp lat lon
                   MACT SCC SIC cntl_eff;
    %end;
    %else %if (%upcase("&Model") = "ISC") %then %do;
       %let list = emis cas site_id FIPS src_type emrelpid emrelpty
                   StackHt StackDia StackVel StkTemp utmx utmy
                   MACT SCC SIC cntl_eff;
    %end;

%If &UseList %then %do;

   data varlist;
      infile reftext("&varlist..txt");
*** Changes to allow for longer variable names in the NIF 2.0 format - DYL 6/19/01 ***;
      informat var $20.;
      informat keep $1.;

      Input @1 var $  @22 Keep $;
       output;

   data _null_;
      set varlist end=last;

      if _N_ = 1 then n = 0;
       if trim(keep) = 'Y' then do;
         n = n + 1;
         call symput('var'||left(n),var);
      end;

      retain n;
      if last then call symput('nvar',left(n));
   run;


   %Do n = 1 %to &nvar;
       %let list = &list &&var&n ;
   %end;

   %if (%upcase("&Model") = "ISC") %then %do;
      *** Retain ISC variables that MAY be present in dataset provided by USER *;

      proc contents data=in_data.&insas out=checkISCvar(keep=name) noprint;
      run;

      *** Initialize tot_iscv in case inventory does not contain any of these "ISC" variables ***;
      %let tot_iscv =;
      
      data iscvars;
         set checkISCvar;
         where trim(lowcase(name)) 
         in('isctype','axlen','aylen','ainplum','aangle','arelhgt','bldh','bldw','volhgt','sigmay','sigmaz');
         call symput('isc_keep'||left(put(_n_,2.)),trim(name));
         call symput('tot_iscv',_n_);
      run;

      %if &tot_iscv ne %then %do; *** Append ISC variables to existing list of retained variables ***;
         %Do n = 1 %to &tot_iscv;
            %let list = &list &&isc_keep&n ;
         %end;
      %end;
   %end;

%end;

Data outdata.&final;
   %If &DoLocate or &DoStack %then %do;
      %IF &DoLocate = 0 %THEN %DO; ** October 2002;
         set outdata.&outsas(rename=(x=lon y=lat %if &renameMACT %then %do;
	        MACTcode=MACT %end;));
      %END;
      %ELSE %DO;
         set outdata.&outsas(%if &renameMACT %then %do;
	  rename=(MACTcode=MACT) %end;) ;
      %END;
   %end;
   %Else %then %do;
      set in_data.&insas(rename=(x=lon y=lat %if &renameMACT %then %do;
	        MACTcode=MACT %end;)); ** October 2002;
   %end;

   keep &list;

run;

%MEND SetVar;

**********************************************************************;
**********************************************************************;


%MACRO WindData;

**********************************************************************;
*  MACRO: WindData                                                    ;
*      This macro retains only those records with location information;
* (lat/lon for ASPEN and utmx/utmy for ISCST3) and emissions data for ;
* further processing.  For data processed through Locate (ASPEN) in   ;
* this program, there will be no records without lat/lon information. ;
**********************************************************************;
;

Data outdata.&final outdata.&nolocate outdata.&zeroemis;
   %If &DoSetVar %then %do;
      set outdata.&final;
   %end;
   %Else %If &DoLocate or &DoStack %then %do;
      %IF &DoLocate = 0 %THEN %DO; ** October 2002;
         set outdata.&outsas(rename=(x=lon y=lat %if &renameMACT %then %do;
	        MACTcode=MACT %end;));
      %END;
      %ELSE %DO;
         set outdata.&outsas(%if &renameMACT %then %do;
	  rename=(MACTcode=MACT) %end;);
      %END;
   %end;
   %Else %then %do;
      set in_data.&insas(rename=(x=lon y=lat %if &renameMACT %then %do;
	        MACTcode=MACT %end;)); ** Oct 2002;
   %end;

   If emis ne . and emis ne 0 then do;
      %if (%upcase("&Model") = "ASPEN") %then %do;
         if lat ne . and lat ne 0 and lon ne . and lon ne 0
            then output outdata.&final;
         else output outdata.&nolocate;
      %end;
      %else %if (%upcase("&Model") = "ISC") %then %do;
         if utmx ne . and utmx ne 0 and utmy ne . and utmy ne 0
            then output outdata.&final;
         else output outdata.&nolocate;
      %end;
   end;

   else output outdata.&zeroemis;

run;

%let keepa = 1;
%let keepb = 1;
%let keepc = 1;
data _null_;
  set sashelp.vtable(keep=nobs libname memname);
  where libname = 'OUTDATA' and memname in('MISSING',%upcase("&nolocate"),%upcase("&zeroemis"));
  if memname = %upcase("&nolocate") then call symput('keepa',nobs);
  if memname = %upcase("&zeroemis") then call symput('keepb',nobs);
  if memname = "MISSING" then call symput('keepc',nobs);
run;

%IF not &keepa or not &keepb %THEN %DO;
   proc datasets library=outdata;
      delete
        %IF not &keepa %THEN %DO; &nolocate %END;
        %IF not &keepb %THEN %DO; &zeroemis %END;
        %IF not &keepc %THEN %DO; missing %END;
      ;
   quit;
%END;


%MEND WindData;

**********************************************************************;
**********************************************************************;


%MACRO Locate;

%If (%upcase("&Model") = "ASPEN") %then %do;

   %If &DoLocate %then %do;
        %Convert
        %ChLocate
        %DftLocat
        %GetFIPS
        %CtyDeflt
        %Merge
   %end;

%end;

%Else %if (%upcase("&Model") = "ISC") %then %do;

   %If &DoLocate %then %do;
        %ISCcnvrt
        %Merge
   %end;

%end;


%MEND Locate;

**********************************************************************;
**********************************************************************;


%MACRO Stack;

%If &DoStack %then %do;
     %ChStack
     %DftStack
%end;


%MEND Stack;


**********************************************************************;
**********************************************************************;


%MACRO ProcFile;

%If &DoSetVar %then %do;
     %SetVar
%end;

%If &DoWindow %then %do;
     %WindData
%end;

**** If ASPEN, ensure lat and lon variables exist after PtDataProc ***;
%IF %upcase("&Model") = "ASPEN" and not &DoLocate and not &DoSetVar and not &DoWindow %THEN %DO;
   data outdata.&outsas;
      set outdata.&outsas(rename=(x=lon y=lat %if &renameMACT %then %do;
	        MACTcode=MACT %end;));
   run;
%END;

%MEND ProcFile;


**********************************************************************;
**********************************************************************;


***    MAIN PROGRAM ***;

OPTIONS mprint symbolgen;


%GetInfo

%Locate

%Stack

%ProcFile

Run;
