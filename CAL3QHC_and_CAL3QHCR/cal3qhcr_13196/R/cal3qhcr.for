C  **************************************************************************
C  *                                                                        *
C  *                         CAL3QHCR (DATED 13196)                         *
C  *                                                                        *
C  *                       *** SEE CAL3QHC MCB#8 ***                        *
C  *                                                                        *
C  *        ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS WEB SITE        *
C  *                                                                        *
C  *                      http://www.epa.gov/scram001                       *
C  *                                                                        *
C  **************************************************************************
C
C        Purpose:       This enhanced version of CAL3QHC was modified to
C                       process up to a year of hourly emissions, traffic,
C                       and signalization (ETS) data together with up to a
C                       year of hourly meteorological (MET) data.  Calculated
C                       concentrations are averaged based on the algorithms
C                       incorporated from the CHAVG program.  A preprocessor
C                       subroutine, CALPRE, is used to synchronize daily ETS
C                       traffic data with the respective day of the week.
C
C        Modified by:   Tom Baker
C                       EPA
C                       4930 Page Road
C                       D243-01
C                       Research Triangle Park, NC 27711
C
C        Written to:    FORTRAN 77 Standards, compatible with FORTRAN 90
C 
C               Note 1. A non-F77 method of obtaining the system date and time
C                       is used, because there was no standard for that in F77.
C                       The method used is the DATE_AND_TIME subroutine, which
C                       became standard later. If you need a F77 method, some
C                       of the other methods (named 'GETDAT', and 'DATE' and
C                       'TIME') are still in the code but commented out, or you
C                       can use a method that works with your compiler.
C 
C               Note 2. F77-specific features are used that were declared
C                       'obsolete' in F90, but which were still supported by
C                       the standard.  (Examples are use of the 'H' specifier,
C                       DO loop using a real or double variable, and the
C                       Arithmetic IF statement.)  Even though some of these
C                       are 'deleted' in the Fortran 95 standard, compiler
C                       manufacturers tend to leave them in the newer
C                       compilers.  Finally, again, all F90 compilers still
C                       support them, because they are compliant with the F90
C                       standard.
C
C        Modifications: September 9, 1994 - Original modifications to
C                       Parsons-Brinckerhoff CAL3QHC source code.
C
C                       December 8, 1994 - Incorporation of comments
C                                          Consolidation of source codes
C
C                       March 15, 1995 - Rural/urban mixing height switch
C                                        coupled to a collapse to "D"
C                                        stability for urban settings
C
C                       July 11, 1995 - The input wind angle is no longer
C                                       being read as a wind direction (WD).
C                                       The input wind angle is now read as
C                                       a wind flow value, (WD + 180 degrees),
C                                       for compatiblity with EPA preprocessed
C                                       meteorological data.
C
C                       June 23, 2004 - An array upper bound overflow condition
C                                       is possible when the number of calms in
C                                       one of the duration classes in the
C                                       Calms Duration Frequency table exceeds
C                                       200.  The related array sizes were
C                                       increased.  Also, when the mixing
C                                       heights are both zero, the program
C                                       crashes.  A substitute value is now
C                                       used and a warning message to check the
C                                       mixing height values has been
C                                       emphasized.
C                       September 1, 2004 - A bug that prevented Year 2000
C                                           meterological data from being used
C                                           was reported and fixed.
C
C                       December 20, 2012 - Increased precision on internal
C                                           concentrations. Increased numner
C                                           sources and receptors allowed.
C                                           Increased number of significant
C                                           digits in output fields.
C
C                       June 3, 2013 - Added output of concentrations in an
C                                      AERMOD Plot File format. Also cleaned
C                                      format and numbers in output of
C                                      date/time.
C
C                       June 10, 2013 - Version 13161 is a tweak, by request,
C                                      adding extra width and digits to fields
C                                      in the .ET1 and .ET2 files.
C                       July 15, 2013 - Version 13196 is a pair of bug fixes.
C                                       1) A change in the build command will keep
C                                       the console from being warned when an
C                                       output field overflows. When CO emission
C                                       rates are used in place of PM,
C                                       unrealistically high numbers can result,
C                                       so fields overflowing with asterisks
C                                       causing the error isn’t likely to happen
C                                       when the model is used correctly; it would
C                                       be nice if the error message is prevented.
C                                       2) The plot file needs to differentiate
C                                       between CO and PM reporting by changing
C                                       the third line in its header to show the
C                                       change in pollutant, units of measure, and
C                                       the averaging interval. 
C
C
C  **************************************************************************
C
C                                INPUT FORMAT
C
C      Note:  Input is in free format.  Single quotes need to be placed around
C             'character string input'.
C
C    'Title (up to 40 Char.)' ATIM, ZO, VS, VD, NR, CONV, IOPT
C    Beginning month, day, year,  Ending month, day, year
C    Meteorological data surface station ID & year, Upper Air Sta. Id & Yr
C    Link contribution flag, Amb. background conc., Rural/urban switch
C    Receptor name, X- and Y-coordinate, elevation
C      (The last line is repeated for each receptor)
C    Tier approach (1 or 2), Pollutant type ( 'c' for CO or 'p' for PM)
C    Pattern numbers (7 integers are entered - all 1's for Tier I approach)
C    'Run name or identification', number of links to be processed
C    Link number, link type
C    One entry for each link:
C    'Link name', type, beginning X, Y-coord, ending X, Y-Coord,
C           source height, lane width, [number of lanes (if IQ = 2)]
C    For each hour: (1 hour for Tier 1, up to 7 24-hourly sets for Tier 2 app.
C      Hour(2 digits), Background ambient concentration
C      One entry for each link:
C        For IQ = 1:
C          Link number, link volume, emission rate
C        For IQ = 2:
C          Link number, traffic light cycle time, red, yellow factor, Q volume,
C          idle emiss. factor, saturation flow rate, signal type, arrival rate
C
C ***************************************************************************
C
C                            EXECUTION PROCEDURES
C
C  Create an input file with the name: CAL3R.CTL
C    CAL3R.CTL Structure:
C      [full pathname] Message filename (eg MainMarket.MSG)
C      [full pathname] input filename  (eg C:\CAL3QHCR\Proj\MainMarket.INP)
C      [full pathname] meteorological filename (eg E:\Met\Medford\S4222590.asc)
C      [full pathname] ET1 filename  (eg fname.et1)
C      [full pathname] ET2 filename
C      [full pathname] Output filename
C      [full pathname] Link data filename
c      [full pathname] Plot file name
C    If all the files and executable are in the same subdirectory, the
C      full pathname is not needed.
C  Copy CAL3QHCR to the subdirectory where CAL3R.CTL is located
C  Double Click: CAL3QHCR
C
C  **************************************************************************
C
C                                DEFINITIONS
C
C   A      - Distance from link end point 1 to a receptor
C          - An hourly concentration
C          - Highest concentration
C   A1     - Ratio related directly to total signal length (=.38)
C   A2     - Ratio of non-red to total signal length
C   A3     - (1-A2) squared
C   A4     - Part of delay calculations
C   A5     -   for variable QAVG4 and written according to
C   A6     -   the 1985 Highway Capacity Manual formula.
C   ABSQD  - Absolute of the X length of a link divided by its actual length.
C   AFAC   - Correction factor for averaging time and surface roughness.  Used
C              in sigma Y and sigma Z equations.
C   ALL    - Labelled common
C   AMB    - Ambient background concentration  [ppm or ug/m**3].
C   ANGMAX - Wind direction in which the first maximum concentration occurs
C   APRI   - Distance from end point 1 to apparent location of receptor
C   ARG    - argument inside the sigma Y part of the Guassian
C              distribution equation
C   ARG1   - first argument inside an exponential function of a Guassian
C              distribution equation
C   ARG2   - second argument inside an exponential function of a Guassian
C              distribution equation
C   ARG3   - reflective term in Guassian equation.
C   ARG4   - reflective term in Guassian equation.
C   ARG5   - reflective term in Guassian equation.
C   ARG6   - reflective term in Guassian equation.
C   AT     - Arrival rate.
C              Enter "1" for worst progression,
C                    "2" for below average progression,
C                    "3" for average progression,
C                    "4" for above average progression,
C                    "5" for best progression.
C              Default is 3.
C   ATIM   - Averaging time [min].
C   AY1    - Dispersion coefficient at a distance of 1 meter based on
C              Pasquill curves.
C   AY2    - Dispersion coefficient at a distance of 10 kilometers based
C              on Pasquill curves.
C   AZ     - Dispersion coefficient at a distance of 10 Km for Sigma Z
C              calculations based on Pasquill curves.
C   B      - Distance from link end point 2 to a receptor.
C          - Highest of the second highest concentration
C   BASE   - Factor for element growth base.  Based on difference between
C              the link and wind flow directions.
C   BK     - Averaged background concentration.
C   BKG    - Averaged maximum or second high background concentration
C   BKGMAX - Background with maximum 1-hour average concentration.
C   BLNK   - Two blank spaces
C   BNTH   - Array of past 24 hours of hourly background concentrations.
C   BPRI   - Distance from end point 2 to apparent location of a receptor.
C   BRG    - Wind flow direction (direction wind is blowing toward) in degrees.
C   BRG1   - Wind direction converted from wind flow direction, BRG.
C   C      - Link contribution to receptor concentration.
C   CAVG   - Average total signal cycle length [s].
C   CHR    - Labeled common for character variables
C   CLAS   - Stability class number.
C   CMAX   - Current maximum concentration.
C   CNHR   - Past hourly concentration subtracted from running total
C   CNT    - Counter of reflective terms in dispersion equation.
C   CNTM   - Value subtracted from CUMT to maintain a running total
C   COD    - Link number
C   CON    - Output receptor concentration array.
C   CONTL  - Total single link contribution by rank, receptor, and
C              averaging period,
C   CPRE   - Subroutine used for preprocessing of ETS data
C   CRMAX  - Maximum concentration at each receptor.
C   CRMAXB - CRMAX with background concentration added.
C   CSL2   - Central element half-length in the wind flow direction..
C   CSUM   - Temporary sum of link concentration contributions for a receptor.
C   CUM    - Sum of concentrations for averaging period
C   CUMB   - Running 8-hour total of ambient background concentrations.
C   CUML   - Sum of link contributions by rank, receptor and averaging period
C   CUMT   - Total number of hours used in calculating averages.
C   D      - Distance from the actual receptor location perpendicular to the
C              specified link line.
C   D1     - Link half-width times twice the mixing height.
C   D2     - Link half-width.
C   DAYWK  - Day of the week (eg 'Monday  ')
C   DC     - Demand capacity ratio (V/IMU).
C   DDJ    - Approach delay * 1.3
C   DEG    - Converts radians to degrees.
C   DJ     - Approach delay.
C   DPRI   - Distance from the apparent receptor location perpendicular to the
C              specified link.
C   DREF   - Natural log of 10,000.  Used in calculating sigmas.
C   DSTR   - Resident time factor for pollutant in depressed highway section.
C   DT     - Labeled common containing date and time variables of run execution
C   DVIR   - Double precision distance to a receptor
C   DWL    - Downwind link offset distance
C   ECLD   - Element centerline distance
C   ED1    - See DWL
C   ED2    - See UWL
C   EF     - Emission factor  [g/veh-mi].  Set to 100.
C   EFL    - Emission factor  [g/veh-mi].  Set to 100.
C   EFRC   - factor in a deposition algorithm.
C   EL2    - Element half distance with respect to link centerline.
C  Element - A link is divided into square elements for processing purposes.
C              The first element has sides equal to the highway or link width.
C              Lengths of subsequent elements are based on the formula:
C                       EL = W * BASE **(NE - 1)
C              Each element is divided into five subelements.
C   ELL    - Projected width of the element based on wind flow direction with
C              respect to an element.
C   ELL2   - Equivalent line half-length.
C   EM     - Central element width.
C   EM2    - Central element half-width.
C   EN2    - Peripheral sub-element width.  There are 4 subelements with
C              this width.
C   EXIN   - Words stating whether AMB is excluded or included in concentration
C              calculations.
C   EXLS   - EXP1 + EXP2.
C   FAC1   - Source strength - wind speed factor.
C   FAC2   - Normal probability density function factor.
C   FAC3   - Deposition correction factor.
C   FAC4   - Settling correction factor.
C   FAC5   - Guassian distribution component.
C   FACT   - Elemental concentration adjusted by FAC1 through FAC5.
C   FAMB   - Flag to include ambient background concentrations in averages
C   FET    - Element fetch which is the downwind distance to a receptor
C              from a crosswind line drawn through the center of an element.
C   FINI   - Flag indicating when emissions from elements are no longer
C              affecting concentrations at a receptor.
C   FLINK  - Flag to include tables of link contributions by averaging period
C   FPPM   - Converts microgram per cubic meter to PPM
C   GAVG   - Average non-red signal cycle time.
C   GETDAT - Library function for retrieving date of program execution
C   GETTIM - Library function for retrieving time of program execution
C   H      - Source height, m.
C   HDS    - Source height in depressed highway.
C   HEM    - Receptor height minus source height.
C   HEP    - Receptor height plus source height.
C   HL     - Source height, m.
C   HLW    - Source height converted to feet.
C   HMAXA  - Highest 6 maximum concentration by receptor and averaging time.
C   HYP    - Direct distance from element center to receptor.
C   I      - Index.  Also flag to identify averaging period (max. = 5)
C   I4     - Reference number for shifting conc downward in high 5 table
C   IAIRID - Expected Upper Air MET station ID.  Read from the control file.
C   IAIRYR - Expected Upper Air MET station data year.
C   IAZ    - Index, if = 2, link contributions are calculated.
C   IB     - Hour flag added to CUMT. IB = 0 if winds are calm for that hour
C   IC15   - Printout line counter for the file containing link variable output
C   ICLM   - Flag indicating a 'calm wind' situation. (1 = yes)
C   ID     - Numeric day of the week in Subroutine CPRE
C   IDAY   - Day of program execution.
C   IDAYN  - Number of days since January 1, 4713 B.C.
C   IDC    - Index value for the variable DC.
C   IDLFAC - Idle emission factor  [g/veh-hr].
C   IDOW   - Integer representing day of the week a year begins.
C              (eg 1-Mon., 3-Wed., 7-Sun.)
C   IDY    - Day of data read or argument in subroutine Julian.
C   IDYMAX - Maximum number of days in each month in Sub. Julian
C   IEDY   - Requested ending day of run (Gregorian)
C   IEFLG  - Flag indicating that the ETS data file has been read to the
C              point where the ETS day and hour matches the current processing
C              day and hour.
C   IEJDAY - Ending Julian Day of processed data.
C   IEMN   - Requested ending month of run (Gregorian)
C   IEN    - Ending number used in controlling number of array values printed
C              per line to an output file.
C   IEYR   - Requested ending year of run (Gregorian)
C   IH     - Current processing hour.
C   IHE    - Ending hour of maximum receptor concentration
C   IHR    - Hour of run execution.
C          - Hour of ending concentration average (ie LH - 24*JDAY)
C   IHREND - Hour ending of ETS input data.
C   II     - Index for writing out link related information
C   IIHR   - Hour of program execution.
C   IJ     - Current processing Julian day.
C   IJ     - Index to concentration to be moved down 1 in high 5 table
C   IJP1   - Index to concentration moved down 1 in high 5 table
C   IJX    - Argument used for shifting High - 5 concentrations down one.
C   IK     - Index of concentration rank (ie maximum or second high)
C   IL     - Link data index or subscript.
C   ILIN   - Printout line number
C   ILK    - Unit number for storing variable link data.
C   IMET   - Unit number for reading MET data.
C   IMFCID - Surface MET station ID as read from the MET file header.
C   IMFCYR - Surface MET station data year.
C   IMFLG  - Flag indicating if there is not a match between the requested
C              MET data identifiers and the identifiers read from the MET file.
C   IMIN   - Minute of program execution.
C   IMIRID - Upper Air MET station ID as read from the MET file header.
C   IMIRYR - Upper Air MET station data year.
C   IMN    - Month argument in subroutine Julian.
C   IMON   - Month of program execution.
C   IMP    - Value used in calculating name of first day of year based on ID.
C   IMU    - Intersectional capacity (veh/hr)/(sec*sec).
C   INC    - Elemental concentration for each link element.
C   IN     - Unit number for control data input
C   INR    - Number of sections of output to generate.
C   INTG   - Probabilty density contribution by element and subelement.
C   IOPT   - Metric to english conversion in output option.  See SCAL.
C              Enter "1" for output in feet.
C              For any other number, the output will be in meters.
C   IP     - Number of consecutive calm wind flags read.
C   IP15   - Printout page counter for the file containing link variable data.
C   IPAG   - Printout page number for main output file.
C   IPATRY - Array storing which pattern number is assigned to which day of the
C              week.
C   IPFLG  - Flag indicating whether to print link variable data to file
C   IPLT   - Unit number for output plot file
C   IPLTI  - Index for output plot file
C   IPMAX  - Maximum number of patterns read.
C   IPO    - Unit number for printout going to file
C   IPP    - Counter to count up to 1 week of link variable data for storage
C   IQ     - Link type flag.
C              Enter "1" for free flow and
C                    "2" for queue links.
C   IR     - Receptor data index or subscript.
C   IRCLM  - Calm wind factor applied to the concentrations.  IRCLM = 0, when
C              ICLM = 1.
C   IRMAX  - Subscript referring to the receptor receiving the highest
C              concentration.
C   IRU    - Rural/urban mixing height flag (0-rural, 1-urban)
C   ISCN   - Unit number for output to the monitor
C   ISCTM  - Flag indicating whether maxima concentrations are being calculated
C              or link contribution are being calculated in main loop.
C   ISDY   - Starting day of processed data.
C   ISDY   - Requested Gregorian starting day of run
C   ISEC   - Second of program execution.
C   ISFCID - Expected Surface MET station ID as read from the control file.
C   ISFCYR - Expected Surface MET station data year.
C   ISIG   - Number of consecutive hours of calm winds considered significant.
C   ISJDAY - Starting Julian day of processed data.
C   ISKP   - Flag to indicate when to skip calculations not used in determining
C              link contributions.
C   ISMN   - Requested Gregorian starting month of run
C   ISN    - Starting number used in controlling number of array values printed
C              per line to an output file.
C   ISW    - Flag for printing new header
C   ISYR   - Starting year of processed data (eg 94).
C   ISYR4  - Four digit year (eg 1994)
C   IT1    - Unit number of ET1 data read by program.
C   IT2    - Unit number of ET2 data read by program.
C   ITAVG  - Subtracted from absolute ending hour for calculating JHS
C   ITR    - Number of ranks (eg maximum, second high, etc)
C   IV     - Approach volume on the queue link  [veh/hr].
C   IX     - Decimal fraction of a second at program executed.
C   IX     - Index in loop for calculating the day hour of each of the top 5
C              concentrations.
C   IY     - Total hours from the first of the year.
C   IYR    - Year argument in Subroutine Julian
C   IYRR   - Year program executed.
C   J      - Index or counter
C   J1-J4  - Counters used in printing Calm Duration Frequency Table
C   J8     - Index to hourly concentration subtracted from 8-hr running total
C   JA     - Index to concentration to be moved down 1 in a rankings table
C   JAX    - Index used in shifting concentrations downward in a rankings table
C   JB     - Index to concentration moved down 1 in a rankings table
C   JD     - Julian day maximum receptor concentration
C   JDAY   - Julian day.  Used as subroutine argument.
C   JDAY1  - Julian day set of calm wind hours ended
C   JDY    - Julian day argument in subroutine Julian.
C   JES    - Number of hours in full processing period.
C   JHR8   - Table of ending hours in subroutine RNRK
C   JHS    - Absolute starting hour of an averaging period.
C   JHX    - Table of ending hours in subroutine RNRK
C   JHXX   - Absolute hour from January 1 by rank, receptor, averaging period
C   JITS   - Current Julian day of ETS data read by program.
C   JJ     - Index used in a number of situations
C   JK     - Pattern index
C   JL     - Reference index for moving concentrations down 1 in a ranking tbl
C   JLONG  - Number of days in the full processing period.
C   JOB    - Job title or identification. (up to 40 characters allowed.)
C   JSDOW  - Numeric day of the week.
C   JTIER  - Flag to signify either a Tier I or II approach.
C   JTIER  - Identifies the type of approach used to preprocess the ETS data.
C              A Tier I (1) or II (2) approach is used.
C   JULIAN - Subroutine for converting Gregorian dates to Julian dates.
C   K      - Common index used in a number of DO LOOPs
C   K1     - Time lost getting queue in motion. (set at 2 seconds)
C   K1     - Receptor number with the highest concentration value.
C   K11    - Receptor number with the highest concentration value.
C   K2     - Receptor number with highest of the second highest concentration
C   KK     - Link number index for summing link contributions
C   KZ     - Sigma Z.
C   L      - Offset length of receptor from link.
C   L      - Index
C   L10    - Number of link contributions to write to a line of output(max.10)
C   L120   - Number of lines of link contributions to write
C   L3     - Denotes either a 8-hour running or 24-hour block averaging period
C   L4     - Denotes either a 1-hour block or annual averaging period
C   LA     - Index one of the four averaging periods covered by L3 and L4
C   LA2    - Index for one or two averaging periods
C   LDAY   - Day of program execution
C   LE     - Ending index value for calculating CO or PM concentrations.
C   LH     - Hour ending of concentration - read from a concentration file
C   LH1    - Ending hour of set of calm winds
C   LH2    - Local ending hour of link contribution
C   LHR    - Hour of run
C   LIMRAT - Positive ratio of crosswind distance to Sigma Y for a receptor,
C              standard deviations of element center from receptor.
C   LIM    - First dimension of RNMX & JHX; 1 + averaging period
C   LIM1   - LIM - 1; hours in averaging period
C   LINK   - Labelled common containing link variable data
C   LL     - Link length, meters.
C   LL     - Index
C   LL1    - Link length value computed from vehicle volume and intersection
C              capacity.  Used in computing new link length end point 2
C   LLL    - Index of averaging period, max value = 4
C   LLLL   - Index of averaging period, max value = 2
C   LLW    - Link length converted to feet.
C   LMIN   - Minute of program execution
C   LMON   - Month of program execution
C   LNK    - Link name (up to 20 characters)
C   LNKC   - Labelled common containing link names, types, mode, and DAYWK
C   LNKN   - Part of link contribution header; writes 'link'
C   LNKT   - Part of link contribution header; writes relative link number
C   LPRI   - Link offset for the apparent receptor measured in the
C              direction of the link.
C   LPYR   - Leap year flag
C   LS     - Starting index value for calculating CO or PM concs.
C   LSEC   - Second of current program running time
C   LX     - Fraction of a second of current program running time
C   LYR    - Year of current program running time
C   M      - Index
C   MAXL   - Maximum number of links that can be processed.
C   MAXR   - Maximum number of receptors that can be processed.
C   MDY    - Day of meteorological data.
C   MFLG   - Flag indicating that one of the met data values is out of range
C   MFLG1  - Flag indicating when to print a line of met data
C   MFLG2  - Flag indicating there may be an error in the met data
C   MHR    - Hour ending of meteorological data.
C   MITS   - Julian day of MET data read by program.
C   MIXH   - Urban mixing height  [m].
C   MMN    - Month of meteorological data.
C   MN     - Current month of ETS data read by program.
C   MODE   - Character flag indicating PM (P) or CO (C) emissions processing.
C   MODET  - Integer flag value. CO is 0; PM is 1. based on MODE
C   MOWT   - Molecular weight of CO.
C   MSGS   - Unit number for error messages
C   MVALN  - Name of meteorological value that maybe out of range
C   MYR    - Year of meteorological data.
C   N      - Index of 9 highest 8-hour running averages
C   NATM   - Set averaging times of 1, 8, 24, and run length hours
C   NC     - Number of calm hours making up the average.
C   NCN    - Maximum of 75% valid hours and total non calm hours for avg period
C   NCNT   - Minimum number of hours needed for 75% valid data.
C   NDAY   - Ending day for averaging period concentrations
C   NDAY   - Julian day at the beginning of each month minus 1
C   NDIF   - Difference between averaging period and non calm hours in period
C   NDV    - Array of NCN's by rank, receptor number, and averaging period type
C   NE     - Element number.
C   NEC    - Number of calm hours for each HMAXA
C   NFLG   - No calm wind hours found flag (0-no calms found).
C   NHR    - Block period counter for annual or period of running average
C   NHRS   - Total number of hours process for annual or period average
C          - Hour ending for averaged concentrations
C   NIP    - Counter for frequency of calm duration hours
C   NIPH   - Ending hour of calm wind period
C   NIPJ   - Ending Julian day of calm wind period
C   NL     - Number of links to be processed, max=5000.
C   NLANES - Number of travel lanes in queue link.
C   NN     - Parameter value setting the maxmium number of receptors
C              that can be processed per run
C   NOLNK  - Flag to indicate when a link is not in used for a particular hour.
C   NR     - Number of receptors to be processed, max=5000.
C   OUTRA  - Subroutine that outputs running average results for various
C              averaging periods
C   PAF    - Factor for calculating DJ based on arrival time,
C              demand capacity ratio, and signalization timing.
C   PD     - Probability distribution factor for an element or subelement
C   PECK1  - Minimun of DC or 1.0 (See A4 to A6).
C   PHI    - Wind angle with respect to link direction.
C   PI     - PI
C   PSCALE - Converts meters to feet.
C   PY1    - Factor of Gaussian Sigma Y power law equation.
C   PY2    - Exponent of Gaussian Sigma Y power law equation.
C   PYC1   - Array of Sigma Y parameters based on stability class
C   PYC2   - Array of Sigma Y factors based on stability class
C   PZ1    - Factor of Gaussian Sigma Z power law equation.
C   PZ2    - Exponent of Gaussian Sigma Z power law equation.
C   Q1     - Linear source strength parallel to the highway in ug/m**3.
C   QAVG   - Average number of vehicle per queue.
C   QAVG4  - Total lost time at intersection.
C   QE     - Central subelement lineal source strength.
C              Other subelements source strengths
C              are multiplied by 0.75 and 0.25 depending on positional distance
C              from the central element.
C   QLL    - Total length of vehicles queued.
C   QLT    - Initial length of link.
C   RAD    - Converts degrees to radians.
C   RAVG   - Average red total signal cycle length  [s].
C   RC     - Ratio of red signal cycle length to total signal cycle length.
C   RCP    - Receptor name. (REC1, REC2, ... REC60)
C   REC    - Receptor names of up to 20 characters each.
C   RNK5   - Subroutine that determines and sorts highest 5 sums
C   RNMX   - Table of high sums for each receptor
C   RNMX8  - Running maximum sums for 8-hour averaging period
C   RNRK   - Subroutine designed to rank running averages
C   RU     - Rural/urban character flag ('R' - rural, 'U' - Urban)
C   RUN    - Current run title.  Based on links used.  Up to 20 characters.
C   SCAL   - Scale conversion factor used to convert input units to meters
C              [if input units are in feet enter 0.3048,
C               if they are in meters, enter 1.0, etc.].
C   SFR    - Saturation flow rate [veh/hr/lane].
C   SGN    - To indicate when an element is downwind of a receptor.
C   SGY    - Vertical diffusivity estimate.
C   SGZ    - Sigma Z component.
C   SGZ1   - Part of Gassian Sigma Z equation.
C   SGZ2   - Sigma Z based factor. (-0.5/(SGZ1*SGZ1))
C   SIDE   - Element fetch (FET) squared. Part of Pythagorean Theorem
C   ST     - Signal type.
C              Enter "1" for pretimed,
C                    "2" for actuated,
C                    "3" for semiactuated.
C              Default is "1."
C   STAR   - Repository for asterisk to indicate highest or second high
C              concentration
C   STP    - Step factor.
C   STR    - Asterisk is stored here
C   SYC1   - Array of Sigma Y values at 1 meter.
C   SYC10  - Array of Sigma Y values at 10 Km.
C   SZ10   - Sigma Z value at 10 Km.
C   SZC10  - Array of Sigma Z values at 10 Km.
C   T      - Factor based on LIMRAT.
C   TC0    - Cosine of link orientation.
C   TC180  - Cosine of link orientation WRT 180 degrees.
C   TC270  - Cosine of link orientation WRT 270 degrees.
C   TC360  - Cosine of link orientation WRT 360 degrees.
C   TEMP   - Temporary storage location for UWL.
C   TER    - Total emission rate for link.
C   THETA  - Orientation of link WRT to true north.
C   TMP    - Ambient temperature, degrees C.
C   TOTL   - Total of link contributions
C   TR     - Resident time of pollutant in depressed highway.
C   TRUNC  - 'Yes'/'No' as to whether a queue has been truncated
C   TS0    - Sine of link orientation.
C   TS180  - Sine of link orientation WRT 180 degrees.
C   TS270  - Sine of link orientation WRT 270 degrees.
C   TS360  - Sine of link orientation WRT 360 degrees.
C   TYP    - Link type.
C              Enter "AG" for "at grade" or
C                    "FL" for "fill,"
C                    "BR" for "bridge"
C                    "DP" for "depressed".
C   U      - Wind speed  [m/s].
C   UWL    - Upwind link offset distance.
C   V      - Approach volume per lane on the queue link  [veh/hr].
C   V1     - Average of depositon and settling velocities [m/s].
C   VAL    - Meteorological data value
C   VD     - Deposition velocity [cm/s].  Converted to m/s.
C   VD1    - Deposition velocity [cm/s].
C   VERSN  - Parameter with program's version number.
C   VJ     - Vehicles per second.  Derived from approach volume, V.
C   VPH    - Traffic volume on link [veh/hr].
C   VPHL   - Traffic volume on link [veh/hr].
C   VS     - Settling velocity [cm/s].  Converted to m/s.
C   VS1    - Settling velocity [cm/s].
C   W      - Mixing zone width.
C   W2     - Mixing zone width divided by 2.
C   WL     - Mixing zone width.
C   WLW    - Mixing zone width converted to input units.
C   WT     - Source strength weighting factor for Jth sub-element.  See QE.
C   X      - Ratio of saturation vehicle flow rate over the total cycle length.
C   X1     - A line of dashes used in dividing sections of output.
C   X1     - Beginning link X-coordinate
C   XL1    - Link X-coordinate for end point 1 at intersection stopping line.
C   XL2    - Link X-coordinate for end point 2.
C   XPRI   - X-coordinate of apparent location of receptor as adjusted for
C              the x component of the wind vector.
C   XQD    - Link X-coordinate length, m.
C   XR     - X-coordinate of receptor.  Converted to meters.
C   XRUR   - Rural mixing height (m).
C   XURB   - Urban mixing height (m).
C   XVEC   - X-component of the wind vector.
C   XW1    - Link X-coordinate for end point 1 converted to the input units.
C   XW2    - Link X-coordinate for end point 2 converted to the input units.
C   XWR    - Receptor X-coordinate converted to the input units.
C   Y(1-6) - Adjustment for element end effect (polynomial approximation).
C   YE     - Third side of FET and SIDE triangle. YE is the distance from
C              the center of an element crosswind to the receptor.
C   YFAC   - Clearance lost time (portion of the yellow phase that is not
C              used by motorist)  [s].
C   YL1    - Link Y-coordinate for end point 1 at intersection stopping line.
C   YL2    - Link Y-coordinate for end point 2.
C   YPRI   - Y-coordinate of apparent location of receptor as adjusted for
C              the y component of the wind vector
C   YQD    - Link Y-coordinate length, m.
C   YR     - Y-coordinate of receptor.
C   YVEC   - Y-component of the wind vector
C   YW1    - Link Y-coordinate for end point 1 converted to feet.
C   YW2    - Link Y-coordinate for end point 2 converted to feet.
C   YWR    - Receptor Y-coordinate converted to feet
C   Z      - Adjustment to receptor height based on mixing height
C   Z0     - Surface roughness [cm].
C   Z0FAC  - Surface roughness adjustment factor based on a roughness length
C              of 10 cm and an exponent of .07.
C   Z2FAC  - Surface roughness adjustment factor based on a roughness length
C              of 3 cm and an exponent of .2.
C   Z7FAC  - Surface roughness adjustment factor based on a roughness length
C              of 3 cm and an exponent of .07.
C   ZFAC   - Time available minus start delay minus time for vehicle to clear
C              the intersection, s.
C   ZR     - Receptor elevation
C   ZWR    - Receptor Z-coordinate converted to feet
C
C  **************************************************************************
C
C                     VARIABLE DECLARATION STATEMENTS
C
C    Variable dimensions and initialization
C
C     Dimension structures:
C
C         (ranking, receptor number, avg type, link number)
C         (ranking, receptor number, avg type)
C         (ranking, receptor number)
C                  (receptor number, avg type)
C
CMOD      PARAMETER (NN = 60)
      PARAMETER (NN = 5000)

      PARAMETER (ILEN_FLD = 200)
      CHARACTER*200 PLTFRM
      CHARACTER*68  TITLE

      CHARACTER*80 FNZ(8)
      CHARACTER*40 JOB, RUN
CMOD      CHARACTER*20 LNK, RCP(60)
CMOD      CHARACTER*15 X1(60), MVALN(10)
      CHARACTER*20 LNK, RCP(NN)
      CHARACTER*15 X1(NN), MVALN(10)
      CHARACTER*10 DAYWK
      CHARACTER*9  EXIN
CMOD      CHARACTER*5  REC(60)
      CHARACTER*7  REC(NN)
      CHARACTER*4  LNKN, LNKT(10)
CMOD      CHARACTER*3  TRUNC(120)
      CHARACTER*3  TRUNC(NN)
      CHARACTER*2  BLNK, TYP
      CHARACTER*1  MODE, RU, STAR, STR
CMOD added variables cdate,ctime, and czone for date_and_time_subroutine
      CHARACTER CDATE*8,CTIME*10,CZONE*5
CL      CHARACTER  IDATE*8, ITIME*5, FILE5*40, FILE6*40
      CHARACTER*10 VERSN

CMOD changed TODAY from 3 to 8 and eliminated variable NOW
      INTEGER  ANGMAX(NN), AT, CAVG, CLAS, CNTM(24), COD,
     +         CUMT, FAMB, FLINK, MSGS, IHE(NN),
     +         IIHR, IMIN, ISEC, IX, IYRR, IMON, IDAY, JD(NN),
     +         MFLG(10), MFLG1, MFLG2, NOLNK(NN), RAVG, SFR,
     +         ST,TODAY(8)


CMOD
      REAL     IMU

      REAL     IDLFAC, INC, K1, KZ, LIMRAT, MIXH, MOWT, NE,
     +         PAF(5, 3, 3), VAL(10)
C
      DOUBLE PRECISION A, ABSQD, APRI, B, BPRI, D, D1, D2, DPRI, FAC2,
     +                 HLW, HYP, INTG(6),
     +                 L, LL(NN), LL1, LLW, LPRI,
     +                 PD, QLL, QLT(NN), SIDE, WLW,
     +                 XPRI, XW1, XW2, XWR,
     +                 YPRI, YW1, YW2, YWR, ZWR
C
CMOD      COMMON /ALL/ BKG(5,NN,2), CUM(NN,2), CUMB(4), CUMT(4),
CMOD     1             HMAXA(5,NN,2), ISYR, LPYR, IHR(5,NN,2), IPAG,
CMOD     2             JDAY, L3, L4, LH, NDAY(25), NHR,
CMOD     3             NHRS(25), NR, NC(9,NN), NEC(5,NN,2),
CMOD     4             NDV(9,NN,2)
      COMMON /ALL/ BKG(6,NN,2), CUM(NN,2), CUMB(4), CUMT(4),
     1             HMAXA(6,NN,2), ISYR, LPYR, IHR(6,NN,2), IPAG,
     2             JDAY, L3, L4, LH, NDAY(25), NHR,
     3             NHRS(25), NR, NC(9,NN), NEC(6,NN,2),
     4             NDV(9,NN,2), VERSN
      COMMON /CHR/ STAR(2,NN), BLNK, STR, JOB, RUN, EXIN
      COMMON /DT/  LHR, LMIN, LSEC, LX, LYR, LMON, LDAY
      COMMON /IO/  ICTL, IN, MSGS, IMET, IT1, IT2, IPO, ILK, ISCN, IPLT
      COMMON /JDY/ IEJDAY, ISJDAY
      COMMON /LINK/ AT(NN), CAVG(NN), COD(NN), EFL(NN),
     +              HL(NN), IDLFAC(NN), IDOW, IQ(NN), IV(NN),
     +              JTIER, MODET, NL, NLANES(NN), IPATRY(7),
     +              RAVG(NN), SFR(NN), ST(NN),
     +              VPHL(NN), WL(NN), XL1(NN), XL2(NN),
     +              YFAC(NN), YL1(NN), YL2(NN)
      COMMON /LNKC/ DAYWK(7), LNK(NN), MODE, TYP(NN)


C      ==================
C       DIMENSION ARRAYS
C      ==================
C
      DIMENSION AY1(6), AY2(6), AZ(6), BKGMAX(NN), C(NN,NN),
     +          CON(NN), CRMAX(NN), CRMAXB(NN), DC(NN), DJ(NN),
     +          GAVG(NN), IMU(NN), PYC1(6), PYC2(6),
     +          QAVG4(NN), RC(NN), SYC1(6), SYC10(6),
     +          SZC10(6), TER(NN), THETA(NN),
     +          TS180(NN), TC180(NN), TC270(NN), TS270(NN),
     +          TS360(NN), TC360(NN), TS0(NN), TC0(NN)
      DIMENSION V(NN), VJ(NN), WT(5),
     +          X(NN), XQD(NN),
     +          Y(6), YQD(NN),
     +          ZFAC(NN)
      DIMENSION DDJ(NN)
      DIMENSION BK(9,NN), BNTH(24), CNHR(NN,24),
     1          JHR8(9,NN), NATM(4),
     2          NIP(24), NIPJ(24,2000), NIPH(24,2000),
     3          RNMX8(9,NN), XR(NN), YR(NN), ZR(NN)
      DIMENSION ITAVG(4), JHXX(2,NN,2), JHS(2,NN,2)
      DIMENSION CONTL(2,NN,2,NN), TOTL(2,NN,2)
C
C      ============
C       DATA INPUT
C      ============
C
      DATA AZ  /1112.,  566., 353., 219.,  124., 56./
      DATA AY1 / 0.46,  0.29, 0.18, 0.11, 0.087, 0.057/
      DATA AY2 /1831., 1155., 717., 438.,  346., 227./
      DATA WT  / 0.25,  0.75,   1., 0.75,  0.25/
      DATA BNTH/24*0.0/, CMAX/0./, CNHR/120000*0./, CNTM/24*0/,
     1     CONTL/100000000*0.0/, CRMAXB/NN*0./, ILIN/0/,
     2     IP/0/, IP15/0/, IPP/0/,
     3     ITAVG / 7, 0, 23, 8784/,J8/16/, JHR8/45000*0/,
     4     NATM/8,1,24,8760/, NIP/24*0/, RNMX8/45000*0./,
     5     TOTL/20000*0.0/,
     6     XR/NN*0.0/, YR/NN*0.0/, ZR/NN*0.0/

      DATA LNKN /'Link'/, LNKT /' +1',' +2',' +3',' +4',' +5',' +6',
     +                          ' +7',' +8',' +9','+10'/
      DATA MVALN / 'Year', 'Month', 'Day', 'Hour', 'Wind Direction',
     +            'Wind Speed', 'Temperature', 'Stability Class',
     +            'Rural Mix Hgt', 'Urban Mix Hgt'/
      DATA X1 /NN*'---------------'/

CMOD:
CMOD      DATA REC/'REC1 ','REC2 ','REC3 ','REC4 ','REC5 ','REC6 ','REC7 ',
CMOD     +         'REC8 ','REC9 ','REC10','REC11','REC12','REC13',
CMOD     +         'REC14','REC15','REC16','REC17','REC18','REC19',
CMOD     +         'REC20','REC21','REC22','REC23','REC24','REC25',
CMOD     +         'REC26','REC27','REC28','REC29','REC30','REC31',
CMOD     +         'REC32','REC33','REC34','REC35','REC36','REC37',
CMOD     +         'REC38','REC39','REC40','REC41','REC42','REC43',
CMOD     +         'REC44','REC45','REC46','REC47','REC48','REC49',
CMOD     +         'REC50','REC51','REC52','REC53','REC54','REC55',
CMOD     +         'REC56','REC57','REC58','REC59','REC60'/
C
C      =========================
C       LOOKUP TABLE DEFINITION
C      =========================
C
      DATA PAF /1.85, 1.35, 1.00, 0.72, 0.53,
     +          1.50, 1.22, 1.00, 0.82, 0.67,
     +          1.40, 1.18, 1.00, 0.90, 0.82,
     +          1.54, 1.08, 0.85, 0.62, 0.40,
     +          1.25, 0.98, 0.85, 0.71, 0.50,
     +          1.16, 0.94, 0.85, 0.78, 0.61,
     +          1.85, 1.35, 1.00, 0.72, 0.42,
     +          1.50, 1.22, 1.00, 0.82, 0.53,
     +          1.40, 1.18, 1.00, 0.90, 0.65/

C     ===========
C      I/O UNITS
C     ===========
C
C        5   ICTL - Control File
C        6   ISCN - Statements sent to Monitor
C        7 - IN   - Input File
C        8 - MSGS - Error & other messages sent to a file
C        9 - IPO  - Printout to a file
C       10 - IMET - Meteorological Input File (ASCII format)
C       13 - IPLT - Output Plot File
C       15 - ILK  - Output file of link variable data
C
C           Intersection Dimensions, Traffic Emissions &
C              Signalization Input Files
C
C       11 - IT1 - File of ET1 data
C       12 - IT2 - File of ET2 data
C
C     ===============================================
C      MAXIMUM NUMBER OF RECEPTORS ALLOWED BY PROGRAM
C     ===============================================
C
      VERSN = '13196'

CMOD: Initialize Receptor No. labels:
      DO I = 1, NN
         WRITE(REC(I),1234) I
1234     FORMAT('REC',I4.4)
      ENDDO
CMOD:
      imflg = 0
      MFLG  = 0
      MFLG1 = 0
      MFLG2 = 0
CMOD
      IPFLG = 1
      MAXR=NN
      IRMAX = 1
      A = 0.0
C
C Get date and time of program execution. (Microsoft compiler specific)
C
CMOD   CALL GETDAT (IYRR, IMON, IDAY)
        CALL DATE_AND_TIME(CDATE,CTIME,CZONE,TODAY)
        IYRR = TODAY(1)
        IMON = TODAY(2)
        IDAY = TODAY(3)
        IIHR = TODAY(5)
        IMIN = TODAY(6)
        ISEC = TODAY(7)
        LYR  = IYRR
        LMON = IMON
        LDAY = IDAY
        LHR  = IIHR
        LMIN = IMIN
        LSEC = ISEC

CMOD   CALL GETTIM (IIHR, IMIN, ISEC, IX)
        IX = 0
        IYRR = MOD(IYRR,100)

CL
CL      CALL TIME(ITIME)
CL      CALL DATE(IDATE)
CLC
CL      CALL GETFIL(FILE5,FILE6)
CLC
CL          WRITE (ISCN,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME

C     Open Control File (*.CTL) and read in I/O file names, and
C          OPEN data files
C
      OPEN (ICTL, FILE ='cal3r.ctl' , STATUS='OLD', ERR = 89)
      DO I = 1, 7
        READ(ICTL, *) FNZ(I)
        WRITE(ISCN, *) 'READ IN: ', TRIM(FNZ(I))
      END DO
c     This one is separated to emphasize that it is a little different
      DO I = 8, 8
        READ(ICTL, *, ERR = 87, END = 87) FNZ(I)
        WRITE(ISCN, *) 'READ IN: ', TRIM(FNZ(I))
      END DO
      CLOSE(ICTL)
      GOTO 88
   87 CONTINUE
      WRITE(ISCN,*) ''
      WRITE(ISCN,*) 'To:      User'
      WRITE(ISCN,*) 'Subject: Format of the .CTL file for CAL3QHCR'
      WRITE(ISCN,*) ''
      WRITE(ISCN,*) 'It is necessary to specify the Plot File name. Tha
     &t appears to'
      WRITE(ISCN,*) 'not be included in the .CTL file that you submitte
     &d. That is'
      WRITE(ISCN,*) 'why this run of CAL3QHCR is being aborted now.'
      WRITE(ISCN,*) ''
      WRITE(ISCN,*) 'Please correct the .CTL file and try your run agai
     &n.'
      WRITE(ISCN,*) ''
        STOP
   88 CONTINUE

      OPEN (MSGS, FILE = FNZ(1),STATUS='UNKNOWN', ERR = 97)
      OPEN (IN, FILE = FNZ(2) ,STATUS='OLD', ERR = 91)
      OPEN (IMET, FILE = FNZ(3) ,STATUS='OLD', ERR = 92)
      OPEN (IT1, FILE = FNZ(4) ,STATUS='UNKNOWN', ERR = 93)
      OPEN (IT2, FILE = FNZ(5) ,STATUS='UNKNOWN', ERR = 94)
      OPEN (IPO, FILE = FNZ(6) ,STATUS='UNKNOWN', ERR = 95)
      OPEN (ILK, FILE = FNZ(7) ,STATUS='UNKNOWN', ERR = 96)
      OPEN (IPLT, FILE = FNZ(8) ,STATUS='UNKNOWN', ERR = 98)
      GOTO 99

   89   WRITE(MSGS,*) 'Error opening the control file CAL3R.CTL.'
        WRITE(MSGS,*) 'CAL3R.CTL replaces listing the filenames '
        WRITE(MSGS,*) 'on a Command Line.'
        WRITE(MSGS,*) 'There should be 7 filenames listed in CAL3R.CTL'
        WRITE(MSGS,*) 'See the source code listing for its structure'
        WRITE(MSGS,*) 'under: EXECUTION Procedures.'
        WRITE(ISCN,*) 'Error opening the control file CAL3R.CTL.'
        WRITE(ISCN,*) 'CAL3R.CTL replaces listing the filenames '
        WRITE(ISCN,*) 'on a Command Line.'
        WRITE(ISCN,*) 'There should be 7 filenames listed in CAL3R.CTL'
        WRITE(ISCN,*) 'See the source code listing for its structure.'
        WRITE(ISCN,*) 'under: EXECUTION Procedures.'
          STOP
   91   WRITE(MSGS,*) 'Error opening the input file.'
        WRITE(ISCN,*) 'Error opening the input file.'
          STOP
   92   WRITE(MSGS,*) 'Error opening the meteorological datafile.'
        WRITE(ISCN,*) 'Error opening the meteorological datafile.'
          STOP
   93   WRITE(MSGS,*) 'Error opening the first intermediate file.'
        WRITE(ISCN,*) 'Error opening the first intermediate file.'
          STOP
   94   WRITE(MSGS,*) 'Error opening the second imtermediate file.'
        WRITE(ISCN,*) 'Error opening the second imtermediate file.'
          STOP
   95   WRITE(MSGS,*) 'Error opening the output file.'
        WRITE(ISCN,*) 'Error opening the output file.'
          STOP
   96   WRITE(MSGS,*) 'Error opening the detailed link file.'
        WRITE(ISCN,*) 'Error opening the detailed link file.'
          STOP
   97   WRITE(ISCN,*) 'Error opening the message file.'
          STOP
   98   WRITE(MSGS,*) 'Error opening the output plot file.'
        WRITE(ISCN,*) 'Error opening the output plot file.'
          STOP
   99 CONTINUE
C      ==========================================
C       INITIALIZATION OF CONSTANTS AND COUNTERS
C      ==========================================
C
      DO 3 I = 1, NN
        COD(I) = I
    3 CONTINUE
C
      PI   = 3.1415926
      RAD  = PI / 180.
      DEG  = 180. / PI
      PSCALE= 1 / 0.3048
      DREF = ALOG(10000.)
C
C      ============
C       INPUT DATA
C      ============
C
      WRITE(MSGS,*) ' '
      WRITE(MSGS,*) 'READING INPUT FROM THE MAIN CONTROL FILE'
      WRITE(ISCN,*) ' '
      WRITE(ISCN,*) 'READING INPUT FROM THE MAIN CONTROL FILE'

C
      READ(IN,*,END=9999) JOB, ATIM, Z0, VS, VD, NR, SCAL, IOPT

C     CHECK NUMBER OF RECEPTORS INPUT DOES NOT EXCEED MAXIMUM ALLOWED.

        IF (NR.GT.MAXR) THEN
           WRITE (MSGS,167) NR, MAXR
           STOP
        END IF
C
C    ------------------------------------------------------------
C     CORRECTION FACTORS FOR AVERAGING TIME AND SURFACE ROUGHNESS
C    -------------------------------------------------------------
C
        AFAC  = (ATIM/3.0)**.2
        Z2FAC = (Z0/3.)**.2
        Z7FAC = (Z0/3.)**.07
        Z0FAC = (Z0/10.)**.07
C
      DO 4 CLAS = 1, 6
        SYC1(CLAS) = ALOG(AY1(CLAS) * Z2FAC * AFAC)
        SYC10(CLAS)= ALOG(AY2(CLAS) * Z7FAC * AFAC)
        SZC10(CLAS)= ALOG(AZ(CLAS)  * Z0FAC * AFAC)
        PYC1(CLAS) = EXP(SYC1(CLAS))
        PYC2(CLAS) = (SYC10(CLAS)-SYC1(CLAS))/DREF
    4 CONTINUE
C
      VS1=VS
      VD1=VD
C
C      ---------------------
C       CONVERT CM/S TO M/S
C      ---------------------
      VS=VS/100.
      VD=VD/100.
      V1=VD-VS/2.
C
C      --------------------------------------
C       READ STARTING & ENDING DATE AND TIME
C      --------------------------------------
C
      READ(IN,*) ISMN, ISDY, ISYR, IEMN, IEDY, IEYR

C      Convert ISYR and IEYR to Four Digits
          IF (ISYR .GE. 50 .AND. ISYR .LE. 99) THEN
              ISYR = 1900 + ISYR
            ELSE IF (ISYR .LT. 50) THEN
              ISYR = 2000 + ISYR
          END IF
          IF (IEYR .GE. 50 .AND. IEYR .LE. 99) THEN
              IEYR = 1900 + IEYR
            ELSE IF (IEYR .LT. 50) THEN
              IEYR = 2000 + IEYR
          END IF

C      Calculate JULIAN Day for Start and End Dates

         CALL JULIAN (ISYR,ISMN,ISDY,ISJDAY)
         CALL JULIAN (IEYR,IEMN,IEDY,IEJDAY)
           ISYR4 = ISYR

C      Convert Years Back to Two Digits

          ISYR = ISYR - 100*INT(ISYR/100)
          IEYR = IEYR - 100*INT(IEYR/100)

         WRITE(MSGS,31) ISMN, ISDY, ISYR, ISJDAY,
     &                  IEMN, IEDY, IEYR, IEJDAY
         WRITE(ISCN,31) ISMN, ISDY, ISYR, ISJDAY,
     &                  IEMN, IEDY, IEYR, IEJDAY

         ITAVG(4) = (IEJDAY-ISJDAY+1) * 24 - 1
C
C
C      ---------------------------------------
C       READ METEOROLOGICAL FILE CONTROL DATA
C      ---------------------------------------
C
C
      WRITE(MSGS,*) ' '
      WRITE(MSGS,*) 'READING DATA FROM THE MET FILE'
      WRITE(ISCN,*) ' '
      WRITE(ISCN,*) 'READING DATA FROM THE MET FILE'
C
      READ(IN,*)     ISFCID, ISFCYR, IAIRID, IAIRYR
      READ(IMET,*)  IMFCID, IMFCYR, IMIRID, IMIRYR

         IF (ISFCID .NE. IMFCID .OR. ISFCYR .NE. IMFCYR) IMFLG = 1
         IF (IAIRID .NE. IMIRID .OR. IAIRYR .NE. IMIRYR) IMFLG = 1
         IF (IMFLG .EQ. 1) THEN
           WRITE (MSGS, 11)
           WRITE (MSGS, 12) ISFCID, ISFCYR, IAIRID, IAIRYR
           WRITE (MSGS, 13) IMFCID, IMFCYR, IMIRID, IMIRYR
           WRITE (ISCN, 11)
           WRITE (ISCN, 12) ISFCID, ISFCYR, IAIRID, IAIRYR
           WRITE (ISCN, 13) IMFCID, IMFCYR, IMIRID, IMIRYR
            STOP
         END IF

      READ(IN, *) FLINK, FAMB, RU
C
C      --------------------
C       RECEPTOR LOCATIONS
C      --------------------
C
      WRITE(MSGS,*) ' '
      WRITE(MSGS,*) 'READING RECEPTOR LOCATIONS FROM THE CONTROL FILE'
      WRITE(ISCN,*) ' '
      WRITE(ISCN,*) 'READING RECEPTOR LOCATIONS FROM THE CONTROL FILE'
C
      DO 1000 I=1,NR
        READ(IN,*) RCP(I),XR(I),YR(I),ZR(I)
          XR(I)=SCAL*XR(I)
          YR(I)=SCAL*YR(I)
          ZR(I)=SCAL*ZR(I)
 1000 CONTINUE

C
C        SELECT TYPE OF AVERAGE NEEDED CO OR PM
C

      READ(IN, *) JTIER, MODE
        MODET = 2

          IF (MODE .EQ. 'c') MODET = 0
          IF (MODE .EQ. 'C') MODET = 0
          IF (MODE .EQ. 'p') MODET = 1
          IF (MODE .EQ. 'P') MODET = 1

C     --- MOLECULAR WEIGHT OF CO --

      MOWT = 28.

      IF (MODET .EQ. 0) THEN
        FPPM = 0.0245 / MOWT
       ELSE
        FPPM = 1.0
      END IF

C
C      ----------------
C       LINK VARIABLES
C      ----------------
C
C    CHECK NUMBER OF LINKS INPUT DOES NOT EXCEED MAXIMUM ALLOWED.
C
      WRITE(MSGS,*) ' '
      WRITE(MSGS,*) 'READING LINK (ETS) DATA FROM THE MAIN INPUT FILE'
      WRITE(ISCN,*) ' '
      WRITE(ISCN,*) 'READING LINK (ETS) DATA FROM THE MAIN INPUT FILE'

      CALL CPRE(ISYR, ISYR4)
C
      WRITE(MSGS,*) ' '
      WRITE(MSGS,*) 'FINISHED READING CONTROL FILE INPUT'
      WRITE(ISCN,*) ' '
      WRITE(ISCN,*) 'FINISHED READING CONTROL FILE INPUT'

C     READ LINK NAME, TYPE AND DIMENSIONS

        DO 15 I = 1, NL
          XL1(I)=SCAL*XL1(I)
          XL2(I)=SCAL*XL2(I)
          YL1(I)=SCAL*YL1(I)
          YL2(I)=SCAL*YL2(I)
          HL(I) =SCAL*HL(I)
          WL(I) =SCAL*WL(I)
C                                                          LINK LENGTH
          LL(I) = SQRT((XL1(I)-XL2(I))**2+(YL1(I)-YL2(I))**2)
          QLT(I) = LL(I)
           IF (LL(I).GE.WL(I)) GOTO 16
             WRITE (MSGS,17)
             WRITE (ISCN,17)
             STOP
   16      IF (ABS(HL(I)).LE.10.) GOTO 19
             WRITE (MSGS,18)
             WRITE (ISCN,18)
             STOP

C          CALCULATION OF THETA(I), THE ANGLE FORMED BY THE ASSUMED
C            QUEUE AND THE CHOSEN COORDINATE SYSTEM.

   19     XQD(I)=XL2(I)-XL1(I)
          YQD(I)=YL2(I)-YL1(I)
          ABSQD=ABS(XQD(I)/LL(I))
          IF(ABSQD.GT.1.0) ABSQD=1.0
          THETA(I)=DEG*DACOS(ABSQD)

          IF (XQD(I).GT.0. .AND.
     +        YQD(I).GE.0.) THETA(I)=90.-THETA(I)
          IF (XQD(I).GE.0. .AND.
     +        YQD(I).LT.0.) THETA(I)=90.+THETA(I)
          IF (XQD(I).LT.0. .AND.
     +        YQD(I).LE.0.) THETA(I)=270.-THETA(I)
          IF (XQD(I).LE.0. .AND.
     +        YQD(I).GT.0.) THETA(I)=270.+THETA(I)

          TS180(I) = SIN(RAD*(180.-THETA(I)))
          TC180(I) = COS(RAD*(180.-THETA(I)))
          TC270(I) = COS(RAD*(270.-THETA(I)))
          TS270(I) = SIN(RAD*(270.-THETA(I)))
          TS360(I) = SIN(RAD*(360.-THETA(I)))
          TC360(I) = COS(RAD*(360.-THETA(I)))
          TS0(I)   = SIN(RAD*THETA(I))
          TC0(I)   = COS(RAD*THETA(I))

C
   15  CONTINUE
C
          IPAG=IPAG+1
          WRITE (IPO, 200) CHAR(32), VERSN
          WRITE (IPO, 462) IMON, IDAY, IYRR, IPAG
          WRITE (IPO, 463) IIHR, IMIN, ISEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
          WRITE (IPO, 210) JOB,RUN
          WRITE (IPO,   1)
          WRITE (IPO,  31) ISMN,  ISDY,  ISYR,  ISJDAY,
     +                     IEMN,  IEDY,  IEYR,  IEJDAY

          WRITE (IPO,  43) JTIER
            IF (MODET .EQ. 0) THEN
              WRITE(IPO,23) MODE(1:1)
            ENDIF
            IF (MODET .EQ. 1) THEN
              WRITE(IPO,24) MODE(1:1)
            ENDIF
            IF (MODET .EQ. 2) THEN
              WRITE(IPO,*) 'The MODE variable, ',MODE,
     +                      ' was incorrectly entered.'
              STOP
            END IF

            IF (FAMB .EQ. 1) THEN
              WRITE(IPO, 47)
              EXIN = 'INCLUDING'
             ELSE
              WRITE(IPO, 48)
              EXIN = 'EXCLUDING'
            END IF

          WRITE (IPO, 220)
          WRITE (IPO, 230) VS1, VD1, Z0, ATIM
          WRITE (IPO,  32) ISFCID, ISFCYR, IAIRID, IAIRYR

            IF (JTIER .EQ. 2 .AND. (ISFCYR .NE. ISYR)) THEN
              ILIN = ILIN + 1
              WRITE (IPO, 34) ISYR, ISFCYR
            END IF

C       Process Rural/Urban mixing height selection switch

          IRU = 2
          IF (RU .EQ. 'R' .OR. RU .EQ. 'r') THEN
             IRU = 0
             ILIN = ILIN + 2
             WRITE(IPO, 56)
          END IF
          IF (RU .EQ. 'U' .OR. RU .EQ. 'u') THEN
             IRU = 1
             ILIN = ILIN + 2
             WRITE(IPO, 57)
          END IF
          IF (IRU .EQ. 2) THEN
            WRITE(IPO, *)
            WRITE(IPO, *) '   The Rural/Urban mixing height selection ',
     &                    'switch was incorrectly set to: ', RU
            STOP
          END IF

          WRITE(IPO,33) ISYR4, DAYWK(IDOW)
          ILIN = ILIN + 31
          IF (JTIER .EQ. 2) THEN
            WRITE (IPO, 44)
            DO 36 I = 1, 7
               WRITE(IPO,37) IPATRY(I), DAYWK(I)
   36       CONTINUE
            ILIN = ILIN + 8
          END IF

          WRITE(IPO,30)
            IF(IOPT.EQ.1)THEN
              WRITE(IPO,265)
              WRITE(IPO,275)
             ELSE
              WRITE(IPO,260)
              WRITE(IPO,270)
            END IF
              WRITE(IPO,280)
          ILIN = ILIN + 8

         DO 60 I = 1, NL

            IF (IOPT.NE.1)THEN
              IF (IQ(I).EQ.2) THEN
                WRITE(IPO,293) COD(I),LNK(I),XL1(I),YL1(I),
     +                   XL2(I),YL2(I),LL(I),THETA(I),TYP(I),
     +                   HL(I),WL(I),NLANES(I)
               ELSE
                WRITE(IPO,295) COD(I),LNK(I),XL1(I),YL1(I),
     +                   XL2(I),YL2(I),LL(I),THETA(I),TYP(I),HL(I),WL(I)
              END IF

            ELSE

               XW1=XL1(I)*PSCALE
               XW2=XL2(I)*PSCALE
               YW1=YL1(I)*PSCALE
               YW2=YL2(I)*PSCALE
               LLW=LL(I)*PSCALE
               HLW=HL(I)*PSCALE
               WLW=WL(I)*PSCALE
             IF(IQ(I).EQ.2)THEN
               WRITE(IPO,293) COD(I),LNK(I),XW1,YW1,XW2,
     +                   YW2,LLW,THETA(I),TYP(I),HLW,WLW,NLANES(I)

              ELSE

               WRITE(IPO,295) COD(I),LNK(I),XW1,YW1,XW2,
     +                   YW2,LLW,THETA(I),TYP(I),HLW,WLW
             END IF

            END IF
            ILIN = ILIN + 1
             IF (ILIN .GT. 55) THEN
               IPAG=IPAG+1
               ILIN = 15
               WRITE (IPO, 200) CHAR(12), VERSN
               WRITE (IPO, 462) IMON, IDAY, IYRR, IPAG
               WRITE (IPO, 463) IIHR, IMIN, ISEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
               WRITE (IPO, 210) JOB,RUN
               IF (NL .NE. I) THEN
                 WRITE(IPO,30)
                 IF(IOPT.EQ.1)THEN
                   WRITE(IPO,265)
                   WRITE(IPO,275)
                  ELSE
                   WRITE(IPO,260)
                   WRITE(IPO,270)
                 END IF
                   WRITE(IPO,280)
               END IF
            END IF
   60    CONTINUE
C
C         -----------------------------------------
C           SUMMARY OUTPUT FOR RECEPTOR LOCATIONS
C         -----------------------------------------
C
          IF (ILIN .GT. 45) THEN
            IPAG=IPAG+1
            ILIN = 6
            WRITE (IPO, 200) CHAR(12), VERSN
            WRITE (IPO, 462) IMON, IDAY, IYRR, IPAG
            WRITE (IPO, 463) IIHR, IMIN, ISEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
            WRITE (IPO, 210) JOB,RUN
          END IF

          IF (ILIN .LE. 45) THEN
            WRITE(IPO,46)
            IF(IOPT.EQ.1)THEN
              WRITE(IPO,310)
             ELSE
              WRITE(IPO,309)
            END IF
            WRITE(IPO,321)
            WRITE(IPO,331)
            ILIN = ILIN + 7
          END IF

          DO 70 I=1,NR

              IF(IOPT.EQ.1)THEN
                XWR=XR(I)*PSCALE
                YWR=YR(I)*PSCALE
                ZWR=ZR(I)*PSCALE
                WRITE(IPO,341) I, RCP(I), XWR, YWR, ZWR
               ELSE
                WRITE(IPO,341) I, RCP(I), XR(I), YR(I), ZR(I)
              END IF
            ILIN=ILIN+1

            IF (ILIN .GT. 55) THEN
              IPAG=IPAG+1
              ILIN = 6
              WRITE (IPO, 200) CHAR(12), VERSN
              WRITE (IPO, 462) IMON, IDAY, IYRR, IPAG
              WRITE (IPO, 463) IIHR, IMIN, ISEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
              WRITE (IPO, 210) JOB,RUN
              IF (NR .NE. I) THEN
                WRITE(IPO,46)
                IF(IOPT.EQ.1)THEN
                  WRITE(IPO,310)
                 ELSE
                  WRITE(IPO,309)
                END IF
                WRITE(IPO,321)
                WRITE(IPO,331)
                ILIN = ILIN + 7
              END IF
            END IF

   70     CONTINUE

C
C      ======================
C       MAIN PROCESSING LOOP
C      ======================
C
      WRITE(MSGS,*) ' '
      WRITE(MSGS,*) 'ENTERING MAIN PROCESSING LOOP'
      WRITE(ISCN,*) ' '
      WRITE(ISCN,*) 'ENTERING MAIN PROCESSING LOOP'

      ISCTM = 0
      IC15 = 60
      REWIND IT2

      DO 9500 IAZ = 1, 1 + FLINK

        IF (IAZ .EQ. 2) THEN
          WRITE(MSGS, *) ' '
          WRITE(MSGS, *) 'CALCULATING LINK CONTRIBUTIONS.'
          WRITE(ISCN, *) ' '
          WRITE(ISCN, *) 'CALCULATING LINK CONTRIBUTIONS.'
        END IF

        JSDOW = ISJDAY + IDOW - 1
        JSDOW = MOD(JSDOW,7)
          IF (JSDOW .EQ. 0) JSDOW = 7
        JSDOW = JSDOW - 1
        DO 8900 IJ = 1, JSDOW
          DO 8902 IH = 1, 24
               READ(IT2,*) IEYR, IDY, IHREND, AMB
            DO 8903 I = 1, NL
              IF (IQ(I) .EQ. 2) THEN
                READ(IT2,*)  COD(I),CAVG(I),RAVG(I),YFAC(I),IV(I),
     +                       IDLFAC(I), SFR(I),ST(I),AT(I)
               ELSE
                READ(IT2,*) COD(I), VPHL(I), EFL(I)
              END IF
 8903       CONTINUE
 8902     CONTINUE
 8900   CONTINUE
        ISKP = 0
        IEFLAG = 0

        WRITE(MSGS,53) ISJDAY
        WRITE(ISCN,53) ISJDAY

        DO 9000 IJ = ISJDAY, IEJDAY

C        Processing data for Julian day #....

          IF ((MOD(IJ,30) .EQ. 0) .AND. (IJ .NE. ISJDAY)) THEN
            MN = IJ
            WRITE(MSGS,53) MN
            WRITE(ISCN,53) MN
C              IMNST = MN
          END IF

C             REWIND ET2 file once a 'week'
            JSDOW = JSDOW + 1
            IF (JSDOW .EQ. 8) THEN
              REWIND IT2
              JSDOW = 1
            END IF

        DO 9000 IH = 1, 24

          NHR = NHR + 1
          LH = (IJ - 1) * 24 + IH

          IF (FLINK .EQ. 1 .AND. ISCTM .EQ. 1) THEN
            ISKP = 1
            LA2 = 2
             IF (MODET .EQ. 1) LA2 = 1
            DO 9100 IR = 1, 2
            DO 9100 I = 1, NR
            DO 9100 LA = 1, LA2
              IF (LH .GE. JHS(IR,I,LA) .AND.
     +            LH .LE. JHXX(IR,I,LA))    ISKP = 0
 9100       CONTINUE
          END IF

        READ(IT2,*) IEYR, IDY, IHREND, AMB

          IF (IEFLAG .EQ. 0) THEN
            IEFLAG = 1
            JITS = MOD(IJ + IDOW - 1,7)
            IF  (JITS .EQ. 0) JITS = 7
            IF ((JITS .NE. IDY) .OR. (IH .NE. IHREND)) THEN
              WRITE (MSGS,*)'Error in Emissions file data date sequence'
              WRITE (MSGS,464) ISYR, JITS,    IH
              WRITE (MSGS,465) IEYR,  JSDOW,  IHREND
              WRITE (ISCN,*)'Error in Emissions file data date sequence'
              WRITE (ISCN,464) ISYR, JITS,    IH
              WRITE (ISCN,465) IEYR,  JSDOW,  IHREND
               STOP
            END IF
          END IF

C        Processing data for month #....

        DO 90 I = 1, NL
            NOLNK(I) = 1
          IF (IQ(I) .EQ. 2) THEN
            READ(IT2,*)  COD(I),CAVG(I),RAVG(I),YFAC(I),IV(I),IDLFAC(I)
     +                  ,SFR(I),ST(I),AT(I)
              IF (IV(I)  .LE. 0) NOLNK(I) = 0
           ELSE
            READ(IT2,*) COD(I), VPHL(I), EFL(I)
              IF (VPHL(I) .LE. 0.0) NOLNK(I) = 0
          END IF
   90 CONTINUE

C
C      --------------------------
C       READ METEOROLOGICAL DATA
C      --------------------------
C
C            U = WIND SPEED (M/S)
C          BRG = WIND FLOW (Direction + 180 Degrees)
C         CLAS = STABILITY CLASS (A-F)
C         MIXH = RURAL OR URBAN MIXING HEIGHT (M)
C          AMB = AMBIENT CONCENTRATION (PPM)
C        -----------------------------------
C
 9002   READ(IMET,51)  MYR, MMN, MDY, MHR, BRG, U, TMP, CLAS,
     +                  XRUR, XURB
          IF (MYR .LT. 0 .OR. MYR .GT. 99) MFLG(1) = 1
          IF (MMN .LT. 1 .OR. MMN .GT. 12) MFLG(2) = 1
          IF (MDY .LT. 1 .OR. MDY .GT. 31) MFLG(3) = 1
          IF (MHR .LT. 1 .OR. MHR .GT. 24) MFLG(4) = 1
          IF (BRG .LT. 0.0 .OR. BRG .GT. 360.0) MFLG(5) = 1
          IF (U .LT. 0.0 .OR. U .GT. 100.0) MFLG(6) = 1
          IF (TMP .LT. 230.0 .OR. TMP .GT. 320.0) MFLG(7) = 1
          IF (CLAS .LT. 1 .OR. CLAS .GT. 7) MFLG(8) = 1
          IF (XRUR .LE. 0.01) MFLG(9) = 1
          IF (XURB .LE. 0.01) MFLG(10) = 1
          DO I = 1, 10
            IF (MFLG(I) .GT. 0) THEN
              VAL(1) = FLOAT(MYR)
              VAL(2) = FLOAT(MMN)
              VAL(3) = FLOAT(MDY)
              VAL(4) = FLOAT(MHR)
              VAL(5) = BRG
              VAL(6) = U
              VAL(7) = TMP
              VAL(8) = FLOAT(CLAS)
              VAL(9) = XRUR
              VAL(10) = XURB
              IF (MFLG1 .EQ. 0) THEN
                 IF (MFLG2.EQ.0) THEN
                    WRITE(IPO,*) ' '
                    WRITE(IPO,*) '*********************************',
     +                           '**************************'
                    WRITE(IPO,*) ' Please check the meteorological ',
     +                           'data listed in CAL3HR.ERR.'
                    WRITE(IPO,*) '*********************************',
     +                           '**************************'
                    WRITE(MSGS,*) ' '
                    WRITE(MSGS,*) 'Meteorological data that appears ',
     +                            'to be in error: '
                    WRITE(ISCN,*) ' '
                    WRITE(ISCN,*) 'Meteorological data that appears ',
     +                            'to be in error: '
                    MFLG2 = 1
                 END IF
                 WRITE(MSGS,*) ' '
                 WRITE(MSGS,51)  MYR, MMN, MDY, MHR, BRG, U, TMP, CLAS,
     +                          XRUR, XURB
                 WRITE(ISCN,*) ' '
                 WRITE(ISCN,51)  MYR, MMN, MDY, MHR, BRG, U, TMP, CLAS,
     +                          XRUR, XURB
                 MFLG1 = 1
              END IF
              WRITE(MSGS,52)  MVALN(I), VAL(I)
              WRITE(ISCN,52)  MVALN(I), VAL(I)
   52         FORMAT('   The ', A15,' value,', F8.1,
     +               ' appears to be out of range.')
              MFLG(I) = 0
            END IF
          END DO
          IF (MFLG1 .EQ. 1) THEN
                WRITE(MSGS,*) '   Please review the ',
     +         'data and adjust accordingly.'
                WRITE(ISCN,*) '   Please review the ',
     +         'data and adjust accordingly.'
          END IF
          MFLG1 = 0


          IF (CLAS .GT. 6) CLAS = 6

          IF (IRU .EQ. 1 .AND. CLAS .GT. 4) CLAS = 4
          IF (IRU .EQ. 0) THEN
             MIXH = XRUR
            ELSE
             MIXH = XURB
          END IF
          IF (MIXH .EQ. 0.00) MIXH = MAX(XRUR,XURB)
          IF (MIXH .EQ. 0.00) MIXH = 10.00

          CALL JULIAN(MYR, MMN, MDY, MITS)
           IF (IMFLG .EQ. 0) THEN
             IF ((MITS .EQ. IJ) .AND. (IH .EQ. MHR)) THEN
               IMFLG = 1
              ELSE
               GO TO 9002
             END IF
            ELSE
             IF ((MITS .NE. IJ) .OR. (IH .NE. MHR)) THEN
               WRITE (MSGS,*) 'Error in Met file data date sequence'
               WRITE (MSGS,464)  ISYR, IJ,   IH
               WRITE (MSGS,465)  MYR,  MITS, MHR
               WRITE (ISCN,*) 'Error in Met file data date sequence'
               WRITE (ISCN,464)  ISYR, IJ,   IH
               WRITE (ISCN,465)  MYR,  MITS, MHR
                STOP
             END IF
           END IF

C
C        Calm wind processing section
C
            ISIG = 1
         IF (U .LT. 1.0) THEN
            U = 1.0
            IRCLM = 0
            ICLM = 1
            IB = 0
            IP = IP + 1
           ELSE
            IRCLM = 1
            ICLM = 0
            IB = 1
             IF (IP .GE. ISIG .AND. ISCTM .EQ. 0) THEN
               NIP(IP) = NIP(IP) + 1
               LH1 = IH - 1
               JDAY1 = IJ
               IF (LH1 .LT. 1) THEN
                 LH1 = 24
                 JDAY1 = IJ - 1
               END IF
               NIPJ(IP,NIP(IP)) = JDAY1
               NIPH(IP,NIP(IP)) = LH1
             END IF
            IP = 0
         END IF
         IF (IP .EQ. 24) IP = 0

       IF (ISKP .EQ. 0) THEN
        IF (ICLM .EQ. 0 ) THEN
C
C       ------------------------------------------
C       SKIPS QUEUE CALCULATION IF FREE FLOW LINK.
C       ------------------------------------------
        DO 1053 I = 1, NL

C        ===================
C       | QUEUE CALCULATION |
C        ===================

        IF (IQ(I) .EQ. 2) THEN
          IF (NOLNK(I) .EQ. 1) THEN
            V(I)=IV(I)/NLANES(I)

C       ----------------------------------------------------------------
C       To start the queue calculations, assume time lost getting queue
C       motion is maximum (K1= 2.0 seconds).
C       ----------------------------------------------------------------
C       RC(I)  = RED TO CYCLE RATIO.
C       ---------------------------
        K1=2.0
C
        GAVG(I)=FLOAT(CAVG(I)-RAVG(I))
        RC(I)=FLOAT(RAVG(I))/FLOAT(CAVG(I))
C
C       ------------------------------------------------
C       CALCULATE INTERSECTION CAPACITY, X(I).
C
C       CONVERT MPH TO FEET PER SECOND.
C            SFR(I) IS IN VEHICLES PER HOUR
C            CMAX IS SECONDS
C           ((3600 IS SECONDS PER HOUR))
C           ((2.0 IS SECONDS OF HEADWAY PER VEHICLE.))
C       ------------------------------------------------
C
CMOD        X(I)=SFR(I)/FLOAT(CAVG(I))
        X(I)=real(SFR(I))/FLOAT(CAVG(I))
C
C       ----------------------------------------------------
C       ZFAC(I)=TIME (sec) AVAILABLE MINUS START DELAY MINUS
C       TIME FOR VEHICLE TO CLEAR INTERSECTION (YFAC).
C       ----------------------------------------------------
C
        ZFAC(I)=GAVG(I)-K1-YFAC(I)
C
C       ------------------------------
C       IMU(I)=INTERSECTIONAL CAPACITY
C       (VEH/HOUR)/SEC*SEC
C       ------------------------------
CMOD        IMU(I)=X(I)*ZFAC(I)
        IMU(I)=X(I)*ZFAC(I)
C
C       -------------------------------------
C       CALCULATION OF DEMAND-CAPACITY RATIO.
C       -------------------------------------
CMOD        DC(I)=V(I)/FLOAT(IMU(I))
        DC(I)=V(I)/IMU(I)
C
C       -----------------------------------------------------
C       CALCULATE THE PAF LOOKUP LINE BASED ON THE V/C RATIO.
C       -----------------------------------------------------
C
        IF     (DC(I) .LE. 0.6)  THEN
          IDC = 1
        ELSE IF (DC(I) .LE. 0.8) THEN
          IDC = 2
        ELSE
          IDC = 3
        ENDIF
C
C        --------------------------------------------------------
C       | DELAY CALCULATIONS FOR QAVG4 ACCORDING TO 1985 HIGHWAY |
C       | CAPACITY MANUAL FORMULA.                               |
C        --------------------------------------------------------
C
CMOD        A1=0.38*CAVG(I)
        A1=0.38*real(CAVG(I))
        A2=GAVG(I)/FLOAT(CAVG(I))
CMOD        A3=(1-A2)**2
        A3=(1.-A2)**2
        PECK1=AMIN1(DC(I),1.0)
CMOD        A4=1-A2*PECK1
        A4=1.-A2*PECK1
        A5=A1*(A3/A4)
CMOD        A6=(PECK1-1)+SQRT((PECK1-1)**2+16*(PECK1/IMU(I)))
        A6=(PECK1-1.)+SQRT((PECK1-1.)**2+16.*(PECK1/IMU(I)))
CMOD        DJ(I)=(A5+(173*PECK1**2)*A6) * PAF(AT(I),IDC,ST(I))
        DJ(I)=(A5+(173.*PECK1**2)*A6) * PAF(AT(I),IDC,ST(I))
C
C       -------------------------------------------------
C       APPROACH DELAY:DDJ(I) = STOPPED DELAY DJ(I) * 1.3
C       -------------------------------------------------
C
        DDJ(I)=DJ(I)*1.3
C
C       -----------------------------------------------
C       CONVERT APPROACH VOLUME TO VEHICLES PER SECOND.
C       -----------------------------------------------
        VJ(I)=V(I)/3600.
C
C       ------------------------
C       COMPUTE TOTAL LOST TIME.
C       ------------------------
C
CMOD        QAVG4(I)=MAX( (VJ(I)*RAVG(I)/2)+(VJ(I)*DDJ(I)), VJ(I)*RAVG(I) )
        QAVG4(I)=MAX( (VJ(I)*real(RAVG(I))/2.)+(VJ(I)*DDJ(I)),
     &                                          VJ(I)*real(RAVG(I)) )
C
C       **************
C       UNDER-CAPACITY
C       **************
C
C       ------------------------------------------------------
C       COMPUTE NEW LINE LENGTH ASSUMING 6 METERS PER VEHICLE.
C       ------------------------------------------------------
C
CMOD        QLL=QAVG4(I)*6
        QLL=QAVG4(I)*6.0
C
        IF (DC(I).LE.1.0) GOTO 1111
C
CCCCCCCC**************************************************
C       Over-Capacity section changed
CCCCCCCC**************************************************
C
C       *************
C       OVER-CAPACITY
C       *************
CMOD        LL1=3*(V(I)-IMU(I))
        LL1=3.0*(V(I)-IMU(I))
        QAVG=LL1/6.0
        QAVG4(I)=QAVG4(I)+QAVG
CMOD        QLL=QLL+QAVG*6
        QLL=QLL+QAVG*6.0
C
C        --------------------------------------------
C       | COMPUTE NEW XL2 AND YL2 COORDINATES TO     |
C       | TELL PROGRAM END OF QUEUE.                 |
C        --------------------------------------------
C
C
 1111   IF (QLL.GT.QLT(I)) THEN
           LL(I) = QLT(I)
           TRUNC(I)='YES'
        ELSE
           LL(I) = QLL
           TRUNC(I)='NO '
        ENDIF
        IF(THETA(I).GT.90.AND.THETA(I).LE.180)
     +  XL2(I)=XL1(I)+LL(I)* TS180(I)
C
        IF(THETA(I).GT.90.AND.THETA(I).LE.180)
     +  YL2(I)=YL1(I)-LL(I)* TC180(I)
C
        IF(THETA(I).GT.180.AND.THETA(I).LE.270)
     +  XL2(I)=XL1(I)-LL(I)* TC270(I)
C
        IF(THETA(I).GT.180.AND.THETA(I).LE.270)
     +  YL2(I)=YL1(I)-LL(I)* TS270(I)
C
        IF(THETA(I).GT.270.AND.THETA(I).LE.360)
     +  XL2(I)=XL1(I)-LL(I)* TS360(I)
C
        IF(THETA(I).GT.270.AND.THETA(I).LE.360)
     +  YL2(I)=YL1(I)+LL(I)* TC360(I)
C
        IF(THETA(I).GT.0.AND.THETA(I).LE.90)
     +  XL2(I)=XL1(I)+LL(I)* TS0(I)
C
        IF(THETA(I).GT.0.AND.THETA(I).LE.90)
     +  YL2(I)=YL1(I)+LL(I)* TC0(I)
C
C        --------------------------------------
C       | COMPUTE TOTAL EMISSION RATE FOR LINK.|
C        --------------------------------------
        TER(I)=IDLFAC(I)*1000000./3600./6.*NLANES(I)*RC(I)
C
C       ----------------------------
C       SET ASSUMED EMISSION FACTOR.
C       ----------------------------
        EFL(I)=100.
C
C       --------------------------------------------------------------
C       COMPUTE NUMBER OF VEHICLES THAT MULTIPLIED BY THE E.F. OF 100.
C       WILL GIVE THE REQUIRED EMISSION RATE.
C       CALINE 3 USES Q=0.1726*VPHL*E.F., THEREFORE-
C       ---------------------------------------------------------------
        VPHL(I)=TER(I)/17.26
C
           END IF
         END IF
 1053  CONTINUE

C        END OF ICLM BLOCK
      END IF
C
C         *************************
C         * PRINT LINK VARIABLES  *
C         *************************
C
C           WRITE LINK VARIABLE DATA TO FILE
C
        IF (IPFLG .EQ. 1) THEN
          IPP = IPP + 1
          IF (JTIER .EQ. 1) IPFLG = 0
          IF (JTIER .EQ. 2) THEN
            IF (IPP .EQ. 168 .OR.
     +          (IJ .EQ. IEJDAY .AND. IH .EQ. 24)) IPFLG = 0
          END IF

            IF(IC15 .GE. 55) THEN
              IP15 = IP15 + 1
              WRITE (ILK, 200) CHAR(12), VERSN
              WRITE (ILK, 462) IMON, IDAY, IYRR, IP15
              WRITE (ILK, 468) IIHR, IMIN, ISEC
CL          WRITE (ILK,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
              WRITE (ILK, 210) JOB, RUN
              IC15 = 8
            END IF
              IF (JTIER .EQ. 2) WRITE(ILK, 251) DAYWK(JSDOW), IH, ISYR4
              WRITE (ILK, 252)
             IF (IOPT.EQ.1) THEN
               WRITE(ILK,266)
               WRITE(ILK,276)
              ELSE
               WRITE(ILK,261)
               WRITE(ILK,271)
             ENDIF
              WRITE(ILK,281)
              IC15 = IC15 + 7 + JTIER
C

          DO 1250 I=1,NL

C
         IF(IOPT.NE.1)THEN
C
            IF(IQ(I).EQ.2) THEN
            WRITE(ILK,294) COD(I),LNK(I),XL1(I),YL1(I),XL2(I),
     +                   YL2(I),LL(I),THETA(I),TYP(I),VPHL(I),
     +                   EFL(I),HL(I),WL(I),DC(I),QAVG4(I),TRUNC(I)
            ELSE
            WRITE(ILK,290) COD(I),LNK(I),XL1(I),YL1(I),XL2(I),
     +                   YL2(I),LL(I),THETA(I),TYP(I),VPHL(I),
     +                   EFL(I),HL(I),WL(I)
            ENDIF
C
         ELSE
C
               XW1=XL1(I)*PSCALE
               XW2=XL2(I)*PSCALE
               YW1=YL1(I)*PSCALE
               YW2=YL2(I)*PSCALE
               LLW=LL(I)*PSCALE
               HLW=HL(I)*PSCALE
               WLW=WL(I)*PSCALE
            IF(IQ(I).EQ.2)THEN
            WRITE(ILK,294) COD(I),LNK(I),XW1,YW1,XW2,
     +                   YW2,LLW,THETA(I),TYP(I),VPHL(I),
     +                   EFL(I),HLW,WLW,DC(I),QAVG4(I), TRUNC(I)
C
            ELSE
C
            WRITE(ILK,290) COD(I),LNK(I),XW1,YW1,XW2,
     +                   YW2,LLW,THETA(I),TYP(I),VPHL(I),
     +                   EFL(I),HLW,WLW
            ENDIF
C
         ENDIF
            IC15=IC15+1
C
            IF(IC15.GE.54 .AND. (NL - I) .GT. 3) THEN
              IP15 = IP15 + 1
              WRITE (ILK, 200) CHAR(12), VERSN
              WRITE (ILK, 462) IMON, IDAY, IYRR, IP15
              WRITE (ILK, 468) IIHR, IMIN, ISEC
CL          WRITE (ILK,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
              WRITE (ILK, 210) JOB, RUN
              IF (JTIER .EQ. 2) WRITE(ILK, 251) DAYWK(JSDOW), IH, ISYR4
              WRITE (ILK, 252)
             IF(IOPT.EQ.1)THEN
              WRITE(ILK,266)
              WRITE(ILK,276)
             ELSE
              WRITE(ILK,261)
              WRITE(ILK,271)
             ENDIF
              WRITE(ILK,281)
              IC15=9
            ENDIF

 1250   CONTINUE
C
C         ******************************************
C         * PRINT ADDITIONAL QUEUE LINK PARAMETERS *
C         ******************************************
C
        IF (MODET .EQ. 0) THEN
          IC15 = IC15 + 7
          IF(IC15 .LT. 57) THEN
            WRITE (ILK, *) ' '
            WRITE (ILK, 421)
            WRITE (ILK, 422)
            WRITE (ILK, 423)
            WRITE (ILK, 424)
            WRITE (ILK, 425)
          ENDIF

          DO 1154 I=1,NL
          IF(IC15 .GT. 54 .AND. (NL-I) .GT. 3) THEN                                 V2EF
            IP15 = IP15 + 1
            IC15 = 14
            WRITE (ILK, 200) CHAR(12), VERSN
            WRITE (ILK, 462) IMON, IDAY, IYRR, IP15
            WRITE (ILK, 468) IIHR, IMIN, ISEC
CL          WRITE (ILK,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
            WRITE (ILK, 210) JOB, RUN
            WRITE (ILK, *) ' '
            WRITE (ILK, 421)
            WRITE (ILK, 422)
            WRITE (ILK, 423)
            WRITE (ILK, 424)
            WRITE (ILK, 425)
          END IF
C
          IF(IQ(I).EQ.2)THEN
            WRITE(ILK,426)COD(I),LNK(I),CAVG(I),RAVG(I),YFAC(I),
     +                  IV(I),SFR(I),IDLFAC(I),ST(I),AT(I)
            IC15=IC15+1
          END IF
 1154    CONTINUE
C       END OF MODET BLOCK
        END IF
C     END OF   IPFLG BLOCK
      END IF
C
C
C       *******************
C       WIND DIRECTION LOOP
C       *******************
C
C         ---------------------


C         WIND ANGLE FOR OUTPUT
C         ---------------------
          BRG1=BRG + 180.
          BRG = BRG
          IF (BRG1.GE.360.) BRG1=BRG1-360.
C
C         --------------------------------
C         CONVERSION TO VECTOR ORIENTATION
C         --------------------------------
C         VIRTUAL DISPLACEMENT VECTORS
C         --------------------------------
          XVEC=COS(RAD*(450.-BRG))
          YVEC=SIN(RAD*(450.-BRG))
C
C         ----------------------------------------------------
C         CORRECTIONS FOR AVERAGING TIME AND SURFACE ROUGHNESS
C         ----------------------------------------------------
C
          SZ10 = SZC10(CLAS)
C
          PY1 = PYC1(CLAS)
          PY2 = PYC2(CLAS)
C
C         -------------------------
C         ZERO CONCENTRATION MATRIX
C         -------------------------
          DO 720 I=1,NL
            DO 720 J=1,NR
              C(I,J)=0.
  720     CONTINUE

        IF (ICLM .EQ. 0) THEN
C
C          *********
C          LINK LOOP
C          *********
C
          DO 8000 IL=1,NL
           IF (NOLNK(IL) .EQ. 1) THEN
            VPH=VPHL(IL)
            EF=EFL(IL)
            IF (TYP(IL).EQ.'DP'.OR.TYP(IL).EQ.'FL') GOTO 870
            H=HL(IL)
            GOTO 880
  870       H=0.
  880       W=WL(IL)
C
C           ------------
C           LINK ROUTINE
C           ------------
            W2=W/2.
C
C           *** LINEAL SOURCE STRENGTH PARALLEL TO HIGHWAY
C               IN MICRO-GRAMS/(METER*SEC)
            Q1=0.1726*VPH*EF

C           *** WIND ANGLE WITH RESPECT TO LINK
            PHI=ABS(BRG-THETA(IL))
C
C           *** SET ELEMENT GROWTH BASE
            IF (PHI.LE.90.) GOTO 7600
            IF (PHI.GE.270.) GOTO 5000
            PHI=ABS(PHI-180.)
            GOTO 7600
 5000       PHI=ABS(PHI-360.)
 7600       IF (PHI.LT.20.) GOTO 7630
            IF (PHI.LT.50.) GOTO 7620
            IF (PHI.LT.70.) GOTO 7610
            BASE=4.
            GOTO 7650
 7610       BASE=2.
            GOTO 7650
 7620       BASE=1.5
            GOTO 7650
 7630       BASE=1.1
C
C           *** CONVERSION OF PHI FROM DEGREES TO RADIANS
 7650       PHI=RAD*(PHI)
C
            IF (PHI .GT. 1.5706)  PHI=1.5706
            IF (PHI .LT. 0.00017) PHI=0.00017
C
C           -----------------
C           DEPRESSED SECTION
C           -----------------
            IF (HL(IL).LT.-1.5) GOTO 7700
            DSTR=1.
            HDS=1.
            GOTO 7800
 7700       HDS=HL(IL)
            DSTR=0.72*ABS(HDS)**0.83
C
C           *** RESIDENCE TIME
 7800       TR=DSTR*W2/U
C
C           *** SIGMA Z POWER CURVE
            SGZ1=ALOG((1.8+0.11*TR)*(ATIM/30.)**0.2)
C
C           *** ALOG(SIGMA Z) AT W2
            PZ2=(SZ10-SGZ1)/(DREF-ALOG(W2))
            PZ1=EXP((SZ10+SGZ1-PZ2*(DREF+ALOG(W2)))/2.)
C
C           *************
C           RECEPTOR LOOP
C           *************

           DO 6000 IR=1,NR
C
C            *** OFFSET LENGTH
             A=(XR(IR)-XL1(IL))**2+(YR(IR)-YL1(IL))**2
             B=(XR(IR)-XL2(IL))**2+(YR(IR)-YL2(IL))**2
             L=(B-A-LL(IL)**2)/(2.*LL(IL))
C
C            *** RECEPTOR DISTANCE
             IF (A.GT.L**2) D=DSQRT(A-L**2)
             IF (A.LE.L**2) D=0.
C
C            *** UPWIND AND DOWNWIND LENGTH
             UWL=LL(IL)+L
             DWL=L
C
             IF(D.EQ.0.D0) DVIR=1.D0
             IF(D.NE.0.D0) DVIR=D
             XPRI=XR(IR)+DVIR*XVEC
             YPRI=YR(IR)+DVIR*YVEC
             APRI=(XPRI-XL1(IL))**2+(YPRI-YL1(IL))**2
             BPRI=(XPRI-XL2(IL))**2+(YPRI-YL2(IL))**2
             LPRI=(BPRI-APRI-LL(IL)**2)/(2.*LL(IL))
             IF (APRI.GT.LPRI**2) DPRI=DSQRT(APRI-LPRI**2)
             IF (APRI.LE.LPRI**2) DPRI=0.
             IF (DPRI.LT.D) D=-D
             IF (LPRI-L) 5725,5735,5735
 5725        TEMP=UWL
             UWL=-DWL
             DWL=-TEMP
 5735        IF (TYP(IL).EQ.'AG' .OR.TYP(IL).EQ.'BR') GOTO 5750
C
             D1=W2+2.*ABS(HL(IL))
             D2=W2
C
C            *** SINGLE PRECISION TO DOUBLE PRECISION FOR LOGICAL 'IF'
             IF (DABS(D).GE.D1) GOTO 5750
C
C            *** 2:1 SLOPE ASSUMED
             IF (DABS(D).LE.D2) Z=ZR(IR)-HL(IL)
             IF (DABS(D).GT.D2)
     +       Z=ZR(IR)-HL(IL)*(1.-(DABS(D)-W2)/(2.*ABS(HL(IL))))
             GOTO 3050
 5750        Z=ZR(IR)
C
C            ---------------
C            CALINE3 ROUTINE
C            ---------------
C            DETERMINES DIRECTION ALONG LINK
C            +1 --> UPWIND ELEMENTS;  -1 --> DOWNWIND ELEMENTS
C            -------------------------------------------------
 3050        SGN=1.
C
C            * ELEMENT NUMBER, STEP FACTOR AND LOOP END INITIALIZATION
             NE=0.
             STP=1.
             FINI=1.
C
            IF (SGN.EQ.1. .AND.
     +          UWL.LE.0. .AND.
     +          DWL.LT.0.) SGN=-1.
 3080       IF (SGN.EQ.-1. .AND.
     +          UWL.GT.0. .AND.
     +          DWL.GE.0.) GOTO 6000
C
C           ------------
C           ELEMENT LOOP
C           ------------
C
C           *** INITIALIZATION OF ELEMENT LIMITS
            ED1=0.
            ED2=SGN*W
C
 3110       IF (SGN.EQ.-1.) GOTO 3160
            IF (ED1.LE.DWL .AND. ED2.LE.DWL) GOTO 3770
            IF (ED1.GT.DWL .AND. ED2.LT.UWL) GOTO 3250
            IF (ED1.LE.DWL) ED1=DWL
            IF (ED2.LT.UWL) GOTO 3250
            ED2=UWL
            SGN=-1.
            NE=-1.
            GOTO 3250
 3160       IF (ED1.GE.UWL .AND. ED2.GE.UWL) GOTO 3770
            IF (ED1.LT.UWL .AND. ED2.GT.DWL) GOTO 3250
            IF (ED1.GE.UWL) ED1=UWL
            IF (ED2.GT.DWL) GOTO 3250
            ED2=DWL
            FINI=0.
C
C           *** ELEMENT HALF-DISTANCE
 3250       EL2=ABS(ED2-ED1)/2.
C
C           *** ELEMENT CENTERLINE DISTANCE
            ECLD=(ED1+ED2)/2.
C
C           *** EQUIVALENT LINE HALF-LENGTH
            ELL2=W2/COS(PHI)+(EL2-W2*TAN(PHI))*SIN(PHI)
C
C           *** CENTRAL SUB-ELEMENT HALF-LENGTH
            IF (PHI.GE.ATAN(W2/EL2)) CSL2=W2/SIN(PHI)
            IF (PHI.LT.ATAN(W2/EL2)) CSL2=EL2/COS(PHI)
C
C           *** CENTRAL SUB-ELEMENT HALF-WIDTH
            EM2=ABS((EL2-W2/TAN(PHI))*SIN(PHI))
C
C           *** PERIPHERAL SUB-ELEMENT WIDTH
            EN2=(ELL2-EM2)/2.
C
C           ----------------------
C           RECEPTOR DISTANCE LOOP
C           ----------------------
C
C           *** CENTRAL SUB-ELEMENT LINEAL SOURCE STRENGTH
            QE=Q1*CSL2/W2
C
C           *** ELEMENT FETCH
            FET=(ECLD+D*TAN(PHI))*COS(PHI)
C
C           *** Y DISTANCE FROM ELEMENT CENTER TO RECEPTOR
            HYP=ECLD**2+D**2
            SIDE=FET**2
            IF (SIDE.GT.HYP) YE=0.
            IF (SIDE.LE.HYP) YE=DSQRT(HYP-SIDE)
C
            IF (FET.LE.-CSL2) GOTO 3830
C
C           *** ELEMENT DOES NOT CONTRIBUTE
            IF (FET.GE.CSL2) GOTO 3320
C
C           *** RECEPTOR WITHIN ELEMENT
C               DETERMINE SIGMA Y AND SIGMA Z
            QE=QE*(FET+CSL2)/(2.*CSL2)
            FET=(CSL2+FET)/2.
 3320       SGZ=PZ1*FET**PZ2
            KZ=SGZ**2*U/(2.*FET)
C
C           *** VERTICAL DIFFUSIVITY ESTIMATE
            SGY=PY1*FET**PY2
C
C           *** SOURCE STRENGTH - WIND SPEED FACTOR
            FAC1=0.399/(SGZ*U)
C
C           ----------------------------------
C           ADJUSTMENT FOR ELEMENT END EFFECT
C           (POLYNOMIAL APPROXIMATION)
C           ----------------------------------
            Y(1)=YE+ELL2
            Y(2)=Y(1)-EN2
            Y(3)=Y(2)-EN2
            Y(4)=Y(3)-2*EM2
            Y(5)=Y(4)-EN2
            Y(6)=Y(5)-EN2
C
C           --------------------------------
C           SUB-ELEMENT SOURCE STRENGTH LOOP
C           --------------------------------
C
            DO 3480 I=1,6
              LIMRAT=ABS(Y(I)/SGY)
              T=1./(1.+0.23164*LIMRAT)
              ARG=LIMRAT**2/(-2.)
              IF (LIMRAT.GT.5.) INTG(I)=0.
              IF (LIMRAT.LE.5.) INTG(I)=0.3989*EXP(ARG)*(0.3194*T-0.3566
     +                              *T**2+1.7815*T**3-1.8213*T**4+1.3303
     +                              *T**5)
 3480       CONTINUE
C
            FAC2=0.
            DO 3530 I=1,5
              IF ((SIGN(1.,Y(I))).EQ.(SIGN(1.,Y(I+1))))
     +        PD=DABS(INTG(I+1)-INTG(I))
              IF ((SIGN(1.,Y(I))).NE.(SIGN(1.,Y(I+1))))
     +        PD=1.-INTG(I)-INTG(I+1)
C
C             *** NORMAL PROBABILITY DENSITY FUNCTION
              FAC2=FAC2+PD*QE*WT(I)
 3530       CONTINUE
C
            FACT=FAC1*FAC2
C
C           -----------------
C           DEPRESSED SECTION
C           -----------------
            IF (HDS.LT.-1.5 .AND.DABS(D).LT.(W2-3.*HDS)) GOTO 3560
            GOTO 3580
 3560       IF (DABS(D).LE.W2) FACT=FACT*DSTR
            IF (DABS(D).GT.W2) FACT=FACT*(DSTR-(DSTR-1.)*(DABS(D)-W2)/
     +                              (-3.*HDS))
C
C           *** ADJUST FOR DEPRESSED SECTION WIND SPEED
C
C           *** DEPOSITION CORRECTION
 3580       FAC3=0.
            IF (V1.EQ.0.) GOTO 3670
            ARG=V1*SGZ/(KZ*SQRT(2.))+(Z+H)/(SGZ*SQRT(2.))
            IF (ARG.GT.5.) GOTO 3770
            T=1./(1.+0.47047*ARG)
            EFRC=(.3480242*T-.0958798*T**2+.7478556*T**3)*
     +            EXP(-1.*ARG**2)
            FAC3=(SQRT(2.*PI)*V1*SGZ*EXP(V1*(Z+H)/KZ+.5*(V1*SGZ/KZ)**2)
     +            *EFRC)/KZ
            IF (FAC3.GT.2.) FAC3=2.
C
C
C           *** SETTLING CORRECTION
 3670       IF (VS.EQ.0.) GOTO 3710
            FAC4=EXP(-VS*(Z-H)/(2.*KZ)-(VS*SGZ/KZ)**2/8.)
            FACT=FACT*FAC4
C
C           *** INCREMENTAL CONCENTRATION
C               THE VERTICAL DISPERSION ALGORITHM FROM ISCST2 WAS
C               INCORPORATED INTO THE FOLLOWING SECTION.
C                 Z - RECEPTOR HEIGHT
C                 H - SOURCE HEIGHT

 3710       FAC5=0.

            SGZ2 = -0.5/(SGZ*SGZ)
            HEM = Z - H
            HEP = Z + H
            IF (CLAS .GT. 4 .OR. MIXH .GE. 10000) THEN
              ARG1 = SGZ2 * HEM * HEM
              ARG2 = SGZ2 * HEP * HEP
               IF ( ARG1. GT. -50.) FAC5 = FAC5 + EXP(ARG1)
               IF ( ARG2. GT. -50.) FAC5 = FAC5 + EXP(ARG2)
             ELSE IF ((SGZ/MIXH) .GE. 1.6) THEN
              FAC5 = 0.399 * SGZ / MIXH
             ELSE
              ARG1 = SGZ2 * HEM * HEM
              ARG2 = SGZ2 * HEP * HEP
               IF ( ARG1. GT. -50.) FAC5 = FAC5 + EXP(ARG1)
               IF ( ARG2. GT. -50.) FAC5 = FAC5 + EXP(ARG2)
              EXLS=0.
              DO 3720 II = 1, 100
                T = 0.
                CNT = 2 * II * MIXH
                ARG3 = SGZ2 * (HEM-CNT)**2
                ARG4 = SGZ2 * (HEP-CNT)**2
                ARG5 = SGZ2 * (HEM+CNT)**2
                ARG6 = SGZ2 * (HEP+CNT)**2
                 IF (ARG3.GT.-50.) T = T + EXP(ARG3)
                 IF (ARG4.GT.-50.) T = T + EXP(ARG4)
                 IF (ARG5.GT.-50.) T = T + EXP(ARG5)
                 IF (ARG6.GT.-50.) T = T + EXP(ARG6)
                EXLS = EXLS + T
                 IF (ABS(T) .LE. 5.0E-9) THEN
                   GO TO 3760
                 END IF
 3720         CONTINUE
 3760        CONTINUE
             FAC5 = FAC5 + EXLS
            END IF
C
C           --------------------------------
C           BYPASS MIXING HEIGHT CALCULATION
C           --------------------------------
C
C           *** INCREMENTAL CONCENTRATION FROM ELEMENT
            INC=FACT*(FAC5-FAC3)
C
C           *** SUMMATION OF CONCENTRATIONS
            C(IL,IR)=C(IL,IR)+INC
C
 3770       IF (FINI.EQ.0.) GOTO 6000
            NE=NE+1.
C
C           *** STEP FACTOR
            STP=BASE**NE
C
C           *** INCREMENT TO NEXT ELEMENT
            IF (NE.EQ.0.) GOTO 3080
            ED1=ED2
            ED2=ED2+SGN*STP*W
C
            GOTO 3110
 3830       IF (SGN.EQ.1.) GOTO 3770
C
C
 6000       CONTINUE
           END IF
 8000     CONTINUE

C
C         -------------------------------------------------------
C         CONVERT CONCENTRATION OF CO FROM MICROGRAMS/M**3 TO PPM
C         -------------------------------------------------------
          DO 1020 I=1,NL
            DO 1010 J=1,NR
              C(I,J)=C(I,J)*FPPM
 1010       CONTINUE
 1020     CONTINUE

C       END OF ICLM BLOCK
        END IF
C
C         *****************
C         * MODEL RESULTS *
C         *****************
C
C         ----------------------------------------------------------
C         CALCULATION OF THE TOTAL CO CONCENTRATION AT EACH RECEPTOR
C         PRODUCED BY ALL THE LINKS, FOR EACH WIND ANGLE.
C         ----------------------------------------------------------

          J2 = 2
            IF (MODET .EQ. 1) J2 = 1

          DO 1152 I=1,NR

            CSUM = 0.0

            DO 1252 J=1,NL
CMOD           Remove code that results in rounding to nearest tenth.
CMODCMOD              C(J,I) = 10. * C(J,I) + .5
CMOD              K = C(J,I) * IRCLM
              REALK = C(J,I) * real(IRCLM)
CMOD              C(J,I) = K / 10.
CMOD              C(J,I) = real(K) / 10.
CMODCMOD              C(J,I) = realk / 10.
              C(J,I) = realk
              CSUM = CSUM + C(J,I)

C               Sum annual or period link contributions
                IF (ISCTM .EQ. 0 .AND. MODET .EQ. 1) THEN
                  CONTL(1,I,2,J) = CONTL(1,I,2,J) + C(J,I)
                END IF

                IF (ISCTM .EQ. 1) THEN
                  DO 1253 II = 1,2
                  DO 1253 JJ = 1,J2
                    IF (LH .GE. JHS(II,I,JJ) .AND.
     +                  LH .LE. JHXX(II,I,JJ)) THEN
                      CONTL(II,I,JJ,J) = CONTL(II,I,JJ,J)
     +                                   + C(J,I)
                    END IF
 1253             CONTINUE
                END IF
 1252       CONTINUE

CMOD            CON(I) = CSUM * IRCLM
CMOD              A = CON(I) + FAMB * IB * AMB
            CON(I) = CSUM * real(IRCLM)
              A = CON(I) + real(FAMB) * real(IB) * AMB
               IF (A .GT. CRMAXB(I)) THEN
                 CRMAXB(I) = A
CMOD                 BKGMAX(I) = FAMB * IB * AMB
                 BKGMAX(I) = real(FAMB) * real(IB) * AMB
                 CRMAX(I)  = CON(I)
                 ANGMAX(I) = BRG1
                 JD(I)     = IJ
                 IHE(I)    = IH
               END IF
               IF (A .GT. CMAX) THEN
                 CMAX = A
                 IRMAX = I
               END IF
C
C             *** Keep in memory only the concentrations (for
C                 each receptor per link) for the wind angle that
C                 produces the maximum sums.
C
 1152     CONTINUE

      IF (ISCTM .EQ. 0) THEN
C
C       DETERMINE WHICH AVERAGES ARE TO BE COMPUTED
C
          IF (MODET .EQ. 0) THEN
C
C             *****  CALCULATIONS FOR CO AVERAGES  *****
C
              J8 = J8 + 1
                IF (J8.GT.24) J8 = 1
              CUMT(1) = CUMT(1) + IB - CNTM(J8)
CMOD              CUMB(1) = CUMB(1) + FAMB * IB * AMB - BNTH(J8)
              CUMB(1) = CUMB(1) + real(FAMB) * real(IB) * AMB - BNTH(J8)
              CNTM(IH) = IB
              BNTH(IH) = real(FAMB) * real(IB) * AMB

C                 STORE HOURLY AVERAGE WITH/WITHOUT BACKGROUND.
C                 ADD CURRENT HOUR TO SUM AND SUBTRACT THE OLDEST HOUR
C                 SAVE CURRENT CONCENTRATIONS

              DO 125 I=1,NR
CMOD                A  = CON(I) + FAMB * IB * AMB
                A  = CON(I) + real(FAMB) * real(IB) * AMB
                CUM(I,1) = CUM(I,1) + A - CNHR(I,J8) - BNTH(J8)
                CUM(I,2) = A
                CNHR(I,IH) = CON(I)
125           CONTINUE
              CALL RNRK(RNMX8,JHR8,9,1,BK)
              IF (IB .EQ. 1) THEN
                CUMT(2) = IB
CMOD                CUMB(2) = FAMB * AMB
                CUMB(2) = real(FAMB) * AMB
                CALL RNK5(2)
              END IF

          ELSE
C
C            *****  CALCULATIONS FOR PM AVERAGES  ***
C
              CUMT(3) = CUMT(3) + IB
              CUMT(4) = CUMT(4) + IB
CMOD              CUMB(3) = CUMB(3) + FAMB * AMB * IB
CMOD              CUMB(4) = CUMB(4) + FAMB * AMB * IB
              CUMB(3) = CUMB(3) + real(FAMB) * AMB * real(IB)
              CUMB(4) = CUMB(4) + real(FAMB) * AMB * real(IB)
C               ADD HOURLY CONC FOR 24-HOUR END-TO-END AVERAGES.
C               ADD HOURLY CONC FOR LENGTH-OF-RECORD AVERAGES.
            DO 135 I=1,NR
CMOD              A= CON(I) + FAMB * AMB * IB
              A= CON(I) + real(FAMB) * AMB * real(IB)
              CUM(I,L3) = CUM(I,L3) + A
              CUM(I,L4) = CUM(I,L4) + A
135         CONTINUE
C
C              END OF LOOP OVER RECEPTORS. NEXT UPDATE TABLES.
C
            IF (IH .EQ.24) THEN
              CALL RNK5(3)
              CUMT(3) = 0
              CUMB(3) = 0.0
              DO 145 I=1,NR
                CUM(I,L3) = 0.
145           CONTINUE
C             Annual data is processed through RNK5 below.
            END IF
          END IF
        END IF

C      ISKP END IF
      END IF

 9000 CONTINUE

        MN = 1
        WRITE(MSGS,*) ' '
        WRITE(MSGS,*) 'END OF PROCESSING LOOP'
        WRITE(ISCN,*) ' '
        WRITE(ISCN,*) 'END OF PROCESSING LOOP'

C     *******************************
C     * END OF MAIN PROCESSING LOOP *
C     *******************************

C     Calculate and save maximum hourly concentrations by receptor

      IF (ISCTM .EQ. 0) THEN

        IF (ILIN .GT. 37) THEN
          IPAG=IPAG+1
          ILIN = 6
          WRITE (IPO, 200) CHAR(12), VERSN
          WRITE (IPO, 462) IMON, IDAY, IYRR, IPAG
          WRITE (IPO, 463) IIHR, IMIN, ISEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
          WRITE (IPO, 210) JOB,RUN
        END IF

          ILIN = ILIN + 9
          WRITE(IPO,8399)
          WRITE(IPO,8400)
          WRITE(IPO,8500)
          IF (MODET .EQ. 0) THEN
            WRITE(IPO,8509)
           ELSE
            WRITE(IPO,8510)
          END IF

            ISN = 1
            IEN = 10
            IF (MOD(NR,10) .EQ. 0) THEN
              INR = NR /10
             ELSE
              INR = (NR /10) + 1
            END IF
          DO 9010 J = 1, INR
            IF (J .EQ. INR) IEN = NR
              WRITE(IPO,8511) (REC(I),   I=ISN,IEN)
              WRITE(IPO,8512) (X1(I),    I=ISN,IEN)
              WRITE(IPO,9040) (CRMAXB(I), I=ISN,IEN)
              WRITE(IPO,9041) (BKGMAX(I), I=ISN,IEN)
              WRITE(IPO,8512) (X1(I),    I=ISN,IEN)
              WRITE(IPO,9042) (CRMAX(I), I=ISN,IEN)
              WRITE(IPO,9043) (ANGMAX(I),I=ISN,IEN)
              WRITE(IPO,9044) (JD(I), I=ISN,IEN)
              WRITE(IPO,9045) (IHE(I),I=ISN,IEN)
              WRITE(IPO,'(1X)')
              ISN = IEN + 1
              IEN = IEN + 10
              ILIN = ILIN + 10

                IF (ILIN .GT. 45) THEN
                  IPAG=IPAG+1
                  ILIN = 8
                  WRITE (IPO, 200) CHAR(12), VERSN
                  WRITE (IPO, 462) IMON, IDAY, IYRR, IPAG
                  WRITE (IPO, 463) IIHR, IMIN, ISEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
                  WRITE (IPO, 210) JOB,RUN

                  IF (J .NE. INR) THEN
                    WRITE(IPO,8500)
                    IF (MODET .EQ. 0) THEN
                      WRITE(IPO,8509)
                     ELSE
                      WRITE(IPO,8510)
                    END IF
                  END IF
               END IF

 9010     CONTINUE

        IF (MODET .EQ. 0) THEN
          WRITE(IPO,9048) CMAX, REC(IRMAX)
         ELSE
          WRITE(IPO,9049) CMAX, REC(IRMAX)
        END IF
      END IF
CMOD   CALL GETTIM (IIHR, IMIN, ISEC, IX)
      CALL DATE_AND_TIME(CDATE,CTIME,CZONE,TODAY)
      IIHR = TODAY(5)
      IMIN = TODAY(6)
      ISEC = TODAY(7)

C   CALCULATES BEGINNING AND ENDING TIMES FOR OBTAINING LINK DATA FOR
C   EACH RECEPTOR CONCENTRATION.

C       SET UP FOR LOOP ON PM OR CO
        IF (MODET. EQ. 1) THEN
          LS = 3
          LE = 4
         ELSE
          LS = 1
          LE = 2
        END IF

        IF (ISCTM .EQ. 0) THEN
          IF (MODET .EQ. 1) CALL RNK5(4)
          ISCTM = 1
          REWIND IT2
          REWIND IMET
            READ(IMET,*)  IMFCID, IMFCYR, IMIRID, IMIRYR
            IMFLG = 0
            LIM = 9
            LIM1 = LIM - 1
          DO 605 LLL = LS, LE
            LA = LLL
             IF (LLL .EQ. 3) LA = 1
             IF (LLL .EQ. 4) LA = 2
            CUMT(LLL) = 0
            ITR = 2
              IF (LLL .EQ. 4) ITR = 1
            DO 610 IR = 1, ITR
              DO 621 M = 1, NR
                IF (LLL .EQ. 1 .AND. IR .EQ. 2) THEN
                  ISW = 0
                  DO 626 N=1,LIM
                    IF ((ISW.EQ.1).OR.(N.EQ.1)) GO TO 626
                    DO 627 J=1,N
                      IF (IABS(JHR8(N,M)-JHR8(J,M)) .LT. LIM1) GO TO 627
                        ISW = 1
                        NC(2,M) = NC(N,M)
                        NDV(2,M,1) = NDV(N,M,1)
                        HMAXA(1,M,1) = RNMX8(1,M)
                        HMAXA(2,M,1) = RNMX8(N,M)
                        BKG(1,M,1)  = BK(1,M)
                        BKG(2,M,1)  = BK(N,M)
                        IHR(1,M,1)  = JHR8(1,M)
                        IHR(2,M,1)  = JHR8(N,M)
                        JHXX(1,M,1) = JHR8(1,M)
                        JHXX(2,M,1) = JHR8(N,M)
  627               CONTINUE
  626             CONTINUE
                 ELSE
                  JHXX(IR,M,LA) = IHR(IR,M,LA)
                END IF
  621         CONTINUE
  610       CONTINUE
            DO 630 IR = 1,ITR
              DO 630 I = 1, NR
                JHS(IR,I,LA) = JHXX(IR,I,LA) - ITAVG(LLL)
  630       CONTINUE
  605     CONTINUE
        END IF

 9500 CONTINUE

C
C      -----------------------------------------------
C       Preparing output tables and storing them to file
C      -----------------------------------------------
C
      WRITE(MSGS, *)
      WRITE(MSGS, *) 'Preparing output tables and storing them to file.'
      WRITE(ISCN, *)
      WRITE(ISCN, *) 'Preparing output tables and storing them to file.'

CMOD   CALL GETDAT (IYRR, IMON, IDAY)
CMOD        CALL IDATE(TODAY)
        CALL DATE_AND_TIME(CDATE,CTIME,CZONE,TODAY)
        IYRR = TODAY(1)
        IMON = TODAY(2)
        IDAY = TODAY(3)
        IIHR = TODAY(5)
        IMIN = TODAY(6)
        ISEC = TODAY(7)

CMOD   CALL GETTIM (IIHR, IMIN, ISEC, IX)
        PLTFRM = '(3(1X,F13.5),3X,A5,3X,A5)'

        TITLE = ' '

C       Write Header Information
        WRITE(IPLT,9005) VERSN, TITLE, IMON,IDAY,MOD(IYRR,100)
        WRITE(IPLT,9007) IIHR,IMIN,ISEC
        IF ( MODET .eq. 0 )  WRITE(IPLT,9008) NR, PLTFRM
        IF ( MODET .eq. 1 )  WRITE(IPLT,9009) NR, PLTFRM
 9005 FORMAT('* CAL3QHCR ( ',A5,'): ',A68,3X,i2.2,'/',i2.2,'/',i2.2)
 9007 FORMAT('* ',                       T93,i2.2,':',i2.2,':',i2.2)
C9009 FORMAT('*',9X,'PLOT FILE OF  MAX 24-HR AVERAGE CONC VALUES',
 9008 FORMAT('*',9X,'PLOT FILE OF MAX 8HR RUNNING AVG CONC VALS (PPM)',
     &      /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
     &      /'*',9X,'FORMAT: ',A)
 9009 FORMAT('*',9X,'PLOT FILE OF MAX 24HR AVG CONC VALS (uG/M**3)',
     &      /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
     &      /'*',9X,'FORMAT: ',A)

        IF ( IOPT .NE. 1 ) THEN
          WRITE(IPLT,9018)
        ELSE
          WRITE(IPLT,9019)
        END IF
 9018 FORMAT(
     &  '*      X (M)         Y (M)    AVERAGE CONC    AVE     RANK  ',
     &/,'* ____________  ____________  ____________  ______  ________')
 9019 FORMAT(
     &  '*      X (FT)        Y (FT)   AVERAGE CONC    AVE     RANK  ',
     &/,'* ____________  ____________  ____________  ______  ________')

        DO 640 IPLTI=1,NR
          IF ( IOPT .NE. 1 ) THEN
            WRITE(IPLT,PLTFRM) XR(IPLTI)       , YR(IPLTI)       ,      &
     &                         HMAXA(1,IPLTI,1),                        &
     &                         '24-HR',   '1ST'
          ELSE
            WRITE(IPLT,PLTFRM) XR(IPLTI)*PSCALE, YR(IPLTI)*PSCALE,      &
     &                         HMAXA(1,IPLTI,1),                        &
     &                         '24-HR',   '1ST'
          END IF
 640    CONTINUE
        CLOSE(IPLT)

      IX = 0
      LYR = MOD(LYR,100)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C     PRINT OUTPUT SECTION TITLE AND NOTES
C
      IPAG = IPAG + 1
      ILIN = 23
      WRITE (IPO, 200) CHAR(12), VERSN
      WRITE (IPO, 462) LMON, LDAY, LYR, IPAG
      WRITE (IPO, 463) LHR, LMIN, LSEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
      WRITE (IPO, 210) JOB,RUN
      WRITE (IPO, 400)
      WRITE(IPO,405)
      WRITE(IPO,410)
      WRITE(IPO,415)
      IF (MODET .EQ. 0) WRITE(IPO,416)
      IF (MODET .EQ. 1) WRITE(IPO,417)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C        OUTPUT CONCENTRATION TABLES.
C        DETERMINE AVG CONC FOR LENGTH OF RECORD AND LABEL HIGHEST
C         AND HIGHEST OF THE SECOND HIGHEST VALUES WITH AN ASTERISK.

C       OUTPUT RUNNING AVERAGES

      IF (MODET .EQ. 0) THEN
        CALL OUTRA (ILIN)
      END IF

C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C        OUTPUT END-TO-END AVERAGES.
C        WRITE TABLE OF HIGH-SIX CONCENTRATIONS
C
      IF (MODET. EQ. 1) THEN
        LS = 3
        LE = 4
       ELSE
        LS = 2
        LE = 2
      END IF

        DO 710 LLL = LS, LE
          LA = LLL
            IF (LLL .EQ. 3) LA = 1
            IF (LLL .EQ. 4) LA = 2


C           FIND RECEPTOR WITH HIGHEST CONCENTRATION AND RECEPTOR WITH
C           SECOND HIGHEST CONC FOR EACH AVERAGING TIME AND LABEL EACH
C           WITH AN ASTERISK.

         IF (ILIN .GT. 55) THEN
           IPAG = IPAG + 1
           ILIN = 6
           WRITE (IPO, 200) CHAR(12), VERSN
           WRITE (IPO, 462) LMON, LDAY, LYR, IPAG
           WRITE (IPO, 463) LHR, LMIN, LSEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
           WRITE (IPO, 210) JOB,RUN
         END IF

         IF (ILIN .LT. 55) THEN
           ILIN = ILIN + 7
           IF (LLL .EQ. 2) WRITE (IPO, 794) NATM(LLL)
           IF (LLL .EQ. 3) WRITE (IPO, 795) NATM(LLL)
           IF (LLL .EQ. 2 .OR. LLL .EQ. 3) THEN
             WRITE(IPO, 792) EXIN
             WRITE(IPO, 796)
           END IF
           IF (LLL .EQ. 4) THEN
             JLONG = IEJDAY-ISJDAY + 1
             IF (JLONG .GE. 365) THEN
               WRITE (IPO,797)
              ELSE
               WRITE (IPO,798) JLONG
             END IF
             WRITE (IPO, 792) EXIN
             WRITE (IPO, 799)
           END IF
         END IF

         K11=1
         K2=1
         A=HMAXA(1,1,LA)
         B=HMAXA(2,1,LA)
         IF (NR .NE. 1) THEN
           DO 690 K=2,NR
             IF (HMAXA(1,K,LA) .GT. A) THEN
               A=HMAXA(1,K,LA)
               K11=K
             END IF
             IF (HMAXA(2,K,LA) .GT. B) THEN
               B=HMAXA(2,K,LA)
               K2=K
             END IF
690        CONTINUE
         END IF
         STAR(1,K11) = STR
         STAR(2,K2) = STR
C
C      PRINT TABLE OF HIGH-FIVE CONCENTRATIONS FOR THIS AVERAGING TIME
C
         DO 700 K=1,NR
CMOD           DO 695 IX=1,5
           DO 695 IX=1,6
             IY = IHR(IX,K,LA) - 1
             NDAY(IX) = IY/24 + 1
             NHRS(IX)  = IY - ((NDAY(IX)-1) * 24) + 1
695        CONTINUE

           ILIN = ILIN + 1
           IF (LLL .EQ. 2 .OR. LLL .EQ. 3) THEN
             WRITE (IPO,800) K,(HMAXA(J,K,LA),STAR(J,K),NDAY(J),
     1             NHRS(J),NEC(J,K,LA),J=1,2),
     2            (HMAXA(J,K,LA),NDAY(J),NHRS(J),NEC(J,K,LA),J=3,6)
CMOD     2            (HMAXA(J,K,LA),NDAY(J),NHRS(J),NEC(J,K,LA),J=3,5)
           END IF
           IF (LLL .EQ. 4) THEN
             WRITE (IPO,801) K, HMAXA(1,K,LA), STAR(1,K),NDAY(1),
     1             NHRS(1), NEC(1,K,LA)
           END IF

           IF (ILIN .GT. 55) THEN
             IPAG = IPAG + 1
             ILIN = 7
             WRITE (IPO, 200) CHAR(12), VERSN
             WRITE (IPO, 462) LMON, LDAY, LYR, IPAG
             WRITE (IPO, 463) LHR, LMIN, LSEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
             WRITE (IPO, 210) JOB,RUN

             IF (K .NE. NR) THEN
                ILIN = ILIN + 8
               IF (LLL .EQ. 2) WRITE (IPO,794) NATM(LLL)
               IF (LLL .EQ. 3) WRITE (IPO,795) NATM(LLL)
               IF (LLL .EQ. 2 .OR. LLL .EQ. 3) THEN
                 WRITE(IPO, 792) EXIN
                 WRITE(IPO, 796)
               END IF
               IF (LLL .EQ. 4) THEN
                 JLONG = IEJDAY-ISJDAY + 1
                 IF (JLONG .GE. 365) THEN
                   WRITE (IPO,797)
                  ELSE
                   WRITE (IPO,798) JLONG
                 END IF
                 WRITE (IPO, 792) EXIN
                 WRITE (IPO, 799)
               END IF
             END IF
           END IF

700      CONTINUE

         STAR(1,K11)=BLNK
         STAR(2,K2)=BLNK

710    CONTINUE

C
C  Link contributions table code
C

      IF (FLINK .EQ. 1) THEN

             WRITE (MSGS, *)
             WRITE (MSGS, *) 'Preparing link contribution tables.'
             WRITE (ISCN, *)
             WRITE (ISCN, *) 'Preparing link contribution tables.'

        ITR = 2
        IF (MODET .EQ. 1) THEN
          LS = 3
          LE = 4
         ELSE
          LS = 1
          LE = 2
        END IF

C    SUM CONTRIBUTIONS FOR TIME SPAN OF HIGH CONCENTRATIONS

      DO 652 I = 1, NR
        DO 653 LLL = LS, LE
          LA = LLL
            IF (LLL .EQ. 3) LA = 1
            IF (LLL .EQ. 4) LA = 2
          ITR = 2
            IF (LLL .EQ. 4) ITR = 1
            IF ((IEJDAY - ISJDAY) .EQ. 0 .AND. MODET .EQ. 1) ITR = 1
        DO 653 IK = 1, ITR
          DO 654 KK = 1 , NL
            CONTL(IK,I,LA,KK) = CONTL(IK,I,LA,KK) / NDV(IK,I,LA)
            TOTL(IK,I,LA) = TOTL(IK,I,LA) + CONTL(IK,I,LA,KK)
  654     CONTINUE
  653   CONTINUE
  652 CONTINUE

C
C CONSTRUCT TABLE OF LINK CONTRIBUTIONS BY RECEPTOR
C
CMOD?
      L10 = 10
        IF (NL .LT. 10) L10 = NL
      L120 = (NL - 1)/10

      DO 685 LLL = LS, LE
        LA = 1
          IF (LLL .EQ. 3) LA = 1
          IF (LLL .EQ. 4) LA = 2

        LLLL = LLL
         IF (LLL .EQ. 2) THEN
           LLLL = 2
           LA = 2
         END IF
         IF (LLL .EQ. 1) THEN
          LLLL = 1
           LA = 1
         END IF

        ITR = 2
          IF (LLLL .EQ. 4) ITR = 1
          IF ((IEJDAY - ISJDAY) .EQ. 0 .AND. MODET .EQ. 1) ITR = 1

        DO 680 IR = 1, ITR

           IF (ILIN .GE. (55-L120-8)) THEN
             IPAG = IPAG + 1
             ILIN = 9
              WRITE (IPO, 200) CHAR(12), VERSN
              WRITE (IPO, 462) LMON, LDAY, LYR, IPAG
              WRITE (IPO, 463) LHR, LMIN, LSEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
              WRITE (IPO, 210) JOB,RUN
              WRITE (IPO, 600)
            END IF

            IF (ILIN .LT. (55-L120-8)) THEN
             IF (LLLL .NE. 4) THEN
               IF (IR .EQ. 1) THEN
                 WRITE(IPO, 671) NATM(LLLL)
                ELSE
                 WRITE(IPO, 672) NATM(LLLL)
               END IF
              ELSE
               IF (JLONG .LT. 365) THEN
                 WRITE(IPO, 802) JLONG
                ELSE
                 WRITE(IPO, 803)
               END IF
             END IF
             IF (MODET .EQ. 0) THEN
               WRITE(IPO, 681)
              ELSE
               WRITE(IPO, 682)
             END IF
             WRITE(IPO, 792) EXIN
             WRITE(IPO, 673) (LNKN, II = 1, L10)
             WRITE(IPO, 674) (LNKT(II),II = 1, L10)
             WRITE(IPO, *)
             ILIN = ILIN + 8
           END IF

         DO 670 I = 1, NR
           JDAY = (JHXX(IR,I,LA)-1)/24 + 1
           LH2 =   JHXX(IR,I,LA)-(JDAY-1)*24
           WRITE(IPO,675) I, HMAXA(IR,I,LA), JDAY, LH2,
     +                     BKG(IR,I,LA), TOTL(IR,I,LA),
     +                    (CONTL(IR,I,LA,J), J = 1, L10)
           ILIN = ILIN + 1

           IF (NL .GT. 10) THEN
             DO 677 J = 1, L120
               J1 = J * 10 + 1
               J2 = J1 + 9
               J3 = J * 10
               ILIN = ILIN + 1
               WRITE(IPO,678) J3, (CONTL(IR,I,LA,JJ), JJ = J1, J2)
  677        CONTINUE
           END IF

           IF (ILIN .GT. (55-L120)) THEN
             IPAG = IPAG + 1
             ILIN = 9
              WRITE (IPO, 200) CHAR(12), VERSN
              WRITE (IPO, 462) LMON, LDAY, LYR, IPAG
              WRITE (IPO, 463) LHR, LMIN, LSEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
              WRITE (IPO, 210) JOB,RUN

             IF (I .NE. NR) THEN
               WRITE (IPO, 600)
               IF (LLLL .NE. 4) THEN
                 IF (IR .EQ. 1) THEN
                   WRITE(IPO, 671) NATM(LLLL)
                  ELSE
                   WRITE(IPO, 672) NATM(LLLL)
                 END IF
                ELSE
                 IF (JLONG .LT. 365) THEN
                   WRITE(IPO, 802) JLONG
                  ELSE
                   WRITE(IPO, 803)
                 END IF
               END IF
               IF (MODET .EQ. 0) THEN
                 WRITE(IPO, 681)
                ELSE
                 WRITE(IPO, 682)
               END IF
               WRITE(IPO, 792) EXIN
               WRITE(IPO, 673) (LNKN, II = 1, L10)
               WRITE(IPO, 674) (LNKT(II),II = 1, L10)
               WRITE(IPO, *)
               ILIN = ILIN + 8
             END IF
           END IF
  670    CONTINUE
  680  CONTINUE
  685 CONTINUE
      END IF

C
C  Frequency of Calm Durations table code
C
           IPAG = IPAG + 1
          WRITE (IPO, 200) CHAR(12), VERSN
          WRITE (IPO, 462) LMON, LDAY, LYR, IPAG
          WRITE (IPO, 463) LHR, LMIN, LSEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
          WRITE (IPO, 210) JOB,RUN
          WRITE (IPO, 900)

             NFLG = 0
             DO 910 I = 1, (ISIG - 1)
               IF (NIP(I) .GT. 0) THEN
                 WRITE (IPO,901) I, NIP(I)
                 NFLG = 1
               END IF
 910         CONTINUE
             DO 920 I = ISIG, 24
               IF (NIP(I) .GT. 0) THEN
                 NFLG = 1
                 J1 = NIP(I) / 10 + 1
                 J2 = MOD(NIP(I),10)
                 DO 930 J = 1, J1
                   J3 = (J-1) * 10 + 1
                   J4 = J * 10
                   IF (J .EQ. J1) J4 = NIP(I)
                   IF (J .EQ. 1) THEN
                      WRITE(IPO,932) I, NIP(I), (NIPJ(I,JJ),
     &                               NIPH(I,JJ), JJ = J3, J4)
                     ELSE
                      WRITE(IPO,933) (NIPJ(I,JJ),
     &                                NIPH(I,JJ), JJ = J3, J4)
                   END IF
 930             CONTINUE
               END IF
 920         CONTINUE
             IF (NFLG .EQ. 0) THEN
               WRITE(IPO, *)
               WRITE(IPO, *) '          No calm wind hours were ',
     +                      'encountered during this processing period.'
             END IF

 9999   CONTINUE

      WRITE(IPO, *)
      WRITE(IPO, *) '    Program terminated normally'
      WRITE(IPO, *)
      WRITE (MSGS, *)
      WRITE (MSGS, *) 'Program terminated normally'
      WRITE (ISCN, *)
      WRITE (ISCN, *) 'Program terminated normally'

C     ================
C      END OF PROGRAM
C     ================

C
C       -----------------
C       FORMAT STATEMENTS
C       -----------------
C
    1  FORMAT(/7X,'===================',
     +        /7X,'General Information',
     +        /7X,'===================')
   11  FORMAT(' The Met data file header expected is not the Met',
     +        ' data header read.')
   12  FORMAT(' Expected Surface ID & Yr/ Upper Air ID & Yr:', 2(I7,I3))
   13  FORMAT(' Met file Surface ID & Yr/ Upper Air ID & Yr:', 2(I7,I3))
   17  FORMAT (1X,//,22HPROGRAM RUN TERMINATED,///,
     + 61H * * LINK LENGTH MUST BE GREATER THAN OR EQUAL TO LINK WIDTH.)
   18  FORMAT (1X,//,22HPROGRAM RUN TERMINATED,///,
     +  46H * * SOURCE MUST BE WITHIN 10 METERS OF DATUM.)
   23  FORMAT(9X,'The MODE flag has been set to ',A1,' for calculating',
     *   ' CO averages.'/)
   24  FORMAT(9X,'The MODE flag has been set to ',A1,' for calculating',
     *   ' PM averages.'/)
   30  FORMAT(/7X,'Link Data Constants - (Variable data in *.LNK file)'
     +        /7X,'-------------------'/)
   31  FORMAT(/9X,'Run start date: ',2(i2.2,'/'),i2.2, 4X,'Julian:',I4,
     +       /9X,'      end date: ',2(i2.2,'/'),i2.2, 4X,'Julian:',I4/)
   32  FORMAT(11X,   'Met. Sfc. Sta. Id & Yr =', 2I6 /
     +        11X,   'Upper Air Sta. Id & Yr =', 2I6)
   34  FORMAT(/9X,'CAUTION: The input years for the Run and ',
     +   'Meteorological data '/9X, '         differ.  The respecti',
     +   've values are:',I4,' and ', I4,'.')
   33 FORMAT(/9X,'In ', I4, ', Julian day 1 is a ',A10/)
   37 FORMAT(11X,'Pattern #',I2,' is assigned to ',A10)
   43 FORMAT(9X,'A Tier',I2,' approach was used for input data prepara',
     +  'tion.'/)
   44  FORMAT(9X,'The patterns from the input file'
     +       /9X,'have been assigned as follows:'/)
   46 FORMAT(/7X,'Receptor Data'/
     +         7X,'-------------'/)
   47 FORMAT(9X,'Ambient background concentrations are included in ',
     +          'the averages below.'/)
   48 FORMAT(9X,'Ambient background concentrations are excluded from ',
     +          'the averages below.'/)
   51   FORMAT   (4I2, 2F9.2, F6.1, I2, 2F7.1)
   53   FORMAT(1X,'  Processing has reached Julian day:',I3)
   56 FORMAT(/9X,'Rural mixing heights were processed.')
   57 FORMAT(/9X,'Urban mixing heights were processed.')
  167   FORMAT(' NUMBER OF RECEPTORS INPUT =',I4, ' > MAXIMUM ALLOWED',
     *  ' BY PROGRAM =',I4,'.  PROGRAM IS TERMINATED!')
CMOD  200   FORMAT (A1,58X,'CAL3QHCR (Dated: 04244)')
  200   FORMAT (A1,58X,'CAL3QHCR (Dated: ',A5,')')
  210   FORMAT (/5X,5HJOB: ,A40,20X,5HRUN: ,A40)
  220   FORMAT (/7X,'Site & Meteorological Constants'
     +          /7X,'-------------------------------')
  230   FORMAT (9X,5HVS = ,F5.1,5H CM/S,7X,5HVD = ,F5.1,5H CM/S,
     +         7X,5HZ0 = ,F4.0,3H CM, 5X,7HATIM = ,F4.0/)
  251 FORMAT(/7X, A10,5X,'Hour ending:',I3,'    Year:',I5/)
  252   FORMAT (/5X,'LINK VARIABLES',/,
     +           5X,'--------------')
  261   FORMAT (9X,16HLINK DESCRIPTION,5X,
     +  42H*         LINK COORDINATES (M)           *,2X,6HLENGTH,
     +    2X,3HBRG,1X,4HTYPE,3X,3HVPH,5X,2HEF,5X,1HH,
     +    4X,1HW,4X,3HV/C,1X,5HQUEUE)
  266   FORMAT (9X,16HLINK DESCRIPTION,5X,
     +  42H*         LINK COORDINATES (FT)          *,2X,6HLENGTH,
     +    2X,3HBRG,1X,4HTYPE,2X,3HVPH,5X,2HEF,5X,1HH,
     +    4X,1HW,4X,3HV/C,1X,5HQUEUE)
  271   FORMAT(30X,42H*     X1        Y1        X2        Y2   *,3X,
     +    3H(M),3X,5H(DEG),12X,6H(G/MI),2X,3H(M),2X,3H(M),
     +    7X,5H(VEH),1X,5HTRUNC)
  276   FORMAT(30X,42H*     X1        Y1        X2        Y2   *,3X,
     +    4H(FT),2X,5H(DEG),12X,6H(G/MI),1X,4H(FT),2X,4H(FT),
     +    6X,5H(VEH),1X,5HTRUNC)
C
  260   FORMAT (13X,16HLINK DESCRIPTION,5X,
     +  42H*         LINK COORDINATES (M)           *,3X,6HLENGTH,
     +    3X,3HBRG,2X,4HTYPE,4X,1HH,5X,1HW,'  NLANES')
  265   FORMAT (13X,16HLINK DESCRIPTION,5X,
     +  42H*         LINK COORDINATES (FT)          *,3X,6HLENGTH,
     +    3X,3HBRG,2X,4HTYPE,4X,1HH,5X,1HW,'  NLANES')
  270   FORMAT(34X,42H*     X1        Y1        X2        Y2   *,5X,
     +    3H(M),3X,5H(DEG),8X,3H(M),3X,3H(M))
  275   FORMAT(34X,42H*     X1        Y1        X2        Y2   *,4X,
     +    4H(FT),3X,5H(DEG),7X,4H(FT),2X,4H(FT))
  280   FORMAT (10X,24(1H-),1H*,40(1H-),1H*,40(1H-))
  281   FORMAT ( 6X,24(1H-),1H*,40(1H-),1H*,60(1H-))

CMOD  290   FORMAT (5X,I3,2H. ,A20,1H*,4(1X,F8.1,1X),1H*,1X,F6.0,
  290   FORMAT (4X,I4,2H. ,A20,1H*,4(1X,F8.1,1X),1H*,1X,F6.0,
     +    2X,F4.0,2X,A2,1X,F6.1,2X,F5.1,1X,F5.1,2X,F4.1)

CMOD  293   FORMAT (9X,I3,2H. ,A20,1H*,4(1X,F8.1,1X),1H*,4X,F5.0,
  293   FORMAT (8X,I4,2H. ,A20,1H*,4(1X,F8.1,1X),1H*,4X,F5.0,
     +    3X,F4.0,2X,A2,1X,F6.1,1X,F5.1,I5)

CMOD  294   FORMAT (5X,I3,2H. ,A20,1H*,4(1X,F8.1,1X),1H*,1X,F6.0,
  294   FORMAT (4X,I4,2H. ,A20,1H*,4(1X,F8.1,1X),1H*,1X,F6.0,
     +    2X,F4.0,2X,A2,1X,F6.1,2X,F5.1,1X,F5.1,2X,F4.1,1X,F4.2,1X,F5.1,
     +    2X,A3)

CMOD  295   FORMAT (9X,I3,2H. ,A20,1H*,4(1X,F8.1,1X),1H*,3X,F6.0,
  295   FORMAT (8X,I4,2H. ,A20,1H*,4(1X,F8.1,1X),1H*,3X,F6.0,
     +    3X,F4.0,2X,A2,1X,F6.1,1X,F5.1)

  309   FORMAT (35X,'*           COORDINATES (M) ')
  310   FORMAT (35X,'*           COORDINATES (FT)')
  321   FORMAT (14X,8HRECEPTOR,13X,'*         X          Y          Z')
  331   FORMAT (10X,25(1H-),1H*,37(1H-))
CMOD  341   FORMAT (10X,I2,2H. ,A20,1X,1H*,4X,F8.1,3X,F8.1,3X,F8.1)
  341   FORMAT (8X,I4,2H. ,A20,1X,1H*,4X,F8.1,3X,F8.1,3X,F8.1)
 400  FORMAT(/7X,'==============',/
     1        7X,'Output Section',/
     2        7X,'=============='/)
 405  FORMAT (7X,'NOTES PERTAINING TO THE REPORT'//
     +9X,'1.  THE HIGHEST AVERAGE IN EACH OF THE FIRST TWO COLUMNS OF '
     +  ,'EACH TABLE BELOW ARE SUFFIXED BY AN ASTERISK (*).'/
     +9X,'    FOR PM OUTPUT, THERE IS ONLY ONE COLUMN AND ASTERISK '
     +  ,'FOR THE ANNUAL AVERAGE/PERIOD OF CONCERN TABLE.'/)
 410  FORMAT (9X,'2.  THE NUMBERS IN PARENTHESES ARE THE JULIAN DAY AND'
     +  , ' ENDING HOUR FOR THE PRECEDING AVERAGE.'/)
 415  FORMAT (9X,'3.  THE NUMBER OF CALM HOURS USED IN PRODUCING EACH',
     + ' AVERAGE ARE PREFIXED BY A ', 2HC./)
 416  FORMAT (/7X,'PRIMARY AVERAGES.')
 417  FORMAT (/7X,'PRIMARY AND SECONDARY AVERAGES.')
C
  421   FORMAT (5X,'ADDITIONAL QUEUE LINK PARAMETERS',/,
     +          5X,'--------------------------------' )
  422   FORMAT (9X,16HLINK DESCRIPTION,5X,'*   '
     +    ,5HCYCLE,4X,3HRED,5X,9HCLEARANCE,2X,8HAPPROACH,
     +2X,10HSATURATION,3X,4HIDLE,3X,6HSIGNAL,3X,7HARRIVAL)
  423   FORMAT(30X,4H*   ,1X,6HLENGTH,3X,4HTIME,4X,9HLOST TIME,4X,
     +  3HVOL,5X,9HFLOW RATE,3X,6HEM FAC,3X,4HTYPE,5X,4HRATE)
  424   FORMAT(30X,4H*   ,2X,5H(SEC),3X,5H(SEC),4X,5H(SEC),6X,
     +  5H(VPH),6X,5H(VPH),4X,7H(gm/hr))
  425   FORMAT (6X,24(1H-),1H*,80(1H-))
C
CMOD  426   FORMAT (5X,I3,2H. ,A20,1H*,2(3X,I5,1X),4X,F5.1,
  426   FORMAT (4X,I4,2H. ,A20,1H*,2(3X,I5,1X),4X,F5.1,
     +    5X,I5,7X,I4,4X,F7.1,6X,I1,8X,I1)

  462   FORMAT(/5X,'DATE : ',2(i2.2,'/'),i2.2, 97X, 'PAGE:',I3)
  463   FORMAT(5X,'TIME : ',2(i2.2,':'),i2.2)
  468   FORMAT(5X,'TIME : ',2(i2.2,':'),i2.2,40X, 'LINK DATA VARIABLES')
  464   FORMAT(1X, 'Expected date (YY/JJJ/IH):', i2.2,'/',I3.3,'/',i2.2)
  465   FORMAT(1X, 'Read     date (YY/JJJ/HR):', i2.2,'/',I3.3,'/',i2.2)

  600  FORMAT (/7X,'LINK CONTRIBUTION TABLES'/)
  671  FORMAT(/9X,'MAXIMUM', I3,'-HOUR AVERAGED LINK CONTRIBUTIONS')
  672  FORMAT(/9X,'SECOND HIGHEST',I3,'-HOUR AVERAGED LINK ',
     +           'CONTRIBUTIONS')
  673  FORMAT(11X,'Rcptr Total  Ending  Ambient  Total ', 10(2X,A4,:))
  674  FORMAT(11X,' No.   Conc  Day Hr  Backgnd   Link', 10(3X,A3,:))
CMOD  675  FORMAT(11X,I3,F8.2,1X,'(',I3,',', I2,')',F7.2,F8.2,1X,10(F6.2,:))
  675  FORMAT(11X,I3,F10.4,1X,'(',I3,',', I2,')',
     +        F9.2,F10.4,1X,10(F8.4,:))
  678  FORMAT(37X,'Links',I4,'+', 10(F6.2,:))
  681  FORMAT(9X,'     IN PARTS PER MILLION (PPM)')
  682  FORMAT(9X,'        IN MICROGRAMS/M**3 ')
  792  FORMAT(9X,A9,' AMBIENT BACKGROUND CONCENTRATIONS.'/)
CMOD  794  FORMAT (/9X,'FIVE HIGHEST',I3,'-HOUR END-TO-END AVERAGE CONCEN',
  794  FORMAT (/9X,'SIX HIGHEST',I3,'-HOUR END-TO-END AVERAGE CONCEN',
     +'TRATIONS IN PARTS PER MILLION')
CMOD  795  FORMAT (/9X,'FIVE HIGHEST',I3,'-HOUR END-TO-END AVERAGE CONCEN',
  795  FORMAT (/9X,'SIX HIGHEST',I3,'-HOUR END-TO-END AVERAGE CONCEN',
     +'TRATIONS IN MICROGRAMS/M**3')
  796  FORMAT (
     + 17X,' Highest',16X,'Second Highest',9X,'Third Highest',
     +  8X,'Fourth Highest',9X, 'Fifth Highest',9x,'Sixth Highest',/
CMOD     +  6X,'Fourth Highest',7X, 'Fifth Highest',/
     + 12X,'Rcptr ',6('        Ending',7X),/
     + 11X,' No.  ',6(' Conc  Day Hr  Calm   ')/)
CMOD     + 11X,'Rcptr ',5('       Ending',7X),/
CMOD     + 11X,' No.  ',5(' Conc  Day Hr  Calm ')/)
  797  FORMAT (/9X,'THE HIGHEST ANNUAL AVERAGE CONCENTRATIONS'/
     +         9X,'        IN MICROGRAMS/M**3 ')
  798  FORMAT (/9X,'THE HIGHEST ', I3,' - DAY AVERAGE CONCENTRATIONS'/
     +         9X,'        IN MICROGRAMS/M**3 ')
  799  FORMAT (12X,'Receptor       Maximum  Ending'/
     +        12X,' Number        Conc     Day Hr  Calm')
C  800  FORMAT (11X,I4,2(F7.2,A1,'(',I3,',',I2,')',1X,1HC,I2),
  800  FORMAT (11X,I4,2(F9.4,A1,'(',I3,',',I2,')',1X,1HC,I2),
     +               4(F9.4,1X,'(',I3,',',I2,')',1X,1HC,I2,:))
CMOD     +               3(F7.2,1X,'(',I3,',',I2,')',1X,1HC,I2,:))
C  801  FORMAT (11X,I5,8X,F7.2,A1,1X,'(',I3,',',I2,')',1X, 1HC,I4,:)
  801  FORMAT (11X,I5,8X,F9.4,A1,1X,'(',I3,',',I2,')',1X, 1HC,I4,:)
  802  FORMAT(/9X,'MAXIMUM',I4,' - DAY AVERAGED LINK CONTRIBUTIONS')
  803  FORMAT(/9X,'MAXIMUM ANNUAL AVERAGED LINK CONTRIBUTIONS')
  900  FORMAT(//59X, 'CALM DURATION FREQUENCY'//
     &         9X, '  Hours of    Frequency',/9X,
     &             'Consecutive       of    ',/9X,
     &             'Calm Winds    Occurrence    (Julian day/hour ending'
     & ,') of Significant Occurrences'/)
  901  FORMAT(13X,I2,11X,I3)
  932  FORMAT(13X,I2,11X,I3,8X,10('(',I3,',',I2,')',:))
  933  FORMAT(13X,2X,11X,3X,8X,10('(',I3,',',I2,')',:))

 8399   FORMAT(/,7X,'Model Results',/,
     +           7X,'-------------'/)
 8400   FORMAT(9X,'Remarks : In search of the wind direction corresp',
     +  'onding'/19X,'to the maximum concentration, only the first',/
     +    19X,'direction, of the directions with the same maximum',/
     +    19X,'concentrations, is indicated as the maximum.')
 8500   FORMAT(/9X,'        ',1H*,1X,'MAXIMUM HOURLY CONCENTRATIONS',
     +    ' WITH ANY AMBIENT BACKGROUND CONCENTRATIONS (BKG) ADDED')
 8509   FORMAT(9X,'        ',1H*,1X,'        (PPM)')
 8510   FORMAT(9X,'        ',1H*,1X,'  (MICROGRAMS/M**3)')
CMOD 8511   FORMAT(9X,'        ',1H*,1X,20(3X,A5))
 8511   FORMAT(9X,'        ',1H*,1X,20(1X,A8))
 8512   FORMAT(9X,'----------',1H*,20A10)
CMOD 9040   FORMAT(9X,'MAX+BKG ',1H*,20(1X,F7.1))
CMOD 9041   FORMAT(9X,'- BKG   ',1H*,20(1X,F7.1))
CMOD 9042   FORMAT(9X,'  MAX   ',1H*,20(1X,F7.1))
 9040   FORMAT(9X,'MAX+BKG ',1H*,20(1X,F8.4))
 9041   FORMAT(9X,'- BKG   ',1H*,20(1X,F8.4))
 9042   FORMAT(9X,'  MAX   ',1H*,20(1X,F8.4))
 9043   FORMAT(9X,'WIND DIR',1H*,1X,20(3X,I4,2X))
 9044   FORMAT(9X,'JULIAN  ',1H*,1X,20(3X,I4,2X))
 9045   FORMAT(9X,'HOUR    ',1H*,1X,20(3X,I4,2X))
 9048   FORMAT(/9X,'THE HIGHEST CONCENTRATION OF',F8.4,' PPM',
CMOD     +            ' OCCURRED AT RECEPTOR ', A5,'.'/)
     +            ' OCCURRED AT RECEPTOR ', A7,'.'/)
 9049   FORMAT(/9X,'THE HIGHEST CONCENTRATION OF',F10.4,' UG/M**3',
CMOD     +            ' OCCURRED AT RECEPTOR ', A5,'.'/)
     +            ' OCCURRED AT RECEPTOR ', A7,'.'/)

      END
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      SUBROUTINE RNRK(RNMX,JHX,LIM,I,BK)
C
C        SUBROUTINE TO RANK RUNNING AVERAGES.  EXTRACTED FROM CHAVG.
C
      PARAMETER (NN = 5000)

      CHARACTER*40 JOB, RUN
      CHARACTER*9  EXIN
      CHARACTER*2 BLNK
      CHARACTER*1 STAR, STR
      INTEGER CUMT

CMOD      COMMON /ALL/ BKG(5,NN,2), CUM(NN,2), CUMB(4), CUMT(4),
CMOD     1             HMAXA(5,NN,2), ISYR, LPYR, IHR(5,NN,2), IPAG,
CMOD     2             JDAY, L3, L4, LH, NDAY(25), NHR,
CMOD     3             NHRS(25), NR, NC(9,NN), NEC(5,NN,2),
CMOD     4             NDV(9,NN,2)
      COMMON /ALL/ BKG(6,NN,2), CUM(NN,2), CUMB(4), CUMT(4),
     1             HMAXA(6,NN,2), ISYR, LPYR, IHR(6,NN,2), IPAG,
     2             JDAY, L3, L4, LH, NDAY(25), NHR,
     3             NHRS(25), NR, NC(9,NN), NEC(6,NN,2),
     4             NDV(9,NN,2)
      COMMON /CHR/ STAR(2,NN), BLNK, STR, JOB, RUN, EXIN
      COMMON /IO/  ICTL, IN, MSGS, IMET, IT1, IT2, IPO, ILK, ISCN, IPLT

      DIMENSION RNMX(9,NN), JHX(9,NN), NCNT(4), NATM(4), BK(9,NN)

      DATA NCNT /6,1,18,6570/, NATM /8,1,24,8760/

CMOD
        jj = 0

        LPYR = 0
          IF (MOD(ISYR, 4) .EQ. 0) LPYR = 1
          IF (LPYR .EQ. 1) NCNT(4) = 6588
          IF (LPYR .EQ. 1) NATM(4) = 8784
C
C***  LIM1 IS THE AVERAGING PERIOD; MAKE SURE THAT THE RUNNING AVERAGE
C***  IS COMPLETE.
C

      LIM1 = LIM - 1
        IF (NHR.LT.LIM1) RETURN

      NCN = MAX (CUMT(I),NCNT(I))
      NDIF = NATM(I) - CUMT(I)

      DO 500 K=1,NR
CMOD        A = CUM(K,I)/NCN
CMOD        B = CUMB(1)/NCN
        A = CUM(K,I)/real(NCN)
        B = CUMB(1)/real(NCN)
C
C         IS CONC. 'A' GREATER THAN CURRENT LEAST RANKED VALUE ?
C         IF SO, THEN SORT AND DISCARD CURRENT LOWEST VALUE IN RANK
C
          DO 10 J = LIM, 1, -1
            IF (A .GT. RNMX(J,K)) JJ = J
10        CONTINUE
          IF (JJ .GT. 0) THEN
          IF (A .GT. RNMX(JJ,K)) THEN
            IF (JJ .NE. LIM) THEN
C                 J IS BETWEEN 1 AND LIM1.
C                 CASCADE DATA DOWN ENDING WITH JTH TO J+1ST
              JL = JJ + LIM1
              DO 30 JAX = JJ, LIM1, 1
                JA = JL - JAX
                JB = JA + 1
                RNMX(JB,K) = RNMX(JA,K)
                JHX(JB,K)  = JHX(JA,K)
                BK(JB,K)   = BK(JA,K)
                NC(JB,K)   = NC(JA,K)
                NDV(JB,K,I) = NDV(JA,K,I)
30            CONTINUE
C                 PUT NEW VALUES IN JTH POSITION.
            END IF
           RNMX(JJ,K) = A
           JHX(JJ,K)  = LH
           BK(JJ,K)   = B
           NC(JJ,K)   = NDIF
           NDV(JJ,K,I) = NCN
          END IF
          END IF
C            CONCENTRATION IS ONE OF THE TOP CONC.'S FOR THIS RECEPTOR
500   CONTINUE

      RETURN
      END
C *********************************************************************
      SUBROUTINE RNK5 (L)
C
C        SUBROUTINE RNK5 IS CALLED BY THE MAIN PROGRAM AS IS DONE
C          IN THE PROGRAM CHAVG.  THIS SUBROUTINE IS DESIGNED
C          TO ARRANGE CONCENTRATION ARRAYS SO THAT HIGH-FIVE
C          CONCENTRATION TABLES CAN BE OUTPUT.
C
      PARAMETER (NN = 5000)

      INTEGER CUMT

CMOD      COMMON /ALL/ BKG(5,NN,2), CUM(NN,2), CUMB(4), CUMT(4),
CMOD     1             HMAXA(5,NN,2), ISYR, LPYR, IHR(5,NN,2), IPAG,
CMOD     2             JDAY, L3, L4, LH, NDAY(25), NHR,
CMOD     3             NHRS(25), NR, NC(9,NN), NEC(5,NN,2),
CMOD     4             NDV(9,NN,2)
      COMMON /ALL/ BKG(6,NN,2), CUM(NN,2), CUMB(4), CUMT(4),
     1             HMAXA(6,NN,2), ISYR, LPYR, IHR(6,NN,2), IPAG,
     2             JDAY, L3, L4, LH, NDAY(25), NHR,
     3             NHRS(25), NR, NC(9,NN), NEC(6,NN,2),
     4             NDV(9,NN,2)
      COMMON /JDY/ IEJDAY, ISJDAY
      COMMON /IO/  ICTL, IN, MSGS, IMET, IT1, IT2, IPO, ILK, ISCN, IPLT

      DIMENSION NCNT(4), NATM(4)

      DATA NCNT /6,1,18,0/, NATM /8,1,24,0/

      JES = (IEJDAY-ISJDAY+1) * 24
      NCNT(4) =  JES * .75 + .99
      NATM(4) =  JES

      IF (LPYR .EQ. 1 .AND. JES .EQ. 365) THEN
        NCNT(4) = JES + 18
        NATM(4) = JES + 24
      ENDIF
      NCN = MAX (CUMT(L),NCNT(L))
      NDIF = NATM(L) - CUMT(L)
      IF (L .EQ. 1) LA = 1
      IF (L .EQ. 2) LA = 2
      IF (L .EQ. 3) LA = 1
      IF (L .EQ. 4) LA = 2
C
CMOD      BK = CUMB(L) / NCN
      BK = CUMB(L) / real(NCN)
      DO 50 K=1,NR
CMOD          A = CUM(K,LA) / NCN
          A = CUM(K,LA) / real(NCN)
CMOD        IF ( A .LE. HMAXA(5,K,LA)) GO TO 50
CMOD          DO 10 J = 1, 5
        IF ( A .LE. HMAXA(6,K,LA)) GO TO 50
          DO 10 J = 1, 6
            IF ( A .GT. HMAXA(J,K,LA)) GO TO 20
C           CONCENTRATION IS ONE OF THE TOP SIX
10        CONTINUE
          WRITE (IPO,70)
          GO TO 50
C          THE FOLLOWING DO-LOOP HAS THE EFFECT OF INSERTING A NEW
C          CONCENTRATION ENTRY INTO ITS PROPER POSITION WHILE SHIFTING
C          DOWN THE 'OLD' LOWER CONCENTRATIONS THUS ESTABLISHING THE
C          'HIGH-FIVE' CONCENTRATION TABLE.
CMOD20      IF (J .LT. 5) THEN
20      IF (J .LT. 6) THEN
          I4 = J+5
CMOD          DO 30 IJX = J, 4, 1
          DO 30 IJX = J, 5, 1
            IJ = I4 - IJX
            IJP1 = IJ+1
            HMAXA(IJP1,K,LA) = HMAXA(IJ,K,LA)
              NEC(IJP1,K,LA) =   NEC(IJ,K,LA)
              BKG(IJP1,K,LA) =   BKG(IJ,K,LA)
              IHR(IJP1,K,LA) =   IHR(IJ,K,LA)
              NDV(IJP1,K,LA) =   NDV(IJ,K,LA)
  30      CONTINUE
        END IF
C           INSERT LATEST CONC, DAY AND START HR INTO THE
C           PROPER RANK IN THE HIGH-FIVE TABLE
CMOD        HMAXA(J,K,LA) = CUM(K,LA) / NCN
        HMAXA(J,K,LA) = CUM(K,LA) / real(NCN)
        IHR(J,K,LA) = LH
        BKG(J,K,LA) = BK
        NEC(J,K,LA) = NDIF
        NDV(J,K,LA)  = NCN
50    CONTINUE
      RETURN
C
70    FORMAT (' ERROR IN RANKING END-TO-END AVERAGES')
C
      END

C *********************************************************************
      SUBROUTINE OUTRA (ILIN)
C
C       THIS SUBROUTINE IS DESIGNED TO PRINTOUT 8-HOUR RUNNING
C         NONOVERLAPPING CO CONCENTRATIONS.  THIS SUBROUTINE
C         WAS EXTRACTED FROM CHAVG.
C
      PARAMETER (NN = 5000)

      CHARACTER*40 JOB, RUN
      CHARACTER*20 LNK
      CHARACTER*10 DAYWK
      CHARACTER*9  EXIN
      CHARACTER*2 BLNK, TYP
      CHARACTER*1 MODE, STAR, STR
      INTEGER CUMT

CMOD      COMMON /ALL/ BKG(5,NN,2), CUM(NN,2), CUMB(4), CUMT(4),
CMOD     1             HMAXA(5,NN,2), ISYR, LPYR, IHR(5,NN,2), IPAG,
CMOD     2             JDAY, L3, L4, LH, NDAY(25), NHR,
CMOD     3             NHRS(25), NR, NC(9,NN), NEC(5,NN,2),
CMOD     4             NDV(9,NN,2)
      COMMON /ALL/ BKG(6,NN,2), CUM(NN,2), CUMB(4), CUMT(4),
     1             HMAXA(6,NN,2), ISYR, LPYR, IHR(6,NN,2), IPAG,
     2             JDAY, L3, L4, LH, NDAY(25), NHR,
     3             NHRS(25), NR, NC(9,NN), NEC(6,NN,2),
     4             NDV(9,NN,2), VERSN
      COMMON /DT/  LHR, LMIN, LSEC, LX, LYR, LMON, LDAY
      COMMON /IO/  ICTL, IN, MSGS, IMET, IT1, IT2, IPO, ILK, ISCN, IPLT
      COMMON /CHR/ STAR(2,NN), BLNK, STR, JOB, RUN, EXIN
      COMMON /LNKC/ DAYWK(7), LNK(NN), MODE, TYP(NN)

C      DIMENSION RNMX(LIM,NN),JHX(LIM,NN)

      ILIN = ILIN + 9
      WRITE (IPO, 380) EXIN
      WRITE (IPO, 381)

      K1=1
      K2=1
      A=HMAXA(1,1,1)
      B=HMAXA(2,1,1)
C        IF NEW CONC. IS HIGHER THAN A, REPLACE A.
      IF (NR.EQ.1) GO TO 191
        DO 190 K=2,NR
          IF (HMAXA(1,K,1).LE.A) GO TO 190
            A=HMAXA(1,K,1)
            K1=K
190     CONTINUE
191     STAR(1,K1)=STR

C FIND HIGHEST SECOND AVERAGE CONCENTRATION

      IF (NR.EQ.1) GO TO 201
        DO 200 K=2,NR
          IF (HMAXA(2,K,1).LE.B) GO TO 200
            B=HMAXA(2,K,1)
            K2=K
200     CONTINUE
201     STAR(2,K2)=STR

C
C***     LOOP OVER ALL RECEPTORS
C***     PREPARE DATE FOR OUTPUT, LOCATE SECOND HIGHEST NONOVERLAPPING
C***     AVERAGE, AND PRINT CONCENTRATIONS

       DO 240 J=1,NR
         ISW = 0
         DO 220 K=1,2
           I = IHR(K,J,1) - 1
           NDAY(K) = I/24 + 1
           NHRS(K)  = I - ((NDAY(K)-1) * 24) + 1
 220     CONTINUE

         ILIN = ILIN + 1
         WRITE (IPO,410) J,(HMAXA(K,J,1),STAR(K,J),NDAY(K),
     &      NHRS(K),NC(K,J),K=1,2)

         IF (ILIN .GT. 55) THEN
           IPAG = IPAG + 1
           ILIN = 6
           WRITE (IPO, 300) CHAR(12), VERSN
           WRITE (IPO, 462) LMON, LDAY, LYR, IPAG
           WRITE (IPO, 463) LHR, LMIN, LSEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')
CL     +            'DATE: ',IDATE,' TIME: ',ITIME
           WRITE (IPO, 310) JOB,RUN

           IF (NR .NE. J) THEN
             ILIN = ILIN + 8
             WRITE (IPO, 380) EXIN
             WRITE (IPO, 381)
           END IF
         END IF

240    CONTINUE

C      REPLACE ASTERISKS WITH BLANKS.
         STAR(1,K1)=BLNK
         STAR(2,K2)=BLNK

      RETURN

CMOD 300  FORMAT (A1,46X,'CAL3QHCR (Dated: 04244)')
 300  FORMAT (A1,46X,'CAL3QHCR (Dated: ',A5,')')
 310  FORMAT (/7X,5HJOB: ,A40,13X,5HRUN: ,A40)
 380  FORMAT (/9X,'MAXIMUM 8-HOUR RUNNING NONOVERLAPPING AVERAGE ',
     +  'CONCENTRATIONS' /9X,'  IN PARTS PER MILLION (PPM),'
     +  /,13X,A9,' AMBIENT BACKGROUND CONCENTRATIONS.'/)
 381  FORMAT (11X,
     + '                       Highest             Second highest'/11X,
     + ' Receptor               Ending                  Ending'/11X,
     + '  Number        Conc    Day Hr  Calm    Conc    Day Hr  Calm'/)
 410  FORMAT (13X,I4,5X,2(2X,F7.2,A1,2X,'(',I3,',',I2,')',1X,'C',I2))
 462  FORMAT(/7X,'DATE : ',i2.2,'/',i2.2,'/',i2.2, 100X, 'PAGE:',I2)
 463  FORMAT(7X,'TIME : ',i2.2,':',i2.2,':',i2.2)
      END
C
      SUBROUTINE JULIAN(IYR, IMN, IDY, JDY)
C***********************************************************************
C                 JULIAN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE:    CONVERT YR/MN/DY DATE TO JULIAN DAY (1-366),
C                    INCLUDES TEST FOR 100 AND 400 YEAR CORRECTIONS TO
C                    HANDLE 4 DIGIT YEARS BEYOND 2099 AND BEFORE 1901
C                    (WILL WORK WITH 2 DIGIT YR FOR PERIOD 1901-2099)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        MODIFICATION: March 30, 1994
C                        Extracted from ISCST2.  Lines of code were
C                          added, deleted, or modified by Peter Eckhoff
C                          for use in CAL3QHCR.
C
C        DATE:    March 2, 1992
C
C        INPUTS:     YEAR,  IYR (2 OR 4 DIGIT)
C                    MONTH, IMN
C                    DAY,   IDY
C
C        OUTPUT:     JULIAN DAY,  JDY (1-366)
C
C        CALLED FROM:   MAIN
C
C        ERROR HANDLING:   Checks for Invalid Month or Day
C***********************************************************************

      COMMON /IO/  ICTL, IN, MSGS, IMET, IT1, IT2, IPO, ILK, ISCN, IPLT

C     Variable Declarations
      INTEGER NDAY(12), IDYMAX(12)

C     Variable Initializations
      DATA NDAY/0,31,59,90,120,151,181,212,243,273,304,334/
      DATA IDYMAX/31,29,31,30,31,30,31,31,30,31,30,31/
      JDY = 0

C     Check for Invalid Month or Day
      IF (IMN.LT.1 .OR. IMN.GT.12) THEN
        WRITE(MSGS,*) 'Month value in error, value:', IMN
        WRITE(MSGS,*) '  Day, year, jday of month above:', IDY, IYR, JDY
        WRITE(ISCN,*) 'Month value in error, value:', IMN
        WRITE(ISCN,*) '  Day, year, jday of month above:', IDY, IYR, JDY
         STOP
      END IF

      IF (IDY .GT. IDYMAX(IMN)) THEN
        WRITE(MSGS,*) 'Day exceeds days in month:', IDY
        WRITE(MSGS,*)'  Month, year & jday of day above:', IMN, IYR, JDY
        WRITE(ISCN,*) 'Day exceeds days in month:', IDY
        WRITE(ISCN,*)'  Month, year & jday of day above:', IMN, IYR, JDY
         STOP
      END IF

C     Determine JULIAN Day Number; For Non-Leap Year First
C       Two-digit year used.
      IF ((MOD(IYR,4) .NE. 0)
     +   .OR. (MOD(IYR,100) .EQ. 0 .AND. MOD(IYR,400) .NE. 0)) THEN
C        Not a Leap Year
        IF (IMN.NE.2 .OR. (IMN.EQ.2 .AND. IDY.LE.28)) THEN
            JDY = IDY + NDAY(IMN)
         ELSE
           WRITE(MSGS,*) 'Invalid Date; 2/29+ in a Non-Leap Year'
           WRITE(MSGS,*) '  Values are m,d,yr,jdy:', IMN, IDY, IYR, JDY
           WRITE(ISCN,*) 'Invalid Date; 2/29+ in a Non-Leap Year'
           WRITE(ISCN,*) '  Values are m,d,yr,jdy:', IMN, IDY, IYR, JDY
             STOP
        END IF
       ELSE
C        Leap Year
         JDY = IDY + NDAY(IMN)
         IF (IMN .GT. 2)  JDY = JDY + 1
      END IF

 999  CONTINUE
       RETURN
      END

C *********************************************************************
      BLOCK DATA
C
      PARAMETER (NN = 5000)

      CHARACTER*40 JOB, RUN
      CHARACTER*20 LNK
      CHARACTER*10 DAYWK
      CHARACTER*9  EXIN
      CHARACTER*2 BLNK, TYP
      CHARACTER*1 MODE, STAR, STR
      INTEGER CUMT


CMOD      COMMON /ALL/ BKG(5,NN,2), CUM(NN,2), CUMB(4), CUMT(4),
CMOD     1             HMAXA(5,NN,2), ISYR, LPYR, IHR(5,NN,2), IPAG,
CMOD     2             JDAY, L3, L4, LH, NDAY(25), NHR,
CMOD     3             NHRS(25), NR, NC(9,NN), NEC(5,NN,2),
CMOD     4             NDV(9,NN,2)
      COMMON /ALL/ BKG(6,NN,2), CUM(NN,2), CUMB(4), CUMT(4),
     1             HMAXA(6,NN,2), ISYR, LPYR, IHR(6,NN,2), IPAG,
     2             JDAY, L3, L4, LH, NDAY(25), NHR,
     3             NHRS(25), NR, NC(9,NN), NEC(6,NN,2),
     4             NDV(9,NN,2)

      COMMON /CHR/ STAR(2,NN), BLNK, STR, JOB, RUN, EXIN
      COMMON /IO/  ICTL, IN, MSGS, IMET, IT1, IT2, IPO, ILK, ISCN, IPLT
      COMMON /LNKC/ DAYWK(7), LNK(NN), MODE, TYP(NN)

CMOD      DATA HMAXA/50000*0./,CUM/10000*0./,STAR/10000*' '/,BLNK/'  '/,
CMOD     +     STR/'*'/, IHR/50000*0/, L3/1/, L4/2/, NHR/0/, IPAG/0/,
CMOD     +     ICTL /5/, ISCN/6/, IN /7/, IMET/10/, MSGS /8/, IPO/9/,
CMOD     *     IT1 /11/, IT2/12/, ILK/15/
      DATA HMAXA/60000*0./,CUM/10000*0./,STAR/10000*' '/,BLNK/'  '/,
     +     STR/'*'/, IHR/60000*0/, L3/1/, L4/2/, NHR/0/, IPAG/0/,
     +     ICTL /5/, ISCN/6/, IN /7/, IMET/10/, MSGS /8/, IPO/9/,
     *     IT1 /11/, IT2/12/, IPLT/13/, ILK/15/
      DATA DAYWK / 'Monday.', 'Tuesday.',  'Wednesday.', 'Thursday.',
     +             'Friday.', 'Saturday.', 'Sunday.' /

       END


C  ********************************************************************

      SUBROUTINE CPRE (ISYR, ISYR4)

C
C        Purpose:
C
C           To preprocess a small data set of emissions, traffic,
C           and signalization (ETS) data into a week of ordered
C           data.  The input data can consist of one hour of ETS
C           data or up to a week of hourly ETS data in the form
C           of up to seven diurnal sets.  The data are synchronized
C           to the day of the week using the MET data year.
C
C
C        Programmed by:
C
C           Peter Eckhoff
C           EPA
C           MD-14
C           Research Triangle Park, NC 27711
C
C        Written to:
C
C           FORTRAN 77 Standards
C
C        Modifications:
C
C           December 8, 1994 - Original code.
C
C  ********************************************************************
C
C      ===========
C       I/O UNITS
C      ===========
C
C         7 - IN   - Input Control File
C         9 - IO   - Messages to the screen
C         8 - IPO  - Summary Output File
C        10 - IMET - Meteorological data
C
C             Preprocessed Intersection Dimensions, Traffic Emissions &
C                  Signalization (ETS) Output File (ASCII format)
C
C        11 - IT1 - Input link variables as read from main input file
C        12 - IT2 - Output for processed link variables
C        13 - IPLT - Output plot file.
C        15 - ILK - Output for link variable queue data
C
C *********************************************************************
C
C      =================================
C       VARIABLE DECLARATION STATEMENTS
C      =================================
C
      PARAMETER (NN = 5000)

C
      CHARACTER*40 JOB, RUN
      CHARACTER*20 LNK
      CHARACTER*10 DAYWK
      CHARACTER*9  EXIN
      CHARACTER*2  BLNK, TYP
      CHARACTER*1  MODE, STAR, STR

      REAL         IDLFAC

      INTEGER      AT, CAVG, COD, RAVG, SFR, ST

C      ========
C       ARRAYS
C      ========

      COMMON /LINK/ AT(NN), CAVG(NN), COD(NN), EFL(NN),
     +              HL(NN), IDLFAC(NN), IDOW, IQ(NN), IV(NN),
     +              JTIER, MODET, NL, NLANES(NN), IPATRY(7),
     +              RAVG(NN), SFR(NN), ST(NN),
     +              VPHL(NN), WL(NN), XL1(NN), XL2(NN),
     +              YFAC(NN), YL1(NN), YL2(NN)
      COMMON /CHR/ STAR(2,NN), BLNK, STR, JOB, RUN, EXIN
      COMMON /LNKC/ DAYWK(7), LNK(NN), MODE, TYP(NN)

      COMMON /IO/  ICTL, IN, MSGS, IMET, IT1, IT2, IPO, ILK, ISCN, IPLT

      DIMENSION AMB(24), IHR(24)

C      =====================
C       DATA INITIALIZATION
C      =====================

      MAXL = NN

C      Calculates the name of the first day of the year

         IMP = 1 + 13
         IYP = ISYR4 - 1
         J = INT(365.25 * IYP) + INT(30.6001 * IMP) + 1 + 1720982
         IDAYN = J - 1720982
         IDOW = MOD((IDAYN + 5),7)
           IF (IDOW .EQ. 0) IDOW = 7

        READ(IN, *) IPATRY

        IPMAX = MAX(IPATRY(1),IPATRY(2),IPATRY(3),IPATRY(4),IPATRY(5),
     +              IPATRY(6),IPATRY(7))
C
C      -----------------------
C       LINK & MET CONDITIONS
C      -----------------------
C
        READ(IN, *) RUN, NL

          IF (NL .GT. MAXL) THEN
            WRITE(MSGS, 168) NL, MAXL
            WRITE(ISCN, 168) NL, MAXL
            STOP
          END IF

C       READ LINK NAME, TYPE AND DIMENSIONS
        DO 30 I = 1, NL
          READ(IN,*)     COD(I), IQ(I)
          IF (IQ(I).EQ.2) THEN
            READ(IN,*) LNK(I),TYP(I), XL1(I),YL1(I),XL2(I),YL2(I),
     +                 HL(I),WL(I),NLANES(I)
           ELSE
            READ(IN,*) LNK(I),TYP(I), XL1(I),YL1(I),XL2(I),YL2(I),
     +                    HL(I),WL(I)
          END IF
   30   CONTINUE

        IF (JTIER .EQ. 2) THEN
          DO 32 I = 1, IPMAX
            DO 34 J = 1, 24
              READ(IN, *) IHR(J), AMB(J)
              WRITE(IT1,3) IHR(J), AMB(J)
              DO 36 K = 1, NL
                IF (IQ(K) .EQ. 1) THEN
                  READ(IN,*) COD(K), VPHL(K), EFL(K)
                  WRITE(IT1,164) COD(K), VPHL(K), EFL(K)
                 ELSE
                  READ(IN,*) COD(I), CAVG(I), RAVG(I),
     +                           YFAC(I), IV(I), IDLFAC(I),
     +                           SFR(I), ST(I), AT(I)
                  WRITE(IT1,4) COD(I), CAVG(I), RAVG(I),
     +                           YFAC(I), IV(I), IDLFAC(I),
     +                           SFR(I), ST(I), AT(I)
                END IF
  36          CONTINUE
  34        CONTINUE
  32      CONTINUE

         ELSE

          READ(IN, *) IHR(1), AMB(1)
          DO 38 K = 1, NL
            IF (IQ(K) .EQ. 1) THEN
              READ(IN,*) COD(K), VPHL(K), EFL(K)
             ELSE
              READ(IN,*) COD(K), CAVG(K), RAVG(K),
     +                       YFAC(K), IV(K), IDLFAC(K),
     +                       SFR(K), ST(K), AT(K)
            END IF
  38      CONTINUE

          DO 44 KK = 1, 24
            WRITE (IT1,3)  IHR(1), AMB(1)
          DO 44 K1 = 1, NL
            IF (IQ(K1) .EQ. 1) THEN
              WRITE(IT1,164) COD(K1), VPHL(K1), EFL(K1)
             ELSE
              WRITE(IT1,4) COD(K1), CAVG(K1), RAVG(K1),
     +                       YFAC(K1), IV(K1), IDLFAC(K1),
     +                       SFR(K1), ST(K1), AT(K1)
            END IF
  44      CONTINUE
        END IF

C     ============================================
C      Read diurnal patterns into scratch memeory
C     ============================================

C      Decide which pattern should be assigned to which day of the week
C        1 is for Monday, 5 is for Friday, and 7 is for Sunday.

      IF (JTIER .EQ. 2) THEN

        DO 52 K = 1, 7
          REWIND IT1
           ID = K
          J2 = IPATRY(ID)
          DO 50 JK = 1, J2
            DO 55 J = 1, 24
              READ(IT1,*) IHR(J), AMB(J)
              IF (J2 .EQ. JK) THEN
                WRITE (IT2,42) ISYR, K, IHR(J), AMB(J)
              END IF
              DO 60 I = 1, NL
                IF (IQ(I) .EQ. 2) THEN
                  READ(IT1,*) COD(I), CAVG(I), RAVG(I), YFAC(I),
     +                       IV(I), IDLFAC(I),
     +                       SFR(I), ST(I), AT(I)

                    IF (SFR(I) .EQ. 0) SFR(I) = 1600
                    IF (ST(I)  .EQ. 0) ST(I)  = 1
                    IF (AT(I)  .EQ. 0) AT(I)  = 3
                  IF (J2 .EQ. JK) THEN
                    WRITE(IT2,4) COD(I), CAVG(I), RAVG(I),
     +                           YFAC(I), IV(I), IDLFAC(I),
     +                           SFR(I), ST(I), AT(I)
                  END IF
                 ELSE
                  READ(IT1,  *) COD(I), VPHL(I), EFL(I)
                  IF (J2 .EQ. JK) THEN
                    WRITE(IT2,164) COD(I), VPHL(I), EFL(I)
                  END IF
                END IF
   60         CONTINUE
   55       CONTINUE
   50     CONTINUE
   52   CONTINUE
      END IF

C         Read one hour of data and process into 7  24-hour patterns

      IF (JTIER .EQ. 1) THEN
        J = 1
        REWIND IT1
        READ(IT1, *) IHR(J), AMB(J)
        DO 61 I = 1, NL
          IF (IQ(I) .EQ. 2) THEN
            READ(IT1,*)  COD(I), CAVG(I), RAVG(I), YFAC(I),
     +                 IV(I), IDLFAC(I),
     +                 SFR(I), ST(I), AT(I)

              IF (SFR(I) .EQ. 0) SFR(I) = 1600
              IF (ST(I)  .EQ. 0) ST(I)  = 1
              IF (AT(I)  .EQ. 0) AT(I)  = 3
           ELSE
             READ(IT1,*) COD(I), VPHL(I), EFL(I)
          END IF
   61   CONTINUE
        DO 71  K = 1,7
          DO 72 J = 1, 24
            WRITE(IT2,42) ISYR, K, J, AMB(1)
            DO 73 I = 1, NL
              IF (IQ(I) .EQ. 2) THEN
                WRITE(IT2,4) COD(I), CAVG(I), RAVG(I), YFAC(I),
     +                       IV(I), IDLFAC(I),
     +                       SFR(I), ST(I), AT(I)
               ELSE
                WRITE(IT2,164) COD(I), VPHL(I), EFL(I)
              END IF
   73       CONTINUE
   72     CONTINUE
   71   CONTINUE
      END IF

      WRITE(MSGS, *)
      WRITE(MSGS, *) '  End of ETS processing subroutine.'
      WRITE(MSGS, *)
      WRITE(ISCN, *)
      WRITE(ISCN, *) '  End of ETS processing subroutine.'
      WRITE(ISCN, *)

CC
CC       -----------------
CC       FORMAT STATEMENTS
CC       -----------------
    3 FORMAT (5X,I3,2X,F7.2)
    4 FORMAT (1X,I4,1X,2(I5,5X),F5.1,I5,F7.2,1X,I4,1X,I1,1X,I1)
   42 FORMAT (3I3,F6.2)

C  164 FORMAT (1X, I4, 2F8.1)
C          Insertion, Adesman 1/16/2003
C  164 FORMAT (1X, I4, 2F9.2)
C          Another, 6/10/2013
  164 FORMAT (1X, I4, 2F11.4)

  168 FORMAT (' NUMBER OF LINKS INPUT = ', I4, '> MAXIMUM ALLOWED',
     *        ' BY PROGRAM = ', I4, '.  PROGRAM IS TERMINATED!')

      RETURN
      END
