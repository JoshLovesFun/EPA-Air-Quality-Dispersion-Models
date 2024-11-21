c      program AERMINUTE
c**********************************************************************************
c     Purpose:  To read 2-minute average ASOS winds and calculate hourly averages for
c               substitution into AERMET 
c               AERMOD input
c     Programmer:  James Thurman US EPA, OAR/OAQPS/AQAD/AGMG
c
c     Version:  15272
c     Description:
c     This program reads the 2-minute average ASOS winds, recorded every minute
c     and uses the even minutes to calculate an hourly average wind to be substituted
c     into AERMET stage 1 and stage 3 output files    
c
c version 15272:  Incorporated the ability to read 5-minute wind files
c                 to supplement hours with no 1-minute hourly averages
c                 added subroutines readfive and read5 to read 5-minute data
c
c                 added subroutine checkwind5 to perform QA on 5-minute data records
c
c                 added subroutine sub5 to substitute missing 1-minute values
c                 with values from 5-minute data
c
c                 added variables nvalid5, neven5, nodd5, minsrc in support of 
c                 5-minute data
c
c                 modified subroutine userinp to read 5-minute data filenames
c
c                 modified subroutine userinp to allow for optional output
c                 files related to 5-minute data. These are a file listing
c                 minutes that have both 1 and 5-minute data and compares
c                 values.  The second file is a file listing the minutes
c                 where 1-minute data is substituted with 5-minute data
c
c                 modified soubroutine initial to initialize 5-minute variables
c
c                 modified subroutine checkfiles to read 1-minute and 5-minute
c                 data filenames
c
c                 added subroutine checkwban to check for non-numeric
c                 WBAN numbers for 1-minute and 5-minute data files
c
c                 modified subroutine writeout to write 5-minute summaries
c                 to summary and output file for AERMET input
c                 added 5-minute summaries
c version 14337:  Adjusted QA flags, eliminating flag 5, check on column 67
c                 and flag 10, check for non-blanks in columns 64-68
c                 to account for shifts in data in the 1-minute datafiles
c                 Renumbered flags 6-9 to 5-8 and flags 11-14 to 9-12 accordingly
c                 Adjusted flag 1 to look through columns 68-90 instead of 67-90
c                 to account for data file format changes
c version 14237:  Adjusted QA steps for flags 6 - 9
c                 to account for change in format of 6405 1-minute files
c                 Winds have been shifted to the right 1 to 2 columns
c                 Flag 6 now checks columns 70-74
c                 Flag 7 checks columns 76-79
c                 Flag 8 checks columns 82-84
c                 Flag 9 checks columns 87-89
c                 new flag 10 check column 90
c                 Flags check for non-numeric characters and if a blank
c                 is surrounded by two non-blank characters on either side
c                 added new flag, flag 11 to check for spurious numbers in
c                 columns 64-68.  if the flag is triggered record will
c                 be considered suspicous and output to check_records.dat
c                 adjust flagcounts array to size 14
c version 11325:  Adjusted QA flags 3, 4, and 5.
c                 Flag 3 now only checks column 30 for a non-blank
c                 Adjusted flag 4 to check for 4 digit integers
c                 Adjusted flag 5 to check for any non-blank character
c version 11059:  final version for release
c version 10167: made prompts based inputs into a file based input system
c since the user has to list the input files anyway    
c**********************************************************************************
      module main1
      implicit none

c DATE VARIABLES
c startmon:     Integer month of start of processing period
c startday:     Integer day of start of processing period
c startyear:    Integer 4-digit year of start of processing period
c endmon:       Integer month of end of processing period
c endday:       Integer day of end of processing period
c endyear:      Integer 4-digit year of end of processing period
c startdate:    Integer start date (YYYYMMDD)
c enddate:      Integer end date (YYYYMMDD) 
c ifwdate:      Integer date (YYYYMMDD) of date of IFW commission date
c               If no IFW commission date then ifwdate set to 99999999
c days:         Integer array of number of days per calendar month
c               February is listed at 28 days
c baddates:     2 dimensional array of minimum and maximum dates read
c               that are outside window set by start and end dates
c               the values for these dates will only be displayed
c               if the entire time period from the one minute files
c               is completely outside the window set by the start and end dates
c baddate5:     2 dimensional array of minimum and maximum dates read
c               that are outside window set by start and end dates
c               the values for these dates will only be displayed
c               if the entire time period from the 5 minute files
c               is completely outside the window set by the start and end dates
      integer startmon,startday,startyear,endmon,endday,endyear,
     +  startdate,enddate,ifwdate,days(12),baddates(2),baddate5(2)

c other integer variables
c iwban:        Integer WBAN number of station.  This number can be found in the one minute data file
c iwban2:       Integer WBAN number of station from first record of first file.  Retained through program
c               even if WBAN changes based on one-minute data files
c wbancnt:      Integer number of times WBAN changes while reading data (10 is maximum)
c statcnt:      Integer number of times station ID changes while reading data (10 is maximum)
c nmonths:      Integer number of months in processing period
c nfiles:       Integer number of one-minute data files to be read in
c nfiles5:       Integer number of five-minute data files to be read in
c nmin:         Integer month (relative to start month) of minimum one-minute wind
c daymin:       Integer day of month of minimum one-minute wind
c hrmin:        Integer hour of day of minimum one-minute wind
c               this value will be subtracted by 1 hour when displayed
c               to show the actual time the minimum speed occurred
c minmin:       Integer minute of hour of minimum one-minute wind
c minsrc1:      Integer to indicate source of minimum wind speed (1=1-minute 5=5-minute data)
c nmax:         Integer month (relative to start month) of maximum one-minute wind
c daymax:       Integer day of month of maximum one-minute wind
c hrmax:        Integer hour of day of maximum one-minute wind
c               this value will be subtracted by 1 hour when displayed
c               to show the actual time the maximum speed occurred
c minmax:       Integer minute of hour of maximum one-minute wind
c maxsrc1:      Integer to indicate source of maximum wind speed (1=1-minute 5=5-minute data)
c nhrmin:       Integer month (relative to startmonth) of minimum hourly averaged wind speed
c dayhrmin:     Integer day (relative to startmonth) of minimum hourly averaged wind speed
c hrhrmin:      Integer hour of day of minimum hourly averaged wind speed
c nhrmax:       Integer month (relative to startmonth) of maximum hourly averaged wind speed
c dayhrmax:     Integer day (relative to startmonth) of maximum hourly averaged wind speed
c hrhrmax:      Integer hour of day of maximum hourly averaged wind speed
c itothrs:      Integer number of total calendar hours in data period
c               regardless if all hours in one-minute data
c               i.e. if processing a year, itothrs=8760 even if there 
c               are only 7000 hours of one minute data
c irec:         Integer record counter used for station and WBAN comparison
c flagcounts:   Integer array of flag counts for each QA flag in checkwinds
c               subscripts 1-11 refer to first 9 flags in checkwind
c               subscript 12 is for suspicious records
c               subscript 13 is for bad records
c               added in version 09295  
c               adjusted in version 14237 and 14337
c reccounts     integer array of record counts (added version 09295)
c               subscript 1 = good records (processed records)
c               subscript 2 = records outside data period
c               subscript 3 = records inside data period
c               subscript 4 = records for minute 1 of the hour
c               subscript 5 = bad records (fail QA check completely)
c               subscript 6 = suspicious records
c               subscript 7 = records without a minute (should always be zero)
c wbans         Array of WBAN numbers that change while reading data
c nsurf:        Integer number of surface ISHD files to read to compare
c               one minute data

      integer iwban,iwban2,wbancnt,statcnt,nmonths,nfiles,nmin,daymin,
     +  hrmin,minmin,nmax,daymax,hrmax,minmax,nhrmin,dayhrmin,hrhrmin,
     +  nhrmax,dayhrmax,hrhrmax,itothrs,irec,flagcounts(12),
     +  reccounts(7),wbans(10),nsurf,nfiles5,minsrc1,maxsrc1

c file units
c mainunit:     Integer input file unite of main input control file, aerminute.inp (24)
c inunit:       Integer input file unit (12) of one-minute data file(s)
c inunit5:      Integer input file unit (24) of 5-minute data file(s)
c ioutunit:     Integer output file unit (16) of hourly averaged winds
c ierrunit:     Integer unit (17) of error file.  This file contains the 
c               bad records read from the one minute data.
c ilogunit:     Integer unit (20) of log file of one minute processing
c               These are records that do not fit the format of the files
c isumunit:     Integer unit (21) of summary file. This file contains the
c               number of minutes read, total calms, even minutes, even calms,
c               odd minutes read, odd calm minutes read, odd minutes used, and
c               odd calm minutes used for each hour
c igdunit:      Integer unit (22) of good records file.  This file contains the records
c               that are read into the wind variables' arrays.  
c isusunit:     Integer unit (23) of suspicious records files.  This file contains the
c               records that are possible viable records but should be checked
c ierrunit5:    Integer unit (25) of error file for 5-minute data.  This file contains the 
c               bad records read from the 5- minute data.
c igdunit5:     Integer unit (26) of good records file for 5-minute data. This file contains the records
c               that are potentially substituted into the wind variables' arrays. 
c iclmvar5:     Integer unit (27) of 5-minute calm and variable winds.
c icomp5:       Integer unit (28) comparing 1-minute and 5-minute winds for same minute
c isubumit:     Integer unit (29) listing substituted minutes
      integer mainunit,inunit,ioutunit,ierrunit,ilogunit,isumunit,
     + igdunit,isusunit,inunit5,igdunit5,ierrunit5,iclmvar5,icomp5,
     + isubunit

c real variables
c dtr:          Conversion factor for degrees to radians (pi/180 degrees)
c ht:           Aneomemeter height input by user.
c trunc:        Factor to account for truncation of wind speeds by ASOS processing
c               Currently, 1/2 knot added to account that winds are truncated by
c               ASOS when calculating the 2-minute average of 5 second speeds
c               Version 09239, dated 8/27/09 truncation factor set to 0
c               Truncation account will take place in AERMET
c minonemin:    Minimum one minute wind speed
c maxonemin:    Maximum one minute wind speed

      real d2r,ht,trunc,minonemin,maxonemin
 
c character    
c versn:        6 character string of one minute program version number
c stat:         4 character string of station ID read from files
c stat2:        4 character string of station ID read from first record of first file
c amonths:      Array of month names
c inpfile:      Name of text file containing the names of the data files
c infiles:      Character name of array of input one minute files
c infiles5:      Character name of array of input five minute files
c outfil:       Name of output hourly averaged winds file
c errfil:       Name of bad records file (bad_records.dat)
c logfil:       Name of log file (areminute.log)
c sumfil:       Name of summary file (minute_counts.dat)
c goodfil:      Name of good records file (good_records.dat)
c suspfil:      Name of file with suspicious records (check_records.dat)
c origdate:     4 dimensional array of dates (YYYYMMDD) dimension nmonths x 31 days x 24 hours x 60 mins
c origdate5:    4 dimensional array of dates (YYYYMMDD) for 5-minute data dimension 
c               nmonths x 31 days x 24 hours x 12 mins
c stats:        Array of character string of station IDs if station IDs change while reading data
c               can only be upto 10 changes
c compfil:      Name of file comparing one minute data against standard observations
c obfiles:      Array of filenames of ISHD standard observation data
c flag:         4 dimensional array flag denoting if standard observation is 
c               calm, valid, variable, or missing
c               dimension nmonths x 31 days x 24 hours x 60 mins
c qflag:         4 dimensional array flag denoting QC code of standard observation
c               dimension nmonths x 31 days x 24 hours x 60 mins 
c goodfil5:     Name of good records file for 5-minute data (good_records_5.dat)
c errfil5:       Name of bad records file for 5-minute data (bad_records_5.dat)
c clmvar5:      Name of calm/variable records file for 5-minute data(calm_variable_records_5.dat)
c comp5fil:     Name of 1-minute and 5-minute comparison file 
c sub5fil:      Name of file listing substituted minutes
      character versn*6,stat*4,stat2*4,amonths(12)*10,inpfile*90,
     +  infiles*250,outfil*90,errfil*90,logfil*90,sumfil*90,goodfil*90,
     +  suspfil*90,origdate*12,origdate5*12,stats(10)*4,compfil*250,
     +  obfiles*250,flag*1,qcflag*1,infiles5*250,goodfil5*90,errfil5*90,
     +  clmvar5*90,comp5fil*90,sub5fil*90
     
c logical variable
c ifw:        Denotes if time period is part of IFW group
c               ifw = .true. IFW commission date is in time period or before time period
c               ifw = .false. IFW commissions date is after time period or unknown
c lwban:        Denotes if WBAN number from first record of first file did not match WBAN from other file(s)
c               lwban = .true. numbers did not match, change WBAN to number from file
c               lwban = .false. numbers matched, no change
c lstat         Denotes if call sign of station differs in files
c               lstat=.true. station ids do not match
c               lstat=.false. no change
c badwban       Denotes if WBAN is not integer, stop program if true
c lkeep1:       logical variable denoting if all 1-minute files are present
c lkeep5:       logical variable denoting if all 5-minute files are present
c lbad1:        logical variable denoting a 1-minute file has a bad filename
c lbad5:        logical variable denoting a 5-minute file has a bad filename
c writesum:     logical variable denoting if AERMINUTE is to write a summary
c               file (SUMMFILE) containing statistics about each hour in the data period
c comp:         logical variable denoting to compare standard observations against
c               one minute data
c ldup:         logical variable denonting that duplicate filenames in the one minute
c               data file list have been detected
c ldup5:         logical variable denonting that duplicate filenames in the 5 minute
c               data file list have been detected
c               true=duplicates found, stop program, false=no duplicates found
c ldupob:       logical variable denonting that duplicate filenames in the standard observation
c               data file list have been detected 
c               true=duplicates found, stop program, false=no duplicates found        
c lgo:          logical variable denoting if any problems found with standard observations
c               true=okay,false=stop program  
c lnodata:      logical variable denoting that there is data within window set by 
c               start and end dates. if lnodata is true, then there is no data that was
c               read from one minute files that is within the window and program will abort
c               if lnodata is false, program will proceeed
c lnodata5:     logical variable denoting that there is 5 minute data within window set by 
c               start and end dates. if lnodata5 is true, then there is no data that was
c               read from fiv3 minute files that is within the window and program will not
c               process 5-minute data
c               if lnodata5 is false, program will proceeed
c lfive         logical variable denoting that 5-minute data will be used
c run5          logical variable denoting if 5-minute data will be procesed
c lno1dat       logical variable denoting that the one-minute data section
c               was not found in the input file and 5-minute data was found         
c writesub5     logical variable denoting to write optional 5-minute substitution
c               file
c comp15        logical variable denoting to write optional 1-minute and 5-minute
c               comparison file.
      logical ifw,lwban,lstat,lkeep1,lbad1,writesum,comp,ldup,ldupob,
     + ldup5,lgo,lnodata,lfive,lkeep5,lbad5,run5,lnodata5,badwban,
     +lno1dat,writesub5,comp15
     
c set values for file units, d2r, and trunc variables
      parameter(mainunit=12,inunit=12,ioutunit=16,ierrunit=17,
     +  ilogunit=20,isumunit=21,igdunit=22,isusunit=23,inunit5=24,
     +  ierrunit5=25,igdunit5=26,iclmvar5=27,icomp5=28,
     +  isubunit=29,d2r=3.141592654/180.0,trunc=0.0)

c assign number of days per month.  February set to 28
c                 J  F  M  A  M  J  J  A S  O  N  D
      data days /31,28,31,30,31,30,31,31,30,31,30,31/
      
c set values for version #, error file, log file, and summary file
!      parameter(versn='11269',errfil='bad_records.dat',
!      parameter(versn='11325',errfil='bad_records.dat',
!      parameter(versn='14237',errfil='bad_records.dat',
!      parameter(versn='14337',errfil='bad_records.dat',
      parameter(versn='15272',errfil='bad_records.dat',
     +  logfil='aerminute.log',goodfil='good_records.dat',
     +  suspfil='check_records.dat',goodfil5='good_records_5.dat',
     + errfil5='bad_records_5.dat',clmvar5='calm_variable_records.dat')
     
   
c allocatable arrays
c ndays:        1 dimensional array of number of days for a month dimension nmonths
c idate:        2 dimensional array of dates (YYYYMMDD) dimension nmonths x 31 days
c               these are the original dates of data before adding 1 hour to the date
c ikeephr:      3 dimensional array of integer flags denoting whether hourly wind 
c               is to be calculated.  Dimension:  nmonths x 31 days x 24 hours
c               Values: 1 = valid hour and calculate average
c                       2 = hour was read by program but does not contain at least one
c                           non-calm minute in each 1/2 of the hour
c                       9 = hour was not in one minute data files
c               hourly averages not calculated for hours with values of -2 and -9
c ikeep:        4 dimensional array of integer flags denoting how one-minute wind is to be
c               processed. Dimension nmonths x 31 days x 24 hours x 60 minutes
c               Values: 0 = calm wind speed (0 or 1 knot for non-ifw aneomemeter)
c                       1 = non-calm wind speed ( 2 knots for non-ifw aneomemeter,
c                           any value for ifw anemometer
c                       9 = minute was not in data file or bad record
c ikeep5:       4 dimensional array of integer flags denoting how 5-minute wind is to be
c               processed. Dimension nmonths x 31 days x 24 hours x 12 minutes
c               Values: 0 = calm wind speed (0 or 1 knot for non-ifw aneomemeter)
c                       1 = non-calm wind speed ( 2 knots for non-ifw aneomemeter,
c                           any value for ifw anemometer
c                       9 = minute was not in data file or bad record
c minsrc:       4 dimensional array of integer flags denoting source of 1-minute
c               data. 1=1-minute file, 5=5-minute file
c icalm:        4 dimensional array of integer flags denoting if minute is calm.
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c               Values:  1 = calm, 0 = non-calm
c icalm5:       4 dimensional array of integer flags denoting if 5-minute is calm.
c               Dimension nmonths x 31 days x 24 hours x 12 minutes
c               Values:  1 = calm, 0 = non-calm
c idirmin:      4 dimensional array of integer wind directions for each minute
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c idirmin5:     4 dimensional array of integer wind directions for each 5-minute
c               Dimension nmonths x 31 days x 24 hours x 12 minutes
c nvalid:       3 dimensional array of integer count of number of valid minutes
c               read from data files for each hour (max of 59)
c               Dimension nmonths x 31 days x 24 hours
c nvalid5:      3 dimensional array of integer count of number of 5-minute valid 
c               minutes substituted for each hour (max of 12)
c               Dimension nmonths x 31 days x 24 hours
c ncalms:       3 dimensional array of integer count of total calms by hour
c               Dimension nmonths x 31 days x 24 hours
c neven:        3 dimensional array of integer count of total even minutes by hour
c               Dimension nmonths x 31 days x 24 hours
c neven5:       3 dimensional array of integer count of total even 5-minutes substituted 
c               by hour
c               Dimension nmonths x 31 days x 24 hours
c nevencalm:    3 dimensional array of integer count of total even calm minutes by hour
c               Dimension nmonths x 31 days x 24 hours
c noddperm:     3 dimensional array of integer count of total odd minutes by hour
c               Dimension nmonths x 31 days x 24 hours
c noddpermc:    3 dimensional array of integer count of total odd calm minutes by hour
c               Dimension nmonths x 31 days x 24 hours
c nodd5:        3 dimensional array of integer count of total odd calm 5-minutes substituted
c               by hour
c               Dimension nmonths x 31 days x 24 hours
c nodd:         3 dimensional array of integer count of total odd minutes used by hour
c               Dimension nmonths x 31 days x 24 hours
c noddcalm:     3 dimensional array of integer count of total odd calm minutes used by hour
c               Dimension nmonths x 31 days x 24 hours
c dirmin:       3 dimensional array of minimum 2-minute wind direction of each hour in m/s
c               Dimension nmonths x 31 days x 24 hours
c dirmax:       3 dimensional array of maximum 2-minute wind direction of each hour in m/s
c               Dimension nmonths x 31 days x 24 hours
c nhours:       2 dimensional array of # of hours per month.  1st dimension is # of months
c               and second dimension has 5 values: # of total hours, # valid hours, # invalid hours,
c               # calm hours, and # of missing hours per month
c               version 15272, added 6 column to include # of 5-minute hours used
c srcmin        4 dimensional array of minimum 2-minute wind speed/direction of each hour
c               Dimension 2 x nmonths x 31 days x 24 hours (1=speed,2=direction)
c srcmax        4 dimensional array of maximum 2-minute wind speed/direction of each hour
c               Dimension 2 x nmonths x 31 days x 24 hours (1=speed,2=direction)   
      integer, allocatable, dimension(:) :: ndays
      integer, allocatable, dimension(:,:) :: idate
      integer, allocatable, dimension(:,:,:) :: ikeephr
      integer, allocatable, dimension(:,:,:,:) :: ikeep
      integer, allocatable, dimension(:,:,:,:) :: icalm
      integer, allocatable, dimension(:,:,:,:) :: idirmin
      integer, allocatable, dimension(:,:,:) :: nvalid
      integer, allocatable, dimension(:,:,:) :: ncalms
      integer, allocatable, dimension(:,:,:) :: neven
      integer, allocatable, dimension(:,:,:) :: nevencalm
      integer, allocatable, dimension(:,:,:) :: noddperm
      integer, allocatable, dimension(:,:,:) :: noddpermc
      integer, allocatable, dimension(:,:,:) :: nodd
      integer, allocatable, dimension(:,:,:) :: noddcalm
      integer, allocatable, dimension(:,:,:) :: dirmin
      integer, allocatable, dimension(:,:,:) :: dirmax
      integer, allocatable, dimension(:,:) :: nhours
c     added 15272
      integer, allocatable, dimension(:,:,:,:) :: minsrc
      integer, allocatable, dimension(:,:,:) :: nvalid5
      integer, allocatable, dimension(:,:,:) :: neven5
      integer, allocatable, dimension(:,:,:) :: nodd5
      integer, allocatable, dimension(:,:,:,:) :: srcmin
      integer, allocatable, dimension(:,:,:,:) :: srcmax
c     152725-minute variables (remove certain variables)
      
      integer, allocatable, dimension(:,:,:,:) :: ikeep5
      integer, allocatable, dimension(:,:,:,:) :: icalm5
      integer, allocatable, dimension(:,:,:,:) :: idir5min
!      integer, allocatable, dimension(:,:,:) :: nvalid5
c real allocatable
c speed:        3 dimensional array of real hourly averaged wind speeds (m/s)
c               Dimension nmonths x 31 days x 24 hours
c speed1:       4 dimensional array of real one-minute wind speeds (m/s) read from one minute files
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c speed5:       4 dimensional array of real one-minute wind speeds (m/s) read from 5-minute files
c               Dimension nmonths x 31 days x 24 hours x 12 minutes
c speedkts:    4 dimensional array of real one-minute wind speeds (knots) read from one minute files
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c speed5kts:    4 dimensional array of real one-minute wind speeds (knots) read from 5 minute files
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c xdir:         4 dimensional array of real one-minute wind x-component of the one minute wind direction
c               in radians.  Dimension nmonths x 31 days x 24 hours x 60 minutes
c ydir:         4 dimensional array of real one-minute wind y-component of the one minute wind direction
c               in radians.  Dimension nmonths x 31 days x 24 hours x 60 minutes
c dirminut:     4 dimensional array of real one-minute wind wind direction in degrees
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c xdir5:        4 dimensional array of real 5-minute wind x-component of the one minute wind direction
c               in radians.  Dimension nmonths x 31 days x 24 hours x 12 minutes
c ydir5:        4 dimensional array of real 5-minute wind y-component of the one minute wind direction
c               in radians.  Dimension nmonths x 31 days x 24 hours x 12 minutes
c dir5minut:    4 dimensional array of real 5-minute wind wind direction in degrees
c               Dimension nmonths x 31 days x 24 hours x 12 minutes
c xdirhr:       3 dimensional array of sum of one minute x-components of wind direction
c               Dimension nmonths x 31 days x 24 hours
c ydirhr:       3 dimensional array of sum of one minute y-components of wind direction
c               Dimension nmonths x 31 days x 24 hours
c dirhr:        3 dimensional array of hourly averaged wind direction in degrees
c               Dimension nmonths x 31 days x 24 hours
c speedmin:     3 dimensional array of minimum 2-minute wind speed of each hour in m/s
c               Dimension nmonths x 31 days x 24 hours
c speedmax:     3 dimensional array of maximum 2-minute wind speed of each hour in m/s
c               Dimension nmonths x 31 days x 24 hours
c obspd:        4 dimensional array of real standard observation wind speeds (m/s) read from standard ob files
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
c obdir:        4 dimensional array of real standard observation wind directions read from standard ob files
c               Dimension nmonths x 31 days x 24 hours x 60 minutes
      real, allocatable, dimension(:,:,:) :: speed
      real, allocatable, dimension(:,:,:,:) :: speed1
      real, allocatable, dimension(:,:,:,:) :: speedkts
      real, allocatable, dimension(:,:,:,:) :: xdir
      real, allocatable, dimension(:,:,:,:) :: ydir
      real, allocatable, dimension(:,:,:,:) :: dirminut
      real, allocatable, dimension(:,:,:) :: xdirhr
      real, allocatable, dimension(:,:,:) :: ydirhr
      real, allocatable, dimension(:,:,:) :: dirhr
      real, allocatable, dimension(:,:,:) :: speedmin
      real, allocatable, dimension(:,:,:) :: speedmax
      real, allocatable, dimension(:,:,:,:) :: obspd
      real, allocatable, dimension(:,:,:,:) :: obdir      

c version 152725-minute variables

      real, allocatable, dimension(:,:,:,:) :: speed5
      real, allocatable, dimension(:,:,:,:) :: xdir5
      real, allocatable, dimension(:,:,:,:) :: ydir5
      real, allocatable, dimension(:,:,:,:) :: dir5minut
      real, allocatable, dimension(:,:,:,:) :: speed5kts
c allocatable statement for various character arrays.    
      allocatable :: infiles(:)
      allocatable :: obfiles(:)
      allocatable :: origdate(:,:,:,:)      
      allocatable :: flag(:,:,:,:)
      allocatable :: qcflag(:,:,:,:)  

c version 15272
      allocatable :: infiles5(:)
      allocatable :: origdate5(:,:,:,:)
      integer iminr,irnd(12,24,12)
c irnd:       integer values of random numbers to generate 5-minute
c             wind directions.  array size is 12 months x 24 hours x 12 minutes
c             each day of a month gets the same values.
      DATA (IRND( 1, 1,iminr),iminr=1,12)/7,0,0,8,6,0,5,8,5,0,3,4/
      DATA (IRND( 1, 2,iminr),iminr=1,12)/4,8,0,1,7,8,6,1,2,1,3,3/
      DATA (IRND( 1, 3,iminr),iminr=1,12)/6,6,5,3,6,2,7,1,5,2,4,8/
      DATA (IRND( 1, 4,iminr),iminr=1,12)/8,6,2,7,3,6,6,8,3,7,3,4/
      DATA (IRND( 1, 5,iminr),iminr=1,12)/2,4,8,1,2,0,7,7,6,8,4,0/
      DATA (IRND( 1, 6,iminr),iminr=1,12)/6,3,1,4,3,1,7,0,0,4,4,5/
      DATA (IRND( 1, 7,iminr),iminr=1,12)/1,5,8,8,7,8,7,8,3,3,8,4/
      DATA (IRND( 1, 8,iminr),iminr=1,12)/5,8,7,2,3,5,8,7,6,2,2,4/
      DATA (IRND( 1, 9,iminr),iminr=1,12)/0,1,6,6,8,2,8,6,0,1,5,4/
      DATA (IRND( 1,10,iminr),iminr=1,12)/4,6,5,2,1,1,6,3,5,4,6,4/
      DATA (IRND( 1,11,iminr),iminr=1,12)/8,0,4,5,6,7,6,1,8,3,1,4/
      DATA (IRND( 1,12,iminr),iminr=1,12)/3,2,2,0,1,4,6,0,2,2,4,4/
      DATA (IRND( 1,13,iminr),iminr=1,12)/7,5,1,3,6,2,6,8,5,1,7,3/
      DATA (IRND( 1,14,iminr),iminr=1,12)/2,8,0,7,2,8,7,7,8,0,2,3/
      DATA (IRND( 1,15,iminr),iminr=1,12)/6,3,8,3,4,7,5,4,5,3,3,4/
      DATA (IRND( 1,16,iminr),iminr=1,12)/1,6,7,6,0,4,5,3,7,2,6,3/
      DATA (IRND( 1,17,iminr),iminr=1,12)/5,0,5,1,5,1,5,2,1,1,0,3/
      DATA (IRND( 1,18,iminr),iminr=1,12)/0,2,4,4,0,8,5,1,4,0,4,3/
      DATA (IRND( 1,19,iminr),iminr=1,12)/4,7,3,0,2,6,3,7,1,3,5,3/
      DATA (IRND( 1,20,iminr),iminr=1,12)/8,1,2,4,7,4,4,6,4,2,8,3/
      DATA (IRND( 1,21,iminr),iminr=1,12)/3,3,1,7,3,1,4,5,6,1,2,3/
      DATA (IRND( 1,22,iminr),iminr=1,12)/7,6,8,2,8,7,4,4,0,1,6,2/
      DATA (IRND( 1,23,iminr),iminr=1,12)/2,2,7,7,1,6,2,0,6,3,7,3/
      DATA (IRND( 1,24,iminr),iminr=1,12)/6,4,6,1,5,3,2,8,0,2,1,3/
      DATA (IRND( 2, 1,iminr),iminr=1,12)/1,7,5,5,1,1,3,7,3,1,5,2/
      DATA (IRND( 2, 2,iminr),iminr=1,12)/5,0,4,8,6,7,3,6,6,1,8,2/
      DATA (IRND( 2, 3,iminr),iminr=1,12)/0,5,3,4,8,6,1,3,2,3,0,3/
      DATA (IRND( 2, 4,iminr),iminr=1,12)/4,8,1,8,4,3,1,2,5,2,3,2/
      DATA (IRND( 2, 5,iminr),iminr=1,12)/8,1,0,2,0,0,1,1,8,1,7,2/
      DATA (IRND( 2, 6,iminr),iminr=1,12)/3,4,8,6,4,7,1,0,2,1,1,2/
      DATA (IRND( 2, 7,iminr),iminr=1,12)/7,0,7,1,6,5,8,6,8,3,2,2/
      DATA (IRND( 2, 8,iminr),iminr=1,12)/2,2,6,5,2,3,0,5,2,2,5,2/
      DATA (IRND( 2, 9,iminr),iminr=1,12)/6,5,4,0,7,0,0,4,4,1,0,2/
      DATA (IRND( 2,10,iminr),iminr=1,12)/1,8,3,3,3,6,0,2,7,1,3,1/
      DATA (IRND( 2,11,iminr),iminr=1,12)/5,3,2,8,5,5,7,8,4,3,4,2/
      DATA (IRND( 2,12,iminr),iminr=1,12)/0,6,1,2,0,2,7,7,7,2,8,2/
      DATA (IRND( 2,13,iminr),iminr=1,12)/5,8,0,6,5,0,8,6,1,2,2,1/
      DATA (IRND( 2,14,iminr),iminr=1,12)/0,2,7,0,1,6,8,5,4,1,5,1/
      DATA (IRND( 2,15,iminr),iminr=1,12)/3,3,2,4,1,0,1,0,8,8,8,2/
      DATA (IRND( 2,16,iminr),iminr=1,12)/0,3,4,4,7,0,1,3,6,0,2,8/
      DATA (IRND( 2,17,iminr),iminr=1,12)/2,3,1,8,4,5,0,1,4,4,1,4/
      DATA (IRND( 2,18,iminr),iminr=1,12)/5,1,6,2,4,8,1,1,7,5,3,0/
      DATA (IRND( 2,19,iminr),iminr=1,12)/7,7,1,6,7,0,3,8,0,6,0,7/
      DATA (IRND( 2,20,iminr),iminr=1,12)/1,5,7,0,7,3,5,8,4,7,1,2/
      DATA (IRND( 2,21,iminr),iminr=1,12)/3,5,3,4,3,8,4,6,1,2,1,8/
      DATA (IRND( 2,22,iminr),iminr=1,12)/5,3,0,7,3,2,5,5,5,3,2,3/
      DATA (IRND( 2,23,iminr),iminr=1,12)/8,1,4,2,6,4,7,4,7,4,8,1/
      DATA (IRND( 2,24,iminr),iminr=1,12)/1,8,0,4,6,7,8,3,2,5,1,6/
      DATA (IRND( 3, 1,iminr),iminr=1,12)/4,8,6,8,2,2,7,1,8,0,0,2/
      DATA (IRND( 3, 2,iminr),iminr=1,12)/6,6,3,2,2,5,0,1,2,1,1,7/
      DATA (IRND( 3, 3,iminr),iminr=1,12)/0,4,6,6,5,7,2,8,5,2,8,5/
      DATA (IRND( 3, 4,iminr),iminr=1,12)/2,2,3,0,5,1,3,8,8,3,0,0/
      DATA (IRND( 3, 5,iminr),iminr=1,12)/4,2,8,4,2,5,2,6,6,7,8,6/
      DATA (IRND( 3, 6,iminr),iminr=1,12)/7,0,5,7,1,8,3,5,0,8,1,2/
      DATA (IRND( 3, 7,iminr),iminr=1,12)/0,7,0,2,4,1,6,4,3,0,7,8/
      DATA (IRND( 3, 8,iminr),iminr=1,12)/3,5,5,5,4,4,7,4,6,1,8,4/
      DATA (IRND( 3, 9,iminr),iminr=1,12)/5,5,2,0,1,8,6,1,3,5,8,1/
      DATA (IRND( 3,10,iminr),iminr=1,12)/8,2,8,3,0,2,7,1,7,6,0,5/
      DATA (IRND( 3,11,iminr),iminr=1,12)/1,0,2,7,3,4,1,8,0,7,6,3/
      DATA (IRND( 3,12,iminr),iminr=1,12)/3,7,8,0,3,7,2,8,4,8,8,8/
      DATA (IRND( 3,13,iminr),iminr=1,12)/6,7,5,4,0,3,1,6,1,3,7,4/
      DATA (IRND( 3,14,iminr),iminr=1,12)/8,5,1,7,8,6,2,5,4,4,8,0/
      DATA (IRND( 3,15,iminr),iminr=1,12)/2,3,5,2,2,7,4,4,7,5,6,7/
      DATA (IRND( 3,16,iminr),iminr=1,12)/4,1,1,5,2,1,5,4,1,6,7,2/
      DATA (IRND( 3,17,iminr),iminr=1,12)/7,1,7,0,8,6,4,1,8,1,6,8/
      DATA (IRND( 3,18,iminr),iminr=1,12)/0,8,4,3,7,0,6,1,2,3,8,3/
      DATA (IRND( 3,19,iminr),iminr=1,12)/3,6,7,7,2,1,8,8,5,3,5,1/
      DATA (IRND( 3,20,iminr),iminr=1,12)/5,4,4,1,1,4,0,8,8,4,7,6/
      DATA (IRND( 3,21,iminr),iminr=1,12)/7,4,1,5,7,0,8,6,6,0,6,2/
      DATA (IRND( 3,22,iminr),iminr=1,12)/1,2,6,8,6,3,0,5,0,1,7,7/
      DATA (IRND( 3,23,iminr),iminr=1,12)/8,5,6,7,5,2,8,4,8,7,7,0/
      DATA (IRND( 3,24,iminr),iminr=1,12)/3,8,5,2,1,0,8,3,1,6,1,0/
      DATA (IRND( 4, 1,iminr),iminr=1,12)/0,4,8,7,1,8,7,0,4,4,0,0/
      DATA (IRND( 4, 2,iminr),iminr=1,12)/4,7,7,1,6,5,7,8,6,3,3,0/
      DATA (IRND( 4, 3,iminr),iminr=1,12)/8,3,6,6,8,4,5,4,3,6,4,0/
      DATA (IRND( 4, 4,iminr),iminr=1,12)/3,5,5,1,4,1,6,3,6,5,7,0/
      DATA (IRND( 4, 5,iminr),iminr=1,12)/8,8,3,4,0,8,6,2,0,4,2,0/
      DATA (IRND( 4, 6,iminr),iminr=1,12)/5,4,7,1,0,7,5,7,2,2,0,0/
      DATA (IRND( 4, 7,iminr),iminr=1,12)/0,0,6,5,2,6,3,4,8,5,1,0/
      DATA (IRND( 4, 8,iminr),iminr=1,12)/4,3,4,0,7,3,3,3,2,4,4,0/
      DATA (IRND( 4, 9,iminr),iminr=1,12)/8,5,3,3,2,0,3,2,5,3,8,0/
      DATA (IRND( 4,10,iminr),iminr=1,12)/3,8,2,7,7,7,4,1,7,2,2,8/
      DATA (IRND( 4,11,iminr),iminr=1,12)/7,4,1,3,0,5,2,7,4,5,3,0/
      DATA (IRND( 4,12,iminr),iminr=1,12)/2,6,0,6,5,3,2,6,7,4,6,0/
      DATA (IRND( 4,13,iminr),iminr=1,12)/6,0,7,1,1,0,2,5,1,3,1,8/
      DATA (IRND( 4,14,iminr),iminr=1,12)/1,2,6,4,6,6,2,4,4,2,4,8/
      DATA (IRND( 4,15,iminr),iminr=1,12)/5,7,5,0,8,5,0,0,1,5,5,0/
      DATA (IRND( 4,16,iminr),iminr=1,12)/0,1,4,4,3,2,1,8,3,4,8,8/
      DATA (IRND( 4,17,iminr),iminr=1,12)/4,3,3,7,8,0,1,7,6,3,3,8/
      DATA (IRND( 4,18,iminr),iminr=1,12)/8,6,1,2,4,6,1,6,0,2,6,8/
      DATA (IRND( 4,19,iminr),iminr=1,12)/3,2,0,7,6,5,8,3,6,5,7,8/
      DATA (IRND( 4,20,iminr),iminr=1,12)/7,4,8,1,2,2,8,2,0,4,2,8/
      DATA (IRND( 4,21,iminr),iminr=1,12)/2,7,7,5,7,8,8,1,3,3,5,8/
      DATA (IRND( 4,22,iminr),iminr=1,12)/6,1,6,8,2,6,0,0,5,2,8,7/
      DATA (IRND( 4,23,iminr),iminr=1,12)/1,5,4,4,4,4,7,6,2,5,0,8/
      DATA (IRND( 4,24,iminr),iminr=1,12)/5,8,3,7,0,2,7,5,5,4,4,8/
      DATA (IRND( 5, 1,iminr),iminr=1,12)/0,2,2,2,5,8,7,4,8,3,7,7/
      DATA (IRND( 5, 2,iminr),iminr=1,12)/4,4,1,6,1,5,7,3,2,2,1,7/
      DATA (IRND( 5, 3,iminr),iminr=1,12)/8,0,0,1,3,4,5,8,8,5,2,8/
      DATA (IRND( 5, 4,iminr),iminr=1,12)/3,2,8,5,7,1,6,7,1,4,6,7/
      DATA (IRND( 5, 5,iminr),iminr=1,12)/7,5,6,8,3,8,6,6,4,3,0,7/
      DATA (IRND( 5, 6,iminr),iminr=1,12)/2,8,5,3,8,5,6,5,7,2,3,7/
      DATA (IRND( 5, 7,iminr),iminr=1,12)/6,6,6,2,1,4,8,6,6,0,4,7/
      DATA (IRND( 5, 8,iminr),iminr=1,12)/0,4,2,5,1,7,0,5,0,1,5,2/
      DATA (IRND( 5, 9,iminr),iminr=1,12)/2,4,8,0,6,2,8,3,7,5,4,8/
      DATA (IRND( 5,10,iminr),iminr=1,12)/4,2,5,3,6,5,0,3,1,6,6,4/
      DATA (IRND( 5,11,iminr),iminr=1,12)/7,0,8,7,0,7,3,1,4,7,3,1/
      DATA (IRND( 5,12,iminr),iminr=1,12)/0,7,5,0,0,1,4,1,7,8,4,6/
      DATA (IRND( 5,13,iminr),iminr=1,12)/3,7,2,4,5,5,3,7,4,3,4,3/
      DATA (IRND( 5,14,iminr),iminr=1,12)/5,5,7,7,5,8,4,7,8,4,5,7/
      DATA (IRND( 5,15,iminr),iminr=1,12)/8,2,2,2,8,1,7,6,1,5,2,5/
      DATA (IRND( 5,16,iminr),iminr=1,12)/1,0,7,5,8,4,8,5,5,6,4,1/
      DATA (IRND( 5,17,iminr),iminr=1,12)/3,0,4,0,5,8,7,3,2,1,3,6/
      DATA (IRND( 5,18,iminr),iminr=1,12)/6,7,1,3,4,2,8,3,6,2,4,2/
      DATA (IRND( 5,19,iminr),iminr=1,12)/8,5,4,7,7,4,1,1,8,3,2,0/
      DATA (IRND( 5,20,iminr),iminr=1,12)/2,3,1,1,7,7,3,1,2,4,3,4/
      DATA (IRND( 5,21,iminr),iminr=1,12)/7,4,3,2,1,0,0,2,4,8,4,2/
      DATA (IRND( 5,22,iminr),iminr=1,12)/1,2,0,5,1,3,1,2,7,0,5,6/
      DATA (IRND( 5,23,iminr),iminr=1,12)/3,0,3,0,4,5,4,0,1,1,3,4/
      DATA (IRND( 5,24,iminr),iminr=1,12)/6,7,0,3,4,8,5,0,4,2,4,0/
      DATA (IRND( 6, 1,iminr),iminr=1,12)/8,7,6,7,0,3,4,7,1,6,3,5/
      DATA (IRND( 6, 2,iminr),iminr=1,12)/1,5,2,1,0,6,5,7,5,8,5,1/
      DATA (IRND( 6, 3,iminr),iminr=1,12)/4,3,6,5,3,8,8,5,7,8,2,8/
      DATA (IRND( 6, 4,iminr),iminr=1,12)/6,1,3,7,3,2,0,5,2,0,4,3/
      DATA (IRND( 6, 5,iminr),iminr=1,12)/0,8,8,1,2,5,1,5,5,1,5,8/
      DATA (IRND( 6, 6,iminr),iminr=1,12)/2,8,5,5,8,1,0,2,3,6,4,5/
      DATA (IRND( 6, 7,iminr),iminr=1,12)/5,6,1,8,8,4,1,2,6,7,5,0/
      DATA (IRND( 6, 8,iminr),iminr=1,12)/7,4,5,3,2,5,4,0,0,7,3,7/
      DATA (IRND( 6, 9,iminr),iminr=1,12)/0,2,2,6,1,8,5,0,3,8,4,3/
      DATA (IRND( 6,10,iminr),iminr=1,12)/3,2,7,1,7,4,4,7,0,4,4,8/
      DATA (IRND( 6,11,iminr),iminr=1,12)/5,0,4,4,7,7,5,6,4,5,5,4/
      DATA (IRND( 6,12,iminr),iminr=1,12)/8,6,8,8,1,8,7,5,6,5,2,2/
      DATA (IRND( 6,13,iminr),iminr=1,12)/1,4,4,2,0,2,0,5,1,6,4,6/
      DATA (IRND( 6,14,iminr),iminr=1,12)/4,4,1,6,6,7,7,2,7,2,3,3/
      DATA (IRND( 6,15,iminr),iminr=1,12)/6,2,7,8,6,1,0,2,1,3,4,7/
      DATA (IRND( 6,16,iminr),iminr=1,12)/6,4,3,4,8,8,6,3,5,8,0,6/
      DATA (IRND( 6,17,iminr),iminr=1,12)/1,7,1,8,4,5,7,2,8,7,3,5/
      DATA (IRND( 6,18,iminr),iminr=1,12)/5,1,0,2,0,2,7,1,1,6,6,5/
      DATA (IRND( 6,19,iminr),iminr=1,12)/0,3,8,6,4,8,7,0,4,5,1,5/
      DATA (IRND( 6,20,iminr),iminr=1,12)/4,8,7,2,6,7,5,6,1,8,2,5/
      DATA (IRND( 6,21,iminr),iminr=1,12)/8,2,6,5,2,5,5,5,4,7,5,5/
      DATA (IRND( 6,22,iminr),iminr=1,12)/3,4,4,0,7,2,6,4,7,6,8,5/
      DATA (IRND( 6,23,iminr),iminr=1,12)/7,7,3,3,3,8,6,3,1,5,3,5/
      DATA (IRND( 6,24,iminr),iminr=1,12)/4,5,7,1,0,0,3,6,6,7,8,5/
      DATA (IRND( 7, 1,iminr),iminr=1,12)/8,8,5,4,5,6,3,5,0,6,2,5/
      DATA (IRND( 7, 2,iminr),iminr=1,12)/3,2,4,8,1,4,3,4,3,5,5,5/
      DATA (IRND( 7, 3,iminr),iminr=1,12)/7,4,3,2,6,1,3,3,5,4,0,4/
      DATA (IRND( 7, 4,iminr),iminr=1,12)/2,0,2,7,8,0,1,0,2,7,1,5/
      DATA (IRND( 7, 5,iminr),iminr=1,12)/6,3,1,2,3,6,2,8,5,6,4,5/
      DATA (IRND( 7, 6,iminr),iminr=1,12)/1,5,8,5,8,3,2,7,8,5,7,4/
      DATA (IRND( 7, 7,iminr),iminr=1,12)/5,8,7,0,4,0,2,5,2,4,2,4/
      DATA (IRND( 7, 8,iminr),iminr=1,12)/0,4,6,5,6,8,0,2,7,7,3,5/
      DATA (IRND( 7, 9,iminr),iminr=1,12)/4,6,5,8,2,6,0,1,1,6,6,4/
      DATA (IRND( 7,10,iminr),iminr=1,12)/8,0,4,3,6,3,0,0,4,5,0,4/
      DATA (IRND( 7,11,iminr),iminr=1,12)/6,5,7,8,7,2,8,5,6,3,8,4/
      DATA (IRND( 7,12,iminr),iminr=1,12)/1,1,6,4,0,1,6,2,3,6,0,5/
      DATA (IRND( 7,13,iminr),iminr=1,12)/5,4,4,7,5,7,7,1,6,5,3,4/
      DATA (IRND( 7,14,iminr),iminr=1,12)/0,6,3,2,0,4,7,0,0,4,6,4/
      DATA (IRND( 7,15,iminr),iminr=1,12)/4,0,2,5,5,2,7,8,3,3,1,4/
      DATA (IRND( 7,16,iminr),iminr=1,12)/8,5,1,1,7,1,5,5,0,6,2,4/
      DATA (IRND( 7,17,iminr),iminr=1,12)/3,7,0,5,3,7,5,4,2,5,5,4/
      DATA (IRND( 7,18,iminr),iminr=1,12)/7,1,7,8,8,4,6,3,5,4,8,4/
      DATA (IRND( 7,19,iminr),iminr=1,12)/2,3,6,3,3,1,6,2,8,3,3,3/
      DATA (IRND( 7,20,iminr),iminr=1,12)/6,8,5,7,5,0,4,7,5,6,4,4/
      DATA (IRND( 7,21,iminr),iminr=1,12)/1,2,4,2,1,7,4,6,8,5,7,4/
      DATA (IRND( 7,22,iminr),iminr=1,12)/5,4,3,6,6,4,4,5,1,4,2,3/
      DATA (IRND( 7,23,iminr),iminr=1,12)/0,7,2,0,2,1,5,4,4,3,5,3/
      DATA (IRND( 7,24,iminr),iminr=1,12)/2,7,6,3,0,2,7,2,7,2,1,7/
      DATA (IRND( 8, 1,iminr),iminr=1,12)/5,4,3,6,0,5,0,2,2,3,2,3/
      DATA (IRND( 8, 2,iminr),iminr=1,12)/7,4,8,1,6,0,8,8,8,7,1,8/
      DATA (IRND( 8, 3,iminr),iminr=1,12)/1,2,5,3,5,3,0,8,3,8,3,4/
      DATA (IRND( 8, 4,iminr),iminr=1,12)/3,0,8,7,8,5,2,7,5,0,0,2/
      DATA (IRND( 8, 5,iminr),iminr=1,12)/5,7,5,1,8,8,3,6,0,1,1,6/
      DATA (IRND( 8, 6,iminr),iminr=1,12)/8,7,2,5,5,3,2,4,6,5,1,3/
      DATA (IRND( 8, 7,iminr),iminr=1,12)/1,5,7,8,4,6,3,4,0,6,2,7/
      DATA (IRND( 8, 8,iminr),iminr=1,12)/4,3,2,3,8,8,6,2,3,7,8,5/
      DATA (IRND( 8, 9,iminr),iminr=1,12)/6,1,8,6,7,2,7,2,6,8,1,1/
      DATA (IRND( 8,10,iminr),iminr=1,12)/0,1,4,1,4,6,6,8,4,3,0,6/
      DATA (IRND( 8,11,iminr),iminr=1,12)/2,8,1,4,3,0,7,8,7,4,1,2/
      DATA (IRND( 8,12,iminr),iminr=1,12)/5,6,5,8,7,2,1,7,1,5,8,0/
      DATA (IRND( 8,13,iminr),iminr=1,12)/7,4,1,2,6,5,2,6,4,6,0,4/
      DATA (IRND( 8,14,iminr),iminr=1,12)/0,4,7,6,3,1,1,4,1,1,8,1/
      DATA (IRND( 8,15,iminr),iminr=1,12)/3,2,4,8,3,4,2,4,5,3,1,6/
      DATA (IRND( 8,16,iminr),iminr=1,12)/5,8,7,3,6,5,5,2,7,3,7,3/
      DATA (IRND( 8,17,iminr),iminr=1,12)/8,6,4,6,5,8,6,2,2,4,0,8/
      DATA (IRND( 8,18,iminr),iminr=1,12)/1,6,1,1,2,4,5,8,8,0,8,5/
      DATA (IRND( 8,19,iminr),iminr=1,12)/3,4,6,4,2,7,6,8,2,1,0,0/
      DATA (IRND( 8,20,iminr),iminr=1,12)/6,2,1,8,5,8,8,7,5,1,7,7/
      DATA (IRND( 8,21,iminr),iminr=1,12)/8,0,6,2,4,2,0,6,8,2,8,3/
      DATA (IRND( 8,22,iminr),iminr=1,12)/2,0,3,6,1,7,8,4,6,7,7,8/
      DATA (IRND( 8,23,iminr),iminr=1,12)/4,7,0,0,1,1,1,4,0,8,0,4/
      DATA (IRND( 8,24,iminr),iminr=1,12)/7,5,3,4,4,3,3,2,3,8,6,2/
      DATA (IRND( 9, 1,iminr),iminr=1,12)/0,3,0,7,3,6,4,2,6,0,7,6/
      DATA (IRND( 9, 2,iminr),iminr=1,12)/6,4,2,8,7,8,2,3,7,5,8,4/
      DATA (IRND( 9, 3,iminr),iminr=1,12)/8,2,8,2,6,2,3,3,2,6,1,8/
      DATA (IRND( 9, 4,iminr),iminr=1,12)/2,0,2,6,1,4,5,1,4,6,7,6/
      DATA (IRND( 9, 5,iminr),iminr=1,12)/4,7,8,0,0,7,7,1,8,7,8,2/
      DATA (IRND( 9, 6,iminr),iminr=1,12)/6,7,5,4,6,2,6,8,5,3,8,7/
      DATA (IRND( 9, 7,iminr),iminr=1,12)/0,5,1,6,6,5,7,8,0,4,0,3/
      DATA (IRND( 9, 8,iminr),iminr=1,12)/0,1,0,7,6,7,5,4,8,0,7,2/
      DATA (IRND( 9, 9,iminr),iminr=1,12)/4,4,8,1,2,4,5,3,2,0,1,2/
      DATA (IRND( 9,10,iminr),iminr=1,12)/8,6,7,5,7,1,5,1,5,8,4,2/
      DATA (IRND( 9,11,iminr),iminr=1,12)/3,0,6,0,2,7,5,0,8,7,8,1/
      DATA (IRND( 9,12,iminr),iminr=1,12)/7,5,5,4,4,6,3,6,5,0,0,2/
      DATA (IRND( 9,13,iminr),iminr=1,12)/2,7,3,8,0,4,4,5,7,0,3,2/
      DATA (IRND( 9,14,iminr),iminr=1,12)/6,1,2,2,5,1,4,4,1,8,7,1/
      DATA (IRND( 9,15,iminr),iminr=1,12)/1,4,1,6,1,7,4,3,4,7,1,1/
      DATA (IRND( 9,16,iminr),iminr=1,12)/5,8,0,2,3,6,2,0,1,1,2,2/
      DATA (IRND( 9,17,iminr),iminr=1,12)/0,2,8,5,8,3,2,8,4,0,5,1/
      DATA (IRND( 9,18,iminr),iminr=1,12)/4,4,6,0,3,0,3,7,7,8,0,1/
      DATA (IRND( 9,19,iminr),iminr=1,12)/8,7,5,3,8,7,3,6,0,7,3,1/
      DATA (IRND( 9,20,iminr),iminr=1,12)/3,3,4,8,1,6,1,2,6,1,4,1/
      DATA (IRND( 9,21,iminr),iminr=1,12)/7,5,3,3,6,3,1,1,0,0,7,1/
      DATA (IRND( 9,22,iminr),iminr=1,12)/2,8,2,6,2,0,1,0,3,8,2,1/
      DATA (IRND( 9,23,iminr),iminr=1,12)/6,2,0,1,6,6,1,8,6,7,5,0/
      DATA (IRND( 9,24,iminr),iminr=1,12)/3,0,4,7,4,7,7,2,2,8,1,1/
      DATA (IRND(10, 1,iminr),iminr=1,12)/7,3,2,2,0,4,7,1,5,8,4,1/
      DATA (IRND(10, 2,iminr),iminr=1,12)/2,5,1,5,4,2,8,0,8,7,8,1/
      DATA (IRND(10, 3,iminr),iminr=1,12)/6,8,0,0,0,8,8,8,1,6,2,0/
      DATA (IRND(10, 4,iminr),iminr=1,12)/1,4,8,5,2,7,6,5,7,8,3,1/
      DATA (IRND(10, 5,iminr),iminr=1,12)/5,6,7,8,7,4,6,4,1,8,6,1/
      DATA (IRND(10, 6,iminr),iminr=1,12)/0,0,5,3,3,1,6,3,4,7,1,0/
      DATA (IRND(10, 7,iminr),iminr=1,12)/4,3,4,6,8,8,7,2,7,6,4,0/
      DATA (IRND(10, 8,iminr),iminr=1,12)/8,7,3,2,1,7,5,8,4,8,5,1/
      DATA (IRND(10, 9,iminr),iminr=1,12)/3,1,2,6,5,4,5,7,6,8,0,0/
      DATA (IRND(10,10,iminr),iminr=1,12)/7,4,1,0,1,1,5,5,0,7,3,0/
      DATA (IRND(10,11,iminr),iminr=1,12)/2,6,0,4,6,7,5,4,3,6,6,0/
      DATA (IRND(10,12,iminr),iminr=1,12)/6,2,7,8,8,6,3,1,0,0,7,0/
      DATA (IRND(10,13,iminr),iminr=1,12)/1,5,6,3,4,3,4,0,3,8,2,0/
      DATA (IRND(10,14,iminr),iminr=1,12)/5,7,5,6,0,1,4,8,5,7,5,0/
      DATA (IRND(10,15,iminr),iminr=1,12)/0,1,4,1,4,7,4,7,8,6,8,8/
      DATA (IRND(10,16,iminr),iminr=1,12)/5,0,1,1,0,6,6,8,6,3,5,3/
      DATA (IRND(10,17,iminr),iminr=1,12)/8,7,6,4,0,0,7,8,0,4,7,7/
      DATA (IRND(10,18,iminr),iminr=1,12)/1,7,3,8,6,4,6,5,6,8,6,4/
      DATA (IRND(10,19,iminr),iminr=1,12)/3,5,8,1,5,7,7,5,1,0,7,0/
      DATA (IRND(10,20,iminr),iminr=1,12)/6,3,3,5,8,0,0,3,3,1,5,6/
      DATA (IRND(10,21,iminr),iminr=1,12)/8,1,0,8,8,3,1,3,7,2,6,2/
      DATA (IRND(10,22,iminr),iminr=1,12)/2,1,5,3,5,7,0,1,4,6,5,8/
      DATA (IRND(10,23,iminr),iminr=1,12)/4,8,2,6,4,1,2,0,7,7,7,3/
      DATA (IRND(10,24,iminr),iminr=1,12)/7,6,8,0,4,4,3,0,2,0,8,8/
      DATA (IRND(11, 1,iminr),iminr=1,12)/0,4,2,4,7,6,5,8,4,0,6,6/
      DATA (IRND(11, 2,iminr),iminr=1,12)/2,2,8,7,6,0,6,7,8,1,7,1/
      DATA (IRND(11, 3,iminr),iminr=1,12)/5,2,5,2,3,4,5,5,5,6,6,7/
      DATA (IRND(11, 4,iminr),iminr=1,12)/7,8,1,5,3,7,7,5,0,7,7,2/
      DATA (IRND(11, 5,iminr),iminr=1,12)/1,6,5,0,6,0,0,3,2,7,5,0/
      DATA (IRND(11, 6,iminr),iminr=1,12)/3,4,1,2,6,3,1,3,5,8,6,5/
      DATA (IRND(11, 7,iminr),iminr=1,12)/6,4,7,6,2,8,0,0,3,4,5,1/
      DATA (IRND(11, 8,iminr),iminr=1,12)/8,2,4,0,2,2,1,0,6,5,7,6/
      DATA (IRND(11, 9,iminr),iminr=1,12)/1,0,7,4,5,3,4,8,0,5,4,4/
      DATA (IRND(11,10,iminr),iminr=1,12)/4,7,4,7,5,6,5,7,3,6,6,8/
      DATA (IRND(11,11,iminr),iminr=1,12)/6,7,1,2,1,2,4,5,1,2,5,5/
      DATA (IRND(11,12,iminr),iminr=1,12)/0,5,6,5,1,5,5,5,4,3,6,1/
      DATA (IRND(11,13,iminr),iminr=1,12)/2,3,1,0,4,6,8,3,7,3,4,7/
      DATA (IRND(11,14,iminr),iminr=1,12)/5,1,7,3,4,0,0,3,1,4,5,3/
      DATA (IRND(11,15,iminr),iminr=1,12)/7,1,3,7,0,5,8,0,7,0,4,0/
      DATA (IRND(11,16,iminr),iminr=1,12)/0,8,0,1,0,8,0,0,2,1,6,4/
      DATA (IRND(11,17,iminr),iminr=1,12)/3,5,4,5,3,1,2,8,4,1,3,2/
      DATA (IRND(11,18,iminr),iminr=1,12)/5,3,0,8,3,4,4,7,8,2,4,7/
      DATA (IRND(11,19,iminr),iminr=1,12)/8,3,6,3,0,8,2,5,5,7,4,3/
      DATA (IRND(11,20,iminr),iminr=1,12)/1,1,3,5,8,2,4,5,8,8,5,8/
      DATA (IRND(11,21,iminr),iminr=1,12)/4,8,6,0,2,4,6,3,2,8,2,6/
      DATA (IRND(11,22,iminr),iminr=1,12)/6,6,3,3,2,7,7,3,5,0,4,1/
      DATA (IRND(11,23,iminr),iminr=1,12)/8,6,0,7,8,2,6,1,3,5,3,7/
      DATA (IRND(11,24,iminr),iminr=1,12)/2,4,5,1,7,5,7,0,6,6,4,2/
      DATA (IRND(12, 1,iminr),iminr=1,12)/4,7,1,2,4,1,4,5,6,2,5,8/
      DATA (IRND(12, 2,iminr),iminr=1,12)/8,0,0,6,0,7,5,4,0,2,8,7/
      DATA (IRND(12, 3,iminr),iminr=1,12)/3,3,8,0,5,4,5,3,3,1,2,7/
      DATA (IRND(12, 4,iminr),iminr=1,12)/7,5,7,4,1,2,5,2,6,0,5,7/
      DATA (IRND(12, 5,iminr),iminr=1,12)/2,1,6,0,3,0,3,8,2,2,7,7/
      DATA (IRND(12, 6,iminr),iminr=1,12)/6,4,4,3,8,7,3,7,5,2,1,7/
      DATA (IRND(12, 7,iminr),iminr=1,12)/3,0,8,8,8,6,2,3,8,0,8,7/
      DATA (IRND(12, 8,iminr),iminr=1,12)/8,3,6,3,4,3,3,2,1,8,2,6/
      DATA (IRND(12, 9,iminr),iminr=1,12)/3,7,5,8,6,2,1,8,7,1,4,7/
      DATA (IRND(12,10,iminr),iminr=1,12)/7,1,4,2,1,8,1,7,1,0,7,7/
      DATA (IRND(12,11,iminr),iminr=1,12)/2,4,3,6,6,6,1,6,4,0,1,6/
      DATA (IRND(12,12,iminr),iminr=1,12)/6,6,2,0,2,3,1,5,7,8,4,6/
      DATA (IRND(12,13,iminr),iminr=1,12)/1,2,0,5,4,2,8,1,3,1,6,7/
      DATA (IRND(12,14,iminr),iminr=1,12)/5,5,8,0,0,8,8,0,6,1,0,6/
      DATA (IRND(12,15,iminr),iminr=1,12)/0,7,7,3,4,5,0,8,0,0,3,6/
      DATA (IRND(12,16,iminr),iminr=1,12)/4,1,6,7,0,3,0,7,3,8,7,6/
      DATA (IRND(12,17,iminr),iminr=1,12)/8,6,5,3,2,1,7,4,0,1,8,6/
      DATA (IRND(12,18,iminr),iminr=1,12)/3,8,3,6,7,8,7,3,3,1,2,6/
      DATA (IRND(12,19,iminr),iminr=1,12)/7,2,2,1,3,5,7,2,5,0,5,6/
      DATA (IRND(12,20,iminr),iminr=1,12)/2,5,1,4,8,2,8,1,8,8,0,5/
      DATA (IRND(12,21,iminr),iminr=1,12)/6,0,0,0,1,1,6,7,5,1,1,6/
      DATA (IRND(12,22,iminr),iminr=1,12)/1,3,8,4,5,7,6,6,8,1,4,6/
      DATA (IRND(12,23,iminr),iminr=1,12)/7,8,2,0,6,7,5,2,1,8,2,6/
      DATA (IRND(12,24,iminr),iminr=1,12)/2,2,1,3,1,4,5,1,4,7,6,5/


c     array of month names
      data amonths /'January','February','March','April','May','June',
     &  'July','August','September','October','November','December'/
      end module main1
c*************************************************************************
      program winds
      use main1
      implicit none
      integer ierr
      logical lstop
c     ierr:   integer variable denoting that an error has occurred. set
c             to 1 if lstop is true, otherwise ierr = 0
c     lstop:  logical variable denoting if program should abort based on
c             errors in various subroutines         
      ierr=0
      open(unit=ilogunit,file=logfil,status='replace')
c     get start date/time of program execution      
      call datetime(ierr,1)
      
c     read aerminute.inp
      call userinp(lstop)
  
c     if an error write end date and stop program
      if (lstop) then
        ierr=1
        goto 10
      endif
c     initialize arrays of flags, speeds and directions to zero or missing
      call initial

c     check for existence of one minute data files
c     version 15272check for existences of 5-minute data files
      if (.not. lno1dat) then 
          call checkfiles(1)
c     if problems with the files, stop program   
          if (.not. lkeep1 .or. lbad1 .or. ldup) then
            ierr=1
            goto 10
          endif
      endif
      if (lfive) then
          run5=.false.
          call checkfiles(5)
          if (.not. lkeep5 .or.  lbad5 .or. ldup5) then
              ierr=1
              goto 10
          endif
          run5=.true.
      endif

c     get list of surface data files for standard observations if
c     comparisons are to be made
      if (comp) then
        call getsurf
c       if problems, stop program
        if (.not. lgo .or. ldupob) then
          ierr=1
          goto 10
        endif
      endif 
   
c     loop through months and read in one minute data
c     version 10193: add logical variable to check to make sure
c     some of the data in the one minute files are within the window
c     determined by the start and end dates
c     example: if start date is January 2005 and end date is December 2005
c     but data files are for January through December 2004, there is no
c     data within window determined by start and end dates, warn user and
c     abort program
c     version 10221, if WBAN number changes, stop program.
      if (.not. lno1dat) then
          call readone
          if (lnodata .or. lwban .or. badwban) then
          ierr=1
          goto 10
          endif
      endif
c     if not all files are present or have bad name (good_records.dat,bad_records.dat,check_records.dat)
c     stop program


c     version 15272, if run5 is true then read the 5 minute data and calculate hourly averges
      if (run5) then
          call readfive
          if (lnodata5 .or. lwban .or. badwban) then
              ierr=1
              goto 10
          endif
          if (run5) call sub5  !substitute 5-minute data into 1 minute data
      endif
      
c     get totals of minutes (total, calms, evens, even calms, odds, and odd calms
      
      call totals
      
c     assign keep flags to hours and get # of odds used and odd calms used
      call assignflag
      
c     6/9/10 - read ISHD data to compare non-QC one minute data to QC'd standard observations
      if (comp) then 
        call qc
      endif
      
c     get min and maximum winds for each hour
      call hourlyminmax
      
c     calculate hourly average speeds and directions       
      call avg

c     write output files
      call writeout


c     deallocate arrays
      call cleanup      
c     get end date/time
      
 10   call datetime(ierr,2)

c     close error file, logfile, output file, and summary file

      close(ilogunit)

      end
c*********************************************************************************
      subroutine datetime(ierr,iflag)
c subroutine to write starting and ending times to screen and log file
c subroutine calls the intrinsic FORTRAN subroutine date_and_time
c inputs are iflag which tells the subroutine if the program starts (iflag=1)
c or ends (iflag not equal to 1)

c variables
c ierr:     integer denoting if AERMINUTE is aborting abnormally
c iflag:    integer denoting if it is program start time (iflag=1) or end time (2)
c idattim:  array of date/time returned by date_and_time with 8 elements
c           elements are:
c           (1) 4-digit year
c           (2) month of year (1 to 12)
c           (3) day of month
c           (4) time difference with respect to GMT in minutes
c           (5) hour of day (0 to 23) in local time
c           (6) minute of time
c           (7) second of time
c           (8) milliseconds of time
c hr:       integer hour of day
c cdate:    character variable of date
c ctime:    character variable of time
c czone:    character variable of time zone from GMT
c ampm:     character variable with value of 'AM' or 'PM'     
    
      use main1
      implicit none
      integer ierr,iflag,idattim(8),hr
      character cdate*8,ctime*10,czone*5,ampm*2
      
      call date_and_time(cdate,ctime,czone,idattim)
      
      if (idattim(5) .le. 11) then
        hr=idattim(5)
        ampm='AM'
      else
        hr=12+(idattim(5)-12)
        ampm='PM'
      endif
c program starting      
      if (iflag .eq. 1) then 
        write(*,10)versn,trim(adjustl(amonths(idattim(2)))),idattim(3),
     + idattim(1),hr,idattim(6),idattim(7),ampm
        write(ilogunit,10)versn,trim(adjustl(amonths(idattim(2)))),
     + idattim(3),idattim(1),hr,idattim(6),idattim(7),ampm
c program ending
      else
        if (ierr .eq. 1) then
          write(*,13)
          write(ilogunit,13)
        else
          write(*,14)
          write(ilogunit,14)
        endif
        write(*,15)trim(adjustl(amonths(idattim(2)))),idattim(3),
     + idattim(1),hr,idattim(6),idattim(7),ampm
        write(ilogunit,15)trim(adjustl(amonths(idattim(2)))),idattim(3),
     + idattim(1),hr,idattim(6),idattim(7),ampm
      endif
 10   format(1x,'AERMINUTE VERSION ',a6/1x,
     +  'Program start date & time: ',a,1x,i2.2,', ',i4,2x,i2,':',
     +  i2.2,':',i2.2,1x,a2)
 13   format(/1x,'Abnormal termination')
 14   format(/1x,'Normal termination')
 15   format(/1x,'Program end date & time: ',a,1x,i2.2,', ',i4,2x,i2,
     +  ':',i2.2,':',i2.2,1x,a2)
      return
      end
c*************************************      
c subroutine to get user input: start and end dates
c if station is part of IFW group and IFW comission date, and
c prefix for output file.
c version 10167 - user input no longer prompts based but now
c control file based   
      subroutine userinp(lstop)
      use main1
      implicit none
         
c version 15272modified to read in list of 5-minute data files
         
c ifwyear:  4-digit year of ifw anemometer installation date
c ifwmon:   Month of ifw anemometer installation date
c ifwday:   Day of ifw anemometer installation date
c ianswer:  Integer choice to change parameter or continue  (remove later)
c eof:      Integer end of file indicator when reading aerminute.inp
c ndates:   Integer index value of 'STARTEND' or 'startend' in line from AERMINUTE.INP
c           denotes that the start and end dates have been found
c nifw:     Integer index value of 'IFWGROUP' or 'ifwgroup' in line from AERMINUTE.INP
c           denotes that information about IFW status of NWS station has been found
c nonedat:  Integer index value of 'DATAFILE' or 'datafile' in line from AERMINUTE.INP
c           denotes that list of one minute data has been found
c n5dat:    Integer index value of 'DAT5FILE' or 'data5file' in line from AERMINUTE.INP
c           denotes that list of 5-minute data has been found
c nsfc:     Integer index value of 'SURFDATA' or 'surfdata' in line from AERMINUTE.INP
c           denotes that list of standard surface observation data files has been found
c nout:     Integer index value of 'OUTFILES' or 'outfiles' in line from AERMINUTE.INP
c           denotes that list of output files has been found
c nstart:   Integer index value of 'STARTING' or 'starting' in line from AERMINUTE.INP
c           denotes that lists of datafiles are starting
c nstop:    Integer index value of 'FINISHED' or 'finished' in line from AERMINUTE.INP
c           denotes that lists of datafiles is complete
c n5start:  Integer index value of 'STARTING' or 'starting' in line from AERMINUTE.INP
c           denotes that lists of 5-minute datafiles are starting
c n5stop:   Integer index value of 'FINISHED' or 'finished' in line from AERMINUTE.INP
c           denotes that lists of 5-minute datafiles is complete
c noutfil:  Integer index value of 'HOURFILE' or 'hourfile' in line from AERMINUTE.INP
c           denotes that name of hourly averaged output filename found
c nsumfil:  Integer index value of 'SUMMFILE' or 'summfile' in line from AERMINUTE.INP
c           denotes that name of optional summary filename found
c ncompfil: Integer index value of 'COMPFILE' or 'compfile' in line from AERMINUTE.INP
c           denotes that name of optional standard vs. one minute comparison filename found
c ilen:     Integer length of string passed to checkdates and checkifw
c nrecone:  Integer number of files found in DATAFILE section
c nrec5:    Integer number of files found in DAT5FILE section
c nrecsfc:  Integer number of files found in SURFDATA section
c i:        Integer loop counter to convert line to uppercase
c nsub5fil: Integer index of 'SUM5FILE' or 'sum5file' in line from AERMINUTE.INP
c ncmp5fil: Integer index of '1_5_FILE' or '1_5_file' in line from AERMINUTE.INP
      integer ifwyear,ifwmon,ifwday,ianswer,eof,ndates,nifw,
     + nonedat,nsfc,nout,nstart,nstop,noutfil,nsumfil,ncompfil,ilen,
     + nrecone,nrecsfc,i,n5dat,n5start,n5stop,nrec5,nsub5fil,ncmp5fil

c line:     character string read from control file
c linuep:   upper case version of line
c confil:   Name of control file
      character line*300,confil*250, lineup*300

c exist1:         logical variable denoting if AERMINUTE.INP exists
c lstop:          logical variable denoting to stop program if a problem found
c ldates:         logical variable denoting that the start and end dates have
c                 been found in AERMINUTE.INP.  ndates is greater than zero
c lifw:           logical variable denoting that the IFW status of the NWS station
c                 have been found in AERMINUTE.INP.  nifw is greater than zero
c lonefiles:      2-dimensional array logical array denoting that the list of
c                 one minute data files has been found in AERMINUTE.INP
c                 First element of array denotes that 'DATAFILES STARTING' or
c                 'datafiles starting' has been found.  Second element of array
c                 denotes that 'DATAFILES FINISHED' or 'datafiles finished' has
c                 been found.  If one elements is false after reading
c                 AERMINUTE.INP, program will abort.
c                 Beginning with version 15272, if both elements are false
c                 and 5-minute data is being processed, program will
c                 warn user and proceed.
c l5files:        2-dimensional array logical array denoting that the list of
c                 5-minute data files has been found in AERMINUTE.INP
c                 First element of array denotes that 'DAT5FILES STARTING' or
c                 'dat5files starting' has been found.  Second element of array
c                 denotes that 'DAT5FILES FINISHED' or 'dat5files finished' has
c                 been found.  IIf one elements is false after reading
c                 AERMINUTE.INP, program will abort.  If both are missing
c                 and no data files listed, program will proceeed.
c lsfcfiles:      2-dimensional array logical array denoting that the list of
c                 optional surface data files has been found in AERMINUTE.INP
c                 First element of array denotes that 'SURFDATA STARTING' or
c                 'sufdata starting' has been found.  Second element of array
c                 denotes that 'SURFDATA FINISHED' or 'surfdata finished' has
c                 been found.  If one element is true and the other is false,
c                 after reading AERMINUTE.INP, the program will stop.  Since
c                 the surface data is optional, if both elements are false,
c                 the program will not abort, if a COMPFILE name is not listed
c                 see lcompfil for more details
c loutput:        2-dimensional array logical array denoting that the list of
c                 output files has been found in AERMINUTE.INP
c                 First element of array denotes that 'OUTFILES STARTING' or
c                 'outfiles starting' has been found.  Second element of array
c                 denotes that 'OUTFILES FINISHED' or 'outfiles finished' has
c                 been found.  If one or both elements are false after reading
c                 AERMINUTE.INP, program will abort.
c lonestart:      logical variable set to true when 'DATAFILES STARTING' or
c                 'datafiles starting' found.  nonedat and nstart both greater than
c                 zero for line in AERMINUTE.INP
c l5start:        logical variable set to true when 'DAT5FILES STARTING' or
c                 'dat5files starting' found.  n5dat and n5start both greater than
c                 zero for line in AERMINUTE.INP
c lonestop:       logical variable set to true when 'DATAFILES FINISHED' or
c                 'datafiles finished' found.  n5dat and n5stop both greater than
c                 zero for line in AERMINUTE.INP
c l5stop:       logical variable set to true when 'DAT5FILES FINISHED' or
c                 'dat5files finished' found.  nonedat and nstop both greater than
c                 zero for line in AERMINUTE.INP
c lsfcstart:      logical variable set to true when 'SURFDATA STARTING' or
c                 'surfdata starting' found.  nsfc and nstart both greater than
c                 zero for line in AERMINUTE.INP
c lsfcstop:       logical variable set to true when 'SURFDATA FINISHED' or
c                 'surfdata finished' found.  nsfc and nstart both greater than
c                 zero for line in AERMINUTE.INP
c loutstart:      logical variable set to true when 'OUTFILES STARTING' or
c                 'outfiles starting' found.  nout and nstart both greater than
c                 zero for line in AERMINUTE.INP
c loutstop:       logical variable set to true when 'OUTFILES FINISHED' or
c                 'outfiles finished' found.  nout and nstop both greater than
c                 zero for line in AERMINUTE.INP
c loutfil:        logical variable denoting that 'HOURFILE' or 'hourfile' found
c                 in line in AERMINUTE.INP. noutfil greater than zero
c lsumfil:        logical variable denoting that 'SUMMFILE' or 'summfile' found
c                 in line in AERMINUTE.INP. nsumfil greater than zero
c lcompfil:       logical variable denoting that 'COMPFILE' or 'compfile' found
c                 in line in AERMINUTE.INP. ncompfil greater than zero.
c                 if lcompfil has a value opposite of lsfcfiles, AERMINUTE will
c                 abort.
c nonblank:       logical varible denoting that text string being checked
c                 is not blank.  Output of subroutine checkblank.
c                 Used to check line from AERMINUTE.INP for various filenames

      logical exist1,lstop,ldates,lifw,lonefiles(2),
     + lsfcfiles(2),l5files(2),loutput(2),lonestart,lonestop,
     + l5start,l5stop,lsfcstart,lsfcstop,loutstart,loutstop,loutfil,
     + lsumfil,lcompfil,nonblank
!      logical leap,exist1,exist2,lstop,ldates,lifw,lonefiles(2),
!     + lsfcfiles(2),loutput(2),lonestart,lonestop,lsfcstart,lsfcstop,
!     + loutstart,loutstop,loutfil,lsumfil,lcompfil,nonblank


c     initialize logical variables to false     
c      leap=.false.
      lstop=.false.
      ldates=.false.
      lifw=.false.
      lonefiles=.false.
      l5files=.false.
      lsfcfiles=.false.
      loutput=.false.
c      ianswer=0
      lonestart=.false.
      lonestop=.false.
      l5start=.false.
      l5stop=.false.
      lsfcstart=.false.
      lsfcstop=.false.
      loutstart=.false.
      loutstop=.false.
      loutfil=.false.
      lsumfil=.false.
      lcompfil=.false.
      lno1dat=.false.
c writesum and comp are defined in module main1
c version 15272 lfive defined in module main1
      writesum=.false.
      comp=.false.
      lfive=.false.
      writesub5=.false.
      comp15=.false.
      nrecone=0
      nrecsfc=0
      nrec5=0

      write(*,40)
      write(ilogunit,40)
40    format(/1x,'################################',' INPUTS ',
     +  '################################') 

c     prompt user for name of control file
      write(*,41)
  41  format(/1x,'Enter the name of the AERMINUTE control file.'/1x,
     +  'If the filename or pathname contains spaces enclose in ',
     +  'quotations'/)
      read(*,*)confil
c     check for existence of AERMINUTE.INP
      inquire(file=confil,exist=exist1)
      if (.not. exist1) then
        write(*,10)trim(adjustl(confil))
        write(ilogunit,10)trim(adjustl(confil))
        lstop=.true.
        goto 99
      endif
 10   format(/1x,a,' DOES NOT EXIST, STOPPING PROGRAM')
 
c     begin reading file
c     check for errors on different data sections
c     if an error found on one line, do not stop program
c     immediately.  check other lines as well and if
c     multiple errors, list and then stop program
      open(mainunit,file=confil,status='old')
           
      read(mainunit,15,iostat=eof)line

 20   if (eof .eq. 0) then
        lineup=line
        do i=1,len_trim(lineup)
          call upcase(lineup(i:i))
        enddo
        if (line(1:2) .ne. '**') then
c         check for various keywords in data line
          ndates=index(lineup,'STARTEND')
          nifw=index(lineup,'IFWGROUP')
          nonedat=index(lineup,'DATAFILE')
          n5dat=index(lineup,'DAT5FILE')   !152725-minute data files
          nsfc=index(lineup,'SURFDATA')
          nout=index(lineup,'OUTFILES')
  
          if (ndates .gt. 0) then !get start and end dates
            ilen=300-(ndates+8)
            call getdates(line(ndates+9:300),lstop,ilen) 
            ldates=.true.
          endif
          if (nifw .gt. 0) then  !get IFW status and commission date if applicable
            ilen=300-(nifw+8)
            call checkifw(line(nifw+9:300),lstop,ifwmon,ifwday,ifwyear,
     +      ilen)
            lifw=.true.
          endif      
          if (nonedat .gt. 0) then  !list of one minute files
            nstart=index(lineup,'STARTING')
            nstop=index(lineup,'FINISHED')
            if (nstart .gt. 0 .and. nstop .eq. 0) then
              lonestart=.true.
              lonefiles(1)=.true.
              open(unit=88,file='datafiles.tmp',status='replace')
            endif
            if (nstop .gt. 0 .and. nstart .eq. 0) then
              lonestop=.true.
              lonefiles(2)=.true.
              close(88)
            endif
            if (nstart .eq. nstop) then
              write(*,11)'DATAFILE'
              write(ilogunit,11)'DATAFILE'
              lstop=.true.
            endif       
          endif
          if (lonestart .and. .not. lonestop .and. nonedat .eq. 0) then  !list of one minute data files
c           check that line is not completely blank
c           blank lines are okay, user may be using blank
c           lines as separators, just make sure that blank
c           line is not written to data file list
            call checkblank(line,nonblank)
c           write data file to temporary file, datafiles.tmp
            if (nonblank) then
              nrecone=nrecone+1
              write(88,15)line
            endif
          endif
c         15272 5-minute data files
          if (n5dat .gt. 0) then  !list of 5-minute files
                n5start=index(lineup,'STARTING')
                n5stop=index(lineup,'FINISHED')
                if (n5start .gt. 0 .and. n5stop .eq. 0) then
                  l5start=.true.
                  l5files(1)=.true.
                  open(unit=89,file='datafiles5.tmp',status='replace')
                endif
                if (n5stop .gt. 0 .and. n5start .eq. 0) then
                  l5stop=.true.
                  l5files(2)=.true.
                  lfive=.true.
                  close(89)
                endif
                if (n5start .eq. n5stop) then
                  write(*,11)'DAT5FILE'
                  write(ilogunit,11)'DAT5FILE'
                  lstop=.true.
                endif       
          endif
          if (l5start .and. .not. l5stop .and. n5dat .eq. 0) then  !list of 5 minute data files
c           check that line is not completely blank
c           blank lines are okay, user may be using blank
c           lines as separators, just make sure that blank
c           line is not written to data file list
                call checkblank(line,nonblank)
c           write data file to temporary file, datafiles.tmp
                if (nonblank) then
                  nrec5=nrec5+1
                  write(89,15)line
                endif
          endif

          if (nsfc .gt. 0) then  !list of surface files to use for QA purposes   
            nstart=index(lineup,'STARTING')
            nstop=index(lineup,'FINISHED')
            if (nstart .gt. 0 .and. nstop .eq. 0) then
              lsfcstart=.true.
              lsfcfiles(1)=.true.
              open(unit=50,file='sfcfiles.tmp',status='replace')
            endif
            if (nstop .gt. 0 .and. nstart .eq. 0) then
              lsfcstop=.true.
              lsfcfiles(2)=.true.
              close(50)
            endif
            if (nstart .eq. nstop) then
              write(*,11)'SURFDATA'
              write(ilogunit,11)'SURFDATA'
              lstop=.true.
            endif            
          endif
          if (lsfcstart .and. .not. lsfcstop .and. nsfc .eq. 0) then  !list of surface data files
c           check that line is not completely blank
c           blank lines are okay, user may be using blank
c           lines as separators, just make sure that blank
c           line is not written to data file list
            call checkblank(line,nonblank)
c           write data file to temporary file, sfcfiles.tmp
            if (nonblank) then
              nrecsfc=nrecsfc+1
              write(50,15)line
            endif
          endif
          if (nout .gt. 0) then  !list of output files
            nstart=index(lineup,'STARTING')
            nstop=index(lineup,'FINISHED ')
            if (nstart .gt. 0 .and. nstop .eq. 0) then
              loutstart=.true.
              loutput(1)=.true.
            endif
            if (nstop .gt. 0 .and. nstart .eq. 0) then
              loutstop=.true.
              loutput(2)=.true.
            endif
            if (nstart .eq. nstop) then
              write(*,11)'OUTFILES'
              write(ilogunit,11)'OUTFILES'
              lstop=.true.
            endif            
          endif
          if (loutstart .and. .not. loutstop) then  !list of output files
            noutfil=index(lineup,'HOURFILE') 
            if (noutfil .gt. 0) then  !output data file, contains hourly averaged winds
c             check for blanks
              call checkblank(line(noutfil+8:300),nonblank)
              if (nonblank) then
                read(line(noutfil+8:MAX(300,len_trim(line))),*)outfil
                loutfil=.true.
              else
                lstop=.true.
                write(*,12)'HOURFILE'
                write(ilogunit,12)'HOURFILE'
              endif
            endif
            nsumfil=index(lineup,'SUMMFILE')
            if (nsumfil .gt. 0) then
c             check for blanks
              call checkblank(line(nsumfil+8:300),nonblank)
              if (nonblank) then
                read(line(nsumfil+8:MAX(300,len_trim(line))),*)sumfil
                lsumfil=.true.
                writesum=.true.
              else
                lstop=.true.
                write(*,12)'SUMMFILE'
                write(ilogunit,12)'SUMMFILE'            
              endif
            endif
            ncompfil=index(lineup,'COMPFILE')
            if (ncompfil .gt. 0) then
c             check for blanks
              call checkblank(line(ncompfil+8:300),nonblank)
              if (nonblank) then
                read(line(ncompfil+8:MAX(300,len_trim(line))),*)compfil
                lcompfil=.true.
                comp=.true.
              else
                lstop=.true.
                write(*,12)'COMPFILE'
                write(ilogunit,12)'COMPFILE'
              endif
            endif  
            nsub5fil=index(lineup,'SUB5FILE')  !5-minute summary file
            if (nsub5fil .gt. 0) then
c             check for blanks
              call checkblank(line(nsub5fil+8:300),nonblank)
              if (nonblank) then
                read(line(nsub5fil+8:MAX(300,len_trim(line))),*)sub5fil
                writesub5=.true.
              endif
            endif
            ncmp5fil=index(lineup,'1_5_FILE')  !5-minute summary file
            if (ncmp5fil .gt. 0) then
c             check for blanks
              call checkblank(line(ncmp5fil+8:300),nonblank)
              if (nonblank) then
                read(line(ncmp5fil+8:MAX(300,len_trim(line))),*)comp5fil
                comp15=.true.
              endif
            endif
          endif
        endif 
        read(mainunit,15,iostat=eof)line
        goto 20
      endif
      
c check to make sure that all data sections have been found      
c version 15272, instead of an error for no 1-minute files
c issue warning only when 5 minute data files are listed
      if (.not. lstop) then
        if (.not. ldates) then
          write(*,13)'STARTEND'
          write(ilogunit,13)'STARTEND'
          lstop=.true.
        endif
        if (.not. lifw) then 
          write(*,13)'IFWGROUP'
          write(ilogunit,13)'IFWGROUP'
          lstop=.true.
        endif
!        if (.not. lonefiles(1)) then
         if (.not. lonefiles(1) .and. lonefiles(2)) then
          write(*,13)'DATAFILE STARTING'
          write(ilogunit,13)'DATAFILE STARTING'
          lstop=.true.
        endif
!        if (.not. lonefiles(2)) then
        if (.not. lonefiles(2) .and. lonefiles(1)) then
          write(*,13)'DATAFILE FINISHED'
          write(ilogunit,13)'DATAFILE FINISHED'
          lstop=.true.
        endif
        if (lonefiles(1) .and. lonefiles(2) .and. nrecone .eq. 0) then
          write(*,14)
          write(ilogunit,14)
          lstop=.true.
        endif
        if (.not. lonefiles(1) .and. .not. lonefiles(2)) then
            if (lfive) then
                write(*,18)
                write(ilogunit,18)
                lno1dat=.true.
            else
                write(*,19)
                write(ilogunit,19)
                lstop=.true.
            endif
        endif
        if (l5files(2) .and. .not. l5files(1)) then !5
          write(*,13)'DAT5FILE STARTING'
          write(ilogunit,13)'DAT5FILE STARTING'
          lstop=.true.
        endif
        if (l5files(1) .and. .not. l5files(2)) then 
          write(*,13)'DAT5FILE FINISHED'
          write(ilogunit,13)'DAT5FILE FINISHED'
          lstop=.true.
        endif
        if (l5files(1) .and. l5files(2) .and. nrec5 .eq. 0) then
          write(*,17)
          write(ilogunit,17)
          lstop=.true.
        endif
        if (.not. loutput(1)) then 
          write(*,13)'OUTFILES STARTING'
          write(ilogunit,13)'OUTFILES STARTING'
          lstop=.true.
        endif
        if (.not. loutput(2)) then 
          write(*,13)'OUTFILES FINISHED'
          write(ilogunit,13)'OUTFILES FINISHED'
          lstop=.true.
        endif
        if (.not. loutfil .and. loutput(1) .and. loutput(2)) then 
          write(*,13)'HOURFILE'
          write(ilogunit,13)'HOURFILE'
          lstop=.true.
        endif
        if (lsfcfiles(1) .and. .not. lsfcfiles(2)) then
          write(*,13)'SURFDATA FINISHED'
          write(ilogunit,13)'SURFDATA FINISHED'
          lstop=.true.
        endif
        if (lsfcfiles(2) .and. .not. lsfcfiles(1)) then
          write(*,13)'SURFDATA STARTING'
          write(ilogunit,13)'SURFDATA STARTING'
          lstop=.true.
        endif
        if (lsfcfiles(1) .and. lsfcfiles(2) .and. .not. lcompfil) then
          if (nrecsfc .gt. 0) then
            write(*,13)'COMPFILE'
            write(ilogunit,13)'COMPFILE'
            lstop=.true.
          else
            write(*,16)
            write(ilogunit,16)
          endif
        endif
        if (.not. lsfcfiles(1) .and. .not. lsfcfiles(2) .and. 
     +  lcompfil) then
          write(*,13)'SURFDATA'
          write(ilogunit,13)'SURFDATA'
          lstop=.true.
        endif
        if (.not. l5files(1) .and. .not. l5files(2) .and. 
     +  writesub5) then
            write(*,21)'SUB5FILE'
            write(ilogunit,21)'SUB5FILE'
            lstop=.true.
        endif
        if (.not. l5files(1) .and. .not. l5files(2) .and. 
     +  comp15) then
            write(*,21)'1_5_FILE'
            write(ilogunit,21)'1_5_FILE'
            lstop=.true.
         endif
         if (lno1dat .and. comp15) then
             write(*,22)'1_5_FILE'
             write(ilogunit,22)'1_5_FILE'
             lstop=.true.
         endif
         
      endif      
      if (lstop) goto 98

c     write summary of dates and IFW status         
      write(*,45)trim(adjustl(confil)),startmon,startday,startyear,
     +  endmon,endday,endyear
      write(ilogunit,45)trim(adjustl(confil)),startmon,startday,
     +  startyear,endmon,endday,endyear
      if (ifw) then
        write(*,46)ifwmon,ifwday,ifwyear
        write(ilogunit,46)ifwmon,ifwday,ifwyear
      else
        if (ifwdate .gt. enddate .and. ifwdate .ne. 99999999) then
          write(*,47)ifwmon,ifwday,ifwyear
          write(ilogunit,47)ifwmon,ifwday,ifwyear
        else
          write(*,48)
          write(ilogunit,48)
        endif
      endif      
c     version 15272, 5-minute data
      if (lfive) then
          write(*,51)
          write(ilogunit,51)
      endif
      
 98   write(ilogunit,49)

 15   format(a300)
 11   format(/1x,a,' is formatted incorrectly')
 12   format(/1x,a,' filename is missing')
 13   format(/1x,'Missing or incorrectly formatted keyword ',a)
 14   format(/1x,'No one minute data files listed')
 16   format(/1x,'Keyword SURFDATA listed but no surface files listed')
 17   format(/1x,'No 5 minute data files listed')
 18   format(/1x,'WARNING:  Keywords missing for 1-minute data files')
 19   format(/1x,'1-minute data STARTING and ENDING keywords missing ',
     +'or incorrectly formatted')
 21   format(/1x,a,1x,'keyword is present but DAT5FILE block is',
     + 'missing')
22    format(/1x,a,1x,'keyword is present but DATAFILE block is',
     + 'missing')
45    format(/1x,'Control input file: ',a
     +  /1x,'Summary of dates'/1x,'Start date: ',
     +  2(i2,1x),i4/1x,'End Date: ',2(i2,1x),i4)
 46   format(/1x,'Station is part of IFW group for data period'/1x,
     + 'With start date of',1x,2(i2,1x),i4)
 47   format(/1x,'Station is part of IFW group'/1x,'With start date of',
     +  1x,2(i2,1x),i4/1x,'But is outside of the range of processed ',
     +  'dates')
 48   format(/1x,'Station is not part of the IFW group for data period')
  
 49   format(//1x,'################',
     +  '########################################################')
 51   format(/1x,'5-minute files will be read')
     
 99   return
      end 
c*************************************************************
      subroutine upcase(flag)
c     convert lowercase variable to upper case variable

c flag:       variable to checked
c lower:      string of lowercase letters
c upper:      string of uppercase letters
c i:          index of flag in lower
      integer i
      character flag*1
      character lower*26,upper*26
      i=0
      lower='abcdefghijklmnopqrstuvwxyz'
      upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      
      i=index(lower,flag)

      if (i .gt. 0) flag=upper(i:i)
      return
      end
c*************************************************************
      subroutine getdates(astr,lstop,ilen)
c     subroutine to check start and end dates when
c     string STARTEND found in AERMINUTE.INP
      use main1
      implicit none

c lstop:  logical variable denoting that a problem has been
c         found with dates strings
c leap:   logical variable denoting if start or end year is
c         a leap year.  Used only when end month is February
c l1:     logical variable denoting that a text string representing
c         a number has been found
c lchar:  logical variable denoting that a non-numeric or nonblank
c         character found in string astr
c astr:   character string being checked for dates
c nums:   character string of numbers 0 through 9
c ifield: integer number of numeric strings found
c i:      integer loop counter
c ilen:   integer actual length of string astr
c cnum:   real representation of numbers being read        
      logical lstop,leap,l1,lchar
      character astr*(*),nums*10
      integer ifield,i,ilen,i1
      real cnum
      nums='0123456789'
      cnum=0.0
      
c      ilen=len(trim(adjustl(astr)))

c     check for non-numeric characters in string
c     if found, stop program
      i1=33
      lchar=.false.
      leap=.false.
      l1=.false.
      do while (.not. lchar .and. i1 .le. 126)
        if (index(astr,char(i1)) .gt. 0) lchar=.true.
        if (i1 .eq. 47) then
          i1=58
        else
          i1=i1+1
        endif
      enddo
      if (lchar) then
        lstop=.true.
        write(*,5)
        write(ilogunit,5)
        goto 99
      endif
      
      ifield=0      
      do i=1,ilen
        if (astr(i:i) .ne. ' ') then
          if (astr(i:i) .ge. '0' .and. astr(i:i) .le. '9') then
            l1=.true.
            CNUM = CNUM*10.+float(index(nums,astr(i:i))-1)
c            if (i .eq. ilen) ifield=ifield+1
          endif
           if (i .eq. ilen) ifield=ifield+1
        else
          if (l1) then
            ifield=ifield+1
            l1=.false.
            cnum=0.0
          endif
        endif
      enddo
     
c     if correct number of fields found, read dates
      if (ifield .eq. 4) then
        read(astr(1:ilen),*)startmon,startyear,endmon,endyear
        if (startyear .lt. 1000) then
          write(*,10)
          write(ilogunit,10)
          lstop=.true.
          goto 99
        endif
        if (endyear .lt. 1000) then
          write(*,11)
          write(ilogunit,11)
          lstop=.true.
          goto 99
        endif
      else
        lstop=.true.
        write(*,6)
        write(ilogunit,6)
        goto 99
      endif
      
c     make sure dates are realistic
c     check that start month is between 1 and 12
      if (startmon .lt. 1 .or. startmon .gt. 12) then
        write(*,7)startmon
        write(ilogunit,7)startmon
        lstop=.true.
      else
        startday=1
        startdate=(startyear*10000)+(startmon*100)+startday
      endif
c     check that end month is between 1 and 12
      if (endmon .lt. 1 .or. endmon .gt. 12) then
        write(*,8)endmon
        write(ilogunit,8)endmon
        lstop=.true.
      else
        endday=days(endmon)
c       if end month is February, check to see if end year is leap year
        if (endmon .eq. 2) then 
          call leapyr(endyear,leap)
          if (leap) endday=days(endmon)+1
        endif
        enddate=(endyear*10000)+(endmon*100)+endday        
      endif
      
c     if dates are okay so far, make sure the end date is 
c     not less than startdate
      if (.not. lstop) then
        if (enddate .lt. startdate) then
          write(*,9)startmon,startday,startyear,endmon,endday,endyear
          lstop=.true.
        endif
      endif 
 
 5    format(1x,'Invalid characters found in STARTEND string') 
 6    format(1x,'Incorrect number of fields found for STARTEND dates')
 7    format(1x,'Start month ',i2,' is not a valid month')
 8    format(1x,'End month ',i2,' is not a valid month')
 9    format(1x,'Start date:',2(1x,i2),1x,i4,' is later than'/,1x,
     +  'end date:',2(1x,i2),1x,i4)
10    format(/1x,'Start year is less than 1000, check file')
11    format(/1x,'End year is less than 1000, check file')
 99   return
      end
c*************************************************************
      subroutine checkifw(astr,lstop,ifwmon,ifwday,ifwyear,ilen)
c     subroutine to check the IFWGROUP string
c     and the presence of y, Y, n, or N for status
c     and if yes, then check for date and compare against 
c     enddate.  If status is yes and commission date is
c     after enddate, reset status to no.

      use main1
      implicit none

c lstop:  logical variable denoting that a problem has been
c         found with dates strings
c leap:   logical variable denoting if start or end year is
c         a leap year.  Used only when end month is February
c l1:     logical variable denoting that a text string representing
c         a number has been found
c lchar:  logical variable denoting that a non-numeric or nonblank
c         character found in string astr
c astr:   character string being checked for IFW status
c astr1:  character string being checked for IFW date
c nums:   character string of numbers 0 through 9
c lastday:  integer last day of month
c ilen:   length of astr and astr1
c i:      integer loop counter
c n:      integer index of 'Y', 'y', 'N', or 'n' in astr
c ind:      Integer function return index value of astr
c ifield: integer number of numeric strings found
c ifwmon:   integer IFW commission month
c ifwday:   integer IFW commission day
c ifwyear:  integer IFW commission year
c i1:       counter
c ilen:   integer actual length of string astr
c cnum:   real representation of numbers being read 

      logical lstop,l1,leap,lchar
      integer lastday,ilen,i,n,ind,ifield,ifwmon,ifwday,ifwyear,i1
      character astr*(*),astr1*300,nums*10
      real cnum
      nums='0123456789'
      cnum=0.0
      ifield=0
      lchar=.false.
      l1=.false.
      leap=.false.
c     check for non-numeric characters in string
c     if found, stop program
      i1=33
      do while (.not. lchar .and. i1 .le. 126)
        if (index(astr,char(i1)) .gt. 0) lchar=.true.
        if (i1 .eq. 77 .or. i1 .eq. 88 .or. i1 .eq. 109 .or. 
     +  i1 .eq. 120) then
          i1=i1+2
        elseif (i1 .eq. 47) then
          i1=58
        else
          i1=i1+1
        endif
      enddo
      if (lchar) then
        write(*,5)
        write(ilogunit,5)
        lstop=.true.
        goto 99
      endif
            
c     check status, should be a y or n
      n=index(astr,'Y')
      if (n .eq. 0) n=index(astr,'y')
      if (n .gt. 0) then
        astr1=astr(n+1:ilen)
        ilen=ilen-n
c        ilen=len(astr1)
        do i=1,ilen
          if (astr1(i:i) .ne. ' ') then
            if (astr1(i:i) .ge. '0' .and. astr1(i:i) .le. '9') then
              l1=.true.
              CNUM = CNUM*10.+float(index(nums,astr1(i:i))-1)
              if (i .eq. ilen) ifield=ifield+1
            endif
c            if (i .eq. ilen) ifield=ifield+1
          else
            if (l1) then
              ifield=ifield+1
              l1=.false.
              cnum=0.0
            endif
          endif
        enddo

c       if correct number of fields found, get IFW commission date
        if (ifield .eq. 3) then
          read(astr1,*)ifwmon,ifwday,ifwyear
          if (ifwyear .lt. 1000) then
            write(*,10)
            write(ilogunit,10)
            lstop=.true.
            goto 99
          endif
        else
          lstop=.true.
          write(*,6)
          write(ilogunit,6)
          goto 99
        endif  
 
c       make sure month is between 1 and 12        
        if (ifwmon .lt. 1 .or. ifwmon .gt. 12) then
          write(*,7)ifwmon
          write(ilogunit,7)ifwmon
          lstop=.true.
        else
          lastday=days(ifwmon)
          if (ifwmon .eq. 2) then
            call leapyr(ifwyear,leap)
            if (leap) lastday=lastday+1
          endif
c         make sure day is between first and last day of month
          if (ifwday .lt. 1 .or. ifwday .gt. lastday) then
            write(*,8)ifwday,lastday,trim(adjustl(amonths(ifwmon)))
            write(ilogunit,8)ifwday,lastday,
     +      trim(adjustl(amonths(ifwmon)))
            lstop=.true.
          endif
        endif
      
c     calculate IFW date
        if (.not. lstop) then
          ifwdate=(ifwyear*10000)+(ifwmon*100)+ifwday
          if (ifwdate .gt. enddate) then   !outside range
            ifw=.false.
          else
            ifw=.true.
          endif
        endif
      else
        n=index(astr,'N')
        if (n .eq. 0) n=index(astr,'n')
        if (n .gt. 0) then
          ifw=.false.
        else
          write(*,9)
          write(ilogunit,9)
          lstop=.true.
        endif
      endif
  5    format(/1x,'Invalid characters found in IFWGROUP string')
  6   format(/1x,'Incorrect number of fields found for IFW date')    
  7   format(/1x,'IFW commission month ',i2,' is not a valid month')
  8   format(/1x,'IFW commission day ',i2,
     +  ' is greater than the last day ',i2,' of ',a)
  9   format(/1x,'IFWGROUP incorrectly formatted')
  10  format(/1x,'IFW commission year is less than 1000, check file')
 99   return
      end
c*************************************************************   
      subroutine checkblank(str,nonblank)
c     subroutine to check a character string
c     to make sure it is not all blank
c     check for upper and lowercase letters, numbers,
c     and symbols, i.e. the ASCII table after number 32
      use main1
      implicit none

c str       character string being checked
c nonblank  logical variable denoting string has at
c           least one nonblank character

      character str*(*)
      logical nonblank
      nonblank=.false.
      if( LEN_TRIM(str) .GT. 0 ) nonblank=.true.
      
      return
      end
      
c*************************************************************             
c subroutine to initialize arrays
      subroutine initial
      use main1
      implicit none

c i:        integer month counter
c imon:     integer month of year
c iyear:    integer 4 digit year
c leap:     logical variable denoting if year is leap year

      integer i,imon,iyear
      logical leap
      
      write(*,1)

      
  1   format(1x,'Initializing arrays...'/)
  
      leap=.false.
c get number of months to be processed   
      nmonths=(12-startmon+1)+((endyear-startyear-1)*12)+endmon
   
c allocate arrays   
      allocate(ndays(nmonths)) 
      allocate(idate(nmonths,31))
      allocate(origdate(nmonths,31,24,60))
      allocate(ikeephr(nmonths,31,24))
      allocate(ikeep(nmonths,31,24,60))
      allocate(icalm(nmonths,31,24,60))
      allocate(idirmin(nmonths,31,24,60))
      allocate(nvalid(nmonths,31,24))
      allocate(ncalms(nmonths,31,24))
      allocate(neven(nmonths,31,24))
      allocate(nevencalm(nmonths,31,24))
      allocate(nodd(nmonths,31,24))
      allocate(noddcalm(nmonths,31,24))
      allocate(noddperm(nmonths,31,24))
      allocate(noddpermc(nmonths,31,24))
      allocate(speed(nmonths,31,24))
      allocate(speed1(nmonths,31,24,60))
      allocate(speedkts(nmonths,31,24,60))
      allocate(xdir(nmonths,31,24,60))
      allocate(ydir(nmonths,31,24,60))
      allocate(dirminut(nmonths,31,24,60))
      allocate(xdirhr(nmonths,31,24))
      allocate(ydirhr(nmonths,31,24))
      allocate(dirhr(nmonths,31,24))
      allocate(speedmin(nmonths,31,24))
      allocate(speedmax(nmonths,31,24))
      allocate(dirmin(nmonths,31,24))
      allocate(dirmax(nmonths,31,24))
      allocate(srcmin(2,nmonths,31,24))
      allocate(srcmax(2,nmonths,31,24))
!      allocate(nhours(nmonths,5))
!      allocate(nhours(nmonths,4))
c     allocate arrays for comparisons between standard 
c     obs and one minute data
      if (comp) then
        allocate(obspd(nmonths,31,24,60))
        allocate(obdir(nmonths,31,24,60))
        allocate(flag(nmonths,31,24,60))
        allocate(qcflag(nmonths,31,24,60))
        obspd=-9.0
        obdir=-9.0
      endif   
c 15272
      allocate(minsrc(nmonths,31,24,60))
      if (lfive) then
          allocate(nvalid5(nmonths,31,24))
          allocate(neven5(nmonths,31,24))
          allocate(nodd5(nmonths,31,24))
          allocate(ikeep5(nmonths,31,24,12))
          allocate(icalm5(nmonths,31,24,12))
          allocate(idir5min(nmonths,31,24,12))
          allocate(speed5(nmonths,31,24,12))
          allocate(xdir5(nmonths,31,24,12))
          allocate(ydir5(nmonths,31,24,12))
          allocate(dir5minut(nmonths,31,24,12))
          allocate(nhours(nmonths,6))
          allocate(speed5kts(nmonths,31,24,12))
          allocate(origdate5(nmonths,31,24,12))
          ikeep5=-9
          icalm5=0
          idir5min=0
          nvalid5=0
          xdir5=-999.0
          ydir5=-999.0
          dir5minut=-999.0
          nvalid5=0
          neven5=0
          nodd5=0
          speed5kts=-999.0
      else
          allocate(nhours(nmonths,5))
      endif
c     now initialize the ndays array
c     and calculate # of hours in data period
      itothrs=0
      do i=1,nmonths
        if (i .eq. 1) then
          imon=startmon
          iyear=startyear
        else if (i .eq. nmonths) then
          imon=endmon
        else
          if (imon .eq. 12) then
            imon=1
            iyear=iyear+1
          else
            imon=imon+1
          endif
        endif     
        if (imon .eq. 2) then
          call leapyr(iyear,leap)
          if (leap) then 
c             ndays(i)=29
            ndays(i)=days(imon)+1
          else 
c             ndays(i)=28
            ndays(i)=days(imon)
          endif
        else
         ndays(i)=days(imon)
c           if (imon .eq. 4 .or. imon .eq. 6 .or. imon .eq. 9 .or. 
c     +      imon .eq. 11) then
c             ndays(i)=30
c           else
c             ndays(i)=31
c           endif
        endif
        itothrs=itothrs+(ndays(i)*24)
      end do
      
            
c set initial values 
      ikeephr=-9
      idate=0
      speed=0.0
      xdirhr=0.0
      ydirhr=0.0
      speed1=-999.0
      speedkts=-999.0
      dirminut=-999.0
      xdir=-999.0
      ydir=-999.0
      ikeep=-9
      dirhr=-999.0
      speedmin=0.0
      speedmax=0.0
      dirmin=0
      dirmax=0
      nvalid=0
      idirmin=-9
      ncalms=0
      neven=0
      nevencalm=0
      nodd=0
      noddcalm=0
      noddpermc=0
      noddperm=0
      srcmin=-9
      srcmax=-9
      minsrc=1
      lwban=.false.
      badwban=.false.
      lstat=.false.
      irec=0
      flagcounts=0
      reccounts=0
      nhours=0
      wbancnt=0
      statcnt=0
      return
      end
c*************************************************
c subroutine to check for existence of input files
c and assign to array infiles
c version 15272, modify to read 1-minute and 5-minute filelists
      subroutine checkfiles(imin)
      use main1
      implicit none

c imin:     integer to read 1-minute or 5-minute list of datafiles
c eof:      integer end-of-file indicator
c i:        integer counter
c n:        file counter
c ifiles:   # of files read from inputfiles.txt
c imin:     integer indicator of data source (1=1-minute data,5=5-minute data)
c ngood:    value of index function of fname when looking for good_records.dat or GOOD_RECORDS.DAT
c nbad:     value of index function of fname when looking for bad_records.dat or BAD_RECORDS.DAT
c ncheck:   value of index function of fname when looking for check_records.dat or CHECK_RECORDS.DAT
c fname:    character variable of individual input file and assigned to infiles
c lkeep:    logical variable denoting existence of individual one-minute files read from inputfiles.txt
c lbad:     logical variable denoting that a file is named good_records.dat, bad_records.dat, or check_records.dat
c           or upper case version.  Files cannot be named one of those files
c ldup1:    logical variable denoting that duplicate filenames have been found
      integer eof,ifiles,ngood,nbad,ncheck,n,i,imin
      character fname*250
      logical lkeep,lbad,ldup1
      
      if (imin .eq. 1) then
          lkeep1=.true.
          lbad=.false.
          lbad1=.false.
      else
          lkeep5=.true.
          lbad1=.false.
      endif
      
      ldup1=.false.
      lbad=.false.
      write(*,2)imin
      write(ilogunit,2)imin

c     check each file listed to see if it exists
      ifiles=0

      if (imin .eq. 1) then
          open(11,file='datafiles.tmp',status='old')
      else
          open(11,file='datafiles5.tmp',status='old')
      endif
      read(11,*,iostat=eof)fname
 10   if (eof .eq. 0) then
c       check for file existence
        inquire(file=fname,exist=lkeep)
        if (.not. lkeep) then
          if (imin .eq. 1) then
              if (lkeep1) lkeep1=.false.
          else
              if (lkeep5) lkeep5=.false.
          endif
          write(*,15)trim(adjustl(fname))
          write(ilogunit,15)trim(adjustl(fname))
c           stop
        else
c         check to make sure fname is not good_records.dat, bad_records.dat, or check_records.dat (upper or lowercase)
          call checkname(fname,ngood,nbad,ncheck)
          if (max(ngood,nbad,ncheck) .gt. 0) then
            write(*,16)trim(adjustl(fname))
            write(ilogunit,16)trim(adjustl(fname))
            lbad=.true.
            if (imin .eq. 1) then
              if (.not. lbad1) lbad1=.true.
            else
              if (.not. lbad5) lbad5=.true.
            endif
          else
            ifiles=ifiles+1
          endif
        endif
        read(11,*,iostat=eof)fname
        goto 10
      endif
      
c one or more files do not exist 
      if (imin .eq. 1) then 
          if (.not. lkeep1) then
            write(*,17)
            write(ilogunit,17)
            goto 50
          endif
      else
          if (.not. lkeep5) then
            write(*,17)
            write(ilogunit,17)
            goto 50
          endif
      endif
c one or more files has the name good_records.dat, bad_records.dat, or check_records.dat    
      if (imin .eq. 1) then
          if (lbad1) then
            write(*,18)
            write(ilogunit,18)
            goto 50
          endif
      else
          if (lbad5) then
            write(*,18)
            write(ilogunit,18)
            goto 50
          endif
      endif 
c allocate infiles and rewind inputfiles.txt      
      if (imin .eq. 1) then
          nfiles=ifiles
          allocate(infiles(nfiles))
      else
          nfiles5=ifiles
          allocate(infiles5(nfiles5))
      endif
      rewind(11)
      eof=0
      ifiles=0
      
c     read inputfiles.txt again to get the filenames and
c     assign to infiles
      read(11,*,iostat=eof)fname
  20  if (eof .eq. 0) then
        ifiles=ifiles+1
        if (imin .eq. 1) then
            infiles(ifiles)=fname
        else
            infiles5(ifiles)=fname
        endif
        read(11,*,iostat=eof)fname
        goto 20
      endif
      
c     check for duplicate filenames
      write(*,45)
      write(ilogunit,45)

      if (imin .eq. 1) then
          do i=2,nfiles
            ldup1=.false.
            n=1
              do while (n .lt. i .and. .not. ldup1)
                if (infiles(i) .eq. infiles(n)) ldup1=.true.  !duplicate filename
                n=n+1
              enddo 
              if (ldup1) then
                write(*,40)trim(adjustl(infiles(i)))
                write(ilogunit,40)trim(adjustl(infiles(i)))
                ldup=ldup1
c            goto 50  !exit subroutine
              endif
           enddo
      else
          do i=2,nfiles5
            ldup1=.false.
            n=1
              do while (n .lt. i .and. .not. ldup1)
                if (infiles5(i) .eq. infiles5(n)) ldup1=.true.  !duplicate filename
                n=n+1
              enddo 
              if (ldup1) then
                write(*,40)trim(adjustl(infiles5(i)))
                write(ilogunit,40)trim(adjustl(infiles5(i)))
                ldup5=ldup1
              endif
          enddo
      endif
      

        
c     write message that there are no duplicate filenames 
      if (imin .eq. 1) then
          if (.not. ldup) then
              write(*,55)
              write(ilogunit,55)
              write(*,3)nfiles
              write(ilogunit,3)nfiles
          endif 
      else
          if (.not. ldup5) then
              write(*,55)
              write(ilogunit,55)
              write(*,3)nfiles5
              write(ilogunit,3)nfiles5
          endif 
      endif
      
      write(*,56)
      write(ilogunit,56)
      close(11,status='delete')
 2    format(/1x,'########################',1x,i1,1x,
     + '-MINUTE DATA FILES ','CHECK ','######################') 
 3    format(/1x,'All files found...'/1x,'Number of files to process: ',
     +  i3)
15    format(/1x,a,' does not exist')
16    format(/1x,' Filename ',a,' is not allowed'/1x,
     +   'Files cannot be named good_records.dat, bad_records.dat, ',
     +   'or check_records.dat')
17    format(/1x,'One or more data files do not exist, ',
     +  'stopping program')
18    format(/1x,'One or more data files is named good_records.dat, ',
     +  'bad_records.dat, or check_records.dat, '/1x,'stopping program')
 40   format(/1x,'ERROR: One-minute file ',a,' is a duplicate'/1x,
     +  'Correct input list')
 45   format(/1x,'Checking for duplicate filenames')
 55   format(/1x,'No duplicate filenames found')  
 56   format(/1x,'###################################################',
     +  '#####################')  
 50   return
      end
c*************************************************
c subroutine to check input filenames to compare against
c good_records.dat, bad_records.dat, and check_records.dat
      subroutine checkname(fname,ng,nb,nc)
c ng:     value of index function of fname when looking for good_records.dat or GOOD_RECORDS.DAT
c nb:     value of index function of fname when looking for bad_records.dat or BAD_RECORDS.DAT
c nc:     value of index function of fname when looking for check_records.dat or CHECK_RECORDS.DAT
c n1:     location of character in fname before good_records.dat string
c n2:     location of character in fname before bad_records.dat string
c n3:     location of character in fname before check_records.dat string
c fname:  character variable of individual input file
      integer ng,nb,nc,n1,n2,n3
      character fname*(*)
     
      ng=index(fname,'good_records.dat')
      nb=index(fname,'bad_records.dat')
      nc=index(fname,'check_records.dat')
      if (ng .eq. 0) then 
        ng=index(fname,'GOOD_RECORDS.DAT')
      else
        n1=ng-1
c check to see if good_records.dat is part of a filename
        if (n1 .gt. 0 .and. fname(n1:n1) .ne. '\' .and. 
     +  fname(n1:n1) .ne. '"') then
          ng=0
        endif
      endif
      if (nb .eq. 0) then 
        nb=index(fname,'BAD_RECORDS.DAT')
      else
        n2=nb-1
c check to see if bad_records.dat is part of a filename
        if (n2 .gt. 0 .and. fname(n2:n2) .ne. '\' .and. 
     +  fname(n2:n2) .ne. '"') then
          nb=0
        endif
      endif
      if (nc .eq. 0) then 
        nc=index(fname,'CHECK_RECORDS.DAT')
      else
        n3=nc-1
c check to see if bad_records.dat is part of a filename
        if (n3 .gt. 0 .and. fname(n3:n3) .ne. '\' .and. 
     +  fname(n3:n3) .ne. '"') then
          nc=0
        endif        
      endif
      return
      end
          
c*************************************************    
c subroutine to read one minute data file
      subroutine readone
      
      use main1
      implicit none

c modified version 14237 to accomodate new flag
c line:     character string of line of data read from one minute data
c03017KDEN DEN2003010100000700   0.091 N                 0.099 N     21      9    25   10   35L60+
c i:        integer file counter
c eof:      integer end-of-file indicator
c totrecs:  integer number of total records read
c ii:       integer counter

      character line*113,flagstr(12)*200,dates(4)*10,adate*8
      integer i,eof,totrecs,ii,ij,idate3(4)
      
c     flag text strings
c     modified for version 14337
      data flagstr /'Non-numeric characters in columns 68 through 90',
     +  'Text string with leading zero in columns 66 through 90',
     +  '4-character numeric string found in columns 30 through 113',
     +  'Column 30 is non-blank',
     +  'No number in columns 70-74 for 2-minute wind direction',
     +  'No number in columns 76-79 for 2-minute wind speed',
     +'No number in columns 82-84 for 5-second gust wind direction',
     +  'No number in columns 87-89 for 5-second gust wind speed',
     +  'Number in column 90',
     +  'Wind speeds and directions outside allowable ranges',
     +  'Other flags switched on, but winds may be okay',
     +  'Other flags switched on, but record is bad'/
c     modified for version 14237
!      data flagstr /'Non-numeric characters in columns 67 through 90',
!     +  'Text string with leading zero in columns 66 through 90',
!     +  '4-character numeric string found in columns 30 through 113',
!     +  'Column 30 is non-blank',
!     +  'Column 67 is non-blank',
!     +  'No number in columns 70-74 for 2-minute wind direction',
!     +  'No number in columns 76-79 for 2-minute wind speed',
!     +'No number in columns 82-84 for 5-second gust wind direction',
!     +  'No number in columns 87-89 for 5-second gust wind speed',
!     +  'Non-blank character in columns 64-68',
!     +  'Number in column 90',
!     +  'Wind speeds and directions outside allowable ranges',
!     +  'Other flags switched on, but winds may be okay',
!     +  'Other flags switched on, but record is bad'/
      
!      data flagstr /'Non-numeric characters in columns 67 through 90',
!     +  'Text string with leading zero in columns 66 through 90',
!     +  '4-character numeric string found in columns 30 through 113',
!     +  'Column 30 is non-blank',
!     +  'Column 67 is non-blank',
!     +  'No number in columns 70 and 71 for 2-minute wind direction',
!     +  'No number in columns 76 and 77 for 2-minute wind speed',
!     +'No number in columns 82 and 83 for 5-second gust wind direction',
!     +  'No number in columns 87 and 88 for 5-second gust wind speed',
!     +  'Wind speeds and directions outside allowable ranges',
!     +  'Other flags switched on, but winds may be okay',
!     +  'Other flags switched on, but record is bad'/
      
!      data flagstr /'Non-numeric characters in columns 67 through 90',
!     +  'Text string with leading zero in columns 66 through 90',
!     +  '4-character numeric string found in columns 30 through 113',
!     +  'Letters D,M, or N not found in column 39, 40, or 41',
!     +  'Number in column 67',
!     +  'No number in columns 70 and 71 for 2-minute wind direction',
!     +  'No number in columns 76 and 77 for 2-minute wind speed',
!     +'No number in columns 82 and 83 for 5-second gust wind direction',
!     +  'No number in columns 87 and 88 for 5-second gust wind speed',
!     +  'Wind speeds and directions outside allowable ranges',
!     +  'Other flags switched on, but winds may be okay',
!     +  'Other flags switched on, but record is bad'/

c     begin reading files
      write(*,14)
      write(ilogunit,14)
      
c open output, error, good record, and summary files     

      open(unit=ierrunit,file=errfil,status='replace')
      open(unit=igdunit,file=goodfil,status='replace')
      open(unit=isusunit,file=suspfil,status='replace')
      
c     put numbers in the columns of errfile and goodfile where wind data should be
      write(ierrunit,19)
      write(igdunit,19)  
      write(isusunit,19)
      
      lnodata=.true.
      baddates(1)=99999999
      baddates(2)=0
      do i=1,nfiles
        eof=0
        open(unit=inunit,file=infiles(i),status='old')
        write(*,15)trim(adjustl(infiles(i)))
        write(ilogunit,15)trim(adjustl(infiles(i)))
        read(inunit,5,iostat=eof)line
10      if (eof .eq. 0) then
c          call the subroutine to read and QA a line of data
          call readline(line)
          if (lwban) goto 99
            
          read(inunit,5,iostat=eof)line
          goto 10
        endif
        close(inunit)
      enddo
      write(*,20)
      write(ilogunit,20)

c if no data has been read within the window set by start and end dates,
c alert user and abort program
      if (lnodata) then
        do ij=1,4
          if (ij .le. 2) then
            idate3(ij)=baddates(ij)
          elseif (ij .eq. 3) then
            idate3(ij)=startdate
          else
            idate3(ij)=enddate
          endif
          write(adate,'(i8)')idate3(ij)
          write(dates(ij),'(2(a2,a1),a4)')adate(5:6),' ',adate(7:8),' ',
     +    adate(1:4)
        enddo
        write(*,24)dates(1),dates(2),dates(3),dates(4)
        write(ilogunit,24)dates(1),dates(2),dates(3),dates(4)
c      else
      endif
c version 09295, summary of record counts
        totrecs=sum(reccounts)
        totrecs=reccounts(1)+reccounts(4)+reccounts(5)+reccounts(6)+
     +  reccounts(7)
        write(ilogunit,21)totrecs,reccounts(1),reccounts(2),
     +   reccounts(3),
     +   reccounts(4)+reccounts(5)+reccounts(6)+reccounts(7),
     +   reccounts(4),reccounts(5),reccounts(6),reccounts(7)
     
c version 09295, summary of flag counts
        write(ilogunit,22)
        do ii=1,12
          if (ii .lt. 12) then 
            write(ilogunit,23)ii,trim(adjustl(flagstr(ii))),
     +      flagcounts(ii)
          else
            write(ilogunit,23)ii-1,trim(adjustl(flagstr(ii))),
     +      flagcounts(ii)
          endif
        enddo
c      endif
      
      write(*,25)
      write(ilogunit,25)

 
      close(ierrunit)
      close(igdunit)  
      close(isusunit)
      
      
      
 5    format(a113)
 14   format(/1x,'######################## 1-MIN FILE PROCESSING ###',
     +    '######################')
 19   format(67x,'123456 1234  123  1234')               
 15   format(/1x,'Reading',1x,a)
 20   format(/1x,'All files read...')
 21   format(/1x,'RECORD FORMAT QA'//1x,
     +  'Total number of records read from files:',t49,i10/1x,
     +  'Number of processed records:',t49,i10/1x,
     +  'Number of records outside data period:',t49,i10/1x,
     +  'Number of records inside data period:',t49,i10/1x,
     +  'Number of non-processed records:',t49,i10/1x,
     +  'Number of records for minute 1:',t49,i10/1x,
     +  'Number of bad records:',t49,i10/1x,
     +  'Number of check records:',t49,i10,/1x,
     +  'Number of bad minute records:',t49,i10/,1x,
     +  'NOTE: Number of non-processed records includes records ',
     +  'outside data period'/)
 22   format(1x,'QA FLAG SUMMARY COUNT'/
     + 1x,'FLAG',t28,'DESCRIPTION',t77,'RECORDS')
 23   format(1x,i2,t8,a,t74,i10)
 24   format(/1x,'NO DATA IN FILES WITHIN DATA PERIOD SET BY USER'//
     + 1x,'DATE RANGE IN FILES: ',a10,' THROUGH ',a10//
     + 1x,'DATE RANGE SET BY USER: ',a10,' THROUGH ',a10//1x
     + 'CHECK DATES IN STARTEND OR DATAFILE LIST'/1x,'STOP PROGRAM')
 25   format(/1x,'###################################################',
     +  '#####################')   
 
 99   return
      end
c***************************************************************
c subroutine to process a line
      subroutine readline(line)
      use main1
      implicit none

c iflag:    integer indicator of validity of record (0=valid,1=invalid) 
c imin:     integer minute of data being read from record
c iyear:    4-digit integer year of data record
c imonth:   2-digit integer month of data record
c iday:     2-digit integer day of month of data record
c ihr:      2-digit integer hour of day (00-23)
c idate1:   integer date of record (YYYYMMDDHHMM)  
c idate2:   character string (length 12) of original date of record (YYYYMMDD)      
c iwban1:   integer WBAN number of station read from data record
c           this number replaces the WBAN entered by user 
c           and program issues warning that WBAN has changed
c idir:     integer 3-digit 2-minute average wind direction read from record
c ispeed:   integer average 2-minute wind speed (knots) read from record
c n:        integer month counter relative to starting month of data period
c           for firsth month of data n=1, 2nd month=2, etc.
c rmin:     real equivalent of imin
c frac:     real result of MOD equation used to determine if minute is even or odd
c line:     record being read and processed (113 length character string)
c aflag:    11-character string of flags from checkwind
c stat1:    4-character string of station call sign
      integer imin,idir,iflag,iyear,imonth,iday,ihr,idate1,iwban1,
     + ispeed,n
      real rmin,frac
      
      character line*113,idate2*12,aflag*11,stat1*4
     
c     read minute of record, convert to real, and get frac      
      read(line(24:25),'(i2)')imin
      rmin=real(imin)     
      frac=mod(rmin,2.0)
      
c do not process minute 1 as it straddles 2-hours      
      if (frac .ge. 0.0 .and. imin .ne. 1) then
c       perform QA on segment of record that contains wind speed and direction
        call checkwind(line,iflag,aflag,idir,ispeed)
c       if iflag = 0, record appears valid, continue processing
        if (iflag .eq. 0) then
          irec=irec+1
           reccounts(1)=irec
c         write line to file of valid records for later check     
          write(igdunit,7)line  
c         read WBAN # and check against user entered WBAN
c         change to iwban1 and warn user at end of program
c         version 15272, check to make sure WBAN section is all integers
          iwban1=0
          call checkwban(line(1:5),iwban1)
          if (badwban) then
              write(*,9)
              write(ilogunit,9)
              goto 5
          endif
c          read(line(1:5),'(i5)')iwban1 
          stat1=line(6:9)
          if (irec .eq. 1) then 
            iwban=iwban1
            iwban2=iwban1
            stat=stat1
            stat2=stat1
          endif
c         if WBAN changes, notify user and stop program
          if (iwban1 .ne. iwban) then
            lwban=.true.
              write(*,6)iwban,iwban1
              write(ilogunit,6)iwban,iwban1
              goto 5
          endif
          if (stat1 .ne. stat) then
            stat=stat1
            lstat=.true.
            statcnt=statcnt+1
            if (statcnt .le. 10) stats(statcnt)=stat1
          endif
c         get year, month, day, and hour of record
          read(line(14:23),'(i4,3i2)')iyear,imonth,iday,ihr
c          get date before adding 1 hour
          write(idate2,'(i4,4i2.2)')iyear,imonth,iday,ihr,imin
           
c         hourly averages will be considered hour ending. so add 1 to hour
c         and change date accordingly (hour 0 is hour 24 of day before)
          call changehr(iyear,imonth,iday,ihr,imin)
c         calculate date variable 
c         example January 1, 2001 would be 20010101
c         check against the start and end dates
c         and if outside the range exit subroutine
          idate1=(iyear*10000)+(imonth*100)+iday
          if (startdate .gt. idate1 .or. idate1 .gt. enddate) then
            baddates(1)=min(baddates(1),idate1)
            baddates(2)=max(baddates(2),idate1)  
            reccounts(2)=reccounts(2)+1
            goto 5
          else
            reccounts(3)=reccounts(3)+1
          endif
          lnodata=.false.
c         calculate month relative to start month
c         if current month is March 2002 and start month is January 2001
c         then n = 15
          n=(iyear-startyear)*12+imonth-(startmon)+1
c         assign idate1 to date array
c         example:  for March 7, 2002 with start date of January 2001 n=15 iday=7
          idate(n,iday)=idate1
          origdate(n,iday,ihr,imin)=idate2
      
c          check speeds
c          if wind speed is less than 2 knots
c          check to see if ASOS is not part of the IFW group or if the date
c          of the current record is before the IFW group date date
c          if wind speed is less than 2 knots and station is not part of IFW at all 
c          or at time of record, set speed to 1 knot (0.51 m/s), set the keep flag for the 
c          minute to 0 the calm flag for the minute to 1.
c          if wind speed is less than 2 knots and the station is part of IFW at the time of the record
c          then set the keep flag for the minute to 1 and convert the speed to m/s. 
c          if the wind speed is 2 knots or greater, set the keep flag to 1 and convert speed
c          to m/s
          if (ispeed .lt. 2) then
            if (.not. ifw .or. ifwdate .gt. idate1) then  
              ikeep(n,iday,ihr,imin)=0                  !this minute will not be used in wind direction averages
              icalm(n,iday,ihr,imin)=1
              speed1(n,iday,ihr,imin)=0.51              !set wind speed to 1 knot (0.51 m/s)
            else
              ikeep(n,iday,ihr,imin)=1  
              icalm(n,iday,ihr,imin)=0         
              speed1(n,iday,ihr,imin)=(real(ispeed)+trunc)*0.51    
            endif
          else
            speed1(n,iday,ihr,imin)=(real(ispeed)+trunc)*0.51
            ikeep(n,iday,ihr,imin)=1
            icalm(n,iday,ihr,imin)=0
          endif
          speedkts(n,iday,ihr,imin)=real(ispeed)
c calculate x and y components of wind direction
c regardless of the value of the keep flag or calm flag
          dirminut(n,iday,ihr,imin)=real(idir)
          xdir(n,iday,ihr,imin)=-1.0*sin(real(idir)*d2r)   !get x and y components of direction
          ydir(n,iday,ihr,imin)=-1.0*cos(real(idir)*d2r)
          idirmin(n,iday,ihr,imin)=idir
      else
c if a bad record (iflag > 0), write to bad records file for later checking
          if (aflag(11:11) .eq. '4' .or. aflag(11:11) .eq. '5') then
            write(isusunit,8)line,aflag
            reccounts(6)=reccounts(6)+1
          else
            write(ierrunit,8)line,aflag
            reccounts(5)=reccounts(5)+1
          endif
        endif    
      else
        if (imin .eq. 1) then
            reccounts(4)=reccounts(4)+1
        else
            reccounts(7)=reccounts(7)+1
        endif    
      endif

 6    format(/1x,'WBAN number has changed from ',i5,1x,'to',1x,i5/1x,
     +  'Stopping AERMINUTE')
 7    format(a113)
 8    format(a113,1x,a13)
 9    format(/1x,'Bad WBAN string in file, stopping AERMINUTE')
 5    return
      end
c*******************************************************************************
c     subroutine 15272
c     subroutine to check WBAN section of line to make sure
c     all integers
      subroutine checkwban(awban,iwban1)
      use main1
      implicit none

c iwban1:     integer input WBAN from data file
c i:          loop counter, each iteration of i is
c             digit of WBAN
c j:          ASCII table value of character i of WBAN
c awban:      character string of WBAN

      integer iwban1,i,j
      character awban*5
      
      i=1
      do while(i .le. 5 .and. .not. badwban)
          j=iachar(awban(i:i))
          if (j .lt. 48 .or. j .gt. 57) badwban=.true.
          i=i+1
      enddo
      
      if (.not. badwban) read(awban,'(i5)')iwban1
      return
      end
c*******************************************************************************
c     version 15272- subroutine to read 5 minute data
      subroutine readfive
      use main1
      implicit none

c line:       line of data from 5-minute data file
c dates:      4x1 character array of dates.  1st and 2nd are the dates
c             in files.  last two are start/end dates set by
c             user.  character version of idate3
c adate:      character string of date
c i:          file loop counter
c eof:        end-of-file indicator
c idate3:     4x1 array of integer dates. 1st and 2nd are the dates
c             in files.  last two are start/end dates set by
c             user.
c ij:         date counter 1 to 4.

      character line*200,dates(4)*10,adate*8
      integer i,eof,idate3(4),ij
      
c     begin reading files
      write(*,14)
      write(ilogunit,14)
      lnodata5=.true.
      baddate5(1)=99999999
      baddate5(2)=0
      
      open(unit=ierrunit5,file=errfil5,status='replace')
      open(unit=igdunit5,file=goodfil5,status='replace')
      open(unit=iclmvar5,file=clmvar5,status='replace')
      if (comp15) then 
          open(unit=icomp5,file=comp5fil,status='replace')
          write(icomp5,16)
      endif
      
      do i=1,nfiles5
          eof=0
          open(unit=inunit5,file=infiles5(i),status='old')
          write(*,15)trim(adjustl(infiles5(i)))
          write(ilogunit,15)trim(adjustl(infiles5(i)))
          read(inunit5,5,iostat=eof)line
 10       if (eof .eq. 0) then
              call read5(line)
              if (lwban) goto 99
              read(inunit5,5,iostat=eof)line
              goto 10
          endif
          close(inunit5)
      enddo
      write(*,20)
      write(ilogunit,20)

c no data within window stet by start and end dates,
c alert user no data substitution will occur
      if (lnodata5) then
        do ij=1,4
          if (ij .le. 2) then
            idate3(ij)=baddate5(ij)
          elseif (ij .eq. 3) then
            idate3(ij)=startdate
          else
            idate3(ij)=enddate
          endif
          write(adate,'(i8)')idate3(ij)
          write(dates(ij),'(2(a2,a1),a4)')adate(5:6),' ',adate(7:8),' ',
     +    adate(1:4)
        enddo
        write(*,24)dates(1),dates(2),dates(3),dates(4)
        write(ilogunit,24)dates(1),dates(2),dates(3),dates(4)
        run5=.false.  !no substitution will occur
      endif
      
      close(ierrunit5)
      close(igdunit5)
      close(iclmvar5)
      if (comp15) close(icomp5)
      
      
5     format(a200)
14    format(/1x,'######################### 5-MIN FILE PROCESSING ##',
     +'######################')
15    format(/1x,'Reading',1x,a)
16    format('date(yyyymmdd),hour,minute,1-min dir,1-min speed,',
     +'5-min 10 deg dir,5-min dir,5-min speed')
 20   format(/1x,'All files read...')
 24   format(/1x,'NO DATA IN 5-MIN FILES WITHIN DATA PERIOD SET BY ',
     + 'USER'//1x,'DATE RANGE IN FILES: ',a10,' THROUGH ',a10//
     + 1x,'DATE RANGE SET BY USER: ',a10,' THROUGH ',a10//1x
     + 'CHECK DATES IN STARTEND OR DATAFILE LIST'/1x,
     + 'NO 5-MIN SUBSTITUTION')
 99   return
      end
c*******************************************************************************
c     version 15272
c     read line of 5-minute data
      subroutine read5(line)
      use main1 
      implicit none
      
c imin:     integer minute of data being read from record  
c iyear:    4-digit integer year of data record
c imonth:   2-digit integer month of data record
c iday:     2-digit integer day of month of data record
c ihr:      2-digit integer hour of day (00-23)
c idate1:   integer date of record (YYYYMMDDHHMM)  
c idate2:   character string (length 12) of original date of record (YYYYMMDD)       
c idir:     integer 3-digit 2-minute average wind direction read from record
c idir10:   integer 3-digit 2-minute average wind direction read from record before 
c           random wind direction correction
c ispeed:   integer average 2-minute wind speed (knots) read from record
c n:        integer month counter relative to starting month of data period
c iflag:    integer indicator of validity of record (0=valid,1=invalid) 
c imin1:    imin/5
c line:     record being read and processed (200 length character string)
c stat1:    4-character string of station call sign
c acom:       comma delimiter for output file      
      integer imin,idir,iyear,imonth,iday,ihr,idate1,iwban1,ispeed,n,
     +iflag,imin1,idir10
      character line*200,stat1*4,idate2*12,acom*1
      acom=','
      
      
      read(line(14:25),'(i4,4i2)')iyear,imonth,iday,ihr,imin
      write(idate2,'(i4,4i2.2)')iyear,imonth,iday,ihr,imin
      call changehr(iyear,imonth,iday,ihr,imin)
      n=(iyear-startyear)*12+imonth-(startmon)+1
      imin1=imin/5
      idate1=(iyear*10000)+(imonth*100)+iday
      if (startdate .gt. idate1 .or. idate1 .gt. enddate) then
          baddate5(1)=min(baddate5(1),idate1)
          baddate5(2)=max(baddate5(2),idate1)  
          goto 5
      endif
      origdate5(n,iday,ihr,imin/5)=idate2
c     check to make sure wind data block is okay
      call checkwind5(line,iflag,idir,ispeed)
      
      if (iflag .eq. 0) then
          call checkwban(line(1:5),iwban1)
          if (badwban) then
              write(*,9)
              write(ilogunit,9)
              goto 5
          endif
          read(line(1:5),'(i5)')iwban1
          stat1=line(6:9)
          if (iwban1 .ne. iwban) then
              if (.not. lno1dat) then
                  lwban=.true.
                  write(*,6)iwban,iwban1
                  write(ilogunit,6)iwban,iwban1
                  goto 5
              else
                  iwban=iwban1
              endif
          endif
          if (stat1 .ne. stat) then
              stat=stat1
              if (.not. lno1dat) lstat=.true.
          endif
          lnodata5=.false.
          if (ispeed .ne. 0 .and. idir .ne. 0) then  !this is a calm, skip
              if (ispeed .lt. 2) then
                  if (.not. ifw .or. ifwdate .gt. idate1) then  
                      ikeep5(n,iday,ihr,imin1)=0                  !this minute will not be used in wind direction averages
                      icalm5(n,iday,ihr,imin1)=1
                      speed5(n,iday,ihr,imin1)=0.51              !set wind speed to 1 knot (0.51 m/s)
                  else
                      ikeep5(n,iday,ihr,imin1)=1  
                      icalm5(n,iday,ihr,imin1)=0         
                      speed5(n,iday,ihr,imin1)=(real(ispeed)+trunc)*0.51    
                  endif
              else
                  speed5(n,iday,ihr,imin1)=(real(ispeed)+trunc)*0.51
                  ikeep5(n,iday,ihr,imin1)=1
                  icalm5(n,iday,ihr,imin1)=0
              endif
              idir10=idir  !before randomnization
              if (idir .gt. 0) idir=idir+irnd(imonth,ihr,imin1)-4  !randomnize wind direction
              if (idir .gt. 360) idir=idir-360
              dir5minut(n,iday,ihr,imin1)=real(idir)
              xdir5(n,iday,ihr,imin1)=-1.0*sin(real(idir)*d2r)   !get x and y components of direction
              ydir5(n,iday,ihr,imin1)=-1.0*cos(real(idir)*d2r)
              idir5min(n,iday,ihr,imin1)=idir
              speed5kts(n,iday,ihr,imin1)=real(ispeed)
              write(igdunit5,10)line
              if (comp15) then
                  if (ikeep(n,iday,ihr,imin) .ge. 0) then
                      write(icomp5,20)idate(n,iday),acom,ihr,
     +                acom,imin,acom,int(dirminut(n,iday,ihr,imin)),
     +                acom,speed1(n,iday,ihr,imin),acom,idir10,acom,
     +                idir,acom,speed5(n,iday,ihr,imin1)
                  endif
              endif
              
          else
              idir10=idir
              write(iclmvar5,10)line
              if (comp15) then
                  if (ikeep(n,iday,ihr,imin) .ge. 0) then 
                      write(icomp5,20)idate(n,iday),acom,ihr,acom,imin,
     +                acom,int(dirminut(n,iday,ihr,imin)),acom,
     +                speed1(n,iday,ihr,imin),acom,idir10,acom,idir,
     +                acom,real(ispeed*0.51)  
                  endif
              endif
          endif
      else
          if (iflag .eq. 1) then
              write(ierrunit5,10)line  !bad record
          else
              write(iclmvar5,10)line  !variable record
              if (comp15) then
                  if (ikeep(n,iday,ihr,imin) .ge. 0) then 
                      write(icomp5,25)idate(n,iday),acom,ihr,acom,imin,
     +                acom,int(dirminut(n,iday,ihr,imin)),acom,
     +                speed1(n,iday,ihr,imin),acom,'VRB',acom,'VRB',
     +                acom,real(ispeed*0.51)
                  endif
              endif
          endif
      endif
 6    format(/1x,'WBAN number has changed from ',i5,1x,'to',1x,i5/1x,
     +  'Stopping AERMINUTE')
 9    format(/1x,'Bad WBAN string in file, stopping AERMINUTE')
 10   format(a200)
 
 20   format(i8,a1,2(i2,a1),i3,a1,f6.2,a1,2(i3,a1),f6.2)
 25   format(i8,a1,2(i2,a1),i3,a1,f6.2,a1,2(a3,a1),f6.2)
 5    return
      end
c*******************************************************************************
c     version 15272
c     check 5-minute data 
      subroutine checkwind5(var1,iflag1,idir1,ispeed1)
      use main1
      implicit none
  
c var1:     line of one minute data record being processed
c var2:     wind data section of line
c iflag1:   integer flag to indicate line is okay (0=okay, 1=bad)
c iflag1:   10 element integer array of flags for 9 error checks
c ind5min:    
      character var1*200,var2*7
      integer iflag1,idir1,ispeed1,ind5kt,i,j1,indg

      iflag1=0
      
c     check to make sure 5-minute data record
      if (index(var1,'5-MIN') .eq. 0) then
          write(ierrunit5,10)var1
          iflag1=1
          goto 5  ! not a 5-minute data record
      endif

c     check to see if AUTO is present. If so, adjust string for searching for winds
      if (index(var1,'AUTO') .gt. 0) then
          var2=var1(72:78)
      else
          var2=var1(67:73)
      endif
      
      ind5kt=index(var2,'KT')
      if (index(var2,'KT') .eq. 0) then 
          if (index(var2,'G') .eq. 0) iflag1=1 !may be a gust reported, if so, data okay, if not, data not okay
          if (iflag1 .eq. 1) goto 5
      endif
      i=1
      do while(iflag1 .eq. 0 .and. i .le. 5)
          j1=iachar(var2(i:i))
          if (j1 .lt. 48 .or. j1 .gt. 57) then   
              iflag1=1
          else
              i=i+1
          endif
      enddo
      if (iflag1 .eq. 0) then
          read(var2(1:2),'(i2)')idir1
          read(var2(3:5),'(i3)')ispeed1
          idir1=idir1*10
          if ((idir1 .gt. 360 .or. idir1 .lt. 0) .or. 
     +    (ispeed1 .ge. 50 .or. ispeed1 .lt. 0)) iflag1=1
      endif
      if (var2(1:3) .eq. 'VRB') then
          iflag1=2
          read(var2(4:5),'(i3)')ispeed1
          if (ispeed1 .ge. 50 .or. ispeed1 .lt. 0) iflag1=1
      endif
 10   format(a200)
 5    return 
      end
c*******************************************************************************
      subroutine getsurf
      use main1
      implicit none

c nf:     integer file counter
c eof:    integer end-of-file indicator
c n:      integer file counter when checking for duplicate names
c i:      integer file loop counter
c fname:  surface filename
c exist2: logical variable checking for file existence
c ldup1:  logical variable checking for duplicate filenames
      integer nf,eof,n,i
      character fname*250
      logical exist2,ldup1
      
      write(*,65)
      write(ilogunit,65)
      write(*,64)
      write(ilogunit,64)
      open(unit=50,file='sfcfiles.tmp',status='old')  !read input file
      eof=0
      nf=0

      read(50,*,iostat=eof)fname
      lgo=.true.
    
 15   if (eof .eq. 0) then
        inquire(file=fname,exist=exist2)
        if (.not. exist2) then
          write(*,4)trim(adjustl(fname))
          write(ilogunit,4)trim(adjustl(fname))
          lgo=.false.
        endif
        nf=nf+1
        read(50,*,iostat=eof)fname
        goto 15
      endif
      if (.not. lgo) then
        write(*,17)
        write(ilogunit,17)
        goto 100
      endif

      rewind(50)
      eof=0
      nsurf=nf
      nf=0
      allocate(obfiles(nsurf))
      read(50,*,iostat=eof)fname
 21   if (eof .eq. 0) then      
        nf=nf+1
        obfiles(nf)=fname
        read(50,*,iostat=eof)fname
        goto 21
      endif
c     check for duplicate filenames
      write(*,45)
      write(ilogunit,45)

      do i=2,nsurf
        ldup1=.false.
        n=1
          do while (n .lt. i .and. .not. ldup1)
            if (obfiles(i) .eq. obfiles(n)) ldup1=.true.  !duplicate filename
            n=n+1
          enddo 
          if (ldup1) then
            write(*,40)trim(adjustl(obfiles(i)))
            write(ilogunit,40)trim(adjustl(obfiles(i)))
            ldupob=ldup1
          endif
      enddo
      
c     write message that there are no duplicate filenames       
       if (.not. ldupob) then
         write(*,55)
         write(ilogunit,55)
         
       endif      
      
      write(*,56)
      write(ilogunit,56)
      call readsurf
      
      write(*,75)
      write(ilogunit,75)
 10   format(a250)
 4    format(/1x,a,' does not exist')
 17   format(/1x,'One or more surface files do not exist, ',
     +  'stopping program')   
 40   format(/1x,'ERROR: Surface file ',a,' is a duplicate'/1x,
     +  'Correct input list')
 45   format(/1x,'Checking for duplicate surface filenames')
 55   format(/1x,'No duplicate filenames found')
 56   format(/1x,'Reading surface files')
 64   format(/1x,'Obtaining surface data filenames')
 65   format(/1x,'#########################',' SURFACE FILES CHECK ',
     +  '##########################')
 75   format(/1x,'###################################################',
     +  '#####################')
100   close(50,status='delete')     
      return
      end  
c*******************************************************************************
c subroutine to read ISHD data
      subroutine readsurf
       use main1
      implicit none
      
c ispeed:     integer wind speed
c idir:       integer wind direction   
c eof1:       integer end-of-file indicator
c lsthr:      integer local hour
c gmthr:      integer GMT hour
c tshift:     integer GMT to local time shift
c n:          integer month relative start month
c id:         integer day of month 
c ihr:        integer hour of day 
c imin:       integer minute of hour
c ifiles:     integer file loop counter
c line:       line of ISHD data to read
c aflag:      integer flag indicating if hour is calm, valid,
c             variable or missing
c qflag:      flag indicating QC flag of record
      
      integer ispeed,idir,ifile,eof1,lsthr,gmthr,tshift,n,
     +  id,ihr,imin,ifiles
      character line*250,aflag*1,qflag*1

       open(unit=inunit,file=infiles(1),status='old')
      read(inunit,10)line
      read(line(22:23),'(i2)')lsthr
      read(line(26:27),'(i2)')gmthr
      tshift=gmthr-lsthr
      if (tshift .lt. 0) tshift=tshift+24
      close(inunit)

      do ifiles=1,nsurf
        eof1=0
        open(unit=51,file=obfiles(ifiles),status='old')
        write(*,30)trim(adjustl(obfiles(ifiles)))
        write(ilogunit,30)trim(adjustl(obfiles(ifiles)))
 25     read(51,10,iostat=eof1)line
          if (eof1 .eq. 0) then
            call rdishd(line,tshift,idir,ispeed,aflag,qflag,n,id,ihr,
     +      imin)
            if (n .ge. 1 .and. n .le. nmonths) then
              obdir(n,id,ihr,imin)=real(idir)
              obspd(n,id,ihr,imin)=real(ispeed)/10.0
              flag(n,id,ihr,imin)=aflag
              qcflag(n,id,ihr,imin)=qflag
            endif
            read(51,10,iostat=eof1)line
            goto 25
          endif
        close(51)
      enddo
 10   format(a250)
 30   format(1x,'Reading surface file ',a)
      return
      end      
c*******************************************************************************    
c subroutine to compare the winds from standard observations
c against one minute data
c winds from standard observations have been quality controlled but one minute
c data is not
      subroutine qc
      use main1
      implicit none

c n:          integer month counter
c j:          integer day counter
c h:          integer hour counter
c m:          integer minute counter
c m1:         integer minute. equals m unless m=60 then m1=0
c ncalmob:    integer number of calm observations
c nvarob:     integer number of variable observations
c nmissob:    integer number of missing observations
c nnormob:    integer number of normal observations
c ncalmone:   integer 3 x 1 array of 1-minute speeds (1=missing speed, 2=speed less than
c             3 knots, 3 > 3 knots) for calm hours
c nvarone:    integer 2 x1 array of 1-minute speeds for variable hours
c             (1=missing,2=non-missing)
c nmissone:   integer 2x1 array number of 1-minute missing speeds 
c             (1=missing, 2=non-missing)
c nnormone:   integer 2x1 array of 1-minute winds for normal hours
c             (1=missing,2=non-missing)
c speedclass: 2x7 array of speed classes
c dirclass:   20x1 array direction classes
c isc:        individual speed class
c idc:        individual direction class
c dclass:     function to find direction class
c sclass:     integer function to find speed class
c dir10:      real direction of ISDHD data
c spd1:       real speed of 1-minute data
c dirdiff:    real direction difference between 1-minute and ISHD observation
c spddiff:    real speed difference between 1-minute and ISHD obs
c adate1:     character date of observation, including hour and minute (YYYYMMDDHHMM)
c adate:      character date of observation (YYYYMMDD)
c acom:       comma delimiter for output file
c tag:        direction class indicator

      integer n,m1,j,h,m,ncalmob,nvarob,nmissob,nnormob,ncalmone(3),
     +  nvarone(2),nmissone(2),nnormone(2),speedclass(2,7),
     +  dirclass(20),isc,idc,dclass,sclass

      real dir10,spd1,dirdiff,spddiff
      character tag*20,adate1*12,acom*1,adate*8
      
      ncalmob=0
      nvarob=0
      nmissob=0
      nnormob=0
      ncalmone=0
      nvarone=0
      nmissone=0
      nnormone=0
      speedclass=0
      dirclass=0
      acom=','



       open(unit=105,file=compfil,status='replace')
        write(105,29)
c     loop through months and compare winds      
      do n=1,nmonths
        do j=1,ndays(n)
          do h=1,24
            do m=2,60
                if (obdir(n,j,h,m) .ge. 0) then
                  spddiff=-999.0
                  dirdiff=-999.0
                  call roundmin(n,j,h,m,spd1,dir10)
                  if (flag(n,j,h,m) .eq. 'C') then   !calms
                    ncalmob=ncalmob+1
                    if (speed1(n,j,h,m) .lt. 0) then
                      ncalmone(1)=ncalmone(1)+1  !missing
                    elseif (speed1(n,j,h,m) .lt. 1.53) then  !less than 3 knots
                      ncalmone(2)=ncalmone(2)+1
                    else
                      ncalmone(3)=ncalmone(3)+1
                    endif
                  elseif (flag(n,j,h,m) .eq. 'V') then  !variable
                    nvarob=nvarob+1
                    if (speed1(n,j,h,m) .lt. 0) then
                      nvarone(1)=nvarone(1)+1  !missing
                    else
                      nvarone(2)=nvarone(2)+1
                      if (obspd(n,j,h,m) .lt. 900) then
                        spddiff=abs(spd1-obspd(n,j,h,m)) 
                        isc=sclass(spddiff)
                        speedclass(1,isc)=speedclass(1,isc)+1
                      endif
                    endif
                  elseif (flag(n,j,h,m) .eq. '9') then  !missing
                    nmissob=nmissob+1
                    if (speed1(n,j,h,m) .lt. 0) then
                      nmissone(1)=nmissone(1)+1  !missing
                    else
                      nmissone(2)=nmissone(2)+1
                    endif
                  else  !valid wind
                    nnormob=nnormob+1
                    if (speed1(n,j,h,m) .lt. 0) then
                      nnormone(1)=nnormone(1)+1  !missing
                    else
                      nnormone(2)=nnormone(2)+1
                      dirdiff=dir10-obdir(n,j,h,m)
                      if (dirdiff .lt. -180.0) dirdiff=dirdiff+360
                      if (dirdiff .gt. 180.0) dirdiff=dirdiff-360
                      dirdiff=abs(dirdiff)
                      spddiff=abs(spd1-obspd(n,j,h,m))
                      isc=sclass(spddiff)
                      idc=dclass(dirdiff)
                      speedclass(2,isc)=speedclass(2,isc)+1
                      dirclass(idc)=dirclass(idc)+1
                    endif
                  endif
                if (dirminut(n,j,h,m) .ge. 0.0) then
                  adate1=origdate(n,j,h,m)
                  adate=adate1(1:8)
                else
                  call makedate2(n,j,h,m,adate1)
                  adate=adate1(1:8)
                endif
                m1=m
                if (m1 .eq. 60)m1=0
                write(105,30)adate,acom,h,acom,m1,acom,icalm(n,j,h,m),
     +            acom,dirminut(n,j,h,m),acom,dir10,acom,
     +            speed1(n,j,h,m),acom,spd1,acom,obdir(n,j,h,m),acom,
     +            obspd(n,j,h,m),acom,dirdiff,acom,spddiff,acom,
     +            flag(n,j,h,m),acom,qcflag(n,j,h,m)
                endif
                
            enddo
          enddo
        enddo
      enddo
 29   format('date(yyyymmdd),hour,minute,calm flag,1-min dir,',
     +  '1-min dir10,1-min speed,1-min speed_1,obs. dir,obs. speed,',
     +  'dirdiff,speeddiff,wind flag,qc flag')
 30   format(a12,2(a1,i2),a1,i1,6(a1,f8.1),2(a1,f8.1),2(a1,a1))   
     
c     write a summary of counts
      write(*,60)
      write(*,64)
      write(ilogunit,60)

      write(ilogunit,61)ncalmob,ncalmone(1),ncalmone(2),ncalmone(3),
     + nmissob,nmissone(1),nmissone(2),nvarob,nvarone(1),nvarone(2),
     + speedclass(1,1),speedclass(1,2),speedclass(1,3),speedclass(1,4),
     + speedclass(1,5),speedclass(1,6),speedclass(1,7),
     + nnormob,nnormone(1),nnormone(2),speedclass(2,1),
     + speedclass(2,2),speedclass(2,3),speedclass(2,4),speedclass(2,5),
     + speedclass(2,6),speedclass(2,7)
      do idc=1,20
        tag=''
        if (idc .eq. 1) then
          tag='     0'
        elseif (idc .le. 19) then
          tag(1:2)='<= '
          write(tag(4:6),'(i3)')(idc-1)*10
        else
          tag=' > 180'
        endif
        write(ilogunit,62)tag,dirclass(idc)
      enddo
      write(*,63)
      write(ilogunit,63)
      

60    format(/1x,'############################ DATA QC SUMMARY #######',
     +  '#####################')
61    format(/1x,'STANDARD CALMS'/
     +  3x,'Number of standard observation calms:',2x,i6,/3x,
     +  'Number of missing 1-minute winds:',6x,i6,/3x,
     +  'Number of 1-minute winds < 3 knots:',4x,i6/3x,
     +  'Number of 1-minute winds >= 3 knots:',3x,i6//1x,
     +  'STANDARD MISSING WINDS'/3x,
     +  'Number of standard observation missing winds:',1x,i6/3x,
     +  'Number of missing 1-minute winds:',13x,i6,/3x,
     +  'Number of non-missing 1-minute winds:',9x,i6,//1x,   
     +  'STANDARD VARIABLE WINDS'/3x,
     +  'Number of standard observation variable winds:',1x,i6/3x,
     +  'Number of missing 1-minute winds:',14x,i6,/3x,
     +  'Number of non-missing 1-minute winds:',10x,i6,//1x,
     +  'VARIABLE WINDS:  SPEED DIFFERENCES (M/S)'/1x,
     +  'Speed difference     Number of minutes',/1x,
     +  '        0              ',i6,/1x,'   <= 0.2              ',i6,/
     +  1x,'0.3 - 0.5              ',i6/1x,'0.5 - 1.0              'i6,/
     +  1x,'1.0 - 3.0              ',i6/
     +  1x,'3.0 - 5.0              ',i6/
     +  1x,'    > 5.0              ',
     +  i6//,1x,'STANDARD VALID WINDS',3x,
     +  'Number of standard observation valid winds:',1x,i6/3x,
     +  'Number of missing 1-minute winds:',11x,i6,/3x,
     +  'Number of non-missing 1-minute winds:',7x,i6,//1x,
     +  'VALID WINDS:  SPEED DIFFERENCES (M/S)'/1x,
     +  'Speed difference     Number of minutes',/1x,
     +  '        0              ',i6/1x,'   <= 0.2              ',i6/
     +  1x,'0.3 - 0.5              ',i6/1x,'0.5 - 1.0              'i6,/
     +  1x,'1.0 - 3.0              ',i6/
     +  1x,'3.0 - 5.0              ',i6/
     +  1x,'    > 5.0              ',i6
     +  //1x,'VALID WINDS:  DIRECTION DIFFERENCES (DEG)'/1x,
     +  1x,'Direction difference   Number of minutes')
  62  format(1x,a20,6x,i6)
 63   format(/1x,'###################################################',
     +  '#####################')
 64   format(1x,'See aerminute.log for QC details')
       close(105)
 
 100  return
      end
c*******************************************************************************
      subroutine roundmin(n,j,h,m,spd1,dir10)
      use main1
      implicit none

c n:          integer month counter
c j:          integer day counter
c h:          integer hour counter
c m:          integer minute counter
c dir10:      real direction of ISDHD data
c spd1:       real speed of 1-minute data

      integer n,j,h,m
      real spd1,dir10
      if (speed1(n,j,h,m) .lt. 0) then
        spd1=speed1(n,j,h,m)
        dir10=dirminut(n,j,h,m)
      else
        if (icalm(n,j,h,m) .eq. 1) then
          spd1=real(nint(speedkts(n,j,h,m)*5.1)/10.0)  !use original speed in knots
        else
          spd1=real(nint(speed1(n,j,h,m)*10.0)/10.0)
        endif
        dir10=real(nint(dirminut(n,j,h,m)/10.0)*10)
        if (dir10 .eq. 0.0) dir10=360.0
      endif
      return
      end
c*******************************************************************************
c     make date
      subroutine makedate2(n,j,h,m,adate)
      use main1
      implicit none
c n:          integer month counter
c j:          integer day counter
c h:          integer hour counter
c h1:         h-1
c m:          integer minute counter
c m1:         integer minute. equals m unless m=60 then m1=0
c adate:      character date of observation, including hour and minute (YYYYMMDDHHMM)
      
      integer n,j,h,m,m1,h1
      character adate*12
      
      write(adate(1:9),'(i8.8)')idate(n,j)
      h1=h-1
      if (m .eq. 60) then
        m1=0
      else
        m1=m
      endif
      write(adate(9:12),'(2i2.2)')h1,m1
      return
      end
      
c*******************************************************************************
c subroutine to read ISHD data to get standard observations for
c comparison against one minute data
      subroutine rdishd(line,tshift,idir,ispeed,aflag,qflag,n,id,ihr,
     + imin)
      use main1

c line:       character line being read
c aflag:      integer flag indicating if hour is calm, valid,
c             variable or missing   
c qflag:      flag indicating QC flag of record
c tshift:     integer GMT to local time shift
c iyr:        integer 4-digit year of observation
c im:         integer month of observation
c id:         integer day of observation
c ihr:        integer hour of observation
c imin:       integer minute of observation
c idir:       integer wind direction
c ispeed:     integer wind speed
c n:          integer month relative start month

      implicit none
      character line*(*),aflag*1,qflag*1
      integer tshift,iyr,im,id,ihr,imin,idir,ispeed,n
      
      read(line(16:27),'(i4,4i2)')iyr,im,id,ihr,imin
c     convert from GMT to LST
      call gmt2lst(iyr,im,id,ihr,imin,tshift)      
c     change the hour to match the convention of the one minute data, i.e. add 1 hour
      call changehr(iyr,im,id,ihr,imin)
      n=(iyr-startyear)*12+im-(startmon)+1
      if (n .ge. 1 .and. n .le. nmonths) then
        read(line(61:63),'(i3)')idir
        read(line(66:69),'(i4)')ispeed
        aflag=line(65:65)
        qflag=line(70:70)
      endif
      return
      end             
c*******************************************************************************
c subroutine to convert from GMT to LST
      subroutine gmt2lst(iyr,im,id,ihr,imin,tshift)
      use main1
      implicit none

c iyr:        integer 4-digit year of observation
c im:         integer month of observation
c id:         integer day of observation
c ihr:        integer hour of observation
c imin:       integer minute of observation
c tshift:     integer GMT to local time shift
c leap:       logical variable indicating if year is leap year
      integer iyr,im,id,ihr,imin,tshift
      logical leap
      
      leap=.false.
      
      ihr=ihr-tshift
      if (ihr .lt. 0)then
        ihr=ihr+24
        if (id .eq. 1) then
          if (im .eq. 1) then
            id=days(12)
            im=12
            iyr=iyr-1
          else
            id=days(im-1)
            if (im .eq. 3) then
              call leapyr(iyr,leap)
              if (leap)id=id+1
            endif
            im=im-1
          endif
        else
          id=id-1
        endif
      endif
      return
      end
c*******************************************************************************
      function sclass(diff)
      use main1
      implicit none
c sclass:     integer speed class
c diff:       real speed difference
      
      integer sclass
      real diff
      
      if (diff .eq. 0.0) then
        sclass=1
      elseif (diff .le. 0.2) then
        sclass=2
      elseif (diff .le. 0.5) then
        sclass=3
      elseif (diff .le. 1.0) then
        sclass=4
      elseif (diff .le. 3.0) then
        sclass=5
      elseif (diff .le. 5.0) then
        sclass=6
      else
        sclass=7
      endif
      return
      end
c******************************************************************************* 
      function dclass(diff)
      use main1
      implicit none

c dclass:     integer direction class
c x:          integer direction difference/10
c diff:       real direction difference
      integer dclass,x
      real diff

      x=int(diff/10.0)
      if (x .le. 18) then
        dclass=x+1
      else
        dclass=20
      endif
      
      return
      end      
c*******************************************************************************
c subroutine to substitute valid 5-minute data into 1-minute data if 1-minute data
c missing
      subroutine sub5
      use main1
      implicit none
      
c im:     integer month counter
c idy:    integer day counter
c ihr:    integer hour counter
c imin:   integer minute counter
c imin5:  imin/5
c idate1: integer calculated date 
c rmin:   real value of imin
c frac:   modulus of rmin and 2 to determine even/odd minute
c acom:   comma character for substitution file
      
      integer im,idy,ihr,imin,imin5,idate1
      real rmin,frac
      character acom*1
      acom=','
      
      write(*,1)
1     format(1x,'Substituting 5-minute data into 1-minute data')
      if (writesub5) then
          open(unit=isubunit,file=sub5fil,status='replace')
          write(isubunit,5)
      endif
      
      do im=1,nmonths
          do idy=1,ndays(im)
              do ihr=1,24
                  do imin=5,60,5
                      imin5=imin/5
                      if (ikeep(im,idy,ihr,imin) .eq. -9 .and. 
     +                ikeep5(im,idy,ihr,imin5) .ne. -9) then
                          if (writesub5) then
                              if (idate(im,idy) .gt. 0) then
                                  idate1=idate(im,idy)
                              else
                                  call makedate(im,idy,idate1)
                              endif
                              write(isubunit,10)idate1,acom,ihr,acom,
     +                        imin,acom,
     +                        int(dir5minut(im,idy,ihr,imin5)),acom,
     +                        speed5(im,idy,ihr,imin5)
                          endif
                          rmin=real(imin)
                          frac=mod(rmin,2.0)                    
                          speed1(im,idy,ihr,imin)=
     +                    speed5(im,idy,ihr,imin5)
                          xdir(im,idy,ihr,imin)=xdir5(im,idy,ihr,imin5)
                          ydir(im,idy,ihr,imin)=ydir5(im,idy,ihr,imin5)
                          dirminut(im,idy,ihr,imin)=
     +                    dir5minut(im,idy,ihr,imin5)
                          idirmin(im,idy,ihr,imin)=
     +                    idir5min(im,idy,ihr,imin5)
                          speedkts(im,idy,ihr,imin)=
     +                    speed5kts(im,idy,ihr,imin5)
                          origdate(im,idy,ihr,imin)=
     +                    origdate5(im,idy,ihr,imin5)
                          icalm(im,idy,ihr,imin)=
     +                    icalm5(im,idy,ihr,imin5)
                          ikeep(im,idy,ihr,imin)=
     +                    ikeep5(im,idy,ihr,imin5)
                          minsrc(im,idy,ihr,imin)=5
                          nvalid5(im,idy,ihr)=nvalid5(im,idy,ihr)+1
                          nhours(im,6)=nhours(im,6)+1
                          if (frac .eq. 0) then
                              neven5(im,idy,ihr)=neven5(im,idy,ihr)+1
                          else
                              nodd5(im,idy,ihr)=nodd5(im,idy,ihr)+1
                          endif
                      endif
                  enddo
              enddo
          enddo
      enddo  
      if (writesub5) close(isubunit)
5     format('date(yyyymmdd),hour,minute,5-min dir,5-min speed')
 10   format(i8,a1,2(i2,a1),i3,a1,f6.2)
      return
      end
c*******************************************************************************
c subroutine to get number of minutes for each hour for total minutes,
c total calms, even valid minutes, even calm minutes, valid odd minutes,
c calm odd minutes, number of odd minutes used in hourly averages, and number
c of odd calms used in hourly averages
      subroutine totals
      use main1
      implicit none
      
c i:    integer month counter
c j:    integer day counter
c k:    integer hour counter
c l:    integer minute counter
c frac: real result of MOD function to determine if minute is 
c       even (frac=0) or odd (frac > 0)
      integer i,j,k,l
      real frac
 
c loop through, months, days, hours, and minutes
c all counts are based on ikeep for the minute being >=0
c if minute keep flag (ikeep) is >= 0, add 1 to number of valid minutes for hour
c if the keep flag is 0, add 1 to number of calm minutes for hour
c if frac is 0, add 1 to number of even minutes
c if frac is 0 and the keep flag is 0 add 1 to number of calm even minutes
c if frac > 0 add 1 to number of odd minutes (noddperm)
c if frac > and keep flag is 0 add 1 to number of odd calm minutes     
      do i=1,nmonths  !month loop
        do j=1,ndays(i) !day loop
          do k=1,24  !hour loop
            do l=2,60  !minute loop
              if (ikeep(i,j,k,l) .ge. 0) then
                nvalid(i,j,k)=nvalid(i,j,k)+1
                if (ikeep(i,j,k,l) .eq. 0) then 
                  ncalms(i,j,k)=ncalms(i,j,k)+1
                endif
                frac=mod(real(l),2.0)
                if (frac .eq. 0.0) then
                  neven(i,j,k)=neven(i,j,k)+1
                  if (ikeep(i,j,k,l) .eq. 0) then 
                    nevencalm(i,j,k)=nevencalm(i,j,k)+1
                  endif 
                else
                  noddperm(i,j,k)=noddperm(i,j,k)+1
                  if (ikeep(i,j,k,l) .eq. 0) then 
                    noddpermc(i,j,k)=noddpermc(i,j,k)+1
                  endif 
                endif
              endif
            enddo !minute loop
          enddo !hour loop
        enddo !day loop
      enddo !month loop

      return
      end
c*******************************************************************************
c subroutine to assign a keep flag for an hour
c based on having 1 minute in each half of the hour
c minute can be even or used odd minute
      subroutine assignflag
      use main1
      implicit none
      
c im:       integer month counter
c idy:      integer day counter
c ihr:      integer hour counter
c imin:     integer minute counter 
c ifirst:   integer array of ikeep flags for 1st 30 minutes of hour (minutes 2-30)
c ilast:    integer array of ikeep flags for last 30 minutes of hour (minutes 31-60)
c idate1:   integer date YYMMDD of dates not in initial array (not in one minute data files)
c sumhr:    integer sum of valid minutes for first half of hour  
c percnt:   real percent of valid observations that are calm
c rcalm:    real number of calm minutes used, even calm + odd calm used
c rtot:     real number total minutes used, even + odd
      integer im,idy,ihr,imin,ifirst(30),ilast(30),idate1,sumhr
      real percnt,rcalm,rtot
   
      
      write(*,1)
c      write(ilogunit,1)
  1   format(/1x,'Checking for valid hours and overlapping ',
     +  'even/odd minutes...')
     

c     make odd minutes that overlap with valid even minutes as non-keepers
c     also create a date for idate where there was no valid observation
      do im=1,nmonths
c       calculate # of total hours in each month
        nhours(im,1)=ndays(im)*24
        do idy=1,ndays(im)
          if (idate(im,idy) .eq. 0) then
            call makedate(im,idy,idate1)
            idate(im,idy)=idate1
          endif
          do ihr=1,24
            sumhr=0
            percnt=0.0
            rtot=0.0
            rcalm=0.0
c           initialize ifirst and ilast to -9.  initialize nodd and noddcalm to the
c           permanent odd minutes and odd calm minutes for each hour
            ifirst=-9
            ilast=-9
            nodd(im,idy,ihr)=noddperm(im,idy,ihr)
            noddcalm(im,idy,ihr)=noddpermc(im,idy,ihr)
c           loop through odd minutes keep flags
c           if there is a valid even minute (calm or non-calm)
c           on either side of the odd minute, set the keep flag to -9
c           and the calm flag to 0
c           subtract 1 from nodd and noddcalm
c           if looking at minute 3 for an hour and there are valid winds for minutes 2 or 4
c           then minute 3 becomes non-usable
c           if there are not valid values for minutes 2 and 4, then minute 3 can be used
            do imin=3,59,2
              if (ikeep(im,idy,ihr,imin) .ge. 0 .and. 
     +          (ikeep(im,idy,ihr,imin-1) .ge. 0 .or.
     +          ikeep(im,idy,ihr,imin+1) .ge. 0)) then
                  nodd(im,idy,ihr)=nodd(im,idy,ihr)-1
                  if (icalm(im,idy,ihr,imin) .eq. 1) then
                    noddcalm(im,idy,ihr)=noddcalm(im,idy,ihr)-1
                  endif
                  ikeep(im,idy,ihr,imin)=-9
                  icalm(im,idy,ihr,imin)=0
              endif
            enddo 
c           loop through all 59 minutes
c           and assign the keep flag to 2 arrays based on location in the hour
c           minutes 30 and lower are in ifirst and and minute 31-60 in ilast
            do imin=2,60   
              if (imin .le. 30) then 
                ifirst(imin)=ikeep(im,idy,ihr,imin)
                if (ikeep(im,idy,ihr,imin) .eq. 1) sumhr=sumhr+1
              else
                ilast(imin-30)=ikeep(im,idy,ihr,imin)
              endif
            enddo 
c JAT SUBJECT TO CHANGE
c            if maximum value of the keep flag is 1 for both arrays, hour is kept and
c            an average calculated
c            if maximum value of the keep flag is 0 for either, that half of the hour
c            is considered calm and a wind direction can't be used and set hour keep flag to -2 (non-valid)
c            version 209, if at least 2 obs in 1st half hour or at least 1 in 2nd half of hour, keep hour
c            version 11059, if at least 1/2 of the number of valid observations are non-calm, keep hour
c            otherwise, set to calm

            
            if (sumhr .ge. 2 .or. maxval(ilast) .eq. 1) then
              rcalm=real(nevencalm(im,idy,ihr)+noddcalm(im,idy,ihr))
              rtot=real(neven(im,idy,ihr)+nodd(im,idy,ihr))
              percnt=rcalm/rtot
              if (percnt .le. 0.50) then 
                ikeephr(im,idy,ihr)=1    ! hour is valid, calculate average
              else
                ikeephr(im,idy,ihr)=0   !consider hour calm
              endif
            else
              if (nvalid(im,idy,ihr) .gt. 0) ikeephr(im,idy,ihr)=-2  !values read but not enough for averaging
            endif
c           previous version, 09141, keep procedure
c           if (maxval(ifirst) .eq. 1 .and. maxval(ilast) .eq. 1) then
c               ikeephr(im,idy,ihr)=1
c             else
c               if (nvalid(im,idy,ihr) .gt. 0) ikeephr(im,idy,ihr)=-2  !values read but not enough for averaging
c             endif
c     version 09313, calculate hours that are valid, invalid, or missing
c     version 11059, calculate hours that are calm
            if (ikeephr(im,idy,ihr) .eq. 1) then
              nhours(im,2)=nhours(im,2)+1
            else if (ikeephr(im,idy,ihr) .eq. -2) then
              nhours(im,3)=nhours(im,3)+1
            else if (ikeephr(im,idy,ihr) .eq. 0) then
              nhours(im,4)=nhours(im,4)+1
            else
              nhours(im,5)=nhours(im,5)+1
!              nhours(im,4)=nhours(im,4)+1
            endif
!            if (ikeephr(im,idy,ihr) .ne. 1 .and. lfive) run5=.true.
          enddo
        enddo 
      enddo 
   
      return
      end
c***************************************************************
c calculate minimum and maximum 2-minute wind speed for each hour
c as well as overall minimum and maximum
      subroutine hourlyminmax
      use main1
      implicit none
c nspd:     integer number of wind speeds used in averaging (includes calms)
c i:        integer month counter
c j:        integer day counter
c k:        integer hour counter
c l:        integer minute counter
c dmin:     integer 2-minute minimum wind direction
c dmax:     integer 2-minute maximum wind direction
c isrc:     4 x 1 array of data sources (1 or 5)
c           1=min speed,2=max speed,3=min direction,4=max direction
c min1:     real 2-minute minimum wind speed
c max1:     real 2-minute maximum wind speed
      integer i,j,k,l,dmin,dmax,isrc(4)
      real min1,max1

c initialize minimum and maximum one minute wind speeds
      minonemin=100.0
      maxonemin=0.0  
      isrc=-9
      do i=1,nmonths
        do j=1,ndays(i)
          do k=1,24
            min1=100.0
            max1=0.0
            dmin=361
            dmax=0
c           only process hours that have hourly keep flag of 1
            if (ikeephr(i,j,k) .ge. -2)then 
              do l=2,60
                if (ikeep(i,j,k,l) .ge. 0) then
c                 find overall minimum and maximum wind speed for 2-minute averages
c                 version 15272 determine source of data
                  if (speed1(i,j,k,l) .ge. 0) then
                    call minmaxone(i,j,k,l)
                    if (speed1(i,j,k,l) .lt. min1) then
                        min1=speed1(i,j,k,l)
                        isrc(1)=minsrc(i,j,k,l)
                    endif
                    if (speed1(i,j,k,l) .gt. max1) then
                        max1=speed1(i,j,k,l)
                        isrc(2)=minsrc(i,j,k,l)
                    endif
                  endif
c                  if (ikeep(i,j,k,l) .gt. 0) then
                  if (ikeep(i,j,k,l) .ge. 0) then
                      if (idirmin(i,j,k,l) .lt. dmin) then
                          dmin=idirmin(i,j,k,l)
                          isrc(3)=minsrc(i,j,k,l)
                      endif
                      if (idirmin(i,j,k,l) .gt. dmax) then
                          dmax=idirmin(i,j,k,l)
                          isrc(4)=minsrc(i,j,k,l)
                      endif
                  endif
                endif
              end do
              speedmin(i,j,k)=min1
              speedmax(i,j,k)=max1
              srcmin(1,i,j,k)=isrc(1)
              srcmax(1,i,j,k)=isrc(2)
              dirmin(i,j,k)=dmin
              dirmax(i,j,k)=dmax
              srcmin(2,i,j,k)=isrc(3)
              srcmax(2,i,j,k)=isrc(4)
            else   !now set min and max values to missing for missing hours
              speedmin(i,j,k)=999.0
              speedmax(i,j,k)=999.0
              dirmin(i,j,k)=999.0
              dirmax(i,j,k)=999.0
            endif
          end do 
        end do
      end do 
      return
      end  
c***************************************************************
c calculate hourly average winds
c determine 2-minute min and max wind speeds 
c determine min and max hourly averaged wind speed
      subroutine avg
      
      use main1
      implicit none
      
c nspd:     integer number of wind speeds used in averaging (includes calms)
c i:        integer month counter
c j:        integer day counter
c k:        integer hour counter
c l:        integer minute counter
c dir:      real hourly wind direction

      integer nspd,i,j,k,l
      real dir
     

      
      write(*,1)

 1    format(1x,'Begin averaging...')
 
      do i=1,nmonths
        do j=1,ndays(i)
          do k=1,24
c         only process hours that have hourly keep flag of 1
            if (ikeephr(i,j,k) .eq. 1)then 
              nspd=0
              do l=2,60
c             process only minutes with keep flags >= 0
c              use calms and non-calms for wind speed average
c              calculate running total of wind speeds
                if (ikeep(i,j,k,l) .ge. 0) then    
                  speed(i,j,k)=speed(i,j,k)+speed1(i,j,k,l)
                  nspd=nspd+1
c               find minimum and maximum wind speed for 2-minute averages
c               use only non-calm wind directions
c               keep running totals of x and y components of wind direction
                  if (ikeep(i,j,k,l) .eq. 1) then   
                    xdirhr(i,j,k)=xdirhr(i,j,k)+xdir(i,j,k,l) 
                    ydirhr(i,j,k)=ydirhr(i,j,k)+ydir(i,j,k,l)
                  endif
                endif
              end do
c           # of speeds should be the same as the total of neven and nodd for hour
c           if not report and stop program
              if (nspd .ne. (neven(i,j,k)+nodd(i,j,k))) then
                write(*,5)nspd,neven(i,j,k)+nodd(i,j,k),idate(i,j),k
                write(ilogunit,5)nspd,neven(i,j,k)+nodd(i,j,k),
     +           idate(i,j),k
                stop
              endif
c           calculate hourly average wind speed (m/s) and direction
              speed(i,j,k)=speed(i,j,k)/real(nspd)  !hourly wind speed average
              dirhr(i,j,k)=dir(xdirhr(i,j,k),ydirhr(i,j,k))
c           hour that is not a keeper set to calm
c           calm hour
            else if (ikeephr(i,j,k) .eq. 0) then
              speed(i,j,k)=0.0
              dirhr(i,j,k)=0.0
c           hour doesn't exist in data
            else                 
              speed(i,j,k)=999.0
              dirhr(i,j,k)=999.0
            endif
          end do
        end do
      end do
      write(*,2)
    
  2   format(/1x,'Averaging complete...')
  5   format(/1x,'Number of speeds ',i2,
     +  ' does not match the total of even and odd used minutes ',i2,/
     +  1x,i6,' hour ',i2/1x,'Stopping program')
      return
      end
c******************************************************************************
c routine to determine min and max 2-minute wind speed
c 15272 determine source of data for min/max
      subroutine minmaxone(i,j,k,l)
      use main1
      implicit none
      
c i:        integer month counter
c j:        integer day counter
c k:        integer hour counter
c l:        integer minute counter

      integer i,j,k,l

      
      if (speedkts(i,j,k,l) .lt. minonemin) then
        minonemin=speedkts(i,j,k,l)
        nmin=i
        daymin=j
        hrmin=k
        minmin=l
        minsrc1=minsrc(i,j,k,l)
      endif
      if (speedkts(i,j,k,l) .gt. maxonemin) then
        maxonemin=speedkts(i,j,k,l)
        nmax=i
        daymax=j
        hrmax=k
        minmax=l
        maxsrc1=minsrc(i,j,k,l)
      endif
      return
      end
c**********************************************      
c function of hourly wind direction
      function dir(x,y)
      use main1
      implicit none
 
c x:    real sum of x-component of wind directions
c y:    real sum of y-component of wind directions
c dir:  real hourly averaged wind direction
c corr: real correction to add to hourly wind direction
c       to account for 0 in geographic space is 90 in Cartesian space

c if x > 0 and y > 0 (SW wind, correction is 180 degrees)
c if x < 0 and y > 0 (SE wind, correction is 180 degrees)
c if x < 0 and y < 0 (NE wind, correction is 0 degrees)
c if x > 0 and y < 0) (NW wind, correction is 360 degrees)
      real x,y,corr,dir
      corr=0.0
      dir=0.0
      
      if ((x .gt. 0.0 .and. y .gt. 0.0) .or.
     +  (x .lt. 0.0 .and. y .gt. 0.0)) then 
        corr=180.0
      endif
      if (x .lt. 0.0 .and. y .lt. 0.0)then
        corr=0.0
      endif
      if (x .gt. 0.0 .and.  y .lt. 0) then 
        corr=360.0
      endif
c     account for exactly east, west, north, or southerly winds
c     north or south wind
      if (x .eq. 0.0) then
        if (y .gt. 0.0) then 
          dir=360.0
        else
          dir=180.0
        endif
      endif
c     east or west wind
      if (y .eq. 0.0) then
        if (x .gt. 0.0) then
          dir=270.0
        else
          dir=90.0
        endif
      endif        
      if (x .ne. 0.0 .and. y .ne. 0.0) then
        dir=real(nint(atan(x/y)/d2r))+corr
      endif
c     if wind direction is 0 set to 360
c     wind speed is non-zero because function is only called when hourly keep flag is 1 (valid non-calm)
      if (dir .eq. 0.0) dir=360.0
      
      return
      end
c*******************************************************************************
c subroutine changehr
c called from readline
c this subroutine adds 1 to the hour of the record from the one minute file
c also changes hour 0 and minute 0 to hour 24 minute 60 of previous day
      subroutine changehr(iyear,imonth,iday,ihr,imin)
      use main1
      implicit none

c iyear:    integer 4-digit year of record 
c imonth:   integer 2-digit month of record
c iday:     integer 2-digit day of record
c ihr:      integer 2-digit hour of record
c imin:     integer minute of record 
c days:     integer number of days per month, set February = 28
c leap:     logical variable denoting if year is leap year (true=leap year, false=non-leap year)


      integer imin,ihr,iday,imonth,iyear
      logical leap

c if hour is 0 and minute is 0, change hour to 24 and change day to day before date of record
c example:  hour 0 minute 0 for March 2, 2002:  date becomes March 1, 2002, hour 24, minute 60

      if (ihr .eq. 0 .and. imin .eq. 0) then
        ihr=24
        imin=60
c        if first day of month set to the last day of previous month
c        and reset month to previous month as well
c        if January 1, reset year to previous year, meaning minute will not be used at all if 
c        start date of processing is January 1
        if (iday .eq. 1) then
          if (imonth .eq. 1) then
            iday=days(12)
            imonth=12
            iyear=iyear-1
          else
c           if March 1, check to see if year is leap year and reset to Feb. 28 or 29 accordingly
            if (imonth .eq. 3) then
              call leapyr(iyear,leap)
              if (leap) then
                iday=29
              else
                iday=28
              endif
            else
              iday=days(imonth-1)
            endif
            imonth=imonth-1
        endif
       else
           iday=iday-1
         endif
      else
c       if imin is not equal to zero, just add 1 to hour (if minute = 2 and hour =23, hour =24)
        if (imin .gt. 0) then
          ihr=ihr+1
c       if hour is not zero and minute is 60, just reset minute to 60
        else
          imin=60
        endif
      endif
      
      return
      end
     
c***************************************************************
c subroutine leapyr
c called from changehr, makedate
c determines if year is leap year
c calculations based on formulas found in Frequently Asked Questions about Calendars," Version 2.2,
c by Claus Tondering, April 9, 2000, available on the web at URL
c http://www.tondering.dk/claus/calendar.html
c from website:
c The Gregorian calendar has 97 leap years every 400 years: 

c Every year divisible by 4 is a leap year. 
c However, every year divisible by 100 is not a leap year. 
c However, every year divisible by 400 is a leap year after all. 
c So, 1700, 1800, 1900, 2100, and 2200 are not leap years. But 1600, 2000, and 2400 are leap years. 


      subroutine leapyr(iyear,leap)
      use main1
      implicit none
      
c iyear:    integer 4-digit year of processed year
c ileap4:   integer result of MOD(iyear,4)
c ileap100: integer result of MOD(iyear,100)
c ileap400: integer result of MOD(iyear,400)
      integer iyear,ileap4,ileap100,ileap400
      logical leap
      
      ileap4=mod(iyear,4)   
      ileap100=mod(iyear,100)
      ileap400=mod(iyear,400)

c if year is not divisible by 4 or year is divisible by 100 but not divisible by 400 then year 
c is not a leap year
c 2001 is not a leap year because it is not divisible by 4
c 2000 is is a leap year because it is divisible by 400      
      if (ileap4 .ne. 0 .or. 
     +  (ileap100 .eq. 0 .and. ileap400 .ne. 0)) then 
        leap=.false.  
      else
        leap=.true.  
      endif
      return
      end
c**************************************************************
c subroutine makedate
c called from assignflag
c subroutine to create a date for hours not in the one minute files
c used in hourly count file output by program
c inputs are month,and day, with output being the date

      subroutine makedate(m,d,idate1)
      use main1
      implicit none

c idate1:   integer date YYYYMMDD
c m:        integer 2-digit input month (relative to start month)
c d:        integer 2-digit day
c imon:     integer 2-digit month (1 through 12)
c iyr:      integer 4-digit year
c b:        real result of mod(m+startmon-1,12)
c leap:     logical variable denoting if year is leap year (true=leap,false=non-leap)    
      integer idate1,m,d,imon,yr
      real b
      logical leap

      leap=.false.
c calculate b where m+startnmon-1 is the month in question relative to the start month
c if start month is February 2001 and month in question is January 2002, January is the 12 month
c being processed (with February 2001 as month 1). January 2002 is 11 months from February 2001
c and b is to determine if the month in question is at the end of a calendar year
      b=mod(real(m+startmon-1),12.0)
      
c if b is zero month in question is December
      if (b .eq. 0) then 
        yr=((m+startmon-1)/12)+startyear-1
      else
c month is not December
        yr=((m+startmon-1)/12)+startyear
      endif
c calculate month of year and day
      imon=(m+startmon-1)-(yr-startyear)*12
      if (imon .eq. 2 .and. d .ge. 28) then
        if (d .eq. 28) then
          idate1=(yr*10000)+(imon*100)+d
        elseif (d .eq. 29) then
          call leapyr(yr,leap)
          if (leap) then
            idate1=(yr*10000)+(imon*100)+d
          else
            idate1=0
          endif
         else
           idate1=0
         endif
      else
c       reset idate for non-existent days like April 31
        if ((imon .eq. 4 .or. imon .eq. 6 .or. imon .eq. 9 .or. 
     +    imon .eq. 11) .and. d .eq. 31) then
          idate1=0
        else
c calculate idate1
          idate1=(yr*10000)+(imon*100)+d
        endif
      endif
      
      return
      end
c**************************************************************
c called from readline
c subroutine to check for non-numeric characters in columns for direction and speed
c input is record of one-minute file and outputs are a flag, wind direction and speed
c version 14237, modified checks for flags 6-9 to account for change in format of 1-minute
c data files
      
      subroutine checkwind(var1,iflag,aflag,idir1,ispeed1)  
      use main1
      implicit none
c updated version 14337      
c var1:     line of one minute data record being processed
c junk:     1-character variable with value 0 through 9
c zeros:    3-character array to check for leading zeros
c aflag:    11 character string of iflag1 values
c iflag:    integer variable denoting if record is to be further processed (0=yes,1=no)
c           uses values from array iflag1
c iflag1:   10 element integer array of flags for 9 error checks
c           values
c           0=valid
c           1=invalid or suspicious
c           positions
c           1=non-numeric characters seen in columns 68 through 90
c           2=leading zero
c           3=4-digit string that may be a time
c           4=non-blank value in column 30
c           5=wind direction problem 
c           6=wind speed problem 
c           7=gust direction problem 
c           8=gust speed problem 
c           9=column 90
c           10=bad wind values
c           12=read for numeric fields in wind locations without checking for specific columns but
c           data does not fit column specifications, i.e. iflag1(6)-iflag1(9) = 1
c iflag2    number of fields read by getwinds when data passes strict QA checks
c idir1:    integer 2-minute average wind direction
c ispeed1:  integer 2-minute average wind speed (knots)
c idir2:    integer 5-second peak wind speed's wind direction
c ispeed2:  integer 5-second peak wind speed (knots)
c j1,j2:    integer values for index function
c i2:       integer counter for chars and chars2
c i1:       integer counter
c p1:       character code for leading blank of 4-character string for flag 3
c p2:       character code for first character of 4-character string for flag 3
c p3:       character code for second character of 4-character string for flag 3
c p4:       character code for third character of 4-character string for flag 3
c iv1:      4-element array of start columns to check for wind data
c iv2:      4-element array of end columns to check for wind data
c c:        column loop counter
c c1:       column counter equal to values between iv1 and iv2
c c2:       column loop counter
c v:        array of character codes
c id:       loop counter equal to iv2-iv1+1
c aflag:    11 character string of iflag1 values
c noblanks: string being read has noblanks
      


      character var1*113,junk*1,zeros(10)*3,aflag*11,c39*1,c40*1,c41*1
      character str*84,fields(42)*84
      logical l1,notnum,noblanks
      integer iflag,iflag1(11),idir1,ispeed1,j1,j2,j3,ilen1,idir2,
     +  ispeed2,i1,i2,p1,p2,p3,p4,iv1(4),iv2(4),c,c1,c2,v1,v2,ifield,k,
     +  i,ii,id,iflag2
      integer, allocatable, dimension(:) :: v
      character form*35
c set values for zeros
      data zeros /' 00',' 01',' 02',' 03',' 04',' 05',' 06',' 07',' 08',
     +  ' 09'/
    
      data iv1 /70,76,82,87/
      data iv2 /74,79,84,89/
       
c initialize wind directions and speeds as well as iflag and iflag1 
      idir1=-9
      ispeed1=-9
      idir2=-9
      ispeed2=-9
      iflag=0
      iflag1=0
      fields=''
c example of record
c                                                                   123456 1234  123  1234
c13722KRDU RDU2001010101470647   0.087 N     0.086 N                264      5   258    5 
c in first line above 123456 refers to columns 68-73, the first 1234 are columns 75-78,
c 123 are columns 81-83 and the last 1234 are columns 86-89

c check record for a time string, 0100, 2314 outside of the station/date information block
c sometimes records appear to be "jammed" into another record, correcting for time
c if such a record is found, do not use.
c      i2=1
c      do while (iflag1(1) .eq. 0 .and. i2 .le. 1440)
c        j1=index(var1(30:90),times(i2))
c        if (j1 .gt. 0) then
c          iflag1(1)=1
c        else
c          i2=i2+1
c        endif  
c      enddo
           


c check columns 67 through 90 for any nonnumeric characters
c if any found do not use line
c     version 14337, reset i2 to 68      
!      i2=67
      i2=68
      do while (iflag1(1) .eq. 0 .and. i2 .le. 90)
        j1=iachar(var1(i2:i2))
        if (j1 .eq. 32 .or. (j1 .ge. 48 .and. j1 .le. 57)) then
          i2=i2+1
        else
          iflag1(1)=1
        endif
      enddo
      

      
c there should be not be anything with a leading zero, i.e 01, 02, 035, etc. 
      i2=1
      do while (i2 .le. 10 .and. iflag1(2) .eq. 0)
        j1=index(var1(30:90),zeros(i2))
        if (j1 .gt. 0) then
          iflag1(2)=1
        else
          i2=i2+1
        endif
      end do


               

c    version 11298 check for numeric fields in columns 30-113 to see if possible leading time
c     if appears to be a time or a set of numeric characters with double decimals, flag as
c     suspicious
      i=30
      ifield=1
      l1=.false.
      k=0
      do while(ifield .le. 42 .and. i .le. 113)
        i1=iachar(var1(i:i))
        if (i1 .ne. 32) then
          l1=.true.
          k=k+1
          str(k:k)=var1(i:i)
        else
          if (l1) then
            fields(ifield)=str
            ifield=ifield+1
            l1=.false.
            k=0
            do ii=1,84
              str(ii:ii)=' '
            enddo
          endif
        endif
       i=i+1
      enddo

c check for numerical fields and determine if a possible 4-digit time
      do i=1,ifield
        str=fields(i)
        call checkfield(str,iflag1(3))
      enddo


c     check column 30, should always be blank

      j1=iachar(var1(30:30))
      if (j1 .ne. 32) iflag1(4)=1
      

         
c now check column 67.  there should not be a number there 
c version 298:  should be blank, any other than blank, flag
c version 14337, do not check
!      j1=iachar(var1(67:67))
!      if (j1 .ne. 32) iflag1(5)=1
         
!      if (j1 .ge. 48 .and. j1. le. 57) iflag1(5)=1  
           
c check column 71 and 70.  Column 70 should have a number if the wind direction is in 68-70, 69-71, or 70-72
c column 71 should have a number for wind direction in columns 69-71, 70-72, or 71-73

c check columns 70 and 71 for 2-minute wind direction, 76 and 77 for 2-minute wind speed,
c 82 and 83 for 5-second gust, and 87 and 88 for 5-sec gust speed
c experience has shown that data should be in these columns
c if no number is found, flag the record


c version 14237, modify checks for directions and speeds based on new format of 1-minute files
c data appears to be shifted to the right in late 2013 and later files
      do c=1,4
          noblanks=.false.
          id=iv2(c)-iv1(c)+1
          allocate(v(id))
          c1=iv1(c)
          do c2=1,id
              v(c2)=iachar(var1(c1:c1))
              if (v(c2) .ne. 32)noblanks=.true.
              c1=c1+1
          enddo 
          
          do c2=1,id
              if ((v(c2) .lt. 48 .and. v(c2) .ne. 32) 
     +        .or. v(c2) .gt. 57) iflag1(c+4)=1
              if (c2 .gt. 1 .and. c2 .lt. id) then  !blank between 2 non-blanks
                  if (v(c2) .eq. 32. .and. v(c2-1) .ne. 32 .and. 
     +            v(c2+1) .ne. 32) iflag1(c+4)=1
              endif
!              if (.not. noblanks) iflag1(c+5)=1  !all blanks
              if (.not. noblanks) iflag1(c+4)=1  !all blanks
          enddo
          deallocate(v)
      enddo


c     version 14337, don't check
c     version 14237, new flag,flag 10, no numbers in columns 64-68
!      do i=64,68
!          j1=iachar(var1(i-1:i-1))
!          j2=iachar(var1(i:i))
!          j3=iachar(var1(i+1:i+1))
!          if (j1 .eq. 32 .and. j2 .ne. 32 .and. j3 .eq. 32) iflag1(10)=1
!      enddo


c     version 14237, check column 90, if something there make iflag1(9) = 1
      allocate(v(1))
      v(1)=iachar(var1(90:90))
      if (v(1) .ne. 32) iflag1(9)=1
      deallocate(v)

     

c read 2-minute average wind direction and speed and 2-minute gust and speed
c version 301, change wind speed to catch for 50 knots
c version 14337, read 68:90
      if (maxval(iflag1) .eq. 0) then            !possible good record according strict checks
        call getwinds(var1(68:90),idir1,ispeed1,idir2,ispeed2,
     +    iflag2)
        
        if (iflag2 .eq. 4) then
          read(var1(68:90),*)idir1,ispeed1,idir2,ispeed2
          if ((idir1 .gt. 360 .or. idir1 .lt. 0) .or. 
     +    (idir2 .gt. 360 .or. idir2 .lt. 0) .or. 
     +    (ispeed1 .ge. 50 .or. ispeed1 .lt. 0) .or.
     +    (ispeed2 .ge. 50 .or. ispeed2 .lt. 0 )) then
             iflag1(10)=1
c              iflag1(12)=1
c             goto 100
          endif
         iflag1(11)=9
c          iflag1(13)=9
        else
c          iflag1(13)=iflag2
c          iflag1(12)=1 
          iflag1(11)=iflag2
          iflag1(10)=1 
        endif
      else
c      record does not have non-numeric characters in columns 68-90
c      and there are no leading zeros and and no n4-digit numbers
c      and wind data may not match specific column criteria
c      but other flags maybe switched on
c     version 14337, read columns 68:90
c        if (iflag1(2) .eq. 0 .and. iflag1(4) .eq. 0 .and. 
c     +   (iflag1(5) .ge. 0 .or. iflag1(6) .ge. 0 
c     +   .or. iflag1(7) .ge. 0 .or. iflag1(8) .ge. 0)) then    
        if (iflag1(1) .eq. 0 .and. iflag1(2) .eq. 0) then  
!         call getwinds(var1(67:90),idir1,ispeed1,idir2,ispeed2,
         call getwinds(var1(68:90),idir1,ispeed1,idir2,ispeed2,
c     +    iflag1(13))
     +    iflag1(11))
        endif
      endif

c get maximum flag value for first 10 flags
c also if flag1(i2) is not zero add to flagcounts
c version 14132, now first 11 flags
      do i2=1,10
c      do i2=1,12
        write(aflag(i2:i2),'(i1)')iflag1(i2)
        iflag=max(iflag,iflag1(i2))
        if (iflag1(i2) .ne. 0) flagcounts(i2)=flagcounts(i2)+1
      enddo
      
      write(aflag(11:11),'(i1)')iflag1(11)
c      write(aflag(13:13),'(i1)')iflag1(13)

c get counts for last flag, flag 12
c if flag 11 = 4 then add 1 to flagcounts(11), count of records that are suspicious
c otherwise add 1 to flagcounts(12), count of records that are bad

      if (iflag .ne. 0) then
        if (iflag1(11) .eq. 4 .or. iflag1(11) .eq. 5) then 
            flagcounts(11)=flagcounts(11)+1   !count for check records
        else
            flagcounts(12)=flagcounts(12)+1   !count for bad records
        endif
      endif
       
 100  return
      end
c****************************************************
      subroutine checkfield(a,iflag)
c     check answers to prompts to determine if response has invalid
c     character, i.e. response is <return> or numeric
c     routine also processes variables that should be character length 1
      use main1
      implicit none

c a:        response entered by user
c b:        left-justified version of a
c lflag:    logical variable denoting if response is bad (true) or acceptable (false)
c lblank:   logical variable denoting if response is all blanks (true) or not (false)
c ldecimal: 
c i:        integer code of character in ASCII table  
c i1:       loop counter for character string a
c idash:    number of dashes read from response
c idecimal: number decimals read from response
c iblank:   number of blanks read from response
c icode:    code indicating what type of final response is
c           icode=1 then final answer is a character, such as a Y or N, or blank
c           icode=2 then final answer is a number
c ie:       number of upper or lowercase E's in string.  E represents exponential format
c iplus:    number of plus signs read from response
c ichar1:   2 element array of number of characters before and after blank
c charstr:  15 element array of integer ASCII codes of response

c lenb:       length of string b without blanks
      character a*84,b*84
      logical lflag,lblank
      integer i,i1,idash,idecimal,iblank,icode,ie,iplus,
     + ichar1(2),charstr(84),lenb,itime,iflag
     
  
c initialize variables
      idash=0
      ie=0
      iplus=0
      ichar1=0
      idecimal=0
      iblank=0
      charstr=32
      lflag=.false.
      lblank=.false.
      lenb=0


      b=trim(adjustl(a))
      lenb=len_trim(b)
      if (lenb .eq. 0) then
        lblank=.true.
        goto 100
      else 
        do i1=1,lenb
          call upcase(b(i1:i1))  !convert to upper case
        enddo
      endif
      

        do i1=1,lenb
          i=ichar(b(i1:i1))
          charstr(i1)=i
          if ((i .ge. 48 .and. i .le. 57) .or. i .eq. 45 .or. i .eq. 46 
     +    .or. i .eq. 69 .or. i .eq. 43) then
            if (i .eq. 45) idash=idash+1
            if (i .eq. 46) then 
              idecimal=idecimal+1
              if (ie .gt. 0)lflag=.true.
            endif
            if (i .eq. 69) ie=ie+1
            if (i .eq. 43) iplus=iplus+1
c          lflag=.false.
          else
            lflag=.true.
          endif
          if (idash .gt. 2 .or. idecimal .gt. 1 .or. ie .gt. 1 .or. 
     +    iplus .gt. 2)! .or. (ichar1(1) .ne. 0 .and. ichar1(2) .ne. 0)) 
     +    lflag=.true.
          
        enddo


     
        if (lflag) goto 100
c     need to look for specific placements of characterics if there were no bad characters
c         if first character is an e or last character is not a blank, number, or e
c         then string is bad
        if ((charstr(1) .eq. 69) .or. 
     +  ((charstr(lenb) .lt. 48 .and. charstr(lenb) .ne. 46)
     +   .or. (charstr(lenb) .gt. 57 .and. charstr(lenb) .ne. 69))) then
          lflag=.true.
          goto 100
        endif
c       check for a blank character between the first and last occurrence of non-blank character
        i=1 
        do while (i .le. lenb .and. .not. lflag)
          if (charstr(i) .eq. 32) lflag=.true.
          i=i+1
        enddo
        if (lflag) goto 100        
c       begin looking at the string beginning with position 2 through position 14
        i=2
        do while (i .le. lenb-1 .and. .not. lflag)
c           if character is an E, make sure that character before that is
c           a number or . sign and character afterwards is a number
c           or + or - sign or blank
          if (charstr(i) .eq. 69) then  !character is an e
            if (((charstr(i-1) .lt. 48 .and. charstr(i-1) .ne. 46) 
     +      .or. charstr(i-1) .gt. 57) .or. (charstr(i+1) .lt. 48 
     +      .and. charstr(i+1) .ne. 43 .and. charstr(i+1) .ne. 45)
     +      .or. charstr(i+1) .gt. 57) 
     +      lflag=.true. 
          endif     
c         if character is a - sign, preceding character should be a
c         number or blank or lower/uppercase e and proceeding character
c         should be a number
          if (charstr(i) .eq. 45 .and. (charstr(i-1) .ne. 69 .or. 
     +    (charstr(i+1) .lt. 48 .or. charstr(i+1) .gt. 57)))lflag=.true.      
c         if character is a + sign, preceding character should be a
c         number or blank or lower/uppercase e and proceeding character
c         should be a number
          if (charstr(i) .eq. 43 .and. (charstr(i-1) .ne. 69 .or. 
     +    (charstr(i+1) .lt. 48 .or. charstr(i+1) .gt. 57)))lflag=.true. 
          i=i+1
        enddo

c     check to see if number is a possible time.
c     this would be a number with no decimals, no plus signs, no dashes, no letter E and
c     length is at least 4 characters
      if (.not. lflag) then
        if (idash .eq. 0 .and. idecimal .eq. 0 .and. ie .eq. 0 .and. 
     +    iplus .eq. 0 .and. lenb .ge. 4) then  !number is an integer and could be a possible time
!          read(b,*)itime
!          if (itime .gt. 0 .and. itime .le. 2359) iflag=1
           iflag=1
        endif
      endif
          
100   return
      end
c****************************************************
c routine to check for numeric fields in wind data column
      subroutine getwinds(astring,idir1,ispeed1,idir2,ispeed2,iflag)
      use main1
      implicit none

c astring:        character string to read in for 1-minute data
c nums:           character string of numbers
c idir1:          integer wind direction
c speed1:         integer wind speed (knots)
c idir2:          integer wind gust direction
c ispeed:         integer wind gust (knots)
c iflag:          flag indicating QA status of data line
c ifield:         integer number of fields read in
c i:              integer counter
c cnum:           real number being read from astring
c lstart:         logical variable denoting numeric string starting

      character astring*(*),nums*10
      integer idir1,ispeed1,idir2,ispeed2,iflag,ifield,i
      real cnum
      logical lstart
      cnum=0.0
      ifield=0
      
      nums='0123456789'
      lstart=.false.
c check string for 4 fields
c     version 14337, reset loop to 23 to account for astring being columns 68:90 of text record
!      do i=1,24
       do i=1,23
        if (astring(i:i) .ne. ' ') then
          if (astring(i:i) .ge. '0' .and. astring(i:i) .le. '9') then
            lstart=.true.
            CNUM = CNUM*10.+float(index(nums,astring(i:i))-1)
            if (i .eq. 23) ifield=ifield+1
          endif 
        else
          if (lstart) then
            ifield=ifield+1
            lstart=.false.
            cnum=0.0
          endif   
        endif     
      enddo
   
      if (ifield .eq. 4 .or. ifield .eq. 5) then
        read(astring,*)idir1,ispeed1,idir2,ispeed2
        if ((idir1 .gt. 360 .or. idir1 .lt. 0) .or. 
     +  (idir2 .gt. 360 .or. idir2 .lt. 0) .or. 
     +  (ispeed1 .ge. 50 .or. ispeed1 .lt. 0) .or.
     +  (ispeed2 .ge. 50 .or. ispeed2 .lt. 0 )) then
         iflag=8   
        else
          iflag=ifield
        endif
      else
        iflag=ifield
      endif
     
      return
      end
c******************************************
c subroutine to write output for input into AERMET
c 15272 summarize 5-minute data
      subroutine writeout
      use main1
      implicit none
      
c i:            integer month counter
c j:            integer day counter
c k:            integer hour counter
c iyr:          integer 2-digit year
c imon:         integer 2-digit calendar month
c imonth1       calendar month
c iyear1:       calendar year

c iday:         integer 2-digit day of month
c iyr4:         integer 4-digit year
c valhrs:       integer number of valid hours
c calmhrs:      integer number of calm hours
c nvhrs:        integer number of non-valid hours.
c               these are hours with minutes in one-minute files but
c               not processed because they did not meet the completeness criteria
c               (did not have a non-calm minute in each 1/2 of the hour)
c icols:        integer number of columns for nvalid (5=no 5-minute data,6=5-minute)
c xx:           column counter xx=1 to icols
c ifwlfag1:     IFW indicator for each hour (0=before IFW date,1=after IFW date)
c mindate:      date of minspeed
c maxdate:      date of maxspeed
c minspeed:     real minimum hourly wind speed
c maxspeed:     real maximum hourly wind speed
c p1:           real percentage of processed hours/total hours of data period
c p2:           real percentage of valid hours/total hours of data period
c p3:           real percentage of non-valid hours/total hours of data period
c p4:           real percentage of calm hours/total hours of data period
c mindate1:     date of minimum 2-minute wind speed 
c               hour is reset to original hour and date for ease of checking in files
c maxdate1:     date of maximum 2-minute wind speed
c               hour is reset to original hour and date for ease of checking in files
c aifwdate:     date of IFW commission 
c junk:         date of IFW commission from ifwdate
c ifwflag:      flag to indicate if data period is IFW 
c form:         format statement for WBAN count and station count
c adate:        character string of current date being processed
c txtstr:       2-character string denoting status of hour
c               V=valid,NV=non-valid,M=missing (not read)
c acom:         1-character variable,used to write a comma delimited
c               file of the summary file.  The variable is set to a comma
c               flag denoting 5-minute data read
c a5min:        flag indicating if 5-minute data read

      integer i,j,k,iyr,imon,iday,iyr4,valhrs,nvhrs,calmhrs,imonth1,
     + iyear1,xx,ifwflag1,icols
      real minspeed,maxspeed,p1,p2,p3,p4
      character mindate*10,maxdate*10,mindate1*12,maxdate1*12,adate*8,
     +  txtstr*2,acom*1,aifwdate*10,junk*8,ifwflag*1,form*10,a5min*1
      
c initialize variables      
      minspeed=100.0
      maxspeed=0.0
      valhrs=0
      nvhrs=0
      calmhrs=0
      acom=','
      
      write(*,1)
      write(ilogunit,1)
      write(*,2)

  1   format(/1x,'################################ ','SUMMARY',
     +  ' ###############################')
  2   format(/1x,'Writing output...')

c     open output and summary files
      open(unit=ioutunit,file=outfil,status='replace')
      if (writesum) then
        open(unit=isumunit,file=sumfil,status='replace')      
c       write a header line for the hourly summary file
        if (.not. run5) then
          write(isumunit,121)
        else
          write(isumunit,126)
        endif
      endif
c     write flag for 5-minute data
      if (run5) then
          a5min='Y'
          icols=6
      else
          a5min='N'
          icols=5
      endif
      
c write a header line for the hourly averaged winds
c header will include WBAN, call sign, IFW date, program name and version #
      if (ifw) then
        ifwflag='Y'
        write(junk,'(i8.8)')ifwdate
        aifwdate(1:2)=junk(5:6)
        aifwdate(3:3)='/'
        aifwdate(4:5)=junk(7:8)
        aifwdate(6:6)='/'
        aifwdate(7:10)=junk(1:4)
        write(ioutunit,96)versn,iwban,stat,ifwflag,aifwdate,a5min
      else
        ifwflag='N'
        aifwdate=''
        write(ioutunit,97)versn,iwban,stat,ifwflag,a5min
      endif
      
      monthloop: do i=1,nmonths        
        dayloop: do j=1,ndays(i)
          if (idate(i,j) .gt. 0 .and. idate(i,j) .ge. startdate .and. 
     +      idate(i,j) .le. enddate) then
            write(adate,'(i8)')idate(i,j)
            read(adate(1:4),'(i4)')iyr4
            read(adate(3:4),'(i2)')iyr
            read(adate(5:6),'(i2)')imon
            read(adate(7:8),'(i2)')iday
            hourloop: do k=1,24
              if (ikeephr(i,j,k) .eq. 1) then
                 txtstr='V'
                 valhrs=valhrs+1
                 call minmaxhr(i,j,k,minspeed,maxspeed)
              else if (ikeephr(i,j,k) .eq. 0) then
                txtstr='C'
                calmhrs=calmhrs+1
              else if (ikeephr(i,j,k) .eq. -2) then
                 txtstr='NV'
                 nvhrs=nvhrs+1
              else
               txtstr='M'
              endif
              write(ioutunit,100)iyr,imon,iday,k,
     +             speed(i,j,k),dirhr(i,j,k)
              if (writesum) then
                if (idate(i,j) .ge. ifwdate
     +            .and. ifw .and. ikeephr(i,j,k) .ne. -9) then
                  ifwflag1=1
                else
                  ifwflag1=0
                endif
                if (.not. run5) then
                  write(isumunit,120)adate,acom,k,acom,txtstr,acom,
     +            ifwflag1,acom,nvalid(i,j,k),acom,ncalms(i,j,k),
     +            acom,neven(i,j,k),acom,nevencalm(i,j,k),acom,
     +            noddperm(i,j,k),acom,noddpermc(i,j,k),acom,
     +            nodd(i,j,k),acom,noddcalm(i,j,k),acom,speedmin(i,j,k),
     +            acom,speed(i,j,k),acom,speedmax(i,j,k),acom,
     +            dirmin(i,j,k),acom,int(dirhr(i,j,k)),acom,
     +            dirmax(i,j,k)
                else
                  write(isumunit,127)adate,acom,k,acom,txtstr,acom,
     +            ifwflag1,acom,nvalid(i,j,k),acom,nvalid5(i,j,k),acom,
     +            ncalms(i,j,k),acom,neven(i,j,k),acom,neven5(i,j,k),
     +            acom,nevencalm(i,j,k),acom,noddperm(i,j,k),acom,
     +            nodd5(i,j,k),acom,noddpermc(i,j,k),acom,nodd(i,j,k),
     +            acom,noddcalm(i,j,k),acom,speedmin(i,j,k),acom,
     +            srcmin(1,i,j,k),acom,speed(i,j,k),acom,
     +            speedmax(i,j,k),acom,srcmax(1,i,j,k),acom,
     +            dirmin(i,j,k),acom,srcmin(2,i,j,k),acom,
     +            int(dirhr(i,j,k)),acom,dirmax(i,j,k),acom,
     +            srcmax(2,i,j,k)
                endif
                
              endif
            end do hourloop
          endif
        end do dayloop
      end do monthloop

c write the original date of the min and max 2-minute wind speed for ease of 
c comparison against the original data files
      mindate1=origdate(nmin,daymin,hrmin,minmin)
      maxdate1=origdate(nmax,daymax,hrmax,minmax)

c write min and max hourly averaged wind speed
      write(mindate(1:8),'(i8)')idate(nhrmin,dayhrmin)
      write(mindate(9:10),'(i2.2)')hrhrmin
      write(maxdate(1:8),'(i8)')idate(nhrmax,dayhrmax)
      write(maxdate(9:10),'(i2.2)')hrhrmax
      
      write(ilogunit,130)minonemin,idirmin(nmin,daymin,hrmin,minmin),
     +  mindate1,minsrc1,maxonemin,idirmin(nmax,daymax,hrmax,minmax),
     +  maxdate1,maxsrc1,minspeed,dirhr(nhrmin,dayhrmin,hrhrmin),
     +  mindate,maxspeed,dirhr(nhrmax,dayhrmax,hrhrmax),maxdate
      
      write(*,130)minonemin,idirmin(nmin,daymin,hrmin,minmin),
     +  mindate1,minsrc1,maxonemin,idirmin(nmax,daymax,hrmax,minmax),
     +  maxdate1,maxsrc1,minspeed,dirhr(nhrmin,dayhrmin,hrhrmin),
     +  mindate,maxspeed,dirhr(nhrmax,dayhrmax,hrhrmax),maxdate
     
      p1=(real(valhrs+calmhrs+nvhrs)/real(itothrs))*100.0
      p2=(real(valhrs)/real(itothrs))*100.0
      p3=(real(nvhrs)/real(itothrs))*100.0
      p4=(real(calmhrs)/real(itothrs))*100.0
      write(ilogunit,131)itothrs,valhrs+calmhrs+nvhrs,p1,valhrs,p2,
     +  nvhrs,p3,calmhrs,p4
      write(*,131)itothrs,valhrs+calmhrs+nvhrs,p1,valhrs,p2,
     +  nvhrs,p3,calmhrs,p4
c     write monthly summaries  
      if (icols .eq. 5) then
          write(ilogunit,122) 
      else
          write(ilogunit,125)
      endif
      do i=1,nmonths
        call monthyear(i,imonth1,iyear1)
        if (icols .eq. 5) then
          write(ilogunit,123)iyear1,trim(adjustl(amonths(imonth1))),
     +    (nhours(i,xx),xx=1,icols)
        else
            write(ilogunit,124)iyear1,trim(adjustl(amonths(imonth1))),
     +    (nhours(i,xx),xx=1,icols)
        endif
      enddo
      
      if (lwban) then
        write(*,133)iwban2,wbancnt
        write(ilogunit,133)iwban2,wbancnt
        if (wbancnt .le. 10) then
            write(*,136)
            write(ilogunit,136)
            if (wbancnt .le. 9) then
                form(1:1)='('
                write(form(2:2),'(i1)')wbancnt
                form(3:9)='(x,i5))'
            else
                form='(10(x,i5))'
            endif
            write(*,form)(wbans(i),i=1,wbancnt)
            write(ilogunit,form)(wbans(i),i=1,wbancnt)
        else
            write(*,138)
            write(ilogunit,138)
        endif
      endif
      if (lstat) then
        write(*,134)stat2,statcnt
        write(ilogunit,134)stat2,statcnt
        if (statcnt .le. 10) then
            write(*,137)
            write(ilogunit,137)
            if (statcnt .le. 9) then
                form(1:1)='('
                write(form(2:2),'(i1)')statcnt
                form(3:9)='(x,a4))'
            else
                form='(10(x,a4))'
            endif
            write(*,form)(stats(i),i=1,statcnt)
            write(ilogunit,form)(stats(i),i=1,statcnt)
        else
            write(*,139)
            write(ilogunit,139)
        endif                     
      endif
      write(*,135)
      write(ilogunit,135)
     
      close(ioutunit)
      if (writesum)close(isumunit)
 96   format('AERMINUTE Version ',a6,1x,'WBAN: ',i5,3x,' Call sign: ',
     +  a4,3x,' IFW: ',a1,3x,' IFW date: ',a10,1x,'5-MIN USED: ',a1)
 97   format('AERMINUTE Version ',a6,1x,'WBAN: ',i5,3x,' Call sign: ',
     +  a4,3x,' IFW: ',a1,1x,'5-MIN USED: ',a1)  
       
100   format(4(i2,1x),f6.2,1x,f5.1)
 120  format(a8,a1,i2.2,a1,a2,9(a1,i2),3(a1,f6.2),3(a1,i3))
 127  format(a8,a1,i2.2,a1,a2,12(a1,i2),a1,f6.2,a1,i2,2(a1,f6.2),a1,i2,
     +a1,i3,a1,i2,a1,i3,a1,i3,a1,i2)
 121  format('Date,hr,flag,IFW flag,total minutes,',
     +  'total calms,total even,even calms,total odd,total odd calm',
     +  ',odd used,odd calms used,min speed,avg speed,max speed,min dir'
     +  ,',avg dir,max dir')
  126 format('Date,hr,flag,IFW flag,total minutes,',
     +  'total 5-min subs,total calms,total even,total even 5-min subs,'
     +  ,'even calms,total odd,total odd 5-min subs,total odd calm',
     +  ',odd used,odd calms used,min speed,min speed source,avg speed,'
     +  ,'max speed,max speed source,min dir,min dir source,avg dir,',
     +  'max dir,max dir source')
 122  format(/t22,'TOTAL   VALID   INVALID    CALM    MISSING'/
     +   1x,'YEAR      MONTH     HOURS   HOURS    HOURS     HOURS    ',
     +   'HOURS'/
     +   1x,'-------------------------------------------------------',
     +   '-------')
 125  format(/t22,'TOTAL   VALID   INVALID    CALM    MISSING    5-MIN'/
     +   1x,'YEAR      MONTH     HOURS   HOURS    HOURS     HOURS    ',
     +   'HOURS    MINUTES'/
     +   1x,'-------------------------------------------------------',
     +    '-----------------')
123   format(1x,i4,1x,a10,5(6x,i3))
124   format(1x,i4,1x,a10,5(6x,i3),6x,i5)
130   format(//1x,'Minimum 2-minute wind speed (knots), direction ',
     +  'and original date (YYYYMMDDHHmm)'/1x,f6.2,1x,i3,1x,a12,3x,'*',
     +  i1,'-minute data',/1x,
     +  'Maximum 2-minute wind speed (knots), direction and original ',
     +  'date (YYYYMMDDHHmm)'/1x,f6.2,1x,i3,1x,a12,3x,'*',i1,
     +  '-minute data',/1x,//1x,
     _  'Minimum hourly wind speed (m/s), direction and date ',
     +   '(YYYYMMDDHH)'/1x,f6.2,1x,f5.1,1x,a10/1x,
     +  'Maximum hourly wind speed (m/s), direction and date ',
     +  '(YYYYMMDDHH)'/1x,f6.2,1x,f5.1,1x,a10//)
     
 131  format(/1x,'Number of total hours in data period: ',i6/1x,
     +   'Number of processed hours: ',11x,i6,' (',f6.2,'%)'
     +  /1x,'Number of valid hours: ',15x,i6,' (',f6.2,'%)'/1x,
     +  'Number of processed non-valid hours: ',1x,i6,' (',f6.2,'%)'
     +  /1x,'Number of calm hours: ',16x,i6,' (',f6.2,'%)'/1x)
 133  format(/1x,'First WBAN number in data: ',i5/1x,
     +  'WBAN numbers change: ',i4,' times'/)
 136  format(1x,'Alternate WBAN numbers are:'/)
134   format(/1x,'First call sign in data: ',a4/1x,
     + 'Call sign change: ',i4,' times'/)
137   format(1x,'Alternate call signs are:'/)
135   format(/1x,'####################################################',
     +   '####################')  
138   format(1x,'Number of different WBAN numbers exceed limit of 10')
139   format(1x,'Number of different station call signs exceed limit',
     +  ' of 10')
      return
      end
c***********************************
      subroutine monthyear(m,imon,iyr)
      use main1
      implicit none

c m:      integer month relative to start month
c imon:   calendar month
c iyr:    calendar year
c b:      modulus of m+startmon-1,12

      integer m,imon,iyr
      real b
      
      b=mod(real(m+startmon-1),12.0)
      if (b .eq. 0) then 
        iyr=((m+startmon-1)/12)+startyear-1
      else
        iyr=((m+startmon-1)/12)+startyear
      endif
      imon=(m+startmon-1)-(iyr-startyear)*12
      return
      end
c***********************************
c subroutine to determine minimum and maximum hourly wind speed
      subroutine minmaxhr(i,j,k,minspeed,maxspeed)
      use main1
      implicit none
c i:            integer month counter
c j:            integer day counter
c k:            integer hour counter   
c minspeed:     minimum hourly wind speed (m/s)
c maxspeed:     maximum hourly wind speed (m/s)         
      integer i,j,k
      real minspeed,maxspeed
      if (speed(i,j,k) .lt. minspeed) then 
           minspeed=speed(i,j,k)
           nhrmin=i
           dayhrmin=j
           hrhrmin=k
      endif
      if (speed(i,j,k) .gt. maxspeed) then
           maxspeed=speed(i,j,k)
           nhrmax=i
           dayhrmax=j
           hrhrmax=k
      endif
      return
      end
c*************************************************
      subroutine cleanup
c subroutine to deallocate data arrays
      use main1
      implicit none
      if (.not. lno1dat) deallocate(infiles)
      deallocate(ndays) 
      deallocate(idate)
      deallocate(origdate)
      deallocate(ikeephr)
      deallocate(ikeep)
      deallocate(icalm)
      deallocate(idirmin)
      deallocate(nvalid)
      deallocate(ncalms)
      deallocate(neven)
      deallocate(nevencalm)
      deallocate(nodd)
      deallocate(noddcalm)
      deallocate(noddperm)
      deallocate(noddpermc)
      deallocate(speed)
      deallocate(speed1)
      deallocate(speedkts)
      deallocate(xdir)
      deallocate(ydir)
      deallocate(dirminut)
      deallocate(xdirhr)
      deallocate(ydirhr)
      deallocate(dirhr)
      deallocate(speedmin)
      deallocate(speedmax)
      deallocate(dirmin)
      deallocate(dirmax)
      deallocate(nhours)
      if (comp) then
        deallocate(obfiles)
        deallocate(obspd)
        deallocate(obdir)
        deallocate(flag)
        deallocate(qcflag)
      endif    
c     15272, 5-minute arrays
      if (lfive) then
          deallocate(nvalid5)
          deallocate(neven5)
          deallocate(nodd5)
          deallocate(ikeep5)
          deallocate(icalm5)
          deallocate(idir5min)
          deallocate(speed5)
          deallocate(speed5kts)
          deallocate(xdir5)
          deallocate(ydir5)
          deallocate(dir5minut)
          deallocate(origdate5)
      endif
      
          
      return
      end      