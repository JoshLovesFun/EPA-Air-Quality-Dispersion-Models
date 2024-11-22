 
                              README.TXT                           12/14/98

                   MIXING HEIGHT PROGRAM (Dated 98340)       

The mixing height program is a computational program which calculates the
twice daily mixing heights, one morning and one afternoon mixing height, for
use in meteorological processing for ISCST3-type models.

Files on SCRAM concerning the mixing height program are the following:

1.  MIXHTS.ZIP   - contains the executable and README.TXT
2.  MIXHCODE.ZIP - contains the source code and batch file for recompiling
3.  MIXHTSD.ZIP  - mixing height program user instructions
4.  MIXEX1.ZIP   - example input and output files using HUSWO data


File inputs to the mixing height program are a surface station and an upper
air station.  The surface station data can be in either the SAMSON (Solar and
Meteorological Surface Observation Network) CD format or the HUSWO (Hourly
United States Weather Observations) CD format.  SAMSON (3 CDs total) contains
data for years 1961-1990.  HUSWO (1 CD) contains data for 1990-1995.  The
Radiosonde Data for North America upper air CD (4 CDs total) contains twice
daily soundings for the period of 1946-1996.  NOTE:  This CD is updated
yearly by the National Climatic Data Center (NCDC) as new data is processed.

Surface station latitude, longitude, and GMT time difference are also required
as inputs.  This information is available from SCRAM in the Met. Data area,
under Related Programs.  Click on STATIONS.


In order to execute the mixing height program, type

                                mixhts

at the DOS prompt where the program resides.  The program will prompt the
user for input information.

IMPORTANT NOTE:  When using 1995 data from the HUSWO CD, an additional
                 observation must added to the end of the file.  Simply
                 duplicate the last ob given, and change the hour from 23 to
                 24.  This allows PCRAMMET to process day 31 of December.
                 HUSWO does NOT contain hour 24 since it is technically hour
                 00 for the next day (which is in 1996!).



CD-ROM data is available from NCDC in Asheville, NC.  Orders can be made by
the following methods:

(1)  Mail orders:  National Climatic Data Center
                   151 Patton Avenue
                   Ashville, NC   28801-5001
                   ATTN:  Climatic Services Branch

(2)  Telephone orders:  828-271-4800
(3)  Fax orders:        828-271-4876
(4)  INTERNET orders:   orders@ncdc.noaa.gov
(5)  On-line ordering:  www.ncdc.noaa.gov   (see On-line store; credit card
                                             number required)

On-line ordering prices typically include a 10-15% discount in price.  NCDC
encourages data orders via INTERNET.

Questions or comments concerning the mixing height program should be directed
to Dennis Atkinson via email at    atkinson.dennis@epa.gov    .
