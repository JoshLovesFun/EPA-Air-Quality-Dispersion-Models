
                             WRPLOT

Program Description

    Enclosed in this package are the IBM PC compatible files for
the WRPLOT program, an interactive program that generates wind rose
statistics and plots for selected meteorological stations for user-
specified date and time ranges.  A wind rose depicts the frequency
of occurrence of winds in each of 16 direction sectors (north,
north-northeast, northeast, etc.) and six wind speed classes for a
given location and time period.  Also included is the percent
frequency of calm winds.  This program uses the surface
meteorological data files for National Weather Service (NWS)
stations available on the SCRAM BBS of the OAQPS Technology
Transfer Network.  The data files can be used in either their
compressed or uncompressed form.  

    Wind roses can sometimes be used to depict graphically the
dominant transport direction of the winds for an area.  Due to the
influences of local terrain, possible coastal effects, the exposure
of the instruments, and the temporal variability of the wind, the
wind rose statistics may not always be representative of true
transport for an area.  Other meteorological conditions may also be
important for determining the formation and transport of certain
atmospheric contaminants, particularly for reactive pollutants. 
The results of this program should therefore be used with caution.

    The WRPLOT program should not be confused with the WINDROSE
utility program also available on the SCRAM BBS.  WRPLOT was
developed primarily to produce graphic plots of wind rose
statistics, whereas the WINDROSE utility determines tables of wind
frequency statistics but has no graphics capability.  The WRPLOT
program produces wind frequency statistics for 16 direction sectors
(every 22.5 degrees), whereas the WINDROSE utility produces
statistics for 36 directions (every 10 degrees).  Since the NWS
wind directions are reported to the nearest 10 degrees, the
WINDROSE program uses these values directly.  To avoid a bias in
the wind rose plots toward the cardinal compass points (N, S, E,
and W), WRPLOT randomizes the wind directions for each 10 degree
sector to one degree increments based on the same random number set
used by the RAMMET meteorological preprocessor.  A bias would occur
for the unrandomized wind directions because three 10-degree
sectors would contribute to the N, S, E, and W sector statistics
(e.g., 350, 360 and 10 degrees for the north sector), while only
two 10-degree sectors would contribute to the other 22.5 degree
sectors.  Thus, the plots produced by WRPLOT represent the same
statistics that would be generated from the randomized flow vectors
in a RAMMET preprocessed meteorological data file.


Program Installation

The WRPLOT package consists of the following file:

INSTALL.EXE - Compressed file containing all files needed to run
the program.

In order to minimize the number of disks needed for distribution,
the 15 files needed to run the program and two sample files were
compressed and combined into a self extracting archive file.  The
files must be expanded prior to being used.  The following example
will illustrate the data decompression process.  
                                                    
1.   Assume that (a) you want to put the program in a subdirectory
     called WRPLOT, and (b) there is a 5¬ 1.2 megabyte disk drive
     designated as drive A.
 
2.   Create a directory for this program (e.g. WRPLOT) and make it
     the current directory by typing the following:

                              CD\           
                              MD WRPLOT
                              CD WRPLOT

3.   Place the 5¬ inch distribution disk in the A drive.

4.   Type the following at the DOS prompt (in your WRPLOT
     subdirectory, not in the A drive):

                              A:INSTALL

This file expands/decompresses as it copies the files onto the hard
drive.  Two years of meteorological data for Columbus, OH are
included with the program for testing purposes.


Program Operation

    The program operates through a series of interactive menus that
can be accessed  through either the keyboard or a mouse. To operate
this program:

1.   First select the data to be plotted.  One or more years of
     data may be used in a single plot.  Data may be selected in
     either of three ways:

     (a)  The user can choose data by selecting a file or group of
          files from a list of files in the current directory. 
          These files contain meteorological data in the ASCII
          format and must have the .DAT extension to appear on the
          file list. The data files obtained from SCRAM must be
          decompressed before accessing them in this manner.
          WRPLOT expects these files to be in the following format:

          Column        Parameter Name             Units

           1-5       Station Identification Number
           6-7       Year
           8-9       Month
          10-11      Day
          12-13      Hour
          17-18      Wind Direction           Tens of Degrees
          19-21      Wind Speed               Knots

     (b)  Data can also be selected from a list of meteorological
          stations for which data are available in the current
          directory in the archived format (e.g., filename.ARC,
          where filename is the meteorological station id number
          plus "-" plus the 2-digit year) or the zipped format
          (e.g., filename.ZIP) as available on SCRAM.  First, a
          list of available states is presented.  After the state
          is chosen, a list of the station names for that state is
          presented.  Finally, a list of years for which data are
          available is presented.  The user can then select the
          year(s) of interest.  If data are selected in this
          manner, then the data are first decompressed.

     (c)  The user can enter their own decompression program if the
          user has files that are compressed in a format other than
          the zip or archive format.  First, a window appears
          requesting the name of the decompression program.  This
          name must include the extension.  The user must also
          enter the extension of the compressed files.  The
          filename of the compressed files must follow the same
          convention as listed in part (b) (i.e., filename is the
          meteorological station id number plus "-" plus the 2-
          digit year.)  After the decompression program and the
          compressed file extension has been entered in the data
          entry window, the user will exit from this window and
          proceed with the series of menus described in part (b).

2.   After selecting the data, date and time ranges for plotting
     may be chosen.  The user can choose the beginning date (i.e.,
     month and day) and the ending date of interest.  Also, a range
     of hours can be selected (e.g., 7AM - 6PM.)  These range
     selections are made by choosing items from a series of lists. 
     If more than one year of data is selected, then the same time
     and date range is used for each year.

3.   Finally, an output device for the plot must be selected.  The
     user can select from a list including the screen and various
     printers.  In addition, the plot can be saved to a file in
     either the PCX or metafile (CGM) formats.  The user can also
     select to save the frequency distribution in tabular form.

    The WRPLOT program makes every effort to utilize the user's
printer to make the highest quality output possible.  It is
therefore important that the printer type and configuration be
known.  Sometimes the printer will require reconfiguration via the
control panel as well.  The types of output and their hardware
requirements for the HP LaserJet are listed below:

HP LaserJet Draft Quality - This common denominator will work on
most printers compatible with Hewlett Packard's PCL standard.

HP LaserJet High Quality - This driver may require expanded printer
memory on some machines.  

HP LaserJet Series III - This Printer driver will only work on the
Hewlett Packard Series III and compatible.  If a "PRINTER OVERFLOW"
error occurs consult the Printer User's Manual for information on
the "Page Protection Mode."  This is only available on printers
with 2 megabytes or greater memory.

     For questions or comments on the WRPLOT program, contact
Dennis Atkinson, EPA-OAQPS, (919) 541-0518.
