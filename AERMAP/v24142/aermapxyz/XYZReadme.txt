XYZ Read me – December 21, 2005

Please read the XYZ User's Guide, XYZuGuide.DOC, first.

A sample XYZ DEM file, XYZ.DEM, is provided along with a sample input file, AERMAPXYZ.INP, for testing with AERMAP.  

To run the sample test case:

1) Find the AERMAP.EXE file and the 14 *.LOS and *.LAS files and copy them
to the subdirectory where the XYZ.DEM and AERMAPXYZ.INP files are located.
2) Rename (or copy) AERMAPXYZ.INP to AERMAP.INP
3) When ready, double click on AERMAP.EXE.  The program should run in less
   then 2 seconds on most computers built within the past several years.

To compare your output with the test case output provided:

1) Bring up a DOS or Command Prompt.  This can be done in Windows XP by clicking on: Start | All Programs | Accessories | Command Prompt
2) Enter the drive letter where the data is stored (e.g. C:)
3) Change directories until you are in the subdirectory where the output is stored (e.g. cd \aermap\testcases\XYZ)
4) Use the DOS ‘FC’ command for file compare (e.g. FC XYZ.REC XYZ.XYZ).  There should be: ‘no differences encountered’.

