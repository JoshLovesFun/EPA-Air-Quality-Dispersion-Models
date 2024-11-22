                   
                                
                         OCD Version 5
                                
The Minerals Management Service has released an updated version of the OCD
model, dated 97363, and a new user's guide.  This new version, which is dated
November 7, 1997, incorporates a graphical user interface (GUI), which makes
the model considerably more user-friendly. With the GUI, the user can
prepare, execute, and analyze an OCD application in a graphical or
menu-driven environment.  Extensive error-checking on the input data is
performed by the GUI program.  The model program also provides an on-line
help system, so that the user rarely would have to consult the user's guide
for further instructions.  However, the modeler still has the option of
constructing the input control file and run the model independent of the GUI
program.  The basic dispersion algorithms in the OCD model were not changed.  

The model system contains a program to automatically generate the shoreline
geometry for any shoreline area in the U.S.  The user therefore no longer
has to generate the shoreline geometry manually.  The mapping data do not
contain topography so the user will still have to generate receptor and
terrain heights from USGS topographical maps or digital data files.  The
number of grid blocks used to define the shoreline geometry and model domain
is flexible and the number of grids allowed has been expanded.  The GUI also
allows the construction of a Cartesian receptor network.  The model system
has the capability of importing PCRAMMET output files in either ascii or
binary format.

The new OCD user control file has a flexible format similar to that for the
CALPUFF model.  The user no longer has to enter the input control data in an
exact format or in a certain order, and an unlimited amount of comments can
be added in the file.  A program also is included that converts files that
are in the old OCD format to the new input control format.  Other
enhancements made to the OCD code include the removal of hardwired array
limits and legacy FORTRAN statements, better error-checking, and the
correction of some minor programming errors.



                   INSTALLATION INSTRUCTIONS


This system was designed for installation in Windows or Windows 95/98. After
the file OCD5.ZIP has been unzipped in a new directory, perform the
following:  1. For Windows installation, select File and Run then type "SETUP"
preceeded by the name of the directory in which OCD5.ZIP has been unzipped
then press "enter" or 2. For Windows 95/98 installation select Start and Run
then type "SETUP" preceeded by the name of the directory in which OCD5.ZIP
has been unzipped, then press "enter".


                   NOTES ON FORTRAN COMPILER 

The source codes for the OCD model were modified and re-compiled with a  
FORTRAN 95 compiler so that the program would be compatible with Windows XP.  
