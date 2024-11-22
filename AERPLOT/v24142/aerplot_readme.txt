AERPLOT.exe
------------
(version 24142)

AERPLOT uses a .PLT (plot) file from AERMOD to create a .KMZ (Google
Earth) file with receptors plotted and colored according to their
concentration.  Locations and shapes of sources can optionally be
plotted.  Contours and/or gradient lines can also be added to the plot
if desired.  Program options are controlled with the inputfile
AERPLOT.inp .

Installation: 
-------------

This program is designed to run on Microsoft Windows. It is provided 
as a stand-alone executable and no installation is required. 

Operation:
----------

1. The executable looks for the AERPLOT.inp file in the local
   directory and that file should be placed in the folder with the
   program executable. The .PLT file may be local or in another
   directory, as specified in the AERPLOT.inp file.

2. Alter the input file, AERPLOT.inp, as necessary. Comments in the
   provided sample .inp file explain the program options and are given
   below. The .inp file uses semicolons (;) to indicate comment
   lines. Blank lines are allowed. Comment characters should be in the
   first column only.

3. Invoke the AERPLOT.exe program, either from the command line, or
   by clicking in Windows Explorer.

3. The program will output a file, with a file name set by the parameter
   OutputFileNameBase, and a file extension of .KMZ .

   The .KMZ file is self-contained. It can be moved to another folder,
   or mailed to another computer, and still be viewed in an earth 
   browser (e.g., Google Earth). 


Resulting Display:
------------------

When opened in Google Earth, each receptor will be be displayed as 
a colored circle. At the time of this release, Google Earth will 
display the circles centered on the given coordinates.

Each receptor is colored according to its concentration.  Red 
indicates the highest concentration, with dark blue the lowest 
concentration. (An alternate set of icons that run from dark red 
to dark green is available, selectable from within the AERPLOT.inp 
file.)

If contours are selected, the contour lines will also be displayed
and the color scale will match the color scale of the receptors.

A legend will be displayed indicating the color scale corresponding 
to the concentration at each receptor.

If gradients are selected, a separate legend will be displayed
indicating the color scale corresponding to the gradient lines.

Clicking on a receptor will open an "information balloon" that has 
all of the associated data from the .PLT file.

Each receptor is named according to the concentration, the X and 
the Y coordinates, plus a leading sequence number. For example, 

           "8_60.54069_87.50000_151.55445", 

Sources are displayed if requested. The location, type, etc., are
determined from the plot's original aermod.inp file.

AERPLOT.inp parameters:
------------------------

For information about the parameters, see the supplied sample
AERPLOT.inp file.


Additional notes:
----------------

The current version of AERPLOT assumes that the .PLT file includes all 
header information that is included by default in AERMOD. Without the 
header, AERPLOT will not be able to read in the .PLT file.