In the zip file, aerscreen_test_cases.zip, are several example cases of AERSCREEN
inputs and outputs for all of the source types that can be processed by AERSCREEN:
point, capped stack, horizontal stack, volume, rectangular area, and circular area 
sources

Each source type is self-contained in its own folder with the necessary input files and
output files.  Each folder also contains a README file describing the files in the 
folders.  To run the test cases, the AERSCREEN, MAKEMET, AERMOD, AERMAP, and BPIPPRM
executables must all be in the same folder as the input files and the user must rename 
the appropriate ".inp" file to AERSCREEN.INP to use the inputs in the files.  
For example, to run one of the test cases in the point folder, the AERSCREEN, 
MAKEMET, AERMOD, AERMAP, and BPIPPRM executables must also be in the point folder. 
To run one of the cases, such as flat terrain and downwash the input file, 
aerscreen_flat_dw.inp must be rename aerscreen.inp or AERSCREEN.INP. If AERSCREEN does 
not detect the file aerscreen.inp, the program will automatically begin prompting the 
user for data.

To execute AERSCREEN for one of the test cases listed below, copy the 
AERSCREEN_BETA1.EXE, MAKEMET.EXE, AERMOD.EXE, AERMAP.EXE, and BPPIPPRM.EXE executables
to the appropriate folder and double click on the AERSCREEN.EXE icon. Follow the 
prompts displayed on the screen.  If executing AERSCREEN from a DOS command window, 
just type AERSCREEN.EXE at the prompt to begin
program execution.

Test case folders
-----------------
area:         rectangular area source inputs and outputs
circle:       circular area source inputs and outputs
flare:        flare source inputs and outputs
point:        point source inputs and outputs
point_cap:    capped vertical stack source inputs and outputs
point_horiz:  horizontal stack source inputs and outputs
point_no2:    point source inputs and outputs for using
              NOx to NO2 conversion methods OLM or
              PVMRM
volume:       volume source inputs and outputs


