This Read me is designed to briefly summarize the purpose of each file in the folders.

Note: Any file that contains "_ORG" is what the expected result should be after running the program.
For example, aermod_ORG.out is the expected output for aermod.out after running through the installation steps.
These can be used for file comparison after finishing the steps to help validate that AERMOD is working properly.
When running through the Installation Guide, the user will expect to produce files named "aermod.out" that should be similar to
the associated _ORG file.


The AERMAP folder contains the following files:

	Before running the executable, the folder will contain the following files:

        File Name                             Description
        ----------------------------------    --------------------------------------
        aermap.exe                            The 64-bit executable program that
                                              runs AERMAP. This will need to be downloaded
					      from the EPA SCRAM website*.
        
        aermap.inp                            The input used for aermap.exe. If
                                              there is no aermap.exe, the user will
                                              have to specify what file to use as 
                                              input on the command line.
        
        
        aermmap_ORG.rec                       The expected output for aermap.rec.

        aermap_ORG.src                        The expected output for aermap.src.

        DOMDETAIL_ORG.OUT                     The expected output for DOMDETAIL_ORG.OUT.

        MAPDETAIL_ORG.OUT                     The expected output for MAPDETAIL_ORG.OUT.

        MAPPARAMS_ORG.OUT                     The expected output for MAPPARAMS_ORG.OUT.
        
        newark-e.dem                          A file that contains mock raw input data.

        newark-w.dem                          A file that contains more mock raw input data.
    
    After running AERMAP, the folder will contain the following ADDITIONAL files:
    
        File Name                             Description
        ----------------------------------    --------------------------------------
        aermap.out                            A file that contains output after running
                                              AERMAP. This will contain a copy of the
                                              input file, then any warnings associated
                                              with the run, and finally a summary
                                              of the output.
                                             
        aermap.rec                            A file that will contain all the data related
                                               related to receptors. This will later
                                              be used in aermod.inp as processing data.
        
        aermap.src                            A file that contains all the data related
                                              to sources. This will later be used in
                                              aermod.inp as processing data.
        
        DOMDETAIL.OUT                         A file that contains information regarding
                                              the user-specified modeling domain.
                                              
        MAPDETAIL.OUT                         A file that contains additional details
                                              on errors or warnings. In this sample run,
                                              this file will refer to any errors associated
                                              with newark-e.dem and newark-w.dem.
        
        MAPPARAMS.OUT                         A file that contains three adjacency
                                              tests with difference distance tolerances
                                              intended to identify possible stray files 
                                              which have non-standard coordinates.
        
The AERMET folder contains the following files:

    Before running the executable, the folder will contain the following files:
    
        File Name                             Description
        ----------------------------------    --------------------------------------
        14735-92.fsl                          A file used as input data for AERMET
                                              focused on surface level data.
                                              
        14737.DAT                             A file that is the first text-formatted
                                              input data used for AERMET.
                                               
        aermet.exe                            The 64-bit executable program that
                                              runs AERMET. This will need to be downloaded
					      from the EPA SCRAM website**. 
                                              
        aermet_ORG.pfl                        The expected output for aermet.pfl
                                              
        aermet_ORG.sfc                        The expected output for aermet.sfc
                                              
        aermet1.inp                           The first input file that is used
                                              in Stage 1 of AERMET.
                                              
        aermet2.inp                           The second input file that is used
                                              in Stage 2 of AERMET.
                                              
        MCSOPFL.DAT                           A file that is the second text-formatted
                                              input data used for AERMET.

    After running the executable, the folder will contain the following ADDITIONAL files:
                                              
        File Name                             Description
        ----------------------------------    --------------------------------------
        14737.iqa                              An input QA file.
                                              
        14737.oqa                              An output QA file.
                                              
        aermet.pfl                            A file that is one of the output files
                                              from running AERMET. This file contains
                                              data related to the upper level wind
                                              profile.
                                              
        aermet.sfc                            A second output file from running AERMET.
                                              This file contains data related to the
                                              surface report.
                                              
        aermet_st1.msg                        A message containing all of the data 
                                              that is processed in Stage 1 of AERMET.
                                              
        aermet_st1.rpt                        A report of AERMET's results after running
                                              stage 1 of AERMET.
                                              
        aermet_st2.msg                        A message containing all of the data
                                              that is processed in Stage 2 of AERMET.
            
        aermet_st2.rpt                        A report of AERMET's results after running
                                              stage 2 of AERMET.
                                              
        alb92-93.iqa                          An input QA file related to the Albany
                                              data related to the Albany data from
                                              1992 - 1993.
                                              
        alb92-93.oqa                          An output QA file related to the Albany
                                              data related to the Albany data from
                                              1992 - 1993.
                                              
        mcsopfl.oqa                           An output QA file produced from the
                                              file MCSOPFL.DAT.
                                              
The AERMOD folder contains the following files:

    Before running the executable, the folder will contain the following files:
    
        File Name                             Description
        ----------------------------------    --------------------------------------
        aermod.emi                            An emissions report used as input data.
                                              
        aermod.exe                            The 64-bit executable program that
                                              runs AERMOD. This will need to be downloaded
					      from the EPA SCRAM website***. 
                                              
        aermod.inp                            A large input file that contains all of
                                              the data that has been produced by AERMET
                                              and AERMAP along with the emissions report
                                              and other corresponding data files.
        
        aermod_ORG.out                        The expected output file for aermod.out.
                                              
        aermod_ORG.sum                        The expected output file for aermod.sum.
                                              
        ERRORS_ORG.OUT                        The expected output file for ERRORS.OUT.
                                              
    After running the executable, the folder will contain the following ADDITIONAL files:

        File Name                             Description
        ----------------------------------    --------------------------------------
    aermod.out                                The output file of AERMOD. It first
                                              contains a copy of the input file, then
                                              any warnings associated with the run,
                                              and finally a summary of the run itself
                                              (a copy of aermod.sum).
                                              
    aermod.sum                                A file that is the summary of the AERMOD
                                              report given by running aermod.exe.
                                              
    ERRORS.OUT                                A collection of all the errors and warnings
                                              found when running AERMOD.

Where to download the executable files
                                              
*AERMAP: https://www.epa.gov/scram/air-quality-dispersion-modeling-related-model-support-programs#aermap
**AERMET: https://www.epa.gov/scram/meteorological-processors-and-accessory-programs#aermet
***AERMOD: https://www.epa.gov/scram/air-quality-dispersion-modeling-preferred-and-recommended-models


References

EPA, 2018: User's Guide for the AERMOD Terrain Preprocessor (AERMAP). EPA- 454/B-18-
    004. U.S. Environmental Protection Agency, Research Triangle Park, North Carolina 
    27711. 

EPA, 2022c: User's Guide for the AERMOD Meteorological Preprocessor (AERMET). EPA-
    454/B-22-006. U.S. Environmental Protection Agency, Research Triangle Park, North 
    Carolina 27711.