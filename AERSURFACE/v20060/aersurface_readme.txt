                             README 
                             
                     AERSURFACE Version 20060
                         February 29, 2020

This file describes the components included in this release of the AERSURFACE tool,
version 20060.  The AERSURFACE tool has been developed to aid AERMOD users in
obtaining reproducible surface characteristic values, including albedo, Bowen ratio,
and surface roughness length, for input to the AERMET meteorological processor.

The following lists the files associated with this release of AERSURFACE:

FILENAME                            DESCRIPTION
-------------------	               -----------------------------------------------------
                                    
aersurface_readme.txt               This README file
                                    
aersurface_exe-64.zip               AERSURFACE executable (64-bit for Windows), 
                                    this README file, and .las and .los files
                                    required for NAD27 to NAD83 conversions
                                    (CONUS and Alaska only)
                                    
aersurface_exe-32.zip               AERSURFACE executable (32-bit for Windows), 
                                    this README file, and .las and .los files
                                    required for NAD27 to NAD83 conversions
                                    (CONUS and Alaska only)
                                    
aersurface_source.zip               AERSURFACE source code, with batch files to 
                                    recompile using the gfortran, Absoft, and Intel 
                                    Fortran compilers. The executables provided 
                                    were compiled with the gfortran for Windows.
                                    
aersurface_ug_v20060.pdf            AERSURFACE User's Guide (EPA-454-B-20-008),
                                    Revised February, 2020
                                    
AERSURFACE UG Appendix_G_Intercomparison 2001 NLCD.zip                 
                                    Input/output files for AERSURFACE intercomparison
                                    provided in the AERSURFACE User's Guide as 
                                    "Section 11 Appendix G: Inter-comparison of 
                                    AERSURFACE," including inter-comparison of AERSURFACE
                                    and AERMOD results based on 2001 NLCD (2011 edition).   
                                    
AERSURFACE UG Appendix_G_Intercomparison 2016 NLCD.zip                 
                                    Input/output files for AERSURFACE intercomparison
                                    provided in the AERSURFACE User's Guide as 
                                    "Section 11 Appendix G: Inter-comparison of 
                                    AERSURFACE," including inter-comparison of AERSURFACE
                                    and AERSURFACE results using 2001 NLCD (2011 edition)
                                    compared to the 2016 NLCD. 
                                    
aersurface_testcase.zip             AERSURFACE test case for RDU (2016 NLCD)

impervious-canopy_simulator.xlsx    Simulate implementation of impervious and canopy
                                    data in assignment of surface roughness length
                                    to 2001, 2006, 2011, and 2016 Developed categories