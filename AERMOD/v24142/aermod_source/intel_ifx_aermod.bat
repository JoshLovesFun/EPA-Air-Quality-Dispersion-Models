@REM                                                                    + + +
@echo off

setlocal

set COMPILE_FLAGS=/O2 /check:format /Qipo /Qprec-div- /QaxSSE4.2 /trace  /Qdiag-disable:8291
set LINK_FLAGS=/O2 /Qipo /check:format /Qprec-div- /QaxSSE4.2 


ifx /compile-only %COMPILE_FLAGS% modules.f  
ifx /compile-only %COMPILE_FLAGS% grsm.f  
ifx /compile-only %COMPILE_FLAGS% aermod.f   
ifx /compile-only %COMPILE_FLAGS% setup.f    
ifx /compile-only %COMPILE_FLAGS% coset.f    
ifx /compile-only %COMPILE_FLAGS% soset.f    
ifx /compile-only %COMPILE_FLAGS% reset.f    
ifx /compile-only %COMPILE_FLAGS% meset.f    
ifx /compile-only %COMPILE_FLAGS% ouset.f    
ifx /compile-only %COMPILE_FLAGS% inpsum.f   
ifx /compile-only %COMPILE_FLAGS% metext.f   
ifx /compile-only %COMPILE_FLAGS% iblval.f   
ifx /compile-only %COMPILE_FLAGS% siggrid.f  
ifx /compile-only %COMPILE_FLAGS% tempgrid.f 
ifx /compile-only %COMPILE_FLAGS% windgrid.f 
ifx /compile-only %COMPILE_FLAGS% calc1.f    
ifx /compile-only %COMPILE_FLAGS% calc2.f    
ifx /compile-only %COMPILE_FLAGS% prise.f    
ifx /compile-only %COMPILE_FLAGS% arise.f    
ifx /compile-only %COMPILE_FLAGS% prime.f    
ifx /compile-only %COMPILE_FLAGS% sigmas.f   
ifx /compile-only %COMPILE_FLAGS% pitarea.f
ifx /compile-only %COMPILE_FLAGS% uninam.f 
ifx /compile-only %COMPILE_FLAGS% output.f   
ifx /compile-only %COMPILE_FLAGS% evset.f    
ifx /compile-only %COMPILE_FLAGS% evcalc.f   
ifx /compile-only %COMPILE_FLAGS% evoutput.f 
ifx /compile-only %COMPILE_FLAGS% rline.f 
ifx /compile-only %COMPILE_FLAGS% bline.f

ifx /exe:aermod.exe %LINK_FLAGS% MODULES.obj GRSM.obj AERMOD.obj SETUP.obj COSET.obj SOSET.obj RESET.obj MESET.obj OUSET.obj INPSUM.obj METEXT.obj IBLVAL.obj SIGGRID.obj TEMPGRID.obj WINDGRID.obj CALC1.obj CALC2.obj PRISE.obj ARISE.obj PRIME.obj SIGMAS.obj PITAREA.obj UNINAM.obj OUTPUT.obj EVSET.obj EVCALC.obj EVOUTPUT.obj RLINE.obj bline.obj

del *.obj
del *.mod
