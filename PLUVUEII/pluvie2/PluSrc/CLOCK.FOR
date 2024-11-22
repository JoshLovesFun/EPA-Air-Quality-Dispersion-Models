CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCLK00010
      FUNCTION CLOK(T1,IINC)                                                    
C                                                                       CLK00030
C  CLOK.FOR                                                             CLK00040
C                                                                       CLK00050
C  ADD A TIME IN MINUTES TO A 2400 HOUR TIME AND RETURN A 2400          CLK00060
C  HOUR TIME                                                            CLK00070
C                                                                       CLK00080
C                                                                       CLK00090
C  PLUVUE II                                                            CLK00100
C  Version 92243   Level 1.0                                            CLK00110
C                                                                       CLK00120
C  U.S. Environmental Protection Agency                                 CLK00130
C  Office of Air Quality Planning and Standards                         CLK00140
C  Research Triangle Park, North Carolina                               CLK00150
C                                                                       CLK00160
C  Modifications:                                                       CLK00170
C                                                                       CLK00180
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCLK00190
           SAVE                                                         CLK00200
C                                                                       CLK00210
      T2 = IINC                                                         CLK00220
      I100 = INT(T1/100.)                                               CLK00230
      T3 = T1-100.0*FLOAT(I100) + T2                                    CLK00240
      I100 = I100 + INT(T3/60.)                                         CLK00250
      CLCK=FLOAT(I100)*100.0 + T3 -60.0 * FLOAT(INT(T3/60.))            CLK00260
      T4=CLCK-FLOAT(IFIX(CLCK) /100)*100.                               CLK00270
      CLOK=CLCK                                                         CLK00280
      IF(T4.GT.59.) CLOK=CLCK-40.                                       CLK00290
      RETURN                                                            CLK00300
      END                                                               CLK00310
