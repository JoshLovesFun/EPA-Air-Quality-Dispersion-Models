CC      Include file common block ONAMES (OBODM Version 1.3)
CC        Names and labels
      character*1 mtitle,malpha,mpolnt,mspeci,mlablx,mlably,mlngnd,
     *            mblnk1,mdrive,mdirec,msoids,mformt,mnpbuf,mscrch,
     *            mforms,mscrnm,mhrlab
      character*4 mcmlab
      character*6 mcolor
      character*7 munita,munitt,munitg
      character*8 moddat                                                090498
      character*10 munitm
      character*11 munitl
      character*18 munitv
      character*22 mdbnam,mwknam,mgtnam,msvnam,motnam,mognam,mnhnam,
     *             mshnam,mplnam,mfdnam,mtrnam,mapnam,mbcnam,mernam,
     *             msgnam,mxynam
      character*26 modelt
      character*60 mxform,myform,mzform
      common /ONAMES/ mdbnam,mwknam,mgtnam,msvnam,motnam,mognam,mnhnam,
     *                mshnam,mplnam,mfdnam,mtrnam,mapnam,mbcnam,mernam,
     *                msgnam,mxynam,mtitle(50),malpha(52),mnpbuf(80),
     *                mpolnt(36),mspeci(36),modelt(7),munitm(8),
     *                munitl(9),munita(3),munitv(5),munitt(3),munitg(2),
     *                mdrive,mdirec(36),mformt(151),mforms(151),mblnk1,
     *                mcolor(16),msoids(36,50),mscrch(300),mlablx(60),  072497
     *                mlably(60),mlngnd(360),mscrnm(80,25),mhrlab(56),
     *                mcmlab(8),mxform,myform,mzform,moddat             090498
CC      End common block ONAMES
