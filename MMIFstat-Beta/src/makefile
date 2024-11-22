
#Use the following flags if you use PGF 64-bit compiler on 64-bit OS:
FC = pgf90
FFLAGS = -O2 -Mconcur=nonuma -Mnoframe -Bstatic_pgi
FFLAGS = -C -g -Mconcur=nonuma -Mnoframe -Bstatic_pgi

PROGRAM = MMIFstat

MODULES = met_fields.f stat_fields.f site_fields.f

SOURCES = averag.f \
averag_0.f \
biaserr.f \
biaserrw.f \
caldate.f \
getcalmet.f \
getioa.f \
getrmse.f \
grid_calmet.f \
interp.f \
isearch15.f \
istrln.f \
juldate.f \
lcpgeo.f \
polgeo.f \
MMIFstat.f \
read_statlib.f \
read_tdl.f \
readasc.f \
readcalhdr.f \
readcalmet.f \
regress.f \
stats.f \
uv2spdr.f \
wrtfils.f

MODOBJ1 = $(MODULES:.f=.o) # note the trick to turn *.[fF]90 to *.o
MODOBJS = $(MODOBJ1:.F=.o)
OBJECT1 = $(SOURCES:.f=.o)
OBJECTS = $(OBJECT1:.F=.o)

$(PROGRAM): $(MODOBJS) $(OBJECTS)
	$(FC) $(FFLAGS) $(MODOBJS) $(OBJECTS) $(LIBS) $(LFLAGS) -o $@

%.o : %.f
	$(FC) $(FFLAGS) $(INCL) -c $< -o $@

%.o : %.F9
	$(FC) $(FFLAGS) $(INCL) -c $< -o $@

clean:
	rm *.mod $(MODOBJS) $(OBJECTS) $(PROGRAM)
