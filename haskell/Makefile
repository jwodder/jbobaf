HC = ghc
HC_OPTS = -O
.SUFFIXES : .o .hs .hi .hc .hi-boot .o-boot .hs-boot

OBJS = Jbobaf/Internals.hi Jbobaf/Vlatai.hi Jbobaf/Tamcux.hi Jbobaf/Valsi.hi \
 Jbobaf/Lerfendi.hi Jbobaf/Selmaho.hi

all : $(OBJS)

.o.hi : ;
.hs.o :
	$(HC) -c $< $(HC_OPTS)

.o-boot.hi-boot : ;
.hs-boot.o-boot :
	$(HC) -c $< $(HC_OPTS)

Jbobaf/Vlatai.o Jbobaf/Valsi.o Jbobaf/Lerfendi.o : Jbobaf/Internals.hi
Jbobaf/Vlatai.o Jbobaf/Valsi.o Jbobaf/Lerfendo.o : Jbobaf/Tamcux.hi

Jbobaf/Internals.o : Jbobaf/Vlatai.hi-boot
Jbobaf/Vlatai.o : Jbobaf/Vlatai.hi-boot
Jbobaf/Valsi.o : Jbobaf/Vlatai.hi
Jbobaf/Lerfendi.o : Jbobaf/Valsi.hi Jbobaf/Vlatai.hi
Jbobaf/Selmaho.o : Jbobaf/Valsi.hi

clean :
	( cd Jbobaf; rm -f *.o *.hi *.o-boot *.hi-boot )
