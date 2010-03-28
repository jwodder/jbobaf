HC = ghc
HC_OPTS = -O
.SUFFIXES : .o .hs .hi .hc .hi-boot .o-boot .hs-boot

all : Jbobaf/Internals.hi Jbobaf/Vlatai.hi Jbobaf/Tamcux.hi

.o.hi : ;
.hs.o :
	$(HC) -c $< $(HC_OPTS)

.o-boot.hi-boot : ;
.hs-boot.o-boot :
	$(HC) -c $< $(HC_OPTS)

Jbobaf/Internals.o : Jbobaf/Vlatai.hi-boot
Jbobaf/Vlatai.o : Jbobaf/Internals.hi Jbobaf/Vlatai.hi-boot Jbobaf/Tamcux.hi

clean :
	( cd Jbobaf; rm -f *.o *.hi *.o-boot *.hi-boot )
