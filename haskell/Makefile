HC = ghc
HC_OPTS = -O
.SUFFIXES : .o .hs .hi .hc .hi-boot .o-boot .hs-boot

OBJS = Jbobaf/Canti.hi Jbobaf/Jitro.hi Jbobaf/Lerfendi.hi Jbobaf/Selmaho.hi \
 Jbobaf/Valsi.hi Jbobaf/Vlatai.hi

all : $(OBJS) Jbobaf.hi

.o.hi : ;
.hs.o :
	$(HC) -c $< $(HC_OPTS)

.o-boot.hi-boot : ;
.hs-boot.o-boot :
	$(HC) -c $< $(HC_OPTS)

Jbobaf/Vlatai.o Jbobaf/Valsi.o Jbobaf/Lerfendi.o : Jbobaf/Canti.hi
Jbobaf/Vlatai.o Jbobaf/Valsi.o Jbobaf/Lerfendi.o : Jbobaf/Jitro.hi

Jbobaf/Canti.o Jbobaf/Vlatai.o : Jbobaf/Vlatai.hi-boot
Jbobaf/Valsi.o : Jbobaf/Vlatai.hi
Jbobaf/Lerfendi.o : Jbobaf/Valsi.hi Jbobaf/Vlatai.hi
Jbobaf/Selmaho.o : Jbobaf/Valsi.hi

Jbobaf.o : $(OBJS)

doc : Jbobaf.hs Jbobaf/*.hs
	haddock -ho doc -t Jbobaf Jbobaf.hs Jbobaf/*.hs

clean :
	( cd Jbobaf; rm -f *.o *.hi *.o-boot *.hi-boot )
	rm -f *.o *.hi
	rm -rf doc
