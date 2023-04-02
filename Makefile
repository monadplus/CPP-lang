.PHONY : all clean distclean format

HSFILES =  $(shell find ./src ./test -type f -name \*.hs -not -path "./dist-newstyle/*")

all : LexCPP.x ParCPP.y

ErrM.hs AbsCPP.hs LexCPP.x PrintCPP.hs ParCPP.y TestCPP.hs : CPP.cf
	bnfc --haskell CPP.cf

%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

format: $(HSFILES)
	@ormolu --ghc-opt -XBangPatterns --mode inplace $^ && echo "Code formatted succesfully!"

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

# EOF
