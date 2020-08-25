.PHONY : all clean distclean

# Default goal.
all : LexCPP.x ParCPP.y

# Rules for building the parser.
ErrM.hs AbsCPP.hs LexCPP.x PrintCPP.hs ParCPP.y TestCPP.hs : CPP.cf
	bnfc --haskell CPP.cf

%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

# EOF
