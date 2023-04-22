.PHONY : clean format

HSFILES =  $(shell find ./src ./test -type f -name \*.hs -not -path "./dist-newstyle/*")

format: $(HSFILES)
	@ormolu --ghc-opt -XBangPatterns --mode inplace $^ && echo "Code formatted succesfully!"

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

# EOF
