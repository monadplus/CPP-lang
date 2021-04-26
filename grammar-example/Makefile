# Install BNFC and alex
# $ nix-env -iA nixpkgs.haskellPackages.BNFC
# $ nix-env -iA nixpkgs.haskellPackages.alex
# $ nix-env -iA nixpkgs.haskellPackages.happy

.PHONY: clean

COMPILER=ghc

all: run clean

run: progs-test-lab1 CPP.cf
	./progs-test-lab1 CPP.cf

progs-test-lab1:
	$(COMPILER) progs-test-lab1.hs

clean:
	rm -rf lab1-test-dir progs-test-lab1 progs-test-lab1.hi progs-test-lab1.o
