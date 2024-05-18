GHC = ghc
GHC_OPTS = -package array -package mtl -package containers

.PHONY : all clean

all: interpreter

interpreter: GeneratedParser/LexEspresso.hs GeneratedParser/AbsEspresso.hs GeneratedParser/ParEspresso.hs TypeChecker.hs Main.hs
	${GHC} ${GHC_OPTS} -o $@ $^

clean:
	-rm -f *.hi *.o GeneratedParser/*.hi GeneratedParser/*.o interpreter