GHC = ghc
GHC_OPTS = -package array

.PHONY : all clean

all: interpreter

interpreter: GeneratedParser/ParEspresso.hs Main.hs
	${GHC} ${GHC_OPTS} -o $@ $^

clean:
	-rm -f *.hi *.o GeneratedParser/*.hi GeneratedParser/*.o interpreter