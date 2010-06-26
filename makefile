all: 
	ghc --make -hidir obj -odir obj -o bin/jhabber *.hs

run: all
	./bin/jhabber

debug: all
	ghci Main

clean:
	rm obj/*.o
	rm bin/*
