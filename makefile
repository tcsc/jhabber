all: 
	ghc --make -hidir obj -odir obj -o bin/jhabber *.hs

run: all
	./bin/jhabber

clean:
	rm obj/*.o
	rm bin/*
