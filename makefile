all: 
	ghc --make -hidir obj -odir obj -o bin/jhabber src/*.hs

run: all
	./bin/jhabber

debug: all
	ghci src/Main

clean:
	rm obj/*.o
	rm bin/*
