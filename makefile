HC = ghc
SRC = src
OBJ = obj
OBJS = Xml.o XmlParse.o Sasl.o Connection.o Main.o

vpath %.o $(OBJ)
vpath %.hi $(OBJ)
vpath %.hs $(SRC)

jhabber:
	ghc --make -XDeriveDataTypeable -isrc -hidir obj -odir obj -o bin/jhabber src/*.hs 

#: $(OBJS)
#	$(HC) -o $@ $< 

run: jhabber
	./bin/jhabber

debug: all
	ghci src/Main
	
%.o : %.hs
	$(HC) -c -odir $(OBJ) $<

clean:
	rm obj/*.o
	rm bin/*
	
depends: 
	ghc -M $(SRC)/*.hs
