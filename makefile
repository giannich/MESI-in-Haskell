HC = stack ghc

ALL: main

main: Main.hs ProtocolLogic.hs DataTypes.hs
	$(HC) Main.hs

clean:
	rm *.o *.hi Main
