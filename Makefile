run: 10
	./10

10: 10.hs
	stack ghc 10.hs

clean:
	rm 10 10.hi 10.o
