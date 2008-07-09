all:
	cd src; make all

install:
	make all
	cp -f src/extractalphabet bin/.
	cp -f src/fsmcrossprod bin/.
	cp -f src/nevertofsm bin/.
	cp -f src/nevertosymbols bin/.

clean:
	cd src; make clean
	rm -f ./bin/extractalphabet
	rm -f ./bin/fsmcrossprod
	rm -f ./bin/nevertofsm
	rm -f ./bin/nevertosymbols
