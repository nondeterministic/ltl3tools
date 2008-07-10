VERSION=0.0.2

all:
	cd src; make all

dist:
	cd ..; cp -r ltl3tools ltl3tools-$(VERSION);            \
	tar cf ltl3tools-$(VERSION).tar ltl3tools-$(VERSION);   \
	gzip ltl3tools-$(VERSION).tar; rm -rf ltl3tools-$(VERSION)

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
