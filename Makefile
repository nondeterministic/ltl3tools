VERSION=0.0.6

all:
	cd src; make all

dist:
	make clean; make all; make install;                     \
	cd ..; cp -r ltl3tools ltl3tools-$(VERSION);            \
	rm -f ltl3tools-$(VERSION)/third-party/*;               \
	rm -rf ltl3tools-$(VERSION)/*/.svn;                     \
	rm -rf ltl3tools-$(VERSION)/.svn;                       \
	rm -rf ltl3tools-$(VERSION)/auto                        \
	rm -rf ltl3tools-$(VERSION)/src/tmp;                    \
	cd ltl3tools-$(VERSION)/src/;                           \
	make clean; cd ..; cd ..;                               \
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
