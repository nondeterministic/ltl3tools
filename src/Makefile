SOURCES = 	config.ml \
		declarations.ml \
		mutils.ml \
		putils.ml \
		gutils.ml \
		getopt.ml

OBJECTS = 	config.cmo \
		declarations.cmo \
		mutils.cmo \
		putils.cmo \
		gutils.cmo \
		getopt.cmo

all:
	make nevertofsm
	make nevertosymbols
	make extractalphabet
	make fsmcrossprod

neverp.cmo:
	ocamlyacc neverp.mly
	ocamlc -c neverp.mli
	ocamllex neverl.mll
	ocamlc -c $(SOURCES) neverl.ml neverp.ml

nevertofsm: neverp.cmo
	ocamlc -c $(SOURCES) nevertofsm.ml
	ocamlc -o nevertofsm          \
	       $(OBJECTS)             \
	       neverl.cmo neverp.cmo  \
	       nevertofsm.cmo

extractalphabet: neverp.cmo
	ocamlc -c $(SOURCES) extractalphabet.ml
	ocamlc -o extractalphabet     \
	        $(OBJECTS)            \
		neverl.cmo neverp.cmo \
		extractalphabet.cmo

nevertosymbols: neverp.cmo
	ocamlc -c $(SOURCES) nevertosymbols.ml
	ocamlc -o nevertosymbols      \
	        $(OBJECTS)            \
		neverl.cmo neverp.cmo \
		nevertosymbols.cmo

fsmcrossprod:
	ocamlc -I +camlp4 -pp camlp4of.opt \
		$(SOURCES)            \
		fsmcrossprod.ml       \
		-o fsmcrossprod

clean:
	rm -rf nevertofsm nevertosymbols fsmcrossprod extractalphabet
	rm -rf dist neverl.ml neverp.ml
	rm -rf *.mli *.cmi *.cmo *.aux *.dvi *.log *.pdf
