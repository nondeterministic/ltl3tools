SOURCES = 	config.ml \
		declarations.ml \
		mutils.ml \
		putils.ml \
		gutils.ml \
		minimise.ml \
		dot.ml    \
		getopt.ml

OBJECTS = 	config.cmo \
		declarations.cmo \
		mutils.cmo \
		putils.cmo \
		gutils.cmo \
		minimise.ml \
		dot.cmo    \
		getopt.cmo

all: nevertofsm nevertosymbols extractalphabet fsmcrossprod

neverp.cmo:
	ocamlyacc neverp.mly
	ocamlc -c neverp.mli
	ocamllex neverl.mll
	ocamlc -c $(SOURCES) neverl.ml neverp.ml

nevertofsm: neverp.cmo
	ocamlc -c $(SOURCES) nevertofsm.ml
	ocamlc -custom -ccopt -static \
	       -o nevertofsm          \
	       $(OBJECTS)             \
	       neverl.cmo neverp.cmo  \
	       nevertofsm.cmo

extractalphabet: neverp.cmo
	ocamlc -c $(SOURCES) extractalphabet.ml
	ocamlc -custom -ccopt -static \
	        -o extractalphabet    \
	        $(OBJECTS)            \
		neverl.cmo neverp.cmo \
		extractalphabet.cmo

nevertosymbols: neverp.cmo
	ocamlc -c $(SOURCES) nevertosymbols.ml
	ocamlc -custom -ccopt -static \
	       -o nevertosymbols      \
	        $(OBJECTS)            \
		neverl.cmo neverp.cmo \
		nevertosymbols.cmo

fsmcrossprod:
	ocamlc -custom -ccopt -static \
	       -I +camlp4 -pp camlp4of.opt \
		$(SOURCES)            \
		fsmcrossprod.ml       \
		-o fsmcrossprod

doc:
	ocamlweb fsmcrossprod.ml > doc.tex
	latex doc; latex doc
	dvipdf doc.dvi

clean:
	rm -rf nevertofsm nevertosymbols fsmcrossprod extractalphabet dot2fsm
	rm -rf dist neverl.ml neverp.ml
	rm -rf a.out *.mli *.cmi *.cmo *.aux *.dvi *.log *.pdf
