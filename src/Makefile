SOURCES = 	config.ml \
		declarations.ml \
		mutils.ml \
		putils.ml \
		gutils.ml \
		minimise.ml \
		dot.ml    \
		getopt.ml

OBJECTS = 	config.cmx \
		declarations.cmx \
		mutils.cmx \
		putils.cmx \
		gutils.cmx \
		minimise.cmx \
		dot.cmx    \
		getopt.cmx

all: nevertofsm nevertosymbols extractalphabet fsmcrossprod

neverp.cmx:
	ocamlyacc neverp.mly
	ocamlopt -c neverp.mli
	ocamllex neverl.mll
	ocamlopt -c $(SOURCES) neverl.ml neverp.ml

nevertofsm: neverp.cmx
	ocamlopt -c $(SOURCES) nevertofsm.ml
	ocamlopt -o nevertofsm          \
	       $(OBJECTS)             \
	       neverl.cmx neverp.cmx  \
	       nevertofsm.cmx

extractalphabet: neverp.cmx
	ocamlopt -c $(SOURCES) extractalphabet.ml
	ocamlopt -o extractalphabet    \
	        $(OBJECTS)            \
		neverl.cmx neverp.cmx \
		extractalphabet.cmx

nevertosymbols: neverp.cmx
	ocamlopt -c $(SOURCES) nevertosymbols.ml
	ocamlopt -o nevertosymbols      \
	        $(OBJECTS)            \
		neverl.cmx neverp.cmx \
		nevertosymbols.cmx

fsmcrossprod:
	ocamlopt -I +camlp4 -pp camlp4of.opt \
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
	rm -rf a.out *.o *.mli *.cmi *.cmx *.cmo *.aux *.dvi *.log *.pdf
