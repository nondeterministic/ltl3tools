SOURCES = 	config.ml \
		declarations.ml \
		mutils.ml \
		putils.ml \
		gutils.ml \
		alphabet.ml \
		minimise.ml \
		dot.ml    \
		getopt.ml

OBJECTS = 	str.cmxa \
		config.cmx \
		declarations.cmx \
		mutils.cmx \
		putils.cmx \
		gutils.cmx \
		alphabet.cmx \
		minimise.cmx \
		dot.cmx    \
		getopt.cmx

all: nevertofsm formulatosymbols extractalphabet fsmcrossprod

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
	ocamlopt -c $(SOURCES) str.cmxa extractalphabet.ml
	ocamlopt -o extractalphabet    \
		$(OBJECTS)             \
		neverl.cmx neverp.cmx \
		extractalphabet.cmx

formulatosymbols: neverp.cmx
	ocamlopt -c $(SOURCES) formulatosymbols.ml
	ocamlopt -o formulatosymbols      \
	        $(OBJECTS)           \
		formulatosymbols.cmx

fsmcrossprod:
	ocamlopt -I +camlp4 -pp camlp4of.opt \
		str.cmxa $(SOURCES) \
		fsmcrossprod.ml       \
		-o fsmcrossprod

doc:
	ocamlweb fsmcrossprod.ml > doc.tex
	latex doc; latex doc
	dvipdf doc.dvi

clean:
	rm -rf nevertofsm formulatosymbols fsmcrossprod extractalphabet dot2fsm
	rm -rf dist neverl.ml neverp.ml
	rm -rf a.out *.o *.mli *.cmi *.cmx *.cmo *.aux *.dvi *.log *.pdf
