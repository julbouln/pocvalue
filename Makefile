OCAMLMAKEFILE = OCamlMakefile

PACKS=str unix extlib lua-ml xml-light
LIBINSTALL_FILES=*.cmi *.cmo *.cmx *.a pocvalue.cma pocvalue.cmxa

SOURCES_C=xinclude_stubs.c 
SOURCES_ML= value_xinclude.ml value_common.ml value_lua.ml value_xml.ml value_val.ml value_xmlparser.ml
SOURCES = linkedHashtbl.ml $(SOURCES_C) $(SOURCES_ML)
RESULT  = pocvalue

OCAMLDOC=ocamlfind ocamldoc -package "$(PACKS)"
DOC_FILES=$(SOURCES_ML)

CLIBS=xml2
CFLAGS=`xml2-config --cflags`

all : ncl bcl
include $(OCAMLMAKEFILE)
