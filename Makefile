OCAMLMAKEFILE = OCamlMakefile

PACKS=str unix extlib lua-ml xml-light unix
LIBINSTALL_FILES=*.cmi *.cmo *.cmx *.a pocvalue.cma pocvalue.cmxa

SOURCES = xinclude_stubs.c value_xinclude.ml value_common.ml value_lua.ml value_xml.ml value_val.ml
RESULT  = pocvalue


CLIBS=xml2
CFLAGS=`xml2-config --cflags`

all : ncl bcl
include $(OCAMLMAKEFILE)
