WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),pa_ppx_fmtformat,ounit2,pa_ppx.utils,pa_ppx_fmtformat.link,pa_ppx.testutils

ML= \
	pa_ppx_fmtformat_test.ml

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=pa_ppx_fmtformat_test.exe

all: $(TESTS)

## NOTE WELL:  toplevel_test.asciidoc.TEST is not being tested (b/c MDX is backlevel)
test:: all
	mkdir -p _build && ./pa_ppx_fmtformat_test.exe

pa_ppx_fmtformat_test.exe: pa_ppx_fmtformat_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

clean::
	rm -rf *.cm* _build *.exe

