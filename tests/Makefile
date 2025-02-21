WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),pa_ppx_fmtformat,ounit2,pa_ppx.utils,pa_ppx_fmtformat.link,pa_ppx.testutils,pp-binary-ints

ML= \
	pa_ppx_fmtformat_test.ml \
	machinery_test.ml \

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=pa_ppx_fmtformat_test.exe machinery_test.exe

all: $(TESTS)

test:: all
	mkdir -p _build && ./pa_ppx_fmtformat_test.exe
	mkdir -p _build && ./machinery_test.exe

pa_ppx_fmtformat_test.exe: pa_ppx_fmtformat_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

machinery_test.exe: machinery_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

clean::
	rm -rf *.cm* _build *.exe

