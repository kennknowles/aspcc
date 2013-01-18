PACKAGE = aspcc
FINDLIB_PACKAGE = aspcc
DESTDIR = 
PREFIX = /usr/local
PREFIX_DESTDIR = $(DESTDIR)$(PREFIX)
FINDLIB_FILES = 
SOURCES = \
	frontends/MyArg.ml parsed/Symbol.ml runtime/Tables.ml parsed/AspAst.ml \
	runtime/VbTypes.ml parsed/Opcode.ml runtime/VbValues.ml output/vb.ml \
	output/opcodeDump.ml parsing/aspParser.ml parsing/aspLexer.ml typing/Typing.ml \
	runtime/Runtime.ml runtime/VbClass.ml runtime/AstRun.ml runtime/OpcodeRun.ml \
	output/lexDump.ml output/astDump.ml frontends/MyArg.mli parsed/Symbol.mli \
	runtime/Tables.mli runtime/VbValues.mli output/vb.mli parsing/aspParser.mli \
	runtime/Runtime.mli runtime/VbClass.mli runtime/AstRun.mli output/lexDump.mli \
	output/astDump.mli parsing/aspLexer.mll parsing/aspParser.mly 
OBJECTS = \
	frontends/MyArg.cmo parsed/Symbol.cmo runtime/Tables.cmo parsed/AspAst.cmo \
	runtime/VbTypes.cmo parsed/Opcode.cmo runtime/VbValues.cmo output/vb.cmo \
	output/opcodeDump.cmo parsing/aspParser.cmo parsing/aspLexer.cmo \
	typing/Typing.cmo runtime/Runtime.cmo runtime/VbClass.cmo runtime/AstRun.cmo \
	runtime/OpcodeRun.cmo output/lexDump.cmo output/astDump.cmo 
INTERFACES = \
	frontends/MyArg.cmi frontends/MyArg.cmi parsed/Symbol.cmi parsed/Symbol.cmi \
	runtime/Tables.cmi runtime/Tables.cmi parsed/AspAst.cmi runtime/VbTypes.cmi \
	parsed/Opcode.cmi runtime/VbValues.cmi runtime/VbValues.cmi output/vb.cmi \
	output/vb.cmi output/opcodeDump.cmi parsing/aspParser.cmi parsing/aspLexer.cmi \
	typing/Typing.cmi runtime/Runtime.cmi runtime/Runtime.cmi runtime/VbClass.cmi \
	runtime/VbClass.cmi runtime/AstRun.cmi runtime/AstRun.cmi runtime/OpcodeRun.cmi \
	output/lexDump.cmi output/lexDump.cmi output/astDump.cmi output/astDump.cmi 
OPT_OBJECTS = \
	frontends/MyArg.cmx parsed/Symbol.cmx runtime/Tables.cmx parsed/AspAst.cmx \
	runtime/VbTypes.cmx parsed/Opcode.cmx runtime/VbValues.cmx output/vb.cmx \
	output/opcodeDump.cmx parsing/aspParser.cmx parsing/aspLexer.cmx \
	typing/Typing.cmx runtime/Runtime.cmx runtime/VbClass.cmx runtime/AstRun.cmx \
	runtime/OpcodeRun.cmx output/lexDump.cmx output/astDump.cmx 
O_OBJECTS = 
LIBS = 
OPT_LIBS = 
FINDLIBS = -package 'str pcre unix netstring'
INCLUDES = \
	-I parsed -I parsing -I runtime -I output -I frontends -I compile -I typing 
COMPILE_BYTE = \
	ocamlfind ocamlc $(FINDLIBS) -c -g $(INCLUDES) 
COMPILE_NATIVE = \
	ocamlfind ocamlopt $(FINDLIBS) -c $(INCLUDES) 
COMPILE_PROF = ocamlfind ocamlcp $(FINDLIBS) -c $(INCLUDES)
COMPILE_C = gcc -DNATIVE_CODE -I`ocamlc -where` -c
PRECOMP_YACC = ocamlyacc -v $(INCLUDES)
PRECOMP_LEX = ocamllex $(INCLUDES)
LINK_BYTE = \
	ocamlfind ocamlc $(FINDLIBS) -linkpkg $(INCLUDES) $(LIBS) 
LINK_NATIVE = \
	ocamlfind ocamlopt $(FINDLIBS) -linkpkg $(INCLUDES) $(OPT_LIBS) 
LINK_PROF = \
	ocamlfind ocamlcp $(FINDLIBS) -linkpkg $(INCLUDES) $(LIBS) $(INCLUDES) \
	$(LIBS) 
LINKLIB_BYTE = \
	ocamlfind ocamlc $(FINDLIBS) -linkpkg -a $(INCLUDES) $(LIBS) 
LINKLIB_NATIVE = \
	ocamlfind ocamlopt $(FINDLIBS) -a $(INCLUDES) $(OPT_LIBS) 
LINKLIB_PROF = \
	ocamlfind ocamlcp $(FINDLIBS) -linkpkg -a $(INCLUDES) $(LIBS) $(INCLUDES) \
	$(LIBS) 
LINKLIB_SHARED = gcc -shared
LINKLIB_STATIC = ar rc
LINK_TOP = \
	ocamlfind ocamlmktop $(FINDLIBS) -linkpkg $(INCLUDES) $(LIBS) 
DOC_HTML = \
	ocamlfind ocamldoc $(FINDLIBS) -html -I ml -colorize-code -sort -keep-code \
	$(INCLUDES) 
DOC_TEXI = \
	ocamlfind ocamldoc $(FINDLIBS) -texi -I ml -colorize-code -sort -keep-code \
	$(INCLUDES) 
DOC_LATEX = \
	ocamlfind ocamldoc $(FINDLIBS) -latex -I ml -colorize-code -sort -keep-code \
	$(INCLUDES) 
DOC_DOT = \
	ocamlfind ocamldoc $(FINDLIBS) -dot -I ml -colorize-code -sort -keep-code \
	$(INCLUDES) 
DOC_MAN = \
	ocamlfind ocamldoc $(FINDLIBS) -man -I ml -colorize-code -sort -keep-code \
	$(INCLUDES) 
docs_SOURCES = \
	runtime/modules/vbStdLib.ml runtime/modules/scripting.ml runtime/modules/msXml_pxp.ml \
	runtime/modules/msXml_gdome2.ml runtime/modules/aspConsole.ml frontends/aspcc.ml \
	frontends/mod_aspcc.ml 
docs_FINDLIBS = 
aspcc_SOURCES = \
	runtime/modules/VbPervasives.ml compile/compile.ml frontends/aspcc.ml 
aspcc_OBJECTS = \
	runtime/modules/VbPervasives.cmo compile/compile.cmo frontends/aspcc.cmo 
aspcc_LIBS = 
aspcc_O_OBJECTS = 
aspcc_OPT_OBJECTS = \
	runtime/modules/VbPervasives.cmx compile/compile.cmx frontends/aspcc.cmx 
aspcc_OPT_LIBS = 
aspcc_FINDLIBS = 
asptop_SOURCES = \
	runtime/modules/VbPervasives.ml compile/compile.ml frontends/asptop.ml 
asptop_OBJECTS = \
	runtime/modules/VbPervasives.cmo compile/compile.cmo frontends/asptop.cmo 
asptop_LIBS = 
asptop_O_OBJECTS = 
asptop_OPT_OBJECTS = \
	runtime/modules/VbPervasives.cmx compile/compile.cmx frontends/asptop.cmx 
asptop_OPT_LIBS = 
asptop_FINDLIBS = 
aspdoc_SOURCES = \
	parsed/doc.ml output/HtmlDoc.ml frontends/aspdoc.ml parsed/doc.mli \
	output/HtmlDoc.mli 
aspdoc_OBJECTS = \
	parsed/doc.cmo output/HtmlDoc.cmo frontends/aspdoc.cmo 
aspdoc_LIBS = 
aspdoc_O_OBJECTS = 
aspdoc_OPT_OBJECTS = \
	parsed/doc.cmx output/HtmlDoc.cmx frontends/aspdoc.cmx 
aspdoc_OPT_LIBS = 
aspdoc_FINDLIBS = 
aspcheck_SOURCES = frontends/aspcheck.ml
aspcheck_OBJECTS = frontends/aspcheck.cmo
aspcheck_LIBS = 
aspcheck_O_OBJECTS = 
aspcheck_OPT_OBJECTS = frontends/aspcheck.cmx
aspcheck_OPT_LIBS = 
aspcheck_FINDLIBS = 

all: aspcc asptop aspdoc aspcheck

byte: aspcc asptop aspdoc aspcheck

opt: aspcc.opt asptop.opt aspdoc.opt aspcheck.opt

doc: docs.html

top: 

install: install-findlib install-aspcc install-asptop install-aspdoc install-aspcheck

uninstall: uninstall-findlib uninstall-aspcc uninstall-asptop uninstall-aspdoc uninstall-aspcheck

reinstall: uninstall
	$(MAKE) install

install-opt: install-aspcc.opt install-asptop.opt install-aspdoc.opt install-aspcheck.opt

clean: 
	rm -rf $(OBJECTS) $(INTERFACES) $(O_OBJECTS) $(OPT_OBJECTS) aspcc aspcc.opt runtime/modules/VbPervasives.cmi compile/compile.cmi frontends/aspcc.cmi runtime/modules/VbPervasives.cmo compile/compile.cmo frontends/aspcc.cmo runtime/modules/VbPervasives.cmx compile/compile.cmx frontends/aspcc.cmx runtime/modules/VbPervasives.o compile/compile.o frontends/aspcc.o asptop asptop.opt runtime/modules/VbPervasives.cmi compile/compile.cmi frontends/asptop.cmi runtime/modules/VbPervasives.cmo compile/compile.cmo frontends/asptop.cmo runtime/modules/VbPervasives.cmx compile/compile.cmx frontends/asptop.cmx runtime/modules/VbPervasives.o compile/compile.o frontends/asptop.o aspdoc aspdoc.opt parsed/doc.cmi parsed/doc.cmi output/HtmlDoc.cmi output/HtmlDoc.cmi frontends/aspdoc.cmi parsed/doc.cmo output/HtmlDoc.cmo frontends/aspdoc.cmo parsed/doc.cmx output/HtmlDoc.cmx frontends/aspdoc.cmx parsed/doc.o output/HtmlDoc.o frontends/aspdoc.o aspcheck aspcheck.opt frontends/aspcheck.cmi frontends/aspcheck.cmo frontends/aspcheck.cmx frontends/aspcheck.o 

distclean: 
	$(MAKE) clean && rm -f Makefile

install-findlib: 

uninstall-findlib: 

docs.html: $(SOURCES) $(docs_SOURCES)
	mkdir -p docs.html && $(DOC_HTML) $(docs_FINDLIBS) -d docs.html $(SOURCES) $(docs_SOURCES)

docs.man: $(SOURCES) $(docs_SOURCES)
	mkdir -p docs.man && $(DOC_MAN) $(docs_FINDLIBS) -d docs.man $(SOURCES) $(docs_SOURCES)

docs.texi: $(SOURCES) $(docs_SOURCES)
	 $(DOC_TEXI) $(docs_FINDLIBS) -o docs.texi $(SOURCES) $(docs_SOURCES)

docs.dot: $(SOURCES) $(docs_SOURCES)
	 $(DOC_DOT) $(docs_FINDLIBS) -o docs.dot $(SOURCES) $(docs_SOURCES)

docs.latex: $(SOURCES) $(docs_SOURCES)
	 $(DOC_LATEX) $(docs_FINDLIBS) -o docs.latex $(SOURCES) $(docs_SOURCES)

aspcc: $(OBJECTS) $(O_OBJECTS) $(aspcc_OBJECTS) $(aspcc_O_OBJECTS)
	$(LINK_BYTE) $(aspcc_FINDLIBS) $(aspcc_LIBS) -o $@ $(O_OBJECTS) $(OBJECTS) $(aspcc_O_OBJECTS) $(aspcc_OBJECTS)

aspcc.opt: $(OPT_OBJECTS) $(O_OBJECTS) $(aspcc_OPT_OBJECTS) $(aspcc_O_OBJECTS)
	$(LINK_NATIVE) $(aspcc_FINDLIBS) $(aspcc_OPT_LIBS) -o $@ $(O_OBJECTS) $(OPT_OBJECTS) $(aspcc_O_OBJECTS) $(aspcc_OPT_OBJECTS)

install-aspcc: aspcc
	install -d $(PREFIX_DESTDIR)/bin
	install -m 755 aspcc $(PREFIX_DESTDIR)/bin/aspcc

uninstall-aspcc: 
	rm -f $(PREFIX_DESTDIR)/bin/aspcc
	rm -f $(PREFIX_DESTDIR)/bin/aspcc.opt

install-aspcc.opt: aspcc.opt
	install -d $(PREFIX_DESTDIR)/bin
	install -m 755 aspcc.opt $(PREFIX_DESTDIR)/bin/aspcc.opt

asptop: $(OBJECTS) $(O_OBJECTS) $(asptop_OBJECTS) $(asptop_O_OBJECTS)
	$(LINK_BYTE) $(asptop_FINDLIBS) $(asptop_LIBS) -o $@ $(O_OBJECTS) $(OBJECTS) $(asptop_O_OBJECTS) $(asptop_OBJECTS)

asptop.opt: $(OPT_OBJECTS) $(O_OBJECTS) $(asptop_OPT_OBJECTS) $(asptop_O_OBJECTS)
	$(LINK_NATIVE) $(asptop_FINDLIBS) $(asptop_OPT_LIBS) -o $@ $(O_OBJECTS) $(OPT_OBJECTS) $(asptop_O_OBJECTS) $(asptop_OPT_OBJECTS)

install-asptop: asptop
	install -d $(PREFIX_DESTDIR)/bin
	install -m 755 asptop $(PREFIX_DESTDIR)/bin/asptop

uninstall-asptop: 
	rm -f $(PREFIX_DESTDIR)/bin/asptop
	rm -f $(PREFIX_DESTDIR)/bin/asptop.opt

install-asptop.opt: asptop.opt
	install -d $(PREFIX_DESTDIR)/bin
	install -m 755 asptop.opt $(PREFIX_DESTDIR)/bin/asptop.opt

aspdoc: $(OBJECTS) $(O_OBJECTS) $(aspdoc_OBJECTS) $(aspdoc_O_OBJECTS)
	$(LINK_BYTE) $(aspdoc_FINDLIBS) $(aspdoc_LIBS) -o $@ $(O_OBJECTS) $(OBJECTS) $(aspdoc_O_OBJECTS) $(aspdoc_OBJECTS)

aspdoc.opt: $(OPT_OBJECTS) $(O_OBJECTS) $(aspdoc_OPT_OBJECTS) $(aspdoc_O_OBJECTS)
	$(LINK_NATIVE) $(aspdoc_FINDLIBS) $(aspdoc_OPT_LIBS) -o $@ $(O_OBJECTS) $(OPT_OBJECTS) $(aspdoc_O_OBJECTS) $(aspdoc_OPT_OBJECTS)

install-aspdoc: aspdoc
	install -d $(PREFIX_DESTDIR)/bin
	install -m 755 aspdoc $(PREFIX_DESTDIR)/bin/aspdoc

uninstall-aspdoc: 
	rm -f $(PREFIX_DESTDIR)/bin/aspdoc
	rm -f $(PREFIX_DESTDIR)/bin/aspdoc.opt

install-aspdoc.opt: aspdoc.opt
	install -d $(PREFIX_DESTDIR)/bin
	install -m 755 aspdoc.opt $(PREFIX_DESTDIR)/bin/aspdoc.opt

aspcheck: $(OBJECTS) $(O_OBJECTS) $(aspcheck_OBJECTS) $(aspcheck_O_OBJECTS)
	$(LINK_BYTE) $(aspcheck_FINDLIBS) $(aspcheck_LIBS) -o $@ $(O_OBJECTS) $(OBJECTS) $(aspcheck_O_OBJECTS) $(aspcheck_OBJECTS)

aspcheck.opt: $(OPT_OBJECTS) $(O_OBJECTS) $(aspcheck_OPT_OBJECTS) $(aspcheck_O_OBJECTS)
	$(LINK_NATIVE) $(aspcheck_FINDLIBS) $(aspcheck_OPT_LIBS) -o $@ $(O_OBJECTS) $(OPT_OBJECTS) $(aspcheck_O_OBJECTS) $(aspcheck_OPT_OBJECTS)

install-aspcheck: aspcheck
	install -d $(PREFIX_DESTDIR)/bin
	install -m 755 aspcheck $(PREFIX_DESTDIR)/bin/aspcheck

uninstall-aspcheck: 
	rm -f $(PREFIX_DESTDIR)/bin/aspcheck
	rm -f $(PREFIX_DESTDIR)/bin/aspcheck.opt

install-aspcheck.opt: aspcheck.opt
	install -d $(PREFIX_DESTDIR)/bin
	install -m 755 aspcheck.opt $(PREFIX_DESTDIR)/bin/aspcheck.opt

%.ml: %.mll
	$(PRECOMP_LEX) $<

%.ml: %.mly
	$(PRECOMP_YACC $<

%.mli: %.mly
	$(PRECOMP_YACC $<

%.cmi: %.mli
	$(COMPILE_BYTE) -o $@ $<

%.cmi: %.ml
	$(COMPILE_BYTE) -o $@ $<

%.cmo: %.ml
	$(COMPILE_BYTE) -o $@ $<

%.cmx: %.ml
	$(COMPILE_NATIVE) -o $@ $<

%.o: %.c
	$(COMPILE_C) -o $@ $<

parsing/aspLexer..ml: parsing/aspLexer.mll

parsing/aspParser..cmo: parsing/aspParser..cmi

parsing/aspParser..cmx: parsing/aspParser..cmi

gzip: 
	$(MAKE) distclean && ln -s . aspcc-0.1 && tar --exclude='*~' --exclude='CVS' --exclude='_darcs' --exclude='.arch*' --exclude='*/aspcc-0.1' --exclude='aspcc-0.1.tar.gz' -z -chvf aspcc-0.1.tar.gz aspcc-0.1 && rm aspcc-0.1

bzip2: 
	$(MAKE) distclean && ln -s . aspcc-0.1 && tar --exclude='*~' --exclude='CVS' --exclude='_darcs' --exclude='.arch*' --exclude='*/aspcc-0.1' --exclude='aspcc-0.1.tar.bz2' -j -chvf aspcc-0.1.tar.bz2 aspcc-0.1 && rm aspcc-0.1

tar: 
	$(MAKE) distclean && ln -s . aspcc-0.1 && tar --exclude='*~' --exclude='CVS' --exclude='_darcs' --exclude='.arch*' --exclude='*/aspcc-0.1' --exclude='aspcc-0.1.tar'  -chvf aspcc-0.1.tar aspcc-0.1 && rm aspcc-0.1

frontends/MyArg.cmo: frontends/MyArg.cmi 
frontends/MyArg.cmx: frontends/MyArg.cmi 
parsed/Symbol.cmo: parsed/Symbol.cmi 
parsed/Symbol.cmx: parsed/Symbol.cmi 
runtime/Tables.cmo: parsed/Symbol.cmi runtime/Tables.cmi 
runtime/Tables.cmx: parsed/Symbol.cmx runtime/Tables.cmi 
parsed/AspAst.cmo: parsed/Symbol.cmi 
parsed/AspAst.cmx: parsed/Symbol.cmx 
runtime/VbTypes.cmo: parsed/Symbol.cmi 
runtime/VbTypes.cmx: parsed/Symbol.cmx 
parsed/Opcode.cmo: runtime/VbTypes.cmo 
parsed/Opcode.cmx: runtime/VbTypes.cmx 
runtime/VbValues.cmo: runtime/VbTypes.cmo parsed/Symbol.cmi parsed/AspAst.cmo \
    runtime/VbValues.cmi 
runtime/VbValues.cmx: runtime/VbTypes.cmx parsed/Symbol.cmx parsed/AspAst.cmx \
    runtime/VbValues.cmi 
output/vb.cmo: parsed/Symbol.cmi parsed/AspAst.cmo output/vb.cmi 
output/vb.cmx: parsed/Symbol.cmx parsed/AspAst.cmx output/vb.cmi 
output/opcodeDump.cmo: runtime/VbTypes.cmo parsed/Opcode.cmo 
output/opcodeDump.cmx: runtime/VbTypes.cmx parsed/Opcode.cmx 
parsing/aspParser.cmo: parsed/Symbol.cmi parsed/AspAst.cmo \
    parsing/aspParser.cmi 
parsing/aspParser.cmx: parsed/Symbol.cmx parsed/AspAst.cmx \
    parsing/aspParser.cmi 
parsing/aspLexer.cmo: parsed/Symbol.cmi parsing/aspParser.cmi 
parsing/aspLexer.cmx: parsed/Symbol.cmx parsing/aspParser.cmx 
typing/Typing.cmo: parsed/Symbol.cmi parsed/AspAst.cmo 
typing/Typing.cmx: parsed/Symbol.cmx parsed/AspAst.cmx 
runtime/Runtime.cmo: runtime/VbValues.cmi runtime/VbTypes.cmo output/vb.cmi \
    runtime/Tables.cmi parsed/Symbol.cmi parsed/AspAst.cmo \
    runtime/Runtime.cmi 
runtime/Runtime.cmx: runtime/VbValues.cmx runtime/VbTypes.cmx output/vb.cmx \
    runtime/Tables.cmx parsed/Symbol.cmx parsed/AspAst.cmx \
    runtime/Runtime.cmi 
runtime/VbClass.cmo: runtime/VbValues.cmi runtime/VbTypes.cmo \
    runtime/Tables.cmi parsed/Symbol.cmi runtime/Runtime.cmi \
    parsed/AspAst.cmo runtime/VbClass.cmi 
runtime/VbClass.cmx: runtime/VbValues.cmx runtime/VbTypes.cmx \
    runtime/Tables.cmx parsed/Symbol.cmx runtime/Runtime.cmx \
    parsed/AspAst.cmx runtime/VbClass.cmi 
runtime/AstRun.cmo: runtime/VbValues.cmi runtime/VbTypes.cmo \
    runtime/VbClass.cmi output/vb.cmi runtime/Tables.cmi parsed/Symbol.cmi \
    runtime/Runtime.cmi parsing/aspParser.cmi parsing/aspLexer.cmo \
    parsed/AspAst.cmo runtime/AstRun.cmi 
runtime/AstRun.cmx: runtime/VbValues.cmx runtime/VbTypes.cmx \
    runtime/VbClass.cmx output/vb.cmx runtime/Tables.cmx parsed/Symbol.cmx \
    runtime/Runtime.cmx parsing/aspParser.cmx parsing/aspLexer.cmx \
    parsed/AspAst.cmx runtime/AstRun.cmi 
runtime/OpcodeRun.cmo: runtime/VbValues.cmi runtime/VbTypes.cmo \
    output/opcodeDump.cmo parsed/Opcode.cmo 
runtime/OpcodeRun.cmx: runtime/VbValues.cmx runtime/VbTypes.cmx \
    output/opcodeDump.cmx parsed/Opcode.cmx 
output/lexDump.cmo: parsed/Symbol.cmi parsing/aspParser.cmi \
    output/lexDump.cmi 
output/lexDump.cmx: parsed/Symbol.cmx parsing/aspParser.cmx \
    output/lexDump.cmi 
output/astDump.cmo: parsed/Symbol.cmi parsed/AspAst.cmo output/astDump.cmi 
output/astDump.cmx: parsed/Symbol.cmx parsed/AspAst.cmx output/astDump.cmi 
runtime/Tables.cmi: parsed/Symbol.cmi 
runtime/VbValues.cmi: runtime/VbTypes.cmo parsed/Symbol.cmi 
output/vb.cmi: parsed/AspAst.cmo 
parsing/aspParser.cmi: parsed/Symbol.cmi parsed/AspAst.cmo 
runtime/Runtime.cmi: runtime/VbTypes.cmo runtime/Tables.cmi parsed/Symbol.cmi \
    parsed/AspAst.cmo 
runtime/VbClass.cmi: runtime/VbTypes.cmo runtime/Tables.cmi parsed/Symbol.cmi \
    runtime/Runtime.cmi parsed/AspAst.cmo 
runtime/AstRun.cmi: runtime/VbTypes.cmo runtime/Runtime.cmi parsed/AspAst.cmo 
output/lexDump.cmi: parsing/aspParser.cmi 
output/astDump.cmi: parsed/AspAst.cmo 
runtime/modules/scripting.cmo: runtime/VbValues.cmi runtime/VbTypes.cmo \
    runtime/VbClass.cmi parsed/Symbol.cmi runtime/Runtime.cmi 
runtime/modules/scripting.cmx: runtime/VbValues.cmx runtime/VbTypes.cmx \
    runtime/VbClass.cmx parsed/Symbol.cmx runtime/Runtime.cmx 
runtime/modules/msXml_gdome2.cmo: runtime/VbTypes.cmo 
runtime/modules/msXml_gdome2.cmx: runtime/VbTypes.cmx 
runtime/modules/aspConsole.cmo: runtime/VbTypes.cmo 
runtime/modules/aspConsole.cmx: runtime/VbTypes.cmx 
frontends/aspcc.cmo: runtime/Runtime.cmi runtime/OpcodeRun.cmo \
    output/opcodeDump.cmo frontends/MyArg.cmi output/lexDump.cmi \
    compile/compile.cmo runtime/AstRun.cmi output/astDump.cmi \
    parsing/aspParser.cmi parsing/aspLexer.cmo 
frontends/aspcc.cmx: runtime/Runtime.cmx runtime/OpcodeRun.cmx \
    output/opcodeDump.cmx frontends/MyArg.cmx output/lexDump.cmx \
    compile/compile.cmx runtime/AstRun.cmx output/astDump.cmx \
    parsing/aspParser.cmx parsing/aspLexer.cmx 
frontends/mod_aspcc.cmo: runtime/VbTypes.cmo parsing/aspParser.cmi \
    parsing/aspLexer.cmo 
frontends/mod_aspcc.cmx: runtime/VbTypes.cmx parsing/aspParser.cmx \
    parsing/aspLexer.cmx 
runtime/modules/VbPervasives.cmo: runtime/VbValues.cmi runtime/VbTypes.cmo \
    runtime/VbClass.cmi parsed/Symbol.cmi runtime/Runtime.cmi 
runtime/modules/VbPervasives.cmx: runtime/VbValues.cmx runtime/VbTypes.cmx \
    runtime/VbClass.cmx parsed/Symbol.cmx runtime/Runtime.cmx 
compile/compile.cmo: runtime/VbTypes.cmo parsed/Symbol.cmi parsed/Opcode.cmo \
    parsed/AspAst.cmo 
compile/compile.cmx: runtime/VbTypes.cmx parsed/Symbol.cmx parsed/Opcode.cmx \
    parsed/AspAst.cmx 
frontends/aspcc.cmo: runtime/Runtime.cmi runtime/OpcodeRun.cmo \
    output/opcodeDump.cmo frontends/MyArg.cmi output/lexDump.cmi \
    compile/compile.cmo runtime/AstRun.cmi output/astDump.cmi \
    parsing/aspParser.cmi parsing/aspLexer.cmo 
frontends/aspcc.cmx: runtime/Runtime.cmx runtime/OpcodeRun.cmx \
    output/opcodeDump.cmx frontends/MyArg.cmx output/lexDump.cmx \
    compile/compile.cmx runtime/AstRun.cmx output/astDump.cmx \
    parsing/aspParser.cmx parsing/aspLexer.cmx 
runtime/modules/VbPervasives.cmo: runtime/VbValues.cmi runtime/VbTypes.cmo \
    runtime/VbClass.cmi parsed/Symbol.cmi runtime/Runtime.cmi 
runtime/modules/VbPervasives.cmx: runtime/VbValues.cmx runtime/VbTypes.cmx \
    runtime/VbClass.cmx parsed/Symbol.cmx runtime/Runtime.cmx 
compile/compile.cmo: runtime/VbTypes.cmo parsed/Symbol.cmi parsed/Opcode.cmo \
    parsed/AspAst.cmo 
compile/compile.cmx: runtime/VbTypes.cmx parsed/Symbol.cmx parsed/Opcode.cmx \
    parsed/AspAst.cmx 
frontends/asptop.cmo: runtime/VbValues.cmi runtime/VbTypes.cmo \
    parsed/Symbol.cmi runtime/Runtime.cmi frontends/MyArg.cmi \
    runtime/AstRun.cmi parsing/aspParser.cmi parsing/aspLexer.cmo 
frontends/asptop.cmx: runtime/VbValues.cmx runtime/VbTypes.cmx \
    parsed/Symbol.cmx runtime/Runtime.cmx frontends/MyArg.cmx \
    runtime/AstRun.cmx parsing/aspParser.cmx parsing/aspLexer.cmx 
parsed/doc.cmo: parsed/Symbol.cmi parsing/aspParser.cmi parsing/aspLexer.cmo \
    parsed/AspAst.cmo parsed/doc.cmi 
parsed/doc.cmx: parsed/Symbol.cmx parsing/aspParser.cmx parsing/aspLexer.cmx \
    parsed/AspAst.cmx parsed/doc.cmi 
output/HtmlDoc.cmo: parsed/Symbol.cmi parsed/doc.cmi output/HtmlDoc.cmi 
output/HtmlDoc.cmx: parsed/Symbol.cmx parsed/doc.cmx output/HtmlDoc.cmi 
frontends/aspdoc.cmo: frontends/MyArg.cmi output/HtmlDoc.cmi \
    parsing/aspParser.cmi 
frontends/aspdoc.cmx: frontends/MyArg.cmx output/HtmlDoc.cmx \
    parsing/aspParser.cmx 
parsed/doc.cmi: parsed/Symbol.cmi parsed/AspAst.cmo 
output/HtmlDoc.cmi: parsed/doc.cmi 
frontends/aspcheck.cmo: typing/Typing.cmo frontends/MyArg.cmi \
    parsing/aspParser.cmi parsing/aspLexer.cmo 
frontends/aspcheck.cmx: typing/Typing.cmx frontends/MyArg.cmx \
    parsing/aspParser.cmx parsing/aspLexer.cmx 
