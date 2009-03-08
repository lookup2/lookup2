SOURCES = lookup.el lookup-utils.el lookup-types.el lookup-vars.el \
	lookup-cache.el lookup-summary.el lookup-content.el lookup-select.el \
	lookup-modules.el lookup-history.el lookup-kanji.el \
        sdicf.el stem-english.el \
	ndtut.el ndic.el ndeb.el ndeb-binary.el ndict.el ndsrd.el ndkks.el \
	ndspell.el ndcookie.el ndlatin.el
ELCFILES = $(SOURCES:.el=.elc)
EXTRA_DIST = $(SOURCES) lookup-compile.el
INSTALLFILES = $(SOURCES) $(ELCFILES) lookup-autoloads.el \
	support-loaddef.el support-chiezo.el support-foldoc.el \
	support-oxford.el support-chujiten.el support-genius.el \
	support-rfc1983.el support-colloc.el support-iwakoku.el \
	support-kojien.el support-plus.el support-plusv2.el \
	support-rika.el support-comp.el support-jargon.el support-srd-fpw.el \
	support-mypaedia.el support-srd.el support-nhd.el support-vera.el \
	support-zhongri.el support-cebd.el support-mwsedic.el
CLEANFILES = $(ELCFILES)
MAINTAINERCLEANFILES = Makefile.in lookup-autoloads.el

all: lookup.elc lookup-autoloads.el

lookup.elc: $(SOURCES)
	@$(EMACS) -batch -l lookup-compile.el -f lookup-bytecomp $(SOURCES)

lookup-autoloads.el: $(SOURCES)
	@$(EMACS) -batch -l lookup-compile.el -f lookup-autoload $(SOURCES)

install: all
	test -d @lispdir@/lookup || mkdir @lispdir@/lookup
	install $(INSTALLFILES) @lispdir@/lookup
