SOURCES = lookup.el lookup-utils.el lookup-types.el lookup-vars.el	\
	lookup-cache.el lookup-summary.el lookup-content.el		\
	lookup-select.el lookup-modules.el lookup-history.el		\
	lookup-text.el stem-english.el  aozora-view.el			\
	ndbtonic.el ndcookie.el ndeb.el ndeb-binary.el nddsl.el		\
	ndest.el ndic.el ndict.el ndjitsuu.el ndkanji.el ndlatin.el	\
	ndmecab.el ndmisc.el ndpdic.el ndsary.el ndsimple.el		\
	ndspell.el ndspotlight.el ndsrd.el ndtut.el nducs.el		\
	ndwikipedia.el ndwinsearch.el					\
	support-defs.el support-cebd.el support-chiezo.el		\
	support-chinakan.el support-chujiten.el support-colloc.el	\
	support-comp.el support-crown.el support-daijirin.el		\
	support-eijiro.el support-foldoc.el support-genius.el		\
	support-iwakoku.el support-jargon.el support-jmdict.el		\
	support-jitsuu.el support-kojien.el support-mwsedic.el		\
	support-mypaedia.el support-nhd.el support-onmusic.el		\
	support-oxford.el support-pdh.el support-plus.el		\
	support-plusv2.el support-rangjung.el support-rfc1983.el	\
	support-rfc4949.el support-rika.el support-sbgy.el		\
	support-sdic.el support-skp.el support-srd.el support-swjz.el	\
	support-unihan.el support-vera.el support-waseikanji.el		\
	support-xszd.el support-zhongri.el support-zigen.el
ELCFILES = $(SOURCES:.el=.elc)
EXTRA_DIST = $(SOURCES) lookup-compile.el
INSTALLFILES = $(SOURCES) $(ELCFILES) lookup-autoloads.el
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
