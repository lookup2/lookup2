SOURCES = bocu.el stem-english.el lookup-types.el lookup-text.el	\
	 lookup.el lookup-utils.el lookup-vars.el lookup-cache.el	\
	 lookup-summary.el lookup-content.el lookup-select.el		\
	 lookup-modules.el lookup-history.el ndbtonic.el ndcookie.el	\
	 ndeb.el ndeb-binary.el nddsl.el ndic.el ndict.el ndjitsuu.el	\
	 ndlatin.el ndpdic.el ndsary.el ndbuffer.el ndspell.el		\
	 ndspotlight.el ndsrd.el ndtut.el ndest.el ndtext.el ndweb.el	\
	 support-defs.el support-files/support-chinakan.el		\
	 support-files/support-eijiro-pdic.el				\
	 support-files/support-eijiro.el				\
	 support-files/support-jitsuu.el				\
	 support-files/support-jmdict.el				\
	 support-files/support-jtfrk.el support-files/support-nhd.el	\
	 support-files/support-onmusic.el				\
	 support-files/support-rangjung.el				\
	 support-files/support-rfc1983.el				\
	 support-files/support-rfc4949.el				\
	 support-files/support-sdic.el support-files/support-srd.el	\
	 support-files/support-ucd.el support-files/support-vera.el	\
	 support-files/support-waseikanji.el				\
	 support-files/support-wikipedia.el				\
	 support-files/support-xszd.el support-files/support-zigen.el
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
