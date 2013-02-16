SOURCES = lookup.el lookup-utils.el lookup-types.el lookup-vars.el	\
	lookup-cache.el lookup-summary.el lookup-content.el		\
	lookup-select.el lookup-modules.el lookup-history.el		\
	lookup-text.el stem-english.el ndbtonic.el ndcookie.el		\
	ndeb.el ndeb-binary.el nddsl.el ndic.el ndict.el ndjitsuu.el	\
	ndlatin.el ndpdic.el ndsary.el ndbuffer.el ndspell.el		\
	ndspotlight.el ndsrd.el ndtut.el
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
