SOURCES = evi.el evi-mule.el lookup.el lookup-utils.el lookup-types.el \
	  lookup-vars.el lookup-gaiji.el lookup-cache.el lookup-record.el \
	  lookup-search.el lookup-select.el lookup-history.el \
	  lookup-autoloads.el \
	  ndtut.el ndic.el ndeb.el ndtp.el ndict.el ndsrd.el ndkks.el \
	  ndspell.el ndcookie.el sdicf.el stem-english.el
EXTRA_DIST = lookup-vars.el.in
DISTCLEANFILES = lookup-vars.el
MAINTAINERCLEANFILES = Makefile.in

lisp_LISP = $(SOURCES)
ELCFILES = $(lisp_LISP:.el=.elc)

$(ELCFILES): lookup-vars.el ../install.el $(lisp_LISP)
	@$(EMACS) -batch $(lisp_LISP)

lookup-vars.el: $(top_builddir)/config.status $(srcdir)/lookup-vars.el.in
	@rm -f lookup-vars.el
	sed -e "s/\@VERSION\@/$(VERSION)/" \
	     -e "s!\@pkgemacsdir\@!$(emacsdir)/lookup!" \
	     $(srcdir)/lookup-vars.el.in > lookup-vars.el
