SOURCES = evi.el evi-mule.el lookup.el lookup-utils.el lookup-types.el \
	  lookup-vars.el lookup-gaiji.el lookup-cache.el lookup-record.el \
	  lookup-search.el lookup-module.el lookup-history.el \
	  lookup-autoloads.el \
	  ndtut.el ndic.el ndeb.el ndtp.el ndict.el ndsrd.el ndkks.el \
	  ndspell.el ndcookie.el sdicf.el stem-english.el
MAINTAINERCLEANFILES = Makefile.in lookup-autoloads.el

lisp_LISP = $(SOURCES)
ELCFILES = $(lisp_LISP:.el=.elc)

$(ELCFILES): $(lisp_LISP)
	@$(EMACS) -batch -f batch-byte-compile $(lisp_LISP)

lookup-autoloads.el:
