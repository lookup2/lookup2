TARGETS = index.html
SUBDIRS = info lists howto

USER = knishida

EXCLUDE = --exclude=CVS --exclude='Makefile*' --exclude='*.scm'

all: $(TARGETS)
	@for subdir in $(SUBDIRS); do \
	  (cd $$subdir && $(MAKE)) \
	done

clean::
	rm -f $(TARGETS)
	@for subdir in $(SUBDIRS); do \
	  (cd $$subdir && $(MAKE) clean) \
	done

install:: all
	rsync -avL -e ssh --delete $(EXCLUDE) \
	  . $(USER)@openlab.ring.gr.jp:/circus/openlab/lookup

%.html:
	guile -s $< | nkf -j > $@ || rm $@
	chmod g+w $@

index.html: index.scm menu.scm news.scm
