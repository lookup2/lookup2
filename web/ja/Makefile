TARGETS = index.html
SUBDIRS = info lists manual guide howto

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
	  . knishida@openlab.ring.gr.jp:/circus/openlab/lookup

%.html:
	guile -s $< | nkf -j > $@ || rm $@

index.html: index.scm menu.scm news.scm
