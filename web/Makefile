TARGETS = index.html
SUBDIRS = info ja

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
	rsync -av -e ssh --delete --exclude=CVS --exclude='Makefile*' --exclude='*.in' \
	  * knishida@lookup.sourceforge.net:/home/groups/lookup/htdocs
	(cd ja && $(MAKE) install)

%.html: %.scm
	guile -s $< > $@ || rm $@
