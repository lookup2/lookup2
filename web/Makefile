TARGETS = index.shtml
SUBDIRS = 

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
	rsync -av -e ssh --delete --exclude=CVS --exclude='*~' \
	  --exclude='Makefile*' --exclude='*.in' --exclude="*.scm" \
	  * $(SF_USERNAME),lookup@web.sourceforge.net:/home/groups/l/lo/lookup/htdocs
#	(cd ja && $(MAKE) install)
