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
	  * kawabata@web.sourceforge.net:/home/project-web/lookup/htdocs
