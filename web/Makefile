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
	rsync -av -e ssh --delete --exclude=CVS --exclude='*~' \
	  --exclude="header.html" --exclude='footer.html' --exclude='subpage.html' \
	  --exclude="*.tpl.html" --exclude='Makefile*' --exclude='*.in' --exclude="*.scm" \
	  * $(SF_USERNAME)@lookup.sourceforge.net:/home/groups/l/lo/lookup/htdocs
#	(cd ja && $(MAKE) install)

%.html: %.tpl.html
	 ruby -p -e 'gsub(/<!-- (.+\.html) -->/) {|f| $$_ + open($$1).readline(nil)}'  $< > $@ || rm $@
	chmod g+w $@
