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
	rsync -av -e ssh --delete --exclude=CVS \
	  --exclude="*.tpl.html" --exclude='Makefile*' --exclude='*.in' \
	  * kawabata@lookup.sourceforge.net:/home/groups/l/lo/lookup/htdocs
#	(cd ja && $(MAKE) install)

%.html: %.tpl.html
	 ruby -p -e 'gsub("<!-- (.+\.html) -->") {|f| $$_ + open($$1).readline(nil)}'  $< > $@ || rm $@
