TARGETS = screenshot.html tips.html
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

%.html: %.tpl.html
	 ruby -p -e 'gsub(/<!-- (.+\.html) -->/) {|f| $$_ + open($$1).readline(nil)}'  $< > $@ || rm $@
	chmod go+r $@
