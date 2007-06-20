TARGETS = screenshot.html dictionaries.html

all: $(TARGETS)

clean::
	rm -f $(TARGETS)

%.html: %.tpl.html
	 ruby -p -e 'gsub(/<!-- (.+\.html) -->/) {|f| $$_ + open($$1).readline(nil)}'  $< > $@ || rm $@
	chmod go+r $@
