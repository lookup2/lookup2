TARGETS = screenshot.html

all: $(TARGETS)

%.html: %.tpl.html
	 ruby -p -e 'gsub("<!-- (.+\.html) -->") {|f| $$_ + open($$1).readline(nil)}'  $< > $@ || rm $@
