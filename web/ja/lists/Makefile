TARGETS = index.html

all: $(TARGETS)

clean::
	rm -f $(TARGETS)

%.html: %.scm ../menu.scm ../subpage.scm
	guile -s $< | nkf -j > $@ || rm $@
