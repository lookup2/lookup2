TARGETS = screenshot.html download.html tips.html

all: $(TARGETS)

clean::
	rm -f $(TARGETS)

%.html: %.scm ../menu.scm ../subpage.scm
	guile -s $< | nkf -j > $@ || rm $@
	chmod g+w $@
