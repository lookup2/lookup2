TARGETS = index.html gaiji.html os2-bitmap.html

all: $(TARGETS)

clean::
	rm -f $(TARGETS)

%.html: %.scm ../menu.scm ../subpage.scm
	guile -s $< | nkf -j > $@ || rm $@
