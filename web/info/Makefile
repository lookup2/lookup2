TARGETS = screenshot.html

%.html: %.scm
	guile -s $< > $@ || rm $@
