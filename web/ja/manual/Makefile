all: index.html

clean:
	rm -f *.html

index.html: lookup.html
	cp lookup.html index.html

lookup.html: lookup.texi
	@mv $< $<.bak
	@nkf -e $<.bak > $<
	texi2html -short_ref -split_chapter $<
	@mv $<.bak $<
