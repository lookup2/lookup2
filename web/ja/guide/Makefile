all: index.html

clean:
	rm -f *.html

index.html: lookup-guide.html
	cp lookup-guide.html index.html

lookup-guide.html: lookup-guide.texi
	@mv $< $<.bak
	@nkf -e $<.bak > $<
	texi2html -short_ref -split_chapter $<
	@mv $<.bak $<
