;;; index.scm

(use-modules (text markup html))

(define title "Lookup - a Search Interface")

(print-html
 (table
  (tr #:valign "bottom"
   (td (img "/images/title.png" #:alt "Lookup"))
   (td "Language:"
       (href "English" "http://lookup.sourceforge.net/") ","
       (href "Japanese" "http://openlab.ring.gr.jp/lookup/index.html.ja"))))

<!-- header.html -->

<h1>What is Lookup</h1>
<ul>
<p>Lookup is an integrated search interface with electronic dictionaries
for the Emacs text editor.  You can use various kinds of dictionaries,
such as CD-ROM books and online dictionaries, in an efficient and
effective manner.</p>
</ul>

<h1>Latest Release</h1>
<ul>
<li>Stable version:
<a href="http://download.sourceforge.net/lookup/lookup-1.3.tar.gz">Lookup 1.3</a>,
<a href="http://openlab.ring.gr.jp/lookup/eblook/">eblook-1.3</a>
</ul>
<HR>
<ADDRESS>
Last modified: $Date: 2000/11/19 23:59:52 $<BR>
</ADDRESS>

<hr>
<a href="http://www2.valinux.com/adbouncer.phtml?f_s=468x60&f_p=478">
<img src="http://www2.valinux.com/adserver.phtml?f_s=468x60&f_p=478"
     alt="Member of the VA Affiliate Underground"
     width="468" height="60" border="0"></a>

<!-- footer.html -->
