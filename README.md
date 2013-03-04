## Lookup for Emacs

*Lookup* is an integrated user interface for various dictionaries.
You can search various on-line and off-line dictionaries
simultaneously with lookup.

## Installation

For now, simply download the archive and place the contents of `lisp`
folder into your site-lisp.

When proper installer is completed, it should be installed via following command.

```
% ./configure --with-emacs=emacs \
              --with-lispdir=/your/home/emacs/lisp \
              --infodir=/your/home/emacs/info
% make install
```

## Usage

Following is an example of the settings of `~/.emacs.d/init.el` file.

```
(define-key ctl-x-map "l" 'lookup)
(define-key ctl-x-map "y" 'lookup-region)
(define-key ctl-x-map "\C-y" 'lookup-pattern)

(load "lookup-autoloads")
(setq lookup-search-agents
      '((ndeb "/usr/local/dict/epwing")
        (ndspell)))
```

(You may also create `~/.emacs.d/lookup/` directory and put `init.el`
file for various settings.)

`M-x lookup` will start *Lookup*.  `M-x lookup-region` will search the
text in the region.  If you `M-x lookup-pattern`, you will be asekd for
search words.  For details, please refer the documentation.

## Supported Dictionaries

### EPWING/EBXA dictionaries

EPWING, EBXA, etc are supported with help of `eblook` tools.  

### Text/XML dictionaries

Textual and XML-based dictionaries
are also supported.  For faster search, suffix array tool called
[sary](http://sary.sourceforge.net/) is used.  With `sary`, you can
use BTONIC or Lingvo DSL file as dictionary.

### Search engines

Google and Wikipedia searches are supported with `opensearch`
interface.

### Misc dictionaries

[Dictd](http://http://en.wikipedia.org/wiki/DICT) and some other
dictionaries are also supported.

