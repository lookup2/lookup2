dnl Copyright (C) 1999 NISHIDA Keisuke <knishida@ring.aist.go.jp>
dnl Copyright (C) 2009 Lookup Development Team
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2, or (at your option)
dnl any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl 02111-1307, USA.

AC_DEFUN([AM_PATH_LISPDIR],
 [dnl #
  dnl # Check Emacs
  dnl #
  AC_PROG_EGREP
  AC_PROG_SED
  AC_ARG_WITH(emacs,
    [  --with-emacs=EMACS      compile with EMACS [EMACS=emacs, xemacs...]],
    [case "${withval}" in
       yes)	EMACS= ;;
       no)	AC_MSG_ERROR([emacs is not available]) ;;
       *)	EMACS=${withval} ;;
     esac], EMACS=)
  if test "x$EMACS" = "xt" -o "x$EMACS" = x; then
    AC_PATH_PROGS(EMACS, emacs, no)
    if test $EMACS = no; then
      AC_MSG_ERROR(you should install Emacs first)
    fi
    if test `$EMACS --version | $EGREP -e 'GNU Emacs [[0-9]]' | $SED -e 's/GNU Emacs \([[0-9]]*\).*/\1/'` -le 22 ; then
      AC_MSG_ERROR(you need Emacs version 23 or later.)
    fi
  fi
  dnl # 
  dnl # Check Emacs directories
  dnl #
  AC_MSG_CHECKING([where emacs files are in])
  EMACS_BASENAME="`echo x$EMACS | sed -e 's/x//' -e 's/^.*\///'`"
  if test "x$emacsdir" = x; then
    if test "x$prefix" = "xNONE"; then
      prefix=$ac_default_prefix
    fi
    emacsdir="\$(datadir)/emacs"
    case "$EMACS_BASENAME" in
    emacs|emacs-*)
      if test -d $prefix/lib/emacs; then
	emacsdir="$prefix/lib/emacs"
      fi
      if test -d $prefix/share/emacs; then
	emacsdir="$prefix/share/emacs"
      fi
      ;;
    esac
  fi
  AC_MSG_RESULT($emacsdir)
  AC_SUBST(emacsdir)
  dnl # 
  dnl # Check Emacs site-lisp directories
  dnl #
  AC_ARG_WITH(lispdir,
    [  --with-lispdir=DIR      emacs lisp files go to DIR [guessed]],
    [case "${withval}" in
       yes)	lispdir= ;;
       no)	AC_MSG_ERROR(lispdir is not available) ;;
       *)	lispdir=${withval} ;;
     esac], lispdir=)
  AC_MSG_CHECKING([where .elc files should go])
  if test "x$lispdir" = x; then
    lispdir="$emacsdir/site-lisp"
    if test -d $emacsdir/lisp; then
      lispdir="$emacsdir/lisp"
    fi
  fi
  AC_MSG_RESULT($lispdir)
  AC_SUBST(lispdir)])
