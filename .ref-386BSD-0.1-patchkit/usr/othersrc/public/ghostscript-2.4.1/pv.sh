#!/bin/sh -f
#
# pv - preview a specified page of a dvi file in a Ghostscript window
# usage: pv page file
#
# pv converts the given page to PostScript and displays it
# in a Ghostscript window.
#
if [ $# -lt 2 ] ;then
  echo usage: $0 'page_number file_name[.dvi]'
  exit 1
fi
RESOLUTION=100
TEMPDIR=.
PAGE=$1
shift
FILE=$1
shift
trap "rm -rf $TEMPDIR/$FILE.$$.pv" 0 1 2 15
dvips -D$RESOLUTION -p $PAGE -n 1 $FILE $* -o $FILE.$$.pv
gs $FILE.$$.pv
exit 0
