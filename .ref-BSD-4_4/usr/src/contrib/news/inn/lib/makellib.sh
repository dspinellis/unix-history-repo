#! /bin/sh
##  $Revision: 1.2 $
##
##  Make a lint library for INN.  Usage:
##	makellib <style> "<lintflags>" <sourcefile...>
case $# in
[012])
    echo "Can't make INN lint library:  wrong number of arguments." 1>&2
    exit 1
    ;;
esac

STYLE="$1"
FLAGS="$2"
shift
shift

##  Note the lack of quotes around ${FLAGS}, below.
case "X${STYLE}" in
XBSD)
    exec lint ${FLAGS} -u -Cinn $* >/dev/null
    ;;
XSYSV)
    exec lint ${FLAGS} -u -v -x -o inn $* >/dev/null
    ;;
XNONE)
    exec cp /dev/null llib-linn.ln
    ;;
esac

echo "Can't make INN lint library:  unknown method ${STYLE}." 1>&2
exit 1
