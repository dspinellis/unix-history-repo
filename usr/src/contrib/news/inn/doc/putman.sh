#! /bin/sh
##  $Revision: 1.5 $
##
##  Prepare a manpage for installation, and install it.  Usage:
##	putman <style> "<installitflags>" <source> <dest-dir>
case $# in
4)
    ;;
*)
    echo "Can't install manpage:  wrong number of arguments." 1>&2
esac

STYLE="$1"
FLAGS="$2"
SRC="$3"
DEST="$4"

case "X${STYLE}" in
XNONE)
    exit 0
    ;;
XSOURCE)
    exec /bin/sh ../installit.sh ${FLAGS} ${SRC} ${DEST}
    ;;
XNROFF-PACK)
    T=${TMPDIR-/tmp}/man$$
    nroff -man ${SRC} >$T
    /bin/sh ../installit.sh ${FLAGS} $T ${DEST} && pack ${DEST}
    rm -f $T
    exit
    ;;
XNROFF-PACK-SCO)
    T=${TMPDIR-/tmp}/man$$
    nroff -man ${SRC} >$T
    DEST2=`echo ${DEST} | sed -e 's/\..$/.INN/'`
    /bin/sh ../installit.sh ${FLAGS} $T ${DEST2} && pack ${DEST2}
    rm -f $T
    exit
    ;;
esac

echo "Can't install manpage:  unknown method ${STYLE}." 1>&2
exit 1
