#! /bin/sh
##  $Revision: 1.5 $
##
##  Safely get a file from the samples directory.  Usage:
##	getsafe <sample> <localfile>
case $# in
2)
    ;;
*)
    echo "Can't get INN sample file:  wrong number of arguments." 1>&2
    exit 1
    ;;
esac

SRC=$1
DEST=$2

##  Try RCS.
if [ -f RCS/${DEST},v ] ; then
    echo "Note: ${SRC} has changed; please compare."
    test -f ${DEST} && exit 0
    exec co -q ${DEST}
fi

##  Try SCCS.
if [ -f SCCS/s.${DEST} ] ; then
    echo "Note: ${SRC} has changed; please compare."
    test -f ${DEST} && exit 0
    exec sccs get -s ${DEST}
fi

##  File exist locally?
if [ -f ${DEST} ] ; then
    echo "${SRC} has changed; please update ${DEST}"
    exit 1
fi

echo Using sample version of ${DEST}
cp ${SRC} ${DEST}

exit 0
