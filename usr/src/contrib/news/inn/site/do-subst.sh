#! /bin/sh
##  $Revision: 1.3 $
##  Run subst over a list of files.  Get files in and out of source control
##  if present.

##  Assume no source control.
MODE=none
PLAIN="$*"
CONTROLLED=
BASEDIRNAME=`basename \`pwd\``

if [ -d RCS -a ! -d SCCS ] ; then
    # Find out which files are under RCS control.
    MODE=rcs
    PLAIN=
    for I
    do
	if [ -f RCS/${I},v ] ; then
	    CONTROLLED="${CONTROLLED} ${I}"
	else
	    PLAIN="${PLAIN} ${I}"
	fi
    done
    test -n "${CONTROLLED}" && co -l -q ${CONTROLLED}
fi
if [ -d SCCS -a ! -d RCS ] ; then
    # Find out which files are under SCCS control.
    MODE=sccs
    PLAIN=
    for I
    do
	if [ -f SCCS/s.${I} ] ; then
	    CONTROLLED="${CONTROLLED} ${I}"
	else
	    PLAIN="${PLAIN} ${I}"
	fi
    done
    test -n "${CONTROLLED}" && sccs get -e ${CONTROLLED}
fi

##  Make an unmodified copy of all files.
for I in ${CONTROLLED} ${PLAIN}; do
    rm -f bak.${I}
    cp ${I} bak.${I}
done

##  Make sure we can write all non-controlled files.
test ! -z "${PLAIN}" && chmod u+w,g+w ${PLAIN}

##  Make the list of files, run config over it.
LISTOFFILES=../${BASEDIRNAME}/files.$$
for I in ${PLAIN} ${CONTROLLED} ; do
    echo ../${BASEDIRNAME}/${I}
done >${LISTOFFILES}
( cd ../config; make quiet FILE=${LISTOFFILES} )
rm -f ${LISTOFFILES}

# check which files we really changed
for I in ${CONTROLLED} ${PLAIN} ; do
    if cmp -s ${I} bak.${I}; then
	##  No change was made, restore things.
	case ${MODE} in
	sccs)
	    test -f SCCS/s.${I} && sccs unedit ${I}
	    ;;
	rcs)
	    test -f $RCS/${I},v && rcs -q -u ${I} && ci -u -q -f ${I}
	    ;;
	esac
    else
	##  Check files back into source control, if we have to.
	case ${MODE} in
	rcs)
	    echo 'Ran subst from Makefile' | ci -u -q -f ${I}
	    ;;
	sccs)
	    echo 'Ran subst from Makefile' | sccs delget -s ${I}
	    ;;
	esac
    fi
    rm -f bak.${I}
done

exit 0
