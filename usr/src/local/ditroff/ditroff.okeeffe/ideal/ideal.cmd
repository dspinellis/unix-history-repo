#!/bin/sh
#
#	ideal.cmd	(CWI)	1.1	85/03/01
#
IDDIR=/usr/lib/ditroff/ideal
filter=t
iflags=
for i
do
	case $i in
	-p*)	filter=p
		shift ;;
	-4*)	filter=4
		shift ;;
	-n*)	filter=n
		shift ;;
	-a*)	filter=a
		shift ;;
	-t*)	filter=t
		shift ;;
	-v*)	filter=v
		shift ;;
	-T202*)	filter=2
		shift ;;
	-Taps*)	filter=a
		shift ;;
	-s*)	sflags=s
		shift ;;
	-q*)	fflags=-q
		shift ;;
	*)	iflags="$iflags $i"
		shift ;;
	esac
done
case $filter in
	p)	$IDDIR/ideal $iflags | $IDDIR/idfilt/pfilt ;;
	v)	$IDDIR/ideal $iflags | $IDDIR/idfilt/vfilt ;;
	4)	$IDDIR/ideal $iflags >/tmp/id`getuid`
		$IDDIR/idfilt/4filt /tmp/id`getuid`
		rm /tmp/id`getuid` ;;
	n)	$IDDIR/ideal $iflags ;;
	t)	case $sflags in
		s)	$IDDIR/ideal $iflags | $IDDIR/idfilt/idsort | $IDDIR/idfilt/tfilt $fflags ;;
		*)	$IDDIR/ideal $iflags | $IDDIR/idfilt/tfilt $fflags ;;
		esac ;;
	a)	case $sflags in
		s)	$IDDIR/ideal $iflags | $IDDIR/idfilt/idsort | $IDDIR/idfilt/apsfilt $fflags ;;
		*)	$IDDIR/ideal $iflags | $IDDIR/idfilt/apsfilt $fflags ;;
		esac ;;
	2)	case $sflags in
		s)	$IDDIR/ideal $iflags | $IDDIR/idfilt/idsort | $IDDIR/idfilt/202filt $fflags ;;
		*)	$IDDIR/ideal $iflags | $IDDIR/idfilt/202filt $fflags ;;
		esac ;;
esac
