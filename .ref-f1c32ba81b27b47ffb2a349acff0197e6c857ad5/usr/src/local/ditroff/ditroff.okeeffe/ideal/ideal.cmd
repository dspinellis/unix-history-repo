#!/bin/sh
#
#	ideal.cmd	(CWI)	1.4	87/04/01
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
	-Tver*)	filter=v
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
	p)	$IDDIR/ideal $iflags | $IDDIR/pfilt ;;
	4)	$IDDIR/ideal $iflags >/tmp/id$$
		$IDDIR/4filt /tmp/id$$
		rm /tmp/id$$ ;;
	n)	$IDDIR/ideal $iflags ;;
	t)	case $sflags in
		s)	$IDDIR/ideal $iflags | $IDDIR/idsort | $IDDIR/tfilt $fflags ;;
		*)	$IDDIR/ideal $iflags | $IDDIR/tfilt $fflags ;;
		esac ;;
	v)	case $sflags in
		s)	$IDDIR/ideal $iflags | $IDDIR/idsort | $IDDIR/vfilt $fflags ;;
		*)	$IDDIR/ideal $iflags | $IDDIR/vfilt $fflags ;;
		esac ;;
	a)	case $sflags in
		s)	$IDDIR/ideal $iflags | $IDDIR/idsort | $IDDIR/apsfilt $fflags ;;
		*)	$IDDIR/ideal $iflags | $IDDIR/apsfilt $fflags ;;
		esac ;;
	2)	case $sflags in
		s)	$IDDIR/ideal $iflags | $IDDIR/idsort | $IDDIR/202filt $fflags ;;
		*)	$IDDIR/ideal $iflags | $IDDIR/202filt $fflags ;;
		esac ;;
esac
