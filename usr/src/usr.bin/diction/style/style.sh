#!/bin/sh -
#
# %sccs.include.proprietary.sh%
#
#	@(#)style.sh	4.9 (Berkeley) %G%
#

L=/usr/libexec
B=/usr/bin
echo " " $*
sflag=-s
eflag=
Pflag=
nflag=
lflag=
lcon=
rflag=
rcon=
mflag=-me
mlflag=-ml
kflag=
for i in $*
do case $i in
-r) rflag=-r; shift; rcon=$1;shift;continue;;
-l)lflag=-l; shift; lcon=$1;shift;continue;;
-mm) mflag=-mm;shift;continue;;
-ms) mflag=-ms;shift;continue;;
-me) mflag=-me;shift;continue;;
-ma) mflag=-ma;shift;continue;;
-li|-ml) mlflag=-ml;shift;continue;;
+li|-tt)mlflag=;shift;continue;;
-p) sflag=-p;shift;continue;;
-a) sflag=-a;shift;continue;;
-e) eflag=-e;shift;continue;;
-P) Pflag=-P;shift;continue;;
-n) nflag=-n;shift;continue;;
-N) nflag=-N;shift;continue;;
-k) kflag=-k;shift;continue;;
-flags) echo $0 "[-flags] [-r num] [-l num] [-e] [-p] [-n] [-N] [-a] [-P] [-mm|-ms] [-li|+li] [file ...]";exit;;
-*) echo unknown style flag $i; exit;;
*) break;;
esac
done
$B/deroff $kflag $mflag $mlflag $* | $L/style1 | $L/style2 | $L/style3 $rflag $rcon $lflag $lcon $sflag $nflag $eflag $Pflag
