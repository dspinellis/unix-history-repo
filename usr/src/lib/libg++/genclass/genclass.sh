#!/bin/sh
# shell script for generating classes from prototypes
#
# usage: genclass [-2] type1 {ref, val} [type2 {ref, val}] proto [out-prefix]


# search in standard g++ prototype directory & in current

PROTODIR=${PROTODIR-/usr/local/lib/g++-include/gen}
CURRENTDIR=`pwd`

N="1"
T2=""
T2ACC=""

case $1 in
 -2) N="2"; shift;;
  *) ;;
esac

T1=$1;
T1NAME=$T1.;
T1SEDNAME=$T1;

case $2 in
 ref) T1ACC="\&";;
 val) T1ACC=" ";;
 *)   echo "Must specify type1 access: ref or val"; exit 1;;
esac

case $N in
 2) T2=$3;
    T2NAME=$T2.; T2SEDNAME=$T2;
    case $4 in
     ref) T2ACC="\&";;
     val) T2ACC=" ";;
     *)   echo "Must specify type2 access: ref or val"; exit 1;;
    esac;
    CLASS=$5;;
 *) CLASS=$3;;
esac

REPLACEPREFIX="N"
DFLTPREFIX=$T1NAME$T2NAME

case $# in
  3) PREFIX=$DFLTPREFIX;;
  5) PREFIX=$DFLTPREFIX;;
  4) PREFIX=$4; REPLACEPREFIX="Y";;
  6) PREFIX=$6; REPLACEPREFIX="Y";;
  *) echo "bad arguments"; exit 1 ;;
esac

HSRC=$CLASS.hP
CCSRC=$CLASS.ccP
HOUT=$PREFIX$CLASS.h;
CCOUT=$PREFIX$CLASS.cc ;


# .h and .cc parts done separately in case only a .h


if   test -f $CURRENTDIR/$HSRC
then HSRC=$CURRENTDIR/$HSRC
elif test -f $PROTODIR/$HSRC
then HSRC=$PROTODIR/$HSRC
else echo "genclass: $HSRC: no such file"; exit 1;
fi

CASES=$N$REPLACEPREFIX

case $CASES in
    2Y) sed < $HSRC > $HOUT -e "s/<T>/$T1/g" -e "s/<T&>/$T1$T1ACC/g" -e "s/<C>/$T2/g" -e "s/<C&>/$T2$T2ACC/g" -e "s/$T1SEDNAME\.$T2SEDNAME\./$PREFIX/g" -e "s/$T1SEDNAME\./$PREFIX/g" -e "s/$T2SEDNAME\./$PREFIX/g" ;;
    2N) sed < $HSRC > $HOUT -e "s/<T>/$T1/g" -e "s/<T&>/$T1$T1ACC/g" -e "s/<C>/$T2/g" -e "s/<C&>/$T2$T2ACC/g" ;;
    1Y) sed < $HSRC > $HOUT -e "s/<T>/$T1/g" -e "s/<T&>/$T1$T1ACC/g" -e "s/$T1SEDNAME\./$PREFIX/g" ;;
    *) sed < $HSRC > $HOUT -e "s/<T>/$T1/g" -e "s/<T&>/$T1$T1ACC/g";;
esac

if   test -f $CURRENTDIR/$CCSRC
then CCSRC=$CURRENTDIR/$CCSRC
elif test -f $PROTODIR/$CCSRC
then CCSRC=$PROTODIR/$CCSRC
else echo "genclass warning: class has a .h but no .cc file"; exit 0;
fi

case $CASES in
    2Y) sed < $CCSRC > $CCOUT -e "s/<T>/$T1/g" -e "s/<T&>/$T1$T1ACC/g" -e "s/<C>/$T2/g" -e "s/<C&>/$T2$T2ACC/g" -e "s/$T1SEDNAME\.$T2SEDNAME\./$PREFIX/g" -e "s/$T1SEDNAME\./$PREFIX/g" -e "s/$T2SEDNAME\./$PREFIX/g" ;;
    2N) sed < $CCSRC > $CCOUT -e "s/<T>/$T1/g" -e "s/<T&>/$T1$T1ACC/g" -e "s/<C>/$T2/g" -e "s/<C&>/$T2$T2ACC/g" ;;
    1Y) sed < $CCSRC > $CCOUT -e "s/<T>/$T1/g" -e "s/<T&>/$T1$T1ACC/g" -e "s/$T1SEDNAME\./$PREFIX/g" ;;
    *) sed < $CCSRC > $CCOUT -e "s/<T>/$T1/g" -e "s/<T&>/$T1$T1ACC/g";;
esac

exit 0
