#!/bin/sh
#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# the Systems Programming Group of the University of Utah Computer
# Science Department.
#
# %sccs.include.redist.sh%
#
#	@(#)cpp.sh	6.4 (Berkeley) %G%
#
# Transitional front end to CCCP to make it behave like (Reiser) CCP:
#	specifies -traditional
#	doesn't search gcc-include
#
cpp="eval /usr/libexec/cpp"
ALST="-traditional -D__GNUC__ -$ "
NSI=no
OPTS=""
INCS="-nostdinc"

for A
do
	case $A in
	-nostdinc)
		NSI=yes
		;;
	-traditional)
		;;
	-I*)
		INCS="$INCS $A"
		;;
	-U__GNUC__)
		ALST=`echo $ALST | sed -e 's/-D__GNUC__//'`
		;;
	-*)
		OPTS="$OPTS '$A'"
		;;
	*)
		if [ $NSI = "no" ]
		then
			INCS="$INCS -I/usr/include"
			NSI=skip
		fi
		$cpp $ALST $INCS $LIBS $CSU $OPTS $A || exit $?
		;;
	esac
done

exit 0
