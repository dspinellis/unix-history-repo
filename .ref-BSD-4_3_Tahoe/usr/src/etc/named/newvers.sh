#!/bin/sh -
#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)newvers.sh	4.5 (Berkeley) 7/9/88
#
if [ ! -r version ]
then
	echo 0 > version
fi
touch version
rm -f version.[oc]
v=`cat version` u=${USER-root} d=`pwd` h=`hostname` t=`date`
sed -e "s|%VERSION%|#${v}: ${t}|" -e "s|%WHOANDWHERE%|${u}@${h}:${d}|" \
	< Version.c >version.c
/bin/echo `expr ${v} + 1` > version
