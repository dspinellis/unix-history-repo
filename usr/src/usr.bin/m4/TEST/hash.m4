#
# Copyright (c) 1989 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Ozan Yigit.
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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)hash.m4	5.1 (Berkeley) %G%
#

dnl	This probably will not run on any m4 that cannot
dnl	handle char constants in eval.
dnl
changequote(<,>) define(HASHVAL,99) dnl
define(hash,<eval(str(substr($1,1),0)%HASHVAL)>) dnl
define(str,
	<ifelse($1,",$2,
		<str(substr(<$1>,1),<eval($2+'substr($1,0,1)')>)>)
	>) dnl
define(KEYWORD,<$1,hash($1),>) dnl
define(TSTART,
<struct prehash {
	char *keyword;
	int   hashval;
} keytab[] = {>) dnl
define(TEND,<	"",0
};>) dnl
