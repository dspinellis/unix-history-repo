#
# Copyright (c) 1989 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Ozan Yigit.
#
# %sccs.include.redist.sh%
#
#	@(#)string.m4	5.2 (Berkeley) %G%
#

define(string,`integer $1(len(substr($2,1)))
str($1,substr($2,1),0)
data $1(len(substr($2,1)))/EOS/
')

define(str,`ifelse($2,",,data $1(incr($3))/`LET'substr($2,0,1)/
`str($1,substr($2,1),incr($3))')')
