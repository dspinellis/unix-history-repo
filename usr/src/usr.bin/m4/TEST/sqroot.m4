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
#	@(#)sqroot.m4	5.1 (Berkeley) %G%
#

define(square_root, 
	`ifelse(eval($1<0),1,negative-square-root,
			     `square_root_aux($1, 1, eval(($1+1)/2))')')
define(square_root_aux,
	`ifelse($3, $2, $3,
		$3, eval($1/$2), $3,
		`square_root_aux($1, $3, eval(($3+($1/$3))/2))')')
