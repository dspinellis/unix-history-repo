#!/bin/sh -
#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that: (1) source distributions retain this entire copyright
# notice and comment, and (2) distributions including binaries display
# the following acknowledgement:  ``This product includes software
# developed by the University of California, Berkeley and its contributors''
# in the documentation or other materials provided with the distribution
# and in all advertising materials mentioning features or use of this
# software. Neither the name of the University nor the names of its
# contributors may be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)yyfix.sh	5.2 (Berkeley) 5/12/90
#
OLDYACC="yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef"
NEWYACC="yylhs yylen yydefred yydgoto yysindex yyrindex yygindex \
	 yytable yycheck"

file=$1
>$file
shift

if [ $# -eq 0 ] ; then
	if grep yylhs y.tab.c > /dev/null ; then
		if grep yyname y.tab.c > /dev/null ; then
			NEWYACC="$NEWYACC yyname"
		fi
		if grep yyrule y.tab.c > /dev/null ; then
			NEWYACC="$NEWYACC yyrule"
		fi
		set $NEWYACC
	else
		set $OLDYACC
	fi
fi

for i
do
ed - y.tab.c << END
/^\(.*\)$i[ 	]*\[]/s//extern \1 $i[];\\
\1 $i []/
.ka
/}/kb
'br $file
'a,.w $file
'a,.d
w
q
END
done
