#!/bin/sh -
#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)yyfix.sh	5.1 (Berkeley) %G%
#
OLDYACC="yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef"
NEWYACC="yylhs yylen yydefred yydgoto yysindex yyrindex yygindex \
	 yytable yycheck"

file=$1
>$file
shift

if [ $# -eq 0 ] ; then
	if grep yylhs y.tab.c > /dev/null ; then
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
