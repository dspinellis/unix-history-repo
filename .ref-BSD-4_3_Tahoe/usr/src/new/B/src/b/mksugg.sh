: 'Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984.'
: '$Header: mksugg.sh,v 1.2 84/07/12 00:02:42 timo Exp $'
:
: 'Create a list of commands digestable by the B editor as suggestion list.'
:
PATH=/bin:/usr/bin:/usr/ucb:/usr/local

sed "/^HOW'TO /!d
	s/	/ /g
	s/^HOW'TO  *//
	s/ *:.*//
	s/ [a-z][^ ]*/ ?/g
	s/   */ /g" \'* >.Bed_sugg 2>&-

if test ! -s .Bed_sugg
then
	: remove if empty
	rm -f .Bed_sugg >&- 2>&-
fi
