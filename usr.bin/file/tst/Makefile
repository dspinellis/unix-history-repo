# Make up some fake test files that are easily produced.
# By no means an exhaustive test!
# @(#) $Header: Makefile,v 1.4 87/11/07 12:46:09 ian Exp $
all:	ar cmd emp i t x
ar:
	echo '<ar> fake fake fake' >$@
	echo 070707 fake fake fake >$@.asc
	echo '!<arch>.__.SYMDEF fake fake fake' >$@.ranlib
	echo - -h- >$@.swt
cmd:
	echo '#! /bin/sh' >$@
	echo '#!/bin/sh' >c.sh2
	echo '#! /bin/csh' >c.csh1
	echo '#!/bin/csh' >c.csh2
	echo '#! /bin/awk' >c.awk1
	echo '#!/bin/awk' >c.awk2
	echo '#! /' >c.misc1
	echo '#!/' >c.misc2
	echo ': ' >c.broken
emp:
	touch $@
i:
	echo '@document(language impress)fake fake' >$@
	echo '@document(language diablo)fake fake' >$@.d
t:
	rm -f $@
	tar cvf $@ *
x:
	echo 'Interpress/Xerox fake fake fake' >$@

clean:
	rm -f [a-z]*
