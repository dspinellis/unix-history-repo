# Makefile,v 3.1 1993/07/06 01:09:44 jbj Exp

all:

tar:
	cwd=`pwd` ; dir=`basename $$cwd` ; set -x; \
	    (cd .. ; tar cFFhf - $$dir) | compress > /tmp/$$dir.tar.Z
