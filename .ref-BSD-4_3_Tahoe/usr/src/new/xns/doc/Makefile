# $Header: Makefile,v 1.3 87/01/27 07:30:28 jqj Exp $
TROFF= ditroff
TRFLAGS= -me -t
RCSREV=
RCSNAME=current


install: troff 

troff: courier.tbl.me
	tbl courier.tbl.me | ${TROFF} ${TRFLAGS}  > troff

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -N${RCSNAME} -q RCS/*

clean:
	-rm -f *.CKP *.BAK troff
