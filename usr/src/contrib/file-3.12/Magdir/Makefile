#
# Makefile for /etc/magic file for Ian Darwin's file(1) command.
# Copyright (c) 1989 Ian F. Darwin, Toronto, Canada.
# $Id: Makefile,v 1.8 1993/01/05 13:22:31 ian Exp $
#
#
SHELL		= /bin/sh
HEADER		= Header
LOCALSTUFF	= Localstuff

ALL		=  $(HEADER) $(LOCALSTUFF) [a-z]*

install:	../magic
../magic:	$(ALL)
		echo "# Magic file created `date` by `who am i`" > $@
		cat $(ALL) >> $@
