#	@(#)Makefile	7.1 (Berkeley) %G%
#
# from: $Header: Makefile,v 1.11 92/06/03 21:48:05 leres Exp $ (LBL)

# Location of tags file
TAGS=	../../compile/GENERIC/tags
TAGS1=	../compile/GENERIC/tags1

# Directories in which to place sparc tags links
DSPARC=	dev fpu include rcons sbus sparc sunos

all:
	@echo '"make links" only'

links:
	-for i in ${DSPARC}; do \
	    (cd $$i && rm -f tags; ln -s ${TAGS}); done
	rm -f tags ; ln -s ${TAGS1} tags
