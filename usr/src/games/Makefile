#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
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
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.13 (Berkeley) %G%
#
DESTDIR=

SUBDIR=	adventure arithmetic atc backgammon battlestar bcd \
	boggle canfield cribbage dm doctor factor fish \
	fortune gnuchess hack hangman hunt larn mille monop number \
	phantasia quiz rain robots rogue sail snake trek wargames \
	worm worms wump games.${MACHINE}

all clean cleandir depend install lint tags: FRC
	@-for i in ${SUBDIR}; do \
		(echo "==> $$i"; \
		    cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} $@); done

${SUBDIR}: FRC
	cd $@; make ${MFLAGS} DESTDIR=${DESTDIR}

FRC:
