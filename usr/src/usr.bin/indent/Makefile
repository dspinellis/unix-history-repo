#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
DESTDIR=
CFLAGS=	-O
SRCS=	indent.c io.c lexi.c parse.c pr_comment.c args.c
OBJS=	indent.o io.o lexi.o parse.o pr_comment.o args.o

indent: ${OBJS}
	cc ${CFLAGS} -o indent ${OBJS}

clean:
	rm -f ${OBJS} indent core

.DEFAULT:
	sccs get $<

install: indent
	install -s indent ${DESTDIR}/usr/ucb/indent

depend:
	cc -M ${SRCS} | \
	    awk '{ if ($$1 != prev) { if (rec != "") print rec; \
		rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec }' >makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >> eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

indent.o: indent.c ./indent_globs.h /usr/include/stdio.h ./indent_codes.h
io.o: io.c ./indent_globs.h /usr/include/stdio.h
lexi.o: lexi.c ./indent_globs.h /usr/include/stdio.h ./indent_codes.h
lexi.o: /usr/include/ctype.h
parse.o: parse.c ././indent_globs.h /usr/include/stdio.h ././indent_codes.h
pr_comment.o: pr_comment.c ./indent_globs.h /usr/include/stdio.h
args.o: args.c ./indent_globs.h /usr/include/stdio.h /usr/include/sys/types.h
args.o: /usr/include/ctype.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
