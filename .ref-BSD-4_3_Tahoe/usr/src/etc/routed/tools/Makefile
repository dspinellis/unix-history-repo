#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific written prior permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.3 (Berkeley) 6/11/88
#
CFLAGS=	-O
SRCS=	query.c trace.c

all: query trace

# test programs and utilities
query:
	${CC} ${CFLAGS} -o $@ query.c

trace:
	${CC} ${CFLAGS} -o $@ trace.c

install: FRC

clean: FRC
	rm -f core query trace tags

depend: FRC
	mkdep -p ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

query: query.c /usr/include/sys/param.h /usr/include/sys/types.h
query: /usr/include/signal.h /usr/include/machine/trap.h
query: /usr/include/machine/machparam.h /usr/include/machine/endian.h
query: /usr/include/sys/protosw.h /usr/include/sys/socket.h
query: /usr/include/sys/time.h /usr/include/time.h /usr/include/netinet/in.h
query: /usr/include/errno.h /usr/include/stdio.h /usr/include/netdb.h
query: /usr/include/protocols/routed.h
trace: trace.c /usr/include/sys/param.h /usr/include/sys/types.h
trace: /usr/include/signal.h /usr/include/machine/trap.h
trace: /usr/include/machine/machparam.h /usr/include/machine/endian.h
trace: /usr/include/sys/protosw.h /usr/include/sys/socket.h
trace: /usr/include/netinet/in.h /usr/include/errno.h /usr/include/stdio.h
trace: /usr/include/netdb.h /usr/include/protocols/routed.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
