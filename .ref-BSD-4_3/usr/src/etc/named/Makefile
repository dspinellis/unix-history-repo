
#	@(#)Makefile	4.3	(Berkeley)	86/05/30
#
# Copyright (c) 1986 Regents of the University of California
#	All Rights Reserved
#
# Berkeley Internet Name Domain server
#

DESTDIR=
CFLAGS= -O 
TAGSFILE= tags
INCPATH=
LDFLAGS= 
LIBS=

# The program itself
#
PROG=named	

# Sources
#
SRCS= db_dump.c db_load.c db_lookup.c db_reload.c db_save.c db_update.c \
	ns_forw.c ns_init.c ns_main.c ns_maint.c ns_req.c ns_resp.c \
	cistrcmp.c version.c

# Objects
#
OBJS= db_dump.o db_load.o db_lookup.o db_reload.o db_save.o db_update.o \
	ns_forw.o ns_init.o ns_main.o ns_maint.o ns_req.o ns_resp.o \
	cistrcmp.o version.o

# Header files
#
HDRS=ns.h db.h	

${PROG}: ${OBJS} ${HDRS}
	cc ${LDFLAGS} -o ${PROG} ${OBJS} ${LIBS}

${OBJS}:
	cc ${CFLAGS} ${INCPATH} -c $*.c

install: ${PROG}
	install -s ${PROG} ${DESTDIR}/etc/${PROG}

tags:
	ctags -f ${TAGSFILE} ${SRCS}

clean:
	rm -f a.out core ${OBJS} ${PROG}

lint:
	lint ${SRCS}

depend:
	cc -M ${INCPATH} ${SRCS} | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it


db_dump.o: db_dump.c /usr/include/sys/types.h /usr/include/sys/time.h
db_dump.o: /usr/include/time.h /usr/include/netinet/in.h /usr/include/netdb.h
db_dump.o: /usr/include/stdio.h /usr/include/syslog.h
db_dump.o: /usr/include/arpa/nameser.h ./ns.h /usr/include/strings.h
db_dump.o: /usr/include/arpa/inet.h ./db.h
db_load.o: db_load.c /usr/include/sys/types.h /usr/include/sys/time.h
db_load.o: /usr/include/time.h /usr/include/netinet/in.h /usr/include/stdio.h
db_load.o: /usr/include/syslog.h /usr/include/ctype.h /usr/include/netdb.h
db_load.o: /usr/include/arpa/nameser.h ./ns.h /usr/include/strings.h
db_load.o: /usr/include/arpa/inet.h ./db.h
db_lookup.o: db_lookup.c /usr/include/sys/types.h /usr/include/stdio.h
db_lookup.o: /usr/include/arpa/nameser.h ./db.h
db_reload.o: db_reload.c /usr/include/sys/types.h /usr/include/netinet/in.h
db_reload.o: /usr/include/stdio.h /usr/include/arpa/nameser.h ./ns.h
db_reload.o: /usr/include/strings.h /usr/include/arpa/inet.h ./db.h
db_save.o: db_save.c /usr/include/sys/types.h /usr/include/stdio.h
db_save.o: /usr/include/syslog.h ./db.h
db_update.o: db_update.c /usr/include/sys/types.h /usr/include/sys/time.h
db_update.o: /usr/include/time.h /usr/include/stdio.h /usr/include/syslog.h
db_update.o: /usr/include/arpa/nameser.h ./db.h
ns_forw.o: ns_forw.c /usr/include/stdio.h /usr/include/sys/types.h
ns_forw.o: /usr/include/sys/socket.h /usr/include/sys/time.h
ns_forw.o: /usr/include/time.h /usr/include/netinet/in.h /usr/include/syslog.h
ns_forw.o: /usr/include/arpa/nameser.h ./ns.h /usr/include/strings.h
ns_forw.o: /usr/include/arpa/inet.h ./db.h
ns_init.o: ns_init.c /usr/include/sys/types.h /usr/include/sys/socket.h
ns_init.o: /usr/include/sys/time.h /usr/include/time.h
ns_init.o: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/errno.h
ns_init.o: /usr/include/signal.h /usr/include/syslog.h /usr/include/ctype.h
ns_init.o: /usr/include/arpa/nameser.h ./ns.h /usr/include/strings.h
ns_init.o: /usr/include/arpa/inet.h ./db.h
ns_main.o: ns_main.c /usr/include/sys/param.h /usr/include/machine/machparam.h
ns_main.o: /usr/include/signal.h /usr/include/sys/types.h
ns_main.o: /usr/include/sys/file.h /usr/include/sys/time.h /usr/include/time.h
ns_main.o: /usr/include/sys/wait.h /usr/include/sys/resource.h
ns_main.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ns_main.o: /usr/include/sys/ttydev.h /usr/include/sys/socket.h
ns_main.o: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/syslog.h
ns_main.o: /usr/include/errno.h /usr/include/signal.h
ns_main.o: /usr/include/arpa/nameser.h /usr/include/arpa/inet.h ./ns.h
ns_main.o: /usr/include/strings.h /usr/include/arpa/inet.h ./db.h
ns_maint.o: ns_maint.c /usr/include/sys/types.h /usr/include/sys/socket.h
ns_maint.o: /usr/include/sys/time.h /usr/include/time.h
ns_maint.o: /usr/include/netinet/in.h /usr/include/stdio.h
ns_maint.o: /usr/include/syslog.h /usr/include/signal.h /usr/include/errno.h
ns_maint.o: /usr/include/arpa/nameser.h ./ns.h /usr/include/strings.h
ns_maint.o: /usr/include/arpa/inet.h ./db.h
ns_req.o: ns_req.c /usr/include/stdio.h /usr/include/sys/types.h
ns_req.o: /usr/include/sys/uio.h /usr/include/sys/time.h /usr/include/time.h
ns_req.o: /usr/include/netinet/in.h /usr/include/syslog.h
ns_req.o: /usr/include/sys/file.h /usr/include/arpa/nameser.h ./ns.h
ns_req.o: /usr/include/strings.h /usr/include/arpa/inet.h ./db.h
ns_resp.o: ns_resp.c /usr/include/stdio.h /usr/include/sys/types.h
ns_resp.o: /usr/include/sys/time.h /usr/include/time.h
ns_resp.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
ns_resp.o: /usr/include/syslog.h /usr/include/arpa/nameser.h ./ns.h
ns_resp.o: /usr/include/strings.h /usr/include/arpa/inet.h ./db.h
cistrcmp.o: cistrcmp.c
version.o: version.c
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
