# Copyright (c) 1982 Regents of the University of California */
#
# @(#)Makefile	4.1 %G%
#
BINDIR=	/bin
CFLAGS=	-O
CC=	/bin/cc

all:	${DESTDIR}${BINDIR}/ar ${DESTDIR}${BINDIR}/cat \
	${DESTDIR}${BINDIR}/cc ${DESTDIR}${BINDIR}/chgrp \
	${DESTDIR}${BINDIR}/chmod ${DESTDIR}${BINDIR}/cmp \
	${DESTDIR}${BINDIR}/cp ${DESTDIR}${BINDIR}/date \
	${DESTDIR}${BINDIR}/dd ${DESTDIR}${BINDIR}/df \
	${DESTDIR}${BINDIR}/du ${DESTDIR}${BINDIR}/echo \
	${DESTDIR}${BINDIR}/ed ${DESTDIR}${BINDIR}/expr \
	${DESTDIR}${BINDIR}/grep \
	${DESTDIR}${BINDIR}/kill ${DESTDIR}${BINDIR}/ld \
	${DESTDIR}${BINDIR}/ln ${DESTDIR}${BINDIR}/login \
	${DESTDIR}${BINDIR}/ls ${DESTDIR}${BINDIR}/mail \
	${DESTDIR}${BINDIR}/mkdir ${DESTDIR}${BINDIR}/mt \
	${DESTDIR}${BINDIR}/mv ${DESTDIR}${BINDIR}/nice \
	${DESTDIR}${BINDIR}/nm ${DESTDIR}${BINDIR}/od \
	${DESTDIR}${BINDIR}/passwd ${DESTDIR}${BINDIR}/pr \
	${DESTDIR}${BINDIR}/ps ${DESTDIR}${BINDIR}/pwd \
	${DESTDIR}${BINDIR}/rm ${DESTDIR}${BINDIR}/rmail \
	${DESTDIR}${BINDIR}/rmdir ${DESTDIR}${BINDIR}/size \
	${DESTDIR}${BINDIR}/strip ${DESTDIR}${BINDIR}/stty \
	${DESTDIR}${BINDIR}/su ${DESTDIR}${BINDIR}/sync \
	${DESTDIR}${BINDIR}/tar ${DESTDIR}${BINDIR}/tee \
	${DESTDIR}${BINDIR}/test ${DESTDIR}${BINDIR}/time \
	${DESTDIR}${BINDIR}/wall ${DESTDIR}${BINDIR}/who \
	${DESTDIR}${BINDIR}/write ${DESTDIR}${BINDIR}/adb \
	${DESTDIR}${BINDIR}/as ${DESTDIR}${BINDIR}/awk \
	${DESTDIR}${BINDIR}/csh ${DESTDIR}${BINDIR}/make \
	${DESTDIR}${BINDIR}/sed ${DESTDIR}${BINDIR}/sh \
	${DESTDIR}${BINDIR}/diff

${DESTDIR}${BINDIR}/ar: ar.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/ar ar.c

${DESTDIR}${BINDIR}/cat: cat.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/cat cat.c

${DESTDIR}${BINDIR}/cc: cc.c
	${CC} ${CFLAGS} -o cc cc.c
	rm -f ${DESTDIR}${BINDIR}/cc
	mv cc ${DESTDIR}${BINDIR}/cc

${DESTDIR}${BINDIR}/chgrp: chgrp.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/chgrp chgrp.c
	chown root ${DESTDIR}${BINDIR}/chgrp
	chmod 4755 ${DESTDIR}${BINDIR}/chgrp

${DESTDIR}${BINDIR}/chmod: chmod.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/chmod chmod.c

${DESTDIR}${BINDIR}/cmp: cmp.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/cmp cmp.c

${DESTDIR}${BINDIR}/cp: cp.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/cp cp.c

${DESTDIR}${BINDIR}/date: date.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/date date.c

${DESTDIR}${BINDIR}/dd: dd.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/dd dd.c

${DESTDIR}${BINDIR}/df: df.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/df df.c
	/etc/chown root ${DESTDIR}${BINDIR}/df
	chmod 4755 ${DESTDIR}${BINDIR}/df

${DESTDIR}${BINDIR}/du: du.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/du du.c

${DESTDIR}${BINDIR}/echo: echo.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/echo echo.c

${DESTDIR}${BINDIR}/ed: ed.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/ed ed.c
	rm -f /bin/e
	ln /bin/ed /bin/e

${DESTDIR}${BINDIR}/expr: expr.y
	yacc expr.y
	${CC} ${CFLAGS} -w -o ${DESTDIR}${BINDIR}/expr y.tab.c
	rm -f y.tab.[ch]

${DESTDIR}${BINDIR}/grep: grep.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/grep grep.c

${DESTDIR}${BINDIR}/kill: kill.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/kill kill.c

${DESTDIR}${BINDIR}/ld: ld.c
	${CC} ${CFLAGS} -o ld ld.c
	rm -f ${DESTDIR}${BINDIR}/ld
	mv ld ${DESTDIR}${BINDIR}/ld

${DESTDIR}${BINDIR}/ln: ln.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/ln ln.c

${DESTDIR}${BINDIR}/login: login.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/login login.c
	/etc/chown root ${DESTDIR}${BINDIR}/login
	chmod 4755 ${DESTDIR}${BINDIR}/login

${DESTDIR}${BINDIR}/ls: ls.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/ls ls.c

${DESTDIR}${BINDIR}/mail: mail.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/mail mail.c
	/etc/chown root ${DESTDIR}${BINDIR}/mail
	chmod 4755 ${DESTDIR}${BINDIR}/mail

${DESTDIR}${BINDIR}/mkdir: mkdir.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/mkdir mkdir.c
	/etc/chown root ${DESTDIR}${BINDIR}/mkdir
	chmod 4755 ${DESTDIR}${BINDIR}/mkdir

${DESTDIR}${BINDIR}/mt: mt.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/mt mt.c

${DESTDIR}${BINDIR}/mv: mv.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/mv mv.c
	/etc/chown root ${DESTDIR}${BINDIR}/mv
	chmod 4755 ${DESTDIR}${BINDIR}/mv

${DESTDIR}${BINDIR}/nice: nice.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/nice nice.c

${DESTDIR}${BINDIR}/nm: nm.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/nm nm.c

${DESTDIR}${BINDIR}/od: od.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/od od.c

${DESTDIR}${BINDIR}/passwd: passwd.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/passwd passwd.c
	/etc/chown root ${DESTDIR}${BINDIR}/passwd
	chmod 4755 ${DESTDIR}${BINDIR}/passwd

${DESTDIR}${BINDIR}/pr: pr.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/pr pr.c

${DESTDIR}${BINDIR}/ps: ps.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/ps ps.c -lm

${DESTDIR}${BINDIR}/pwd: pwd.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/pwd pwd.c

${DESTDIR}${BINDIR}/rm: rm.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/rm rm.c

${DESTDIR}${BINDIR}/rmail: rmail.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/rmail rmail.c

${DESTDIR}${BINDIR}/rmdir: rmdir.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/rmdir rmdir.c
	/etc/chown root ${DESTDIR}${BINDIR}/rmdir
	chmod 4755 ${DESTDIR}${BINDIR}/rmdir

${DESTDIR}${BINDIR}/size: size.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/size size.c

${DESTDIR}${BINDIR}/strip: strip.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/strip strip.c

${DESTDIR}${BINDIR}/stty: stty.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/stty stty.c

${DESTDIR}${BINDIR}/su: su.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/su su.c
	/etc/chown root ${DESTDIR}${BINDIR}/su
	chmod 4755 ${DESTDIR}${BINDIR}/su

${DESTDIR}${BINDIR}/sync: sync.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/sync sync.c

${DESTDIR}${BINDIR}/tar: tar.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/tar tar.c

${DESTDIR}${BINDIR}/tee: tee.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/tee tee.c

${DESTDIR}${BINDIR}/test: test.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/test test.c
	rm -f /bin/[
	ln /bin/test /bin/[

${DESTDIR}${BINDIR}/time: time.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/time time.c

${DESTDIR}${BINDIR}/wall: wall.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/wall wall.c

${DESTDIR}${BINDIR}/who: who.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/who who.c

${DESTDIR}${BINDIR}/write: write.c
	${CC} ${CFLAGS} -o ${DESTDIR}${BINDIR}/write write.c

${DESTDIR}${BINDIR}/adb: adb/adb
	cd adb; make install
adb/adb:
	cd adb; make adb

${DESTDIR}${BINDIR}/as: as/as
	cd as; make install
as/as:
	cd as; make as

${DESTDIR}${BINDIR}/awk: awk/awk
	cd awk; make install
awk/awk:
	cd awk; make awk

${DESTDIR}${BINDIR}/csh: csh/csh
	rm -f ${DESTDIR}${BINDIR}/csh
	cd csh; make install
csh/csh:
	cd csh; make csh

${DESTDIR}${BINDIR}/make: make/make
	rm -f ${DESTDIR}${BINDIR}/make
	cd make; make install
make/make:
	cd make; make make

${DESTDIR}${BINDIR}/sed: sed/sed
	cd sed; make install
sed/sed:
	cd sed; make sed

${DESTDIR}${BINDIR}/sh: sh/sh
	cd sh; make install
sh/sh:
	cd sh; make sh

${DESTDIR}${BINDIR}/diff: diff/diff
	cd diff; make install
diff/diff:
	cd diff; make all
