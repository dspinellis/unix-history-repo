#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.3 (Berkeley) 12/12/87
#
MDIR=	/usr/man/cat1
SRCS=	adb.1 addbib.1 apply.1 apropos.1 ar.1 as.1 at.1 atq.1 atrm.1 awk.1 \
	basename.1 bc.1 biff.1 binmail.1 cal.1 calendar.1 cat.1 cb.1 cc.1 \
	cd.1 checknr.1 chgrp.1 chmod.1 clear.1 cmp.1 col.1 colcrt.1 colrm.1 \
	comm.1 compress.1 cp.1 crypt.1 csh.1 ctags.1 date.1 dbx.1 dc.1 dd.1 \
	deroff.1 df.1 diction.1 diff.1 diff3.1 du.1 echo.1 ed.1 \
	efl.1 eqn.1 error.1 ex.1 expand.1 expr.1 f77.1 false.1 \
	file.1 find.1 finger.1 fmt.1 fold.1 fp.1 fpr.1 from.1 fsplit.1 \
	ftp.1 gcore.1 gprof.1 graph.1 grep.1 groups.1 head.1 \
	hostid.1 hostname.1 indent.1 install.1 intro.1 iostat.1 join.1 kill.1 \
	last.1 lastcomm.1 ld.1 learn.1 leave.1 lex.1 lint.1 lisp.1 liszt.1 \
	ln.1 lock.1 logger.1 login.1 look.1 lookbib.1 lorder.1 lpq.1 lpr.1 \
	lprm.1 lptest.1 ls.1 lxref.1 m4.1 mail.1 make.1 man.1 mesg.1 mkdep.1 \
	mkdir.1 mkstr.1 more.1 mset.1 msgs.1 mt.1 mv.1 netstat.1 newaliases.1 \
	nice.1 nm.1 nroff.1 od.1 pagesize.1 passwd.1 pc.1 pdx.1 pi.1 pix.1 \
	plot.1 pmerge.1 pr.1 printenv.1 prof.1 ps.1 \
	ptx.1 pwd.1 px.1 pxp.1 \
	pxref.1 quota.1 ranlib.1 ratfor.1 rcp.1 rdist.1 refer.1 rev.1 \
	rlogin.1 rm.1 rmail.1 rmdir.1 roffbib.1 rsh.1 ruptime.1 rwho.1 sccs.1 \
	script.1 sed.1 sendbug.1 sh.1 size.1 sleep.1 soelim.1 sort.1 \
	sortbib.1 spell.1 spline.1 split.1 strings.1 strip.1 struct.1 stty.1 \
	style.1 su.1 sum.1 symorder.1 sysline.1 systat.1 tabs.1 tail.1 \
	talk.1 tar.1 tbl.1 tc.1 tcopy.1 tee.1 telnet.1 test.1 tftp.1 time.1 \
	tip.1 tk.1 tn3270.1 touch.1 tp.1 tr.1 troff.1 true.1 tset.1 tsort.1 \
	tty.1 ul.1 unifdef.1 uniq.1 units.1 uptime.1 users.1 uucp.1 \
	uuencode.1 uulog.1 uuname.1 uuq.1 uusend.1 uux.1 vacation.1 vgrind.1 \
	vi.1 vlp.1 vmstat.1 w.1 wait.1 wall.1 wc.1 what.1 whatis.1 \
	whereis.1 which.1 who.1 whoami.1 whois.1 window.1 write.1 xsend.1 \
	xstr.1 yacc.1 yes.1
OBJS=	adb.0 addbib.0 apply.0 apropos.0 ar.0 as.0 at.0 atq.0 atrm.0 awk.0 \
	basename.0 bc.0 biff.0 binmail.0 cal.0 calendar.0 cat.0 cb.0 cc.0 \
	cd.0 checknr.0 chgrp.0 chmod.0 clear.0 cmp.0 col.0 colcrt.0 colrm.0 \
	comm.0 compress.0 cp.0 crypt.0 csh.0 ctags.0 date.0 dbx.0 dc.0 dd.0 \
	deroff.0 df.0 diction.0 diff.0 diff3.0 du.0 echo.0 ed.0 \
	efl.0 eqn.0 error.0 ex.0 expand.0 expr.0 f77.0 false.0 \
	file.0 find.0 finger.0 fmt.0 fold.0 fp.0 fpr.0 from.0 fsplit.0 \
	ftp.0 gcore.0 gprof.0 graph.0 grep.0 groups.0 head.0 \
	hostid.0 hostname.0 indent.0 install.0 intro.0 iostat.0 join.0 kill.0 \
	last.0 lastcomm.0 ld.0 learn.0 leave.0 lex.0 lint.0 lisp.0 liszt.0 \
	ln.0 lock.0 logger.0 login.0 look.0 lookbib.0 lorder.0 lpq.0 lpr.0 \
	lprm.0 lptest.0 ls.0 lxref.0 m4.0 mail.0 make.0 man.0 mesg.0 mkdep.0 \
	mkdir.0 mkstr.0 more.0 mset.0 msgs.0 mt.0 mv.0 netstat.0 newaliases.0 \
	nice.0 nm.0 nroff.0 od.0 pagesize.0 passwd.0 pc.0 pdx.0 pi.0 pix.0 \
	plot.0 pmerge.0 pr.0 printenv.0 prof.0 ps.0 \
	ptx.0 pwd.0 px.0 pxp.0 \
	pxref.0 quota.0 ranlib.0 ratfor.0 rcp.0 rdist.0 refer.0 rev.0 \
	rlogin.0 rm.0 rmail.0 rmdir.0 roffbib.0 rsh.0 ruptime.0 rwho.0 sccs.0 \
	script.0 sed.0 sendbug.0 sh.0 size.0 sleep.0 soelim.0 sort.0 \
	sortbib.0 spell.0 spline.0 split.0 strings.0 strip.0 struct.0 stty.0 \
	style.0 su.0 sum.0 symorder.0 sysline.0 systat.0 tabs.0 tail.0 \
	talk.0 tar.0 tbl.0 tc.0 tcopy.0 tee.0 telnet.0 test.0 tftp.0 time.0 \
	tip.0 tk.0 tn3270.0 touch.0 tp.0 tr.0 troff.0 true.0 tset.0 tsort.0 \
	tty.0 ul.0 unifdef.0 uniq.0 units.0 uptime.0 users.0 uucp.0 \
	uuencode.0 uulog.0 uuname.0 uuq.0 uusend.0 uux.0 vacation.0 vgrind.0 \
	vi.0 vlp.0 vmstat.0 w.0 wait.0 wall.0 wc.0 what.0 whatis.0 \
	whereis.0 which.0 who.0 whoami.0 whois.0 window.0 write.0 xsend.0 \
	xstr.0 yacc.0 yes.0
LINKS=	case.0 edit.0 for.0 if.0 while.0 checkeq.0 neqn.0 cu.0 diffh.0 \
	egrep.0 fgrep.0 nohup.0 page.0 reset.0 enroll.0 spellin.0 \
	spellout.0 xget.0 chfn.0 chsh.0 unexpand.0 explain.0 indxbib.0 \
	uncompress.0 zcat.0 uudecode.0 Mail.0

.SUFFIXES: .1 .0

.1.0:
	${DESTDIR}/usr/man/manroff $*.1 > $*.0

all: ${OBJS}

eqn.0:
	eqn eqn.1 | ${DESTDIR}/usr/man/manroff > $@

clean: FRC
	rm -f ${OBJS}

install: FRC
	cd ${DESTDIR}${MDIR}; rm -f *.0
	install -c -o bin -g bin -m 444 ${OBJS} ${DESTDIR}${MDIR}
	ln ${DESTDIR}${MDIR}/mail.0 ${DESTDIR}${MDIR}/Mail.0
	ln ${DESTDIR}${MDIR}/sh.0 ${DESTDIR}${MDIR}/case.0
	ln ${DESTDIR}${MDIR}/ex.0 ${DESTDIR}${MDIR}/edit.0
	ln ${DESTDIR}${MDIR}/sh.0 ${DESTDIR}${MDIR}/for.0
	ln ${DESTDIR}${MDIR}/sh.0 ${DESTDIR}${MDIR}/if.0
	ln ${DESTDIR}${MDIR}/sh.0 ${DESTDIR}${MDIR}/while.0
	ln ${DESTDIR}${MDIR}/eqn.0 ${DESTDIR}${MDIR}/checkeq.0
	ln ${DESTDIR}${MDIR}/eqn.0 ${DESTDIR}${MDIR}/neqn.0
	ln ${DESTDIR}${MDIR}/tip.0 ${DESTDIR}${MDIR}/cu.0
	ln ${DESTDIR}${MDIR}/diff.0 ${DESTDIR}${MDIR}/diffh.0
	ln ${DESTDIR}${MDIR}/grep.0 ${DESTDIR}${MDIR}/egrep.0
	ln ${DESTDIR}${MDIR}/grep.0 ${DESTDIR}${MDIR}/fgrep.0
	ln ${DESTDIR}${MDIR}/nice.0 ${DESTDIR}${MDIR}/nohup.0
	ln ${DESTDIR}${MDIR}/more.0 ${DESTDIR}${MDIR}/page.0
	ln ${DESTDIR}${MDIR}/tset.0 ${DESTDIR}${MDIR}/reset.0
	ln ${DESTDIR}${MDIR}/xsend.0 ${DESTDIR}${MDIR}/enroll.0
	ln ${DESTDIR}${MDIR}/spell.0 ${DESTDIR}${MDIR}/spellin.0
	ln ${DESTDIR}${MDIR}/spell.0 ${DESTDIR}${MDIR}/spellout.0
	ln ${DESTDIR}${MDIR}/xsend.0 ${DESTDIR}${MDIR}/xget.0
	ln ${DESTDIR}${MDIR}/passwd.0 ${DESTDIR}${MDIR}/chfn.0
	ln ${DESTDIR}${MDIR}/passwd.0 ${DESTDIR}${MDIR}/chsh.0
	ln ${DESTDIR}${MDIR}/expand.0 ${DESTDIR}${MDIR}/unexpand.0
	ln ${DESTDIR}${MDIR}/diction.0 ${DESTDIR}${MDIR}/explain.0
	ln ${DESTDIR}${MDIR}/lookbib.0 ${DESTDIR}${MDIR}/indxbib.0
	ln ${DESTDIR}${MDIR}/compress.0 ${DESTDIR}${MDIR}/uncompress.0
	ln ${DESTDIR}${MDIR}/compress.0 ${DESTDIR}${MDIR}/zcat.0
	ln ${DESTDIR}${MDIR}/uuencode.0 ${DESTDIR}${MDIR}/uudecode.0
	ln ${DESTDIR}${MDIR}/csh.0 ${DESTDIR}${MDIR}/limit.0

FRC:
