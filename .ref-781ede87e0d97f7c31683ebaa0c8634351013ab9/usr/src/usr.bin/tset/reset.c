/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)reset.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * reset - restore tty to sensible state after crapping out in raw mode.
 */
#include <sgtty.h>

main()
{
	struct sgttyb buf;
	struct tchars tbuf;
	struct ltchars ltbuf;
	static int lclear = LMDMBUF|LLITOUT|LPASS8;

	ioctl(2, TIOCGETP, &buf);
	ioctl(2, TIOCGETC, &tbuf);
	ioctl(2, TIOCGLTC, &ltbuf);
	buf.sg_flags &= ~(RAW|CBREAK|VTDELAY|ALLDELAY);
	buf.sg_flags |= XTABS|ECHO|CRMOD|ANYP;
	reset(&buf.sg_erase, CERASE);
	reset(&buf.sg_kill, CKILL);
	reset(&tbuf.t_intrc, CINTR);
	reset(&tbuf.t_quitc, CQUIT);
	reset(&tbuf.t_startc, CSTART);
	reset(&tbuf.t_stopc, CSTOP);
	reset(&tbuf.t_eofc, CEOF);
	reset(&ltbuf.t_suspc, CSUSP);
	reset(&ltbuf.t_dsuspc, CDSUSP);
	reset(&ltbuf.t_rprntc, CRPRNT);
	reset(&ltbuf.t_flushc, CFLUSH);
	reset(&ltbuf.t_lnextc, CLNEXT);
	reset(&ltbuf.t_werasc, CWERASE);
	/* brkc is left alone */
	ioctl(2, TIOCLBIC, &lclear);
	ioctl(2, TIOCSETN, &buf);
	ioctl(2, TIOCSETC, &tbuf);
	ioctl(2, TIOCSLTC, &ltbuf);
	execlp("tset", "tset", "-Q", "-I", 0);	/* fix term dependent stuff */
	exit(1);
}

reset(cp, def)
	char *cp;
	int def;
{

	if (*cp == 0 || (*cp&0377)==0377)
		*cp = def;
}
