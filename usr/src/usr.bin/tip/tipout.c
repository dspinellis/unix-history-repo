/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)tipout.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

#include "tip.h"
/*
 * tip
 *
 * lower fork of tip -- handles passive side
 *  reading from the remote host
 */

static	jmp_buf sigbuf;

/*
 * TIPOUT wait state routine --
 *   sent by TIPIN when it wants to posses the remote host
 */
intIOT()
{

	write(repdes[1],&ccc,1);
	read(fildes[0], &ccc,1);
	longjmp(sigbuf, 1);
}

/*
 * Scripting command interpreter --
 *  accepts script file name over the pipe and acts accordingly
 */
intEMT()
{
	char c, line[256];
	register char *pline = line;
	char reply;

	read(fildes[0], &c, 1);
	while (c != '\n') {
		*pline++ = c;
		read(fildes[0], &c, 1);
	}
	*pline = '\0';
	if (boolean(value(SCRIPT)) && fscript != NULL)
		fclose(fscript);
	if (pline == line) {
		boolean(value(SCRIPT)) = FALSE;
		reply = 'y';
	} else {
		if ((fscript = fopen(line, "a")) == NULL)
			reply = 'n';
		else {
			reply = 'y';
			boolean(value(SCRIPT)) = TRUE;
		}
	}
	write(repdes[1], &reply, 1);
	longjmp(sigbuf, 1);
}

intTERM()
{

	if (boolean(value(SCRIPT)) && fscript != NULL)
		fclose(fscript);
	exit(0);
}

intSYS()
{

	boolean(value(BEAUTIFY)) = !boolean(value(BEAUTIFY));
	longjmp(sigbuf, 1);
}

/*
 * ****TIPOUT   TIPOUT****
 */
tipout()
{
	char buf[BUFSIZ];
	register char *cp;
	register int cnt;
	extern int errno;
	int omask;

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGEMT, intEMT);		/* attention from TIPIN */
	signal(SIGTERM, intTERM);	/* time to go signal */
	signal(SIGIOT, intIOT);		/* scripting going on signal */
	signal(SIGHUP, intTERM);	/* for dial-ups */
	signal(SIGSYS, intSYS);		/* beautify toggle */
	(void) setjmp(sigbuf);
	for (omask = 0;; sigsetmask(omask)) {
		cnt = read(FD, buf, BUFSIZ);
		if (cnt <= 0) {
			/* lost carrier */
			if (cnt < 0 && errno == EIO) {
				sigblock(sigmask(SIGTERM));
				intTERM();
				/*NOTREACHED*/
			}
			continue;
		}
#define	ALLSIGS	sigmask(SIGEMT)|sigmask(SIGTERM)|sigmask(SIGIOT)|sigmask(SIGSYS)
		omask = sigblock(ALLSIGS);
		for (cp = buf; cp < buf + cnt; cp++)
			*cp &= 0177;
		write(1, buf, cnt);
		if (boolean(value(SCRIPT)) && fscript != NULL) {
			if (!boolean(value(BEAUTIFY))) {
				fwrite(buf, 1, cnt, fscript);
				continue;
			}
			for (cp = buf; cp < buf + cnt; cp++)
				if ((*cp >= ' ' && *cp <= '~') ||
				    any(*cp, value(EXCEPTIONS)))
					putc(*cp, fscript);
		}
	}
}
