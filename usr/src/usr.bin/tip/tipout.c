/*	tipout.c	4.5	81/12/16	*/
#include "tip.h"
/*
 * tip
 *
 * lower fork of tip -- handles passive side
 *  reading from the remote host
 */

/*
 * TIPOUT wait state routine --
 *   sent by TIPIN when it wants to posses the remote host
 */
intIOT()
{
	signal(SIGIOT, SIG_IGN);
	write(repdes[1],&ccc,1);
	read(fildes[0], &ccc,1);
	signal(SIGIOT, intIOT);
	intflag = 1;
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

	signal(SIGEMT, SIG_IGN);
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
	signal(SIGEMT, intEMT);
	intflag = 1;
}

intTERM()
{
	signal(SIGTERM, SIG_IGN);
	if (boolean(value(SCRIPT)) && fscript != NULL)
		fclose(fscript);
	exit(0);
}

intSYS()
{
	signal(SIGSYS, intSYS);
	boolean(value(BEAUTIFY)) = !boolean(value(BEAUTIFY));
	intflag = 1;
}

/*
 * ****TIPOUT   TIPOUT****
 */
tipout()
{
	char buf[BUFSIZ];
	register char *cp;
	register int cnt;

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGEMT, intEMT);		/* attention from TIPIN */
	signal(SIGTERM, intTERM);	/* time to go signal */
	signal(SIGIOT, intIOT);		/* scripting going on signal */
	signal(SIGHUP, intTERM);	/* for dial-ups */
	signal(SIGSYS, intSYS);		/* beautify toggle */

	for (;;) {
		do {
			intflag = 0;
			cnt = read(FD, buf, BUFSIZ);
		} while (intflag);
		if (cnt <= 0)
			continue;
		for (cp = buf; cp < buf + cnt; cp++)
			*cp &= 0177;
		write(1, buf, cnt);
		if (boolean(value(SCRIPT)) && fscript != NULL) {
			if (!boolean(value(BEAUTIFY))) {
				fputs(buf, fscript);
				continue;
			}
			for (cp = buf; cp < buf + cnt; cp++) {
				if (*cp < ' ' && !any(*cp, value(EXCEPTIONS)))
					continue;
				putc(*cp, fscript);
			}
		}
	}
}
