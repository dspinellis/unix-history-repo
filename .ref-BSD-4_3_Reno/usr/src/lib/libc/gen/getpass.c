/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getpass.c	5.7 (Berkeley) 6/17/90";
#endif /* LIBC_SCCS and not lint */

#include <sys/termios.h>
#include <sys/signal.h>
#include <stdio.h>
#include <pwd.h>

char *
getpass(prompt)
	char *prompt;
{
	struct termios term;
	register int ch;
	register char *p;
	FILE *fp, *outfp;
	long omask;
	int echo;
	static char buf[_PASSWORD_LEN + 1];

	/*
	 * read and write to /dev/tty if possible; else read from
	 * stdin and write to stderr.
	 */
	if ((outfp = fp = fopen("/dev/tty", "w+")) == NULL) {
		outfp = stderr;
		fp = stdin;
	}
	/*
	 * note - blocking signals isn't necessarily the
	 * right thing, but we leave it for now.
	 */
	omask = sigblock(sigmask(SIGINT)|sigmask(SIGTSTP));
	(void)tcgetattr(fileno(fp), &term);
	if (echo = (term.c_lflag & ECHO)) {
		term.c_lflag &= ~ECHO;
		term.c_cflag |= CIGNORE;
		(void)tcsetattr(fileno(fp), TCSAFLUSH, &term);
	}
	(void)fputs(prompt, outfp);
	rewind(outfp);			/* implied flush */
	for (p = buf; (ch = getc(fp)) != EOF && ch != '\n';)
		if (p < buf + _PASSWORD_LEN)
			*p++ = ch;
	*p = '\0';
	(void)write(fileno(outfp), "\n", 1);
	if (echo) {
		term.c_lflag |= ECHO;
		term.c_cflag |= CIGNORE;
		tcsetattr(fileno(fp), TCSAFLUSH, &term);
	}
	(void)sigsetmask(omask);
	if (fp != stdin)
		(void)fclose(fp);
	return(buf);
}
