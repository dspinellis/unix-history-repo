/*
 * Copyright (c) 1986, 1987 Regents of the University of California.
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
static char sccsid[] = "@(#)reply.c	5.8 (Berkeley) 6/1/90";
#endif /* not lint */

#include <bug.h>
#include <sys/file.h>
#include <stdio.h>
#include "pathnames.h"

/*
 * reply --
 *	tell the user we got their silly little bug report
 */
reply()
{
	register char	*C,			/* traveling pointer */
			*to;			/* who we're replying to */
	register int	afd,			/* ack file descriptor */
			rval;			/* return value */
	FILE	*pf,				/* pipe pointer */
		*popen();
	char	*index();

	if (mailhead[RPLY_TAG].found) {
		for (C = mailhead[RPLY_TAG].line + mailhead[RPLY_TAG].len;*C != '\n' && (*C == ' ' || *C == '\t');++C);
		if (*C)
			goto gotone;
	}
	if (mailhead[FROM_TAG].found) {
		for (C = mailhead[FROM_TAG].line + mailhead[FROM_TAG].len;*C != '\n' && (*C == ' ' || *C == '\t');++C);
		if (*C)
			goto gotone;
	}
	if (mailhead[CFROM_TAG].found) {
		for (C = mailhead[CFROM_TAG].line + mailhead[CFROM_TAG].len;*C != '\n' && (*C == ' ' || *C == '\t');++C);
		if (*C)
			goto gotone;
	}
	return;

	/* if it's a foo <XXX>, get the XXX, else get foo (first string) */
gotone:	if (to = index(C, '<'))
		for (C = ++to;*C != '\n' && *C != ' ' && *C != '\t' && *C != '>';++C);
	else {
		to = C;
		for (to = C++;*C != '\n' && *C != ' ' && *C != '\t';++C);
	}
	*C = EOS;

	if (!(pf = popen(MAIL_CMD, "w")))
		error("sendmail pipe failed.", CHN);

	fprintf(pf, "Reply-To: %s\nFrom: %s (Bugs Bunny)\nTo: %s\n", BUGS_HOME, BUGS_HOME, to);
	if (mailhead[SUBJ_TAG].found)
		fprintf(pf, "Subject: Re:%s", mailhead[SUBJ_TAG].line + mailhead[SUBJ_TAG].len);
	else
		fputs("Subject: Bug report acknowledgement.\n", pf);
	if (mailhead[DATE_TAG].found)
		fprintf(pf, "In-Acknowledgement-Of: Your message of %s", mailhead[DATE_TAG].line + mailhead[DATE_TAG].len);
	if (mailhead[MSG_TAG].found)
		fprintf(pf, "\t\t%s", mailhead[MSG_TAG].line);
	fputs("Precedence: bulk\n\n", pf);	/* vacation(1) uses this... */
	fflush(pf);

	(void)sprintf(bfr, "%s/%s", dir, ACK_FILE);
	if ((afd = open(bfr, O_RDONLY, 0)) >= 0) {
		while ((rval = read(afd, bfr, sizeof(bfr))) != ERR && rval)
			(void)write(fileno(pf), bfr, rval);
		(void)close(afd);
	}
	pclose(pf);
}
