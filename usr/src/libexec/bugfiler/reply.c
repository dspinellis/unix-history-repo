/*
 * Copyright (c) 1986, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)reply.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <bug.h>
#include <sys/file.h>
#include <stdio.h>

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
