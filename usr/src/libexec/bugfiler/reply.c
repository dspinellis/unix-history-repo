/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)reply.c	5.1 (Berkeley) 86/11/25";
#endif not lint

#include <bug.h>
#include <sys/file.h>
#include <stdio.h>

extern HEADER	mailhead[];			/* mail headers */

/*
 * reply --
 *	tell the user we got their bug report
 */
reply()
{
	register char	*C,			/* traveling pointer */
			*to;			/* who we're replying to */
	register int	afd,			/* ack file descriptor */
			rval;			/* return value */
	FILE	*pf,				/* pipe pointer */
		*popen();
	char	*mktemp(), *strcpy(), *index();

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
gotone:	if (to = index(C,'<'))
		for (C = ++to;*C != '\n' && *C != ' ' && *C != '\t' && *C != '>';++C);
	else {
		to = C;
		for (to = C++;*C != '\n' && *C != ' ' && *C != '\t';++C);
	}
	*C = EOS;

	if (!(pf = popen(MAIL_CMD,"w")))
		error("sendmail pipe failed.",CHN);

	fprintf(pf,"Reply-To: %s\nFrom: %s (Bugs Bunny)\nTo: %s\n",BUGS_HOME,BUGS_HOME,to);
	if (mailhead[SUBJ_TAG].found)
		fprintf(pf,"Subject: Re:%s",mailhead[SUBJ_TAG].line + mailhead[SUBJ_TAG].len);
	else
		fputs("Subject: Bug report acknowledgement.\n",pf);
	if (mailhead[DATE_TAG].found)
		fprintf(pf,"In-Acknowledgement-Of: Your message of %s",mailhead[DATE_TAG].line + mailhead[DATE_TAG].len);
	if (mailhead[MSG_TAG].found)
		fprintf(pf,"\t\t%s",mailhead[MSG_TAG].line);
	putc('\n',pf);
	fflush(pf);

	if ((afd = open(ACK_FILE,O_RDONLY,0)) >= 0) {
		while ((rval = read(afd,bfr,sizeof(bfr))) != ERR && rval)
			write(fileno(pf),bfr,rval);
		close(afd);
	}

	pclose(pf);
}
