/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)redist.c	5.1 (Berkeley) 86/11/25";
#endif not lint

#include <sys/file.h>
#include <stdio.h>
#include <bug.h>

extern HEADER	mailhead[];			/* mail headers */
extern char	*distf,				/* redist temp file */
		pfile[],			/* permanent bug file */
		folder[];			/* system name */

/*
 * redist --
 *	Redistribute a bug report to those people indicated in the
 *	redistribution list file.
 */
redist()
{
	register char	*C1,		/* traveling chars */
			*C2;
	register int	first = YES;	/* if first blank line */
	FILE	*pf,			/* pipe pointer */
		*dfp,			/* dist file fp */
		*popen();
	char	*index(), *mktemp();

	if (!freopen(DIST_FILE,"r",stdin))
		return;

	for (;;) {			/* get first part of entry */
		if (!gets(bfr))
			return;
		if (*bfr == COMMENT || *bfr == ' ' || *bfr == '\t' || !(C1 = index(bfr,':')))
			continue;
		*C1 = EOS;
		if (!strcmp(bfr,folder))
			break;
	}
	for (++C1;*C1 && (*C1 == ' ' || *C1 == '\t');++C1);
	if (!*C1)			/* if empty */
		return;

	if (!(pf = popen(MAIL_CMD,"w")))
		error("sendmail pipe failed.",CHN);

	fprintf(pf,"Reply-To: %s\n",BUGS_HOME);
	if (mailhead[SUBJ_TAG].found)
		fprintf(pf,"%s",mailhead[SUBJ_TAG].line);
	else
		fputs("Subject: Untitled Bug Report\n",pf);
	fputs("Resent-To: ",pf);

	/*
	 * write out first entry, then succeeding entries
	 * backward compatible, handles back slashes at end of line
	 */
	for (;;) {
		if (C2 = index(C1,'\\'))
			*C2 = EOS;
		fputs(C1,pf);
		if (!gets(bfr) || (*bfr != ' ' && *bfr != '\t'))
			break;
		for (C1 = bfr;*C1 && (*C1 == ' ' || *C1 == '\t');++C1);
	}
	fputs("\n",pf);

	if (!(dfp = fopen(distf,"r")))
		error("unable to read temporary file %s.",distf);
	while (fgets(bfr,sizeof(bfr),dfp))
		if (*bfr == '\n' && first) {
			first = NO;
			fprintf(pf,"\n%sReference: %s\n",mailhead[INDX_TAG].line,pfile);
		}
		else
			fputs(bfr,pf);
	fclose(dfp);
	pclose(pf);
	unlink(distf);
}
