/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)redist.c	5.4 (Berkeley) 87/09/01";
#endif not lint

#include <sys/file.h>
#include <stdio.h>
#include <bug.h>

/*
 * redist --
 *	Redistribute a bug report to those people indicated in the
 *	redistribution list file.
 */
redist()
{
	extern FILE	*dfp;		/* dist file fp */
	extern char	pfile[];	/* permanent bug file */
	register char	*C1,
			*C2;
	register int	first;		/* if first blank line */
	FILE	*pf,
		*popen();
	char	*index();

	sprintf(bfr, "%s/%s", dir, DIST_FILE);
	if (!freopen(bfr, "r", stdin))
		return;
	for (;;) {			/* get first part of entry */
		if (!gets(bfr))
			return;
		if (*bfr == COMMENT || *bfr == ' ' || *bfr == '\t' || !(C1 = index(bfr, ':')))
			continue;
		*C1 = EOS;
		if (!strcmp(bfr, folder))
			break;
	}
	for (++C1;*C1 && (*C1 == ' ' || *C1 == '\t');++C1);
	if (!*C1)			/* if empty */
		return;

	if (!(pf = popen(MAIL_CMD, "w")))
		error("sendmail pipe failed.", CHN);

	if (mailhead[SUBJ_TAG].found)
		fprintf(pf, "%s", mailhead[SUBJ_TAG].line);
	else
		fputs("Subject: Untitled Bug Report\n", pf);
	if (mailhead[TO_TAG].line == 0 && mailhead[APPAR_TO_TAG].line != 0)
		fprintf(pf, "To%s", index(mailhead[APPAR_TO_TAG].line, ':'));
	fputs("Resent-To: ", pf);

	/*
	 * write out first entry, then succeeding entries
	 * backward compatible, handles back slashes at end of line
	 */
	for (;;) {
		if (C2 = index(C1, '\\'))
			*C2 = EOS;
		fputs(C1, pf);
		if (!gets(bfr) || (*bfr != ' ' && *bfr != '\t'))
			break;
		for (C1 = bfr;*C1 && (*C1 == ' ' || *C1 == '\t');++C1);
	}
	putc('\n', pf);

	rewind(dfp);
	for (first = YES;fgets(bfr, sizeof(bfr), dfp);)
		if (*bfr == '\n' && first) {
			first = NO;
			fprintf(pf, "\n%sReference: %s\n", mailhead[INDX_TAG].line, pfile);
		}
		else
			fputs(bfr, pf);
	(void)pclose(pf);
}
