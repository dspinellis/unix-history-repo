/*
 * Copyright (c) 1986, 1987, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)redist.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>

#include <ctype.h>
#include <dirent.h>
#include <stdio.h>
#include <string.h>

#include "bug.h"
#include "pathnames.h"
#include "extern.h"

/*
 * redist --
 *	Redistribute a bug report to those people indicated in the
 *	redistribution list file.
 */
void
redist()
{
	extern FILE	*dfp;		/* dist file fp */
	extern char	pfile[];	/* permanent bug file */
	register char	*C1, *C2;
	FILE	*pf;
	int	group;

	(void)sprintf(bfr, "%s/%s", dir, DIST_FILE);
	if (!freopen(bfr, "r", stdin))
		return;
	for (pf = NULL, group = 0; fgets(bfr, sizeof(bfr), stdin);) {
		if (C1 = strchr(bfr, '\n'))
			*C1 = '\0';
nextline:	if (*bfr == COMMENT ||
		    isspace(*bfr) || !(C1 = index(bfr, ':')))
			continue;
		*C1 = EOS;
		if (!strcmp(bfr, folder) || !strcmp(bfr, "all")) {
			for (++C1; *C1 && (*C1 == ' ' || *C1 == '\t'); ++C1);
			if (!*C1)			/* if empty list */
				continue;
			if (!pf) {
				if (!(pf = popen(MAIL_CMD, "w")))
					error("sendmail pipe failed.", CHN);
				if (mailhead[SUBJ_TAG].found)
					fprintf(pf,
					    "%s", mailhead[SUBJ_TAG].line);
				else
					fprintf(pf,
					    "Subject: Untitled Bug Report\n");
				if (!mailhead[TO_TAG].line) {
					if (mailhead[APPAR_TO_TAG].line)
					    fprintf(pf, "To%s",
				      strchr(mailhead[APPAR_TO_TAG].line,
					      ':'));
					else
					    fprintf(pf, "To: %s\n",  BUGS_ID);
				}
				fputs("Resent-To: ", pf);
			}
			/*
			 * write out first entry, then succeeding entries
			 * backward compatible, handles back slashes at end
			 * of line
			 */
			if (group++)
				fputs(", ", pf);
			for (;;) {
				if (C2 = strchr(C1, '\\'))
					*C2 = EOS;
				fputs(C1, pf);
				if (!fgets(bfr, sizeof(bfr), stdin))
					break;
				if (C1 = strchr(bfr, '\n'))
					*C1 = '\0';
				if (*bfr != ' ' && *bfr != '\t')
					goto nextline;
				for (C1 = bfr;
				    *C1 && (*C1 == ' ' || *C1 == '\t'); ++C1);
			}
		}
	}
	if (!pf)
		return;

	putc('\n', pf);

	rewind(dfp);
	/* add Reference header and copy bug report out */
	while (fgets(bfr, sizeof(bfr), dfp) && *bfr != '\n')
		fputs(bfr, pf);
	fprintf(pf, "\n%sReference: %s\n\n", mailhead[INDX_TAG].line, pfile);
	while (fgets(bfr, sizeof(bfr), dfp))
		fputs(bfr, pf);
	(void)pclose(pf);
}
