/* tai_args.c - Argument processing routines */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/tai_args.c,v 7.1 91/02/22 09:20:26 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/tai_args.c,v 7.1 91/02/22 09:20:26 mrose Interim $
 *
 *
 * $Log:	tai_args.c,v $
 * Revision 7.1  91/02/22  09:20:26  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:47:50  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/attr.h"

extern char *oidtable,
	    *tailfile,
	    *myname;

extern LLog *log_dsap;

char        *usage = "[flags]";
char        *options = "T:t:c:";

tai_args (acptr,avptr)
int *acptr;
char ***avptr;
{
register char ** av;

int cnt;
register char *cp;

	if (acptr == (int *)NULL)
		return;

	if (*acptr <= 1)
		return;

	av = *avptr;
	av++, cnt = 1;

	while ((cp = *av) && *cp == '-') {
		switch (*++cp) {
			case 'T': oidtable = *++av;
				  cnt++;
					break;
			case 'c': myname = *++av;
				  cnt++;
					break;
			case 't': tailfile = *++av;
				  cnt++;
					break;

			default:
				LLOG (log_dsap,LLOG_FATAL,("Usage: %s %s\n",*avptr[0],usage));
				fatal(-1,"Usage...");
		}
		av++;
		cnt++;
	}

	*acptr -= cnt;
	*avptr = av;
}
