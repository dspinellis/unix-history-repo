/* tai_args.c - Argument processing routines */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/tai_args.c,v 7.2 91/02/22 09:39:55 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/tai_args.c,v 7.2 91/02/22 09:39:55 mrose Interim $
 *
 *
 * $Log:	tai_args.c,v $
 * Revision 7.2  91/02/22  09:39:55  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:54:48  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:18:09  mrose
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


#include "quipu/util.h"
#include "quipu/attr.h"

extern char *dsaoidtable,
	    *dsatailfile,
	    *mydsaname,
	    *treedir;

static char        *usage = "[-t <tailor>] [-c <dsa name>] [-T <oidtable>] [-D <directory>]";

extern LLog * log_dsap;

dsa_tai_args (acptr,avptr)
int *acptr;
char ***avptr;
{
char ** av;
register char *cp;
int cnt;

	if (acptr == (int *)NULL)
		return;

	av = *avptr;
	av++, cnt = 1;

	while ((cp = *av) && *cp == '-') {
		switch (*++cp) {
			case 'T': dsaoidtable = *++av;
				  cnt++;
					break;
			case 'D': treedir = *++av;
				  cnt++;
					break;
			case 'c': mydsaname = *++av;
				  cnt++;
					break;
			case 't': dsatailfile = *++av;
				 cnt++;
				 break;

			default:
				LLOG (log_dsap,LLOG_FATAL,("Unknown option\nUsage: %s %s\n",*avptr[0],usage));
				fatal(-46,"Usage...");
		}
		av++;
		cnt++;
	}

	*acptr -= cnt;
	*avptr = av;
}
