/*	@(#)gdpriv.c	4.3	(Melbourne)	82/07/17	*/

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/quota.h>
#include <udata.h>
#include <lpdquota.h>
#include <mushmuck.h>

static char dpprefix[] = "/usr/adm/defprivs/";

gdpriv(name, mp, lp, dp, dnp, up)
char *name;
struct mushmuck *mp;
struct lpquota *lp;
struct dquot *dp;
char (*dnp)[32];
struct udata *up;
{
	char	dpfile[sizeof dpprefix + 128 + 1];

	strcpy(dpfile, dpprefix);
	strcat(dpfile, name);

	return(rdprivf(dpfile, mp, lp, dp, dnp, up));
}
