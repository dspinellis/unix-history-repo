/* $Header: last.c,v 4.3 85/05/01 11:42:16 lwall Exp $
 *
 * $Log:	last.c,v $
 * Revision 4.3  85/05/01  11:42:16  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "rn.h"
#include "util.h"
#include "intrp.h"
#include "INTERN.h"
#include "last.h"

char *lastname = Nullch;	/* path name of .rnlast file */

void
last_init(tcbuf)
char *tcbuf;
{
    lastname = savestr(filexp(LASTNAME));
    if ((tmpfp = fopen(lastname,"r")) != Nullfp) {
	fgets(tcbuf,1024,tmpfp);
	tcbuf[strlen(tcbuf)-1] = '\0';
	lastngname = savestr(tcbuf);
	fgets(tcbuf,1024,tmpfp);
	lasttime = atol(tcbuf);
	fgets(tcbuf,1024,tmpfp);
	lastactsiz = atol(tcbuf);
	fclose(tmpfp);
    }
    else {
	lastngname = nullstr;
	lasttime = 0;
	lastactsiz = 0;
    }
}

/* put out certain values for next run of rn */

void
writelast()
{
    if ((tmpfp = fopen(lastname,"w")) != Nullfp) {
	fprintf(tmpfp,"%s\n%ld\n%ld\n",ngname,(long)lasttime,(long)lastactsiz);
	fclose(tmpfp);
    }
    else
	printf(cantcreate,lastname) FLUSH;
}
