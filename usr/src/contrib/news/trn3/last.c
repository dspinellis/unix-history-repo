/* $Id: last.c,v 3.0 1992/02/01 03:09:32 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "trn.h"
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
    if ((tmpfp = fopen(lastname,"r")) != Nullfp &&
	fgets(tcbuf,1024,tmpfp) != Nullch) {
	tcbuf[strlen(tcbuf)-1] = '\0';
	lastngname = savestr(tcbuf);
	fgets(tcbuf,1024,tmpfp);
	lasttime = atol(tcbuf);
	fgets(tcbuf,1024,tmpfp);
	lastactsiz = atol(tcbuf);
	if (fgets(tcbuf,1024,tmpfp) != Nullch)
	    lastnewtime = atol(tcbuf);
	else
	    lastnewtime = (lasttime? lasttime : time(Null(time_t*))-24L*60*60);
	if (fgets(tcbuf,1024,tmpfp) != Nullch)
	    lastnewsize = atol(tcbuf);
	else
	    lastnewsize = 0;
	fclose(tmpfp);
    }
    else {
	lastngname = nullstr;
	lasttime = 0;
	lastactsiz = 0;
	lastnewsize = 0;
	/* Use yesterday as an initial value for finding new groups. */
	lastnewtime = time(Null(time_t*)) - 24L*60*60;
    }
}

/* put out certain values for next run of rn */

void
writelast()
{
    if ((tmpfp = fopen(lastname,"w")) != Nullfp) {
	fprintf(tmpfp,"%s\n%ld\n%ld\n%ld\n%ld\n",
	    (ngname==Nullch?nullstr:ngname),(long)lasttime,(long)lastactsiz,
	    (long)lastnewtime,(long)lastnewsize);
	fclose(tmpfp);
    }
    else
	printf(cantcreate,lastname) FLUSH;
}
