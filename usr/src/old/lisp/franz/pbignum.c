#ifndef lint
static char *rcsid =
   "$Header: pbignum.c,v 1.3 83/09/12 14:17:59 sklower Exp $";
#endif

/*					-[Sat Jan 29 13:30:47 1983 by jkf]-
 * 	pbignum.c			$Locker:  $
 * print a bignum
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"

pbignum(current, useport)
register lispval current;
register FILE *useport;
{
	long  *top, *bot, *work, negflag = 0;
	char *alloca();
	register int *digitp;
	Keepxs();

	/* copy bignum onto stack */
	top = (sp()) - 1;
	do {
		stack(current->s.I);
	} while(current = current->s.CDR);

	bot = sp();
	if (top==bot) {
		fprintf(useport,"%d",*bot);
		Freexs();
		return;
	}

	/* save space for printed digits*/
	work = (int *)alloca((top-bot)*2*sizeof(int));
	if( *bot < 0) {
		negflag = 1;
		dsneg(top,bot);
	}

	/* figure out nine digits at a time by destructive division*/
	for(digitp = work; bot <= top; digitp++) {
		*digitp = dodiv(top,bot);
		if(*bot==0) bot += 1;
	}
	
	/* print them out */

	if(negflag) putc('-',useport);
	fprintf(useport,"%d",*--digitp);
	while ( digitp > work) fprintf(useport,"%.09d",*--digitp);
	Freexs();
}
