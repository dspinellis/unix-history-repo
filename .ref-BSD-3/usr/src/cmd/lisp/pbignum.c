#include "global.h"

pbignum(current, useport)
register lispval current;
register FILE *useport;
{
	int  *top, *bot, *work, negflag = 0, *sp(), *alloca();
	register int *digitp, *binp;
	register lispval last;

	/* copy bignum onto stack */
	top = sp() - 1;
	do {
		stack(current->I);
	} while(current = current->CDR);

	bot = sp();
	if (top==bot) {
		fprintf(useport,"%d",*bot);
		return;
	}

	/* save space for printed digits*/
	work = alloca((top-bot)*2*sizeof(int));
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
	while ( digitp > work) fprintf(useport,"%09d",*--digitp);
}
