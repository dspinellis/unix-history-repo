/*	Acmpf.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
cmpf(infop)	process_info *infop;
/*
/*	Compare accumulator (float) with operand.
/*
/*************************************************/
{
	register float	*Register_12;	/* Has to be first reg ! */
	register float	*Register_11;
	register long	Register_10;

	Register_12 = (float *) &acc_high;
	Register_11 = (float *) &operand(infop,0)->data;
	if ( reserved( *(long *)Register_11 ) )
			exception(infop, ILL_OPRND);
	asm ("	ldf	(r12)");
	Register_10=psl;
	Set_psl(r10);	/* restore the user psl */
	asm ("	cmpf	(r11)");
	asm ("	movpsl	r10");
	New_cc (Register_10);
}
