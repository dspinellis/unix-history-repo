/*	Acmpf2.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
cmpf2(infop)	process_info *infop;
/*
/*	Compare operand 1 with operand2 (float).
/*
/*************************************************/
{
	register float	*Register_12;	/* Has to be first reg ! */
	register float	*Register_11;
	register long	Register_10;

	Register_12 = (float *) &operand(infop,0)->data;
	Register_11 = (float *) &operand(infop,1)->data;
	if ( reserved( *(long *)Register_12 ) ||
	     reserved( *(long *)Register_11 ) )
			{exception(infop, ILL_OPRND);}

	Register_10=psl;
	Set_psl(r10);	/* restore the user psl */
	asm ("	cmpf2	(r12),(r11)");
	asm ("	movpsl	r10");
	New_cc (Register_10);
}
