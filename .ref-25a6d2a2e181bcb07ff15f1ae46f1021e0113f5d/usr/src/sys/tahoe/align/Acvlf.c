/*	Acvlf.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
cvlf(infop)	process_info *infop;
/*
/*	Convert integer to float (into accumulator).
/*
/******************************************************/
{
	register	long	Register_12;	/* Has to be first reg ! */
	register	long	Register_11;
	register	float	*Register_10;
	/*
	register	long	Register_9;
	register	long	Register_8;
	*/
	register struct oprnd *oppnt;

	Register_11 = operand(infop,0)->data;
	Register_10 = (float *) &acc_high;
	Register_12 = psl;
	Set_psl (r12);  
	asm ("	cvlf	r11");		/* Don't change the order !! */
	asm ("	movpsl	r12");
	asm ("	stf	(r10)");
	New_cc ( Register_12 );
}
