/*	Affc.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
ffc(infop)	process_info *infop;
/*
/*	Find first clear bit.
/*
/********************************/
{

	register long	Register_12;	/* Has to be first reg ! */
	register long	Register_11;
	register long	Register_10;

	Register_12 = operand(infop,0)->data;
	Register_10=psl;
	Set_psl(r10);	/* restore the user psl */
	asm ("	ffc	r12,r11");
	asm ("	movpsl	r12");
	New_cc (Register_12);
	write_back (infop,Register_11, operand(infop,1));
}
