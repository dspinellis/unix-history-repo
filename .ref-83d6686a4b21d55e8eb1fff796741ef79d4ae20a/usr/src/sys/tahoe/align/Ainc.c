/*	Ainc.c	1.1	86/07/20	*/

#include "../tahoealign/align.h" 
inc(infop)	process_info *infop;
/*
/*	Increment operand.
/*
/***************************************/
{
	register	long	Register_12;	/* Has to be first reg ! */
	register	long	Register_11;

	Register_11 = operand(infop,0)->data;
	Register_12=psl;
	Set_psl(r12);	/* restore the user psl */
	asm ("	incl	r11");			/* Make sure to use the
						 * right opcode */
	asm ("	movpsl	r12");
	New_cc (Register_12);

	write_back (infop,Register_11, operand(infop,0) );
}	
