/*	Amneg.c	1.1	86/07/20	*/

#include "../tahoealign/align.h" 
mneg(infop)	process_info *infop;
/*
/*	Move negated operand.
/*
/**********************************/
{
	register	long	Register_12;	/* Has to be first reg ! */
	register	long	Register_11;

	Register_12 = operand(infop, 0)->data;
	Register_11=psl;
	Set_psl(r11);	/* restore the user psl */
	asm ("	mnegl	r12,r11");
	asm ("	movpsl	r12");
	New_cc (Register_12);
	write_back (infop, Register_11, operand(infop,1));
}	
