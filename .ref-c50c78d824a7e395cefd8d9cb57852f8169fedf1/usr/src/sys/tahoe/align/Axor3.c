/*	Axor3.c	1.1	86/07/20	*/

#include "../tahoealign/align.h" 
xor3 (infop)	process_info *infop;
/*
/*	Xor , 3 operands.
/*
/****************************************/
{
	register	long	Register_12;	/* Has to be first reg ! */
	register	long	result, data0, data1;

	data0 = operand(infop,0)->data; 
	data1 = operand(infop,1)->data; 
	Register_12=psl;
	Set_psl(r12);	/* restore the user psl */
	result = data0 ^ data1;
	asm ("	movpsl	r12");
	New_cc (Register_12);
	write_back (infop,result, operand(infop,2) );
}
