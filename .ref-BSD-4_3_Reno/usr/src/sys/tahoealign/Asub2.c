/*	Asub2.c	1.1	86/07/20	*/

#include "../tahoealign/align.h" 
sub2(infop)	process_info *infop;
/*
/*	Subtract , 2 operands.
/*
/*****************************************/
{

	register	long	Register_12;	/* Has to be first reg ! */
	register 	long	data0, data1, result;

	data0 = operand(infop,0)->data; 
	data1 = operand(infop,1)->data; 
	Register_12=psl;
	Set_psl(r12);	/* restore the user psl */
	result = data1 - data0;		/* 32 bits of true result */
	asm ("	movpsl	r12");
	New_cc (Register_12);
	write_back (infop,result, operand(infop,1) );
}
