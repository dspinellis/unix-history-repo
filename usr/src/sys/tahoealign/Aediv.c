/*	Aediv.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
int	zzz1,zzz2,zzz3,zzz4,zzz5;
ediv(infop)	process_info *infop;
/*
/*	Extended precision division.
/*
/***************************************/
{
	register long Register_12;	/* Has to be the first reg !! */
	register long Register_11;	/* remainder */
	register long Register_10;	/* quotient */
	register long Register_9;	/* divident least */
	register long Register_8;	/* divident most */
	register long Register_7;	/* divisor */

	Register_7 = operand(infop, 0)->data;
	Register_8 = operand(infop, 1)->data;
	Register_9 = operand(infop, 1)->data2;
	Register_12=psl;
	Set_psl(r12);	/* restore the user psl */
	asm ("	ediv	r7,r8,r10,r11");
	asm ("	movpsl	r12");
	New_cc (Register_12);
	write_back (infop, Register_10, operand(infop, 2));
	write_back (infop, Register_11, operand(infop, 3));
}
