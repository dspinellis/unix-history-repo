/*	Atst.c	1.1	86/07/20	*/

#include "../tahoealign/align.h" 
tst(infop)	process_info *infop;
/*
/*	Test operand, set condition codes.
/*
/************************************************/
{
	register long quantity;

	quantity = operand(infop,0)->data;
	if (quantity < 0) negative_1; else negative_0;
	if (quantity == 0) zero_1; else zero_0;
	overflow_0; carry_1;
}
