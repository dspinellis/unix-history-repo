/*	Atst.c	1.2	90/12/04	*/

#include "align.h" 
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
