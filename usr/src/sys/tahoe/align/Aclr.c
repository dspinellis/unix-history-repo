/*	Aclr.c	1.2	90/12/04	*/

#include "align.h" 
clr(infop)	process_info *infop;
/*
/*	Clear operand
/*
/*************************************/
{

	write_back(infop, 0, operand(infop,0));
	negative_0;
	zero_1;
	overflow_0;
	carry_1;
}
