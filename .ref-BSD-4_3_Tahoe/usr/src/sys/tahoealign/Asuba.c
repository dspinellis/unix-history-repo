/*	Asuba.c	1.1	86/07/20	*/

#include "../tahoealign/align.h" 
suba(infop)	process_info *infop;
/*
/*	Subtract address.
/*
/****************************/
{
	register int data0, data1, result;

	data0 = operand(infop,0)->data;
	data1 = operand(infop,1)->data;
	result = data1 - data0;
	write_back (infop,result, operand(infop,1));
}
