/*	Astd.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
std(infop)	process_info *infop;
/*
/*	Store accumulator (double) in destination.
/*
/*************************************************/
{
	quadword ac;

	ac.high = acc_high;
	ac.low = acc_low;
	write_quadword (infop, ac, operand(infop,0) );
	if (ac.high < 0) negative_1; else negative_0;
	if ( (ac.high & 0xff800000) == 0 ) zero_1; else zero_0;
	carry_1;
	overflow_0;
}
