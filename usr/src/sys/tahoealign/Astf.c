/*	Astf.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
stf(infop)	process_info *infop;
/*
/*	Store accumulator (float) in destination.
/*
/*************************************************/
{

	write_back (infop, acc_high, operand(infop,0) );
	if (acc_high < 0) negative_1; else negative_0;
	if ( (acc_high & 0xff800000) == 0 ) zero_1; else zero_0;
	carry_1; overflow_0;
}
