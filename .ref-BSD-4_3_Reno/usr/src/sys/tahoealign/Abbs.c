/*	Abbs.c	1.1	86/07/20	*/

#include "../tahoealign/align.h" 
bbs(infop)	process_info *infop;
/*
/*	Branch on bit set.
/*
/********************************/
{
	register int position,base, new_address;

	position = operand(infop,0)-> data & 0x1f;
	base = operand(infop,1)->data;
	new_address = operand(infop,2) -> address;
	negative_0; zero_1; overflow_0; carry_1;
	if  (base & 1 << position) pc = new_address;
}
