/*	Abispsw.c	1.1	86/07/20	*/

#include "../tahoealign/align.h" 
bispsw(infop)	process_info *infop;
/*
/*	Bits set in PSW.
/*
/*************************************/
{
	register int mask;

	mask = operand(infop,0)->data;
	psl |= mask & 0x7f;
}
