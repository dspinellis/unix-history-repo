/*	Abicpsw.c	1.2	90/12/04	*/

#include "align.h" 
bicpsw(infop)	process_info *infop;
/*
/*	Bits clear in PSW.
/*
/*************************************/
{
	register int mask;

	mask = operand(infop,0)->data;
	psl &= ~(mask & 0x7f);
}
