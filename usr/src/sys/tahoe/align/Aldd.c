/*	Aldd.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
ldd(infop)	process_info *infop;
/*
/*	Load a double operand into accumulator.
/*
/*************************************************/
{
	register struct oprnd *oprnd_pnt;

	oprnd_pnt = operand(infop,0);
	if ( reserved( oprnd_pnt->data ) ) 
		exception(infop, ILL_OPRND);
	acc_high = oprnd_pnt->data ;
	acc_low = oprnd_pnt->data2 ;
	psl |= PSL_DBL;
	infop->acc_dbl = 1;
}
