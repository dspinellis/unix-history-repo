/*	Aldfd.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
ldfd(infop)	process_info *infop;
/*
/*	Load into accumulator float operand converted to double.
/*
/***************************************************************/
{
	register struct oprnd *oprnd_pnt;

	oprnd_pnt = operand(infop,0);
	if ( reserved( oprnd_pnt->data ) ) 
		exception(infop, ILL_OPRND);
	acc_high = oprnd_pnt->data ;
	acc_low = 0;
	psl |= PSL_DBL;
	infop->acc_dbl = 1;
}
