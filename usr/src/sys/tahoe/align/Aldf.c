/*	Aldf.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
ldf(infop)	process_info *infop;
/*
/*	Load a float operand into accumulator.
/*
/*************************************************/
{

	register struct oprnd *oprnd_pnt;

	oprnd_pnt = operand(infop,0);
	if ( reserved( oprnd_pnt->data ) ) 
		exception(infop, ILL_OPRND);
	if ( (oprnd_pnt->data & 0xff800000) == 0 ) acc_high = 0;
	else acc_high = oprnd_pnt->data ;
	psl &= ~PSL_DBL;
	infop->acc_dbl = 0;
}
