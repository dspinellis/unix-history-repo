/*	Alnf.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
lnf(infop)	process_info *infop;
/*
/*	Load a negated float operand into accumulator.
/*
/******************************************************/
{

	register struct oprnd	*op_pnt;

	op_pnt = operand(infop,0);
	if ( reserved( op_pnt->data ) ) 
		exception(infop, ILL_OPRND);
	if ( op_pnt->data == 0 ) acc_high = 0;
	else acc_high = 0x80000000 ^ op_pnt->data ;
	psl &= ~PSL_DBL;
	infop->acc_dbl = 0;
}
