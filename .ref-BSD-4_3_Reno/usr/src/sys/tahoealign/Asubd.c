/*	Asubd.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
subd(infop)	process_info *infop;
/*
/*	Subtract operand from accumulator to accumulator (double).
/*
/*******************************************************************/
{
	register double 	*operand_pnt;
	register double		*acc_pnt;

	operand_pnt = (double *)&operand(infop,0)->data;
	acc_pnt = (double *) &acc_high;
	*acc_pnt = *acc_pnt - *operand_pnt;
}
