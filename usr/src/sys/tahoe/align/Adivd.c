/*	Adivd.c	1.2	90/12/04	*/

#include "align.h"
divd(infop)	process_info *infop;
/*
/*	Divide accumulator by operand to accumulator (double).
/*
/*******************************************************************/
{
	register double	*operand_pnt;
	register double	*acc_pnt;

	operand_pnt = (double *)&operand(infop,0)->data;
	acc_pnt = (double *) &acc_high;
	*acc_pnt = *acc_pnt / *operand_pnt;
}
