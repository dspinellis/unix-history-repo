/*	Aaddf.c	1.2	90/12/04	*/

#include "align.h"
addf(infop)	process_info *infop;
/*
/*	Add operand with accumulator to accumulator (float).
/*
/*************************************************************/
{
	register float 	*operand_pnt;
	register float	*acc_pnt;

	operand_pnt = (float *)&operand(infop,0)->data;
	acc_pnt = (float *) &acc_high;
	*acc_pnt = *acc_pnt + *operand_pnt;
}
