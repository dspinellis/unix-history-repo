/*	Aaddf.c	1.1	86/07/20	*/

#include "../tahoealign/align.h"
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
