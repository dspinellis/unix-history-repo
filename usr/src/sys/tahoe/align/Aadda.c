/*	Aadda.c	1.2	90/12/04	*/

#include "align.h" 
adda(infop)	process_info *infop;
/*
/*	Add address.
/*
/************************/
{
	register long result;

	result = operand(infop,1)->data + operand(infop,0)->data;
	write_back (infop,result, operand(infop,1));
}
