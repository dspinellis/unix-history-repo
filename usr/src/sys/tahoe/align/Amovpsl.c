/*	Amovpsl.c	1.2	90/12/04	*/

#include "align.h" 
movpsl(infop)	process_info *infop;
/*
/*	Move PSL to some place.
/*
/************************************/
{
	write_back (infop, psl, operand(infop,0));
}
