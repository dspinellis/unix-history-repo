/*	Amovpsl.c	1.1	86/07/20	*/

#include "../tahoealign/align.h" 
movpsl(infop)	process_info *infop;
/*
/*	Move PSL to some place.
/*
/************************************/
{
	write_back (infop, psl, operand(infop,0));
}
