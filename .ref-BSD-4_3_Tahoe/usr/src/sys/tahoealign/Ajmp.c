/*	Ajmp.c	1.1	86/07/20	*/

#include "../tahoealign/align.h" 
jmp(infop)
process_info *infop;
/*
/*	Jump to the given address.
/*
/********************************************/
{
	pc = operand(infop,0)->address ;
}
