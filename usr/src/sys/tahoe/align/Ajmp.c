/*	Ajmp.c	1.2	90/12/04	*/

#include "align.h" 
jmp(infop)
process_info *infop;
/*
/*	Jump to the given address.
/*
/********************************************/
{
	pc = operand(infop,0)->address ;
}
