/*	Acasel.c	1.1	86/07/20	*/

#include "../tahoealign/align.h" 
casel(infop)	process_info *infop;
/*
/*	Case (longword).
/*	Can't use real HW opcode, don't want to branch out !
/*
/***********************************/
{
	register long selector, base;
	register unsigned temporary, limit;

	selector = operand(infop,0)->data;
	base = operand(infop,1)->data;
	limit = operand(infop,2)->data;
	if (pc & 1) pc += 1;	/* Displacements are aligned ! */
	temporary = selector - base;
	if (temporary <= limit)
		pc = pc + get_word (infop, (char *)(pc + 2*temporary) );
	else pc = pc + limit*2 + 2;
}
