/*	Awrite_quad.c	1.1	86/07/20	*/

#include	"../tahoealign/align.h"

write_quadword (infop, qword, where)
process_info	*infop;
quadword 	qword;
struct oprnd 	*where;
/*
/*	Put the quadword at the given address in memory.
/*	
/*
/**************************************************/
{
	if (! (where->mode & W)) exception(infop, ILL_ADDRMOD);
	switch (where->mode & ADDFIELD)	/* Mask out R/W bits */
	{
	case Add:
		put_longword (infop, qword.high, where->address);
		where->address += 4;
		put_longword (infop, qword.low, where->address);
		break;
	case Dir:
		if ( where->reg_number >= SPOINTER || (where->reg_number & 1) == 1 )
			exception (infop, ILL_OPRND); 
		Replace (infop, where->reg_number, qword.high);
		Replace (infop, where->reg_number+1, qword.low);
		break;
	case SPmode:
		exception(infop, ILL_ADDRMOD);
		break;
	default:
		printf("Unknown destination in write_quad (alignment code)\n");
	};
}	
