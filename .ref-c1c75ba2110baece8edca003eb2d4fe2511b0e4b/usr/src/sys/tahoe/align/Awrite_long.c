/*	Awrite_long.c	1.1	86/07/20	*/

#include	"../tahoealign/align.h"

write_longword (infop, longword, where)
process_info	*infop;
long		longword;
struct oprnd 	*where;
/*
/*	Put the longword at the given address in
/*	tahoe's memory.
/*
/**************************************************/
{
	if (! (where->mode & W)) exception(infop, ILL_ADDRMOD);
	switch (where->mode & ADDFIELD)	/* Mask out R/W bits */
	{
	case Add:
	case SPmode:
		put_longword (infop, longword, where->address);
		break;
	case Dir:
		Replace (infop, where->reg_number, longword);
		break;
	default:
		printf("Unknown destination in write_long (alignment code)\n");
	};
}	
