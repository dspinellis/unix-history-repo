/*	Aget_word.c	1.1	86/07/20	*/

#include	"../tahoealign/align.h"
int get_word (infop, address)
process_info	*infop;
char		*address;
/*
/*	Fetch the word at the given 'address' from memory.
/*	Caveat: It's quite difficult to find a pte reference
/*		fault.  So I took the easy way out and just signal
/*		an illegal access.
/*	
/**************************************************/
{
	register long code, value;

	code = readable(infop, address, 2);
	if (code == TRUE) {
		value = *address++ << 8;
		value = value | *address & 0xff;
		return(value);
	} else exception (infop, ILL_ACCESS, address, code);
}
