/*	clearg.c	4.1	83/03/09	*/

#include "2648.h"

clearg()
{
	sync();
	escseq(ESCD);
	outchar((_video==INVERSE) ? 'b' : 'a');
}
