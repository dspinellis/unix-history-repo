# include "../hdr/macros.h"
# include <stdio.h>
SCCSID(@(#)xalloc	2.1);

xalloc(asize)
unsigned asize;
{

	fprintf(stderr, "Call to xalloc!?!\n");
}


xfree(aptr)
char *aptr;
{

	fprintf(stderr, "Call to xfree!?!\n");
}


xfreeall()
{

}
