#ifndef lint
static char sccsid[] = "@(#)label.c	4.1 (Berkeley) 6/27/83";
#endif

#include "con.h"
label(s)
char *s;
{
	int i,c;
		while((c = *s++) != '\0'){
			xnow += 6;
			spew(c);
		}
		return;
}
