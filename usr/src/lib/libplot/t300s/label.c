#ifndef lint
static char sccsid[] = "@(#)label.c	4.1 (Berkeley) %G%";
#endif

#include "con.h"
label(s)
char *s;
{
	int i,c;
		while((c = *s++) != '\0'){
			xnow += HORZRES;
			spew(c);
		}
		return;
}
