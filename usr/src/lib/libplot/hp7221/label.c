#ifndef lint
static char sccsid[] = "@(#)label.c	4.1 (Berkeley) %G%";
#endif

#include "hp7221.h"

label(s)
char *s;
{
	printf("~'%s", s);
	putchar( ENDOFSTRING );
}
