#ifndef lint
static char sccsid[] = "@(#)label.c	4.1 (Berkeley) %G%";
#endif

#include "gigi.h"

label(s)
char *s;
{
	printf("T(S0 H2 D0 I0) \"");
	for(;*s!='\0';s++) {
		putchar(*s);
		if (*s == '"') putchar('"');
	}
	putchar('"');
}
