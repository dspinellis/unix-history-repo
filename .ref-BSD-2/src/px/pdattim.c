/* (c) 1979 Regents of the University of California */
#include "opcode.h"

char	pd_date[] {
	8, 9, 10, 4, 5, 6, 10, 22, 23, 10, 0
};

pdattim(op, alfap)
char *alfap;
{
	register char *ap, *cp, *dp;
	long a;
	int i;

	time(&a);
	cp = ctime(&a);
	ap = alfap;
	if (op == O_DATE)
		for (dp = pd_date; *dp; *ap++ = cp[*dp++]);
	else
		for (cp =+ 10, i = 10; i; *ap++ = *cp++, i--);
}
