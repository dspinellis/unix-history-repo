#include <stdio.h>

ambigsw(arg, swp)
char *arg;
{
	fprintf(stderr, "%s: ", invo_name());
	fprintf(stderr, "-%s ambiguous.  It matches \n", arg);
	printsw(arg, swp, "-");
}

