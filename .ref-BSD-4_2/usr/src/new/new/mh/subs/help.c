#include "mh.h"

help(str, swp)
char *str;
{
	printf("syntax: %s\n", str);
	printf("  switches are:\n");
	printsw(ALL, swp, "-");
}
