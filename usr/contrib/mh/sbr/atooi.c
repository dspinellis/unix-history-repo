/* atooi.c - octal version of atoi() */


int atooi(cp)
register char *cp;
{
    register int    i,
                    base;

    i = 0;
    base = 8;
    while (*cp >= '0' && *cp <= '7') {
	i *= base;
	i += *cp++ - '0';
    }

    return i;
}
