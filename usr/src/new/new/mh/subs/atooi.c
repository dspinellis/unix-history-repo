atooi(cp)
register char *cp;
{
	register int i, base;

/*      base = 10;      */
	i = 0;
/*      if(*cp == '0')  */
		base = 8;
	while(*cp >= '0' && *cp <= '9') {
		i *= base;
		i += *cp++ - '0';
	}
	return(i);
}
