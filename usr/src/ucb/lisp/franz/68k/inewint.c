/* Copyright (c) 1982, Regents, University of California */

extern int Fixzero[];
int *inewint(n)
{
	register int *ip;
	int *newint();
	if(n < 1024 && n >= -1024) return (Fixzero+n);
	ip = newint();
	*ip = n;
	return(ip);
}
blzero(where,howmuch)
register char *where;
{
	register char *p;
	for(p = where + howmuch; p > where; ) *--p = 0;
}
