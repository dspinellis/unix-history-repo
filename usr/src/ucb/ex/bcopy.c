/* block copy from from to to, count bytes */
static char *sccsid = "@(#)bcopy.c	7.1	7/8/81";
bcopy(from, to, count)
#ifdef vax
	char *from, *to;
	int count;
{

	asm("	movc3	12(ap),*4(ap),*8(ap)");
}
#else
	register char *from, *to;
	register int count;
{
	while ((count--) > 0)	/* mjm */
		*to++ = *from++;
}
#endif
