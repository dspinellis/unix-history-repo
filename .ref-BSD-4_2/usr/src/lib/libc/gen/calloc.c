/* @(#)calloc.c	4.1 (Berkeley) 12/21/80 */
/*	calloc - allocate and clear memory block
*/
#define CHARPERINT (sizeof(int)/sizeof(char))
#define NULL 0

char *
calloc(num, size)
unsigned num, size;
{
	register char *mp;
	char *malloc();
	register int *q;
	register m;

	num *= size;
	mp = malloc(num);
	if(mp == NULL)
		return(NULL);
	q = (int *) mp;
	m = (num+CHARPERINT-1)/CHARPERINT;
	while(--m>=0)
		*q++ = 0;
	return(mp);
}

cfree(p, num, size)
char *p;
unsigned num, size;
{
	free(p);
}
