/*	valloc.c	4.2	83/07/01	*/

char	*malloc();

char *
valloc(i)
	int i;
{
	int valsiz = getpagesize(), j;
	char *cp = malloc(i + (valsize-1));

	j = ((int)cp + (valsiz-1)) &~ (valsiz-1);
	return ((char *)j);
}
