/* copyip.c - copy a string array and return pointer to end */


char  **copyip (p, q)
register char  **p,
	       **q;
{
    while (*p)
	*q++ = *p++;
    *q = 0;

    return q;
}
