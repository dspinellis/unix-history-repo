/* copy.c - copy a string and return pointer to NULL terminator */


char   *copy (from, to)
register char  *from,
               *to;
{
    while (*to++ = *from++)
	continue;

    return (to - 1);
}
