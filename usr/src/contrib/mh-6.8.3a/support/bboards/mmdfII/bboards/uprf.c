/* uprf.c - "unsigned" lexical prefix  */


uprf (c1, c2)
register char  *c1,
               *c2;
{
    register int    c;

    while (c = *c2++)
	if ((c | 040) != (*c1 | 040))
	    return 0;
	else
	    c1++;

    return 1;
}
