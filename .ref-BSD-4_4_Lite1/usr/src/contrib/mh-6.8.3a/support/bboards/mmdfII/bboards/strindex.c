/* strindex.c - "unsigned" lexical index */


int  stringdex (p1, p2)
register char  *p1,
               *p2;
{
    register char  *p;

    for (p = p2; *p; p++)
	if (uprf (p, p1))
	    return (p - p2);

    return (-1);
}
