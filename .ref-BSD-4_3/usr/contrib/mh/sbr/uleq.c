/* uleq.c - "unsigned" lexical compare */


uleq (c1, c2)
register char  *c1,
               *c2;
{
    register int    c;

    if (!c1)
	c1 = "";
    if (!c2)
	c2 = "";

    while (c = *c1++)
	if ((c | 040) != (*c2 | 040))
	    return 0;
	else
	    c2++;
    return (*c2 == 0);
}
