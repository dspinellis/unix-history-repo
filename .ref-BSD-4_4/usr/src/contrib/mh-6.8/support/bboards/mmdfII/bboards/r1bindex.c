/* r1bindex.c - right plus 1 or beginning index */


char *r1bindex(str, chr)
register char *str;
register int chr;
{
    register char  *cp;

    for (cp = str; *cp; cp++)
	continue;
    --cp;
    while (cp >= str && *cp != chr)
	--cp;
    return (++cp);
}
