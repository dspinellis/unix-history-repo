/*	@(#)genbsubs.c	1.1	*/

/* The output of bunequal is the offset of the byte which didn't match;
 * if all the bytes match, then we return n.
 * bunequal(s1, s2, n) */

int
bunequal(s1, s2, n)
register char *s1, *s2;
register n;
{
    register int i = 0;

    while (i++ < n) {
	if (*s1++ != *s2++) {
	    break;
	}
    }
    return(i-1);
}

/* bskip(s1, n, b) : finds the first occurrence of any byte != 'b' in the 'n'
 * bytes beginning at 's1'.
 */

int
bskip(s1, n, b)
register char *s1;
register int n;
register int b;
{
    register int i = 0;

    while (i++ < n) {
	if (*s1++ != b) {
	    break;
	}
    }
    return(i-1);
}
