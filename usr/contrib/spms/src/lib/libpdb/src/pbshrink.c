/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbshrink() shrinks the buffer by n characters just before the point
 * marked by buffer pointer bp.
 */
void
pbshrink(bp, n)
	register char *bp;		/* buffer pointer */
	int n;				/* shrink amount */
{
	register char *lowerbp;		/* lower roving buffer pointer */
	register char *upperbp;		/* upper roving buffer pointer */
	
	for (upperbp = bp; *upperbp != '\0'; upperbp++)
		continue;
	lowerbp = bp + n;
	if (lowerbp >= upperbp)
		*bp = '\0';
	while (lowerbp <= upperbp)
		*bp++ = *lowerbp++;
}
