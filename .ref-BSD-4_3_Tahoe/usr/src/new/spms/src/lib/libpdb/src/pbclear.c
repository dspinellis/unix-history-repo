/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbclear() clears the current database buffer CURPBUF.
 */
extern char *CURPBUF;			/* current database buffer */
extern char *FP;			/* next non-key field */
extern char *KP;			/* next key field */

void
pbclear()
{
	CURPBUF[0] = '\0';
	FP = KP = CURPBUF;
}
