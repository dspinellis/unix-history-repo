/*
 * This routine returns the address on the stack of the text of the
 *  first argument to the process.  It only works on the VAX, and only
 *  if the process was not called with 4 empty args in a row.
 */

char *invo_name()
{
	register int *ip;

	ip = (int *) 0x7ffffff8;        /* Highest stack address -4 */

	while(*--ip != 0)               /* Look backwards for bumber */
		;
	return (char *) &ip[1];         /* Next string is it. */
}
