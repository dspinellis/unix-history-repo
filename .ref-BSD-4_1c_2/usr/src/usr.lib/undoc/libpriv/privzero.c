/*	@(#)privzero.c	4.1	(Melbourne)	82/01/04	*/

privzero (p, size)
	register char *p;		/* r11 */
	register size;			/* r10 */
{
	asm("movc5 $0, (r11), $0, r10, (r11)");
}
