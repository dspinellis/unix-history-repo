/*	pause.c	4.1	83/06/09	*/

/*
 * Backwards compatible pause.
 */
pause()
{

	sigpause(sigblock(0));
}
