/*	error.c	4.1	83/03/09	*/
/*
 * error: default handling of errors.
 */

error(msg)
char *msg;
{
	message(msg);
	/* Maybe it would be nice to longjmp somewhere here */
}
