/*	setuid.c	4.1	83/06/30	*/

/*
 * Backwards compatible setuid.
 */
setuid(uid)
	int uid;
{

	return (setreuid(uid, uid));
}
