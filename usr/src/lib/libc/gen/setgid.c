/*	setgid.c	4.1	83/06/30	*/

/*
 * Backwards compatible setgid.
 */
setgid(gid)
	int gid;
{

	return (setregid(gid, gid));
}
