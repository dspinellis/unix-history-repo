/*
char id_getgid[] = "@(#)getgid_.c	1.1";
 *
 * get group id
 *
 * calling sequence:
 *	integer getgid, gid
 *	gid = getgid()
 * where:
 *	gid will be the real group id
 */

long getgid_()
{
	return((long)getgid());
}
