/*
char id_getuid[] = "@(#)getuid_.c	1.1";
 *
 * get user id
 *
 * calling sequence:
 *	integer getuid, uid
 *	uid = getuid()
 * where:
 *	uid will be the real user id
 */

long getuid_()
{
	return((long)getuid());
}
