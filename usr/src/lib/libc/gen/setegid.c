/*	setegid.c	4.1	83/06/30	*/

setegid(egid)
	int egid;
{

	return (setregid(-1, egid));
}
