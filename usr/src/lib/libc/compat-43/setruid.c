/*	setruid.c	4.1	83/06/30	*/

setruid(ruid)
	int ruid;
{

	return (setreuid(ruid, -1));
}
