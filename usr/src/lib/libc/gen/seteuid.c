/*	seteuid.c	4.1	83/06/30	*/

seteuid(euid)
	int euid;
{

	return (setreuid(-1, euid));
}
