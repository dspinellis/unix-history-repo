/*	setrgid.c	4.1	83/06/30	*/

setrgid(rgid)
	int rgid;
{

	return (setregid(rgid, -1));
}
