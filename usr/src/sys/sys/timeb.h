/*	timeb.h	3.1	%H%	*/

/*
 * Structure returned by ftime system call
 */
struct timeb
{
	time_t	time;
	unsigned short millitm;
	short	timezone;
	short	dstflag;
};
