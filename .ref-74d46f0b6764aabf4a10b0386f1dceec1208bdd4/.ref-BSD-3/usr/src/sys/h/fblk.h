/*	fblk.h	2.1	1/5/80 */

struct fblk
{
	int    	df_nfree;
	daddr_t	df_free[NICFREE];
};
