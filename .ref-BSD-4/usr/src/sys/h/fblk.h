/*	fblk.h	4.1	11/9/80	*/

struct fblk
{
	int    	df_nfree;
	daddr_t	df_free[NICFREE];
};
