/*	fblk.h	4.2	81/02/19	*/

struct fblk
{
	int    	df_nfree;
	daddr_t	df_free[NICFREE];
};
