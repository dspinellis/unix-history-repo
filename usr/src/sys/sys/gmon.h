/*	gmon.h	4.1	82/06/27	*/

struct phdr {
	char	*lpc;
	char	*hpc;
	int	ncnt;
};

struct tostruct {
	char	*selfpc;
	long	count;
	u_short	link;
};

/*
 * a raw arc,
 *	with pointers to the calling site and the called site
 *	and a count.
 */
struct rawarc {
	u_long	raw_frompc;
	u_long	raw_selfpc;
	long	raw_count;
};
