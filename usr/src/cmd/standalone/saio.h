/*
 * header file for standalone package
 */

/*
 * io block: includes an
 * inode, cells for the use of seek, etc,
 * and a buffer.
 */
struct	iob {
	char	i_flgs;
	struct inode i_ino;
	int i_unit;
	daddr_t	i_boff;
	daddr_t	i_cyloff;
	off_t	i_offset;
	daddr_t	i_bn;
	char	*i_ma;
	int	i_cc;
	char	i_buf[512];
};

#define F_READ	01
#define F_WRITE	02
#define F_ALLOC	04
#define F_FILE	010




/*
 * dev switch
 */
struct devsw {
	char	*dv_name;
	int	(*dv_strategy)();
	int	(*dv_open)();
	int	(*dv_close)();
};

struct devsw devsw[];

/*
 * request codes. Must be the same a F_XXX above
 */
#define	READ	1
#define	WRITE	2


#define	NBUFS	4


char	b[NBUFS][512];
daddr_t	blknos[NBUFS];



#define NFILES	4
struct	iob iob[NFILES];

/*
 * Set to which 32Kw segment the code is physically running in.
 * Must be set by the users main (or there abouts).
 */
int	segflag;
