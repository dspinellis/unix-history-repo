/*	text.h	2.1	1/5/80	*/

/*
 * Text structure.
 * One allocated per pure
 * procedure on swap device.
 * Manipulated by text.c
 */
struct text
{
	swblk_t	x_daddr;	/* disk address of segment */
	size_t	x_size;		/* size (clicks) */
	struct proc *x_caddr;	/* ptr to linked proc, if loaded */
	struct inode *x_iptr;	/* inode of prototype */
	short	x_rssize;
	short	x_swrss;
	char	x_count;	/* reference count */
	char	x_ccount;	/* number of loaded references */
	char	x_flag;		/* traced, written flags */
	char	x_slptime;
	short	x_poip;		/* page out in progress count */
};

#ifdef	KERNEL
extern	struct text text[];
#endif

#define	XTRC	01		/* Text may be written, exclusive use */
#define	XWRIT	02		/* Text written into, must swap out */
#define	XLOAD	04		/* Currently being read from file */
#define	XLOCK	010		/* Being swapped in or out */
#define	XWANT	020		/* Wanted for swapping */
#define	XPAGI	040		/* Page in on demand from inode */
