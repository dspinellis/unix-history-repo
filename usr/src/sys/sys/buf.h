/*	buf.h	3.3	%H%	*/

/*
 * Each buffer in the pool is usually doubly linked into 2 lists:
 * the device with which it is currently associated (always)
 * and also on a list of blocks available for allocation
 * for other use (usually).
 * The latter list is kept in last-used order, and the two
 * lists are doubly linked to make it easy to remove
 * a buffer from one list when it was found by
 * looking through the other.
 * A buffer is on the available list, and is liable
 * to be reassigned to another disk block, if and only
 * if it is not marked BUSY.  When a buffer is busy, the
 * available-list pointers can be used for other purposes.
 * Most drivers use the forward ptr as a link in their I/O
 * active queue.
 * A buffer header contains all the information required
 * to perform I/O.
 * Most of the routines which manipulate these things
 * are in bio.c.
 */
struct buf
{
	int	b_flags;		/* see defines below */
	struct	buf *b_forw;		/* headed by d_tab of conf.c */
	struct	buf *b_back;		/*  "  */
	struct	buf *av_forw;		/* position on free list, */
	struct	buf *av_back;		/*     if not BUSY*/
	dev_t	b_dev;			/* major+minor device name */
	unsigned b_bcount;		/* transfer count */
	union {
	    caddr_t b_addr;		/* low order core address */
	    int	*b_words;		/* words for clearing */
	    struct filsys *b_filsys;	/* superblocks */
	    struct dinode *b_dino;	/* ilist */
	    daddr_t *b_daddr;		/* indirect block */
	} b_un;
	daddr_t	b_blkno;		/* block # on device */
	char	b_xmem;			/* high order core address */
	char	b_error;		/* returned after I/O */
	short	b_hlink;		/* hash links for buffer cache */
	unsigned int b_resid;		/* words not transferred after error */
	struct  proc  *b_proc;		/* process doing physical or swap I/O */
};

#ifdef	KERNEL
extern	struct buf buf[];		/* The buffer pool itself */
extern	struct buf swbuf[];		/* swap I/O headers */
extern	struct buf bfreelist;		/* head of available list */
extern	struct buf bswlist;		/* head of free swap header list */
extern	struct buf *bclnlist;		/* head of cleaned page list */

struct	buf *alloc();
struct	buf *baddr();
struct	buf *getblk();
struct	buf *geteblk();
struct	buf *bread();
struct	buf *breada();

unsigned minphys();
#endif

#define	NSWBUF	48			/* number of swap I/O headers */

/*
 * These flags are kept in b_flags.
 */
#define	B_WRITE	 0x0000	/* non-read pseudo-flag */
#define	B_READ		0x0001	/* read when I/O occurs */
#define	B_DONE		0x0002	/* transaction finished */
#define	B_ERROR		0x0004	/* transaction aborted */
#define	B_BUSY		0x0008	/* not on av_forw/back list */
#define	B_PHYS		0x0010	/* physical IO */
#define	B_MAP		0x0020	/* UNIBUS map allocated */
#define	B_WANTED	0x0040	/* issue wakeup when BUSY goes off */
#define	B_AGE		0x0080	/* delayed write for correct aging */
#define	B_ASYNC		0x0100	/* don't wait for I/O completion */
#define	B_DELWRI	0x0200	/* write at exit of avail list */
#define	B_TAPE		0x0400	/* this is a magtape (no bdwrite) */
#define	B_UAREA		0x0800	/* add u-area to a swap operation */
#define	B_PAGET		0x1000	/* page in/out of page table space */
#define	B_DIRTY		0x2000	/* dirty page to be pushed out async */
#define	B_PGIN		0x4000	/* pagein op, so swap() can count it */
#define	B_CACHE		0x8000	/* did bread find us in the cache ? */

/*
 * special redeclarations for
 * the head of the queue per
 * device driver.
 */
#define	b_actf		av_forw
#define	b_actl		av_back
#define	b_active	b_bcount
#define	b_errcnt	b_resid
#define	b_pfcent	b_resid
