/*	buf.h	4.11	81/02/27	*/

/*
 * The header for buffers in the buffer pool and otherwise used
 * to describe a block i/o request is given here.  The routines
 * which manipulate these things are given in bio.c.
 *
 * Each buffer in the pool is usually doubly linked into 2 lists:
 * hashed into a chain by <dev,blkno> so it can be located in the cache,
 * and (usually) on (one of several) queues.  These lists are circular and
 * doubly linked for easy removal.
 *
 * There are currently three queues for buffers:
 *	one for buffers which must be kept permanently (super blocks)
 * 	one for buffers containing ``useful'' information (the cache)
 *	one for buffers containing ``non-useful'' information
 *		(and empty buffers, pushed onto the front)
 * The latter two queues contain the buffers which are available for
 * reallocation, are kept in lru order.  When not on one of these queues,
 * the buffers are ``checked out'' to drivers which use the available list
 * pointers to keep track of them in their i/o active queues.
 */

/*
 * Bufhd structures used at the head of the hashed buffer queues.
 * We only need three words for these, so this abbreviated
 * definition saves some space.
 */
struct bufhd
{
	long	b_flags;		/* see defines below */
	struct	buf *b_forw, *b_back;	/* fwd/bkwd pointer in chain */
};
struct buf
{
	long	b_flags;		/* too much goes here to describe */
	struct	buf *b_forw, *b_back;	/* hash chain (2 way street) */
	struct	buf *av_forw, *av_back;	/* position on free list if not BUSY */
#define	b_actf	av_forw			/* alternate names for driver queue */
#define	b_actl	av_back			/*    head - isn't history wonderful */
	long	b_bcount;		/* transfer count */
#define	b_active b_bcount		/* driver queue head: drive active */
	short	b_error;		/* returned after I/O */
	dev_t	b_dev;			/* major+minor device name */
	union {
	    caddr_t b_addr;		/* low order core address */
	    int	*b_words;		/* words for clearing */
	    struct filsys *b_filsys;	/* superblocks */
	    struct dinode *b_dino;	/* ilist */
	    daddr_t *b_daddr;		/* indirect block */
	} b_un;
	daddr_t	b_blkno;		/* block # on device */
	long	b_resid;		/* words not transferred after error */
#define	b_errcnt b_resid		/* while i/o in progress: # retries */
#define	b_pfcent b_resid		/* garbage: don't ask */
	struct  proc *b_proc;		/* proc doing physical or swap I/O */
};

#define	BQUEUES		3		/* number of free buffer queues */
#define	BQ_LOCKED	0		/* super-blocks &c */
#define	BQ_LRU		1		/* lru, useful buffers */
#define	BQ_AGE		2		/* rubbish */

#ifdef	KERNEL
extern	struct buf *buf;		/* the buffer pool itself */
extern	char *buffers;
extern	int nbuf;
extern	struct buf *swbuf;		/* swap I/O headers */
extern	int nswbuf;
extern	short *swsize;
extern	int *swpf;
extern	struct buf bfreelist[BQUEUES];	/* heads of available lists */
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

/*
 * These flags are kept in b_flags.
 */
#define	B_WRITE		0x00000		/* non-read pseudo-flag */
#define	B_READ		0x00001		/* read when I/O occurs */
#define	B_DONE		0x00002		/* transaction finished */
#define	B_ERROR		0x00004		/* transaction aborted */
#define	B_BUSY		0x00008		/* not on av_forw/back list */
#define	B_PHYS		0x00010		/* physical IO */
#define	B_XXX		0x00020		/* was B_MAP, alloc UNIBUS on pdp-11 */
#define	B_WANTED	0x00040		/* issue wakeup when BUSY goes off */
#define	B_AGE		0x00080		/* delayed write for correct aging */
#define	B_ASYNC		0x00100		/* don't wait for I/O completion */
#define	B_DELWRI	0x00200		/* write at exit of avail list */
#define	B_TAPE		0x00400		/* this is a magtape (no bdwrite) */
#define	B_UAREA		0x00800		/* add u-area to a swap operation */
#define	B_PAGET		0x01000		/* page in/out of page table space */
#define	B_DIRTY		0x02000		/* dirty page to be pushed out async */
#define	B_PGIN		0x04000		/* pagein op, so swap() can count it */
#define	B_CACHE		0x08000		/* did bread find us in the cache ? */
#define	B_INVAL		0x10000		/* does not contain valid info  */
#define	B_LOCKED	0x20000		/* locked in core (not reusable) */
#define	B_HEAD		0x40000		/* a buffer header, not a buffer */
