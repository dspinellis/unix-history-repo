/*	cmap.h	2.1	1/5/80	*/

/*
 * core map entry
 *
 * N.B.: sizeof (struct cmap) is well known in cmap.m
 */
struct cmap
{
unsigned int 	c_next:14,	/* index of next free list entry */
		c_page:17,	/* virtual page number in segment */
		c_intrans:1,	/* intransit bit */
		c_prev:14,	/* index of previous free list entry */
		c_ndx:10,	/* index of owner proc or text */
		c_flag:8;	/* flags */
};

#define	CMHEAD	0

#ifdef	KERNEL
extern	struct cmap *cmap;
extern	struct cmap *ecmap;
int	firstfree, maxfree;
#endif

/* bits defined in c_flag */

#define	MTEXT		0x01		/* belongs to shared text segment */
#define	MDATA		0x02		/* belongs to data segment */
#define	MSTACK		0x04		/* belongs to stack segment */
#define	MSYS		0x08		/* allocated to u area */
#define	MGONE		0x10		/* associated page has been released */
#define	MFREE		0x20		/* on the free list */
#define	MLOCK		0x40		/* locked for raw i/o or pagein */
#define	MWANT		0x80		/* wanted */

#define	pgtocm(x)	((((x)-firstfree) / CLSIZE) + 1)
#define	cmtopg(x)	((((x)-1) * CLSIZE) + firstfree)
