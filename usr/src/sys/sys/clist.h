/*	clist.h	4.3	81/02/27	*/

/*
 * Raw structures for the character list routines.
 */
struct cblock {
	struct cblock *c_next;
	char	c_info[CBSIZE];
};
#ifdef KERNEL
extern	struct	cblock *cfree;
extern	int nclist;
struct	cblock *cfreelist;
int	cfreecount;
#endif
