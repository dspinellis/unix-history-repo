/*
 *	Copyright (c) 1982 Regents of the University of California
 *	@(#)assyms.h 4.5 6/30/83
 */
/*
 *	To speed up walks through symbols defined in a particular
 *	segment, we buil up a table of pointers into the symbol table
 *	and a table of delimiters for each segment.  The delimiter for
 *	the particular segment points to the first word in that segment.
 */

extern	struct	symtab	**symptrs;		/*dynamically allocated*/
extern	struct	symtab	**symdelim[NLOC + NLOC + 1];
extern	struct	symtab	**symptrub;
extern	int	nsyms;			/*number in the symbol table*/
extern	int	njxxx;			/*the number of jxxx entries in the table*/
extern	int	nforgotten;		/*how many entries erroneously entered*/
extern	int	nlabels;		/*how many labels in the symbol table*/
extern	int	hshused;		/*how many hash slots used*/

#define SEGITERATE(segno, start, end, copointer, walkpointer, ubpointer, direction) \
	for(copointer = start == 0? symdelim[segno]:start,\
	    ubpointer = end == 0 ? *symdelim[segno+1] : *(symdelim[segno]-1),\
	    walkpointer = *copointer;\
	    walkpointer != ubpointer;\
	    walkpointer = * direction copointer)

#define SYMITERATE(copointer, walkpointer) \
	for(copointer = symptrs, \
	    walkpointer = *copointer; \
	    copointer < symptrub; \
	    walkpointer = * ++ copointer)
/*
 *	Symbols are allocated in non contiguous chunks by extending
 *	the data area.
 */

#define SYMDALLOP	200
struct 	allocbox{
	struct		allocbox	*nextalloc;
	struct		symtab		symslots[SYMDALLOP];
};

/*
 *	Names are allocated in a dynamically extensible string pool.
 */
struct	strpool{
	struct	strpool	*str_next;
	int		str_nalloc;
	char		str_names[STRPOOLDALLOP];
};

extern	struct	strpool *strplhead;

extern	struct	allocbox	*allochead;
extern	struct	allocbox	*alloctail;
extern	struct	symtab		*nextsym;
extern	struct	allocbox	*newbox;
extern	char			*namebuffer;
extern	int			symsleft;

#define ALLOCQTY 	sizeof (struct allocbox)
/*
 *	Iterate through all symbols in the symbol table in declaration
 *	order
 */
#define DECLITERATE(allocwalk, walkpointer, ubpointer) \
	for(allocwalk = allochead; \
	    allocwalk != 0; \
	    allocwalk = allocwalk->nextalloc) \
		for (walkpointer = &allocwalk->symslots[0],\
		        ubpointer = &allocwalk->symslots[SYMDALLOP], \
		        ubpointer = ubpointer > ( (struct symtab *)alloctail) \
				 ? nextsym : ubpointer ;\
		     walkpointer < ubpointer; \
		     walkpointer++ )
/*
 *	The hash table is segmented, and dynamically extendable.
 *	We have a linked list of hash table segments; within each
 *	segment we use a quadratic rehash that touches no more than 1/2
 *	of the buckets in the hash table when probing.
 *	If the probe does not find the desired symbol, it moves to the
 *	next segment, or allocates a new segment.
 *
 *	Hash table segments are kept on the linked list with the first
 *	segment always first (that contains the reserved words) and
 *	the last added segment immediately after the first segment
 *	to hopefully gain something by locality of reference.
 */
struct hashdallop {
	int	h_nused;
	struct	hashdallop	*h_next;
	struct	symtab		*h_htab[NHASH];
};
