/* Copyright (c) 1979 Regents of the University of California */
/*
 *	To speed up walks through symbols defined in a particular
 *	segment, we buil up a table of pointers into the symbol table
 *	and a table of delimiters for each segment.  The delimiter for
 *	the particular segment points to the first word in that segment.
 */

struct	symtab	**symptrs;		/*dynamically allocated*/
struct	symtab	**symdelim[NLOC + NLOC + 1];
struct	symtab	*hshtab[NHASH];
struct	symtab	**symptrub;
	int	nsyms;			/*number in the symbol table*/
	int	njxxx;			/*the number of jxxx entries in the table*/
	int	nforgotten;		/*how many entries erroneously entered*/
	int	nlabels;		/*how many labels in the symbol table*/
	int	hshused;		/*how many hash slots used*/

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
 *	the data area.  This way, it is extremely easy to
 *	allow virtual memory temporary files, change the length
 *	of NCPS, and allows for a much more flexible storage
 *	allocation
 */

#define SYMDALLOP	200
struct 	allocbox{
	struct		allocbox	*nextalloc;
	struct		symtab		symslots[SYMDALLOP];
	char				symnames[SYMDALLOP * NCPS];
};

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
