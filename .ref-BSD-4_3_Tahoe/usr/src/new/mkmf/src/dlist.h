/* $Header: dlist.h,v 1.1 85/04/23 13:56:24 nicklin Exp $ */

/*
 * Dependency list definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Dependency list block
 */
typedef struct _dlblk
	{
	int d_type;			/* source file type */
	struct slblk *d_src;		/* points to a source list block */
	struct _iblk *d_incl;		/* pointer to include block chain */
	struct _dlblk *d_next;		/* ptr to next list block */
	} DLBLK;
/*
 * Dependency list head block
 */
typedef struct _dlisthb
	{
	DLBLK *d_head;			/* pointer to first list block */
	DLBLK *d_tail;			/* pointer to last list block */
	} DLIST;
/*
 * Functions defined for dependency list operations
 */
extern DLBLK *dlappend();		/* append to list */
extern DLIST *dlinit();			/* initialize list */
extern void dlprint();			/* print list */
