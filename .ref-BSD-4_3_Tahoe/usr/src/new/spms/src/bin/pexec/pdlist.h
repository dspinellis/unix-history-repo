/* $Header$ */

/*
 * Project directory list definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Singly-linked project directory list macros
 */
#define pdnum(pdlist)	(pdlist)->nd
/*
 * Singly-linked project directory list block
 */
typedef struct _pdblk
	{
	char *ppath;			/* project directory project pathname */
	char *rpath;			/* project directory regular pathname */
	char *project;			/* project directory's project */
	struct _pdblk *next;		/* ptr to next list block */
	} PDBLK;
/*
 * Singly-linked project directory list head block
 */
typedef struct _pdlisthb
	{
	int nd;				/* number of directories in list */
	PDBLK *head;			/* pointer to first list block */
	} PDLIST;
/*
 * Functions defined for singly-linked project directory list operations
 */
extern PDLIST *pdinit();		/* initialize project directory list */
extern char *pdprepend();		/* prepend project directory */
extern void pdrm();			/* remove project directory list */
extern void pdsort();			/* sort project directory list */
