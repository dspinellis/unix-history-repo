/* $Header$ */

/*
 * Singly-linked list definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Singly-linked list macros
 */
#define SLNUM(slist)	(slist)->nk
/*
 * Singly-linked list block
 */
typedef struct slblk
	{
	char *key;			/* points to a key */
	struct slblk *next;		/* ptr to next list block */
	} SLBLK;
/*
 * Singly-linked list head block
 */
typedef struct slisthb
	{
	int nk;				/* number of keys in list */
	int maxkey;			/* length of longest key */
	SLBLK *head;			/* pointer to first list block */
	SLBLK *curblk;			/* pointer to current block */
	SLBLK *tail;			/* pointer to last list block */
	} SLIST;
/*
 * Functions defined for singly-linked list operations
 */
extern char *slappend();		/* append key */
extern char *slget();			/* get next key */
extern SLIST *slinit();			/* initialize list */
extern char *slinsert();		/* insert key */
extern int slpop();			/* pop key */
extern char *slprepend();		/* prepend key */
extern void slprint();			/* print list */
extern void slrewind();			/* rewind list */
extern void slrm();			/* remove list item */
extern int slsort();			/* sort list */
extern void slsplice();			/* splice two lists */
