/* $Header$ */

/*
 * Singly-linked key+string list definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Singly-linked key+string list macros
 */
#define SLSNUM(slslist)	(slslist)->nk
/*
 * Singly-linked key+string list block
 */
typedef struct _slsblk
	{
	char *key;			/* points to key */
	char *string;			/* points to non-key string */
	struct _slsblk *next;		/* ptr to next list block */
	} SLSBLK;
/*
 * Singly-linked key+string list head block
 */
typedef struct _slslisthb
	{
	int nk;				/* number of keys in list */
	int maxkey;			/* length of longest key */
	int maxstr;			/* length of longest non-key string */
	SLSBLK *head;			/* pointer to first list block */
	SLSBLK *curblk;			/* pointer to current block */
	SLSBLK *tail;			/* pointer to last list block */
	} SLSLIST;
/*
 * Functions defined for singly-linked key+string list operations
 */
extern char *slsappend();		/* append key+string */
extern SLSBLK *slsget();		/* get next key+string block */
extern SLSLIST *slsinit();		/* initialize key+string list */
extern char *slsinsert();		/* insert key+string */
extern char *slsprepend();		/* prepend key+string */
extern void slsprint();			/* print key+string list */
extern void slsrewind();		/* rewind key+string list */
extern void slsrm();			/* remove key+string list item */
extern int slssort();			/* sort key+string list */
