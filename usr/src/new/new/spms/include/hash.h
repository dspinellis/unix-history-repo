/* $Header$ */

/*
 * Hash table definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Hash table block
 */
typedef struct _hblk
	{
	char *h_key;			/* points to key */
	char *h_def;			/* points to definition string */
	int h_val;			/* integer value */
	struct _hblk *h_next;		/* ptr to next block */
	} HASHBLK;
/*
 * Hash pointer table struct
 */
typedef struct _hash
	{
	HASHBLK **hashtab;		/* hash pointer table */
	int hashsiz;			/* hash table size */
	} HASH;
/*
 * Functions defined for hash tables
 */
extern HASHBLK *htbrm();		/* remove hash table block */
extern int hthash();			/* compute hash value */
extern HASH *htinit();			/* initialize hash table */
extern HASHBLK *htinstall();		/* install hash table entry */
extern HASHBLK *htlookup();		/* find hash table entry */
extern void htrm();			/* remove hash table entry */
