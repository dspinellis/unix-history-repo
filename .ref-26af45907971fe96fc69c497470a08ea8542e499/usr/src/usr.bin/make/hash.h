/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * Copyright (c) 1988, 1989 by Adam de Boor
 * Copyright (c) 1989 by Berkeley Softworks
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)hash.h	5.5 (Berkeley) %G%
 */

/* hash.h --
 *
 * 	This file contains definitions used by the hash module,
 * 	which maintains hash tables.
 */

#ifndef	_HASH
#define	_HASH

/* 
 * The following defines one entry in the hash table.
 */

typedef struct Hash_Entry {
    struct Hash_Entry *next;		/* Used to link together all the
    					 * entries associated with the same
					 * bucket. */
    ClientData	      clientData;	/* Arbitrary piece of data associated
    					 * with key. */
    unsigned	      namehash;		/* hash value of key */
    char	      name[1];		/* key string */
} Hash_Entry;

typedef struct Hash_Table {
    struct Hash_Entry **bucketPtr;/* Pointers to Hash_Entry, one
    				 * for each bucket in the table. */
    int 	size;		/* Actual size of array. */
    int 	numEntries;	/* Number of entries in the table. */
    int 	mask;		/* Used to select bits for hashing. */
} Hash_Table;

/* 
 * The following structure is used by the searching routines
 * to record where we are in the search.
 */

typedef struct Hash_Search {
    Hash_Table  *tablePtr;	/* Table being searched. */
    int 	nextIndex;	/* Next bucket to check (after current). */
    Hash_Entry 	*hashEntryPtr;	/* Next entry to check in current bucket. */
} Hash_Search;

/*
 * Macros.
 */

/*
 * ClientData Hash_GetValue(h) 
 *     Hash_Entry *h; 
 */

#define Hash_GetValue(h) ((h)->clientData)

/* 
 * Hash_SetValue(h, val); 
 *     Hash_Entry *h; 
 *     char *val; 
 */

#define Hash_SetValue(h, val) ((h)->clientData = (ClientData) (val))

/* 
 * Hash_Size(n) returns the number of words in an object of n bytes 
 */

#define	Hash_Size(n)	(((n) + sizeof (int) - 1) / sizeof (int))

Hash_Entry	*Hash_CreateEntry __P((Hash_Table *, char *, Boolean *));
void		 Hash_DeleteEntry __P((Hash_Table *, Hash_Entry *));
void		 Hash_DeleteTable __P((Hash_Table *));
Hash_Entry	*Hash_EnumFirst __P((Hash_Table *, Hash_Search *));
Hash_Entry	*Hash_EnumNext __P((Hash_Search *));
Hash_Entry	*Hash_FindEntry __P((Hash_Table *, char *));
void		 Hash_InitTable __P((Hash_Table *, int));

#endif /* _HASH */
