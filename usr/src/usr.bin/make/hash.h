/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * Copyright (c) 1988, 1989 by Adam de Boor
 * Copyright (c) 1989 by Berkeley Softworks
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)hash.h	5.3 (Berkeley) 6/1/90
 */

/* hash.h --
 *
 * 	This file contains definitions used by the hash module,
 * 	which maintains hash tables.
 */

#ifndef	_HASH
#define	_HASH

#include "list.h"
/* 
 * The following defines one entry in the hash table.
 */

typedef struct Hash_Entry {
    List_Links	      links;		/* Used to link together all the
    					 * entries associated with the same
					 * bucket. */
    ClientData	      clientData;	/* Arbitrary piece of data associated
    					 * with key. */
    union {
	Address	 ptr;			/* One-word key value to identify
					 * entry. */
	int words[1];			/* N-word key value.  Note: the actual
					 * size may be longer if necessary to
					 * hold the entire key. */
	char 	 name[4];		/* Text name of this entry.  Note: the
					 * actual size may be longer if
					 * necessary to hold the whole string.
					 * This MUST be the last entry in the
					 * structure!!! */
    } key;
} Hash_Entry;

/* 
 * A hash table consists of an array of pointers to hash
 * lists.  Tables can be organized in either of three ways, depending
 * on the type of comparison keys:
 *
 *	Strings:	  these are NULL-terminated; their address
 *			  is passed to HashFind as a (char *).
 *	Single-word keys: these may be anything, but must be passed
 *			  to Hash_Find as an Address.
 *	Multi-word keys:  these may also be anything; their address
 *			  is passed to HashFind as an Address.
 *
 *	Single-word keys are fastest, but most restrictive.
 */
 
#define HASH_STRING_KEYS	0
#define HASH_ONE_WORD_KEYS	1

typedef struct Hash_Table {
    List_Links 	*bucketPtr;	/* Pointer to array of List_Links, one
    				 * for each bucket in the table. */
    int 	size;		/* Actual size of array. */
    int 	numEntries;	/* Number of entries in the table. */
    int 	downShift;	/* Shift count, used in hashing function. */
    int 	mask;		/* Used to select bits for hashing. */
    int 	keyType;	/* Type of keys used in table:
    				 * HASH_STRING_KEYS, HASH_ONE-WORD_KEYS,
				 * or >1 menas keyType gives number of words
				 * in keys.
				 */
} Hash_Table;

/* 
 * The following structure is used by the searching routines
 * to record where we are in the search.
 */

typedef struct Hash_Search {
    Hash_Table  *tablePtr;	/* Table being searched. */
    int 	nextIndex;	/* Next bucket to check (after current). */
    Hash_Entry 	*hashEntryPtr;	/* Next entry to check in current bucket. */
    List_Links	*hashList;	/* Hash chain currently being checked. */
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
 *     HashEntry *h; 
 *     char *val; 
 */

#define Hash_SetValue(h, val) ((h)->clientData = (ClientData) (val))

/* 
 * Hash_Size(n) returns the number of words in an object of n bytes 
 */

#define	Hash_Size(n)	(((n) + sizeof (int) - 1) / sizeof (int))

/*
 * The following procedure declarations and macros
 * are the only things that should be needed outside
 * the implementation code.
 */

extern Hash_Entry *	Hash_CreateEntry();
extern void		Hash_DeleteTable();
extern void		Hash_DeleteEntry();
extern void		Hash_DeleteTable();
extern Hash_Entry *	Hash_EnumFirst();
extern Hash_Entry *	Hash_EnumNext();
extern Hash_Entry *	Hash_FindEntry();
extern void		Hash_InitTable();
extern void		Hash_PrintStats();

#endif _HASH
