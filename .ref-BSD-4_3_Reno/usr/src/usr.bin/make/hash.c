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
 */

#ifndef lint
static char sccsid[] = "@(#)hash.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

/* hash.c --
 *
 * 	This module contains routines to manipulate a hash table.
 * 	See hash.h for a definition of the structure of the hash
 * 	table.  Hash tables grow automatically as the amount of
 * 	information increases.
 */

#include "sprite.h"
#include "hash.h"
#include "list.h"

/*
 * Forward references to local procedures that are used before they're
 * defined:
 */

static Hash_Entry *	ChainSearch();
static int		Hash();
static void		RebuildTable();

/* 
 * The following defines the ratio of # entries to # buckets
 * at which we rebuild the table to make it larger.
 */

static rebuildLimit = 3;

/*
 *---------------------------------------------------------
 * 
 * Hash_InitTable --
 *
 *	This routine just sets up the hash table.
 *
 * Results:	
 *	None.
 *
 * Side Effects:
 *	Memory is allocated for the initial bucket area.
 *
 *---------------------------------------------------------
 */

void
Hash_InitTable(tablePtr, numBuckets, keyType)
    register Hash_Table *tablePtr;	/* Structure to use to hold table. */
    int 	        numBuckets;	/* How many buckets to create for
					 * starters. This number is rounded
					 * up to a power of two.   If <= 0,
					 * a reasonable default is chosen.
					 * The table will grow in size later
					 * as needed. */
    int 	        keyType;	/* HASH_STRING_KEYS means that key
    					 * values passed to HashFind will be
					 * strings, passed via a (char *)
					 * pointer.  HASH_ONE_WORD_KEYS means
					 * that key values will be any
					 * one-word value passed as Address.
			 		 * > 1 means that key values will be 
				 	 * multi-word values whose address is
					 * passed as Address.  In this last
					 * case, keyType is the number of
					 * words in the key, not the number
					 * of bytes. */
{
    register	int 		i;
    register	List_Links 	*bucketPtr;

    /* 
     * Round up the size to a power of two. 
     */

    if (numBuckets <= 0) {
	numBuckets = 16;
    }
    tablePtr->numEntries = 0;
    tablePtr->keyType = keyType;
    tablePtr->size = 2;
    tablePtr->mask = 1;
    tablePtr->downShift = 29;
    while (tablePtr->size < numBuckets) {
	tablePtr->size <<= 1;
	tablePtr->mask = (tablePtr->mask << 1) + 1;
	tablePtr->downShift--;
    }

    tablePtr->bucketPtr = (List_Links *) emalloc(sizeof(List_Links)
	    * tablePtr->size);
    for (i=0, bucketPtr = tablePtr->bucketPtr; i < tablePtr->size;
	    i++, bucketPtr++) {
	List_Init(bucketPtr);
    }
}

/*
 *---------------------------------------------------------
 *
 * Hash_DeleteTable --
 *
 *	This routine removes everything from a hash table
 *	and frees up the memory space it occupied (except for
 *	the space in the Hash_Table structure).
 *
 * Results:	
 *	None.
 *
 * Side Effects:
 *	Lots of memory is freed up.
 *
 *---------------------------------------------------------
 */

void
Hash_DeleteTable(tablePtr)
    Hash_Table *tablePtr;		/* Hash table whose entries are all to
					 * be freed.  */
{
    register List_Links *hashTableEnd;
    register Hash_Entry *hashEntryPtr;
    register List_Links *bucketPtr;

    bucketPtr = tablePtr->bucketPtr;
    hashTableEnd = &(bucketPtr[tablePtr->size]);
    for (; bucketPtr < hashTableEnd; bucketPtr++) {
	while (!List_IsEmpty(bucketPtr)) {
	    hashEntryPtr = (Hash_Entry *) List_First(bucketPtr);
	    List_Remove((List_Links *) hashEntryPtr);
	    free((Address) hashEntryPtr);
	}
    }
    free((Address) tablePtr->bucketPtr);

    /*
     * Set up the hash table to cause memory faults on any future
     * access attempts until re-initialization.
     */

    tablePtr->bucketPtr = (List_Links *) NIL;
}

/*
 *---------------------------------------------------------
 *
 * Hash_FindEntry --
 *
 * 	Searches a hash table for an entry corresponding to key.
 *
 * Results:
 *	The return value is a pointer to the entry for key,
 *	if key was present in the table.  If key was not
 *	present, NULL is returned.
 *
 * Side Effects:
 *	None.
 *
 *---------------------------------------------------------
 */

Hash_Entry *
Hash_FindEntry(tablePtr, key)
    Hash_Table *tablePtr;	/* Hash table to search. */
    Address key;		/* A hash key. */
{
    return(ChainSearch(tablePtr, key,
	    &(tablePtr->bucketPtr[Hash(tablePtr, key)])));
}

/*
 *---------------------------------------------------------
 *
 * Hash_CreateEntry --
 *
 *	Searches a hash table for an entry corresponding to
 *	key.  If no entry is found, then one is created.
 *
 * Results:
 *	The return value is a pointer to the entry.  If *newPtr
 *	isn't NULL, then *newPtr is filled in with TRUE if a
 *	new entry was created, and FALSE if an entry already existed
 *	with the given key.
 *
 * Side Effects:
 *	Memory may be allocated, and the hash buckets may be modified.
 *---------------------------------------------------------
 */

Hash_Entry *
Hash_CreateEntry(tablePtr, key, newPtr)
    register Hash_Table *tablePtr;	/* Hash table to search. */
    Address key;			/* A hash key. */
    Boolean *newPtr;			/* Filled in with TRUE if new entry
    					 * created, FALSE otherwise. */
{
    register Hash_Entry *hashEntryPtr;
    register int 	*hashKeyPtr;
    register int 	*keyPtr;
    List_Links 		*hashList;

    keyPtr = (int *) key;

    hashList = &(tablePtr->bucketPtr[Hash(tablePtr, (Address) keyPtr)]);
    hashEntryPtr = ChainSearch(tablePtr, (Address) keyPtr, hashList);

    if (hashEntryPtr != (Hash_Entry *) NULL) {
	if (newPtr != NULL) {
	    *newPtr = FALSE;
	}
    	return hashEntryPtr;
    }

    /* 
     * The desired entry isn't there.  Before allocating a new entry,
     * see if we're overloading the buckets.  If so, then make a
     * bigger table.
     */

    if (tablePtr->numEntries >= rebuildLimit * tablePtr->size) {
	RebuildTable(tablePtr);
	hashList = &(tablePtr->bucketPtr[Hash(tablePtr, (Address) keyPtr)]);
    }
    tablePtr->numEntries += 1;

    /*
     * Not there, we have to allocate.  If the string is longer
     * than 3 bytes, then we have to allocate extra space in the
     * entry.
     */

    switch (tablePtr->keyType) {
	case HASH_STRING_KEYS:
	    hashEntryPtr = (Hash_Entry *) emalloc(sizeof(Hash_Entry) + 
		    strlen((char *) keyPtr) - 3);
	    strcpy(hashEntryPtr->key.name, (char *) keyPtr);
	    break;
	case HASH_ONE_WORD_KEYS:
	    hashEntryPtr = (Hash_Entry *) emalloc(sizeof(Hash_Entry));
	    hashEntryPtr->key.ptr = (Address) keyPtr;
	    break;
	case 2:
	    hashEntryPtr = 
		(Hash_Entry *) emalloc(sizeof(Hash_Entry) + sizeof(int));
	    hashKeyPtr = hashEntryPtr->key.words;
	    *hashKeyPtr++ = *keyPtr++;
	    *hashKeyPtr = *keyPtr;
	    break;
	default: {
	    register 	n;

	    n = tablePtr->keyType;
	    hashEntryPtr = (Hash_Entry *) 
		    emalloc(sizeof(Hash_Entry) + (n - 1) * sizeof(int));
	    hashKeyPtr = hashEntryPtr->key.words;
	    do { 
		*hashKeyPtr++ = *keyPtr++; 
	    } while (--n);
	    break;
	}
    }

    hashEntryPtr->clientData = (ClientData) NULL;
    List_Insert((List_Links *) hashEntryPtr, LIST_ATFRONT(hashList));

    if (newPtr != NULL) {
	*newPtr = TRUE;
    }
    return hashEntryPtr;
}

/*
 *---------------------------------------------------------
 *
 * Hash_DeleteEntry --
 *
 * 	Delete the given hash table entry and free memory associated with
 *	it.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Hash chain that entry lives in is modified and memory is freed.
 *
 *---------------------------------------------------------
 */

void
Hash_DeleteEntry(tablePtr, hashEntryPtr)
    Hash_Table			*tablePtr;
    register	Hash_Entry	*hashEntryPtr;
{
    if (hashEntryPtr != (Hash_Entry *) NULL) {
	List_Remove((List_Links *) hashEntryPtr);
	free((Address) hashEntryPtr);
	tablePtr->numEntries--;
    }
}

/*
 *---------------------------------------------------------
 *
 * Hash_EnumFirst --
 *	This procedure sets things up for a complete search
 *	of all entries recorded in the hash table.
 *
 * Results:	
 *	The return value is the address of the first entry in
 *	the hash table, or NULL if the table is empty.
 *
 * Side Effects:
 *	The information in hashSearchPtr is initialized so that successive
 *	calls to Hash_Next will return successive HashEntry's
 *	from the table.
 *
 *---------------------------------------------------------
 */

Hash_Entry *
Hash_EnumFirst(tablePtr, hashSearchPtr)
    Hash_Table *tablePtr;			/* Table to be searched. */
    register Hash_Search *hashSearchPtr;	/* Area in which to keep state 
						 * about search.*/
{
    hashSearchPtr->tablePtr = tablePtr;
    hashSearchPtr->nextIndex = 0;
    hashSearchPtr->hashEntryPtr = (Hash_Entry *) NULL;
    return Hash_EnumNext(hashSearchPtr);
}

/*
 *---------------------------------------------------------
 *
 * Hash_EnumNext --
 *    This procedure returns successive entries in the hash table.
 *
 * Results:
 *    The return value is a pointer to the next HashEntry
 *    in the table, or NULL when the end of the table is
 *    reached.
 *
 * Side Effects:
 *    The information in hashSearchPtr is modified to advance to the
 *    next entry.
 *
 *---------------------------------------------------------
 */

Hash_Entry *
Hash_EnumNext(hashSearchPtr)
    register Hash_Search *hashSearchPtr; /* Area used to keep state about 
					    search. */
{
    register List_Links *hashList;
    register Hash_Entry *hashEntryPtr;

    hashEntryPtr = hashSearchPtr->hashEntryPtr;
    while (hashEntryPtr == (Hash_Entry *) NULL ||
	   List_IsAtEnd(hashSearchPtr->hashList,
	   (List_Links *) hashEntryPtr)) {
	if (hashSearchPtr->nextIndex >= hashSearchPtr->tablePtr->size) {
	    return((Hash_Entry *) NULL);
	}
	hashList = &(hashSearchPtr->tablePtr->bucketPtr[
		hashSearchPtr->nextIndex]);
	hashSearchPtr->nextIndex++;
	if (!List_IsEmpty(hashList)) {
	    hashEntryPtr = (Hash_Entry *) List_First(hashList);
	    hashSearchPtr->hashList = hashList;
	    break;
	}
    }

    hashSearchPtr->hashEntryPtr = 
		(Hash_Entry *) List_Next((List_Links *) hashEntryPtr);

    return(hashEntryPtr);
}

/*
 *---------------------------------------------------------
 *
 * Hash_PrintStats --
 *
 *	This routine calls a caller-supplied procedure to print
 *	statistics about the current bucket situation.
 *
 * Results:	
 *	None.
 *
 * Side Effects:	
 *	Proc gets called (potentially many times) to output information
 *	about the hash table. It must have the following calling sequence:
 *
 *	void
 *	proc(clientData, string)
 *	    ClientData clientData;
 *	    char *string;
 *	{
 *	}
 *
 *	In each call, clientData is the same as the clientData argument
 *	to this procedure, and string is a null-terminated string of
 *	characters to output.
 *
 *---------------------------------------------------------
 */

void
Hash_PrintStats(tablePtr, proc, clientData)
    Hash_Table *tablePtr;		/* Table for which to print info. */
    void (*proc)();			/* Procedure to call to do actual
    					 * I/O. */
{
    int count[10], overflow, i, j;
    char msg[100];
    Hash_Entry 	*hashEntryPtr;
    List_Links	*hashList;

    for (i=0; i<10; i++) {
	count[i] = 0;
    }
    overflow = 0;
    for (i = 0; i < tablePtr->size; i++) {
	j = 0;
	hashList = &(tablePtr->bucketPtr[i]);
	LIST_FORALL(hashList, (List_Links *) hashEntryPtr) {
	    j++;
	}
	if (j < 10) {
	    count[j]++;
	} else {
	    overflow++;
	}
    }

    sprintf(msg, "Entries in table %d number of buckets %d\n", 
		tablePtr->numEntries, tablePtr->size);
    (*proc)(clientData, msg);
    for (i = 0;  i < 10; i++) {
	sprintf(msg, "Number of buckets with %d entries: %d.\n",
		i, count[i]);
	(*proc)(clientData, msg);
    }
    sprintf(msg, "Number of buckets with > 9 entries: %d.\n",
	    overflow);
    (*proc)(clientData, msg);
}

/*
 *---------------------------------------------------------
 *
 * Hash --
 *	This is a local procedure to compute a hash table
 *	bucket address based on a string value.
 *
 * Results:
 *	The return value is an integer between 0 and size - 1.
 *
 * Side Effects:	
 *	None.
 *
 * Design:
 *	It is expected that most keys are decimal numbers,
 *	so the algorithm behaves accordingly.  The randomizing
 *	code is stolen straight from the rand library routine.
 *
 *---------------------------------------------------------
 */

static int
Hash(tablePtr, key)
    register Hash_Table *tablePtr;
    register char 	*key;
{
    register int 	i = 0;
    register int 	j;
    register int 	*intPtr;

    switch (tablePtr->keyType) {
	case HASH_STRING_KEYS:
	    while (*key != 0) {
		i = (i * 10) + (*key++ - '0');
	    }
	    break;
	case HASH_ONE_WORD_KEYS:
	    i = (int) key;
	    break;
	case 2:
	    i = ((int *) key)[0] + ((int *) key)[1];
	    break;
	default:
	    j = tablePtr->keyType;
	    intPtr = (int *) key;
	    do { 
		i += *intPtr++; 
		j--;
	    } while (j > 0);
	    break;
    }


    return(((i*1103515245 + 12345) >> tablePtr->downShift) & tablePtr->mask);
}

/*
 *---------------------------------------------------------
 *
 * ChainSearch --
 *
 * 	Search the hash table for the entry in the hash chain.
 *
 * Results:
 *	Pointer to entry in hash chain, NULL if none found.
 *
 * Side Effects:
 *	None.
 *
 *---------------------------------------------------------
 */

static Hash_Entry *
ChainSearch(tablePtr, key, hashList)
    Hash_Table 		*tablePtr;	/* Hash table to search. */
    register Address	key;	/* A hash key. */
    register List_Links *hashList;
{
    register Hash_Entry *hashEntryPtr;
    register int 	*hashKeyPtr;
    int 		*keyPtr;
    register int	numKeys;

    numKeys = tablePtr->keyType;
    LIST_FORALL(hashList, (List_Links *) hashEntryPtr) {
	switch (numKeys) {
	    case 0:
		if (strcmp(hashEntryPtr->key.name, key) == 0) {
		    return(hashEntryPtr);
		}
		break;
	    case 1:
		if (hashEntryPtr->key.ptr == key) {
		    return(hashEntryPtr);
		}
		break;
	    case 2:
		hashKeyPtr = hashEntryPtr->key.words;
		keyPtr = (int *) key;
		if (*hashKeyPtr++ == *keyPtr++ && *hashKeyPtr == *keyPtr) {
		    return(hashEntryPtr);
		}
		break;
	    default:
		if (bcmp((Address) hashEntryPtr->key.words,
			    (Address) key, numKeys * sizeof(int))) {
		    return(hashEntryPtr);
		}
		break;
	}
    }

    /* 
     * The desired entry isn't there 
     */

    return ((Hash_Entry *) NULL);
}

/*
 *---------------------------------------------------------
 *
 * RebuildTable --
 *	This local routine makes a new hash table that
 *	is larger than the old one.
 *
 * Results:	
 * 	None.
 *
 * Side Effects:
 *	The entire hash table is moved, so any bucket numbers
 *	from the old table are invalid.
 *
 *---------------------------------------------------------
 */

static void
RebuildTable(tablePtr)
    Hash_Table 	*tablePtr;		/* Table to be enlarged. */
{
    int 		 oldSize;
    int 		 bucket;
    List_Links		 *oldBucketPtr;
    register Hash_Entry  *hashEntryPtr;
    register List_Links	 *oldHashList;

    oldBucketPtr = tablePtr->bucketPtr;
    oldSize = tablePtr->size;

    /* 
     * Build a new table 4 times as large as the old one. 
     */

    Hash_InitTable(tablePtr, tablePtr->size * 4, tablePtr->keyType);

    for (oldHashList = oldBucketPtr; oldSize > 0; oldSize--, oldHashList++) {
	while (!List_IsEmpty(oldHashList)) {
	    hashEntryPtr = (Hash_Entry *) List_First(oldHashList);
	    List_Remove((List_Links *) hashEntryPtr);
	    switch (tablePtr->keyType) {
		case HASH_STRING_KEYS:
		    bucket = Hash(tablePtr, (Address) hashEntryPtr->key.name);
		    break;
		case HASH_ONE_WORD_KEYS:
		    bucket = Hash(tablePtr, (Address) hashEntryPtr->key.ptr);
		    break;
		default:
		    bucket = Hash(tablePtr, (Address) hashEntryPtr->key.words);
		    break;
	    }
	    List_Insert((List_Links *) hashEntryPtr, 
		LIST_ATFRONT(&(tablePtr->bucketPtr[bucket])));
	    tablePtr->numEntries++;
	}
    }

    free((Address) oldBucketPtr);
}
