/* $Id: hash.c,v 3.0 1992/12/14 00:14:13 davison Trn $
*/
/* This file is an altered version of a set of hash routines by
** Geoffrey Collyer.  See the end of the file for his copyright.
*/

#include "EXTERN.h"
#include "common.h"
#include "util.h"
#include "INTERN.h"
#include "hash.h"

/* tunable parameters */
#define RETAIN 600		/* retain & recycle this many HASHENTs */

static HASHENT *hereuse = NULL;
static int reusables = 0;

static HASHENT **hashfind _((HASHTABLE*,char*,int));
static unsigned hash _((char*,int));
static int default_cmp _((char*,int,HASHDATUM));
static HASHENT *healloc _((void));
static void hefree _((HASHENT*));

HASHTABLE *
hashcreate(size, cmpfunc)
unsigned size;			/* a crude guide to size */
int (*cmpfunc)();
{
    register HASHTABLE *tbl;
    /* allocate HASHTABLE and (HASHENT*) array together to reduce the
    ** number of malloc calls. */
    register struct alignalloc {
	HASHTABLE ht;
	HASHENT *hepa[1];	/* longer than it looks */
    } *aap;

    aap = (struct alignalloc*)
	safemalloc(sizeof *aap + (size-1)*sizeof (HASHENT*));
    bzero((char*)aap, sizeof *aap + (size-1)*sizeof (HASHENT*));
    tbl = &aap->ht;
    tbl->ht_size = (size == 0? 1: size);	/* size of 0 is nonsense */
    tbl->ht_magic = HASHMAG;
    tbl->ht_cmp = (cmpfunc == NULL? default_cmp: cmpfunc);
    tbl->ht_addr = aap->hepa;
    return tbl;
}

/* Free all the memory associated with tbl, erase the pointers to it, and
** invalidate tbl to prevent further use via other pointers to it.
*/
void
hashdestroy(tbl)
register HASHTABLE *tbl;
{
    register unsigned idx;
    register HASHENT *hp, *next;
    register HASHENT **hepp;
    register int tblsize;

    if (tbl == NULL || BADTBL(tbl))
	return;
    tblsize = tbl->ht_size;
    hepp = tbl->ht_addr;
    for (idx = 0; idx < tblsize; idx++) {
	for (hp = hepp[idx]; hp != NULL; hp = next) {
	    next = hp->he_next;
	    hp->he_next = NULL;
	    hefree(hp);
	}
	hepp[idx] = NULL;
    }
    tbl->ht_magic = 0;			/* de-certify this table */
    tbl->ht_addr = NULL;
    free((char*)tbl);
}

void
hashstore(tbl, key, keylen, data)
register HASHTABLE *tbl;
char *key;
int keylen;
HASHDATUM data;
{
    register HASHENT *hp;
    register HASHENT **nextp;

    nextp = hashfind(tbl, key, keylen);
    hp = *nextp;
    if (hp == NULL) {			/* absent; allocate an entry */
	hp = healloc();
	hp->he_next = NULL;
	hp->he_keylen = keylen;
	*nextp = hp;			/* append to hash chain */
    }
    hp->he_data = data;		/* supersede any old data for this key */
}

void
hashdelete(tbl, key, keylen)
register HASHTABLE *tbl;
char *key;
int keylen;
{
    register HASHENT *hp;
    register HASHENT **nextp;

    nextp = hashfind(tbl, key, keylen);
    hp = *nextp;
    if (hp == NULL)			/* absent */
	return;
    *nextp = hp->he_next;		/* skip this entry */
    hp->he_next = NULL;
    hp->he_data.dat_ptr = NULL;
    hefree(hp);
}

HASHENT **slast_nextp;
int slast_keylen;

HASHDATUM				/* data corresponding to key */
hashfetch(tbl, key, keylen)
register HASHTABLE *tbl;
char *key;
int keylen;
{
    register HASHENT *hp;
    register HASHENT **nextp;
    static HASHDATUM errdatum = { NULL, 0 };

    nextp = hashfind(tbl, key, keylen);
    slast_nextp = nextp;
    slast_keylen = keylen;
    hp = *nextp;
    if (hp == NULL)			/* absent */
	return errdatum;
    else
	return hp->he_data;
}

void
hashstorelast(data)
HASHDATUM data;
{
    register HASHENT *hp;

    hp = *slast_nextp;
    if (hp == NULL) {			/* absent; allocate an entry */
	hp = healloc();
	hp->he_next = NULL;
	hp->he_keylen = slast_keylen;
	*slast_nextp = hp;		/* append to hash chain */
    }
    hp->he_data = data;		/* supersede any old data for this key */
}

/* Visit each entry by calling nodefunc at each, with key, data and extra as
** arguments.
*/
void
hashwalk(tbl, nodefunc, extra)
HASHTABLE *tbl;
register void (*nodefunc)();
register int extra;
{
    register unsigned idx;
    register HASHENT *hp;
    register HASHENT **hepp;
    register int tblsize;

    if (BADTBL(tbl))
	return;
    hepp = tbl->ht_addr;
    tblsize = tbl->ht_size;
    for (idx = 0; idx < tblsize; idx++)
	for (hp = hepp[idx]; hp != NULL; hp = hp->he_next)
	    (*nodefunc)(&hp->he_data, extra);
}

/* The returned value is the address of the pointer that refers to the
** found object.  Said pointer may be NULL if the object was not found;
** if so, this pointer should be updated with the address of the object
** to be inserted, if insertion is desired.
*/
static HASHENT **
hashfind(tbl, key, keylen)
register HASHTABLE *tbl;
char *key;
register int keylen;
{
    register HASHENT *hp, *prevhp = NULL;
    register HASHENT **hepp;
    register unsigned size; 

    if (BADTBL(tbl))
	fatal_error("Hash table is invalid.");
    size = tbl->ht_size;
    hepp = &tbl->ht_addr[hash(key,keylen) % size];
    for (hp = *hepp; hp != NULL; prevhp = hp, hp = hp->he_next) {
	if (hp->he_keylen == keylen && !(*tbl->ht_cmp)(key, keylen, hp->he_data))
	    break;
    }
    /* assert: *(returned value) == hp */
    return (prevhp == NULL? hepp: &prevhp->he_next);
}

static unsigned				/* not yet taken modulus table size */
hash(key, keylen)
register char *key;
register int keylen;
{
    register unsigned hash = 0;

    while (keylen--)
	hash += *key++;
    return hash;
}

static int
default_cmp(key, keylen, data)
char *key;
int keylen;
HASHDATUM data;
{
    /* We already know that the lengths are equal, just compare the strings */
    return bcmp(key, data.dat_ptr, keylen);
}

static HASHENT *
healloc()				/* allocate a hash entry */
{
    register HASHENT *hp;

    if (hereuse == NULL)
	return (HASHENT*)safemalloc(sizeof (HASHENT));
    /* pull the first reusable one off the pile */
    hp = hereuse;
    hereuse = hereuse->he_next;
    hp->he_next = NULL;			/* prevent accidents */
    reusables--;
    return hp;
}

static void
hefree(hp)				/* free a hash entry */
register HASHENT *hp;
{
    if (reusables >= RETAIN)		/* compost heap is full? */
	free((char*)hp);		/* yup, just pitch this one */
    else {				/* no, just stash for reuse */
	++reusables;
	hp->he_next = hereuse;
	hereuse = hp;
    }
}

/*
 * Copyright (c) 1992 Geoffrey Collyer
 * All rights reserved.
 * Written by Geoffrey Collyer.
 *
 * This software is not subject to any license of the American Telephone
 * and Telegraph Company, the Regents of the University of California, or
 * the Free Software Foundation.
 *
 * Permission is granted to anyone to use this software for any purpose on
 * any computer system, and to alter it and redistribute it freely, subject
 * to the following restrictions:
 *
 * 1. The author is not responsible for the consequences of use of this
 *    software, no matter how awful, even if they arise from flaws in it.
 *
 * 2. The origin of this software must not be misrepresented, either by
 *    explicit claim or by omission.  Since few users ever read sources,
 *    credits must appear in the documentation.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.  Since few users
 *    ever read sources, credits must appear in the documentation.
 *
 * 4. This notice may not be removed or altered.
 */
