/*
 * $XConsortium: Quarks.c,v 1.36 91/03/11 09:03:34 rws Exp $
 */

/***********************************************************
Copyright 1987, 1988, 1990 by Digital Equipment Corporation, Maynard,
Massachusetts, and the Massachusetts Institute of Technology, Cambridge,
Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include "Xlibint.h"
#include <X11/Xresource.h>

/* Not cost effective, at least for vanilla MIT clients */
/* #define PERMQ */

typedef unsigned long Signature;
typedef unsigned long Entry;
#ifdef PERMQ
typedef unsigned char Bits;
#endif

static XrmQuark nextQuark = 1;	/* next available quark number */
static unsigned long quarkMask = 0;
static Entry zero = 0;
static Entry *quarkTable = &zero; /* crock */
static unsigned long quarkRehash;
static XrmString **stringTable = NULL;
#ifdef PERMQ
static Bits **permTable = NULL;
#endif
static XrmQuark nextUniq = -1;	/* next quark from XrmUniqueQuark */

#define QUANTUMSHIFT	8
#define QUANTUMMASK	((1 << QUANTUMSHIFT) - 1)
#define CHUNKPER	8
#define CHUNKMASK	((CHUNKPER << QUANTUMSHIFT) - 1)

#define LARGEQUARK	((Entry)0x80000000L)
#define QUARKSHIFT	18
#define QUARKMASK	((LARGEQUARK - 1) >> QUARKSHIFT)
#define SIGMASK		((1L << QUARKSHIFT) - 1)

#define STRQUANTSIZE	(sizeof(XrmString) * (QUANTUMMASK + 1))
#ifdef PERMQ
#define QUANTSIZE	(STRQUANTSIZE + \
			 (sizeof(Bits) * ((QUANTUMMASK + 1) >> 3))
#else
#define QUANTSIZE	STRQUANTSIZE
#endif

#define HASH(sig) ((sig) & quarkMask)
#define REHASHVAL(sig) ((((sig) % quarkRehash) + 2) | 1)
#define REHASH(idx,rehash) ((idx + rehash) & quarkMask)
#define NAME(q) stringTable[(q) >> QUANTUMSHIFT][(q) & QUANTUMMASK]
#ifdef PERMQ
#define BYTEREF(q) permTable[(q) >> QUANTUMSHIFT][((q) & QUANTUMMASK) >> 3]
#define ISPERM(q) (BYTEREF(q) & (1 << ((q) & 7)))
#define SETPERM(q) BYTEREF(q) |= (1 << ((q) & 7))
#define CLEARPERM(q) BYTEREF(q) &= ~(1 << ((q) & 7))
#endif

/* Permanent memory allocation */

#define WALIGN sizeof(unsigned long)
#define DALIGN sizeof(double)

#define NEVERFREETABLESIZE ((8192-12) & ~(DALIGN-1))
static char *neverFreeTable = NULL;
static int  neverFreeTableSize = 0;

static char *permalloc(length)
    register unsigned int length;
{
    char *ret;

    if (neverFreeTableSize < length) {
	if (length >= NEVERFREETABLESIZE)
	    return Xmalloc(length);
	if (! (ret = Xmalloc(NEVERFREETABLESIZE)))
	    return (char *) NULL;
	neverFreeTableSize = NEVERFREETABLESIZE;
	neverFreeTable = ret;
    }
    ret = neverFreeTable;
    neverFreeTable += length;
    neverFreeTableSize -= length;
    return(ret);
}

char *Xpermalloc(length)
    unsigned int length;
{
    int i;

    if (neverFreeTableSize && length < NEVERFREETABLESIZE) {
#ifndef WORD64
	if ((sizeof(struct {char a; double b;}) !=
	     (sizeof(struct {char a; unsigned long b;}) -
	      sizeof(unsigned long) + sizeof(double))) &&
	    !(length & (DALIGN-1)) &&
	    (i = (NEVERFREETABLESIZE - neverFreeTableSize) & (DALIGN-1))) {
	    neverFreeTableSize -= DALIGN - i;
	    neverFreeTable += DALIGN - i;
	} else
#endif
	    if (i = (NEVERFREETABLESIZE - neverFreeTableSize) & (WALIGN-1)) {
		neverFreeTableSize -= WALIGN - i;
		neverFreeTable += WALIGN - i;
	    }
    }
    return permalloc(length);
}

static Bool
ExpandQuarkTable()
{
    unsigned long oldmask, newmask;
    register char c, *s;
    register Entry *oldentries, *entries;
    register Entry entry;
    register int oldidx, newidx, rehash;
    Signature sig;
    XrmQuark q;

    oldentries = quarkTable;
    if (oldmask = quarkMask)
	newmask = (oldmask << 1) + 1;
    else {
	if (!stringTable) {
	    stringTable = (XrmString **)Xmalloc(sizeof(XrmString *) *
						CHUNKPER);
	    if (!stringTable)
		return False;
	    stringTable[0] = (XrmString *)NULL;
	}
#ifdef PERMQ
	if (!permTable)
	    permTable = (Bits **)Xmalloc(sizeof(Bits *) * CHUNKPER);
	if (!permTable)
	    return False;
#endif
	stringTable[0] = (XrmString *)Xpermalloc(QUANTSIZE);
	if (!stringTable[0])
	    return False;
#ifdef PERMQ
	permTable[0] = (Bits *)((char *)stringTable[0] + STRQUANTSIZE);
#endif
	newmask = 0x1ff;
    }
    entries = (Entry *)Xmalloc(sizeof(Entry) * (newmask + 1));
    if (!entries)
	return False;
    bzero((char *)entries, sizeof(Entry) * (newmask + 1));
    quarkTable = entries;
    quarkMask = newmask;
    quarkRehash = quarkMask - 2;
    for (oldidx = 0; oldidx <= oldmask; oldidx++) {
	if (entry = oldentries[oldidx]) {
	    if (entry & LARGEQUARK)
		q = entry & (LARGEQUARK-1);
	    else
		q = (entry >> QUARKSHIFT) & QUARKMASK;
	    for (sig = 0, s = NAME(q); c = *s++; )
		sig = (sig << 1) + c;
	    newidx = HASH(sig);
	    if (entries[newidx]) {
		rehash = REHASHVAL(sig);
		do {
		    newidx = REHASH(newidx, rehash);
		} while (entries[newidx]);
	    }
	    entries[newidx] = entry;
	}
    }
    if (oldmask)
	Xfree((char *)oldentries);
    return True;
}

#if NeedFunctionPrototypes
XrmQuark _XrmInternalStringToQuark(
    register _Xconst char *name, register int len, register Signature sig,
    Bool permstring)
#else
XrmQuark _XrmInternalStringToQuark(name, len, sig, permstring)
    register XrmString name;
    register int len;
    register Signature sig;
    Bool permstring;
#endif
{
    register XrmQuark q;
    register Entry entry;
    register int idx, rehash;
    register int i;
    register char *s1, *s2;
    char *new;

    rehash = 0;
    idx = HASH(sig);
    while (entry = quarkTable[idx]) {
	if (entry & LARGEQUARK)
	    q = entry & (LARGEQUARK-1);
	else {
	    if ((entry - sig) & SIGMASK)
		goto nomatch;
	    q = (entry >> QUARKSHIFT) & QUARKMASK;
	}
	for (i = len, s1 = (char *)name, s2 = NAME(q); --i >= 0; ) {
	    if (*s1++ != *s2++)
		goto nomatch;
	}
	if (*s2) {
nomatch:    if (!rehash)
		rehash = REHASHVAL(sig);
	    idx = REHASH(idx, rehash);
	    continue;
	}
#ifdef PERMQ
	if (permstring && !ISPERM(q)) {
	    Xfree(NAME(q));
	    NAME(q) = (char *)name;
	    SETPERM(q);
	}
#endif
	return q;
    }
    if (nextUniq == nextQuark)
	return NULLQUARK;
    if ((nextQuark + (nextQuark >> 2)) > quarkMask) {
	if (!ExpandQuarkTable())
	    return NULLQUARK;
	return _XrmInternalStringToQuark(name, len, sig, permstring);
    }
    q = nextQuark;
    if (!(q & QUANTUMMASK)) {
	if (!(q & CHUNKMASK)) {
	    if (!(new = Xrealloc((char *)stringTable,
				 sizeof(XrmString *) *
				 ((q >> QUANTUMSHIFT) + CHUNKPER))))
		return NULLQUARK;
	    stringTable = (XrmString **)new;
#ifdef PERMQ
	    if (!(new = Xrealloc((char *)permTable,
				 sizeof(Bits *) *
				 ((q >> QUANTUMSHIFT) + CHUNKPER))))
		return NULLQUARK;
	    permTable = (Bits **)new;
#endif
	}
	new = Xpermalloc(QUANTSIZE);
	if (!new)
	    return NULLQUARK;
	stringTable[q >> QUANTUMSHIFT] = (XrmString *)new;
#ifdef PERMQ
	permTable[q >> QUANTUMSHIFT] = (Bits *)(new + STRQUANTSIZE);
#endif
    }
    if (!permstring) {
	s2 = (char *)name;
#ifdef PERMQ
	name = Xmalloc(len+1);
#else
	name = permalloc(len+1);
#endif
	if (!name)
	    return NULLQUARK;
	for (i = len, s1 = (char *)name; --i >= 0; )
	    *s1++ = *s2++;
	*s1++ = '\0';
#ifdef PERMQ
	CLEARPERM(q);
    }
    else {
	SETPERM(q);
#endif
    }
    NAME(q) = (char *)name;
    if (q <= QUARKMASK)
	entry = (q << QUARKSHIFT) | (sig & SIGMASK);
    else
	entry = q | LARGEQUARK;
    quarkTable[idx] = entry;
    nextQuark++;
    return q;
}

#if NeedFunctionPrototypes
XrmQuark XrmStringToQuark(
    _Xconst char *name)
#else
XrmQuark XrmStringToQuark(name)
    XrmString name;
#endif
{
    register char c, *tname;
    register Signature sig = 0;

    if (!name)
	return (NULLQUARK);
    
    for (tname = (char *)name; c = *tname++; )
	sig = (sig << 1) + c;

    return _XrmInternalStringToQuark(name, tname-(char *)name-1, sig, False);
}

#if NeedFunctionPrototypes
XrmQuark XrmPermStringToQuark(
    _Xconst char *name)
#else
XrmQuark XrmPermStringToQuark(name)
    XrmString name;
#endif
{
    register char c, *tname;
    register Signature sig = 0;

    if (!name)
	return (NULLQUARK);

    for (tname = (char *)name; c = *tname++; )
	sig = (sig << 1) + c;

    return _XrmInternalStringToQuark(name, tname-(char *)name-1, sig, True);
}

XrmQuark XrmUniqueQuark()
{
    if (nextUniq == nextQuark)
	return NULLQUARK;
    return nextUniq--;
}

XrmString XrmQuarkToString(quark)
    register XrmQuark quark;
{
    if (quark <= 0 || quark >= nextQuark)
    	return NULLSTRING;
#ifdef PERMQ
    /* We have to mark the quark as permanent, since the caller might hold
     * onto the string pointer forver.
     */
    SETPERM(quark);
#endif
    return NAME(quark);
}
