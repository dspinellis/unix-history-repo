/* $XConsortium: makekeys.c,v 11.6 91/05/01 14:29:38 rws Exp $ */
/*
Copyright 1990 by the Massachusetts Institute of Technology

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

/* Constructs hash tables for XStringToKeysym and XKeysymToString. */

#include <X11/X.h>
#include <X11/Xos.h>
#include <X11/keysymdef.h>
#include <stdio.h>

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
char *malloc();
#endif
#if defined(macII) && !defined(__STDC__)  /* stdlib.h fails to define these */
char *malloc();
#endif /* macII */

typedef unsigned long Signature;

#define KTNUM 3000

static struct info {
    char	*name;
    KeySym	val;
} info[KTNUM];

#define MIN_REHASH 10
#define MATCHES 10

char tab[KTNUM];
unsigned short offsets[KTNUM];
unsigned short indexes[KTNUM];
KeySym values[KTNUM];
char buf[1024];

main()
{
    int ksnum;
    int max_rehash;
    Signature sig;
    register int i, j, k, z;
    register char *name;
    register char c;
    int first;
    int best_max_rehash;
    int best_z;
    int num_found;
    KeySym val;

    for (ksnum = 0; 1; (void)gets(buf)) {
	i = scanf("#define XK_%s 0x%lx", buf, &info[ksnum].val);
	if (i == EOF)
	    break;
	if (i != 2)
	    continue;
	if (info[ksnum].val == XK_VoidSymbol)
	    info[ksnum].val = 0;
	if (info[ksnum].val > 0xffff) {
	    fprintf(stderr,
		    "ignoring illegal keysym (%s), remove it from .h file!\n",
		    buf);
	    continue;
	}
	name = malloc((unsigned)strlen(buf)+1);
	if (!name) {
	    fprintf(stderr, "makekeys: out of memory!\n");
	    exit(1);
	}
	(void)strcpy(name, buf);
	info[ksnum].name = name;
	ksnum++;
	if (ksnum == KTNUM) {
	    fprintf(stderr, "makekeys: too many keysyms!\n");
	    exit(1);
	}
    }

    printf("/* This file is generated from keysymdef.h. */\n");
    printf("/* Do not edit. */\n");
    printf("\n");

    best_max_rehash = ksnum;
    num_found = 0;
    for (z = ksnum; z < KTNUM; z++) {
	max_rehash = 0;
	for (name = tab, i = z; --i >= 0;)
		*name++ = 0;
	for (i = 0; i < ksnum; i++) {
	    name = info[i].name;
	    sig = 0;
	    while (c = *name++)
		sig = (sig << 1) + c;
	    first = j = sig % z;
	    for (k = 0; tab[j]; k++) {
		j += first + 1;
		if (j >= z)
		    j -= z;
		if (j == first)
		    goto next1;
	    }
	    tab[j] = 1;
	    if (k > max_rehash)
		max_rehash = k;
	}
	if (max_rehash < MIN_REHASH) {
	    if (max_rehash < best_max_rehash) {
		best_max_rehash = max_rehash;
		best_z = z;
	    }
	    num_found++;
	    if (num_found >= MATCHES)
		break;
	}
next1:	;
    }

    z = best_z;
    printf("#ifdef NEEDKTABLE\n");
    printf("Const unsigned char _XkeyTable[] = {\n");
    printf("0,\n");
    k = 1;
    for (i = 0; i < ksnum; i++) {
	name = info[i].name;
	sig = 0;
	while (c = *name++)
	    sig = (sig << 1) + c;
	first = j = sig % z;
	while (offsets[j]) {
	    j += first + 1;
	    if (j >= z)
		j -= z;
	}
	offsets[j] = k;
	indexes[i] = k;
	val = info[i].val;
	printf("0x%.2x, 0x%.2x, 0x%.2x, 0x%.2x, ",
	       (sig >> 8) & 0xff, sig & 0xff,
	       (val >> 8) & 0xff, val & 0xff);
	for (name = info[i].name, k += 5; c = *name++; k++)
	    printf("'%c',", c);
	printf((i == (ksnum-1)) ? "0\n" : "0,\n");
    }
    printf("};\n");
    printf("\n");
    printf("#define KTABLESIZE %d\n", z);
    printf("#define KMAXHASH %d\n", best_max_rehash + 1);
    printf("\n");
    printf("static Const unsigned short hashString[KTABLESIZE] = {\n");
    for (i = 0; i < z;) {
	printf("0x%.4x", offsets[i]);
	i++;
	if (i == z)
	    break;
	printf((i & 7) ? ", " : ",\n");
    }
    printf("\n");
    printf("};\n");
    printf("#endif /* NEEDKTABLE */\n");

    best_max_rehash = ksnum;
    num_found = 0;
    for (z = ksnum; z < KTNUM; z++) {
	max_rehash = 0;
	for (name = tab, i = z; --i >= 0;)
		*name++ = 0;
	for (i = 0; i < ksnum; i++) {
	    val = info[i].val;
	    first = j = val % z;
	    for (k = 0; tab[j]; k++) {
		if (values[j] == val)
		    goto skip1;
		j += first + 1;
		if (j >= z)
		    j -= z;
		if (j == first)
		    goto next2;
	    }
	    tab[j] = 1;
	    values[j] = val;
	    if (k > max_rehash)
		max_rehash = k;
skip1:	;
	}
	if (max_rehash < MIN_REHASH) {
	    if (max_rehash < best_max_rehash) {
		best_max_rehash = max_rehash;
		best_z = z;
	    }
	    num_found++;
	    if (num_found >= MATCHES)
		break;
	}
next2:	;
    }

    z = best_z;
    for (i = z; --i >= 0;)
	offsets[i] = 0;
    for (i = 0; i < ksnum; i++) {
	val = info[i].val;
	first = j = val % z;
	while (offsets[j]) {
	    if (values[j] == val)
		goto skip2;
	    j += first + 1;
	    if (j >= z)
		j -= z;
	}
	offsets[j] = indexes[i] + 2;
	values[j] = val;
skip2:	;
    }
    printf("\n");
    printf("#ifdef NEEDVTABLE\n");
    printf("#define VTABLESIZE %d\n", z);
    printf("#define VMAXHASH %d\n", best_max_rehash + 1);
    printf("\n");
    printf("static Const unsigned short hashKeysym[VTABLESIZE] = {\n");
    for (i = 0; i < z;) {
	printf("0x%.4x", offsets[i]);
	i++;
	if (i == z)
	    break;
	printf((i & 7) ? ", " : ",\n");
    }
    printf("\n");
    printf("};\n");
    printf("#endif /* NEEDVTABLE */\n");

    exit(0);
}
