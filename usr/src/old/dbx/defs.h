/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)defs.h	5.3 (Berkeley) %G%
 */

/*
 * Public definitions, common to all.
 */

#include <stdio.h>

#ifdef sgi
#   define double long float
#   define atof _latof
#   define IRIS
#   define mc68000
#endif

#define new(type)           ((type) malloc(sizeof(struct type)))
#define newarr(type, n)     ((type *) malloc((unsigned) (n) * sizeof(type)))
#define dispose(ptr)        { free((char *) ptr); ptr = 0; }

#define public
#define private static

#define ord(enumcon) ((unsigned int) enumcon)
#define nil 0
#define and &&
#define or ||
#define not !
#define div /
#define mod %
#define max(a, b)    ((a) > (b) ? (a) : (b))
#define min(a, b)    ((a) < (b) ? (a) : (b))

#define assert(b) { \
    if (not(b)) { \
	panic("assertion failed at line %d in file %s", __LINE__, __FILE__); \
    } \
}

#define badcaseval(v) { \
    panic("unexpected value %d at line %d in file %s", v, __LINE__, __FILE__); \
}

#define checkref(p) { \
    if (p == nil) { \
	panic("reference through nil pointer at line %d in file %s", \
	    __LINE__, __FILE__); \
    } \
}

typedef int Integer;
typedef int integer;
typedef char Char;
typedef double Real;
typedef double real;
typedef enum { false, true } Boolean;
typedef Boolean boolean;
typedef char *String;

#define strdup(s)       strcpy(malloc((unsigned) strlen(s) + 1), s)
#define streq(s1, s2)   (strcmp(s1, s2) == 0)

typedef FILE *File;
typedef int Fileid;
typedef String Filename;

#define get(f, var) fread((char *) &(var), sizeof(var), 1, f)
#define put(f, var) fwrite((char *) &(var), sizeof(var), 1, f)

#undef FILE

extern long atol();
extern double atof();
extern char *malloc();
extern String strcpy(), index(), rindex();
extern int strlen();

extern String cmdname;
extern String errfilename;
extern short errlineno;
extern int debug_flag[];
