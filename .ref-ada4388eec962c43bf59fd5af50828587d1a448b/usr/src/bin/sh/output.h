/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)output.h	8.2 (Berkeley) %G%
 */

#ifndef OUTPUT_INCL

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

struct output {
	char *nextc;
	int nleft;
	char *buf;
	int bufsize;
	short fd;
	short flags;
};

extern struct output output;
extern struct output errout;
extern struct output memout;
extern struct output *out1;
extern struct output *out2;

void open_mem __P((char *, int, struct output *));
void out1str __P((const char *));
void out2str __P((const char *));
void outstr __P((const char *, struct output *));
void emptyoutbuf __P((struct output *));
void flushall __P((void));
void flushout __P((struct output *));
void freestdout __P((void));
void outfmt __P((struct output *, char *, ...));
void out1fmt __P((char *, ...)); 
void dprintf __P((char *, ...));
void fmtstr __P((char *, int, char *, ...));
void doformat __P((struct output *, char *, va_list));
int xwrite __P((int, char *, int));
int xioctl __P((int, unsigned long, char *));

#define outc(c, file)	(--(file)->nleft < 0? (emptyoutbuf(file), *(file)->nextc++ = (c)) : (*(file)->nextc++ = (c)))
#define out1c(c)	outc(c, out1);
#define out2c(c)	outc(c, out2);

#define OUTPUT_INCL
#endif
