/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

#include <sys/cdefs.h>

void block __P((void));
void block_close __P((void));
void dd_close __P((void));
void dd_out __P((int));
void def __P((void));
void def_close __P((void));
void err __P((const char *, ...));
void jcl __P((char **));
void pos_in __P((void));
void pos_out __P((void));
void setup __P((void));
void summary __P((int));
void terminate __P((int));
void unblock __P((void));
void unblock_close __P((void));
void warn __P((const char *, ...));

extern struct io_desc in, out;
extern void (*cfunc)();
extern u_long cpy_cnt;
extern u_int cbsz;
extern u_int ddflags;
extern u_int files_cnt;
extern u_char *ctab;
