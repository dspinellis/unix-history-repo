/*-
 * Copyright (c) 1991, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego and Lance
 * Visser of Convex Computer Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.3 (Berkeley) %G%
 */

#include <sys/cdefs.h>

void block __P((void));
void block_close __P((void));
void dd_out __P((int));
void def __P((void));
void def_close __P((void));
void jcl __P((char **));
void pos_in __P((void));
void pos_out __P((void));
void summary __P((void));
void summaryx __P((int));
void terminate __P((int));
void unblock __P((void));
void unblock_close __P((void));

extern IO in, out;
extern STAT st;
extern void (*cfunc)();
extern u_long cpy_cnt;
extern u_int cbsz;
extern u_int ddflags;
extern u_int files_cnt;
extern u_char *ctab;
extern u_char a2e_32V[], a2e_POSIX[], a2ibm_32V[], a2ibm_POSIX[], e2a_32V[];
extern u_char e2a_POSIX[], l2u[], u2l[];
