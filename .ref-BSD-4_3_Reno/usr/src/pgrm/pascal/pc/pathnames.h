/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)pathnames.h	5.1 (Berkeley) 6/29/90
 */

#define	_PATH_PC0	"/usr/libexec/pascal/pc0"
#define	_PATH_PC1	"/usr/libexec/f1"
#define	_PATH_PC2	"/usr/libexec/pascal/pc2"
#define	_PATH_C2	"/usr/libexec/c2"
#define	_PATH_PC3	"/usr/libexec/pascal/pc3"
#define	_PATH_PCEXTERN	"/usr/lib/pcexterns.o"

#define	_PATH_AS	"/usr/bin/as"
#define	_PATH_LD	"/usr/bin/ld"
#define	_PATH_CRT0	"/usr/lib/crt0.o"
#define	_PATH_MCRT0	"/usr/lib/mcrt0.o"
#define	_PATH_GCRT0	"/usr/lib/gcrt0.o"

#define	_PATH_TMP	"/tmp"
#define	_PATH_CAT	"/bin/cat"
#define	_PATH_HOWPC	"/usr/libdata/pascal/how_pc"

/* DEBUG */
#define	_PATH_DPC0	"/usr/src/pgrm/pascal/pc0/obj/pc0"
#ifdef vax
#define	_PATH_DPC1	"/usr/src/libexec/pcc/f1.vax/obj/f1"
#else
#ifdef tahoe
#define	_PATH_DPC1	"/usr/src/libexec/pcc/f1.tahoe/obj/f1"
#else
NO F1 PROGRAM AVAILABLE
#endif
#endif
#define	_PATH_DPC2	"/usr/src/pgrm/pascal/pc2/obj/pc2"
#define	_PATH_DPC3	"/usr/src/pgrm/pascal/pc3/obj/pc3"
#define	_PATH_DLPC	"/usr/src/lib/libpc/obj/libpc.a"
