/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)pathnames.h	8.1 (Berkeley) 6/6/93
 */

#define	_PATH_PC0	"/usr/libexec/pascal/pc0"
#define	_PATH_PC1	"/usr/libexec/f1"
#define	_PATH_PC2	"/usr/libexec/pascal/pc2"
#define	_PATH_C2	"/usr/libexec/c2"
#define	_PATH_PC3	"/usr/libexec/pascal/pc3"
#define	_PATH_PCEXTERN	"/usr/lib/pcexterns.o"

#define	_PATH_AS	"/usr/old/bin/as"
#define	_PATH_LD	"/usr/old/bin/ld"
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
