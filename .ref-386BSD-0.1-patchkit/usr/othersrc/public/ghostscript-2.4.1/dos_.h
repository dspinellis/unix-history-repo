/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* dos.h */
/* Generic DOS interface for Ghostscript */

/* This file is needed because the Borland and Watcom compilers */
/* provide slightly different procedures for interfacing to DOS and */
/* the I/O hardware, and because the Watcom compiler is 32-bit. */
#include <dos.h>
#ifdef __WATCOMC__

/* ---------------- Watcom compiler, 32-bit flat model. */
/* ---------------- inp/outp prototypes are in conio.h, not dos.h. */

#  include <conio.h>
#  define inportb(px) inp(px)
#  define outportb(px,b) outp(px,b)
#  define enable() _enable()
#  define disable() _disable()
#  define MK_PTR(seg,off) (((seg) << 4) + (off))
#  define PTR_OFF(ptr) ((unsigned)(ptr) & 0xffff)
#  define int86 int386
#  define int86x int386x
/* Fetch and discard a byte.  Prevent the compiler from */
/* optimizing this away. */
static unsigned char byte_discard_;
#  define byte_discard(expr) byte_discard_ = (expr)
/* Define the regs union tag for short registers. */
#  define rshort w
/* Define the structure and procedures for file enumeration. */
#define ff_struct_t struct find_t
#define ff_name name
#define dos_findfirst(n,b) _dos_findfirst(n, _A_NORMAL | _A_RDONLY, b)
#define dos_findnext(b) _dos_findnext(b)

#else

/* ---------------- Borland compiler, 16:16 pseudo-segmented model. */
/* ---------------- ffblk is in dir.h, not dos.h. */
#include <dir.h>
#  define MK_PTR(seg,off) MK_FP(seg,off)
#  define PTR_OFF(ptr) FP_OFF(ptr)
#  define PTR_SEG(ptr) FP_SEG(ptr)
/* Fetch and discard a byte.  No special cleverness required. */
#  define byte_discard(expr) byte discard = (expr)
/* Define the regs union tag for short registers. */
#  define rshort x
/* Define the structure and procedures for file enumeration. */
#define ff_struct_t struct ffblk
#define dos_findfirst(n,b) findfirst(n, b, 0)
#define dos_findnext(b) findnext(b)

#endif
