/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)a.out.h	5.5 (Berkeley) %G%
 */

#ifndef	_AOUT_H_
#define	_AOUT_H_

#include <sys/exec.h>

/* Valid magic number check. */
#define	N_BADMAG(ex) \
	((ex).a_magic != NMAGIC && (ex).a_magic != OMAGIC && \
	    (ex).a_magic != ZMAGIC)

/* Text segment offset. */
#if defined(vax) || defined(tahoe)
#define	N_TXTOFF(ex) \
	((ex).a_magic == ZMAGIC ? 1024 : sizeof(struct exec))
#endif

#if defined(hp300) || defined(i386)
#define	N_TXTOFF(ex) \
	((ex).a_magic == ZMAGIC ? 4096 : sizeof(struct exec)) 
#endif

/* Symbol table offset. */
#define N_SYMOFF(ex) \
	(N_TXTOFF(ex) + (ex).a_text + (ex).a_data + (ex).a_trsize + \
	    (ex).a_drsize)

/* String table offset. */
#define	N_STROFF(ex) \
	(N_SYMOFF(ex) + (ex).a_syms)

/* Relocation format. */
struct relocation_info {
	int r_address;			/* offset in text or data segment */
	unsigned int r_symbolnum : 24,	/* ordinal number of add symbol */
			 r_pcrel :  1,	/* 1 if value should be pc-relative */
			r_length :  2,	/* log base 2 of value's width */
			r_extern :  1,	/* 1 if need to add symbol to value */
				 :  4;	/* reserved */
};

#define _AOUT_INCLUDE_
#include <nlist.h>

#endif /* !_AOUT_H_ */
