/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)encrypt.h	5.3 (Berkeley) %G%
 */

/*
 * Copyright (C) 1990 by the Massachusetts Institute of Technology
 *
 * Export of this software from the United States of America is assumed
 * to require a specific license from the United States Government.
 * It is the responsibility of any person or organization contemplating
 * export to obtain such a license before exporting.
 *
 * WITHIN THAT CONSTRAINT, permission to use, copy, modify, and
 * distribute this software and its documentation for any purpose and
 * without fee is hereby granted, provided that the above copyright
 * notice appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation, and that
 * the name of M.I.T. not be used in advertising or publicity pertaining
 * to distribution of the software without specific, written prior
 * permission.  M.I.T. makes no representations about the suitability of
 * this software for any purpose.  It is provided "as is" without express
 * or implied warranty.
 */

#ifndef	__ENCRYPTION__
#define	__ENCRYPTION__

#define	DIR_DECRYPT		1
#define	DIR_ENCRYPT		2

typedef	unsigned char Block[8];
typedef unsigned char *BlockT;
typedef struct { Block _; } Schedule[16];

#define	VALIDKEY(key)	( key[0] | key[1] | key[2] | key[3] | \
			  key[4] | key[5] | key[6] | key[7])

#define	SAMEKEY(k1, k2)	(!bcmp((void *)k1, (void *)k2, sizeof(Block)))

typedef	struct {
	short		type;
	int		length;
	unsigned char	*data;
} Session_Key;

#if	!defined(P)
#ifdef	__STDC__
#define P(x)	x
#else
#define P(x)	()
#endif
#endif

typedef struct {
	char	*name;
	int	type;
	void	(*output) P((unsigned char *, int));
	int	(*input) P((int));
	void	(*init) P((int));
	int	(*start) P((int, int));
	int	(*is) P((unsigned char *, int));
	int	(*reply) P((unsigned char *, int));
	void	(*session) P((Session_Key *, int));
	int	(*keyid) P((int, unsigned char *, int *));
	void	(*printsub) P((unsigned char *, int, unsigned char *, int));
} Encryptions;

#define	SK_DES		1	/* Matched Kerberos v5 KEYTYPE_DES */

#include "enc-proto.h"

extern int encrypt_debug_mode;
extern int (*decrypt_input) P((int));
extern void (*encrypt_output) P((unsigned char *, int));
#endif
