/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ndbm.h	8.1 (Berkeley) %G%
 */

#ifndef _NDBM_H_
#define	_NDBM_H_

#include <db.h>

/* Map dbm interface onto db(3). */
#define DBM_RDONLY	O_RDONLY

/* Flags to dbm_store(). */
#define DBM_INSERT      0
#define DBM_REPLACE     1

/*
 * The db(3) support for ndbm(3) always appends this suffix to the
 * file name to avoid overwriting the user's original database.
 */
#define	DBM_SUFFIX	".db"

typedef struct {
	char *dptr;
	int dsize;
} datum;

typedef DB DBM;
#define	dbm_pagfno(a)	DBM_PAGFNO_NOT_AVAILABLE

__BEGIN_DECLS
void	 dbm_close __P((DBM *));
int	 dbm_delete __P((DBM *, datum));
datum	 dbm_fetch __P((DBM *, datum));
datum	 dbm_firstkey __P((DBM *));
long	 dbm_forder __P((DBM *, datum));
datum	 dbm_nextkey __P((DBM *));
DBM	*dbm_open __P((const char *, int, int));
int	 dbm_store __P((DBM *, datum, datum, int));
int	 dbm_dirfno __P((DBM *));
__END_DECLS

#endif /* !_NDBM_H_ */
