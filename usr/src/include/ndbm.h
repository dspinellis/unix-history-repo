/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ndbm.h	5.5 (Berkeley) %G%
 */

#include <sys/cdefs.h>
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

__BEGIN_DECLS
void	 dbm_close __P((DBM *));
int	 dbm_delete __P((DBM *, datum));
datum	 dbm_fetch __P((DBM *, datum));
datum	 dbm_firstkey __P((DBM *));
long	 dbm_forder __P((DBM *, datum));
datum	 dbm_nextkey __P((DBM *));
DBM	*dbm_open __P((const char *, int, int));
int	 dbm_store __P((DBM *, datum, datum, int));
__END_DECLS
