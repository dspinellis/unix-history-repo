/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)db.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#define	__DBINTERFACE_PRIVATE
#include <db.h>
#include <stdio.h>
#include <stddef.h>
#include <errno.h>
#include "../btree/btree.h"

DB *
dbopen(fname, flags, mode, type, openinfo)
	const char *fname;
	int flags, mode;
	DBTYPE type;
	const void *openinfo;
{
	switch (type) {
	case DB_BTREE:
		return (__bt_open(fname, flags, mode, openinfo));
	case DB_HASH:
		return (__hash_open(fname, flags, mode, openinfo));
	case DB_RECNO:
		return (__rec_open(fname, flags, mode, openinfo));
	}
	errno = EINVAL;
	return (NULL);
}

static int __db_edel __P((const DB *, const DBT *, u_int));
static int __db_eget __P((const DB *, const DBT *, DBT *, u_int));
static int __db_eput __P((const DB *dbp, DBT *, const DBT *, u_int));
static int __db_eseq __P((const DB *, DBT *, DBT *, u_int));
static int __db_esync __P((const DB *));

/*
 * __DBPANIC -- Stop.
 *
 * Parameters:
 *	dbp:	pointer to the DB structure.
 */
void
__dbpanic(dbp)
	DB *dbp;
{
	/* The only thing that can succeed is a close. */
	dbp->del = __db_edel;
	dbp->get = __db_eget;
	dbp->put = __db_eput;
	dbp->seq = __db_eseq;
	dbp->sync = __db_esync;
}

static int
__db_edel(dbp, key, flags)
	const DB *dbp;
	const DBT *key;
	u_int flags;
{
	return (RET_ERROR);
}

static int
__db_eget(dbp, key, data, flag)
	const DB *dbp;
	const DBT *key;
	DBT *data;
	u_int flag;
{
	return (RET_ERROR);
}

static int
__db_eput(dbp, key, data, uflags)
	const DB *dbp;
	DBT *key;
	const DBT *data;
	u_int uflags;
{
	return (RET_ERROR);
}

static int
__db_eseq(dbp, key, data, flags)
	const DB *dbp;
	DBT *key, *data;
	u_int flags;
{
	return (RET_ERROR);
}

static int
__db_esync(dbp)
	const DB *dbp;
{
	return (RET_ERROR);
}
