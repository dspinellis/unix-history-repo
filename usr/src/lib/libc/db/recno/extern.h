/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.1 (Berkeley) %G%
 */

#include "../btree/extern.h"

int	 __rec_close __P((DB *));
int	 __rec_delete __P((const DB *, const DBT *, u_int));
int	 __rec_dleaf __P((BTREE *, PAGE *, int));
int	 __rec_fd __P((const DB *));
int	 __rec_fmap __P((BTREE *, recno_t));
int	 __rec_fout __P((BTREE *));
int	 __rec_fpipe __P((BTREE *, recno_t));
int	 __rec_get __P((const DB *, const DBT *, DBT *, u_int));
int	 __rec_iput __P((BTREE *, recno_t, const DBT *, u_int));
int	 __rec_put __P((const DB *dbp, DBT *, const DBT *, u_int));
int	 __rec_ret __P((BTREE *, EPG *, recno_t, DBT *, DBT *));
EPG	*__rec_search __P((BTREE *, recno_t, enum SRCHOP));
int	 __rec_seq __P((const DB *, DBT *, DBT *, u_int));
int	 __rec_sync __P((const DB *, u_int));
int	 __rec_vmap __P((BTREE *, recno_t));
int	 __rec_vout __P((BTREE *));
int	 __rec_vpipe __P((BTREE *, recno_t));
