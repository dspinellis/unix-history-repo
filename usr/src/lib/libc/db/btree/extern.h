/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

int	 __bt_close __P((DB *));
int	 __bt_cmp __P((BTREE *, const DBT *, EPG *));
int	 __bt_crsrdel __P((BTREE *, EPGNO *));
int	 __bt_defcmp __P((const DBT *, const DBT *));
int	 __bt_defpfx __P((const DBT *, const DBT *));
int	 __bt_delete __P((const DB *, const DBT *, u_int));
int	 __bt_dleaf __P((BTREE *, PAGE *, int));
EPG	*__bt_first __P((BTREE *, DBT *, int *));
int	 __bt_get __P((const DB *, DBT *, DBT *, u_int));
DB	*__bt_open __P((const char *, int, int, const BTREEINFO *));
void	 __bt_pgin __P((void *, pgno_t, void *));
void	 __bt_pgout __P((void *, pgno_t, void *));
int	 __bt_push __P((BTREE *, pgno_t, int));
int	 __bt_put __P((const DB *dbp, const DBT *, const DBT *, u_int));
int	 __bt_ret __P((BTREE *, EPG *, DBT *, DBT *));
EPG	*__bt_search __P((BTREE *, const DBT *, int *));
int	 __bt_seq __P((const DB *, DBT *, DBT *, u_int));
int	 __bt_split __P((BTREE *, PAGE *,
	    const DBT *, const DBT *, u_long, size_t, int));
int	 __bt_sync __P((const DB *));

int	 __ovfl_delete __P((BTREE *, void *));
int	 __ovfl_get __P((BTREE *, void *, size_t *, char **, size_t *));
int	 __ovfl_put __P((BTREE *, const DBT *, pgno_t *));

int	 __rec_close __P((DB *));
int	 __rec_delete __P((const DB *, const DBT *, u_int));
int	 __rec_fmap __P((BTREE *, recno_t));
int	 __rec_fout __P((BTREE *));
int	 __rec_fpipe __P((BTREE *, recno_t));
int	 __rec_get __P((const DB *, DBT *, DBT *, u_int));
int	 __rec_iput __P((BTREE *, recno_t, const DBT *, u_int));
int	 __rec_put __P((const DB *dbp, const DBT *, const DBT *, u_int));
int	 __rec_ret __P((BTREE *, EPG *, DBT *));
EPG	*__rec_search __P((BTREE *, recno_t, int *));
int	 __rec_seq __P((const DB *, DBT *, DBT *, u_int));
int	 __rec_sync __P((const DB *));
int	 __rec_vmap __P((BTREE *, recno_t));
int	 __rec_vout __P((BTREE *));
int	 __rec_vpipe __P((BTREE *, recno_t));

#ifdef DEBUG
void	 __bt_dpage __P((PAGE *));
void	 __bt_dump __P((DB *));
#endif
#ifdef STATISTICS
void	 __bt_stat __P((DB *));
#endif
