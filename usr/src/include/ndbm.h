/*      ndbm.h     4.3     84/08/28     */

/*
 * Hashed key data base library.
 */
#define PBLKSIZ 1024
#define DBLKSIZ 4096

typedef struct {
	int     dbm_dirf;                /* open directory file */
	int     dbm_pagf;                /* open page file */
	int     dbm_flags;		/* flags, see below */
	long    dbm_maxbno;              /* last ``block'' in page file */
	long    dbm_bitno;
	long    dbm_hmask;
	long    dbm_blkno;               /* current page to read/write */
	long    dbm_pagbno;              /* current page in pagbuf */
	char    dbm_pagbuf[PBLKSIZ];     /* page file block buffer */
	long    dbm_dirbno;              /* current block in dirbuf */
	char    dbm_dirbuf[DBLKSIZ];     /* directory file block buffer */
} DBM;

#define _DBM_RDONLY	0x1	/* data base open read-only */
#define _DBM_IOERR	0x2	/* data base I/O error */

#define dbm_rdonly(db)	((db)->dbm_flags & _DBM_RDONLY)

#define dbm_error(db)	((db)->dbm_flags & _DBM_IOERR)
	/* use this one at your own risk! */
#define dbm_clearerr(db)	((db)->dbm_flags &= ~_DBM_IOERR)

typedef struct {
	char    *dptr;
	int     dsize;
} datum;

/*
 * flags to dbm_store()
 */
#define DBM_INSERT	0
#define DBM_REPLACE	1

DBM     *dbm_open();
void    dbm_close();
datum   dbm_fetch();
datum   dbm_firstkey();
datum   dbm_nextkey();
long    dbm_forder();
int     dbm_delete();
int     dbm_store();
