/*      ndbm.h     4.1     83/12/02     */

/*
 * (New) Hashed key data base library (-lndbm).
 */
#define PBLKSIZ 1024
#define DBLKSIZ 4096

typedef struct {
	int     db_dirf;                /* open directory file */
	int     db_pagf;                /* open page file */
	int     db_flags;
#define _DB_RDONLY      0x1     /* data base open read-only */
	long    db_maxbno;              /* last ``block'' in page file */
	long    db_bitno;
	long    db_hmask;
	long    db_blkno;               /* current page to read/write */
	long    db_pagbno;              /* current page in pagbuf */
	char    db_pagbuf[PBLKSIZ];     /* page file block buffer */
	long    db_dirbno;              /* current block in dirbuf */
	char    db_dirbuf[DBLKSIZ];     /* directory file block buffer */
} DBM;

#define dbrdonly(db)    ((db)->db_flags&_DB_RDONLY) != 0

typedef struct {
	char    *dptr;
	int     dsize;
} datum;

datum   dbmfetch();
datum   dbmfirstkey();
datum   dbmnextkey();
long    dbmforder();
int     dbmdelete();
int     dbmstore();

DBM     *ndbmopen();
void    ndbmclose();
