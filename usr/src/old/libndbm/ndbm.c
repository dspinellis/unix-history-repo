#ifndef lint
static char sccsid[] = "@(#)ndbm.c	4.2 (Berkeley) %G%";
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <errno.h>
#include <ndbm.h>

#define NULL    (char *)0
#define BYTESIZ 8

static  datum firsthash();
static  dbm_access();
static  getbit();
static  setbit();
static  datum makdatum();
static  cmpdatum();
static  long hashinc();
static  long dcalchash();
static  delitem();
static  additem();
static  chkblk();
extern  int errno;

DBM *
ndbmopen(file, flags, mode)
	char *file;
	int flags, mode;
{
	struct stat statb;
	register DBM *db;

	if ((db = (DBM *)malloc(sizeof *db)) == 0) {
		errno = ENOMEM;
		return ((DBM *)0);
	}
	if ((flags & 03) == O_WRONLY)
		flags = (flags & ~03) | O_RDWR;
	db->db_flags = 0;
	strcpy(db->db_pagbuf, file);
	strcat(db->db_pagbuf, ".pag");
	db->db_pagf = open(db->db_pagbuf, flags, mode);
	if (db->db_pagf < 0)
		goto bad;
	strcpy(db->db_pagbuf, file);
	strcat(db->db_pagbuf, ".dir");
	db->db_dirf = open(db->db_pagbuf, flags, mode);
	if (db->db_dirf < 0)
		goto bad1;
	fstat(db->db_dirf, &statb);
	db->db_maxbno = statb.st_size*BYTESIZ-1;
	db->db_pagbno = db->db_dirbno = -1;
	return (db);
bad1:
	(void) close(db->db_pagf);
bad:
	free((char *)db);
	return ((DBM *)0);
}

void
ndbmclose(db)
	DBM *db;
{

	(void) close(db->db_dirf);
	(void) close(db->db_pagf);
	free((char *)db);
}

long
dbmforder(db, key)
	register DBM *db;
	datum key;
{
	long hash;

	hash = dcalchash(key);
	for (db->db_hmask=0;; db->db_hmask=(db->db_hmask<<1)+1) {
		db->db_blkno = hash & db->db_hmask;
		db->db_bitno = db->db_blkno + db->db_hmask;
		if (getbit(db) == 0)
			break;
	}
	return (db->db_blkno);
}

datum
dbmfetch(db, key)
	register DBM *db;
	datum key;
{
	register i;
	datum item;

	dbm_access(db, dcalchash(key));
	for (i=0;; i+=2) {
		item = makdatum(db->db_pagbuf, i);
		if (item.dptr == NULL)
			return (item);
		if (cmpdatum(key, item) == 0) {
			item = makdatum(db->db_pagbuf, i+1);
			if (item.dptr == NULL)
				printf("items not in pairs\n");
			return (item);
		}
	}
}

dbmdelete(db, key)
	register DBM *db;
	datum key;
{
	register i;
	datum item;

	if (dbrdonly(db)) {
		errno = EPERM;
		return (-1);
	}
	dbm_access(db, dcalchash(key));
	for (i=0;; i+=2) {
		item = makdatum(db->db_pagbuf, i);
		if (item.dptr == NULL)
			return (-1);
		if (cmpdatum(key, item) == 0) {
			delitem(db->db_pagbuf, i);
			delitem(db->db_pagbuf, i);
			break;
		}
	}
	(void) lseek(db->db_pagf, db->db_blkno*PBLKSIZ, L_SET);
	(void) write(db->db_pagf, db->db_pagbuf, PBLKSIZ);
	db->db_pagbno = db->db_blkno;
	return (0);
}

dbmstore(db, key, dat, replace)
	register DBM *db;
	datum key, dat;
	int replace;
{
	register i;
	datum item;
	char ovfbuf[PBLKSIZ];

	if (dbrdonly(db)) {
		errno = EPERM;
		return (-1);
	}
loop:
	dbm_access(db, dcalchash(key));
	for (i=0;; i+=2) {
		item = makdatum(db->db_pagbuf, i);
		if (item.dptr == NULL)
			break;
		if (cmpdatum(key, item) == 0) {
			if (!replace)
				return (1);
			delitem(db->db_pagbuf, i);
			delitem(db->db_pagbuf, i);
			break;
		}
	}
	i = additem(db->db_pagbuf, key);
	if (i < 0)
		goto split;
	if (additem(db->db_pagbuf, dat) < 0) {
		delitem(db->db_pagbuf, i);
		goto split;
	}
	(void) lseek(db->db_pagf, db->db_blkno*PBLKSIZ, L_SET);
	(void) write(db->db_pagf, db->db_pagbuf, PBLKSIZ);
	db->db_pagbno = db->db_blkno;
	return (0);

split:
	if (key.dsize+dat.dsize+2*sizeof(short) >= PBLKSIZ) {
		errno = ENOSPC;
		return (-1);
	}
	bzero(ovfbuf, PBLKSIZ);
	for (i=0;;) {
		item = makdatum(db->db_pagbuf, i);
		if (item.dptr == NULL)
			break;
		if (dcalchash(item) & (db->db_hmask+1)) {
			additem(ovfbuf, item);
			delitem(db->db_pagbuf, i);
			item = makdatum(db->db_pagbuf, i);
			if (item.dptr == NULL) {
				printf("ndbm: split not paired\n");
				break;
			}
			additem(ovfbuf, item);
			delitem(db->db_pagbuf, i);
			continue;
		}
		i += 2;
	}
	(void) lseek(db->db_pagf, db->db_blkno*PBLKSIZ, L_SET);
	(void) write(db->db_pagf, db->db_pagbuf, PBLKSIZ);
	db->db_pagbno = db->db_blkno;
	(void) lseek(db->db_pagf, (db->db_blkno+db->db_hmask+1)*PBLKSIZ, L_SET);
	(void) write(db->db_pagf, ovfbuf, PBLKSIZ);
	setbit(db);
	goto loop;
}

datum
dbmfirstkey(db)
	DBM *db;
{

	return (firsthash(db, 0L));
}

datum
dbmnextkey(db, key)
	register DBM *db;
	datum key;
{
	register i;
	datum item, bitem;
	long hash;
	int f;

	hash = dcalchash(key);
	dbm_access(db, hash);
	f = 1;
	for (i=0;; i+=2) {
		item = makdatum(db->db_pagbuf, i);
		if (item.dptr == NULL)
			break;
		if (cmpdatum(key, item) <= 0)
			continue;
		if (f || cmpdatum(bitem, item) < 0) {
			bitem = item;
			f = 0;
		}
	}
	if (f == 0)
		return (bitem);
	hash = hashinc(db, hash);
	if (hash == 0)
		return (item);
	return (firsthash(db, hash));
}

static datum
firsthash(db, hash)
	register DBM *db;
	long hash;
{
	register i;
	datum item, bitem;

loop:
	dbm_access(db, hash);
	bitem = makdatum(db->db_pagbuf, 0);
	for (i=2;; i+=2) {
		item = makdatum(db->db_pagbuf, i);
		if (item.dptr == NULL)
			break;
		if (cmpdatum(bitem, item) < 0)
			bitem = item;
	}
	if (bitem.dptr != NULL)
		return (bitem);
	hash = hashinc(db, hash);
	if (hash == 0)
		return (item);
	goto loop;
}

static
dbm_access(db, hash)
	register DBM *db;
	long hash;
{
	
	for (db->db_hmask=0;; db->db_hmask=(db->db_hmask<<1)+1) {
		db->db_blkno = hash & db->db_hmask;
		db->db_bitno = db->db_blkno + db->db_hmask;
		if (getbit(db) == 0)
			break;
	}
	if (db->db_blkno != db->db_pagbno) {
		bzero(db->db_pagbuf, PBLKSIZ);
		(void) lseek(db->db_pagf, db->db_blkno*PBLKSIZ, L_SET);
		(void) read(db->db_pagf, db->db_pagbuf, PBLKSIZ);
		chkblk(db->db_pagbuf);
		db->db_pagbno = db->db_blkno;
	}
}

static
getbit(db)
	register DBM *db;
{
	long bn;
	register b, i, n;
	

	if (db->db_bitno > db->db_maxbno)
		return (0);
	n = db->db_bitno % BYTESIZ;
	bn = db->db_bitno / BYTESIZ;
	i = bn % DBLKSIZ;
	b = bn / DBLKSIZ;
	if (b != db->db_dirbno) {
		bzero(db->db_dirbuf, DBLKSIZ);
		(void) lseek(db->db_dirf, (long)b*DBLKSIZ, L_SET);
		(void) read(db->db_dirf, db->db_dirbuf, DBLKSIZ);
		db->db_dirbno = b;
	}
	if (db->db_dirbuf[i] & (1<<n))
		return (1);
	return (0);
}

static
setbit(db)
	register DBM *db;
{
	long bn;
	register i, n, b;

	if (dbrdonly(db)) {
		errno = EPERM;
		return (-1);
	}
	if (db->db_bitno > db->db_maxbno) {
		db->db_maxbno = db->db_bitno;
		getbit(db);
	}
	n = db->db_bitno % BYTESIZ;
	bn = db->db_bitno / BYTESIZ;
	i = bn % DBLKSIZ;
	b = bn / DBLKSIZ;
	db->db_dirbuf[i] |= 1<<n;
	(void) lseek(db->db_dirf, (long)b*DBLKSIZ, L_SET);
	(void) write(db->db_dirf, db->db_dirbuf, DBLKSIZ);
	db->db_dirbno = b;
}

static datum
makdatum(buf, n)
	char buf[PBLKSIZ];
{
	register short *sp;
	register t;
	datum item;

	sp = (short *)buf;
	if (n < 0 || n >= sp[0])
		goto null;
	t = PBLKSIZ;
	if (n > 0)
		t = sp[n+1-1];
	item.dptr = buf+sp[n+1];
	item.dsize = t - sp[n+1];
	return (item);

null:
	item.dptr = NULL;
	item.dsize = 0;
	return (item);
}

static
cmpdatum(d1, d2)
	datum d1, d2;
{
	register n;
	register char *p1, *p2;

	n = d1.dsize;
	if (n != d2.dsize)
		return (n - d2.dsize);
	if (n == 0)
		return (0);
	p1 = d1.dptr;
	p2 = d2.dptr;
	do
		if (*p1++ != *p2++)
			return (*--p1 - *--p2);
	while (--n);
	return (0);
}

static  int hitab[16]
/* ken's
{
	055,043,036,054,063,014,004,005,
	010,064,077,000,035,027,025,071,
};
*/
 = {    61, 57, 53, 49, 45, 41, 37, 33,
	29, 25, 21, 17, 13,  9,  5,  1,
};
static  long hltab[64]
 = {
	06100151277L,06106161736L,06452611562L,05001724107L,
	02614772546L,04120731531L,04665262210L,07347467531L,
	06735253126L,06042345173L,03072226605L,01464164730L,
	03247435524L,07652510057L,01546775256L,05714532133L,
	06173260402L,07517101630L,02431460343L,01743245566L,
	00261675137L,02433103631L,03421772437L,04447707466L,
	04435620103L,03757017115L,03641531772L,06767633246L,
	02673230344L,00260612216L,04133454451L,00615531516L,
	06137717526L,02574116560L,02304023373L,07061702261L,
	05153031405L,05322056705L,07401116734L,06552375715L,
	06165233473L,05311063631L,01212221723L,01052267235L,
	06000615237L,01075222665L,06330216006L,04402355630L,
	01451177262L,02000133436L,06025467062L,07121076461L,
	03123433522L,01010635225L,01716177066L,05161746527L,
	01736635071L,06243505026L,03637211610L,01756474365L,
	04723077174L,03642763134L,05750130273L,03655541561L,
};

static long
hashinc(db, hash)
	register DBM *db;
	long hash;
{
	long bit;

	hash &= db->db_hmask;
	bit = db->db_hmask+1;
	for (;;) {
		bit >>= 1;
		if (bit == 0)
			return (0L);
		if ((hash&bit) == 0)
			return (hash|bit);
		hash &= ~bit;
	}
}

static long
dcalchash(item)
	datum item;
{
	register i, j, f;
	long hashl;
	int hashi;

	hashl = 0;
	hashi = 0;
	for (i=0; i<item.dsize; i++) {
		f = item.dptr[i];
		for (j=0; j<BYTESIZ; j+=4) {
			hashi += hitab[f&017];
			hashl += hltab[hashi&63];
			f >>= 4;
		}
	}
	return (hashl);
}

static
delitem(buf, n)
	char buf[PBLKSIZ];
{
	register short *sp;
	register i1, i2, i3;

	sp = (short *)buf;
	if (n < 0 || n >= sp[0])
		goto bad;
	i1 = sp[n+1];
	i2 = PBLKSIZ;
	if (n > 0)
		i2 = sp[n+1-1];
	i3 = sp[sp[0]+1-1];
	if (i2 > i1)
	while (i1 > i3) {
		i1--;
		i2--;
		buf[i2] = buf[i1];
		buf[i1] = 0;
	}
	i2 -= i1;
	for (i1=n+1; i1<sp[0]; i1++)
		sp[i1+1-1] = sp[i1+1] + i2;
	sp[0]--;
	sp[sp[0]+1] = 0;
	return;

bad:
	printf("ndbm: bad delitem\n");
	abort();
}

static
additem(buf, item)
	char buf[PBLKSIZ];
	datum item;
{
	register short *sp;
	register i1, i2;

	sp = (short *)buf;
	i1 = PBLKSIZ;
	if (sp[0] > 0)
		i1 = sp[sp[0]+1-1];
	i1 -= item.dsize;
	i2 = (sp[0]+2) * sizeof(short);
	if (i1 <= i2)
		return (-1);
	sp[sp[0]+1] = i1;
	for (i2=0; i2<item.dsize; i2++) {
		buf[i1] = item.dptr[i2];
		i1++;
	}
	sp[0]++;
	return (sp[0]-1);
}

static
chkblk(buf)
	char buf[PBLKSIZ];
{
	register short *sp;
	register t, i;

	sp = (short *)buf;
	t = PBLKSIZ;
	for (i=0; i<sp[0]; i++) {
		if (sp[i+1] > t)
			goto bad;
		t = sp[i+1];
	}
	if (t < (sp[0]+1)*sizeof(short))
		goto bad;
	return;

bad:
	printf("ndbm: bad block\n");
	abort();
	bzero(buf, PBLKSIZ);
}
