#ifndef lint
static char sccsid[] = "@(#)ndbm.c	4.4 (Berkeley) %G%";
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>
#include <errno.h>
#include <ndbm.h>

#define BYTESIZ 8

static  datum firsthash();
static  datum makdatum();
static  long hashinc();
static  long dcalchash();
extern  int errno;

DBM *
dbm_open(file, flags, mode)
	char *file;
	int flags, mode;
{
	struct stat statb;
	register DBM *db;

	if ((db = (DBM *)malloc(sizeof *db)) == 0) {
		errno = ENOMEM;
		return ((DBM *)0);
	}
	db->dbm_flags = (flags & 03) == O_RDONLY ? _DBM_RDONLY : 0;
	if ((flags & 03) == O_WRONLY)
		flags = (flags & ~03) | O_RDWR;
	strcpy(db->dbm_pagbuf, file);
	strcat(db->dbm_pagbuf, ".pag");
	db->dbm_pagf = open(db->dbm_pagbuf, flags, mode);
	if (db->dbm_pagf < 0)
		goto bad;
	strcpy(db->dbm_pagbuf, file);
	strcat(db->dbm_pagbuf, ".dir");
	db->dbm_dirf = open(db->dbm_pagbuf, flags, mode);
	if (db->dbm_dirf < 0)
		goto bad1;
	fstat(db->dbm_dirf, &statb);
	db->dbm_maxbno = statb.st_size*BYTESIZ-1;
	db->dbm_pagbno = db->dbm_dirbno = -1;
	return (db);
bad1:
	(void) close(db->dbm_pagf);
bad:
	free((char *)db);
	return ((DBM *)0);
}

void
dbm_close(db)
	DBM *db;
{

	(void) close(db->dbm_dirf);
	(void) close(db->dbm_pagf);
	free((char *)db);
}

long
dbm_forder(db, key)
	register DBM *db;
	datum key;
{
	long hash;

	hash = dcalchash(key);
	for (db->dbm_hmask=0;; db->dbm_hmask=(db->dbm_hmask<<1)+1) {
		db->dbm_blkno = hash & db->dbm_hmask;
		db->dbm_bitno = db->dbm_blkno + db->dbm_hmask;
		if (getbit(db) == 0)
			break;
	}
	return (db->dbm_blkno);
}

datum
dbm_fetch(db, key)
	register DBM *db;
	datum key;
{
	register i;
	datum item;

	if (dbm_error(db))
		goto err;
	dbm_access(db, dcalchash(key));
	if (dbm_error(db))
		goto err;
	for (i=0;; i+=2) {
		item = makdatum(db->dbm_pagbuf, i);
		if (item.dptr == NULL)
			return (item);
		if (cmpdatum(key, item) == 0) {
			item = makdatum(db->dbm_pagbuf, i+1);
			if (item.dptr == NULL)
				fprintf(stderr, "dbm: items not in pairs\n");
			return (item);
		}
	}
err:
	item.dptr = NULL;
	item.dsize = 0;
	return (item);
}

dbm_delete(db, key)
	register DBM *db;
	datum key;
{
	register i;
	datum item;

	if (dbm_error(db))
		return (-1);
	if (dbm_rdonly(db)) {
		errno = EPERM;
		return (-1);
	}
	dbm_access(db, dcalchash(key));
	if (dbm_error(db))
		return (-1);
	for (i=0;; i+=2) {
		item = makdatum(db->dbm_pagbuf, i);
		if (item.dptr == NULL)
			return (-1);
		if (cmpdatum(key, item) == 0) {
			if (!delitem(db->dbm_pagbuf, i) ||
			    !delitem(db->dbm_pagbuf, i))
				db->dbm_flags |= _DBM_IOERR;
			break;
		}
	}
	db->dbm_pagbno = db->dbm_blkno;
	(void) lseek(db->dbm_pagf, db->dbm_blkno*PBLKSIZ, L_SET);
	if (write(db->dbm_pagf, db->dbm_pagbuf, PBLKSIZ) != PBLKSIZ) {
		db->dbm_flags |= _DBM_IOERR;
		return (-1);
	}
	return (0);
}

dbm_store(db, key, dat, replace)
	register DBM *db;
	datum key, dat;
	int replace;
{
	register i;
	datum item;
	char ovfbuf[PBLKSIZ];

	if (dbm_error(db))
		return (-1);
	if (dbm_rdonly(db)) {
		errno = EPERM;
		return (-1);
	}
loop:
	dbm_access(db, dcalchash(key));
	if (dbm_error(db))
		return (-1);
	for (i=0;; i+=2) {
		item = makdatum(db->dbm_pagbuf, i);
		if (item.dptr == NULL)
			break;
		if (cmpdatum(key, item) == 0) {
			if (!replace)
				return (1);
			if (!delitem(db->dbm_pagbuf, i) ||
			    !delitem(db->dbm_pagbuf, i)) {
				db->dbm_flags |= _DBM_IOERR;
				return (-1);
			}
		}
	}
	if (!additem(db->dbm_pagbuf, key))
		goto split;
	if (!additem(db->dbm_pagbuf, dat)) {
		/* special case of delitem() expanded inline here */
		((short *) db->dbm_pagbuf)[0]--;
		goto split;
	}
	db->dbm_pagbno = db->dbm_blkno;
	(void) lseek(db->dbm_pagf, db->dbm_blkno*PBLKSIZ, L_SET);
	if (write(db->dbm_pagf, db->dbm_pagbuf, PBLKSIZ) != PBLKSIZ) {
		db->dbm_flags |= _DBM_IOERR;
		return (-1);
	}
	return (0);

split:
	if (key.dsize+dat.dsize+2*sizeof(short) >= PBLKSIZ) {
		db->dbm_flags |= _DBM_IOERR;
		errno = ENOSPC;
		return (-1);
	}
	bzero(ovfbuf, PBLKSIZ);
	for (i=0;;) {
		item = makdatum(db->dbm_pagbuf, i);
		if (item.dptr == NULL)
			break;
		if (dcalchash(item) & (db->dbm_hmask+1)) {
			if (!additem(ovfbuf, item) ||
			    !delitem(db->dbm_pagbuf, i)) {
				db->dbm_flags |= _DBM_IOERR;
				return (-1);
			}
			item = makdatum(db->dbm_pagbuf, i);
			if (item.dptr == NULL) {
				fprintf(stderr, "ndbm: split not paired\n");
				db->dbm_flags |= _DBM_IOERR;
				break;
			}
			if (!additem(ovfbuf, item) ||
			    !delitem(db->dbm_pagbuf, i)) {
				db->dbm_flags |= _DBM_IOERR;
				return (-1);
			}
			continue;
		}
		i += 2;
	}
	db->dbm_pagbno = db->dbm_blkno;
	(void) lseek(db->dbm_pagf, db->dbm_blkno*PBLKSIZ, L_SET);
	if (write(db->dbm_pagf, db->dbm_pagbuf, PBLKSIZ) != PBLKSIZ) {
		db->dbm_flags |= _DBM_IOERR;
		return (-1);
	}
	(void) lseek(db->dbm_pagf, (db->dbm_blkno+db->dbm_hmask+1)*PBLKSIZ, L_SET);
	if (write(db->dbm_pagf, ovfbuf, PBLKSIZ) != PBLKSIZ) {
		db->dbm_flags |= _DBM_IOERR;
		return (-1);
	}
	setbit(db);
	goto loop;
}

datum
dbm_firstkey(db)
	DBM *db;
{

	return (firsthash(db, 0L));
}

datum
dbm_nextkey(db, key)
	register DBM *db;
	datum key;
{
	register i;
	datum item, bitem;
	long hash;
	int f;

	if (dbm_error(db))
		goto err;
	dbm_access(db, hash = dcalchash(key));
	if (dbm_error(db))
		goto err;
	f = 1;
	for (i=0;; i+=2) {
		item = makdatum(db->dbm_pagbuf, i);
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
err:
	item.dptr = NULL;
	item.dsize = 0;
	return (item);
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
	if (dbm_error(db)) {
		item.dptr = NULL;
		return (item);
	}
	bitem = makdatum(db->dbm_pagbuf, 0);
	for (i=2;; i+=2) {
		item = makdatum(db->dbm_pagbuf, i);
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
	
	for (db->dbm_hmask=0;; db->dbm_hmask=(db->dbm_hmask<<1)+1) {
		db->dbm_blkno = hash & db->dbm_hmask;
		db->dbm_bitno = db->dbm_blkno + db->dbm_hmask;
		if (getbit(db) == 0)
			break;
	}
	if (db->dbm_blkno != db->dbm_pagbno) {
		db->dbm_pagbno = db->dbm_blkno;
		(void) lseek(db->dbm_pagf, db->dbm_blkno*PBLKSIZ, L_SET);
		if (read(db->dbm_pagf, db->dbm_pagbuf, PBLKSIZ) != PBLKSIZ)
			bzero(db->dbm_pagbuf, PBLKSIZ);
		else if (chkblk(db->dbm_pagbuf) < 0)
			db->dbm_flags |= _DBM_IOERR;
	}
}

static
getbit(db)
	register DBM *db;
{
	long bn;
	register b, i, n;
	

	if (db->dbm_bitno > db->dbm_maxbno)
		return (0);
	n = db->dbm_bitno % BYTESIZ;
	bn = db->dbm_bitno / BYTESIZ;
	i = bn % DBLKSIZ;
	b = bn / DBLKSIZ;
	if (b != db->dbm_dirbno) {
		db->dbm_dirbno = b;
		(void) lseek(db->dbm_dirf, (long)b*DBLKSIZ, L_SET);
		if (read(db->dbm_dirf, db->dbm_dirbuf, DBLKSIZ) != DBLKSIZ)
			bzero(db->dbm_dirbuf, DBLKSIZ);
	}
	if (db->dbm_dirbuf[i] & (1<<n))
		return (1);
	return (0);
}

static
setbit(db)
	register DBM *db;
{
	long bn;
	register i, n, b;

	if (dbm_rdonly(db)) {
		errno = EPERM;
		return (-1);
	}
	if (db->dbm_bitno > db->dbm_maxbno) {
		db->dbm_maxbno = db->dbm_bitno;
		getbit(db);
	}
	n = db->dbm_bitno % BYTESIZ;
	bn = db->dbm_bitno / BYTESIZ;
	i = bn % DBLKSIZ;
	b = bn / DBLKSIZ;
	db->dbm_dirbuf[i] |= 1<<n;
	db->dbm_dirbno = b;
	(void) lseek(db->dbm_dirf, (long)b*DBLKSIZ, L_SET);
	if (write(db->dbm_dirf, db->dbm_dirbuf, DBLKSIZ) != DBLKSIZ) {
		db->dbm_flags |= _DBM_IOERR;
		return (-1);
	}
	return (0);
}

static datum
makdatum(buf, n)
	char buf[PBLKSIZ];
{
	register short *sp;
	register t;
	datum item;

	sp = (short *)buf;
	if (n < 0 || n >= sp[0]) {
		item.dptr = NULL;
		item.dsize = 0;
		return (item);
	}
	t = PBLKSIZ;
	if (n > 0)
		t = sp[n];
	item.dptr = buf+sp[n+1];
	item.dsize = t - sp[n+1];
	return (item);
}

static
cmpdatum(d1, d2)
	datum d1, d2;
{
	register int n;
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

	hash &= db->dbm_hmask;
	bit = db->dbm_hmask+1;
	for (;;) {
		bit >>= 1;
		if (bit == 0)
			return (0L);
		if ((hash & bit) == 0)
			return (hash | bit);
		hash &= ~bit;
	}
}

static long
dcalchash(item)
	datum item;
{
	register int s, c, j;
	register char *cp;
	register long hashl;
	register int hashi;

	hashl = 0;
	hashi = 0;
	for (cp = item.dptr, s=item.dsize; --s >= 0; ) {
		c = *cp++;
		for (j=0; j<BYTESIZ; j+=4) {
			hashi += hitab[c&017];
			hashl += hltab[hashi&63];
			c >>= 4;
		}
	}
	return (hashl);
}

static
delitem(buf, n)
	char buf[PBLKSIZ];
{
	register short *sp, *sp1;
	register i1, i2;

	sp = (short *)buf;
	i2 = sp[0];
	if (n < 0 || n >= i2)
		return (0);
	if (n == i2-1) {
		sp[0]--;
		return (1);
	}
	i1 = PBLKSIZ;
	if (n > 0)
		i1 = sp[n];
	i1 -= sp[n+1];
	if (i1 > 0) {
		i2 = sp[i2];
		bcopy(&buf[i2], &buf[i2 + i1], sp[n+1] - i2);
	}
	for (sp1 = sp + sp[0]--, sp += n+1; sp < sp1; sp++)
		sp[0] = sp[1] + i1;
	return (1);
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
	i2 = sp[0];
	if (i2 > 0)
		i1 = sp[i2];
	i1 -= item.dsize;
	i2 = (i2+2) * sizeof(short);
	if (i1 <= i2)
		return (0);
	sp[++sp[0]] = i1;
	bcopy(item.dptr, &buf[i1], item.dsize);
	return (1);
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
			return (-1);
		t = sp[i+1];
	}
	if (t < (sp[0]+1)*sizeof(short))
		return (-1);
	return (0);
}
