#ifndef lint
static char sccsid[] = "@(#)dbm.c	4.4 (Berkeley) %G%";
#endif

#include	"dbm.h"
#include	<sys/types.h>
#include	<sys/stat.h>

dbminit(file)
char *file;
{
	struct stat statb;

	dbrdonly = 0;
	strcpy(pagbuf, file);
	strcat(pagbuf, ".pag");
	pagf = open(pagbuf, 2);
	if (pagf < 0) {
		pagf = open(pagbuf, 0);
		dbrdonly = 1;
	}

	strcpy(pagbuf, file);
	strcat(pagbuf, ".dir");
	dirf = open(pagbuf, 2);
	if (dirf < 0) {
		dirf = open(pagbuf, 0);
		dbrdonly = 1;
	}
	if(pagf < 0 || dirf < 0) {
		printf("dbm: %s: cannot open database\n", file);
		return(-1);
	}
	fstat(dirf, &statb);
	maxbno = statb.st_size*BYTESIZ-1;
	return(0);
}

long
forder(key)
datum key;
{
	long hash;

	hash = calchash(key);
	for(hmask=0;; hmask=(hmask<<1)+1) {
		blkno = hash & hmask;
		bitno = blkno + hmask;
		if(getbit() == 0)
			break;
	}
	return(blkno);
}

datum
fetch(key)
datum key;
{
	register i;
	datum item;

	dbm_access(calchash(key));
	for(i=0;; i+=2) {
		item = makdatum(pagbuf, i);
		if(item.dptr == NULL)
			return(item);
		if(cmpdatum(key, item) == 0) {
			item = makdatum(pagbuf, i+1);
			if(item.dptr == NULL)
				printf("dbm: items not in pairs\n");
			return(item);
		}
	}
}

delete(key)
datum key;
{
	register i;
	datum item;

	if (dbrdonly)
		return -1;
	dbm_access(calchash(key));
	for(i=0;; i+=2) {
		item = makdatum(pagbuf, i);
		if(item.dptr == NULL)
			return(-1);
		if(cmpdatum(key, item) == 0) {
			delitem(pagbuf, i);
			delitem(pagbuf, i);
			break;
		}
	}
	lseek(pagf, blkno*PBLKSIZ, 0);
	write(pagf, pagbuf, PBLKSIZ);
	return(0);
}

store(key, dat)
datum key, dat;
{
	register i;
	datum item;
	char ovfbuf[PBLKSIZ];

	if (dbrdonly)
		return -1;
loop:
	dbm_access(calchash(key));
	for(i=0;; i+=2) {
		item = makdatum(pagbuf, i);
		if(item.dptr == NULL)
			break;
		if(cmpdatum(key, item) == 0) {
			delitem(pagbuf, i);
			delitem(pagbuf, i);
			break;
		}
	}
	i = additem(pagbuf, key);
	if(i < 0)
		goto split;
	if(additem(pagbuf, dat) < 0) {
		delitem(pagbuf, i);
		goto split;
	}
	lseek(pagf, blkno*PBLKSIZ, 0);
	write(pagf, pagbuf, PBLKSIZ);
	return (0);

split:
	if(key.dsize+dat.dsize+3*sizeof(short) >= PBLKSIZ) {
		printf("dbm: entry too big\n");
		return (-1);
	}
	clrbuf(ovfbuf, PBLKSIZ);
	for(i=0;;) {
		item = makdatum(pagbuf, i);
		if(item.dptr == NULL)
			break;
		if(calchash(item) & (hmask+1)) {
			additem(ovfbuf, item);
			delitem(pagbuf, i);
			item = makdatum(pagbuf, i);
			if(item.dptr == NULL) {
				printf("dbm: split not paired\n");
				break;
			}
			additem(ovfbuf, item);
			delitem(pagbuf, i);
			continue;
		}
		i += 2;
	}
	lseek(pagf, blkno*PBLKSIZ, 0);
	write(pagf, pagbuf, PBLKSIZ);
	lseek(pagf, (blkno+hmask+1)*PBLKSIZ, 0);
	write(pagf, ovfbuf, PBLKSIZ);
	setbit();
	goto loop;
}

datum
firstkey()
{
	return(firsthash(0L));
}

datum
nextkey(key)
datum key;
{
	register i;
	datum item, bitem;
	long hash;
	int f;

	hash = calchash(key);
	dbm_access(hash);
	f = 1;
	for(i=0;; i+=2) {
		item = makdatum(pagbuf, i);
		if(item.dptr == NULL)
			break;
		if(cmpdatum(key, item) <= 0)
			continue;
		if(f || cmpdatum(bitem, item) < 0) {
			bitem = item;
			f = 0;
		}
	}
	if(f == 0)
		return(bitem);
	hash = hashinc(hash);
	if(hash == 0)
		return(item);
	return(firsthash(hash));
}

datum
firsthash(hash)
long hash;
{
	register i;
	datum item, bitem;

loop:
	dbm_access(hash);
	bitem = makdatum(pagbuf, 0);
	for(i=2;; i+=2) {
		item = makdatum(pagbuf, i);
		if(item.dptr == NULL)
			break;
		if(cmpdatum(bitem, item) < 0)
			bitem = item;
	}
	if(bitem.dptr != NULL)
		return(bitem);
	hash = hashinc(hash);
	if(hash == 0)
		return(item);
	goto loop;
}

dbm_access(hash)
long hash;
{
	static long oldb = -1;

	for(hmask=0;; hmask=(hmask<<1)+1) {
		blkno = hash & hmask;
		bitno = blkno + hmask;
		if(getbit() == 0)
			break;
	}
	if(blkno != oldb) {
		clrbuf(pagbuf, PBLKSIZ);
		lseek(pagf, blkno*PBLKSIZ, 0);
		read(pagf, pagbuf, PBLKSIZ);
		chkblk(pagbuf);
		oldb = blkno;
	}
}

getbit()
{
	long bn;
	register b, i, n;
	static oldb = -1;

	if(bitno > maxbno)
		return(0);
	n = bitno % BYTESIZ;
	bn = bitno / BYTESIZ;
	i = bn % DBLKSIZ;
	b = bn / DBLKSIZ;
	if(b != oldb) {
		clrbuf(dirbuf, DBLKSIZ);
		lseek(dirf, (long)b*DBLKSIZ, 0);
		read(dirf, dirbuf, DBLKSIZ);
		oldb = b;
	}
	if(dirbuf[i] & (1<<n))
		return(1);
	return(0);
}

setbit()
{
	long bn;
	register i, n, b;

	if (dbrdonly)
		return -1;
	if(bitno > maxbno) {
		maxbno = bitno;
		getbit();
	}
	n = bitno % BYTESIZ;
	bn = bitno / BYTESIZ;
	i = bn % DBLKSIZ;
	b = bn / DBLKSIZ;
	dirbuf[i] |= 1<<n;
	lseek(dirf, (long)b*DBLKSIZ, 0);
	write(dirf, dirbuf, DBLKSIZ);
}

clrbuf(cp, n)
register char *cp;
register n;
{

	do
		*cp++ = 0;
	while(--n);
}

datum
makdatum(buf, n)
char buf[PBLKSIZ];
{
	register short *sp;
	register t;
	datum item;

	sp = (short *)buf;
	if(n < 0 || n >= sp[0])
		goto null;
	t = PBLKSIZ;
	if(n > 0)
		t = sp[n+1-1];
	item.dptr = buf+sp[n+1];
	item.dsize = t - sp[n+1];
	return(item);

null:
	item.dptr = NULL;
	item.dsize = 0;
	return(item);
}

cmpdatum(d1, d2)
datum d1, d2;
{
	register n;
	register char *p1, *p2;

	n = d1.dsize;
	if(n != d2.dsize)
		return(n - d2.dsize);
	if(n == 0)
		return(0);
	p1 = d1.dptr;
	p2 = d2.dptr;
	do
		if(*p1++ != *p2++)
			return(*--p1 - *--p2);
	while(--n);
	return(0);
}

int	hitab[16]
/* ken's
{
	055,043,036,054,063,014,004,005,
	010,064,077,000,035,027,025,071,
};
*/
 = {	61, 57, 53, 49, 45, 41, 37, 33,
	29, 25, 21, 17, 13,  9,  5,  1,
};
long	hltab[64]
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

long
hashinc(hash)
long hash;
{
	long bit;

	hash &= hmask;
	bit = hmask+1;
	for(;;) {
		bit >>= 1;
		if(bit == 0)
			return(0L);
		if((hash&bit) == 0)
			return(hash|bit);
		hash &= ~bit;
	}
}

long
calchash(item)
datum item;
{
	register i, j, f;
	long hashl;
	int hashi;

	hashl = 0;
	hashi = 0;
	for(i=0; i<item.dsize; i++) {
		f = item.dptr[i];
		for(j=0; j<BYTESIZ; j+=4) {
			hashi += hitab[f&017];
			hashl += hltab[hashi&63];
			f >>= 4;
		}
	}
	return(hashl);
}

delitem(buf, n)
char buf[PBLKSIZ];
{
	register short *sp;
	register i1, i2, i3;

	sp = (short *)buf;
	if(n < 0 || n >= sp[0])
		goto bad;
	i1 = sp[n+1];
	i2 = PBLKSIZ;
	if(n > 0)
		i2 = sp[n+1-1];
	i3 = sp[sp[0]+1-1];
	if(i2 > i1)
	while(i1 > i3) {
		i1--;
		i2--;
		buf[i2] = buf[i1];
		buf[i1] = 0;
	}
	i2 -= i1;
	for(i1=n+1; i1<sp[0]; i1++)
		sp[i1+1-1] = sp[i1+1] + i2;
	sp[0]--;
	sp[sp[0]+1] = 0;
	return;

bad:
	printf("dbm: bad delitem\n");
	abort();
}

additem(buf, item)
char buf[PBLKSIZ];
datum item;
{
	register short *sp;
	register i1, i2;

	sp = (short *)buf;
	i1 = PBLKSIZ;
	if(sp[0] > 0)
		i1 = sp[sp[0]+1-1];
	i1 -= item.dsize;
	i2 = (sp[0]+2) * sizeof(short);
	if(i1 <= i2)
		return(-1);
	sp[sp[0]+1] = i1;
	for(i2=0; i2<item.dsize; i2++) {
		buf[i1] = item.dptr[i2];
		i1++;
	}
	sp[0]++;
	return(sp[0]-1);
}

chkblk(buf)
char buf[PBLKSIZ];
{
	register short *sp;
	register t, i;

	sp = (short *)buf;
	t = PBLKSIZ;
	for(i=0; i<sp[0]; i++) {
		if(sp[i+1] > t)
			goto bad;
		t = sp[i+1];
	}
	if(t < (sp[0]+1)*sizeof(short))
		goto bad;
	return;

bad:
	printf("dbm: bad block\n");
	abort();
	clrbuf(buf, PBLKSIZ);
}
