#include "tp.h"

gettape(how)
int (*how)();
{
	register char *ptr0, *ptr1;
	register struct dent *d;
	int count;

	do {
		d = &dir[0];
		count = 0;
		do {
			if (d->d_namep == 0)  continue;
			decode(name,d);
			if (rnarg > 2) {
				ptr0 = name;
				ptr1 = *parg;
				while (*ptr1)
					if (*ptr0++ != *ptr1++)  goto cont;
				if (*ptr0 && *ptr0 != '/')       goto cont;
			}
			(*how)(d);  /* delete, extract, or taboc */
			++count;
cont:			continue;
		}  while (++d <= lastd);
		if (count == 0 && rnarg > 2)
			printf("%s  not found\n", *parg);
		++parg;
	} while (--narg > 2);
}

delete(dd)
struct dent *dd;
{
	if (verify('d') >= 0)
		clrent(dd);
}


update()
{
	register struct dent *d;
	register b, last;
	int first, size;


	bitmap();
	d = &dir[0];
	do {
		if(d->d_namep == 0 || (d->d_mode&OK) == 0) continue;
		if (d->d_size == 0)	  continue;
/* find a place on the tape for this file */
		size = (d->d_size+BSIZE-1)/BSIZE;
		first = ndentb;
toosmall:	++first;
		if ((last = first + size) >= tapsiz)	maperr();
		for (b = first; b < last; ++b)
			if (map[(b>>3) & MAPMASK] & (1<<(b&7))) {
				first = b;
				goto toosmall;
			};
		d->d_tapea = first;
		setmap(d);
	}  while (++d <= lastd);
	wrdir();
	update1();
}


update1()
{
	register struct dent *d, *id;
	register index;
	int f;

	for (;;) {
		d = &dir[0];
		index = MTSIZ;
		id = 0;
		do {	/* find new dent with lowest tape address */
			if(d->d_namep == 0 || (d->d_mode&OK) == 0) continue;
			if (d->d_tapea < index) {
				index = d->d_tapea;
				id = d;
			}
		} while (++d <= lastd);
		if ((d = id) == 0)	return;
		d->d_mode &= ~OK;  /* change from new to old */
		if (d->d_size == 0)  continue;
		decode(name,d);
		wseek(index);
		if ((f = open(name,0)) < 0) {
			printf("Can't open %s\n", name);
			continue;
		}
		for (index = d->d_size/BSIZE; index != 0; --index)  {
			if (read(f,(char *)tapeb,BSIZE) != BSIZE)	    phserr();
			twrite();
		}
		if (index = d->d_size % BSIZE) {
			if (read(f,(char *)tapeb,index) != index)  phserr();
			twrite();
		}
		if (read(f,(char *)tapeb,1) != 0)		    phserr();
		close(f);
	}
}

phserr()
{	printf("%s -- Phase error \n", name);  }


bitmap()	/* place old files in the map */
{
	register char *m;
	register count;
	register struct dent *d;

	for(m=map;m<&map[MAPSIZE];) *m++ = 0;
	count = ndirent;
	d = dir;
	do {
		if(d->d_namep != 0 && (d->d_mode&OK) == 0
		   && d->d_size != 0) setmap(d);
		d++;
	}  while (--count);
}

setmap(d)
register struct dent *d;
{
	unsigned c, block;
	char bit;
	int i;

	c = d->d_size/BSIZE;
	if (d->d_size % BSIZE)  c++;
	block = d->d_tapea;
	if ((c += block) >= tapsiz)		maperr();
	do {
		bit = 1 << (block & 7);
		i = (block>>3) & MAPMASK;
		if (bit & map[i])		maperr();
		map[i] |= bit;
	} while (++block < c);
}

maperr()
{
	printf("Tape overflow\n");
	done();
}


usage()
{
	register reg,count;
	int	nused, nentr, nfree;
	static lused;

	bitmap();
	for(count=0,nentr=0;count<ndirent;count++)
		if(dir[count].d_namep != 0) nentr++;
	nused = nfree = 0;
	reg = ndentb;
	++reg;		/* address of first non-directory tape block */
	count = tapsiz - reg;
	do {
		if (reg >= tapsiz) {
			printf("Tape overflow\n");
			done();
		}
		if (map[(reg>>3) & MAPMASK] & (1 << (reg&7))) {
			nused++;
			lused = reg;
		} else {
			if (flags & flm)   break;
			nfree++;
		}
		reg++;
	} while (--count);
	printf("%4d entries\n%4d used\n", nentr, nused);
	if ((flags & flm)==0)
		printf("%4d free\n", nfree);
	printf("%4d last\n", lused);
}


taboc(dd)
struct dent *dd;
{
	register  mode;
	register *m;
	register char *s;
	int count, *localtime();
	char work[20];

	if (flags & flv)  {
		mode = dd->d_mode;
		s = &work[19];
		*s = 0;
		for (count = 3; count; --count) {
			if (mode&1)	*--s = 'x';
			  else		*--s = '-';
			if (mode&2)	*--s = 'w';
			  else		*--s = '-';
			if (mode&4)	*--s = 'r';
			  else		*--s = '-';
			mode >>= 3;
		}
		if (mode&4)		s[2] = 's';
		if (mode&2)		s[5] = 's';
		printf("%s%4d%4d%5d%9D ",s,dd->d_uid, dd->d_gid,dd->d_tapea,dd->d_size);
		m = localtime(&dd->d_time);
		printf("%2d/%2d/%2d %2d:%2d ",m[5],m[4]+1,m[3],m[2],m[1]);
	}
	printf("%s\n", name);
}


extract(d)
register struct dent *d;
{
	register count, id;

	if (d->d_size==0)	return;
	if (verify('x') < 0)			return;
	rseek(d->d_tapea);
	unlink(name);
	if ((id = creat(name,d->d_mode)) < 0)
		printf("%s -- create error\n", name);
	count = d->d_size/BSIZE;
	while (count--) {
		tread();
		if (write(id, (char *)tapeb, BSIZE) != BSIZE)	goto ng;
	}
	if (count = d->d_size % BSIZE) {
		tread();
		if (write(id, (char *)tapeb, count) != count) {
ng:			printf("%s -- write error\n", name);
			close(id);
			return;
		}
	}
	close(id);
	chown(name,d->d_uid & 0377, d->d_gid&0377);
}
