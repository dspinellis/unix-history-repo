#define	NI	16
#define	DIRPB	(BSIZE/sizeof(struct direct))

#include <stdio.h>
#include <sys/param.h>
#include <sys/inode.h>
#include <sys/ino.h>
#include <sys/fblk.h>
#include <sys/filsys.h>
#include <sys/dir.h>
#include <dumprestor.h>

#define	MWORD(m,i) (m[(unsigned)(i-1)/MLEN])
#define	MBIT(i)	(1<<((unsigned)(i-1)%MLEN))
#define	BIS(i,w)	(MWORD(w,i) |=  MBIT(i))
#define	BIC(i,w)	(MWORD(w,i) &= ~MBIT(i))
#define	BIT(i,w)	(MWORD(w,i) & MBIT(i))

struct	filsys	sblock;
struct	dinode	itab[INOPB*NI];
short	clrmap[MSIZ];
short	dirmap[MSIZ];
short	nodmap[MSIZ];

char	*disk;
char	*tape;
char	*increm;
char	incno;
int	uflag;
int	fi;
int	to;
ino_t	ino;
int	nsubdir;
int	ntape;
int	nadded;
int	dadded;
int	density = 160;

char	*ctime();
char	*prdate();
long	atol();
int	fi;
long	tsize;
long	esize;
long	asize;
int	mark();
int	add();
int	dump();
int	tapsrec();
int	dmpspc();
int	dsrch();
int	nullf();

#define	HOUR	(60L*60L)
#define	DAY	(24L*HOUR)
#define	YEAR	(365L*DAY)

main(argc, argv)
char *argv[];
{
	char *arg;
	register i;

	time(&spcl.c_date);

	tsize = 2300L*12L*10L;
	tape = "/dev/rmt1";
	disk = "/dev/rrp3";
	increm = "/etc/ddate";
	incno = '9';
	uflag = 0;
	arg = "u";
	if(argc > 1) {
		argv++;
		argc--;
		arg = *argv;
	}
	while(*arg)
	switch (*arg++) {

	case 'f':
		if(argc > 1) {
			argv++;
			argc--;
			tape = *argv;
		}
		break;

	case 'd':
		if (argc > 1) {
			argv++;
			argc--;
			density = atoi(*argv)/10;
		}
		break;

	case 's':
		if(argc > 1) {
			argv++;
			argc--;
			tsize = atol(*argv);
			tsize *= 12L*10L;
		}
		break;

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		incno = arg[-1];
		break;

	case 'u':
		uflag++;
		break;

	default:
		printf("bad key '%c%'\n", arg[-1]);
		exit(1);
	}
	if(argc > 1) {
		argv++;
		argc--;
		disk = *argv;
	}

	getitime();
	printf("     date = %s\n", prdate(spcl.c_date));
	printf("dump date = %s\n", prdate(spcl.c_ddate));
	printf("dumping %s to %s\n", disk, tape);
	fi = open(disk, 0);
	if(fi < 0) {
		printf("dump: cannot open %s\n", disk);
		exit(1);
	}
	otape();
	printf("I\n");
	esize = 0;
	CLR(clrmap);
	CLR(dirmap);
	CLR(nodmap);

	pass(mark, (short *)NULL);
	do {
		printf("II\n");
		nadded = 0;
		pass(add, dirmap);
	} while(nadded);

	bmapest(clrmap);
	bmapest(nodmap);
	printf("estimated %ld tape blocks on %d tape(s)\n",
		esize, 0);

	printf("III\n");
	bitmap(clrmap, TS_CLRI);
	pass(dump, dirmap);
	printf("IV\n");
	pass(dump, nodmap);
	putitime();
	printf("DONE\n");
	spcl.c_type = TS_END;
	for(i=0; i<NTREC; i++)
		spclrec();
	printf("%ld tape blocks on %d tape(s)\n",
		spcl.c_tapea, spcl.c_volume);
}

pass(fn, map)
int (*fn)();
short *map;
{
	register i, j;
	int bits;
	ino_t mino;
	daddr_t d;

	sync();
	bread((daddr_t)1, (char *)&sblock, sizeof(sblock));
	mino = (sblock.s_isize-2) * INOPB;
	ino = 0;
	for(i=2;; i+=NI) {
		if(ino >= mino)
			break;
		d = (unsigned)i;
		for(j=0; j<INOPB*NI; j++) {
			if(ino >= mino)
				break;
			if((ino % MLEN) == 0) {
				bits = ~0;
				if(map != NULL)
					bits = *map++;
			}
			ino++;
			if(bits & 1) {
				if(d != 0) {
					bread(d, (char *)itab, sizeof(itab));
					d = 0;
				}
				(*fn)(&itab[j]);
			}
			bits >>= 1;
		}
	}
}

icat(ip, fn1, fn2)
struct	dinode	*ip;
int (*fn1)(), (*fn2)();
{
	register i;
	daddr_t d[NADDR];

	l3tol(&d[0], &ip->di_addr[0], NADDR);
	(*fn2)(d, NADDR-3);
	for(i=0; i<NADDR; i++) {
		if(d[i] != 0) {
			if(i < NADDR-3)
				(*fn1)(d[i]); else
				indir(d[i], fn1, fn2, i-(NADDR-3));
		}
	}
}

indir(d, fn1, fn2, n)
daddr_t d;
int (*fn1)(), (*fn2)();
{
	register i;
	daddr_t	idblk[NINDIR];

	bread(d, (char *)idblk, sizeof(idblk));
	if(n <= 0) {
		spcl.c_type = TS_ADDR;
		(*fn2)(idblk, NINDIR);
		for(i=0; i<NINDIR; i++) {
			d = idblk[i];
			if(d != 0)
				(*fn1)(d);
		}
	} else {
		n--;
		for(i=0; i<NINDIR; i++) {
			d = idblk[i];
			if(d != 0)
				indir(d, fn1, fn2, n);
		}
	}
}

mark(ip)
struct dinode *ip;
{
	register f;

	f = ip->di_mode & IFMT;
	if(f == 0)
		return;
	BIS(ino, clrmap);
	if(f == IFDIR)
		BIS(ino, dirmap);
	if(ip->di_mtime >= spcl.c_ddate ||
	   ip->di_ctime >= spcl.c_ddate) {
		BIS(ino, nodmap);
		if (f != IFREG)
			return;
		est(ip);
	}
}

add(ip)
struct dinode *ip;
{

	if(BIT(ino, nodmap))
		return;
	nsubdir = 0;
	dadded = 0;
	icat(ip, dsrch, nullf);
	if(dadded) {
		BIS(ino, nodmap);
		est(ip);
		nadded++;
	}
	if(nsubdir == 0)
		if(!BIT(ino, nodmap))
			BIC(ino, dirmap);
}

dump(ip)
struct dinode *ip;
{
	register i;

	if(ntape) {
		ntape = 0;
		bitmap(nodmap, TS_BITS);
	}
	BIC(ino, nodmap);
	spcl.c_dinode = *ip;
	spcl.c_type = TS_INODE;
	spcl.c_count = 0;
	i = ip->di_mode & IFMT;
	if(i != IFDIR && i != IFREG) {
		spclrec();
		return;
	}
	icat(ip, tapsrec, dmpspc);
}

dmpspc(dp, n)
daddr_t *dp;
{
	register i, t;

	spcl.c_count = n;
	for(i=0; i<n; i++) {
		t = 0;
		if(dp[i] != 0)
			t++;
		spcl.c_addr[i] = t;
	}
	spclrec();
}

bitmap(map, typ)
short *map;
{
	register i, n;
	char *cp;

	n = -1;
	for(i=0; i<MSIZ; i++)
		if(map[i])
			n = i;
	if(n < 0)
		return;
	spcl.c_type = typ;
	spcl.c_count = (n*sizeof(map[0]) + BSIZE)/BSIZE;
	spclrec();
	cp = (char *)map;
	for(i=0; i<spcl.c_count; i++) {
		taprec(cp);
		cp += BSIZE;
	}
}

spclrec()
{
	register i, *ip, s;

	spcl.c_inumber = ino;
	spcl.c_magic = MAGIC;
	spcl.c_checksum = 0;
	ip = (int *)&spcl;
	s = 0;
	for(i=0; i<BSIZE/sizeof(*ip); i++)
		s += *ip++;
	spcl.c_checksum = CHECKSUM - s;
	taprec((char *)&spcl);
}

dsrch(d)
daddr_t d;
{
	register char *cp;
	register i;
	register ino_t in;
	struct direct dblk[DIRPB];

	if(dadded)
		return;
	bread(d, (char *)dblk, sizeof(dblk));
	for(i=0; i<DIRPB; i++) {
		in = dblk[i].d_ino;
		if(in == 0)
			continue;
		cp = dblk[i].d_name;
		if(cp[0] == '.') {
			if(cp[1] == '\0')
				continue;
			if(cp[1] == '.' && cp[2] == '\0')
				continue;
		}
		if(BIT(in, nodmap)) {
			dadded++;
			return;
		}
		if(BIT(in, dirmap))
			nsubdir++;
	}
}

nullf()
{
}

bread(da, ba, c)
daddr_t da;
char *ba;
{
	register n;

	lseek(fi, da*512, 0);
	n = read(fi, ba, c);
	if(n != c)
		printf("asked %d; got %d\n", c, n);
}

CLR(map)
register short *map;
{
	register n;

	n = MSIZ;
	do
		*map++ = 0;
	while(--n);
}


char	tblock[NTREC][BSIZE];
daddr_t	tdaddr[NTREC];
int	trecno;

taprec(dp)
char *dp;
{
	register i;

	for(i=0; i<BSIZE; i++)
		tblock[trecno][i] = *dp++;
	tdaddr[trecno] = 0;
	trecno++;
	spcl.c_tapea++;
	if(trecno >= NTREC)
		flusht();
}

tapsrec(d)
daddr_t d;
{

	if(d == 0)
		return;
	tdaddr[trecno] = d;
	trecno++;
	spcl.c_tapea++;
	if(trecno >= NTREC)
		flusht();
}

flusht()
{
	char place[100];
	register i, si;
	daddr_t d;

	while(trecno < NTREC)
		tdaddr[trecno++] = 1;

loop:
	d = 0;
	for(i=0; i<NTREC; i++)
		if(tdaddr[i] != 0)
		if(d == 0 || tdaddr[i] < d) {
			si = i;
			d = tdaddr[i];
		}
	if(d != 0) {
		bread(d, tblock[si], BSIZE);
		tdaddr[si] = 0;
		goto loop;
	}
	trecno = 0;
	write(to, tblock[0], sizeof(tblock));
	asize += sizeof(tblock)/density;
	asize += 7;
	if(asize > tsize) {
		close(to);
		printf("change tapes\n");
		read(0, place, sizeof(place));
		otape();
	}
}

otape()
{
	to = creat(tape, 0666);
	if(to < 0) {
		printf("dump: cannot create %s\n", tape);
		exit(1);
	}
	asize = 0;
	ntape++;
	spcl.c_volume++;
	spcl.c_type = TS_TAPE;
	spclrec();
}

char *
prdate(d)
time_t d;
{
	char *p;

	if(d == 0)
		return("the epoch");
	p = ctime(&d);
	p[24] = 0;
	return(p);
}

getitime()
{
	register i, df;
	struct idates idbuf;
	char *fname;

	fname = disk;
l1:
	for(i=0; fname[i]; i++)
		if(fname[i] == '/') {
			fname += i+1;
			goto l1;
		}

	spcl.c_ddate = 0;
	df = open(increm, 0);
	if(df < 0) {
		printf("cannot open %s\n", increm);
		exit(1);
	}

l2:
	i = read(df, (char *)&idbuf, sizeof(idbuf));
	if(i != sizeof(idbuf)) {
		close(df);
		return;
	}
	for(i=0;; i++) {
		if(fname[i] != idbuf.id_name[i])
			goto l2;
		if(fname[i] == '\0')
			break;
	}
	if(idbuf.id_incno >= incno)
		goto l2;
	if(idbuf.id_ddate <= spcl.c_ddate)
		goto l2;
	spcl.c_ddate = idbuf.id_ddate;
	goto l2;
}

putitime()
{
	register i, n, df;
	struct idates idbuf;
	char *fname;

	if(uflag == 0)
		return;
	fname = disk;
l1:
	for(i=0; fname[i]; i++)
		if(fname[i] == '/') {
			fname += i+1;
			goto l1;
		}

	spcl.c_ddate = 0;
	df = open(increm, 2);
	if(df < 0) {
		printf("cannot open %s\n", increm);
		exit(1);
	}
	n = 0;
l2:
	i = read(df, (char *)&idbuf, sizeof(idbuf));
	if(i != sizeof(idbuf))
		goto l3;
	n++;
	for(i=0;; i++) {
		if(fname[i] != idbuf.id_name[i])
			goto l2;
		if(fname[i] == '\0')
			break;
	}
	if(idbuf.id_incno != incno)
		goto l2;
l3:
	lseek(df, (long)n*sizeof(idbuf), 0);
	for(i=0;; i++) {
		idbuf.id_name[i] = fname[i];
		if(fname[i] == '\0')
			break;
	}
	idbuf.id_incno = incno;
	idbuf.id_ddate = spcl.c_date;
	write(df, (char *)&idbuf, sizeof(idbuf));
	close(df);
	printf("level %c dump on %s\n", incno, prdate(spcl.c_date));
}

est(ip)
struct dinode *ip;
{
	long s;

	esize++;
	s = (ip->di_size + BSIZE-1) / BSIZE;
	esize += s;
	if(s > NADDR-3) {
		s -= NADDR-3;
		s = (s + (BSIZE/sizeof(daddr_t))-1) / (BSIZE/sizeof(daddr_t));
		esize += s;
	}
}

bmapest(map)
short *map;
{
	register i, n;

	n = -1;
	for(i=0; i<MSIZ; i++)
		if(map[i])
			n = i;
	if(n < 0)
		return;
	esize++;
	esize += (n + (BSIZE/sizeof(short))-1) / (BSIZE/sizeof(short));
}
