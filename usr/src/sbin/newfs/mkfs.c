static	char *sccsid = "@(#)mkfs.c	1.8 (Berkeley) %G%";

/*
 * make file system for cylinder-group style file systems
 *
 * usage: mkfs fs proto
 * or: mkfs size [ nsect ntrak cpg ]
 */

#define	NDIRECT	(BSIZE/sizeof(struct direct))
#define	MAXFN	500

#ifndef STANDALONE
#include <stdio.h>
#include <a.out.h>
#endif

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/dir.h"

time_t	utime;

#ifndef STANDALONE
FILE 	*fin;
#else
int	fin;
#endif

int	fsi;
int	fso;
char	*charp;
char	buf[BSIZE];
#ifndef STANDALONE
struct exec head;
#endif
char	string[50];

union {
	struct fs fs;
	char pad[BSIZE];
} fsun;
#define	sblock	fsun.fs
struct	csum *fscs;

union {
	struct cg cg;
	char pad[BSIZE];
} cgun;
#define	acg	cgun.cg

#define	howmany(x, y)	(((x)+((y)-1))/(y))
#define	roundup(x, y)	((((x)+((y)-1))/(y))*(y))

char	*fsys;
char	*proto;
int	error;
ino_t	ino = ROOTINO - 1;
long	getnum();
daddr_t	alloc();

struct	dinode zino[MAXIPG];

main(argc, argv)
char *argv[];
{
	int f, c;
	long i,n;

	argc--, argv++;
#ifndef STANDALONE
	time(&utime);
	if(argc < 2) {
		printf("usage: mkfs sblock proto/size [ nsect ntrak cpg ]\n");
		exit(1);
	}
	fsys = argv[0];
	proto = argv[1];
#else
	{
		static char protos[60];

		printf("file sys size: ");
		gets(protos);
		proto = protos;
	}
#endif
#ifdef STANDALONE
	{
		char fsbuf[100];

		do {
			printf("file system: ");
			gets(fsbuf);
			fso = open(fsbuf, 1);
			fsi = open(fsbuf, 0);
		} while (fso < 0 || fsi < 0);
	}
	fin = NULL;
	argc = 0;
#else
	fso = creat(fsys, 0666);
	if(fso < 0) {
		printf("%s: cannot create\n", fsys);
		exit(1);
	}
	fsi = open(fsys, 0);
	if(fsi < 0) {
		printf("%s: cannot open\n", fsys);
		exit(1);
	}
	fin = fopen(proto, "r");
#endif
#ifndef STANDALONE
	if (fin != NULL) {
		getstr();
		f = open(string, 0);
		if (f < 0) {
			printf("%s: cannot open init\n", string);
			goto noinit;
		}
		read(f, (char *)&head, sizeof head);
		c = head.a_text + head.a_data;
		if (c > BSIZE)
			printf("%s: too big\n", string);
		else {
			read(f, buf, c);
			wtfs(BBLOCK, BSIZE, buf);
		}
		close(f);
noinit:
		n = sblock.fs_size = getnum();
		sblock.fs_ntrak = getnum();
		sblock.fs_nsect = getnum();
		sblock.fs_cpg = getnum();
	} else
#endif
	{
		charp = "d--777 0 0 $ ";
		n = 0;
		for (f=0; c=proto[f]; f++) {
			if (c<'0' || c>'9') {
				printf("%s: cannot open\n", proto);
				exit(1);
			}
			n = n*10 + (c-'0');
		}
		sblock.fs_size = n;
		if (argc > 2)
			sblock.fs_nsect = atoi(argv[2]);
		else
			sblock.fs_nsect = 32;
		if (argc > 3)
			sblock.fs_ntrak = atoi(argv[3]);
		else
			sblock.fs_ntrak = 19;
	}
	/*
	 * Now have size for file system and nsect and ntrak.
	 * (And, if coming from prototype, cpg).
	 * Determine number of cylinders occupied by file system.
	 */
	if (sblock.fs_ntrak <= 0)
		printf("preposterous ntrak %d\n", sblock.fs_ntrak), exit(1);
	if (sblock.fs_nsect <= 0)
		printf("preposterous nsect %d\n", sblock.fs_nsect), exit(1);
	if (sblock.fs_size <= 0)
		printf("preposterous size %d\n", sblock.fs_size), exit(1);
	if (sblock.fs_ntrak * sblock.fs_nsect > MAXBPG * NSPB) {
		printf("cylinder too large (%d sectors)\n", 
		    sblock.fs_ntrak * sblock.fs_nsect);
		printf("maximum cylinder size: %d sectors\n",
		    MAXBPG * NSPB);
		exit(1);
	}
	sblock.fs_ncyl = n * NSPF / (sblock.fs_nsect * sblock.fs_ntrak);
	if (n * NSPF > sblock.fs_ncyl * sblock.fs_nsect * sblock.fs_ntrak) {
		printf("%d sector(s) in last cylinder unused\n",
		    n * NSPF - sblock.fs_ncyl * sblock.fs_nsect * sblock.fs_ntrak);
		sblock.fs_ncyl++;
	}
	sblock.fs_magic = FS_MAGIC;
	/*
	 * Validate specified/determined cpg.
	 */
#define	CGTOOBIG(fs)	((fs).fs_nsect*(fs).fs_ntrak*(fs).fs_cpg/NSPB > MAXBPG)
	if (argc > 4 || fin) {
		if (fin == NULL)
			sblock.fs_cpg = atoi(argv[4]);
		if (CGTOOBIG(sblock)) {
			printf("cylinder group too large (%d blocks); ",
			    sblock.fs_cpg * sblock.fs_nsect * sblock.fs_ntrak / NSPB);
			printf("max: %d blocks\n", MAXBPG);
			exit(1);
		}
		if (sblock.fs_cpg > MAXCPG) {
			printf("cylinder groups are limited to %d cylinders\n",
			    MAXCPG);
			exit(1);
		}
	} else {
		sblock.fs_cpg = DESCPG;
		while (CGTOOBIG(sblock))
			--sblock.fs_cpg;
	}
	/*
	 * Compute/validate number of cylinder groups.
	 */
	sblock.fs_ncg = sblock.fs_ncyl / sblock.fs_cpg;
	if (sblock.fs_ncyl % sblock.fs_cpg)
		sblock.fs_ncg++;
	if ((sblock.fs_nsect*sblock.fs_ntrak*sblock.fs_cpg) % NSPF) {
		printf("mkfs: nsect %d, ntrak %d, cpg %d is not tolerable\n",
		    sblock.fs_nsect, sblock.fs_ntrak, sblock.fs_cpg);
		printf("as this would would have cyl groups whose size\n");
		printf("is not a multiple of %d; choke!\n", FSIZE);
		exit(1);
	}
	fscs = (struct csum *)
	    calloc(1, roundup(sblock.fs_ncg * sizeof (struct csum), BSIZE));
	/*
	 * Compute number of inode blocks per cylinder group.
	 * Start with one inode per NBPI bytes; adjust as necessary.
	 */
	n = ((n * BSIZE) / NBPI) / INOPB;
	if (n <= 0)
		n = 1;
	if (n > 65500/INOPB)
		n = 65500/INOPB;
	sblock.fs_ipg = ((n / sblock.fs_ncg) + 1) * INOPB;
	if (sblock.fs_ipg < INOPB)
		sblock.fs_ipg = INOPB;
	if (sblock.fs_ipg > MAXIPG)
		sblock.fs_ipg = MAXIPG;
	while (sblock.fs_ipg * sblock.fs_ncyl > 65500)
		sblock.fs_ipg -= INOPB;
	sblock.fs_spc = sblock.fs_ntrak * sblock.fs_nsect;
	sblock.fs_fpg = (sblock.fs_cpg * sblock.fs_spc) / (FSIZE / 512);
	if (cgdmin(0,&sblock) >= sblock.fs_fpg)
		printf("inode blocks/cyl group (%d) >= data blocks (%d)\n",
		    cgdmin(0,&sblock)/FRAG, sblock.fs_fpg/FRAG), exit(1);
	sblock.fs_nifree = sblock.fs_ipg * sblock.fs_ncg;
	sblock.fs_cgsize = cgsize(&sblock);
	sblock.fs_cssize = cssize(&sblock);
	sblock.fs_sblkno = SBLOCK;
	sblock.fs_fmod = 0;
	sblock.fs_ronly = 0;

	/*
	 * Dump out information about file system.
	 */
	printf("%s:\t%d sectors in %d cylinders of %d tracks, %d sectors\n",
	    fsys, sblock.fs_size*NSPF, sblock.fs_ncyl, sblock.fs_ntrak, sblock.fs_nsect);
	printf("\t%.1fMb in %d cyl groups (%d c/g, %.2fMb/g, %d i/g)\n",
	    (float)sblock.fs_size*FSIZE*1e-6, sblock.fs_ncg, sblock.fs_cpg,
	    (float)sblock.fs_fpg*FSIZE*1e-6, sblock.fs_ipg);
/*
	printf("%7d size (%d blocks)\n", sblock.fs_size, sblock.fs_size/FRAG);
	printf("%7d cylinder groups\n", sblock.fs_ncg);
	printf("%7d cylinder group block size\n", sblock.fs_cgsize);
	printf("%7d tracks\n", sblock.fs_ntrak);
	printf("%7d sectors\n", sblock.fs_nsect);
	printf("%7d sectors per cylinder\n", sblock.fs_spc);
	printf("%7d cylinders\n", sblock.fs_ncyl);
	printf("%7d cylinders per group\n", sblock.fs_cpg);
	printf("%7d blocks per group\n", sblock.fs_fpg/FRAG);
	printf("%7d inodes per group\n", sblock.fs_ipg);
	if (sblock.fs_ncyl % sblock.fs_cpg) {
		printf("%7d cylinders in last group\n",
		    i = sblock.fs_ncyl % sblock.fs_cpg);
		printf("%7d blocks in last group\n",
		    i * sblock.fs_spc / NSPB);
	}
*/
	/*
	 * Now build the cylinders group blocks and
	 * then print out indices of cylinder groups forwarded
	 * past bad blocks or other obstructions.
	 */
	sblock.fs_nffree = 0;
	sblock.fs_nbfree = 0;
	sblock.fs_cgrotor = 0;
	for (i = 0; i < NRPOS; i++)
		sblock.fs_postbl[i] = -1;
	for (i = 0; i < sblock.fs_spc; i += (NSPF * FRAG))
		/* void */;
	for (i -= (NSPF * FRAG); i >= 0; i -= (NSPF * FRAG)) {
		c = i % sblock.fs_nsect * NRPOS / sblock.fs_nsect;
		sblock.fs_rotbl[i / (NSPF * FRAG)] = sblock.fs_postbl[c];
		sblock.fs_postbl[c] = i / (NSPF * FRAG);
	}
	for (c = 0; c < sblock.fs_ncg; c++)
		initcg(c);
	printf("\tsuper-block backups (for fsck -b#) at %d+k*%d (%d .. %d)\n",
	    SBLOCK, sblock.fs_fpg, SBLOCK+sblock.fs_fpg,
	    SBLOCK+(sblock.fs_ncg-1)*sblock.fs_fpg);
	/*
	 * Now construct the initial file system, and
	 * then write out the super-block.
	 */
	cfile((struct inode *)0);
	sblock.fs_time = utime;
	wtfs(SBLOCK, BSIZE, (char *)&sblock);
	for (i = 0; i < cssize(&sblock); i += BSIZE)
		wtfs(csaddr(&sblock) + i/BSIZE, BSIZE, ((char *)fscs)+i);
	for (c = 0; c < sblock.fs_ncg; c++)
		wtfs(cgsblock(c, &sblock), BSIZE, (char *)&sblock);
#ifndef STANDALONE
	exit(error);
#endif
}

/*
 * Initialize a cylinder group.
 */
initcg(c)
	int c;
{
	daddr_t cbase, d, dmin, dmax;
	long i, j, s;
	register struct csum *cs;

	/*
	 * Determine block bounds for cylinder group.
	 * Allow space for super block summary information in first
	 * cylinder group.
	 */
	cbase = cgbase(c,&sblock);
	dmax = cbase + sblock.fs_fpg;
	if (dmax > sblock.fs_size)
		dmax = sblock.fs_size;
	dmin = cgdmin(c,&sblock) - cbase;
	d = cbase;
	cs = fscs+c;
	cs->cs_ndir = 0;
	acg.cg_time = utime;
	acg.cg_magic = CG_MAGIC;
	acg.cg_cgx = c;
	acg.cg_ncyl = sblock.fs_cpg;
	acg.cg_niblk = sblock.fs_ipg;
	acg.cg_ndblk = dmax - cbase;
	acg.cg_ndir = 0;
	acg.cg_nffree = 0;
	acg.cg_nbfree = 0;
	acg.cg_nifree = 0;
	acg.cg_rotor = dmin;
	acg.cg_frotor = dmin;
	acg.cg_irotor = 0;
	for (i = 0; i < FRAG; i++) {
		acg.cg_frsum[i] = 0;
	}
	for (i = 0; i < sblock.fs_ipg; ) {
		for (j = INOPB; j > 0; j--) {
			clrbit(acg.cg_iused, i);
			i++;
		}
		acg.cg_nifree += INOPB;
	}
	while (i < MAXIPG) {
		clrbit(acg.cg_iused, i);
		i++;
	}
	lseek(fso, cgimin(c,&sblock)*FSIZE, 0);
	if (write(fso, (char *)zino, sblock.fs_ipg * sizeof (struct dinode)) !=
	    sblock.fs_ipg * sizeof (struct dinode))
		printf("write error %D\n", tell(fso) / BSIZE);
	for (i = 0; i < MAXCPG; i++)
		for (j = 0; j < NRPOS; j++)
			acg.cg_b[i][j] = 0;
	if (c == 0) {
		dmin += howmany(cssize(&sblock), BSIZE) * FRAG;
	}
	for (d = 0; d < dmin; d += FRAG)
		clrblock(acg.cg_free, d/FRAG);
	while ((d+FRAG) <= dmax - cbase) {
		setblock(acg.cg_free, d/FRAG);
		acg.cg_nbfree++;
		s = d * NSPF;
		acg.cg_b[s/sblock.fs_spc]
		    [s%sblock.fs_nsect*NRPOS/sblock.fs_nsect]++;
		d += FRAG;
	}
	if (d < dmax - cbase)
		for (; d < dmax - cbase; d++) {
			setbit(acg.cg_free, d);
			acg.cg_nffree++;
		}
	for (; d < MAXBPG; d++)
		clrbit(acg.cg_free, d);
	sblock.fs_nffree += acg.cg_nffree;
	sblock.fs_nbfree += acg.cg_nbfree;
	cs->cs_nifree = acg.cg_nifree;
	cs->cs_nbfree = acg.cg_nbfree;
	wtfs(cgtod(c, &sblock), BSIZE, (char *)&acg);
}

cfile(par)
struct inode *par;
{
	struct inode in;
	int dbc, ibc;
	char db[BSIZE];
	daddr_t ib[NINDIR];
	int i, f, c;

	/*
	 * get mode, uid and gid
	 */

	getstr();
	in.i_mode = gmode(string[0], "-bcd", IFREG, IFBLK, IFCHR, IFDIR);
	in.i_mode |= gmode(string[1], "-u", 0, ISUID, 0, 0);
	in.i_mode |= gmode(string[2], "-g", 0, ISGID, 0, 0);
	for(i=3; i<6; i++) {
		c = string[i];
		if(c<'0' || c>'7') {
			printf("%c/%s: bad octal mode digit\n", c, string);
			error = 1;
			c = 0;
		}
		in.i_mode |= (c-'0')<<(15-3*i);
	}
	in.i_uid = getnum();
	in.i_gid = getnum();
	in.i_atime = utime;
	in.i_mtime = utime;
	in.i_ctime = utime;

	/*
	 * general initialization prior to
	 * switching on format
	 */

	ino++;
	in.i_number = ino;
	for(i=0; i<BSIZE; i++)
		db[i] = 0;
	for(i=0; i<NINDIR; i++)
		ib[i] = (daddr_t)0;
	in.i_nlink = 1;
	in.i_size = 0;
	for(i=0; i<NDADDR; i++)
		in.i_db[i] = (daddr_t)0;
	for(i=0; i<NIADDR; i++)
		in.i_ib[i] = (daddr_t)0;
	if(par == (struct inode *)0) {
		par = &in;
		in.i_nlink--;
	}
	dbc = 0;
	ibc = 0;
	switch(in.i_mode&IFMT) {

	case IFREG:
		/*
		 * regular file
		 * contents is a file name
		 */

		getstr();
		f = open(string, 0);
		if(f < 0) {
			printf("%s: cannot open\n", string);
			error = 1;
			break;
		}
		while((i=read(f, db, BSIZE)) > 0) {
			in.i_size += i;
			newblk(&dbc, db, &ibc, ib, ibc < NDADDR ? i : BSIZE, 0);
		}
		close(f);
		break;

	case IFBLK:
	case IFCHR:
		/*
		 * special file
		 * content is maj/min types
		 */

		i = getnum() & 0377;
		f = getnum() & 0377;
		in.i_rdev = makedev(i, f);
		break;

	case IFDIR:
		/*
		 * directory
		 * put in extra links
		 * call recursively until
		 * name of "$" found
		 */

		par->i_nlink++;
		in.i_nlink++;
		entry(in.i_number, ".", &dbc, db, &ibc, ib);
		entry(par->i_number, "..", &dbc, db, &ibc, ib);
		in.i_size = 2*sizeof(struct direct);
		for(;;) {
			getstr();
			if(string[0]=='$' && string[1]=='\0')
				break;
			if (in.i_size >= BSIZE * NDADDR) {
				printf("can't handle direct of > %d entries\n",
				    NDIRECT * NDADDR);
				exit(1);
			}
			entry(ino+1, string, &dbc, db, &ibc, ib);
			in.i_size += sizeof(struct direct);
			cfile(&in);
		}
		newblk(&dbc, db, &ibc, ib, roundup(dbc, FSIZE), IFDIR);
		break;
	}
	iput(&in, &ibc, ib);
}

gmode(c, s, m0, m1, m2, m3)
char c, *s;
{
	int i;

	for(i=0; s[i]; i++)
		if(c == s[i])
			return((&m0)[i]);
	printf("%c/%s: bad mode\n", c, string);
	error = 1;
	return(0);
}

long
getnum()
{
	int i, c;
	long n;

	getstr();
	n = 0;
	i = 0;
	for(i=0; c=string[i]; i++) {
		if(c<'0' || c>'9') {
			printf("%s: bad number\n", string);
			error = 1;
			return((long)0);
		}
		n = n*10 + (c-'0');
	}
	return(n);
}

getstr()
{
	int i, c;

loop:
	switch(c=getch()) {

	case ' ':
	case '\t':
	case '\n':
		goto loop;

	case '\0':
		printf("EOF\n");
		exit(1);

	case ':':
		while(getch() != '\n');
		goto loop;

	}
	i = 0;

	do {
		string[i++] = c;
		c = getch();
	} while(c!=' '&&c!='\t'&&c!='\n'&&c!='\0');
	string[i] = '\0';
}

rdfs(bno, size, bf)
daddr_t bno;
int size;
char *bf;
{
	int n;

	lseek(fsi, bno*FSIZE, 0);
	n = read(fsi, bf, size);
	if(n != size) {
		printf("read error: %ld\n", bno);
		exit(1);
	}
}

wtfs(bno, size, bf)
daddr_t bno;
int size;
char *bf;
{
	int n;

	lseek(fso, bno*FSIZE, 0);
	n = write(fso, bf, size);
	if(n != size) {
		printf("write error: %D\n", bno);
		exit(1);
	}
}

daddr_t
alloc(size, mode)
	int size;
	int mode;
{
	int c, i, s, frag;
	daddr_t d;

	c = 0;
	rdfs(cgtod(0,&sblock), sblock.fs_cgsize, (char *)&acg);
	if (acg.cg_nbfree == 0) {
		printf("first cylinder group ran out of space\n");
		return (0);
	}
	for (d = 0; d < acg.cg_ndblk; d += FRAG)
		if (isblock(acg.cg_free, d/FRAG))
			goto goth;
	printf("internal error: can't find block in cyl 0\n");
	return (0);
goth:
	clrblock(acg.cg_free, d/FRAG);
	acg.cg_nbfree--;
	sblock.fs_nbfree--;
	fscs[0].cs_nbfree--;
	if (mode & IFDIR) {
		acg.cg_ndir++;
		fscs[0].cs_ndir++;
	}
	s = d * NSPF;
	acg.cg_b[s/sblock.fs_spc][s%sblock.fs_nsect*NRPOS/sblock.fs_nsect]--;
	if (size != BSIZE) {
		frag = howmany(size, FSIZE);
		acg.cg_nffree += FRAG - frag;
		acg.cg_frsum[FRAG - frag]++;
		sblock.fs_nffree += FRAG - frag;
		for (i = frag; i < FRAG; i++)
			setbit(acg.cg_free, d+i);
	}
	wtfs(cgtod(0,&sblock), sblock.fs_cgsize, (char *)&acg);
	return (d);
}

entry(inum, str, adbc, db, aibc, ib)
ino_t inum;
char *str;
int *adbc, *aibc;
char *db;
daddr_t *ib;
{
	struct direct *dp;
	int i;

	if (*adbc == NDIRECT)
		newblk(adbc, db, aibc, ib, BSIZE, 0);
	dp = (struct direct *)db;
	dp += *adbc;
	(*adbc)++;
	dp->d_ino = inum;
	for(i=0; i<DIRSIZ; i++)
		dp->d_name[i] = 0;
	for(i=0; i<DIRSIZ; i++)
		if((dp->d_name[i] = str[i]) == 0)
			break;
}

newblk(adbc, db, aibc, ib, size, mode)
	int *adbc, *aibc;
	char *db;
	daddr_t *ib;
	int size;
	int mode;
{
	int i;
	daddr_t bno;

	bno = alloc(size, mode);
	wtfs(bno, size, db);
	for(i=0; i<size; i++)
		db[i] = 0;
	*adbc = 0;
	ib[*aibc] = bno;
	(*aibc)++;
	if(*aibc >= NINDIR) {
		printf("indirect block full\n");
		error = 1;
		*aibc = 0;
	}
}

getch()
{

#ifndef STANDALONE
	if(charp)
#endif
		return(*charp++);
#ifndef STANDALONE
	return(getc(fin));
#endif
}

iput(ip, aibc, ib)
struct inode *ip;
int *aibc;
daddr_t *ib;
{
	struct dinode *dp;
	daddr_t d;
	int i, c = ip->i_number / sblock.fs_ipg;

	rdfs(cgtod(c,&sblock), sblock.fs_cgsize, (char *)&acg);
	acg.cg_nifree--;
	setbit(acg.cg_iused, ip->i_number);
	wtfs(cgtod(c,&sblock), sblock.fs_cgsize, (char *)&acg);
	sblock.fs_nifree--;
	fscs[0].cs_nifree--;
	if(ip->i_number >= sblock.fs_ipg) {
		printf("mkfs: cant handle more than one cg of inodes (yet)\n");
		exit(1);
	}
	if(ip->i_number >= sblock.fs_ipg * sblock.fs_ncg) {
		if(error == 0)
			printf("ilist too small\n");
		error = 1;
		return;
	}
	d = itod(ip->i_number,&sblock);
	rdfs(d, BSIZE, buf);
	for(i=0; i<*aibc; i++) {
		if(i >= NDADDR)
			break;
		ip->i_db[i] = ib[i];
	}
	if(*aibc >= NDADDR) {
		ip->i_ib[0] = alloc(BSIZE, 0);
		for(i=0; i<NINDIR-NDADDR; i++) {
			ib[i] = ib[i+NDADDR];
			ib[i+NDADDR] = (daddr_t)0;
		}
		wtfs(ip->i_ib[0], (char *)ib);
	}
	((struct dinode *)buf+itoo(ip->i_number))->di_ic = ip->i_ic;
	wtfs(d, BSIZE, buf);
}
