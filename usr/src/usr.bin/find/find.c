#ifndef	lint
static char *sccsid = "@(#)find.c	4.23 (Berkeley) %G%";
#endif

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <stdio.h>
#include "pathnames.h"

#define A_DAY	86400L /* a day full of seconds */
#define EQ(x, y)	(strcmp(x, y)==0)

int	Randlast;
char	Pathname[MAXPATHLEN+1];

#define MAXNODES	100

struct anode {
	int (*F)();
	struct anode *L, *R;
} Node[MAXNODES];
int Nn;  /* number of nodes */
char	*Fname;
long	Now;
int	Argc,
	Ai,
	Pi;
char	**Argv;
/* cpio stuff */
int	Cpio;
short	*Buf, *Dbuf, *Wp;
int	Bufsize = 5120;
int	Wct = 2560;

long	Newer;

int	Xdev = 1;	/* true if SHOULD cross devices (file systems) */
int	follow;		/* true if SHOULD descend symlinked directories */
struct	stat Devstat;	/* stats of each argument path's file system */

struct stat Statb;

struct	anode	*exp(),
		*e1(),
		*e2(),
		*e3(),
		*mk();
char	*nxtarg();
char	Home[MAXPATHLEN + 1];
long	Blocks;
char *rindex();
char *sbrk();

/*
 * SEE ALSO:	code.c, updatedb, bigram.c
 *		Usenix ;login:, Vol 8, No 1, February/March, 1983, p. 8.
 *
 * REVISIONS: 	James A. Woods, Informatics General Corporation,
 *		NASA Ames Research Center, 6/81.
 *
 *		The second form searches a pre-computed filelist
 *		(constructed nightly by /usr/lib/crontab) which is
 *		compressed by updatedb (v.i.z.)  The effect of
 *			find <name>
 *		is similar to
 *			find / +0 -name "*<name>*" -print
 *		but much faster.
 *
 *		8/82 faster yet + incorporation of bigram coding -- jaw
 *
 *		1/83 incorporate glob-style matching -- jaw
 */
#define	AMES	1

main(argc, argv)
	int argc;
	char *argv[];
{
	struct anode *exlist;
	int paths;
	register char *cp, *sp = 0;
#ifdef	SUID_PWD
	FILE *pwd, *popen();
#endif

#ifdef  AMES
	if (argc < 2) {
		fprintf(stderr,
			"Usage: find name, or find path-list predicate-list\n");
		exit(1);
	}
	if (argc == 2) {
		fastfind(argv[1]);
		exit(0);
	}
#endif
	time(&Now);
	setpassent(1);
	setgroupent(1);
#ifdef	SUID_PWD
	pwd = popen("pwd", "r");
	fgets(Home, sizeof Home, pwd);
	pclose(pwd);
	Home[strlen(Home) - 1] = '\0';
#else
	if (getwd(Home) == NULL) {
		fprintf(stderr, "find: Can't get current working directory\n");
		exit(1);
	}
#endif
	Argc = argc; Argv = argv;
	if(argc<3) {
usage:		fprintf(stderr, "Usage: find path-list predicate-list\n");
		exit(1);
	}
	for(Ai = paths = 1; Ai < (argc-1); ++Ai, ++paths)
		if(*Argv[Ai] == '-' || EQ(Argv[Ai], "(") || EQ(Argv[Ai], "!"))
			break;
	if(paths == 1) /* no path-list */
		goto usage;
	if(!(exlist = exp())) { /* parse and compile the arguments */
		fprintf(stderr, "find: parsing error\n");
		exit(1);
	}
	if(Ai<argc) {
		fprintf(stderr, "find: missing conjunction\n");
		exit(1);
	}
	for(Pi = 1; Pi < paths; ++Pi) {
		sp = 0;
		chdir(Home);
		strcpy(Pathname, Argv[Pi]);
		if(cp = rindex(Pathname, '/')) {
			sp = cp + 1;
			*cp = '\0';
			if(chdir(*Pathname? Pathname: "/") == -1) {
				fprintf(stderr, "find: bad starting directory\n");
				exit(2);
			}
			*cp = '/';
		}
		Fname = sp? sp: Pathname;
		if (!Xdev)
			stat(Pathname, &Devstat);
		descend(Pathname, Fname, exlist); /* to find files that match  */
	}
	if(Cpio) {
		strcpy(Pathname, "TRAILER!!!");
		Statb.st_size = 0;
		cpio();
		printf("%D blocks\n", Blocks*10);
	}
	exit(0);
}

/* compile time functions:  priority is  exp()<e1()<e2()<e3()  */

struct anode *exp() { /* parse ALTERNATION (-o)  */
	int or();
	register struct anode * p1;

	p1 = e1() /* get left operand */ ;
	if(EQ(nxtarg(), "-o")) {
		Randlast--;
		return(mk(or, p1, exp()));
	}
	else if(Ai <= Argc) --Ai;
	return(p1);
}
struct anode *e1() { /* parse CONCATENATION (formerly -a) */
	int and();
	register struct anode * p1;
	register char *a;

	p1 = e2();
	a = nxtarg();
	if(EQ(a, "-a")) {
And:
		Randlast--;
		return(mk(and, p1, e1()));
	} else if(EQ(a, "(") || EQ(a, "!") || (*a=='-' && !EQ(a, "-o"))) {
		--Ai;
		goto And;
	} else if(Ai <= Argc) --Ai;
	return(p1);
}
struct anode *e2() { /* parse NOT (!) */
	int not();

	if(Randlast) {
		fprintf(stderr, "find: operand follows operand\n");
		exit(1);
	}
	Randlast++;
	if(EQ(nxtarg(), "!"))
		return(mk(not, e3(), (struct anode *)0));
	else if(Ai <= Argc) --Ai;
	return(e3());
}
struct anode *e3() { /* parse parens and predicates */
	int exeq(), ok(), glob(),  mtime(), atime(), user(),
		group(), size(), perm(), links(), print(),
		type(), ino(), cpio(), newer(), 
		nouser(), nogroup(), ls(), dummy();
	struct anode *p1;
	int i;
	register char *a, *b;
	register int s;

	a = nxtarg();
	if(EQ(a, "(")) {
		Randlast--;
		p1 = exp();
		a = nxtarg();
		if(!EQ(a, ")")) goto err;
		return(p1);
	}
	else if(EQ(a, "-print")) {
		return(mk(print, (struct anode *)0, (struct anode *)0));
	}
	else if (EQ(a, "-nouser")) {
		return (mk(nouser, (struct anode *)0, (struct anode *)0));
	}
	else if (EQ(a, "-nogroup")) {
		return (mk(nogroup, (struct anode *)0, (struct anode *)0));
	}
	else if (EQ(a, "-ls")) {
		return (mk(ls, (struct anode *)0, (struct anode *)0));
	}
	else if (EQ(a, "-xdev")) {
		Xdev = 0;
		return (mk(dummy, (struct anode *)0, (struct anode *)0));
	}
	else if (EQ(a, "-follow")) {
		follow=1;
		return mk(dummy, (struct anode *)0, (struct anode *)0);
	}
	b = nxtarg();
	s = *b;
	if(s=='+') b++;
	if(EQ(a, "-name"))
		return(mk(glob, (struct anode *)b, (struct anode *)0));
	else if(EQ(a, "-mtime"))
		return(mk(mtime, (struct anode *)atoi(b), (struct anode *)s));
	else if(EQ(a, "-atime"))
		return(mk(atime, (struct anode *)atoi(b), (struct anode *)s));
	else if(EQ(a, "-user")) {
		if((i=getuid(b)) == -1) {
			if(gmatch(b, "[0-9]*"))
				return mk(user, (struct anode *)atoi(b), (struct anode *)s);
			fprintf(stderr, "find: cannot find -user name\n");
			exit(1);
		}
		return(mk(user, (struct anode *)i, (struct anode *)s));
	}
	else if(EQ(a, "-inum"))
		return(mk(ino, (struct anode *)atoi(b), (struct anode *)s));
	else if(EQ(a, "-group")) {
		if((i=getgid(b)) == -1) {
			if(gmatch(b, "[0-9]*"))
				return mk(group, (struct anode *)atoi(b), (struct anode *)s);
			fprintf(stderr, "find: cannot find -group name\n");
			exit(1);
		}
		return(mk(group, (struct anode *)i, (struct anode *)s));
	} else if(EQ(a, "-size"))
		return(mk(size, (struct anode *)atoi(b), (struct anode *)s));
	else if(EQ(a, "-links"))
		return(mk(links, (struct anode *)atoi(b), (struct anode *)s));
	else if(EQ(a, "-perm")) {
		for(i=0; *b ; ++b) {
			if(*b=='-') continue;
			i <<= 3;
			i = i + (*b - '0');
		}
		return(mk(perm, (struct anode *)i, (struct anode *)s));
	}
	else if(EQ(a, "-type")) {
		i = s=='d' ? S_IFDIR :
		    s=='b' ? S_IFBLK :
		    s=='c' ? S_IFCHR :
		    s=='f' ? S_IFREG :
		    s=='l' ? S_IFLNK :
		    s=='s' ? S_IFSOCK :
		    0;
		return(mk(type, (struct anode *)i, (struct anode *)0));
	}
	else if (EQ(a, "-exec")) {
		i = Ai - 1;
		while(!EQ(nxtarg(), ";"));
		return(mk(exeq, (struct anode *)i, (struct anode *)0));
	}
	else if (EQ(a, "-ok")) {
		i = Ai - 1;
		while(!EQ(nxtarg(), ";"));
		return(mk(ok, (struct anode *)i, (struct anode *)0));
	}
	else if(EQ(a, "-cpio")) {
		if((Cpio = creat(b, 0666)) < 0) {
			fprintf(stderr, "find: cannot create < %s >\n", s);
			exit(1);
		}
		Buf = (short *)sbrk(512);
		Wp = Dbuf = (short *)sbrk(5120);
		return(mk(cpio, (struct anode *)0, (struct anode *)0));
	}
	else if(EQ(a, "-newer")) {
		if(stat(b, &Statb) < 0) {
			fprintf(stderr, "find: cannot access < %s >\n", b);
			exit(1);
		}
		Newer = Statb.st_mtime;
		return mk(newer, (struct anode *)0, (struct anode *)0);
	}
err:	fprintf(stderr, "find: bad option < %s >\n", a);
	exit(1);
}
struct anode *mk(f, l, r)
int (*f)();
struct anode *l, *r;
{
	if (Nn >= MAXNODES) {
		fprintf(stderr, "find: Too many options\n");
		exit(1);
	}

	Node[Nn].F = f;
	Node[Nn].L = l;
	Node[Nn].R = r;
	return(&(Node[Nn++]));
}

char *nxtarg() { /* get next arg from command line */
	static strikes = 0;

	if(strikes==3) {
		fprintf(stderr, "find: incomplete statement\n");
		exit(1);
	}
	if(Ai>=Argc) {
		strikes++;
		Ai = Argc + 1;
		return("");
	}
	return(Argv[Ai++]);
}

/* execution time functions */
and(p)
register struct anode *p;
{
	return(((*p->L->F)(p->L)) && ((*p->R->F)(p->R))?1:0);
}
or(p)
register struct anode *p;
{
	 return(((*p->L->F)(p->L)) || ((*p->R->F)(p->R))?1:0);
}
not(p)
register struct anode *p;
{
	return( !((*p->L->F)(p->L)));
}
glob(p)
register struct { int f; char *pat; } *p; 
{
	return(gmatch(Fname, p->pat));
}
print(p)
struct anode *p;
{
	puts(Pathname);
	return(1);
}
mtime(p)
register struct { int f, t, s; } *p; 
{
	return(scomp((int)((Now - Statb.st_mtime) / A_DAY), p->t, p->s));
}
atime(p)
register struct { int f, t, s; } *p; 
{
	return(scomp((int)((Now - Statb.st_atime) / A_DAY), p->t, p->s));
}
user(p)
register struct { int f, u, s; } *p; 
{
	return(scomp(Statb.st_uid, p->u, p->s));
}
nouser(p)
struct anode *p;
{
	char *getname();

	return (getname(Statb.st_uid) == NULL);
}
ino(p)
register struct { int f, u, s; } *p;
{
	return(scomp((int)Statb.st_ino, p->u, p->s));
}
group(p)
register struct { int f, u; } *p; 
{
	return(p->u == Statb.st_gid);
}
nogroup(p)
struct anode *p;
{
	char *getgroup();

	return (getgroup(Statb.st_gid) == NULL);
}
links(p)
register struct { int f, link, s; } *p; 
{
	return(scomp(Statb.st_nlink, p->link, p->s));
}
size(p)
register struct { int f, sz, s; } *p; 
{
	return(scomp((int)((Statb.st_size+511)>>9), p->sz, p->s));
}
perm(p)
register struct { int f, per, s; } *p; 
{
	register i;
	i = (p->s=='-') ? p->per : 07777; /* '-' means only arg bits */
	return((Statb.st_mode & i & 07777) == p->per);
}
type(p)
register struct { int f, per, s; } *p;
{
	return((Statb.st_mode&S_IFMT)==p->per);
}
exeq(p)
register struct { int f, com; } *p;
{
	fflush(stdout); /* to flush possible `-print' */
	return(doex(p->com));
}
ok(p)
struct { int f, com; } *p;
{
	char c;  int yes;
	yes = 0;
	fflush(stdout); /* to flush possible `-print' */
	fprintf(stderr, "< %s ... %s > ?   ", Argv[p->com], Pathname);
	fflush(stderr);
	if((c=getchar())=='y') yes = 1;
	while(c!='\n') c = getchar();
	if(yes) return(doex(p->com));
	return(0);
}

#define MKSHORT(v, lv) {U.l=1L;if(U.c[0]) U.l=lv, v[0]=U.s[1], v[1]=U.s[0]; else U.l=lv, v[0]=U.s[0], v[1]=U.s[1];}
union { long l; short s[2]; char c[4]; } U;
long mklong(v)
short v[];
{
	U.l = 1;
	if(U.c[0] /* VAX */)
		U.s[0] = v[1], U.s[1] = v[0];
	else
		U.s[0] = v[0], U.s[1] = v[1];
	return U.l;
}
cpio(p)
struct anode *p;
{
#define MAGIC 070707
	struct header {
		short	h_magic,
			h_dev,
			h_ino,
			h_mode,
			h_uid,
			h_gid,
			h_nlink,
			h_rdev;
		short	h_mtime[2];
		short	h_namesize;
		short	h_filesize[2];
		char	h_name[256];
	} hdr;
	register ifile, ct;
	static long fsz;
	register i;

	hdr.h_magic = MAGIC;
	strcpy(hdr.h_name, !strncmp(Pathname, "./", 2)? Pathname+2: Pathname);
	hdr.h_namesize = strlen(hdr.h_name) + 1;
	hdr.h_uid = Statb.st_uid;
	hdr.h_gid = Statb.st_gid;
	hdr.h_dev = Statb.st_dev;
	hdr.h_ino = Statb.st_ino;
	hdr.h_mode = Statb.st_mode;
	MKSHORT(hdr.h_mtime, Statb.st_mtime);
	hdr.h_nlink = Statb.st_nlink;
	fsz = hdr.h_mode & S_IFREG? Statb.st_size: 0L;
	MKSHORT(hdr.h_filesize, fsz);
	hdr.h_rdev = Statb.st_rdev;
	if(EQ(hdr.h_name, "TRAILER!!!")) {
		bwrite((short *)&hdr, (sizeof hdr-256)+hdr.h_namesize);
		for(i = 0; i < 10; ++i)
			bwrite(Buf, 512);
		return;
	}
	if(!mklong(hdr.h_filesize))
		return;
	if((ifile = open(Fname, 0)) < 0) {
cerror:
		fprintf(stderr, "find: cannot copy < %s >\n", hdr.h_name);
		return;
	}
	bwrite((short *)&hdr, (sizeof hdr-256)+hdr.h_namesize);
	for(fsz = mklong(hdr.h_filesize); fsz > 0; fsz -= 512) {
		ct = fsz>512? 512: fsz;
		if(read(ifile, (char *)Buf, ct) < 0)
			goto cerror;
		bwrite(Buf, ct);
	}
	close(ifile);
	return;
}
newer(p)
struct anode *p;
{
	return Statb.st_mtime > Newer;
}
ls(p)
struct anode *p;
{
	list(Pathname, &Statb);
	return (1);
}
dummy(p)
struct anode *p;
{
	/* dummy */
	return (1);
}

/* support functions */
scomp(a, b, s) /* funny signed compare */
register a, b;
register char s;
{
	if(s == '+')
		return(a > b);
	if(s == '-')
		return(a < (b * -1));
	return(a == b);
}

doex(com)
{
	register np;
	register char *na;
	static char *nargv[50];
	static ccode;
	register int w, pid, omask;

	ccode = np = 0;
	while (na=Argv[com++]) {
		if(np >= sizeof nargv / sizeof *nargv - 1) break;
		if(strcmp(na, ";")==0) break;
		if(strcmp(na, "{}")==0) nargv[np++] = Pathname;
		else nargv[np++] = na;
	}
	nargv[np] = 0;
	if (np==0) return(9);
	switch (pid = vfork()) {
	case -1:
		perror("find: Can't fork");
		exit(1);
		break;

	case 0:
		chdir(Home);
		execvp(nargv[0], nargv, np);
		write(2, "find: Can't execute ", 20);
		perror(nargv[0]);
		/*
		 * Kill ourselves; our exit status will be a suicide
		 * note indicating we couldn't do the "exec".
		 */
		kill(getpid(), SIGUSR1);
		break;

	default:
		omask = sigblock(sigmask(SIGINT)|sigmask(SIGQUIT));
		while ((w = wait(&ccode)) != pid && w != -1)
			;
		(void) sigsetmask(omask);
		if ((ccode & 0177) == SIGUSR1)
			exit(1);
		return (ccode != 0 ? 0 : 1);
	}
}

getunum(f, s) char *f, *s; { /* find user/group name and return number */
	register i;
	register char *sp;
	register c;
	char str[20];
	FILE *pin;

	i = -1;
	pin = fopen(f, "r");
	c = '\n'; /* prime with a CR */
	do {
		if(c=='\n') {
			sp = str;
			while((c = *sp++ = getc(pin)) != ':')
				if(c == EOF) goto RET;
			*--sp = '\0';
			if(EQ(str, s)) {
				while((c=getc(pin)) != ':')
					if(c == EOF) goto RET;
				sp = str;
				while((*sp = getc(pin)) != ':') sp++;
				*sp = '\0';
				i = atoi(str);
				goto RET;
			}
		}
	} while((c = getc(pin)) != EOF);
 RET:
	fclose(pin);
	return(i);
}

descend(name, fname, exlist)
	struct anode *exlist;
	char *name, *fname;
{
	DIR	*dir = NULL;
	register struct direct	*dp;
	register char *c1;
	int rv = 0;
	char *endofname;

	if ((follow?stat(fname, &Statb):lstat(fname, &Statb))<0) {
		fprintf(stderr, "find: bad status < %s >\n", name);
		return(0);
	}
	(*exlist->F)(exlist);
	if((Statb.st_mode&S_IFMT)!=S_IFDIR ||
	   !Xdev && Devstat.st_dev != Statb.st_dev)
		return(1);

	if (Statb.st_nlink == 2 && exlist->F == and &&
		exlist->L->F == type && ((int) (exlist->L->L)) == S_IFDIR)
			return(1);

	for (c1 = name; *c1; ++c1);
	if (*(c1-1) == '/')
		--c1;
	endofname = c1;

	if (chdir(fname) == -1)
		return(0);
	if ((dir = opendir(".")) == NULL) {
		fprintf(stderr, "find: cannot open < %s >\n", name);
		rv = 0;
		goto ret;
	}
	for (dp = readdir(dir); dp != NULL; dp = readdir(dir)) {
		if ((dp->d_name[0]=='.' && dp->d_name[1]=='\0') ||
		    (dp->d_name[0]=='.' && dp->d_name[1]=='.' && dp->d_name[2]=='\0'))
			continue;
		c1 = endofname;
		*c1++ = '/';
		strcpy(c1, dp->d_name);
		Fname = endofname+1;
		if(!descend(name, Fname, exlist)) {
			*endofname = '\0';
			chdir(Home);
			if(chdir(Pathname) == -1) {
				fprintf(stderr, "find: bad directory tree\n");
				exit(1);
			}
		}
	}
	rv = 1;
ret:
	if(dir)
		closedir(dir);
	if(chdir("..") == -1) {
		*endofname = '\0';
		fprintf(stderr, "find: bad directory <%s>\n", name);
		rv = 1;
	}
	return(rv);
}

gmatch(s, p) /* string match as in glob */
register char *s, *p;
{
	if (*s=='.' && *p!='.') return(0);
	return amatch(s, p);
}

amatch(s, p)
register char *s, *p;
{
	register cc;
	int scc, k;
	int c, lc;

	scc = *s;
	lc = 077777;
	switch (c = *p) {

	case '[':
		k = 0;
		while (cc = *++p) {
			switch (cc) {

			case ']':
				if (k)
					return(amatch(++s, ++p));
				else
					return(0);

			case '-':
				cc = p[1];
				k |= lc <= scc && scc <= cc;
			}
			if (scc==(lc=cc)) k++;
		}
		return(0);

	case '?':
	caseq:
		if(scc) return(amatch(++s, ++p));
		return(0);
	case '*':
		return(umatch(s, ++p));
	case 0:
		return(!scc);
	}
	if (c==scc) goto caseq;
	return(0);
}

umatch(s, p)
register char *s, *p;
{
	if(*p==0) return(1);
	while(*s)
		if (amatch(s++, p)) return(1);
	return(0);
}

bwrite(rp, c)
register short *rp;
register c;
{
	register short *wp = Wp;

	c = (c+1) >> 1;
	while(c--) {
		if(!Wct) {
again:
			if(write(Cpio, (char *)Dbuf, Bufsize)<0) {
				Cpio = chgreel(1, Cpio);
				goto again;
			}
			Wct = Bufsize >> 1;
			wp = Dbuf;
			++Blocks;
		}
		*wp++ = *rp++;
		--Wct;
	}
	Wp = wp;
}
chgreel(x, fl)
{
	register f;
	char str[22];
	FILE *devtty;
	struct stat statb;
	extern errno;

	fprintf(stderr, "find: errno: %d, ", errno);
	fprintf(stderr, "find: can't %s\n", x? "write output": "read input");
	fstat(fl, &statb);
	if((statb.st_mode&S_IFMT) != S_IFCHR)
		exit(1);
again:
	fprintf(stderr, "If you want to go on, type device/file name %s\n",
		"when ready");
	devtty = fopen(_PATH_TTY, "r");
	fgets(str, 20, devtty);
	str[strlen(str) - 1] = '\0';
	if(!*str)
		exit(1);
	close(fl);
	if((f = open(str, x? 1: 0)) < 0) {
		fprintf(stderr, "That didn't work");
		fclose(devtty);
		goto again;
	}
	return f;
}

#ifdef	AMES
/*
 * 'fastfind' scans a file list for the full pathname of a file
 * given only a piece of the name.  The list has been processed with
 * with "front-compression" and bigram coding.  Front compression reduces
 * space by a factor of 4-5, bigram coding by a further 20-25%.
 * The codes are:
 *
 *	0-28	likeliest differential counts + offset to make nonnegative 
 *	30	switch code for out-of-range count to follow in next word
 *	128-255 bigram codes (128 most common, as determined by 'updatedb')
 *	32-127  single character (printable) ascii residue (ie, literal)
 *
 * A novel two-tiered string search technique is employed: 
 *
 * First, a metacharacter-free subpattern and partial pathname is
 * matched BACKWARDS to avoid full expansion of the pathname list.
 * The time savings is 40-50% over forward matching, which cannot efficiently
 * handle overlapped search patterns and compressed path residue.
 *
 * Then, the actual shell glob-style regular expression (if in this form)
 * is matched against the candidate pathnames using the slower routines
 * provided in the standard 'find'.
 */

#include "find.h"

#define	YES	1
#define	NO	0

fastfind ( pathpart )	
	char pathpart[];
{
	register char *p, *s;
	register int c; 
	char *q, *index(), *patprep();
	int count = 0, found = NO, globflag;
	FILE *fp, *fopen();
	char *patend, *cutoff;
	char path[MAXPATHLEN];
	char bigram1[NBG], bigram2[NBG];

	if ( (fp = fopen ( _PATH_FCODES, "r" )) == NULL ) {
		perror( _PATH_FCODES );
		exit ( 1 );
	}
	for ( c = 0, p = bigram1, s = bigram2; c < NBG; c++ ) 
		p[c] = getc ( fp ),  s[c] = getc ( fp );

	p = pathpart;
	globflag = index ( p, '*' ) || index ( p, '?' ) || index ( p, '[' );
	patend = patprep ( p );

	for ( c = getc ( fp ); c != EOF; ) {

		count += ( (c == SWITCH) ? getw ( fp ) : c ) - OFFSET;

		for ( p = path + count; (c = getc ( fp )) > SWITCH; )	/* overlay old path */
			if ( c < PARITY )	
				*p++ = c;
			else {			/* bigrams are parity-marked */
				c &= PARITY-1;
				*p++ = bigram1[c], *p++ = bigram2[c];
			}
		*p-- = NULL;
		cutoff = ( found ? path : path + count );

		for ( found = NO, s = p; s >= cutoff; s-- ) 
			if ( *s == *patend ) {		/* fast first char check */
				for ( p = patend - 1, q = s - 1; *p != NULL; p--, q-- )
					if ( *q != *p )
						break;
				if ( *p == NULL ) {	/* success on fast match */
					found = YES;
					if ( globflag == NO || amatch ( path, pathpart ) )
						puts ( path );
					break;
				}
			}
	}
}

/*
    extract last glob-free subpattern in name for fast pre-match;
    prepend '\0' for backwards match; return end of new pattern
*/
static char globfree[100];

char *
patprep ( name )
	char *name;
{
	register char *p, *endmark;
	register char *subp = globfree;

	*subp++ = '\0';
	p = name + strlen ( name ) - 1;
	/*
	   skip trailing metacharacters (and [] ranges)
	*/
	for ( ; p >= name; p-- )
		if ( index ( "*?", *p ) == 0 )
			break;
	if ( p < name )
		p = name;
	if ( *p == ']' )
		for ( p--; p >= name; p-- )
			if ( *p == '[' ) {
				p--;
				break;
			}
	if ( p < name )
		p = name;
	/*
	   if pattern has only metacharacters,
	   check every path (force '/' search)
	*/
	if ( (p == name) && index ( "?*[]", *p ) != 0 )
		*subp++ = '/';					
	else {				
		for ( endmark = p; p >= name; p-- )
			if ( index ( "]*?", *p ) != 0 )
				break;
		for ( ++p; (p <= endmark) && subp < (globfree + sizeof ( globfree )); )
			*subp++ = *p++;
	}
	*subp = '\0';
	return ( --subp );
}
#endif

/* rest should be done with nameserver or database */

#include <pwd.h>
#include <grp.h>
#include <utmp.h>

struct	utmp utmp;
#define	NMAX	(sizeof (utmp.ut_name))
#define SCPYN(a, b)	strncpy(a, b, NMAX)

#define NUID	64
#define NGID	300

struct ncache {
	int	uid;
	char	name[NMAX+1];
} nc[NUID];
char	outrangename[NMAX+1];
int	outrangeuid = -1;
char	groups[NGID][NMAX+1];
char	outrangegroup[NMAX+1];
int	outrangegid = -1;

/*
 * This function assumes that the password file is hashed
 * (or some such) to allow fast access based on a name key.
 * If this isn't true, duplicate the code for getgroup().
 */
char *
getname(uid)
{
	register struct passwd *pw;
	struct passwd *getpwent();
	register int cp;

#if	(((NUID) & ((NUID) - 1)) != 0)
	cp = uid % (NUID);
#else
	cp = uid & ((NUID) - 1);
#endif
	if (uid >= 0 && nc[cp].uid == uid && nc[cp].name[0])
		return (nc[cp].name);
	pw = getpwuid(uid);
	if (!pw)
		return (0);
	nc[cp].uid = uid;
	SCPYN(nc[cp].name, pw->pw_name);
	return (nc[cp].name);
}

char *
getgroup(gid)
{
	register struct group *gr;
	static init;
	struct group *getgrent();

	if (gid >= 0 && gid < NGID && groups[gid][0])
		return (&groups[gid][0]);
	if (gid >= 0 && gid == outrangegid)
		return (outrangegroup);
rescan:
	if (init == 2) {
		if (gid < NGID)
			return (0);
		setgrent();
		while (gr = getgrent()) {
			if (gr->gr_gid != gid)
				continue;
			outrangegid = gr->gr_gid;
			SCPYN(outrangegroup, gr->gr_name);
			endgrent();
			return (outrangegroup);
		}
		endgrent();
		return (0);
	}
	if (init == 0)
		setgrent(), init = 1;
	while (gr = getgrent()) {
		if (gr->gr_gid < 0 || gr->gr_gid >= NGID) {
			if (gr->gr_gid == gid) {
				outrangegid = gr->gr_gid;
				SCPYN(outrangegroup, gr->gr_name);
				return (outrangegroup);
			}
			continue;
		}
		if (groups[gr->gr_gid][0])
			continue;
		SCPYN(groups[gr->gr_gid], gr->gr_name);
		if (gr->gr_gid == gid)
			return (&groups[gid][0]);
	}
	init = 2;
	goto rescan;
}

int
getuid(username)
	char *username;
{
	register struct passwd *pw;
	struct passwd *getpwnam();

	pw = getpwnam(username);
	if (pw != NULL)
		return (pw->pw_uid);
	else
		return (-1);
}

int
getgid(groupname)
	char *groupname;
{
	register struct group *gr;
	struct group *getgrnam();

	gr = getgrnam(groupname);
	if (gr != NULL)
		return (gr->gr_gid);
	else
		return (-1);
}

#define permoffset(who)		((who) * 3)
#define permission(who, type)	((type) >> permoffset(who))
#define kbytes(bytes)		(((bytes) + 1023) / 1024)

list(file, stp)
	char *file;
	register struct stat *stp;
{
	char pmode[32], uname[32], gname[32], fsize[32], ftime[32];
	char *getname(), *getgroup(), *ctime();
	static long special[] = { S_ISUID, 's', S_ISGID, 's', S_ISVTX, 't' };
	static time_t sixmonthsago = -1;
#ifdef	S_IFLNK
	char flink[MAXPATHLEN + 1];
#endif
	register int who;
	register char *cp;
	time_t now;

	if (file == NULL || stp == NULL)
		return (-1);

	time(&now);
	if (sixmonthsago == -1)
		sixmonthsago = now - 6L*30L*24L*60L*60L;

	switch (stp->st_mode & S_IFMT) {
#ifdef	S_IFDIR
	case S_IFDIR:	/* directory */
		pmode[0] = 'd';
		break;
#endif
#ifdef	S_IFCHR
	case S_IFCHR:	/* character special */
		pmode[0] = 'c';
		break;
#endif
#ifdef	S_IFBLK
	case S_IFBLK:	/* block special */
		pmode[0] = 'b';
		break;
#endif
#ifdef	S_IFLNK
	case S_IFLNK:	/* symbolic link */
		pmode[0] = 'l';
		break;
#endif
#ifdef	S_IFSOCK
	case S_IFSOCK:	/* socket */
		pmode[0] = 's';
		break;
#endif
#ifdef	S_IFREG
	case S_IFREG:	/* regular */
#endif
	default:
		pmode[0] = '-';
		break;
	}

	for (who = 0; who < 3; who++) {
		if (stp->st_mode & permission(who, S_IREAD))
			pmode[permoffset(who) + 1] = 'r';
		else
			pmode[permoffset(who) + 1] = '-';

		if (stp->st_mode & permission(who, S_IWRITE))
			pmode[permoffset(who) + 2] = 'w';
		else
			pmode[permoffset(who) + 2] = '-';

		if (stp->st_mode & special[who * 2])
			pmode[permoffset(who) + 3] = special[who * 2 + 1];
		else if (stp->st_mode & permission(who, S_IEXEC))
			pmode[permoffset(who) + 3] = 'x';
		else
			pmode[permoffset(who) + 3] = '-';
	}
	pmode[permoffset(who) + 1] = '\0';

	cp = getname(stp->st_uid);
	if (cp != NULL)
		sprintf(uname, "%-9.9s", cp);
	else
		sprintf(uname, "%-9d", stp->st_uid);

	cp = getgroup(stp->st_gid);
	if (cp != NULL)
		sprintf(gname, "%-9.9s", cp);
	else
		sprintf(gname, "%-9d", stp->st_gid);

	if (pmode[0] == 'b' || pmode[0] == 'c')
		sprintf(fsize, "%3d,%4d",
			major(stp->st_rdev), minor(stp->st_rdev));
	else {
		sprintf(fsize, "%8ld", stp->st_size);
#ifdef	S_IFLNK
		if (pmode[0] == 'l') {
			/*
			 * Need to get the tail of the file name, since we have
			 * already chdir()ed into the directory of the file
			 */
			cp = rindex(file, '/');
			if (cp == NULL)
				cp = file;
			else
				cp++;
			who = readlink(cp, flink, sizeof flink - 1);
			if (who >= 0)
				flink[who] = '\0';
			else
				flink[0] = '\0';
		}
#endif
	}

	cp = ctime(&stp->st_mtime);
	if (stp->st_mtime < sixmonthsago || stp->st_mtime > now)
		sprintf(ftime, "%-7.7s %-4.4s", cp + 4, cp + 20);
	else
		sprintf(ftime, "%-12.12s", cp + 4);

	printf("%5lu %4ld %s %2d %s%s%s %s %s%s%s\n",
		stp->st_ino,				/* inode #	*/
#ifdef	S_IFSOCK
		(long) kbytes(dbtob(stp->st_blocks)),	/* kbytes       */
#else
		(long) kbytes(stp->st_size),		/* kbytes       */
#endif
		pmode,					/* protection	*/
		stp->st_nlink,				/* # of links	*/
		uname,					/* owner	*/
		gname,					/* group	*/
		fsize,					/* # of bytes	*/
		ftime,					/* modify time	*/
		file,					/* name		*/
#ifdef	S_IFLNK
		(pmode[0] == 'l') ? " -> " : "",
		(pmode[0] == 'l') ? flink  : ""		/* symlink	*/
#else
		"",
		""
#endif
	);

	return (0);
}
