/*	cpio	COMPILE:	cc -O cpio.c -s -i -o cpio -lS
	cpio -- copy file collections

*/
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#define EQ(x,y)	(strcmp(x,y)==0)
/* for VAX, Interdata, ... */
#define MKSHORT(v,lv) {U.l=1L;if(U.c[0]) U.l=lv,v[0]=U.s[1],v[1]=U.s[0]; else U.l=lv,v[0]=U.s[0],v[1]=U.s[1];}
#define MAGIC	070707
#define FILETYPE	060000
#define IN	1
#define OUT	2
#define PASS	3
#define HDRSIZE	((sizeof Hdr)-256)
#define LINKS	1000

struct	stat	Statb, Xstatb;

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
} Hdr;

int	Bufsize = 512;
short	Buf[256], *Dbuf;
int	Wct;
short	*Wp;

short	Option,
	Dir,
	Uncond,
	Link,
	Rename,
	Toc,
	Verbose,
	Select,
	Mod_time,
	Swap;

int	Ifile,
	Ofile,
	Input = 0,
	Output = 1;
long	Blocks;

char	Fullname[256],
	Name[256];
int	Pathend;

FILE	*Rtty,
	*Wtty;

char	*Pattern;
short	Dev,
	Uid,
	Gid,
	A_directory,
	A_special;
extern	errno;
char	*malloc();
char 	*cd();
char	*Cd_name;
FILE 	*popen();

union { long l; short s[2]; char c[4]; } U;

/* for VAX, Interdata, ... */
long mklong(v)
short v[];
{
	U.l = 1;
	if(U.c[0])
		U.s[0] = v[1], U.s[1] = v[0];
	else
		U.s[0] = v[0], U.s[1] = v[1];
	return U.l;
}

main(argc, argv)
char **argv;
{
	register ct;
	long	filesz;
	register char *lastarg, *fullp;

	if(argc < 2 || argc > 4) {
usage:
		err("Usage: cpio -o[vB] <name-list >collection\n%s\n%s\n",
		"       cpio -i[drstuvB] [pattern] <collection",
		"       cpio -p[dlruv] [pattern] directory <name-list");
		exit(1);
	}
	signal(SIGSYS, 1);
	lastarg = argv[argc-1];
	if(*argv[1] != '-')
		goto usage;
	Uid = getuid();
	umask(0);
	Gid = getgid();

	while(*++argv[1]) {
		switch(*argv[1]) {
		case 'B':
			Bufsize = 5120;
			break;
		case 'i':
			Option = IN;
			Pattern = argc == 2? "*": argv[2];
			break;
		case 'o':
			Option = OUT;
			break;
		case 'p':
			if(access(lastarg, 2) == -1) {
accerr:
				err("cannot write in <%s>\n", lastarg);
				exit(1);
			}
			strcpy(Fullname, lastarg);
			strcat(Fullname, "/");
			stat(Fullname, &Xstatb);
			if((Xstatb.st_mode&S_IFMT) != S_IFDIR)
				goto accerr;
			Option = PASS;
			Dev = Xstatb.st_dev;
			Pattern = argc == 3? "*": argv[2];
			break;
		case 'd':
			Dir++;
			break;
		case 'l':
			Link++;
			break;
		case 'm':
			Mod_time++;
			break;
		case 'r':
			Rename++;
			Rtty = fopen("/dev/tty", "r");
			Wtty = fopen("/dev/tty", "w");
			if(Rtty==NULL || Wtty==NULL) {
				err(
				  "Cannot rename (/dev/tty missing)\n");
				exit(1);
			}
			break;
		case 's':
			Swap++;
			break;
		case 't':
			Toc++;
			break;
		case 'u':
			Uncond++;
			break;
		case 'v':
			Verbose++;
			break;
		default:
			goto usage;
		}
	}
	if(!Option) {
		err("Options must include o|i|p\n");
		exit(1);
	}

	if(Option != PASS)
		Wp = Dbuf = malloc(Bufsize);
	Wct = Bufsize >> 1;

	if(Option == PASS && Rename) {
		err("Pass and Rename cannot be used together");
		exit(1);
	}
	switch(Option) {

	case OUT:
		while(getname()) {
			if(mklong(Hdr.h_filesize) == 0L) {
				bwrite(&Hdr, HDRSIZE+Hdr.h_namesize);
				continue;
			}
			if((Ifile = open(Hdr.h_name, 0)) < 0) {
				err("<%s> ?\n", Hdr.h_name);
				continue;
			}
			bwrite(&Hdr, HDRSIZE+Hdr.h_namesize);
			for(filesz=mklong(Hdr.h_filesize); filesz>0; filesz-= 512){
				ct = filesz>512? 512: filesz;
				if(read(Ifile, Buf, ct) < 0) {
					err("Cannot read %s\n", Hdr.h_name);
					continue;
				}
				bwrite(Buf, ct);
			}
			close(Ifile);
			if(Verbose)
				err("%s\n", Hdr.h_name);
		}
		strcpy(Hdr.h_name, "TRAILER!!!");
		MKSHORT(Hdr.h_filesize, 0L);
		Hdr.h_namesize = strlen("TRAILER!!!") + 1;
		bwrite(&Hdr, HDRSIZE+Hdr.h_namesize);
		bwrite(Dbuf, Bufsize);
		break;

	case IN:
		pwd();
		while(gethdr()) {
			Ofile = ckname(Hdr.h_name)? openout(Hdr.h_name): 0;
			for(filesz=mklong(Hdr.h_filesize); filesz>0; filesz-= 512){
				ct = filesz>512? 512: filesz;
				bread(Buf, ct);
				if(Ofile) {
					if(Swap)
						swap(Buf, ct);
					if(write(Ofile, Buf, ct) < 0) {
					 err("Cannot write %s\n", Hdr.h_name);
					 continue;
					}
				}
			}
			if(Ofile) {
				close(Ofile);
				set_time(Cd_name, mklong(Hdr.h_mtime), mklong(Hdr.h_mtime));
			}
			if(!Select)
				continue;
			if(Verbose)
				if(Toc)
					pentry(Hdr.h_name);
				else
					puts(Hdr.h_name);
			else if(Toc)
				puts(Hdr.h_name);
		}
		break;

	case PASS:
		fullp = Fullname + strlen(Fullname);

		while(getname()) {
			if(!ckname(Hdr.h_name))
				continue;
			strcpy(fullp, Hdr.h_name);

			if(Link
			&& !A_directory
			&& Dev == Statb.st_dev
			&& (Uid == Statb.st_uid || !Uid)) {
				unlink(Fullname);
				if(link(Hdr.h_name, Fullname) < 0) {
					err(
					 "Cannot link <%s> & <%s>\n",
					 Hdr.h_name, Fullname);
					continue;
				}
				set_time(Hdr.h_name, mklong(Hdr.h_mtime), mklong(Hdr.h_mtime));
				goto ckverbose;
			}
			if(!(Ofile = openout(Fullname)))
				continue;
			if((Ifile = open(Hdr.h_name, 0)) < 0) {
				err("<%s> ?\n", Hdr.h_name);
				close(Ofile);
				continue;
			}
			filesz = Statb.st_size;
			for(; filesz > 0; filesz -= 512) {
				ct = filesz>512? 512: filesz;
				if(read(Ifile, Buf, ct) < 0) {
					err("Cannot read %s\n", Hdr.h_name);
					break;
				}
				if(Ofile)
					if(write(Ofile, Buf, ct) < 0) {
					 err("Cannot write %s\n", Hdr.h_name);
					 break;
					}
				++Blocks;
			}
			close(Ifile);
			if(Ofile) {
				close(Ofile);
				set_time(Fullname, Statb.st_atime, mklong(Hdr.h_mtime));
ckverbose:
				if(Verbose)
					puts(Fullname);
			}
		}
	}
	err("%D blocks\n", Blocks * (Bufsize>>9));
	exit(0);
}

getname()
{
	register char *namep = Name;
	static long tlong;

	for(;;) {
		if(gets(namep) == NULL)
			return 0;
		if(*namep == '.' && namep[1] == '/')
			namep += 2;
		strcpy(Hdr.h_name, namep);
		if(stat(namep, &Statb) < 0) {
			err("< %s > ?\n", Hdr.h_name);
			continue;
		}
		A_directory = (Statb.st_mode & FILETYPE) == S_IFDIR;
		A_special = ((Statb.st_mode & FILETYPE) == S_IFBLK)
			|| ((Statb.st_mode & FILETYPE) == S_IFCHR);
		Hdr.h_magic = MAGIC;
		Hdr.h_namesize = strlen(Hdr.h_name) + 1;
		Hdr.h_uid = Statb.st_uid;
		Hdr.h_gid = Statb.st_gid;
		Hdr.h_dev = Statb.st_dev;
		Hdr.h_ino = Statb.st_ino;
		Hdr.h_mode = Statb.st_mode;
		MKSHORT(Hdr.h_mtime, Statb.st_mtime);
		Hdr.h_nlink = Statb.st_nlink;
		tlong = Hdr.h_mode & S_IFREG? Statb.st_size: 0L;
		MKSHORT(Hdr.h_filesize, tlong);
		Hdr.h_rdev = Statb.st_rdev;
		return 1;
	}
}

gethdr()
{

	bread(&Hdr, HDRSIZE);

	if(Hdr.h_magic != MAGIC) {
		err("Out of phase--get help");
abort();
		exit(1);
	}
	bread(Hdr.h_name, Hdr.h_namesize);
	if(Swap) {
		swap(Hdr.h_name, Hdr.h_namesize);
	}
	if(EQ(Hdr.h_name, "TRAILER!!!"))
		return 0;
	A_directory = (Hdr.h_mode & FILETYPE) == S_IFDIR;
	A_special =((Hdr.h_mode & FILETYPE) == S_IFBLK)
		|| ((Hdr.h_mode & FILETYPE) == S_IFCHR);
	return 1;
}

ckname(namep)
register char *namep;
{
	++Select;
	if(!gmatch(namep, Pattern)) {
		Select = 0;
		return 0;
	}
	if(Rename && !A_directory) {
		fprintf(Wtty, "Rename <%s>\n", namep);
		fflush(Wtty);
		fgets(namep, 128, Rtty);
		if(feof(Rtty))
			exit(1);
		namep[strlen(namep) - 1] = '\0';
		if(EQ(namep, "")) {
			printf("Skipped\n");
			return 0;
		}
	}
	return !Toc;
}

openout(namep)
register char *namep;
{
	register f;
	register char *np;

	if(!strncmp(namep, "./", 2))
		namep += 2;
	np = namep;
	if(Option == IN)
		Cd_name = namep = cd(namep);
	if(A_directory) {
		if(!Dir
		|| Rename
		|| EQ(namep, ".")
		|| EQ(namep, "..")
		|| stat(namep, &Xstatb) == 0)
			return 0;

		while(!makdir(namep))
			missdir(namep);
ret:
		chmod(namep, Hdr.h_mode);
		if(Uid == 0)
			chown(namep, Hdr.h_uid, Hdr.h_gid);
		set_time(namep, mklong(Hdr.h_mtime), mklong(Hdr.h_mtime));
		return 0;
	}
	if(Hdr.h_nlink > 1)
		if(!postml(namep, np))
			return 0;
	if(A_special) {
s_again:
		if(mknod(namep, Hdr.h_mode, Hdr.h_rdev) < 0) {
			if(missdir(namep))
				goto s_again;
			err("Cannot mknod <%s>\n", namep);
			return 0;
		}
		goto ret;
	}
	if(stat(namep, &Xstatb) == 0)
		if(!Uncond && (mklong(Hdr.h_mtime) < Xstatb.st_mtime)) {
			err("current <%s> newer\n", namep);
			return 0;
		}
	if(Option == PASS
	&& Hdr.h_ino == Xstatb.st_ino
	&& Hdr.h_dev == Xstatb.st_dev) {
		err("Attempt to pass file to self!\n");
		exit(1);
	}
c_again:
	if((f = creat(namep, Hdr.h_mode)) < 0) {
		if(missdir(namep))
			goto c_again;
		err("Cannot create <%s> (errno:%d)\n", namep, errno);
		return 0;
	}
	if(Uid == 0)
		chown(namep, Hdr.h_uid, Hdr.h_gid);
	return f;
}

bread(b, c)
register c;
register short *b;
{
	static nleft = 0;
	static short *ip;
	register short *p = ip;

	c = (c+1)>>1;
	while(c--) {
		if(!nleft) {
again:
			if(read(Input, Dbuf, Bufsize)!=Bufsize) {
				Input = chgreel(0, Input);
				goto again;
			}
			nleft = Bufsize >> 1;
			p = Dbuf;
			++Blocks;
		}
		*b++ = *p++;
		--nleft;
	}
	ip = p;
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
			if(write(Output, Dbuf, Bufsize)<0) {
				Output = chgreel(1, Output);
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

postml(namep, np)
register char *namep, *np;
{
	register i;
	static struct ml {
		short	m_dev,
			m_ino;
		char	m_name[2];
	} *ml[LINKS];
	static	mlinks = 0;
	char *mlp;

	for(i = 0; i < mlinks; ++i) {
		if(mlinks == LINKS) break;
		if(ml[i]->m_ino==Hdr.h_ino &&
			ml[i]->m_dev==Hdr.h_dev) {
			if(Verbose)
			  printf("%s linked to %s\n", ml[i]->m_name,
				np);
			unlink(namep);
			if(Option == IN) {
				Fullname[Pathend] = '\0';
				strcat(Fullname, ml[i]->m_name);
				mlp = Fullname;
			} else
				mlp = ml[i]->m_name;
l_again:
			if(link(mlp, namep) < 0) {
				if(missdir(np))
					goto l_again;
				err("Cannot link <%s>&<%s>.\n",
					ml[i]->m_name, np);
			}
			set_time(namep, mklong(Hdr.h_mtime), mklong(Hdr.h_mtime));
			return 0;
		}
	}
	if(mlinks == LINKS
	|| (ml[mlinks] = malloc(strlen(np) + sizeof(struct ml))) == 0) {
		static int first=1;

		if(first)
			if(mlinks == LINKS)
				err("Too many links\n");
			else
				err("No memory for links\n");
		mlinks = LINKS;
		first = 0;
		return 1;
	}
	ml[mlinks]->m_dev = Hdr.h_dev;
	ml[mlinks]->m_ino = Hdr.h_ino;
	strcpy(ml[mlinks]->m_name, np);
	++mlinks;
	return 1;
}

pentry(namep)
register char *namep;
{

	register i;
	static short lastid = -1;
#include <pwd.h>
	static struct passwd *pw;
	struct passwd *getpwuid();
	static char tbuf[32];

	printf("%-7o", Hdr.h_mode & 0177777);
	if(lastid == Hdr.h_uid)
		printf("%-6s", pw->pw_name);
	else {
		setpwent();
		if(pw = getpwuid(Hdr.h_uid)) {
			printf("%-6s", pw->pw_name);
			lastid = Hdr.h_uid;
		} else
			printf("%-6d", Hdr.h_uid);
	}
	printf("%7D ", mklong(Hdr.h_filesize));
	U.l = mklong(Hdr.h_mtime);
	strcpy(tbuf, ctime(&U.l));
	tbuf[24] = '\0';
	printf(" %s  %s\n", &tbuf[4], namep);
}

gmatch(s, p)
register char *s, *p;
{
	register int c;
	int cc, ok, lc, scc;

	if(EQ(p, "*"))
		return 1;
	scc = *s;
	lc = 077777;
	switch (c = *p) {

	case '[':
		ok = 0;
		while (cc = *++p) {
			switch (cc) {

			case ']':
				if (ok)
					return(gmatch(++s, ++p));
				else
					return(0);

			case '-':
				ok |= (lc <= scc & scc <= (cc=p[1]));
			}
			if (scc==(lc=cc)) ok++;
		}
		return(0);

	case '?':
	caseq:
		if(scc) return(gmatch(++s, ++p));
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
		if (gmatch(s++,p)) return(1);
	return(0);
}

makdir(namep)
register char *namep;
{
	static status;

	if(fork())
		wait(&status);
	else {
		close(2);
		execl("/bin/mkdir", "mkdir", namep, 0);
		exit(1);
	}
	return ((status>>8) & 0377)? 0: 1;
}

swap(buf, ct)
register ct;
register union swp { short	shortw; char	charv[2]; } *buf;
{
	register char c;

	ct = (ct + 1) >> 1;

	while(ct--) {
		c = buf->charv[0];
		buf->charv[0] = buf->charv[1];
		buf->charv[1] = c;
		++buf;
	}
}
set_time(namep, atime, mtime)
register *namep;
long atime, mtime;
{
	static long timevec[2];

	if(Uid || !Mod_time)
		return;
	timevec[0] = atime;
	timevec[1] = mtime;
	utime(namep, timevec);
}
chgreel(x, fl)
{
	register f;
	char str[22];
	FILE *devtty;
	struct stat statb;

	err("errno: %d, ", errno);
	err("Can't %s\n", x? "write output": "read input");
	fstat(fl, &statb);
	if((statb.st_mode&S_IFMT) != S_IFCHR)
		exit(1);
again:
	err("If you want to go on, type device/file name when ready\n");
	devtty = fopen("/dev/tty", "r");
	fgets(str, 20, devtty);
	str[strlen(str) - 1] = '\0';
	if(!*str)
		exit(1);
	close(fl);
	if((f = open(str, x? 1: 0)) < 0) {
		err("That didn't work");
		fclose(devtty);
		goto again;
	}
	return f;
}
missdir(namep)
register char *namep;
{
	register char *np;
	register ct = 0;

	if(!Dir)
		return 0;
	for(np = namep; *np; ++np)
		if(*np == '/') {
			*np = '\0';
			if(stat(namep, &Xstatb) == -1)
				makdir(namep), ++ct;
			*np = '/';
		}
	return ct;
}
err(a, b, c)
{
	fprintf(stderr, a, b, c);
}
pwd()
{
	FILE *dir;

	dir = popen("pwd", "r");
	fgets(Fullname, 128, dir);
	pclose(dir);
	Pathend = strlen(Fullname);
	Fullname[Pathend - 1] = '/';
}
char * cd(n)
register char *n;
{
	char *p_save = Name, *n_save = n, *p_end = 0;
	register char *p = Name;
	static char dotdot[]="../../../../../../../../../../../../../../../../../../../../../../../../../../../../../../../../";
	int slashes;

	for(; *p && *n == *p; ++p, ++n) { /* whatever part of strings == */
		if(*p == '/')
			p_save = p+1, n_save = n+1;
	}

	p = p_save;
	*p++ = '\0';
	for(slashes = 0; *p; ++p) { /* if prev is longer, chdir("..") */
		if(*p == '/')
			++slashes;
	}
	p = p_save;
	if(slashes) {
		slashes = slashes * 3 - 1;
		dotdot[slashes] = '\0';
		chdir(dotdot);
		dotdot[slashes] = '/';
	}

	n = n_save;
	for(; *n; ++n, ++p) {
		*p = *n;
		if(*n == '/')
			p_end = p+1, n_save = n+1;
	}
	*p = '\0';

	if(p_end) {
		*p_end = '\0';
		if(chdir(p_save) == -1) {
			if(!missdir(p_save)) {
cd_err:
				err("Cannot chdir\n");
				abort();
			} else if(chdir(p_save) == -1)
				goto cd_err;
		}
	} else
		*p_save = '\0';
	return n_save;
}
