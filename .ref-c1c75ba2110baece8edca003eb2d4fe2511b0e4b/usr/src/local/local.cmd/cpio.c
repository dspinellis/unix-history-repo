/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)cpio.c	4.3	(Berkeley)	%G%";
#endif not lint

/*	cpio	COMPILE:	cc -O cpio.c -s -i -o cpio 
	cpio -- copy file collections

*/
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#ifdef RT
#define S_IFEXT 0120000	/*  allocated by extents  */
#define S_IF1EXT 0130000	/*  one extent  */
#endif
#define EQ(x,y)	(strcmp(x,y)==0)
/* for VAX, Interdata, ... */
#define MKSHORT(v,lv) {U.l=1L;if(U.c[0]) U.l=lv,v[0]=U.s[1],v[1]=U.s[0]; else U.l=lv,v[0]=U.s[0],v[1]=U.s[1];}
#define MAGIC	070707
#define IN	1
#define OUT	2
#define PASS	3
#define HDRSIZE	((sizeof Hdr)-256)
#define LINKS	1000
#define MERT 0
#define CHARS 76
#ifdef RT
#define MERT 1	/* yes = 1 ;  no = 0 */
extern long filespace;
#endif

struct	stat	Statb, Xstatb;

struct header {
	short	h_magic,
		h_dev;
	unsigned short	h_ino,
		h_mode,
		h_uid,
		h_gid;
	short	h_nlink,
		h_rdev,
		h_mtime[2],
		h_namesize,
		h_filesize[2];
	char	h_name[256];
} Hdr;

int	Bufsize = 512;
short	Buf[256], *Dbuf;
char    BBuf[512];
char    *Cbuf;
int	Wct,Wc;
short	*Wp;
char    *Cp;
#ifdef RT
short Actual_size[2];	/* MERT variable */
struct{
	long long_size;
};
#endif

short	Option,
	Dir,
	Uncond,
	Link,
	Rename,
	Toc,
	Verbose,
	Select,
	Mod_time,
	Acc_time,
	Cflag,
	Swap;

int	Ifile,
	Ofile,
	Input = 0,
	Output = 1;
long	Blocks,
	Longfile,
	Longtime;

char	Fullname[256],
	Name[256];
int	Pathend;

FILE	*Rtty,
	*Wtty;

char	*Pattern[100];
char	Strhdr[500];
char	*Chdr = Strhdr;
short	Dev,
	Uid,
	Gid,
	A_directory,
	A_special,
#ifdef RT
	One_extent,
	Multi_extent,
#endif
	Filetype = S_IFMT;
#ifdef RT
short Remove_mode = 0007777;
short New_mode;
#endif

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
	long lng;
	register char *fullp;
	register i;

	signal(SIGSYS, 1);
	if(*argv[1] != '-')
		usage();
	Uid = getuid();
	umask(0);
	Gid = getgid();
	Pattern[0] = "*";

	while(*++argv[1]) {
		switch(*argv[1]) {
		case 'a':
			Acc_time++;
			break;
		case 'B':
			Bufsize = 5120;
			break;
		case 'i':
			Option = IN;
			if(argc > 2 ) {
				for(i = 0; (i+2) < argc; ++i)
					Pattern[i] = argv[i+2];
			}
			break;
		case 'o':
			if(argc != 2)
				usage();
			Option = OUT;
			break;
		case 'p':
			if(argc != 3)
				usage();
			if(access(argv[2], 2) == -1) {
accerr:
				err("cannot write in <%s>\n", argv[2]);
				exit(2);
			}
			strcpy(Fullname, argv[2]);
			strcat(Fullname, "/");
			stat(Fullname, &Xstatb);
			if((Xstatb.st_mode&S_IFMT) != S_IFDIR)
				goto accerr;
			Option = PASS;
			Dev = Xstatb.st_dev;
			break;
		case 'c':
			Cflag++;
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
				exit(2);
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
		case '6':
			Filetype = 060000;
			break;
		default:
			usage();
		}
	}
	if(!Option) {
		err("Options must include o|i|p\n");
		exit(2);
	}

	if (Cflag && Swap)  {
		err("Swap flag is ignored with Cflag\n");
		Swap = 0;
	}

	if(Option != PASS)  {
		Wp = Dbuf = (short *)malloc(Bufsize);
		Cp = Cbuf = (char *)malloc(Bufsize);
	}
	     Wct = Bufsize >> 1;
	     Wc = Bufsize;

	if(Option == PASS && Rename) {
		err("Pass and Rename cannot be used together");
		exit(2);
	}
	switch(Option) {

	case OUT:
		while(getname()) {
			if( mklong(Hdr.h_filesize) == 0L) {
				if ( Cflag )
				     writehdr(Chdr,CHARS+Hdr.h_namesize);
				else
				     bwrite(&Hdr, HDRSIZE+Hdr.h_namesize);
#ifdef RT
				if( (MERT) && (((Hdr.h_mode & Filetype) == S_IF1EXT)
					|| ((Hdr.h_mode & Filetype) == S_IFEXT))) {
					actsize();
					bwrite(&Hdr, HDRSIZE+Hdr.h_namesize);
				}
#endif
				continue;
			}
			if((Ifile = open(Hdr.h_name, 0)) < 0) {
				err("<%s> ?\n", Hdr.h_name);
				continue;
			}
			if ( Cflag )
			     writehdr(Chdr,CHARS+Hdr.h_namesize);
			else
			     bwrite(&Hdr, HDRSIZE+Hdr.h_namesize);
#ifdef RT
			if( (MERT) && (((Hdr.h_mode & Filetype) == S_IF1EXT)
				|| ((Hdr.h_mode & Filetype) == S_IFEXT))) {
				actsize();
				bwrite(&Hdr, HDRSIZE+Hdr.h_namesize);
			}
#endif
			for(filesz=mklong(Hdr.h_filesize); filesz>0; filesz-= 512){
				ct = filesz>512? 512: filesz;
				   if(read(Ifile, Cflag? BBuf: (char *)Buf, ct) < 0) {
					err("Cannot read %s\n", Hdr.h_name);
					continue;
				   }
				Cflag? writehdr(BBuf,ct): bwrite(Buf,ct);
			}
			close(Ifile);
			if(Acc_time)
				utime(Hdr.h_name, &Statb.st_atime);
			if(Verbose)
				err("%s\n", Hdr.h_name);
		}
		strcpy(Hdr.h_name, "TRAILER!!!");
		Hdr.h_magic = MAGIC;
		MKSHORT(Hdr.h_filesize, 0L);
		Hdr.h_namesize = strlen("TRAILER!!!") + 1;
		if ( Cflag )  {
		     lng = 0;
		     bintochar(lng);
		     writehdr(Chdr,CHARS+Hdr.h_namesize);
		}
		else
		     bwrite(&Hdr, HDRSIZE+Hdr.h_namesize);
		Cflag? writehdr(Cbuf, Bufsize): bwrite(Dbuf, Bufsize);
		break;

	case IN:
		pwd();
		while(gethdr()) {
			Ofile = ckname(Hdr.h_name)? openout(Hdr.h_name): 0;
			for(filesz=mklong(Hdr.h_filesize); filesz>0; filesz-= 512){
				ct = filesz>512? 512: filesz;
				Cflag? readhdr(BBuf, ct): bread(Buf, ct);
				if(Ofile) {
					if(Swap)
						swap(Buf, ct);
					   if(write(Ofile, Cflag? BBuf: (char *)Buf, ct) < 0) {
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
			&& Dev == Statb.st_dev) {
/* ???			&& (Uid == Statb.st_uid || !Uid)) {*/
				if(link(Hdr.h_name, Fullname) < 0) { /* missing dir.? */
					unlink(Fullname);
					missdir(Fullname);
					if(link(Hdr.h_name, Fullname) < 0) {
						err(
						 "Cannot link <%s> & <%s>\n",
						 Hdr.h_name, Fullname);
						continue;
					}
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
			if(Acc_time)
				utime(Hdr.h_name, &Statb.st_atime);
			if(Ofile) {
				close(Ofile);
				set_time(Fullname, Statb.st_atime, mklong(Hdr.h_mtime));
ckverbose:
				if(Verbose)
					puts(Fullname);
			}
		}
	}
	err("%ld blocks\n", Blocks * (Bufsize>>9));
	exit(0);
}
usage()
{
	err("Usage: cpio -o[acvB] <name-list >collection\n%s\n%s\n",
	"       cpio -i[cdmrstuvB6] [pattern ...] <collection",
	"       cpio -p[adlmruv] directory <name-list");
	exit(2);
}

getname()
{
	register char *namep = Name;
	long tlong;

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
		A_directory = (Statb.st_mode & Filetype) == S_IFDIR;
		A_special = ((Statb.st_mode & Filetype) == S_IFBLK)
			|| ((Statb.st_mode & Filetype) == S_IFCHR);
#ifdef RT
		if(MERT) {
			One_extent = (Statb.st_mode & Filetype) == S_IF1EXT;
			Multi_extent = (Statb.st_mode & Filetype) == S_IFEXT;
		}
#endif
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
		if(Cflag)
			bintochar(tlong);
		return 1;
	}
}

bintochar(t)
long t;
{
	sprintf(Chdr,"%.6o%.6ho%.6ho%.6ho%.6ho%.6ho%.6ho%.6ho%.11lo%.6ho%.11lo%s",
		MAGIC,Statb.st_dev,Statb.st_ino,Statb.st_mode,Statb.st_uid,
		Statb.st_gid,Statb.st_nlink,Statb.st_rdev & 00000177777,
		Statb.st_mtime,(short)strlen(Hdr.h_name)+1,t,Hdr.h_name);
}

chartobin()
{
	sscanf(Chdr,"%6ho%6ho%6ho%6ho%6ho%6ho%6ho%6ho%11lo%6ho%11lo",
		&Hdr.h_magic,&Hdr.h_dev,&Hdr.h_ino,&Hdr.h_mode,&Hdr.h_uid,
		&Hdr.h_gid,&Hdr.h_nlink,&Hdr.h_rdev,&Longtime,&Hdr.h_namesize,
		&Longfile);
	MKSHORT(Hdr.h_filesize, Longfile);
	MKSHORT(Hdr.h_mtime, Longtime);
}

gethdr()
{

	if ( Cflag )  {
		readhdr(Chdr,CHARS);
		chartobin();
	}
	else
		bread(&Hdr, HDRSIZE);

	if(Hdr.h_magic != MAGIC) {
		err("Out of phase--get help\n");
		exit(2);
	}
	if(!Cflag)
	     bread(Hdr.h_name, Hdr.h_namesize);
	else
	     readhdr(Hdr.h_name, Hdr.h_namesize);
	if(Swap)
		swap(Hdr.h_name, Hdr.h_namesize);
	if(EQ(Hdr.h_name, "TRAILER!!!"))
		return 0;
	A_directory = (Hdr.h_mode & Filetype) == S_IFDIR;
	A_special =((Hdr.h_mode & Filetype) == S_IFBLK)
		|| ((Hdr.h_mode & Filetype) == S_IFCHR);
#ifdef RT
	if( (MERT) && (((Hdr.h_mode & Filetype) == S_IF1EXT)
		|| ((Hdr.h_mode & Filetype) == S_IFEXT))) {
		One_extent = (Hdr.h_mode & Filetype) == S_IF1EXT;
		Multi_extent = (Hdr.h_mode & Filetype) == S_IFEXT;
		Actual_size[0] = Hdr.h_filesize[0];
		Actual_size[1] = Hdr.h_filesize[1];
		bread(&Hdr, HDRSIZE);
		if(Hdr.h_magic != MAGIC) {
			err("Out of phase--get MERT help\n");
			exit(2);
		}
		bread(Hdr.h_name, Hdr.h_namesize);
	}
#endif
	return 1;
}

ckname(namep)
register char *namep;
{
	++Select;
	if(!nmatch(namep, Pattern)) {
		Select = 0;
		return 0;
	}
	if(Rename && !A_directory) {
		fprintf(Wtty, "Rename <%s>\n", namep);
		fflush(Wtty);
		fgets(namep, 128, Rtty);
		if(feof(Rtty))
			exit(2);
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

		if(!makdir(namep)) {
			missdir(namep);
		}
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
	if(stat(namep, &Xstatb) == 0) {
		if(Uncond && !(Xstatb.st_mode & S_IWRITE))
			unlink(namep);
		if(!Uncond && (mklong(Hdr.h_mtime) < Xstatb.st_mtime)) {
			err("current <%s> newer\n", namep);
			return 0;
		}
	}
	if(Option == PASS
	&& Hdr.h_ino == Xstatb.st_ino
	&& Hdr.h_dev == Xstatb.st_dev) {
		err("Attempt to pass file to self!\n");
		exit(2);
	}
#ifdef RT
one_again:
	if(One_extent || Multi_extent) {
		if((f = falloc(namep, Hdr.h_mode, Hdr.h_filesize[0].long_size)) < 0) {
			if(missdir(namep))
				goto one_again;
			err("Cannot create <%s> (errno:%d)\n", namep, errno);
			return 0;
		}
		if(filespace < Hdr.h_filesize[0].long_size){
			err("Cannot create contiguous file <%s> proper size\n", namep);
			err("    <%s> will be created as a regular file\n", namep);
			if(unlink(Fullname) != 0)
				err("<%s> not removed\n", namep);
			New_mode = Hdr.h_mode & Remove_mode;
			New_mode = New_mode | S_IFREG;
once_again:
			if((f = creat(namep, New_mode)) < 0){
				if(missdir(namep))
					goto once_again;
				err("Cannot create <%s> (errno:%d)\n", namep, errno);
				return (0);
			}
		}
	}
#endif
#ifdef RT
	if(MERT && (One_extent || Multi_extent))
		goto skip_c;
#endif
c_again:
	if((f = creat(namep, Hdr.h_mode)) < 0) {
		if(missdir(namep))
			goto c_again;
		err("Cannot create <%s> (errno:%d)\n", namep, errno);
		return 0;
	}
#ifdef RT
skip_c:
#endif
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

readhdr(b, c)
register c;
register char *b;
{
	static nleft = 0;
	static char *ip;
	register char *p = ip;

	while(c--)  {
		if(!nleft)  {
again:
			if(read(Input, Cbuf, Bufsize) != Bufsize)  {
				Input = chgreel(0, Input);
				goto again;
			}
			nleft = Bufsize;
			p = Cbuf;
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
writehdr(rp,c)
register char *rp;
register c;
{
         register char *cp = Cp;

         while(c--)  {
                 if(!Wc)  {
again:
                        if(write(Output,Cbuf,Bufsize)<0)  {
                                Output = chgreel(1, Output);
                                goto again;
                        }
                        Wc = Bufsize;
                        cp = Cbuf;
                        ++Blocks;
                 }
                 *cp++ = *rp++;
                 --Wc;
         }
         Cp = cp;
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
	|| !(ml[mlinks] = (struct ml *)malloc(strlen(np) + 2 + sizeof(struct ml)))) {
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
		} else {
			printf("%-6d", Hdr.h_uid);
			lastid = -1;
		}
	}
	printf("%7ld ", mklong(Hdr.h_filesize));
	U.l = mklong(Hdr.h_mtime);
	strcpy(tbuf, ctime(&U.l));
	tbuf[24] = '\0';
	printf(" %s  %s\n", &tbuf[4], namep);
}

nmatch(s, pat)
char *s, **pat;
{
	if(EQ(*pat, "*"))
		return 1;
	while(*pat) {
		if((**pat == '!' && !gmatch(s, *pat+1))
		|| gmatch(s, *pat))
			return 1;
		++pat;
	}
	return 0;
}
gmatch(s, p)
register char *s, *p;
{
	register int c;
	register cc, ok, lc, scc;

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
	register pid;

	if(pid = fork())
		while(wait(&status) != pid);
	else {
		close(2);
		execl("/bin/mkdir", "mkdir", namep, 0);
		exit(2);
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

	if(!Mod_time)
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
#ifdef RT
	if(!MERT){
		if((statb.st_mode&S_IFMT) != S_IFCHR)
			exit(2);
	}
	else if((statb.st_mode & (S_IFBLK|S_IFREC))==0)
		exit(2);
#endif
#ifndef RT
	if((statb.st_mode&S_IFMT) != S_IFCHR)
		exit(2);
#endif
again:
	err("If you want to go on, type device/file name when ready\n");
	devtty = fopen("/dev/tty", "r");
	fgets(str, 20, devtty);
	str[strlen(str) - 1] = '\0';
	if(!*str)
		exit(2);
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
	fgets(Fullname, 256, dir);
	if(pclose(dir))
		exit(2);
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

	if(*n == '/') /* don't try to chdir on full pathnames */
		return n;
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
				err("Cannot chdir (no `d' option)\n");
				exit(2);
			} else if(chdir(p_save) == -1)
				goto cd_err;
		}
	} else
		*p_save = '\0';
	return n_save;
}
#ifdef RT
actsize()
{
}
#endif
