/*	Copyright (c) 1988 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ident	"@(#)cpio:cpio.c	1.30.1.11"
/*	/sccs/src/cmd/s.cpio.c
	cpio.c	1.30.1.11	1/11/86 13:46:48
	Reworked cpio which uses getopt(3) to interpret flag arguments and
	changes reels to the save file name.
	Performance and size improvements.
*/

/*	cpio	COMPILE:	cc -O cpio.c -s -i -o cpio -lgen -lerr
	cpio -- copy file collections

*/
#include <errno.h>
#include <fcntl.h>
#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <varargs.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>

struct utimbuf {
	time_t	actime;
	time_t	modtime;
};
#ifndef S_IFIFO
#define	S_IFIFO	010000
#endif

#define EQ(x,y)	(strcmp(x,y)==0)

				/* MKSHORT:  for VAX, Interdata, ...	*/
				/* Take a 4-byte long, lv, and turn it	*/
				/* into an array of two 2-byte shorts, v*/
#define MKSHORT(v,lv) {U.l=1L;if(U.c[0]) U.l=lv,v[0]=U.s[1],v[1]=U.s[0]; else U.l=lv,v[0]=U.s[0],v[1]=U.s[1];}

#define MAGIC	070707		/* cpio magic number */
#define BSMAGIC	0143561		/* byte-swapped cpio magic number */
#define IN	'i'		/* copy in */
#define OUT	'o'		/* copy out */
#define PASS	'p'		/* direct copy */
#define HDRSIZE	(Hdr.h_name - (char *)&Hdr)	/* header size minus filename field */
#define LINKS	500		/* no. of links allocated per bunch */
#define CHARS	76		/* ASCII header size minus filename field */
#define BUFSIZE 512		/* In u370, can't use BUFSIZ or BSIZE */
#define CPIOBSZ 4096		/* file read/write */
#define MK_USHORT(a)	(a & 00000177777)	/* Make unsigned shorts for portable  */
						/* header.  Hardware may only know    */
						/* integer operations and sign extend */
						/* the large unsigned short resulting */
						/* in 8 rather than 6 octal char in   */
						/* the header.			      */

static struct	stat	Statb, Xstatb;

	/* Cpio header format */
static struct header {
	short	h_magic;
	short	h_dev;
	ushort	h_ino;
	ushort	h_mode,
		h_uid,
		h_gid;
	short	h_nlink;
	short	h_rdev;
	short	h_mtime[2],
		h_namesize,
		h_filesize[2];
	char	h_name[256];
} Hdr;

char	Symlbuf[MAXPATHLEN + 1];	/* target of symbolic link */
static unsigned	Bufsize = BUFSIZE;		/* default record size */
static char	Buf[CPIOBSZ], *Cbuf;
static char	*Cp;


static
short	Option,
	Dir,
	Uncond,
	PassLink,
	Rename,
	Toc,
	Verbose,
	Mod_time,
	Acc_time,
	Cflag,
	fflag,
	Swap,
	byteswap,
	halfswap;

static
int	Ifile,
	Ofile,
	Input = 0,
	Output = 1;
			/* sBlocks: short Blocks.  Cumulative character   */
			/* count for short reads in bread().  Encountered */
			/* with communication lines and pipes as in:      */
			/* split -100 cpio_archive; cat xa* | cpio -icd   */
static
long	sBlocks,
	Blocks,
	Longfile,
	Longtime;

static
char	Fullname[256],
	Name[256];
static
int	Pathend;
static
char	*swfile;
static
char	*eommsg = "Change to part %d and press RETURN key. [q] ";

static
FILE	*Rtty,
	*Wtty;
static
char	ttyname[] = "/dev/tty";

static
char	**Pattern = 0;
static
char	Chdr[500];
static
short	Dev;
ushort	Uid,
	A_directory,
	A_special,
	A_symlink,
	Filetype = S_IFMT;

extern	errno;
extern	void exit();
char	*malloc();
FILE 	*popen();

static char *smemcpy();

static
union {
	long l;
	short s[2];
	char c[4];
} U;

/* for VAX, Interdata, ... */
static
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
	int	symlsz;
	register char *fullp;
	register i;
	int ans;
	register char *symlinkp;
	short select;			/* set when files are selected */
	extern char	*optarg;
	extern int	optind;

	signal(SIGSYS, SIG_IGN);
	if(argc <= 1 || *argv[1] != '-')
		usage();
	Uid = getuid();

	while( (ans = getopt( argc, argv, "aBC:ifopcdlmrSsbtuvM:6eI:O:")) != EOF ) {

		switch( ans ) {
		case 'a':		/* reset access time */
			Acc_time++;
			break;
		case 'B':		/* change record size to 5120 bytes */
			Bufsize = 5120;
			break;
		case 'C':		/* reset buffer size to arbitrary valu
					*/
			Bufsize = atoi( optarg );
			if( Bufsize == 0 ) {
				fperr("Illegal argument to -%c, '%s'",
					ans, optarg );
				exit(2);
			}
			break;
		case 'i':
			Option = IN;
			break;
		case 'f':	/* copy files not matched by patterns */
			fflag++;
			break;
		case 'o':
			Option = OUT;
			break;
		case 'p':
			Option = PASS;
			break;
		case 'c':		/* ASCII header */
			Cflag++;
			break;
		case 'd':		/* create directories when needed */
			Dir++;
			break;
		case 'l':		/* link files, when necessary */
			PassLink++;
			break;
		case 'm':		/* retain mod time */
			Mod_time++;
			break;
		case 'r':		/* rename files interactively */
			Rename++;
			Rtty = fopen(ttyname, "r");
			Wtty = fopen(ttyname, "w");
			if(Rtty==NULL || Wtty==NULL) {
				fperrno("Cannot rename (%s missing)",
					ttyname );
				exit(2);
			}
			break;
		case 'S':		/* swap halfwords */
			halfswap++;
			Swap++;
			break;
		case 's':		/* swap bytes */
			byteswap++;
			Swap++;
			break;
		case 'b':		/* swap both bytes and halfwords */
			halfswap++;
			byteswap++;
			Swap++;
			break;
		case 't':		/* table of contents */
			Toc++;
			break;
		case 'u':		/* copy unconditionally */
			Uncond++;
			break;
		case 'v':		/* verbose - print out file names */
			Verbose++;
			break;
		case 'M':		/* alternate message for end-of-media */
			eommsg = optarg;
			break;
		case '6':		/* for old, sixth-edition files */
			Filetype = 060000;
			break;
		case 'I':
			chkswfile( swfile, ans, Option );
			if( (i = open( optarg, O_RDONLY ) ) < 0) {
				fperrno("Cannot open <%s> for input", optarg);
				exit(2);
			}
			if( dup2(i, Input ) < 0 ) {
				fperrno("Cannot dup to standard input");
				exit(2);
			}
			swfile = optarg;
			break;
		case 'O':
			chkswfile( swfile, ans, Option );
			if( (i = open( optarg, O_WRONLY | O_CREAT | O_TRUNC,
			    0666 ) ) < 0) {
				fperrno("Cannot open <%s> for output", optarg);
				exit(2);
			}
			if( dup2(i, Output ) < 0 ) {
				fperrno("Cannot dup to standard output");
				exit(2);
			}
			swfile = optarg;
			break;
		default:
			usage();
		}
	}
	if(!Option) {
		(void) fprintf(stderr,
		    "Options must include one of -o, -i, or -p\n");
		exit(2);
	}

	if(Option == PASS) {
		if(Rename) {
			(void) fprintf(stderr,
			    "Pass and Rename cannot be used together\n");
			exit(2);
		}
		if( Bufsize != BUFSIZE ) {
			fprintf( stderr, "`B' or `C' option is irrelevant with the '-p' option\n");
			Bufsize = BUFSIZE;
		}

	}else  {
		Cp = Cbuf = (char *)malloc(Bufsize);
		if(Cp == NULL) {
			perror("cpio");
			exit(2);
		}
	}
	argc -= optind;
	argv += optind;

	switch(Option) {
	case OUT:
		if(argc != 0)
			usage();
		/* get filename, copy header and file out */
		while(getname()) {
			if( mklong(Hdr.h_filesize) == 0L) {
				if( Cflag )
					bwrite(Chdr,CHARS+Hdr.h_namesize);
				else
					bwrite(&Hdr, HDRSIZE+Hdr.h_namesize);
				if(Verbose)
					(void) fprintf(stderr, "%s\n",
					    Hdr.h_name);
				continue;
			} else if( A_symlink ) {
				symlsz = (int) mklong(Hdr.h_filesize);
				if (readlink(Hdr.h_name, Symlbuf, symlsz) < 0) {
					fperrno("Cannot read symbolic link <%s>",
					    Hdr.h_name);
					continue;
				}
				Symlbuf[symlsz] = '\0';
				bwrite(&Hdr, HDRSIZE+Hdr.h_namesize);
				bwrite(Symlbuf, symlsz);
				if(Verbose)
					(void) fprintf(stderr, "%s\n",
					    Hdr.h_name);
				continue;
			}
			if((Ifile = open(Hdr.h_name, 0)) < 0) {
				fperrno("Cannot open <%s>", Hdr.h_name);
				continue;
			}
			if ( Cflag )
				bwrite(Chdr,CHARS+Hdr.h_namesize);
			else
				bwrite(&Hdr, HDRSIZE+Hdr.h_namesize);
			for(filesz=mklong(Hdr.h_filesize); filesz>0; filesz-= CPIOBSZ){
				ct = filesz>CPIOBSZ? CPIOBSZ: filesz;
				if(read(Ifile, Buf, ct) < 0) {
 					fperrno("Cannot read %s", Hdr.h_name);
					continue;
				}
				bwrite(Buf,ct);
			}
			close(Ifile);
			if(Acc_time) {
				struct utimbuf utb;

				utb.actime = Statb.st_atime;
				utb.modtime = Statb.st_mtime;
				(void)utime(Hdr.h_name, &utb);
			}
			if(Verbose)
				(void) fprintf(stderr, "%s\n", Hdr.h_name);
		}

	/* copy trailer, after all files have been copied */
		strcpy(Hdr.h_name, "TRAILER!!!");
		Hdr.h_magic = MAGIC;
		MKSHORT(Hdr.h_filesize, 0L);
		Hdr.h_namesize = strlen("TRAILER!!!") + 1;
		if ( Cflag )  {
			bintochar(0L);
			bwrite(Chdr, CHARS+Hdr.h_namesize);
		}
		else
			bwrite(&Hdr, HDRSIZE+Hdr.h_namesize);
		bwrite(Cbuf, Bufsize);
		break;

	case IN:
		if(argc > 0 ) {	/* save patterns, if any */
			Pattern = argv;
		}
		pwd();
		chkhdr();
		while(gethdr()) {
			if (A_symlink) {
				symlsz = (int) mklong(Hdr.h_filesize);
				bread(Symlbuf, symlsz);
				Symlbuf[symlsz] = '\0';
				if((void) ckname(Hdr.h_name)  &&  !Toc)
					(void)openout(Hdr.h_name, Symlbuf);
			} else {
				if( (select = ckname(Hdr.h_name))  &&  !Toc )
					Ofile = openout(Hdr.h_name, (char *)0);
				else
					Ofile = 0;
				for(filesz=mklong(Hdr.h_filesize); filesz>0; filesz-= CPIOBSZ){
					ct = filesz>CPIOBSZ? CPIOBSZ: filesz;
					bread(Buf, ct);
					if(Ofile) {
						if(Swap)
						       swap(Buf,ct,byteswap,halfswap);
						if(write(Ofile, Buf, ct) < 0) {
						 fperrno("Cannot write %s", Hdr.h_name);
						 continue;
						}
					}
				}
				if( Ofile ) {
					(void) close(Ofile);
					if(chmod(Hdr.h_name, Hdr.h_mode) < 0)
						fperrno("Cannot change mode of <%s>",
						    Hdr.h_name);
					set_time(Hdr.h_name, mklong(Hdr.h_mtime), mklong(Hdr.h_mtime));
				}
			}
			if(select) {
				if(Verbose)
					if(Toc)
						pentry(Hdr.h_name);
					else
						puts(Hdr.h_name);
				else if(Toc)
					puts(Hdr.h_name);
			}
		}
		break;

	case PASS:		/* move files around */
		if(argc != 1)
			usage();
		if(access(argv[0], 2) == -1) {
			(void) fperrno("Cannot write in <%s>", argv[0]);
			exit(2);
		}
		strcpy(Fullname, argv[0]);	/* destination directory */
		if(stat(Fullname, &Xstatb) < 0) {
			fperrno("Cannot stat <%s>", Fullname);
			exit(2);
		}
		if((Xstatb.st_mode&S_IFMT) != S_IFDIR) {
			(void) fprintf(stderr, "<%s> is not a directory",
			    Fullname);
			exit(2);
		}
		Dev = Xstatb.st_dev;
		if( Fullname[ strlen(Fullname) - 1 ] != '/' )
			strcat(Fullname, "/");
		fullp = Fullname + strlen(Fullname);

		while(getname()) {
			if (A_directory && !Dir)
				fperr("Use `-d' option to copy <%s>",
					Hdr.h_name);
			if(!ckname(Hdr.h_name))
				continue;
			i = 0;
			while(Hdr.h_name[i] == '/')
				i++;
			strcpy(fullp, &(Hdr.h_name[i]));

			if( PassLink  &&  !A_directory  &&  Dev == Statb.st_dev ) {
				if(link(Hdr.h_name, Fullname) < 0) {
					switch(errno) {
						case ENOENT:
							if(missdir(Fullname) != 0) {
								fperrno("Cannot create directory for <%s>",
									Fullname);
								continue;
							}
							break;
						case EEXIST:
							if(unlink(Fullname) < 0) {
								fperrno("Cannot unlink <%s>",
									Fullname);
								continue;
							}
							break;
						default:
							fperrno("Cannot link <%s> to <%s>",
								Hdr.h_name, Fullname);
							continue;
						}
					if(link(Hdr.h_name, Fullname) < 0) {
						fperrno("Cannot link <%s> to <%s>",
							Hdr.h_name, Fullname);
						continue;
					}
				}

				goto ckverbose;
			}
			if( A_symlink ) {
			   symlsz = (int) mklong(Hdr.h_filesize);
			   if (readlink(Hdr.h_name, Symlbuf, symlsz) < 0) {
				fperrno("Cannot read symbolic link <%s>",
				    Hdr.h_name);
				continue;
			   }
			   Symlbuf[symlsz] = '\0';
			   if(!openout(Fullname, Symlbuf))
				continue;
			   Blocks += ((symlsz + (BUFSIZE - 1)) / BUFSIZE);
			   if(Verbose)
				puts(Fullname);
			   continue;
			}
			if(!(Ofile = openout(Fullname, (char *)0)))
				continue;
			if((Ifile = open(Hdr.h_name, 0)) < 0) {
				fperrno("Cannot open <%s>", Hdr.h_name);
				close(Ofile);
				continue;
			}
			filesz = Statb.st_size;
			for(; filesz > 0; filesz -= CPIOBSZ) {
				ct = filesz>CPIOBSZ? CPIOBSZ: filesz;
				if(read(Ifile, Buf, ct) < 0) {
					fperrno("Cannot read %s", Hdr.h_name);
					break;
				}
				if(write(Ofile, Buf, ct) < 0) {
				 fperrno("Cannot write %s", Hdr.h_name);
				 break;
				}
				/* Removed u370 ifdef which caused cpio */
				/* to report blocks in terms of 4096 bytes. */

				Blocks += ((ct + (BUFSIZE - 1)) / BUFSIZE);
			}
			close(Ifile);
			if(Acc_time) {
				struct utimbuf utb;

				utb.actime = Statb.st_atime;
				utb.modtime = Statb.st_mtime;
				(void)utime(Hdr.h_name, &utb);
			}
			if(Ofile) {
				close(Ofile);
				if(chmod(Fullname, Hdr.h_mode) < 0)
					fperrno("Cannot change mode of <%s>",
					    Fullname);
				set_time(Fullname, Statb.st_atime, mklong(Hdr.h_mtime));
ckverbose:
				if(Verbose)
					puts(Fullname);
			}
		}
	}
	/* print number of blocks actually copied */
	Blocks += ((sBlocks + (BUFSIZE - 1)) / BUFSIZE);
	(void) fprintf(stderr, "%ld blocks\n", Blocks * (Bufsize>>9));
	exit(0);
}

static
usage()
{
	(void) fprintf("Usage: %s\n     %s\n     %s\n     %s\n       %s\n",
	    "cpio -o[acvB] <name-list >collection",
	    "cpio -o[acvB] -Ocollection <name-list",
	    "cpio -i[cdmrstuvfB6] [ pattern ... ] <collection",
	    "cpio -i[cdmrstuvfB6] -Icollection [ pattern ... ]",
	    "cpio -p[adlmruv] directory <name-list");
}

static
chkswfile( sp, c, option )
char	*sp;
char	c;
short	option;
{
	if( !option ) {
		fperr( "-%c must be specified before -%c option",
			c == 'I' ? 'i' : 'o', c );
		exit(2);
	}
	if( (c == 'I'  &&  option != IN)  ||  (c == 'O'  &&  option != OUT) ) {
		fperr( "-%c option not permitted with -%c option", c,
			option );
		exit(2);
	}
	if( !sp )
		return;
	fperr("No more than one -I or -O flag permitted");
	exit(2);
}

static
getname()		/* get file name, get info for header */
{
	register char *namep = Name;
	register ushort ftype;
	struct stat Lstatb;
	long tlong;

	for(;;) {
		if(gets(namep) == NULL)
			return 0;
		while(*namep == '.' && namep[1] == '/') {
			namep++;
			while(*namep == '/') namep++;
		}
		strcpy(Hdr.h_name, namep);
		if(lstat(namep, &Statb) < 0) {
			fperrno("Cannot stat <%s>", Hdr.h_name);
			continue;
		}
		ftype = Statb.st_mode & Filetype;
		A_directory = (ftype == S_IFDIR);
		A_special = (ftype == S_IFBLK)
			|| (ftype == S_IFCHR)
			|| (ftype == S_IFIFO);
		A_symlink = (ftype == S_IFLNK);
		Hdr.h_magic = MAGIC;
		Hdr.h_namesize = strlen(Hdr.h_name) + 1;
		Hdr.h_uid = Statb.st_uid;
		Hdr.h_gid = Statb.st_gid;
		Hdr.h_dev = Statb.st_dev;
		Hdr.h_ino = Statb.st_ino;
		Hdr.h_mode = Statb.st_mode;
		MKSHORT(Hdr.h_mtime, Statb.st_mtime);
		Hdr.h_nlink = Statb.st_nlink;
		tlong = ((Hdr.h_mode&S_IFMT) == S_IFREG ||
			(Hdr.h_mode&S_IFMT) == S_IFLNK)? Statb.st_size: 0L;
		MKSHORT(Hdr.h_filesize, tlong);
		Hdr.h_rdev = Statb.st_rdev;
		if( Cflag )
			bintochar(tlong);
		return 1;
	}
}

static
bintochar(t)		/* ASCII header write */
long t;
{
	sprintf(Chdr,"%.6o%.6ho%.6ho%.6ho%.6ho%.6ho%.6ho%.6ho%.11lo%.6ho%.11lo%s",
		MAGIC, MK_USHORT(Statb.st_dev), MK_USHORT(Statb.st_ino), Statb.st_mode, Statb.st_uid,
		Statb.st_gid, Statb.st_nlink, MK_USHORT(Statb.st_rdev),
		Statb.st_mtime, (short)strlen(Hdr.h_name)+1, t, Hdr.h_name);
}

static
chartobin()		/* ASCII header read */
{
	sscanf(Chdr, "%6ho%6ho%6ho%6ho%6ho%6ho%6ho%6ho%11lo%6ho%11lo",
		&Hdr.h_magic, &Hdr.h_dev, &Hdr.h_ino, &Hdr.h_mode, &Hdr.h_uid,
		&Hdr.h_gid, &Hdr.h_nlink, &Hdr.h_rdev, &Longtime,
		&Hdr.h_namesize, &Longfile);
	MKSHORT(Hdr.h_filesize, Longfile);
	MKSHORT(Hdr.h_mtime, Longtime);
}


/*	Check the header for the magic number.  Switch modes automatically to
	match the type of header found.
*/
static
chkhdr()
{
	bread(Chdr, CHARS);
	chartobin();
	if( Hdr.h_magic == MAGIC )
		Cflag = 1;
	else {
		breread(&Hdr.h_magic, sizeof Hdr.h_magic);
		if( Hdr.h_magic == MAGIC || Hdr.h_magic == (short)BSMAGIC )
			Cflag = 0;
		else {
			fperr("This is not a cpio file.  Bad magic number.");
			exit(2);
		}
	}
	breread(Chdr, 0);
}


static
gethdr()		/* get file headers */
{
	register ushort ftype;

	if (Cflag)  {
		bread(Chdr, CHARS);
		chartobin();
	}
	else
		bread(&Hdr, HDRSIZE);

	if(Hdr.h_magic == (short)BSMAGIC)
		swap((char *)&Hdr, HDRSIZE, 1, 0);
	else if( Hdr.h_magic != MAGIC ) {
		fperr("Out of phase--get help");
		exit(2);
	}
	bread(Hdr.h_name, Hdr.h_namesize);
	if(EQ(Hdr.h_name, "TRAILER!!!"))
		return 0;
	ftype = Hdr.h_mode & Filetype;
	A_directory = (ftype == S_IFDIR);
	A_special = (ftype == S_IFBLK)
		||  (ftype == S_IFCHR)
		||  (ftype == S_IFIFO);
	A_symlink = (ftype == S_IFLNK);
	return 1;
}

static
ckname(namep)	/* check filenames with patterns given on cmd line */
register char *namep;
{
	char	buf[sizeof Hdr.h_name];

	if(fflag ^ !nmatch(namep, Pattern)) {
		return 0;
	}
	if(Rename && !A_directory) {	/* rename interactively */
		fprintf(Wtty, "Rename <%s>\n", namep);
		fflush(Wtty);
		fgets(buf, sizeof buf, Rtty);
		if(feof(Rtty))
			exit(2);
		buf[strlen(buf) - 1] = '\0';
		if(EQ(buf, "")) {
			strcpy(namep,buf);
			printf("Skipped\n");
			return 0;
		}
		else if(EQ(buf, "."))
			printf("Same name\n");
		else
			strcpy(namep,buf);
	}
	return  1;
}

static
openout(namep, symlname)	/* open files for writing, set all necessary info */
register char *namep;
char *symlname;
{
	register f;
	register char *np;
	int ans;

	if(!strncmp(namep, "./", 2))
		namep += 2;
	np = namep;
	if(A_directory) {
		if( !Dir  ||  Rename  ||  EQ(namep, ".")  ||  EQ(namep, "..") )
			/* do not consider . or .. files */
			return 0;
		if(stat(namep, &Xstatb) == -1) {

/* try creating (only twice) */
			ans = 0;
			do {
				if(mkdir(namep, Hdr.h_mode) != 0) {
					ans += 1;
				}else {
					ans = 0;
					break;
				}
			}while(ans < 2 && missdir(namep) == 0);
			if(ans == 1) {
				fperrno("Cannot create directory for <%s>",
					namep);
				return(0);
			}else if(ans == 2) {
				fperrno("Cannot create directory <%s>", namep);
				return(0);
			}
		}

ret:
		if(chmod(namep, Hdr.h_mode) < 0)
			fperrno("Cannot change mode of <%s>", namep);
		if(Uid == 0)
			if(chown(namep, Hdr.h_uid, Hdr.h_gid) < 0)
				fperrno("Cannot change ownership of <%s>",
				    namep);
		set_time(namep, mklong(Hdr.h_mtime), mklong(Hdr.h_mtime));
		return 0;
	}
	if(Hdr.h_nlink > 1)
		if(!postml(namep, np))
			return 0;
	if(lstat(namep, &Xstatb) == 0) {
		if(Uncond && !((!(Xstatb.st_mode & S_IWRITE) || A_special) && (Uid != 0))) {
			if(unlink(namep) < 0) {
				fperrno("cannot unlink current <%s>", namep);
			}
		}
		if(!Uncond && (mklong(Hdr.h_mtime) <= Xstatb.st_mtime)) {
		/* There's a newer or same aged version of file on destination */
			fperr("current <%s> newer or same age", np);
			return 0;
		}
	}
	if( Option == PASS
		&& Hdr.h_ino == Xstatb.st_ino
		&& Hdr.h_dev == Xstatb.st_dev) {
		fperr("Attempt to pass file to self!");
		exit(2);
	}
	if(A_symlink) {
/* try symlinking (only twice) */
		ans = 0;
		do {
			if(symlink(
symlname, namep) < 0) {
				ans += 1;
			}else {
				ans = 0;
				break;
			}
		}while(ans < 2 && missdir(np) == 0);
		if(ans == 1) {
			fperrno("Cannot create directory for <%s>", namep);
			return(0);
		}else if(ans == 2) {
			fperrno("Cannot symlink <%s> and <%s>", namep, symlname);
			return(0);
		}

		return 0;
	}
	if(A_special) {
		if((Hdr.h_mode & Filetype) == S_IFIFO)
			Hdr.h_rdev = 0;

/* try creating (only twice) */
		ans = 0;
		do {
			if(mknod(namep, Hdr.h_mode, Hdr.h_rdev) < 0) {
				ans += 1;
			}else {
				ans = 0;
				break;
			}
		}while(ans < 2 && missdir(np) == 0);
		if(ans == 1) {
			fperrno("Cannot create directory for <%s>", namep);
			return(0);
		}else if(ans == 2) {
			fperrno("Cannot mknod <%s>", namep);
			return(0);
		}

		goto ret;
	}

/* try creating (only twice) */
	ans = 0;
	do {
		if((f = creat(namep, Hdr.h_mode)) < 0) {
			ans += 1;
		}else {
			ans = 0;
			break;
		}
	}while(ans < 2 && missdir(np) == 0);
	if(ans == 1) {
		fperrno("Cannot create directory for <%s>", namep);
		return(0);
	}else if(ans == 2) {
		fperrno("Cannot create <%s>", namep);
		return(0);
	}

	if(Uid == 0)
		if(chown(namep, Hdr.h_uid, Hdr.h_gid) < 0)
			fperrno("Cannot change ownership of <%s>", namep);
	return f;
}


/*	Shared by bread() and breread()
*/
static int	nleft = 0;	/* unread chars left in Cbuf */
static char	*ip;		/* pointer to next char to be read from Cbuf */

/*	Reread the current buffer Cbuf.
	A character count, c, of 0 simply resets the pointer so next bread gets
	the same data again.
*/
static
breread(b, c)
char	*b;
int	c;
{
	ip = Cbuf;
	if( nleft )
		nleft = Bufsize;
	if( !c )
		return;
	bread(b, c);
}

static
bread(b, c)
register char	*b;
register int	c;
{
	register int	rv;
	register char	*p = ip;

	if( !Cflag ) {
		/* round c up to an even number */
		c = (c+1)/2;
		c *= 2;
	}
	while( c )  {
		if( nleft == 0 ) {
			while( (rv = read(Input, Cbuf, Bufsize)) == 0 ) {
				Input = chgreel(0, Input, rv);
			}
			if( rv == Bufsize ) {
				nleft = Bufsize;
				p = Cbuf;
				++Blocks;
			}
			else if( rv == -1 ) {
				fperrno("Read error on archive");
				exit(2);
			}
			else if( rv < Bufsize ) {	/* short read */
				smemcpy( &Cbuf[ Bufsize - rv ], Cbuf, rv );
				nleft = rv;
				p = &Cbuf[ Bufsize - rv ];
				sBlocks += rv;
			}
		}
		if( nleft <= c ) {
			memcpy( b, p, nleft );
			c -= nleft;
			b += nleft;
			p += nleft;
			nleft = 0;
		}
		else {
			memcpy( b, p, c );
			nleft -= c;
			b += c;
			p += c;
			c = 0;
		}
	}
	ip = p;
}


static
bwrite(rp, c)
register char *rp;
register c;
{
	register char	*cp = Cp;
	static unsigned	Ccnt = 0;
	register unsigned Cleft;
	register int	rv;

	if( !Cflag ) {
		/* round c up to an even number */
		c = (c+1)/2;
		c *= 2;
	}
	while( c )  {
		if( (Cleft = Bufsize - Ccnt) <= c ) {
			memcpy( cp, rp, Cleft );
			rv = write(Output, Cbuf, Bufsize);
			if( rv == 0  ||  ( rv == -1  &&  errno == ENXIO ) ) {
				rv = eomchgreel();
			}
			if( rv == Bufsize ) {
				Ccnt = 0;
				cp = Cbuf;
			}
			else if( rv == -1 ) {
				fperrno("Write error on archive");
				exit(2);
			}
			else if( rv < Bufsize ) {
				Output = chgreel(1, Output, 0);
				smemcpy( Cbuf, &Cbuf[ Bufsize - rv ], rv );
				Ccnt = Bufsize - rv;
				cp = &Cbuf[ rv ];
			}
			++Blocks;
			rp += Cleft;
			c -= Cleft;
		}
		else {
			memcpy( cp, rp, c );
			Ccnt += c;
			cp += c;
			rp += c;
			c = 0;
		}
	}
	Cp = cp;
}


static int	reelcount = 1;	/* used below and in chgreel() */

/*	Change reel due to reaching end-of-media.
	Keep trying to get a successful write before considering the
	change-of-reel as successful.
*/
static
int
eomchgreel()
{
	int	rv;

	while( 1 ) {
		Output = chgreel(1, Output, 0);
		rv = write(Output, Cbuf, Bufsize);
		if( rv == Bufsize )
			return  rv;
		if( rv == -1 )
			fperrno( "Unable to write this medium" );
		else
			fperr( "Unable to write this medium: Premature EOF" );
		(void) fprintf(stderr, "Try again.\n");
		reelcount--;
	}
	/*NOTREACHED*/
}


static
postml(namep, np)		/* linking funtion:  Postml() is called after */
register char *namep, *np;	/* namep is created.  Postml() checks to see  */
{				/* if namep should be linked to np.  If so,   */
				/* postml() removes the independent instance  */
	register i;		/* of namep and links namep to np.	      */
	static struct ml {
		short	m_dev;
		ushort	m_ino;
		char	m_name[2];
	} **ml = 0;
	register struct ml	*mlp;
	static unsigned	mlsize = 0;
	static unsigned	mlinks = 0;
	char		*lnamep;
	int		ans;

	if( !ml ) {
		mlsize = LINKS;
		ml = (struct ml **) malloc(mlsize * sizeof(struct ml));
	}
	else if( mlinks == mlsize ) {
		mlsize += LINKS;
		ml = (struct ml **) realloc((char *) ml,
		    mlsize * sizeof(struct ml));
	}
	if (ml == NULL) {
		fperr("Out of memory for links");
		exit(2);
	}
	for(i = 0; i < mlinks; ++i) {
		mlp = ml[i];
		if(mlp->m_ino==Hdr.h_ino  &&  mlp->m_dev==Hdr.h_dev) {
			if(Verbose)
			  printf("%s linked to %s\n", ml[i]->m_name,
				np);
			unlink(namep);
			if(Option == IN && *(mlp->m_name) != '/') {
				Fullname[Pathend] = '\0';
				strcat(Fullname, mlp->m_name);
				lnamep = Fullname;
			}
			lnamep = mlp->m_name;

/* try linking (only twice) */
			ans = 0;
			do {
				if(link(lnamep, namep) < 0) {
					ans += 1;
				}else {
					ans = 0;
					break;
				}
			}while(ans < 2 && missdir(np) == 0);
			if(ans == 1) {
				fperrno("Cannot create directory for <%s>", np);
				return(0);
			}else if(ans == 2) {
				fperrno("Cannot link <%s> & <%s>", lnamep, np);
				return(0);
			}

			set_time(namep, mklong(Hdr.h_mtime), mklong(Hdr.h_mtime));
			return 0;
		}
	}
	if( !(ml[mlinks] = (struct ml *)malloc(strlen(np) + 2 + sizeof(struct ml)))) {
		static int first=1;

		if(first)
			fperr("Out of memory for links");
		first = 0;
		return 1;
	}
	ml[mlinks]->m_dev = Hdr.h_dev;
	ml[mlinks]->m_ino = Hdr.h_ino;
	strcpy(ml[mlinks]->m_name, np);
	++mlinks;
	return 1;
}

static
pentry(namep)		/* print verbose table of contents */
register char *namep;
{

	static short lastid = -1;
#include <pwd.h>
	static struct passwd *pw;
	struct passwd *getpwuid();
	static char tbuf[32];
	char *ctime();

	printf("%-7o", MK_USHORT(Hdr.h_mode));
	if(lastid == Hdr.h_uid)
		printf("%-6s", pw->pw_name);
	else {
		setpwent();
		if(pw = getpwuid((int)Hdr.h_uid)) {
			printf("%-6s", pw->pw_name);
			lastid = Hdr.h_uid;
		} else {
			printf("%-6d", Hdr.h_uid);
			lastid = -1;
		}
	}
	printf("%7ld ", mklong(Hdr.h_filesize));
	U.l = mklong(Hdr.h_mtime);
	strcpy(tbuf, ctime((long *)&U.l));
	tbuf[24] = '\0';
	printf(" %s  %s", &tbuf[4], namep);
	if (A_symlink)
		printf(" -> %s", Symlbuf);
	putchar('\n');
}

		/* pattern matching functions */
static
nmatch(s, pat)
char *s, **pat;
{
	if( !pat )
		return 1;
	while(*pat) {
		if((**pat == '!' && !gmatch(s, *pat+1))
		|| gmatch(s, *pat))
			return 1;
		++pat;
	}
	return 0;
}


static
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
				ok |= ((lc <= scc) && (scc <= (cc=p[1])));
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



static
umatch(s, p)
register char *s, *p;
{
	if(*p==0) return(1);
	while(*s)
		if (gmatch(s++,p)) return(1);
	return(0);
}

swap(buf, bytecount, bytes, halfwords)	/* swap halfwords, bytes or both */
char *buf;
int bytecount;
int bytes, halfwords;
{
	register int count;
	int n, i;

	if(bytes) {
		register union swpbytes {
			short	shortw;
			char	charv[2];
		} *pbuf;
		register char c;

		count = bytecount;
		pbuf = (union swpbytes *)buf;
		if (count % sizeof(union swpbytes))
			pbuf->charv[count] = 0;
		count = (count + (sizeof(union swpbytes) - 1)) / sizeof(union swpbytes);
		while (count--) {
			c = pbuf->charv[0];
			pbuf->charv[0] = pbuf->charv[1];
			pbuf->charv[1] = c;
			++pbuf;
		}
	}
	if (halfwords) {
		register union swphalf {
			long	longw;
			short	shortv[2];
			char	charv[4];
		} *pbuf;
		register short cc;

		count = bytecount;
		pbuf = (union swphalf *)buf;
		if (n = count % sizeof(union swphalf))
			if(bytes && n % 2)
				for(i = count + 1; i <= count + (sizeof(union swphalf) - n); i++)
					pbuf->charv[i] = 0;
			else
				for (i = count; i < count + (sizeof(union swphalf) - n); i++)
					pbuf->charv[i] = 0;
		count = (count + (sizeof(union swphalf) - 1)) / sizeof(union swphalf);
		while (count--) {
			cc = pbuf->shortv[0];
			pbuf->shortv[0] = pbuf->shortv[1];
			pbuf->shortv[1] = cc;
			++pbuf;
		}
	}
}


static
set_time(namep, atime, mtime)	/* set access and modification times */
register char *namep;
time_t atime, mtime;
{
	static struct utimbuf timevec;

	if(!Mod_time)
		return;
	timevec.actime = atime;
	timevec.modtime = mtime;
	(void)utime(namep, &timevec);
}



static
chgreel(x, fl, rv)
{
	register f;
	char str[BUFSIZ];
	struct stat statb;

	fstat(fl, &statb);
	if((statb.st_mode&S_IFMT) != S_IFCHR) {
		fperrno("Can't %s: ", x? "write output": "read input");
		exit(2);
	}
	if( rv == 0  ||
		( rv == -1  &&  ( errno == ENOSPC  ||  errno == ENXIO ) ) )
		fperr( "\007Reached end of medium on %s",
			x? "output":"input" );
	else {
		fperrno( "\007Encountered an error on %s",
			x? "output":"input" );
		exit(2);
	}
	if( Rtty == NULL ) {
		Rtty = fopen(ttyname, "r");
		if( Rtty == NULL ) {
			fperrno("Cannot prompt (can't open %s)", ttyname);
			exit(2);
		}
	}
	close(fl);
	reelcount++;
again:
	if( swfile ) {
	    askagain:
		fperr( eommsg, reelcount );
		fgets(str, sizeof str, Rtty);
		switch( *str ) {
		case '\n':
			strcpy( str, swfile );
			break;
		case 'q':
			exit(2);
		default:
			goto askagain;
		}
	}
	else {
		fperr("If you want to go on, type device/file name when ready.");
		fgets(str, sizeof str, Rtty);
		str[strlen(str) - 1] = '\0';
		if(!*str)
			exit(2);
	}
	if((f = open(str, x? 1: 0)) < 0) {
		fperrno("Can't open <%s>", str);
		goto again;
	}
	return f;
}



static
missdir(namep)
register char *namep;
{
	register char *np;
	register ct = 2;

	for(np = namep; *np; ++np)
		if(*np == '/') {
			if(np == namep) continue;	/* skip over 'root slash' */
			*np = '\0';
			if(stat(namep, &Xstatb) == -1) {
				if(Dir) {
					if((ct = mkdir(namep, 0777)) != 0) {
						*np = '/';
						return(ct);
					}
				}else {
					fperr("missing 'd' option");
					return(-1);
				}
			}
			*np = '/';
		}
	if (ct == 2) ct = 0;		/* the file already exists */
	return ct;
}



static
pwd()		/* get working directory */
{
	if (getwd(Fullname) == 0) {
		(void)fprintf(stderr, "cpio: %s\n",
		    Fullname);
		exit(2);
	}
	Pathend = strlen(Fullname);
	Fullname[Pathend++] = '/';
	Fullname[Pathend] = '\0';
}


/*
	print message on the stderr
*/
static
fperr( va_alist )
va_dcl
{
	va_list	args;
	char	*fmt;

	va_start( args );
	fprintf( stderr, "cpio: ");
	fmt = va_arg( args, char * );
	vfprintf( stderr, fmt, args );
	putc( '\n', stderr);
	fflush( stderr );
}

/*
	print message on the stderr followed by error number and meaning.
*/
static
fperrno( va_alist )
va_dcl
{
	va_list	args;
	char	*fmt;

	va_start( args );
	fprintf( stderr, "cpio: ");
	fmt = va_arg( args, char * );
	vfprintf( stderr, fmt, args );
	fprintf( stderr, ": " );
	fflush( stderr );
	perror("");
}


/*	Safe memory copy.
	Fast if the to and from strings do not overlap,
	slower but safe if they do.
*/

static char *
smemcpy( to, from, count )
register char		*to, *from;
register unsigned	count;
{
	char	*savedto;

	if( &to[ count ] <= from  ||  &from[ count ] <= to )
		return  memcpy( to, from, count );

	if( to == from )
		return  to;

	savedto = to;
	if( to < from )
		while( count-- )
			*(to++) = *(from++);
	else {
		to += count;
		from += count;
		while( count-- )
			*(--to) = *(--from);
	}

	return  savedto;
}
