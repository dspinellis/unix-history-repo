/* Copyright (c) 1981 Regents of the University of California */
static char *sccsid = "@(#)ex_io.c	5.6.1.1	%G%";
#include "ex.h"
#include "ex_argv.h"
#include "ex_temp.h"
#include "ex_tty.h"
#include "ex_vis.h"

/*
 * File input/output, source, preserve and recover
 */

/*
 * Following remember where . was in the previous file for return
 * on file switching.
 */
int	altdot;
int	oldadot;
bool	wasalt;
short	isalt;

long	cntch;			/* Count of characters on unit io */
#ifndef VMUNIX
short	cntln;			/* Count of lines " */
#else
int	cntln;
#endif
long	cntnull;		/* Count of nulls " */
long	cntodd;			/* Count of non-ascii characters " */

/*
 * Parse file name for command encoded by comm.
 * If comm is E then command is doomed and we are
 * parsing just so user won't have to retype the name.
 */
filename(comm)
	int comm;
{
	register int c = comm, d;
	register int i;

	d = getchar();
	if (endcmd(d)) {
		if (savedfile[0] == 0 && comm != 'f')
			error("No file|No current filename");
		CP(file, savedfile);
		wasalt = (isalt > 0) ? isalt-1 : 0;
		isalt = 0;
		oldadot = altdot;
		if (c == 'e' || c == 'E')
			altdot = lineDOT();
		if (d == EOF)
			ungetchar(d);
	} else {
		ungetchar(d);
		getone();
		eol();
		if (savedfile[0] == 0 && c != 'E' && c != 'e') {
			c = 'e';
			edited = 0;
		}
		wasalt = strcmp(file, altfile) == 0;
		oldadot = altdot;
		switch (c) {

		case 'f':
			edited = 0;
			/* fall into ... */

		case 'e':
			if (savedfile[0]) {
				altdot = lineDOT();
				CP(altfile, savedfile);
			}
			CP(savedfile, file);
			break;

		default:
			if (file[0]) {
				if (c != 'E')
					altdot = lineDOT();
				CP(altfile, file);
			}
			break;
		}
	}
	if (hush && comm != 'f' || comm == 'E')
		return;
	if (file[0] != 0) {
		lprintf("\"%s\"", file);
		if (comm == 'f') {
			if (value(READONLY))
				printf(" [Read only]");
			if (!edited)
				printf(" [Not edited]");
			if (tchng)
				printf(" [Modified]");
		}
		flush();
	} else
		printf("No file ");
	if (comm == 'f') {
		if (!(i = lineDOL()))
			i++;
		printf(" line %d of %d --%ld%%--", lineDOT(), lineDOL(),
		    (long) 100 * lineDOT() / i);
	}
}

/*
 * Get the argument words for a command into genbuf
 * expanding # and %.
 */
getargs()
{
	register int c;
	register char *cp, *fp;
	static char fpatbuf[32];	/* hence limit on :next +/pat */

	pastwh();
	if (peekchar() == '+') {
		for (cp = fpatbuf;;) {
			c = *cp++ = getchar();
			if (cp >= &fpatbuf[sizeof(fpatbuf)])
				error("Pattern too long");
			if (c == '\\' && isspace(peekchar()))
				c = getchar();
			if (c == EOF || isspace(c)) {
				ungetchar(c);
				*--cp = 0;
				firstpat = &fpatbuf[1];
				break;
			}
		}
	}
	if (skipend())
		return (0);
	CP(genbuf, "echo "); cp = &genbuf[5];
	for (;;) {
		c = getchar();
		if (endcmd(c)) {
			ungetchar(c);
			break;
		}
		switch (c) {

		case '\\':
			if (any(peekchar(), "#%|"))
				c = getchar();
			/* fall into... */

		default:
			if (cp > &genbuf[LBSIZE - 2])
flong:
				error("Argument buffer overflow");
			*cp++ = c;
			break;

		case '#':
			fp = altfile;
			if (*fp == 0)
				error("No alternate filename@to substitute for #");
			goto filexp;

		case '%':
			fp = savedfile;
			if (*fp == 0)
				error("No current filename@to substitute for %%");
filexp:
			while (*fp) {
				if (cp > &genbuf[LBSIZE - 2])
					goto flong;
				*cp++ = *fp++;
			}
			break;
		}
	}
	*cp = 0;
	return (1);
}

/*
 * Glob the argument words in genbuf, or if no globbing
 * is implied, just split them up directly.
 */
glob(gp)
	struct glob *gp;
{
	int pvec[2];
	register char **argv = gp->argv;
	register char *cp = gp->argspac;
	register int c;
	char ch;
	int nleft = NCARGS;

	gp->argc0 = 0;
	if (gscan() == 0) {
		register char *v = genbuf + 5;		/* strlen("echo ") */

		for (;;) {
			while (isspace(*v))
				v++;
			if (!*v)
				break;
			*argv++ = cp;
			while (*v && !isspace(*v))
				*cp++ = *v++;
			*cp++ = 0;
			gp->argc0++;
		}
		*argv = 0;
		return;
	}
	if (pipe(pvec) < 0)
		error("Can't make pipe to glob");
	pid = fork();
	io = pvec[0];
	if (pid < 0) {
		close(pvec[1]);
		error("Can't fork to do glob");
	}
	if (pid == 0) {
		int oerrno;

		close(1);
		dup(pvec[1]);
		close(pvec[0]);
		close(2);	/* so errors don't mess up the screen */
		open("/dev/null", 1);
		execl(svalue(SHELL), "sh", "-c", genbuf, 0);
		oerrno = errno; close(1); dup(2); errno = oerrno;
		filioerr(svalue(SHELL));
	}
	close(pvec[1]);
	do {
		*argv = cp;
		for (;;) {
			if (read(io, &ch, 1) != 1) {
				close(io);
				c = -1;
			} else
				c = ch & TRIM;
			if (c <= 0 || isspace(c))
				break;
			*cp++ = c;
			if (--nleft <= 0)
				error("Arg list too long");
		}
		if (cp != *argv) {
			--nleft;
			*cp++ = 0;
			gp->argc0++;
			if (gp->argc0 >= NARGS)
				error("Arg list too long");
			argv++;
		}
	} while (c >= 0);
	waitfor();
	if (gp->argc0 == 0)
		error("No match");
}

/*
 * Scan genbuf for shell metacharacters.
 * Set is union of v7 shell and csh metas.
 */
gscan()
{
	register char *cp;

	for (cp = genbuf; *cp; cp++)
		if (any(*cp, "~{[*?$`'\"\\"))
			return (1);
	return (0);
}

/*
 * Parse one filename into file.
 */
struct glob G;
getone()
{
	register char *str;

	if (getargs() == 0)
		error("Missing filename");
	glob(&G);
	if (G.argc0 > 1)
		error("Ambiguous|Too many file names");
	str = G.argv[G.argc0 - 1];
	if (strlen(str) > FNSIZE - 4)
		error("Filename too long");
samef:
	CP(file, str);
}

/*
 * Read a file from the world.
 * C is command, 'e' if this really an edit (or a recover).
 */
rop(c)
	int c;
{
	register int i;
	struct stat stbuf;
	short magic;
	static int ovro;	/* old value(READONLY) */
	static int denied;	/* 1 if READONLY was set due to file permissions */

	io = open(file, 0);
	if (io < 0) {
		if (c == 'e' && errno == ENOENT) {
			edited++;
			/*
			 * If the user just did "ex foo" he is probably
			 * creating a new file.  Don't be an error, since
			 * this is ugly, and it screws up the + option.
			 */
			if (!seenprompt) {
				printf(" [New file]");
				noonl();
				return;
			}
		}
		syserror();
	}
	if (fstat(io, &stbuf))
		syserror();
	switch (stbuf.st_mode & S_IFMT) {

	case S_IFBLK:
		error(" Block special file");

	case S_IFCHR:
		if (isatty(io))
			error(" Teletype");
		if (samei(&stbuf, "/dev/null"))
			break;
		error(" Character special file");

	case S_IFDIR:
		error(" Directory");

	case S_IFREG:
#ifdef CRYPT
		if (xflag)
			break;
#endif
		i = read(io, (char *) &magic, sizeof(magic));
		lseek(io, 0l, 0);
		if (i != sizeof(magic))
			break;
		switch (magic) {

		case 0405:	/* data overlay on exec */
		case 0407:	/* unshared */
		case 0410:	/* shared text */
		case 0411:	/* separate I/D */
		case 0413:	/* VM/Unix demand paged */
		case 0430:	/* PDP-11 Overlay shared */
		case 0431:	/* PDP-11 Overlay sep I/D */
			error(" Executable");

		/*
		 * We do not forbid the editing of portable archives
		 * because it is reasonable to edit them, especially
		 * if they are archives of text files.  This is
		 * especially useful if you archive source files together
		 * and copy them to another system with ~%take, since
		 * the files sometimes show up munged and must be fixed.
		 */
		case 0177545:
		case 0177555:
			error(" Archive");

		default:
#ifdef mbb
			/* C/70 has a 10 bit byte */
			if (magic & 03401600)
#else
			/* Everybody else has an 8 bit byte */
			if (magic & 0100200)
#endif
				error(" Non-ascii file");
			break;
		}
	}
	if (c != 'r') {
		if (value(READONLY) && denied) {
			value(READONLY) = ovro;
			denied = 0;
		}
		if ((stbuf.st_mode & 0222) == 0 || access(file, 2) < 0) {
			ovro = value(READONLY);
			denied = 1;
			value(READONLY) = 1;
		}
	}
	if (value(READONLY)) {
		printf(" [Read only]");
		flush();
	}
	if (c == 'r')
		setdot();
	else
		setall();
	if (FIXUNDO && inopen && c == 'r')
		undap1 = undap2 = dot + 1;
	rop2();
	rop3(c);
}

rop2()
{
	line *first, *last, *a;
	struct stat statb;

	deletenone();
	clrstats();
	first = addr2 + 1;
	if (fstat(io, &statb) < 0)
		bsize = LBSIZE;
	else {
		bsize = statb.st_blksize;
		if (bsize <= 0)
			bsize = LBSIZE;
	}
	ignore(append(getfile, addr2));
	last = dot;
	/*
	 *	if the modeline variable is set,
	 *	check the first and last five lines of the file
	 *	for a mode line.
	 */
	if (value(MODELINE)) {
		for (a=first; a<=last; a++) {
			if (a==first+5 && last-first > 10)
				a = last - 4;
			getline(*a);
			checkmodeline(linebuf);
		}
	}
}

rop3(c)
	int c;
{

	if (iostats() == 0 && c == 'e')
		edited++;
	if (c == 'e') {
		if (wasalt || firstpat) {
			register line *addr = zero + oldadot;

			if (addr > dol)
				addr = dol;
			if (firstpat) {
				globp = (*firstpat) ? firstpat : "$";
				commands(1,1);
				firstpat = 0;
			} else if (addr >= one) {
				if (inopen)
					dot = addr;
				markpr(addr);
			} else
				goto other;
		} else
other:
			if (dol > zero) {
				if (inopen)
					dot = one;
				markpr(one);
			}
		if(FIXUNDO)
			undkind = UNDNONE;
		if (inopen) {
			vcline = 0;
			vreplace(0, LINES, lineDOL());
		}
	}
}

/*
 * Are these two really the same inode?
 */
samei(sp, cp)
	struct stat *sp;
	char *cp;
{
	struct stat stb;

	if (stat(cp, &stb) < 0 || sp->st_dev != stb.st_dev)
		return (0);
	return (sp->st_ino == stb.st_ino);
}

/* Returns from edited() */
#define	EDF	0		/* Edited file */
#define	NOTEDF	-1		/* Not edited file */
#define	PARTBUF	1		/* Write of partial buffer to Edited file */

/*
 * Write a file.
 */
wop(dofname)
bool dofname;	/* if 1 call filename, else use savedfile */
{
	register int c, exclam, nonexist;
	line *saddr1, *saddr2;
	struct stat stbuf;

	c = 0;
	exclam = 0;
	if (dofname) {
		if (peekchar() == '!')
			exclam++, ignchar();
		ignore(skipwh());
		while (peekchar() == '>')
			ignchar(), c++, ignore(skipwh());
		if (c != 0 && c != 2)
			error("Write forms are 'w' and 'w>>'");
		filename('w');
	} else {
		if (savedfile[0] == 0)
			error("No file|No current filename");
		saddr1=addr1;
		saddr2=addr2;
		addr1=one;
		addr2=dol;
		CP(file, savedfile);
		if (inopen) {
			vclrech(0);
			splitw++;
		}
		lprintf("\"%s\"", file);
	}
	nonexist = stat(file, &stbuf);
	switch (c) {

	case 0:
		if (!exclam && (!value(WRITEANY) || value(READONLY)))
		switch (edfile()) {
		
		case NOTEDF:
			if (nonexist)
				break;
			if ((stbuf.st_mode & S_IFMT) == S_IFCHR) {
				if (samei(&stbuf, "/dev/null"))
					break;
				if (samei(&stbuf, "/dev/tty"))
					break;
			}
			io = open(file, 1);
			if (io < 0)
				syserror();
			if (!isatty(io))
				serror(" File exists| File exists - use \"w! %s\" to overwrite", file);
			close(io);
			break;

		case EDF:
			if (value(READONLY))
				error(" File is read only");
			break;

		case PARTBUF:
			if (value(READONLY))
				error(" File is read only");
			error(" Use \"w!\" to write partial buffer");
		}
cre:
/*
		synctmp();
*/
#ifdef V6
		io = creat(file, 0644);
#else
		io = creat(file, 0666);
#endif
		if (io < 0)
			syserror();
		writing = 1;
		if (hush == 0)
			if (nonexist)
				printf(" [New file]");
			else if (value(WRITEANY) && edfile() != EDF)
				printf(" [Existing file]");
		break;

	case 2:
		io = open(file, 1);
		if (io < 0) {
			if (exclam || value(WRITEANY))
				goto cre;
			syserror();
		}
		lseek(io, 0l, 2);
		break;
	}
	putfile(0);
	ignore(iostats());
	if (c != 2 && addr1 == one && addr2 == dol) {
		if (eq(file, savedfile))
			edited = 1;
		sync();
	}
	if (!dofname) {
		addr1 = saddr1;
		addr2 = saddr2;
	}
	writing = 0;
}

/*
 * Is file the edited file?
 * Work here is that it is not considered edited
 * if this is a partial buffer, and distinguish
 * all cases.
 */
edfile()
{

	if (!edited || !eq(file, savedfile))
		return (NOTEDF);
	return (addr1 == one && addr2 == dol ? EDF : PARTBUF);
}

/*
 * Extract the next line from the io stream.
 */
char *nextip;

getfile()
{
	register short c;
	register char *lp, *fp;

	lp = linebuf;
	fp = nextip;
	do {
		if (--ninbuf < 0) {
			ninbuf = read(io, genbuf, bsize) - 1;
			if (ninbuf < 0) {
				if (lp != linebuf) {
					lp++;
					printf(" [Incomplete last line]");
					break;
				}
				return (EOF);
			}
#ifdef CRYPT
			if (kflag) {
				fp = genbuf;
				while(fp < &genbuf[ninbuf]) {
					if (*fp++ & 0200) {
						crblock(perm, genbuf, ninbuf+1,
	cntch);
						break;
					}
				}
			}
#endif
			fp = genbuf;
			cntch += ninbuf+1;
		}
		if (lp >= &linebuf[LBSIZE]) {
			error(" Line too long");
		}
		c = *fp++;
		if (c == 0) {
			cntnull++;
			continue;
		}
		if (c & QUOTE) {
			cntodd++;
			c &= TRIM;
			if (c == 0)
				continue;
		}
		*lp++ = c;
	} while (c != '\n');
	*--lp = 0;
	nextip = fp;
	cntln++;
	return (0);
}

/*
 * Write a range onto the io stream.
 */
putfile(isfilter)
int isfilter;
{
	line *a1;
	register char *fp, *lp;
	register int nib;
	struct stat statb;

	a1 = addr1;
	clrstats();
	cntln = addr2 - a1 + 1;
	if (cntln == 0)
		return;
	if (fstat(io, &statb) < 0)
		bsize = LBSIZE;
	else {
		bsize = statb.st_blksize;
		if (bsize <= 0)
			bsize = LBSIZE;
	}
	nib = bsize;
	fp = genbuf;
	do {
		getline(*a1++);
		lp = linebuf;
		for (;;) {
			if (--nib < 0) {
				nib = fp - genbuf;
#ifdef CRYPT
                		if(kflag && !isfilter)
                                        crblock(perm, genbuf, nib, cntch);
#endif
				if (write(io, genbuf, nib) != nib) {
					wrerror();
				}
				cntch += nib;
				nib = bsize - 1;
				fp = genbuf;
			}
			if ((*fp++ = *lp++) == 0) {
				fp[-1] = '\n';
				break;
			}
		}
	} while (a1 <= addr2);
	nib = fp - genbuf;
#ifdef CRYPT
	if(kflag && !isfilter)
		crblock(perm, genbuf, nib, cntch);
#endif
	if (write(io, genbuf, nib) != nib) {
		wrerror();
	}
	cntch += nib;
}

/*
 * A write error has occurred;  if the file being written was
 * the edited file then we consider it to have changed since it is
 * now likely scrambled.
 */
wrerror()
{

	if (eq(file, savedfile) && edited)
		change();
	syserror();
}

/*
 * Source command, handles nested sources.
 * Traps errors since it mungs unit 0 during the source.
 */
short slevel;
short ttyindes;

source(fil, okfail)
	char *fil;
	bool okfail;
{
	jmp_buf osetexit;
	register int saveinp, ointty, oerrno;
	char *saveglobp;
	short savepeekc;

	signal(SIGINT, SIG_IGN);
	saveinp = dup(0);
	savepeekc = peekc;
	saveglobp = globp;
	peekc = 0; globp = 0;
	if (saveinp < 0)
		error("Too many nested sources");
	if (slevel <= 0)
		ttyindes = saveinp;
	close(0);
	if (open(fil, 0) < 0) {
		oerrno = errno;
		setrupt();
		dup(saveinp);
		close(saveinp);
		errno = oerrno;
		if (!okfail)
			filioerr(fil);
		return;
	}
	slevel++;
	ointty = intty;
	intty = isatty(0);
	oprompt = value(PROMPT);
	value(PROMPT) &= intty;
	getexit(osetexit);
	setrupt();
	if (setexit() == 0)
		commands(1, 1);
	else if (slevel > 1) {
		close(0);
		dup(saveinp);
		close(saveinp);
		slevel--;
		resexit(osetexit);
		reset();
	}
	intty = ointty;
	value(PROMPT) = oprompt;
	close(0);
	dup(saveinp);
	close(saveinp);
	globp = saveglobp;
	peekc = savepeekc;
	slevel--;
	resexit(osetexit);
}

/*
 * Clear io statistics before a read or write.
 */
clrstats()
{

	ninbuf = 0;
	cntch = 0;
	cntln = 0;
	cntnull = 0;
	cntodd = 0;
}

/*
 * Io is finished, close the unit and print statistics.
 */
iostats()
{

	(void) fsync(io);
	close(io);
	io = -1;
	if (hush == 0) {
		if (value(TERSE))
			printf(" %d/%D", cntln, cntch);
		else
			printf(" %d line%s, %D character%s", cntln, plural((long) cntln),
			    cntch, plural(cntch));
		if (cntnull || cntodd) {
			printf(" (");
			if (cntnull) {
				printf("%D null", cntnull);
				if (cntodd)
					printf(", ");
			}
			if (cntodd)
				printf("%D non-ASCII", cntodd);
			putchar(')');
		}
		noonl();
		flush();
	}
	return (cntnull != 0 || cntodd != 0);
}

#if USG | USG3TTY
/* It's so wonderful how we all speak the same language... */
# define index strchr
# define rindex strrchr
#endif

checkmodeline(line)
char *line;
{
	char *beg, *end;
	char cmdbuf[1024];
	char *index(), *rindex();

	beg = index(line, ':');
	if (beg == NULL)
		return;
	if (&beg[-3] < line)
		return;
	if (!(  ( (beg[-3] == ' ' || beg[-3] == '\t')
	        && beg[-2] == 'e'
		&& beg[-1] == 'x')
	     || ( (beg[-3] == ' ' || beg[-3] == '\t')
	        && beg[-2] == 'v'
		&& beg[-1] == 'i'))) return;
	strncpy(cmdbuf, beg+1, sizeof cmdbuf);
	end = rindex(cmdbuf, ':');
	if (end == NULL)
		return;
	*end = 0;
	globp = cmdbuf;
	commands(1, 1);
}
