/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static	char sccsid[] = "@(#)nm.c 4.8 4/7/87";
#endif

/*
 * nm - print name list; VAX string table version
 */

#include <sys/types.h>
#include <sys/file.h>
#include <ar.h>
#include <stdio.h>
#include <ctype.h>
#include <a.out.h>
#include <stab.h>
#include <ranlib.h>

#define	OARMAG		0177545		/* OLD archive magic number */
#define	SELECT		(archive ? archdr.ar_name : *xargv)

#define	YES		1
#define	NO		0

#define	u_strx		n_un.n_strx
#define	u_name		n_un.n_name

typedef struct nlist	NLIST;

union {				/* exec header, or magic string from library */
	char	mag_armag[SARMAG + 1];
	struct	exec mag_exp;
} mag_un;

struct	ar_hdr	archdr;		/* archive file header structure */
FILE	*fi;			/* input file stream */
off_t	off;			/* offset into file */
int	aflg,			/* print debugger symbols */
	gflg,			/* print only global (external symbols */
	nflg,			/* sort numerically, not alphabetically */
	oflg,			/* prepend element name to each output line */
	pflg,			/* don't sort */
	rflg = 1,		/* how to sort */
	uflg,			/* print only undefined symbols */
	narg,			/* global number of arguments */
	errs,			/* global error flag */
	archive;		/* if file is an archive */
char	**xargv;		/* global pointer to file name */

main(argc, argv)
	int	argc;
	char	**argv;
{
	extern int	optind;
	int	ch;			/* getopts char */

	while ((ch = getopt(argc, argv, "agnopru")) != EOF)
		switch((char)ch) {
		case 'a':
			aflg = YES;
			break;
		case 'g':
			gflg = YES;
			break;
		case 'n':
			nflg = YES;
			break;
		case 'o':
			oflg = YES;
			break;
		case 'p':
			pflg = YES;
			break;
		case 'r':
			rflg = -1;
			break;
		case 'u':
			uflg = YES;
			break;
		case '?':
		default:
			fputs("usage: nm [-agnopru] [file ...]\n", stderr);
			exit(2);
		}
	argc -= optind;
	argv += optind;
	if (!argc) {
		argc = 1;
		argv[0] = "a.out";
	}
	narg = argc;
	for (xargv = argv; argc--; ++xargv)
		if (fi = fopen(*xargv, "r")) {
			namelist();
			(void)fclose(fi);
		}
		else
			error(NO, "cannot open");
	exit(errs);
}

namelist()
{
	register NLIST	*N, **L;
	register int	symcount, nsyms;
	static	NLIST	*symp, **list;
	static int	lastnsyms = -1,
			laststrsiz = -1;
	static char	*strp;
	off_t	strsiz;
	long	lseek();
	int	compare();
	char	*malloc(), *realloc();

	/*
	 * read first few bytes, determine if an archive,
	 * or executable; if executable, check magic number
	 */
	/*NOSTRICT*/
	if (!fread((char *)&mag_un, sizeof(mag_un), 1, fi)) {
		error(NO, "unable to read file");
		return;
	}
	if (mag_un.mag_exp.a_magic == OARMAG) {
		error(NO, "old archive");
		return;
	}
	if (bcmp(mag_un.mag_armag, ARMAG, SARMAG)) {
		if (N_BADMAG(mag_un.mag_exp)) {
			error(NO, "bad format");
			return;
		}
		archive = NO;
		rewind(fi);
	}
	else {
		/*
		 * if archive, skip first entry
		 * if ranlib'd, skip second entry
		 */
		off = SARMAG;		/* see nextel() */
		(void)nextel();
		if (!strcmp(RANLIBMAG, archdr.ar_name))
			(void)nextel();
		if (narg > 1)
			printf("\n%s:\n", *xargv);
		archive = YES;
	}

	do {
		/* check for bad magic number */
		/*NOSTRICT*/
		if (!fread((char *)&mag_un.mag_exp, sizeof(struct exec), 1, fi)) {
			error(NO, "unable to read magic number");
			return;
		}
		if (N_BADMAG(mag_un.mag_exp))
			continue;

		/* calculate number of symbols in object */
		if (!(nsyms = mag_un.mag_exp.a_syms / sizeof(NLIST))) {
			error(NO, "no name list");
			continue;
		}

		/* seek to and read symbols */
		(void)fseek(fi, (long)(N_SYMOFF(mag_un.mag_exp) - sizeof(struct exec)), L_INCR);
		if (!symp || nsyms > lastnsyms) {
			if (!symp) {
				/*NOSTRICT*/
				symp = (NLIST *)malloc((u_int)(nsyms * sizeof(NLIST)));
				/*NOSTRICT*/
				list = (NLIST **)malloc((u_int)(nsyms * sizeof(NLIST *)));
			}
			else {
				/*NOSTRICT*/
				symp = (NLIST *)realloc((char *)symp, (u_int)(nsyms * sizeof(NLIST)));
				/*NOSTRICT*/
				list = (NLIST **)realloc((char *)list, (u_int)(nsyms * sizeof(NLIST *)));
			}
			if (!symp || !list)
				error(YES, "out of memory");
			lastnsyms = nsyms;
		}
		/*NOSTRICT*/
		if (fread((char *)symp, sizeof(NLIST), nsyms, fi) != nsyms) {
			error(NO, "bad symbol table");
			continue;
		}

		/* read number of strings, string table */
		/*NOSTRICT*/
		if (!fread((char *)&strsiz, sizeof(strsiz), 1, fi)) {
			error(NO, "no string table (old format .o?)");
			continue;
		}
		if (!strp || strsiz > laststrsiz) {
			strp = strp ? realloc(strp, (u_int)strsiz) : malloc((u_int)strsiz);
			if (!strp)
				error(YES, "out of memory");
			laststrsiz = strsiz;
		}
		if (!fread(strp + sizeof(strsiz), 1, (int)(strsiz - sizeof(strsiz)), fi)) {
			error(NO, "no string table (old format .o?)");
			continue;
		}

		for (symcount = nsyms, L = list, N = symp;--nsyms >= 0;++N)
			if (!(N->n_type & N_EXT) && gflg || N->n_type & N_STAB && (!aflg || gflg || uflg))
				--symcount;
			else {
				N->u_name = N->u_strx ? strp + N->u_strx : "";
				*L++ = N;
			}

		if (!pflg)
			qsort(list, symcount, sizeof(NLIST *), compare);

		if ((archive || narg > 1) && !oflg)
			printf("\n%s:\n", SELECT);

		psyms(list, symcount);
	} while(archive && nextel());
}

psyms(list, nsyms)
	NLIST	**list;
	register int	nsyms;
{
	register NLIST	*L;
	register u_char	type;
	char	*stab();

	while (nsyms--) {
		L = *list++;
		type = L->n_type;
		if (type & N_STAB) {
			if (oflg) {
				if (archive)
					printf("%s:", *xargv);
				printf("%s:", SELECT);
			}
			printf("%08x - %02x %04x %5.5s %s\n", (int)L->n_value, L->n_other & 0xff, L->n_desc & 0xffff, stab(L->n_type), L->u_name);
			continue;
		}
		switch (type & N_TYPE) {
		case N_UNDF:
			type = L->n_value ? 'c' : 'u';
			break;
		case N_ABS:
			type = 'a';
			break;
		case N_TEXT:
			type = 't';
			break;
		case N_DATA:
			type = 'd';
			break;
		case N_BSS:
			type = 'b';
			break;
		case N_FN:
			type = 'f';
			break;
		default:
			type = '?';
			break;
		}
		if (uflg && type != 'u')
			continue;
		if (oflg) {
			if (archive)
				printf("%s:", *xargv);
			printf("%s:", SELECT);
		}
		if (L->n_type & N_EXT)
			type = toupper(type);
		if (!uflg) {
			if (type == 'u' || type == 'U')
				fputs("        ", stdout);
			else
				printf(N_FORMAT, (int)L->n_value);
			printf(" %c ", (char)type);
		}
		puts(L->u_name);
	}
}

compare(p1, p2)
	NLIST	**p1, **p2;
{
	if (nflg) {
		if ((*p1)->n_value > (*p2)->n_value)
			return(rflg);
		if ((*p1)->n_value < (*p2)->n_value)
			return(-rflg);
	}
	return(rflg * strcmp((*p1)->u_name, (*p2)->u_name));
}

nextel()
{
	register char	*cp;
	long	arsize,
		lseek();

	(void)fseek(fi, off, L_SET);
	/*NOSTRICT*/
	if (!fread((char *)&archdr, sizeof(struct ar_hdr), 1, fi))
		return(0);
	for (cp = archdr.ar_name; cp < &archdr.ar_name[sizeof(archdr.ar_name)]; ++cp)
		if (*cp == ' ') {
			*cp = '\0';
			break;
		}
	arsize = atol(archdr.ar_size);
	if (arsize & 1)
		++arsize;
	off = ftell(fi) + arsize;	/* beginning of next element */
	return(1);
}

struct	stabnames {
	int	st_value;
	char	*st_name;
} stabnames[] ={
	N_GSYM,		"GSYM",
	N_FNAME,	"FNAME",
	N_FUN,		"FUN",
	N_STSYM,	"STSYM",
	N_LCSYM,	"LCSYM",
	N_RSYM,		"RSYM",
	N_SLINE,	"SLINE",
	N_SSYM,		"SSYM",
	N_SO,		"SO",
	N_LSYM,		"LSYM",
	N_SOL,		"SOL",
	N_PSYM,		"PSYM",
	N_ENTRY,	"ENTRY",
	N_LBRAC,	"LBRAC",
	N_RBRAC,	"RBRAC",
	N_BCOMM,	"BCOMM",
	N_ECOMM,	"ECOMM",
	N_ECOML,	"ECOML",
	N_LENG,		"LENG",
	N_PC,		"PC",
	0,		0
};

char *
stab(val)
	register u_char	val;
{
	register struct stabnames	*sp;
	static char	prbuf[5];

	for (sp = stabnames; sp->st_value; ++sp)
		if (sp->st_value == val)
			return(sp->st_name);
	(void)sprintf(prbuf, "%02x", (int)val);
	return(prbuf);
}

error(doexit, msg)
	int	doexit;
	char	*msg;
{
	fprintf(stderr, "nm: %s:", *xargv);
	if (archive)
		fprintf(stderr, "(%s): %s\n", archdr.ar_name, msg);
	else
		fprintf(stderr, " %s\n", msg);
	if (doexit)
		exit(2);
	errs = 1;
}
