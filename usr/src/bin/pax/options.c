/*-
 * Copyright (c) 1992 Keith Muller.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)options.c	1.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/mtio.h>
#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <limits.h>
#include "pax.h"
#include "options.h"
#include "cpio.h"
#include "extern.h"

/*
 * Routines which handle command line options
 */

static char flgch[] = FLGCH;	/* list of all possible flags */
static OPLIST *ophead = NULL;	/* head for format specific options -x */
static OPLIST *optail = NULL;	/* option tail */

static int no_op __P((void));
static void printflg __P((unsigned int));
static int c_frmt __P((const void *, const void *));
static off_t str_offt __P((char *));

/*
 *	Format specific routine table - MUST BE IN SORTED ORDER BY NAME
 *	(see pax.h for description of each function)
 *
 * 	name, blksz, hdsz, udev, hlk, blkagn, inhead, id, st_read,
 *	read, end_read, st_write, write, end_write, trail,
 *	rd_data, wr_data, options
 */

FSUB fsub[] = {
/* 0: OLD BINARY CPIO */
	"bcpio", 5120, sizeof(HD_BCPIO), 1, 0, 0, 1, bcpio_id, cpio_strd,
	bcpio_rd, bcpio_endrd, cpio_stwr, bcpio_wr, cpio_endwr, cpio_trail,
	rd_wrfile, wr_rdfile, bad_opt,

/* 1: OLD OCTAL CHARACTER CPIO */
	"cpio", 5120, sizeof(HD_CPIO), 1, 0, 0, 1, cpio_id, cpio_strd,
	cpio_rd, cpio_endrd, cpio_stwr, cpio_wr, cpio_endwr, cpio_trail,
	rd_wrfile, wr_rdfile, bad_opt,

/* 2: SVR4 HEX CPIO */
	"sv4cpio", 5120, sizeof(HD_VCPIO), 1, 0, 0, 1, vcpio_id, cpio_strd,
	vcpio_rd, vcpio_endrd, cpio_stwr, vcpio_wr, cpio_endwr, cpio_trail,
	rd_wrfile, wr_rdfile, bad_opt,

/* 3: SVR4 HEX CPIO WITH CRC */
	"sv4crc", 5120, sizeof(HD_VCPIO), 1, 0, 0, 1, crc_id, crc_strd,
	vcpio_rd, vcpio_endrd, crc_stwr, vcpio_wr, cpio_endwr, cpio_trail,
	rd_wrfile, wr_rdfile, bad_opt,

/* 4: OLD TAR */
	"tar", 10240, BLKMULT, 0, 1, BLKMULT, 0, tar_id, no_op,
	tar_rd, tar_endrd, no_op, tar_wr, tar_endwr, tar_trail,
	rd_wrfile, wr_rdfile, tar_opt,

/* 5: POSIX USTAR */
	"ustar", 10240, BLKMULT, 0, 1, BLKMULT, 0, ustar_id, ustar_strd,
	ustar_rd, tar_endrd, ustar_stwr, ustar_wr, tar_endwr, tar_trail,
	rd_wrfile, wr_rdfile, bad_opt,
};
#define DEFLT	5	/* default write format from list above */

/*
 * ford is the archive search order used by get_arc() to determine what kind
 * of archive we are dealing with. This helps to properly id  archive formats
 * some formats may be subsets of others....
 */
int ford[] = {5, 4, 3, 2, 1, 0, -1 };

/*
 * options()
 *	look at the user specified flags. set globals as required and check if
 *	the user specified a legal set of flags. If not, complain and exit
 */

#if __STDC__
void
options(register int argc, register char **argv)
#else
void
options(argc, argv)
	register int argc;
	register char **argv;
#endif
{
	register int c;
	register int i;
	unsigned int flg = 0;
	unsigned int bflg = 0;
	register char *pt;
        FSUB tmp;
	extern char *optarg;
	extern int optind;

	/*
	 * process option flags
	 */
	while ((c=getopt(argc,argv,"ab:cdf:iklno:p:rs:tuvwx:B:DE:G:HLT:U:XYZ"))
	    != EOF) {
		switch (c) {
		case 'a':
			/*
			 * append
			 */
			flg |= AF;
			break;
		case 'b':
			/*
			 * specify blocksize on write
			 */
			flg |= BF;
			if ((wrblksz = (int)str_offt(optarg)) <= 0) {
				warn(1, "Invalid block size %s", optarg);
				usage();
			}
			break;
		case 'c':
			/*
			 * inverse match on patterns
			 */
			cflag = 1;
			flg |= CF;
			break;
		case 'd':
			/*
			 * match only dir on extract, not the subtree at dir
			 */
			dflag = 1;
			flg |= DF;
			break;
		case 'f':
			/*
			 * filename where the archive is stored
			 */
			arcname = optarg;
			flg |= FF;
			break;
		case 'i':
			/*
			 * interactive file rename
			 */
			iflag = 1;
			flg |= IF;
			break;
		case 'k':
			/*
			 * do not clobber files that exist
			 */
			kflag = 1;
			flg |= KF;
			break;
		case 'l':
			/*
			 * try to link src to dest with copy (-rw)
			 */
			lflag = 1;
			flg |= LF;
			break;
		case 'n':
			/*
			 * select first match for a pattern only
			 */
			nflag = 1;
			flg |= NF;
			break;
		case 'o':
			/*
			 * pass format specific options
			 */
			flg |= OF;
			if (opt_add(optarg) < 0)
				usage();
			break;
		case 'p':
			/*
			 * specify file characteristic options
			 */
			for (pt = optarg; *pt != '\0'; ++pt) {
				switch(*pt) {
				case 'a':
					/*
					 * do not preserve access time
					 */
					patime = 0;
					break;
				case 'e':
					/*
					 * preserve user id, group id, file
					 * mode, access/modification times
					 */
					pids = 1;
					pmode = 1;
					patime = 1;
					pmtime = 1;
					break;
				case 'm':
					/*
					 * do not preserve modification time
					 */
					pmtime = 0;
					break;
				case 'o':
					/*
					 * preserve uid/gid
					 */
					pids = 1;
					break;
				case 'p':
					/*
					 * preserver file mode bits
					 */
					pmode = 1;
					break;
				default:
					warn(1, "Invalid -p string: %c", *pt);
					usage();
					break;
				}
			}
			flg |= PF;
			break;
		case 'r':
			/*
			 * read the archive
			 */
			flg |= RF;
			break;
		case 's':
			/*
			 * file name substitution name pattern
			 */
			if (rep_add(optarg) < 0) {
				usage();
				break;
			}
			flg |= SF;
			break;
		case 't':
			/*
			 * preserve access time on filesystem nodes we read
			 */
			tflag = 1;
			flg |= TF;
			break;
		case 'u':
			/*
			 * ignore those older files
			 */
			uflag = 1;
			flg |= UF;
			break;
		case 'v':
			/*
			 * verbose operation mode
			 */
			vflag = 1;
			flg |= VF;
			break;
		case 'w':
			/*
			 * write an archive
			 */
			flg |= WF;
			break;
		case 'x':
			/*
			 * specify an archive format on write
			 */
			tmp.name = optarg;
			if (frmt = (FSUB *)bsearch((void *)&tmp, (void *)fsub,
			    sizeof(fsub)/sizeof(FSUB), sizeof(FSUB), c_frmt)) {
				flg |= XF;
				break;
			}
			warn(1, "Unknown -x format: %s", optarg);
			(void)fputs("pax: Known -x formats are:", stderr);
			for (i = 0; i < (sizeof(fsub)/sizeof(FSUB)); ++i)
				(void)fprintf(stderr, " %s", fsub[i].name);
			(void)fputs("\n\n", stderr);
			usage();
			break;
		case 'B':
			/*
			 * non-standard option on number of bytes written on a
			 * single archive volume.
			 */
			if ((wrlimit = str_offt(optarg)) <= 0) {
				warn(1, "Invalid write limit %s", optarg);
				usage();
			}
			if (wrlimit % BLKMULT) {
				warn(1, "Write limit is not a %d byte multiple",
				    BLKMULT);
				usage();
			}
			flg |= CBF;
			break;
		case 'D':
			/*
			 * On extraction check file inode change time before the
			 * modification of the file name. Non standard option.
			 */
			Dflag = 1;
			flg |= CDF;
			break;
		case 'E':
			/*
			 * non-standard limit on read faults
			 * 0 indicates stop after first error, values
			 * indicate a limit, "NONE" try forever
			 */
			flg |= CEF;
			if (strcmp(NONE, optarg) == 0)
				maxflt = -1;
			else if ((maxflt = atoi(optarg)) < 0) {
				warn(1, "Error count value must be positive");
				usage();
			}
			break;
		case 'G':
			/*
			 * non-standard option for selecting files within an
			 * archive by group (gid or name)
			 */
			if (grp_add(optarg) < 0) {
				usage();
				break;
			}
			flg |= CGF;
			break;
		case 'H':
			/*
			 * follow command line symlinks only
			 */
			Hflag = 1;
			flg |= CHF;
			break;
		case 'L':
			/*
			 * follow symlinks
			 */
			Lflag = 1;
			flg |= CLF;
			break;
		case 'T':
			/*
			 * non-standard option for selecting files within an
			 * archive by modification time range (lower,upper)
			 */
			if (trng_add(optarg) < 0) {
				usage();
				break;
			}
			flg |= CTF;
			break;
		case 'U':
			/*
			 * non-standard option for selecting files within an
			 * archive by user (uid or name)
			 */
			if (usr_add(optarg) < 0) {
				usage();
				break;
			}
			flg |= CUF;
			break;
		case 'X':
			/*
			 * do not pass over mount points in the file system
			 */
			Xflag = 1;
			flg |= CXF;
			break;
		case 'Y':
			/*
			 * On extraction check file inode change time after the
			 * modification of the file name. Non standard option.
			 */
			Yflag = 1;
			flg |= CYF;
			break;
		case 'Z':
			/*
			 * On extraction check modification time after the
			 * modification of the file name. Non standard option.
			 */
			Zflag = 1;
			flg |= CZF;
			break;
		case '?':
		default:
			usage();
			break;
		}
	}

	/*
	 * figure out the operation mode of pax read,write,extract,copy,append
	 * or list. check that we have not been given a bogus set of flags
	 * for the operation mode.
	 */
	if (ISLIST(flg)) {
		act = LIST;
		bflg = flg & BDLIST;
	} else if (ISEXTRACT(flg)) {
		act = EXTRACT;
		bflg = flg & BDEXTR;
	} else if (ISARCHIVE(flg)) {
		act = ARCHIVE;
		bflg = flg & BDARCH;
	} else if (ISAPPND(flg)) {
		act = APPND;
		bflg = flg & BDARCH;
	} else if (ISCOPY(flg)) {
		act = COPY;
		bflg = flg & BDCOPY;
	} else
		usage();
	if (bflg) {
		printflg(flg);
		usage();
	}

	/*
	 * if we are writing (ARCHIVE) we use the default format if the user
	 * did not specify a format. when we write during an APPEND, we will
	 * adopt the format of the existing archive if none was supplied.
	 */
	if (!(flg & XF) && (act == ARCHIVE))
		frmt = &(fsub[DEFLT]);

	/*
	 * process the args as they are interpreted by the operation mode
	 */
	switch (act) {
	case LIST:
	case EXTRACT:
		for (; optind < argc; optind++)
			if (pat_add(argv[optind]) < 0)
				usage();
		break;
	case COPY:
		if (optind >= argc) {
			warn(0, "Destination directory was not supplied");
			usage();
		}
		--argc;
		dirptr = argv[argc];
		/* FALL THROUGH */
	case ARCHIVE:
	case APPND:
		for (; optind < argc; optind++)
			if (ftree_add(argv[optind]) < 0)
				usage();
		/*
		 * no read errors allowed on updates/append operation!
		 */
		maxflt = 0;
		break;
	}
}

/*
 * printflg()
 *	print out those invalid flag sets found to the user
 */

#if __STDC__
static void
printflg(unsigned int flg)
#else
static void
printflg(flg)
	unsigned int flg;
#endif
{
	int nxt;
	int pos = 0;

	(void)fputs("pax: Invalid combination of options:", stderr);
	while (nxt = ffs(flg)) {
		flg = flg >> nxt;
		pos += nxt;
		(void)fprintf(stderr, " -%c", flgch[pos-1]);
	}
	(void)putc('\n', stderr);
}

/*
 * c_frmt()
 *	comparison routine used by bsearch to find the format specified
 *	by the user
 */

#if __STDC__
static int
c_frmt(const void *a, const void *b)
#else
static int
c_frmt(a, b)
        void *a;
        void *b;
#endif
{
        return(strcmp(((FSUB *)a)->name, ((FSUB *)b)->name));
}

/*
 * opt_next()
 *	called by format specific options routines to get each format specific
 *	flag and value specified with -o
 * Return:
 *	pointer to next OPLIST entry or NULL (end of list).
 */

#if __STDC__
OPLIST *
opt_next(void)
#else
OPLIST *
opt_next()
#endif
{
	OPLIST *opt;

	if ((opt = ophead) != NULL)
		ophead = ophead->fow;
	return(opt);
}

/*
 * bad_opt()
 *	generic routine used to complain about a format specific options
 *	when the format does not support options.
 */

#if __STDC__
int
bad_opt(void)
#else
int
bad_opt()
#endif
{
	register OPLIST *opt;

	if (ophead == NULL)
		return(0);
	/*
	 * print all we were given
	 */
	warn(1,"These format options are not supported");
	while ((opt = opt_next()) != NULL)
		(void)fprintf(stderr, "\t%s = %s\n", opt->name, opt->value);
	usage();
	return(0);
}

/*
 * opt_add()
 *	breaks the value supplied to -o into a option name and value. options
 *	are given to -o in the form -o name-value,name=value
 *	mulltiple -o may be specified.
 * Return:
 *	0 if format in name=value format, -1 if -o is passed junk
 */

#if __STDC__
int
opt_add(register char *str)
#else
int
opt_add(str)
	register char *str;
#endif
{
	register OPLIST *opt;
	register char *frpt;
	register char *pt;
	register char *endpt;

	if ((str == NULL) || (*str == '\0')) {
		warn(0, "Invalid option name");
		return(-1);
	}
	frpt = endpt = str;

	/*
	 * break into name and values pieces and stuff each one into a
	 * OPLIST structure. When we know the format, the format specific
	 * option function will go through this list
	 */
	while ((frpt != NULL) && (*frpt != '\0')) {
		if ((endpt = strchr(frpt, ',')) != NULL)
			*endpt = '\0';
		if ((pt = strchr(frpt, '=')) == NULL) {
			warn(0, "Invalid options format");
			return(-1);
		}
		if ((opt = (OPLIST *)malloc(sizeof(OPLIST))) == NULL) {
			warn(0, "Unable to allocate space for option list");
			return(-1);
		}
		*pt++ = '\0';
		opt->name = frpt;
		opt->value = pt;
		opt->fow = NULL;
		if (endpt != NULL)
			frpt = endpt + 1;
		else
			frpt = NULL;
		if (ophead == NULL) {
			optail = ophead = opt;
			continue;
		}
		optail->fow = opt;
		optail = opt;
	}
	return(0);
}

/*
 * str_offt()
 *	Convert an expression of the following forms to an off_t > 0.
 * 	1) A positive decimal number.
 *	2) A positive decimal number followed by a b (mult by 512).
 *	3) A positive decimal number followed by a k (mult by 1024).
 *	4) A positive decimal number followed by a m (mult by 512).
 *	5) A positive decimal number followed by a w (mult by sizeof int)
 *	6) Two or more positive decimal numbers (with/without k,b or w).
 *	   seperated by x (also * for backwards compatibility), specifying
 *	   the product of the indicated values.
 * Return:
 *	0 for an error, a positive value o.w.
 */

#if __STDC__
static off_t
str_offt(char *val)
#else
static off_t
str_offt(val)
	char *val;
#endif
{
	char *expr;
	off_t num, t;

#	ifdef NET2_STAT
	num = strtol(val, &expr, 0);
	if ((num == LONG_MAX) || (num <= 0) || (expr == val))
#	else
	num = strtoq(val, &expr, 0);
	if ((num == QUAD_MAX) || (num <= 0) || (expr == val))
#	endif
		return(0);

	switch(*expr) {
	case 'b':
		t = num;
		num *= 512;
		if (t > num)
			return(0);
		++expr;
		break;
	case 'k':
		t = num;
		num *= 1024;
		if (t > num)
			return(0);
		++expr;
		break;
	case 'm':
		t = num;
		num *= 1048576;
		if (t > num)
			return(0);
		++expr;
		break;
	case 'w':
		t = num;
		num *= sizeof(int);
		if (t > num)
			return(0);
		++expr;
		break;
	}

	switch(*expr) {
		case '\0':
			break;
		case '*':
		case 'x':
			t = num;
			num *= str_offt(expr + 1);
			if (t > num)
				return(0);
			break;
		default:
			return(0);
	}
	return(num);
}

/*
 * no_op()
 *	for those option functions where the archive format has nothing to do.
 * Return:
 *	0
 */

#if __STDC__
static int
no_op(void)
#else
static int
no_op()
#endif
{
	return(0);
}
