/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)dumpmain.c	1.4 (UKC) %G%	5.6 (Berkeley) 2/23/87";
#endif not lint

#include "dump.h"

int	notify = 0;	/* notify operator flag */
int	blockswritten = 0;	/* number of blocks written on current tape */
int	tapeno = 0;	/* current tape number */
int	density = 0;	/* density in bytes/0.1" */
int	ntrec = NTREC;	/* # tape blocks in each tape record */
int	cartridge = 0;	/* Assume non-cartridge tape */

extern int labchk;	/* check labels */
int	rewindoffline;	/* put tape offline after every write */

#ifdef RDUMP
char	*host;
#endif
int	anydskipped;	/* set true in mark() if any directories are skipped */
			/* this lets us avoid map pass 2 in some cases */

main(argc, argv)
	int	argc;
	char	*argv[];
{
	char		*arg;
	int		bflag = 0, i;
	float		fetapes;
	register	struct	fstab	*dt;

	time(&(spcl.c_date));

	tsize = 0;	/* Default later, based on 'c' option for cart tapes */
	tape = TAPE;
	disk = DISK;
	increm = NINCREM;
	temp = TEMP;
	if (TP_BSIZE / DEV_BSIZE == 0 || TP_BSIZE % DEV_BSIZE != 0) {
		msg("TP_BSIZE must be a multiple of DEV_BSIZE\n");
		dumpabort();
	}
	incno = '9';
	uflag = 0;
	arg = "u";
	if(argc > 1) {
		argv++;
		argc--;
		arg = *argv;
		if (*arg == '-')
			argc++;
	}
	while(*arg)
	switch (*arg++) {
	case 'w':
		lastdump('w');		/* tell us only what has to be done */
		exit(0);
		break;
	case 'W':			/* what to do */
		lastdump('W');		/* tell us the current state of what has been done */
		exit(0);		/* do nothing else */
		break;

	case 'f':			/* output file */
		if(argc > 1) {
			argv++;
			argc--;
			tape = *argv;
		}
		break;

	case 'd':			/* density, in bits per inch */
		if (argc > 1) {
			argv++;
			argc--;
			density = atoi(*argv) / 10;
			if (density >= 625 && !bflag)
				ntrec = HIGHDENSITYTREC;
		}
		break;

	case 's':			/* tape size, feet */
		if(argc > 1) {
			argv++;
			argc--;
			tsize = atol(*argv);
			tsize *= 12L*10L;
		}
		break;

	case 'b':			/* blocks per tape write */
		if(argc > 1) {
			argv++;
			argc--;
			bflag++;
			ntrec = atol(*argv);
		}
		break;

	case 'c':			/* Tape is cart. not 9-track */
		cartridge++;
		break;

	case '0':			/* dump level */
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

	case 'u':			/* update /etc/dumpdates */
		uflag++;
		break;

	case 'n':			/* notify operators */
		notify++;
		break;

	case 'l':			/* set a label format */
		if (argc > 1) {
			argv++;
			argc--;
			storelabel(*argv);
		}
		break;

	case 'm':			/* set a label map */
		if (argc > 1) {
			argv++;
			argc--;
			storelabelmap(*argv);
		}
		break;

	case 't':
		labchk = 1;
		break;

	case 'o':
		rewindoffline = 1;
		break;

	default:
		fprintf(stderr, "bad key '%c%'\n", arg[-1]);
		Exit(X_ABORT);
	}
	if(argc > 1) {
		argv++;
		argc--;
		disk = *argv;
	}
	if (strcmp(tape, "-") == 0) {
		pipeout++;
		tape = "standard output";
	}

	/*
	 * Determine how to default tape size and density
	 *
	 *         	density				tape size
	 * 9-track	1600 bpi (160 bytes/.1")	2300 ft.
	 * 9-track	6250 bpi (625 bytes/.1")	2300 ft.
 	 * cartridge	8000 bpi (100 bytes/.1")	1700 ft. (450*4 - slop)
	 */
	if (density == 0)
		density = cartridge ? 100 : 160;
	if (tsize == 0)
 		tsize = cartridge ? 1700L*120L : 2300L*120L;

#ifdef RDUMP
	{ char *index();
	  host = tape;
	  tape = index(host, ':');
	  if (tape == 0) {
		msg("need keyletter ``f'' and device ``host:tape''\n");
		exit(1);
	  }
	  *tape++ = 0;
	  if (rmthost(host) == 0)
		exit(X_ABORT);
	}
	setuid(getuid());	/* rmthost() is the only reason to be setuid */
#endif
	if (signal(SIGHUP, sighup) == SIG_IGN)
		signal(SIGHUP, SIG_IGN);
	if (signal(SIGTRAP, sigtrap) == SIG_IGN)
		signal(SIGTRAP, SIG_IGN);
	if (signal(SIGFPE, sigfpe) == SIG_IGN)
		signal(SIGFPE, SIG_IGN);
	if (signal(SIGBUS, sigbus) == SIG_IGN)
		signal(SIGBUS, SIG_IGN);
	if (signal(SIGSEGV, sigsegv) == SIG_IGN)
		signal(SIGSEGV, SIG_IGN);
	if (signal(SIGTERM, sigterm) == SIG_IGN)
		signal(SIGTERM, SIG_IGN);
	

	if (signal(SIGINT, interrupt) == SIG_IGN)
		signal(SIGINT, SIG_IGN);

	set_operators();	/* /etc/group snarfed */
	getfstab();		/* /etc/fstab snarfed */
	/*
	 *	disk can be either the full special file name,
	 *	the suffix of the special file name,
	 *	the special name missing the leading '/',
	 *	the file system name with or without the leading '/'.
	 */
	dt = fstabsearch(disk);
	if (dt != 0) {
		disk = rawname(dt->fs_spec);
		strncpy(spcl.c_dev, dt->fs_spec, NAMELEN);
		strncpy(spcl.c_filesys, dt->fs_file, NAMELEN);
	} else {
		strncpy(spcl.c_dev, disk, NAMELEN);
		strncpy(spcl.c_filesys, "an unlisted file system", NAMELEN);
	}
	strcpy(spcl.c_label, createlabel(0));
	gethostname(spcl.c_host, NAMELEN);
	spcl.c_level = incno - '0';
	spcl.c_type = TS_TAPE;
	getitime();		/* /etc/dumpdates snarfed */

	msg("Date of this level %c dump: %s\n", incno, prdate(spcl.c_date));
 	msg("Date of last level %c dump: %s\n",
		lastincno, prdate(spcl.c_ddate));
	msg("Dumping %s ", disk);

	if (dt != 0)
		msgtail("(%s) ", dt->fs_file);
#ifdef RDUMP
	msgtail("to %s on host %s\n", tape, host);
#else
	msgtail("to %s\n", tape);
#endif

	initialtape();	/* print label message if required */


	fi = open(disk, 0);
	if (fi < 0) {
		msg("Cannot open %s\n", disk);
		Exit(X_ABORT);
	}
	esize = 0;
	sblock = (struct fs *)buf;
	sync();
	bread(SBLOCK, sblock, SBSIZE);
	if (sblock->fs_magic != FS_MAGIC) {
		msg("bad sblock magic number\n");
		dumpabort();
	}
	msiz = roundup(howmany(sblock->fs_ipg * sblock->fs_ncg, NBBY),
		TP_BSIZE);
	clrmap = (char *)calloc(msiz, sizeof(char));
	dirmap = (char *)calloc(msiz, sizeof(char));
	nodmap = (char *)calloc(msiz, sizeof(char));

	anydskipped = 0;
	msg("mapping (Pass I) [regular files]\n");
	pass(mark, (char *)NULL);		/* mark updates esize */

	if (anydskipped) {
		do {
			msg("mapping (Pass II) [directories]\n");
			nadded = 0;
			pass(add, dirmap);
		} while(nadded);
	} else				/* keep the operators happy */
		msg("mapping (Pass II) [directories]\n");

	bmapest(clrmap);
	bmapest(nodmap);

	if (cartridge) {
		/* Estimate number of tapes, assuming streaming stops at
		   the end of each block written, and not in mid-block.
		   Assume no erroneous blocks; this can be compensated for
		   with an artificially low tape size. */
		fetapes = 
		(	  esize		/* blocks */
			* TP_BSIZE	/* bytes/block */
			* (1.0/density)	/* 0.1" / byte */
		  +
			  esize		/* blocks */
			* (1.0/ntrec)	/* streaming-stops per block */
			* 15.48		/* 0.1" / streaming-stop */
		) * (1.0 / tsize );	/* tape / 0.1" */
	} else {
		/* Estimate number of tapes, for old fashioned 9-track tape */
		int tenthsperirg = (density == 625) ? 3 : 7;
		fetapes =
		(	  esize		/* blocks */
			* TP_BSIZE	/* bytes / block */
			* (1.0/density)	/* 0.1" / byte */
		  +
			  esize		/* blocks */
			* (1.0/ntrec)	/* IRG's / block */
			* tenthsperirg	/* 0.1" / IRG */
		) * (1.0 / tsize );	/* tape / 0.1" */
	}
	etapes = fetapes;		/* truncating assignment */
	etapes++;
	/* count the nodemap on each additional tape */
	for (i = 1; i < etapes; i++)
		bmapest(nodmap);
	esize += i + 10;	/* headers + 10 trailer blocks */
	msg("estimated %ld tape blocks on %3.2f tape(s).\n", esize, fetapes);

	labelest(fetapes);		/* check we have enough labels */

	alloctape();			/* Allocate tape buffer */

	otape();			/* bitmap is the first to tape write */
	time(&(tstart_writing));
	bitmap(clrmap, TS_CLRI);

	msg("dumping (Pass III) [directories]\n");
 	pass(dirdump, dirmap);

	msg("dumping (Pass IV) [regular files]\n");
	pass(dump, nodmap);

	spcl.c_type = TS_END;
#ifndef RDUMP
	for(i=0; i<ntrec; i++)
		spclrec();
#endif
	msg("DUMP: %ld tape blocks on %d tape(s)\n",spcl.c_tapea,spcl.c_volume);
	msg("DUMP IS DONE\n");

	putitime();
#ifndef RDUMP
	if (!pipeout) {
		rewind();
		close(to);
	}
#else
	tflush(1);
	rewind();
#endif
	broadcast("DUMP IS DONE!\7\7\n");
	Exit(X_FINOK);
}

int	sighup(){	msg("SIGHUP()  try rewriting\n"); sigAbort();}
int	sigtrap(){	msg("SIGTRAP()  try rewriting\n"); sigAbort();}
int	sigfpe(){	msg("SIGFPE()  try rewriting\n"); sigAbort();}
int	sigbus(){	msg("SIGBUS()  try rewriting\n"); sigAbort();}
int	sigsegv(){	msg("SIGSEGV()  ABORTING!\n"); abort();}
int	sigalrm(){	msg("SIGALRM()  try rewriting\n"); sigAbort();}
int	sigterm(){	msg("SIGTERM()  try rewriting\n"); sigAbort();}

sigAbort()
{
	if (pipeout) {
		msg("Unknown signal, cannot recover\n");
		dumpabort();
	}
	msg("Rewriting attempted as response to unknown signal.\n");
	fflush(stderr);
	fflush(stdout);
	close_rewind();
	exit(X_REWRITE);
}

char *rawname(cp)
	char *cp;
{
	static char rawbuf[32];
	char *rindex();
	char *dp = rindex(cp, '/');

	if (dp == 0)
		return (0);
	*dp = 0;
	strcpy(rawbuf, cp);
	*dp = '/';
	strcat(rawbuf, "/r");
	strcat(rawbuf, dp+1);
	return (rawbuf);
}
