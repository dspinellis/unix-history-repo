/*	cpm.c	1.17	85/01/10	*/
/*
	Copyright (c) 1982, 1983 by
	Helge Skrivervik, UCB.
	All rights reserved.
	Permission granted for use by UNIX* licencees.
	UNIX is a trademark of Bell Labs.

*/

#include <stdio.h>
#include "cpmio.h"
#include "cpmfio.h"

#define BIG	2147483647

C_FILE	c_iob[C_NFILE];
int	fid;


int	dflag, cflag, iflag, tflag;
int	blksiz	=	1024;	/* default block size */
int	tracks	=	77;	/* default number of tracks */
int	maxdir	=	64;	/* default number of directory slots per disk */
int	seclth	=	128;	/* default sector length [bytes] */
int	sectrk	=	26;	/* default number of sectors per track */
int	skew	=	6;	/* default sector skew factor */

int	*bitmap = 0;
struct directory *dirbuf;

char *string;

main(argc, argv)
	int	argc;
	char	*argv[];
{

	char *cpmname, *unixname, *malloc();
	int xflag, stat=0, Cflag, Iflag, Bflag;

	if (argc == 1)
		usage();
	for (; argc > 1 && argv[1][0] == '-'; argc--, argv++) {
		switch (argv[1][1]) {

		case 0:
			break;

		case 'B':
			Bflag++;
			continue;

		case 'd':
			dflag++;
			continue;

		case 'c':
			if (Cflag)
				stat++;
			cpmname = argv[2];
			unixname = argv[3];
			argc -= 2;
			argv += 2;
			cflag++;
			continue;

		case 'i':
			if (isatty(0) && isatty(1)) iflag++;
			continue;

		case 'p':
			cpmname = argv[2];
			++argv;
			--argc;
			tflag++;
			continue;

		case 's':
			string = argv[2];
			++argv;
			skew = number(BIG);
			argc--;
			if ((skew < 1) || (skew > 10)) {
				fprintf(stderr, "cpm: skew factor out of range\n");
				exit(1);
			}
			continue;

		case 'b':
			string = argv[2];
			++argv;
			blksiz = number(BIG);
			argc--;
			if (blksiz & 0xc3) {
				fprintf(stderr, "cpm: illegal block size: %d\n",blksiz);
				exit(1);
			}
			continue;

		case 'm':
			string = argv[2];
			++argv;
			maxdir = number(BIG);
			argc--;
			if ((maxdir < 64) || (tracks >1024 )) {
				fprintf(stderr, "cpm: illegal value of m-flag: %d\n",maxdir);
				exit(1);
			}
			continue;

		case 'l':
			string = argv[2];
			++argv;
			seclth = number(BIG);
			argc--;
			if ((seclth < 128) || (tracks >1024 )) {
				fprintf(stderr, "cpm: illegal sector length: %d\n",seclth);
				exit(1);
			}
			continue;

		case 'r':
			string = argv[2];
			++argv;
			sectrk = number(BIG);
			argc--;
			if (sectrk > 48) {
				fprintf(stderr, "cpm: illegal r-flag: %d\n", sectrk);
				exit(1);
			}
			continue;

		case 'C':
			if (cflag)
				stat++;
			cpmname = argv[3];
			unixname = argv[2];
			argc -= 2;
			argv += 2;
			Cflag++;
			continue;

		case 'I':
			Iflag++;
			continue;

#ifdef DEBUG
		case 'x':
			xflag++;
			continue;
#endif

		default:
			fprintf(stderr, "cpm: unknown flag: -%c\n",argv[1][1]);
			stat++;
		}
		break;
	}
	if (stat > 0) {
	}
	if (argc <= 1 && iflag) {
		fprintf(stderr, "cpm: can't copy from stdin in iteractive mode\n");
		exit(1);
	} else {
		if (argc <= 1)
			fid = fileno(stdin);
		else {
			int ic;
			if ((fid = open(*++argv,2)) == -1 ) {
				/*
				 * The specified input file does not exist,
				 * does the user want to initialize a new
				 * file?
				 */

				printf("Input file does not exist.\n");
				printf("Initialize new floppy file? (y/n)?");
				if ((ic = getchar()) != 'y' && ic != 'Y')
					exit(1);
				fid = initcpm(*argv);
				ic = getchar();		/* get <eol> */
			} else {
				if (Iflag) {
				     printf("Initialize floppy file? (y/n)?");
				     if ((ic = getchar()) != 'y' && ic != 'Y')
					exit(1);
				     fid = initcpm(*argv);
				     ic = getchar();		/* get <eol> */
				}
			}
		}
	}
	/* allocate memory for the directory buffer */
	if ((dirbuf = (struct directory *) malloc(maxdir*32)) == NULL ) {
		fprintf(stderr, "cpm: can't allocate memory for directory buffer\n");
		exit(1);
	}
	gen_sktab();
	getdir();
	build_bmap();
#ifdef DEBUG
	if (xflag > 0) {
		int i;
		char ch;

		dbmap("current bitmap:\n"); 
		for (i = (int)dirbuf; i< (int)dirbuf+maxdir*32; i++) {
			ch = *(char *)i;
			putchar((int)ch);
		}
	}
#endif
	if (iflag > 0) {
		interact();
		printf("\n");
		exit(0);
	}
	if (dflag > 0) 
		dispdir();
	if (cflag > 0) {
		copy(cpmname, unixname, Bflag);
		exit(0);
	}
	if (Cflag > 0) {
		pipc(unixname, cpmname, Bflag);
		exit(0);
	}
	if (tflag > 0) {
		copy(cpmname, stdout, 0);
		exit(0);
	}
}


number(big)
{
	register char *cs;
	long n;

	cs = string;
	n = 0;
	while(*cs >= '0' && *cs <= '9')
		n = n*10 + *cs++ - '0';
	for(;;)
	switch(*cs++) {

	case 'k':
		n *= 1024;
		continue;

	case 'w':
		n *= sizeof(int);
		continue;

	case 'b':
		n *= 512;
		continue;

	case '*':
	case 'x':
		string = cs;
		n *= number(BIG);

	case '\0':
		if (n >= big || n < 0) {
			fprintf(stderr,"cpm: number: argument %d out of range\n", n);
			exit(1);
		}
		return (n);
	}
}

usage()
{
	printf("Usage: cpm [-i][-d][-p name][-c|C name1 name2] file-name\n");
	exit(1);
}
