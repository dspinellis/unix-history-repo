/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)makedb.c	8.1 (down!honey) 86/01/19";
#endif lint

#include <stdio.h>
#include "config.h"

typedef struct {
	char *dptr;
	int dsize;
} datum;

char *Ofile = ALIASDB, *ProgName;

#define USAGE "%s [-o dbmname] [-a] [file ...]"

main(argc, argv)
	char *argv[];
{	char *ofptr;
	int c, append = 0;
	extern int optind;
	extern char *optarg;

	ProgName = argv[0];
	while ((c = getopt(argc, argv, "o:av")) != EOF)
		switch(c) {
		case 'o':	/* dbm output file */
			Ofile = optarg;
			break;

		case 'a':	/* append mode */
			append++;
			break;

		default:
			fprintf(stderr, USAGE, ProgName);
			exit(1);
			break;
		}


	if ((ofptr = rindex(Ofile, '/')) != 0)
		ofptr++;
	else
		ofptr = Ofile;
	if (strlen(ofptr) > 10) {
		ofptr[10] = 0;
		fprintf(stderr, "%s: using %s for dbm output\n", ProgName, Ofile);
	}

	if (append == 0 && dbfile(Ofile) != 0) {
		perror_(Ofile);
		exit(1);
	}

	if (dbminit(Ofile) < 0) {
		perror_(Ofile);
		exit(1);
	}

	if (optind == argc)
		makedb((char *) 0);
	else for ( ; optind < argc; optind++)
		makedb(argv[optind]);
	exit(0);
}

dbfile(dbf)
	char *dbf;
{
	return (dbcreat(dbf, "dir") != 0 || dbcreat(dbf, "pag") != 0);
}

dbcreat(dbf, suffix)
	char *dbf, *suffix;
{	char buf[BUFSIZ];
	int fd;

	(void) sprintf(buf, "%s.%s", dbf, suffix);
	if ((fd = creat(buf, 0666)) < 0)
		return(-1);
	(void) close(fd);
	return(0);
}


makedb(ifile)
	char *ifile;
{	char line[BUFSIZ];
	datum key, val;

	if (ifile && (freopen(ifile, "r", stdin) == NULL)) {
		perror_(ifile);
		return;
	}

	/*
	 * keys and values are 0 terminated.  this wastes time and (disk) space,
	 * but does lend simplicity and backwards compatibility.
	 */
	key.dptr = line;
	while (fgets(line, sizeof(line), stdin) != NULL) {
		char *op, *end;

		end = line + strlen(line);
		end[-1] = 0;	/* kill newline, stuff null terminator */
		op = index(line, '\t');
		if (op != 0) {
			*op++ = 0;
			key.dsize = op - line;		/* 0 terminated */
			val.dptr = op;
			val.dsize = end - op;		/* 0 terminated */
		} else {
			key.dsize = end - line;		/* 0 terminated */
			val.dptr = "\0";		/* why must i do this? */
			val.dsize = 1;
		}
		if (store(key, val) < 0)
			perror_(Ofile);
	}
}

perror_(str)
	char	*str;
{
	fprintf(stderr, "%s: ", ProgName);
	perror(str);
}
