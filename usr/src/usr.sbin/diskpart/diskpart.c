/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)diskpart.c	5.3 (Berkeley) %G%";
#endif not lint

/*
 * Program to calculate standard disk partition sizes.
 */
#include <sys/param.h>

#include <stdio.h>
#include <disktab.h>

#define	NPARTITIONS	8
#define	PART(x)		(x - 'a')

/*
 * Default partition sizes, where they exist.
 */
#define	NDEFAULTS	4
int	defpart[NDEFAULTS][NPARTITIONS] = {
   { 15884, 66880, 0, 15884, 307200, 0, 0, 291346 },	/* ~ 356+ Mbytes */
   { 15884, 33440, 0, 15884, 55936, 0, 0, 291346 },	/* ~ 206-355 Mbytes */
   { 15884, 33440, 0, 15884, 55936, 0, 0, 0 },		/* ~ 61-205 Mbytes */
   { 15884, 10032, 0, 15884, 0, 0, 0, 0 },		/* ~ 20-60 Mbytes */
};

/*
 * Each array defines a layout for a disk;
 * that is, the collection of partitions totally
 * covers the physical space on a disk.
 */
#define	NLAYOUTS	3
char	layouts[NLAYOUTS][NPARTITIONS] = {
   { 'a', 'b', 'h', 'g' },
   { 'a', 'b', 'h', 'd', 'e', 'f' },
   { 'c' },
};

/*
 * Default disk block and disk block fragment
 * sizes for each file system.  Those file systems
 * with zero block and frag sizes are special cases
 * (e.g. swap areas or for access to the entire device).
 */
struct	defparam {
	int	p_bsize;	/* block size */
	int	p_fsize;	/* frag size */
} defparam[NPARTITIONS] = {
	{ 8192, 1024 },		/* a */
	{ 0 },			/* b */
	{ 0 },			/* c */
	{ 8192, 1024 },		/* d */
	{ 4096, 512 },		/* e */
	{ 4096, 1024 },		/* f */
	{ 4096, 1024 },		/* g */
	{ 4096, 512 }		/* h */
};

/*
 * Each disk has some space reserved for a bad sector
 * forwarding table.  DEC standard 144 uses the first
 * 5 even numbered sectors in the last track of the
 * last cylinder for replicated storage of the bad sector
 * table; another 126 sectors past this is needed as a
 * pool of replacement sectors.
 */
int	badsecttable = 126;	/* # sectors */

int	pflag;			/* print device driver partition tables */
int	dflag;			/* print disktab entry */

struct	disktab *promptfordisk();

main(argc, argv)
	int argc;
	char *argv[];
{
	struct disktab *dp;
	register int curcyl, spc, def, part, layout;
	int threshhold, numcyls[NPARTITIONS], startcyl[NPARTITIONS];
	char *lp;

	argc--, argv++;
	if (argc < 1) {
		fprintf(stderr, "usage: disktab [ -p ] [ -d ] disk-type\n");
		exit(1);
	}
	if (argc > 0 && strcmp(*argv, "-p") == 0) {
		pflag++;
		argc--, argv++;
	}
	if (argc > 0 && strcmp(*argv, "-d") == 0) {
		dflag++;
		argc--, argv++;
	}
	dp = getdiskbyname(*argv);
	if (dp == NULL) {
		if (isatty(0))
			dp = promptfordisk(*argv);
		if (dp == NULL) {
			fprintf(stderr, "%s: unknown disk type\n", *argv);
			exit(2);
		}
	}
	spc = dp->d_nsectors * dp->d_ntracks;
	/*
	 * Bad sector table contains one track for the replicated
	 * copies of the table and enough full tracks preceding
	 * the last track to hold the pool of free blocks to which
	 * bad sectors are mapped.
	 */
	badsecttable = dp->d_nsectors + roundup(badsecttable, dp->d_nsectors);
	threshhold = howmany(spc, badsecttable);

	/* 
	 * Figure out if disk is large enough for
	 * expanded swap area and 'd', 'e', and 'f'
	 * partitions.  Otherwise, use smaller defaults
	 * based on RK07.
	 */
	for (def = 0; def < NDEFAULTS; def++) {
		curcyl = 0;
		for (part = PART('a'); part < NPARTITIONS; part++)
			curcyl += howmany(defpart[def][part], spc);
		if (curcyl < dp->d_ncylinders - threshhold)
			break;
	}
	if (def >= NDEFAULTS) {
		fprintf(stderr, "%s: disk too small, calculate by hand\n",
			*argv);
		exit(3);
	}

	/*
	 * Calculate number of cylinders allocated to each disk
	 * partition.  We may waste a bit of space here, but it's
	 * in the interest of compatibility (for mixed disk systems).
	 */
	for (curcyl = 0, part = PART('a'); part < NPARTITIONS; part++) {
		numcyls[part] = 0;
		if (defpart[def][part] != 0) {
			numcyls[part] = howmany(defpart[def][part], spc);
			curcyl += numcyls[part];
		}
	}
	numcyls[PART('f')] = dp->d_ncylinders - curcyl;
	numcyls[PART('g')] =
		numcyls[PART('d')] + numcyls[PART('e')] + numcyls[PART('f')];
	numcyls[PART('c')] = dp->d_ncylinders;
	defpart[def][PART('f')] = numcyls[PART('f')] * spc - badsecttable;
	defpart[def][PART('g')] = numcyls[PART('g')] * spc - badsecttable;
	defpart[def][PART('c')] = numcyls[PART('c')] * spc;

	/*
	 * Calculate starting cylinder number for each partition.
	 * Note the 'h' partition is physically located before the
	 * 'g' or 'd' partition.  This is reflected in the layout
	 * arrays defined above.
	 */
	for (layout = 0; layout < NLAYOUTS; layout++) {
		curcyl = 0;
		for (lp = layouts[layout]; *lp != 0; lp++) {
			startcyl[PART(*lp)] = curcyl;
			curcyl += numcyls[PART(*lp)];
		}
	}

	if (pflag) {
		printf("}, %s_sizes[%d] = {\n", dp->d_name, NPARTITIONS);
		for (part = PART('a'); part < NPARTITIONS; part++) {
			if (numcyls[part] == 0) {
				printf("\t0,\t0,\n");
				continue;
			}
			printf("\t%d,\t%d,\t\t/* %c=cyl %d thru %d */\n",
				defpart[def][part], startcyl[part],
				'A' + part, startcyl[part],
				startcyl[part] + numcyls[part] - 1);
		}
		exit(0);
	}
	if (dflag) {
		int nparts;

		/*
		 * In case the disk is in the ``in-between'' range
		 * where the 'g' partition is smaller than the 'h'
		 * partition, reverse the frag sizes so the /usr partition
		 * is always set up with a frag size larger than the
		 * user's partition.
		 */
		if (defpart[def][PART('g')] < defpart[def][PART('h')]) {
			int temp;

			temp = defparam[PART('h')].p_fsize;
			defparam[PART('h')].p_fsize =
				defparam[PART('g')].p_fsize;
			defparam[PART('g')].p_fsize = temp;
		}
		printf("%s:\\\n", dp->d_name);
		printf("\t:ty=%s:ns#%d:nt#%d:nc#%d:\\\n", dp->d_type,
			dp->d_nsectors, dp->d_ntracks, dp->d_ncylinders);
		for (nparts = 0, part = PART('a'); part < NPARTITIONS; part++)
			if (defpart[def][part] != 0)
				nparts++;
		for (part = PART('a'); part < NPARTITIONS; part++) {
			if (defpart[def][part] == 0)
				continue;
			printf("\t:p%c#%d:", 'a' + part, defpart[def][part]);
			if (defparam[part].p_bsize != 0) {
				printf("b%c#%d:f%c#%d:",
				  'a' + part, defparam[part].p_bsize,
				  'a' + part, defparam[part].p_fsize);
			}
			nparts--;
			printf("%s\n", nparts > 0 ? "\\" : "");
		}
		exit(0);
	}
	printf("%s: #sectors/track=%d, #tracks/cylinder=%d #cylinders=%d\n",
		dp->d_name, dp->d_nsectors, dp->d_ntracks, dp->d_ncylinders);
	printf("\n    Partition\t   Size\t   Range\n");
	for (part = PART('a'); part < NPARTITIONS; part++) {
		printf("\t%c\t", 'a' + part);
		if (numcyls[part] == 0) {
			printf(" unused\n");
			continue;
		}
		printf("%7d\t%4d - %d\n", defpart[def][part], startcyl[part],
			startcyl[part] + numcyls[part] - 1);
	}
}

struct disktab disk;

struct	field {
	char	*f_name;
	char	*f_defaults;
	int	*f_location;
} fields[] = {
	{ "sector size",		"512",	&disk.d_secsize },
	{ "#sectors/track",		0,	&disk.d_nsectors },
	{ "#tracks/cylinder",		0,	&disk.d_ntracks },
	{ "#cylinders",			0,	&disk.d_ncylinders },
	{ "revolutions/minute",		"3600",	&disk.d_rpm },
	{ 0, 0, 0 },
};

struct disktab *
promptfordisk(name)
	char *name;
{
	register struct disktab *dp = &disk;
	register struct field *fp;
	static char type[BUFSIZ];
	char buf[BUFSIZ], *cp, *gets();

	dp->d_name = name;
	fprintf(stderr,
		"%s: unknown disk type, want to supply parameters (y/n)? ",
		name);
	(void) gets(buf);
	if (*buf != 'y')
		return ((struct disktab *)0);
gettype:
	fprintf(stderr, "type (winchester|removable|simulated)? ");
	(void) gets(type);
	if (strcmp(type, "winchester") && strcmp(type, "removable") &&
	    strcmp(type, "simulated")) {
		fprintf(stderr, "%s: bad disk type\n", type);
		goto gettype;
	}
	dp->d_type = type;
	fprintf(stderr, "(type <cr> to get default value, if only one)\n");
	for (fp = fields; fp->f_name != NULL; fp++) {
again:
		fprintf(stderr, "%s ", fp->f_name);
		if (fp->f_defaults != NULL)
			fprintf(stderr, "(%s)", fp->f_defaults);
		fprintf(stderr, "? ");
		cp = gets(buf);
		if (*cp == '\0') {
			if (fp->f_defaults == NULL) {
				fprintf(stderr, "no default value\n");
				goto again;
			}
			cp = fp->f_defaults;
		}
		*fp->f_location = atoi(cp);
		if (*fp->f_location == 0) {
			fprintf(stderr, "%s: bad value\n", cp);
			goto again;
		}
	}
	return (dp);
}
