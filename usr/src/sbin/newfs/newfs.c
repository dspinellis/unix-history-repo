#ifndef lint
static char sccsid[] = "@(#)newfs.c	4.1 %G%";
#endif

/*
 * makefs: friendly front end to mkfs
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/fs.h>

#include <stdio.h>
#include <dir.h>
#include <disktab.h>

int	verbose;		/* show mkfs line before exec */
int	fssize;			/* file system size */
int	fsize;			/* fragment size */
int	bsize;			/* block size */
int	ntracks;		/* # tracks/cylinder */
int	nsectors;		/* # sectors/track */
int	sectorsize;		/* bytes/sector */
int	cpg;			/* cylinders/cylinder group */

char	*av[20];		/* argv array and buffers for exec */
char	a2[20];
char	a3[20];
char	a4[20];
char	a5[20];
char	a6[20];
char	a7[20];
char	device[MAXPATHLEN];
char	cmd[BUFSIZ];

char	*index();
char	*rindex();
char	*sprintf();

main(argc, argv)
	char *argv[];
{
	char *cp, *special;
	register struct disktab *dp;
	register struct partition *pp;
	struct stat st;
	register int i;
	int status;

	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		for (cp = &argv[0][1]; *cp; cp++)
			switch (*cp) {

			case 'v':
				verbose++;
				break;

			case 's':
				if (argc < 1)
					fatal("-s: missing file system size");
				argc--, argv++;
				fssize = atoi(*argv);
				if (fssize < 0)
					fatal("%s: bad file system size",
						*argv);
				goto next;

			case 't':
				if (argc < 1)
					fatal("-t: missing track total");
				argc--, argv++;
				ntracks = atoi(*argv);
				if (ntracks < 0)
					fatal("%s: bad total tracks", *argv);
				goto next;

			case 'b':
				if (argc < 1)
					fatal("-b: missing block size");
				argc--, argv++;
				bsize = atoi(*argv);
				if (bsize < 0 || bsize < MINBSIZE)
					fatal("%s: bad block size", *argv);
				goto next;

			case 'f':
				if (argc < 1)
					fatal("-f: missing frag size");
				argc--, argv++;
				fsize = atoi(*argv);
				if (fsize < 0)
					fatal("%s: bad frag size", *argv);
				goto next;

			case 'S':
				if (argc < 1)
					fatal("-S: missing sector size");
				argc--, argv++;
				sectorsize = atoi(*argv);
				if (sectorsize < 0)
					fatal("%s: bad sector size", *argv);
				goto next;

			case 'c':
				if (argc < 1)
					fatal("-c: missing cylinders/group");
				argc--, argv++;
				cpg = atoi(*argv);
				if (cpg < 0)
					fatal("%s: bad cylinders/group", *argv);
				goto next;

			default:
				fatal("-%c: unknown flag", cp);
			}
next:
		argc--, argv++;
	}
	if (argc < 2) {
		fprintf(stderr, "usage: makefs [ -v ] [ mkfs-options ] %s\n",
			"special-device device-type");
		fprintf(stderr, "where mkfs-options are:\n");
		fprintf(stderr, "-s sectors/track\n");
		fprintf(stderr, "-b block-size\n");
		fprintf(stderr, "-f frag-size\n");
		fprintf(stderr, "-t tracks/cylinder\n");
		fprintf(stderr, "-c cylinders/group\n");
		fprintf(stderr, "-S sector-size\n");
		exit(1);
	}
	special = argv[0];
again:
	if (stat(special, &st) < 0) {
		if (*special != '/') {
			special = sprintf(device, "/dev/%s", argv[0]);
			goto again;
		}
		fprintf(stderr, "makefs: "); perror(special);
		exit(2);
	}
	if ((st.st_mode & S_IFMT) != S_IFBLK &&
	    (st.st_mode & S_IFMT) != S_IFCHR)
		fatal("%s: not a block or character device", special);
	dp = getdiskbyname(argv[1]);
	if (dp == 0)
		fatal("%s: unknown disk type", argv[1]);
	cp = index(argv[0], '\0') - 1;
	if (cp == 0 || *cp < 'a' || *cp > 'h')
		fatal("%s: can't figure out file system partition", argv[0]);
	pp = &dp->d_partitions[*cp - 'a'];
	if (fssize == 0) {
		fssize = pp->p_size;
		if (fssize < 0)
			fatal("%s: no default size for `%c' partition",
				argv[1], *cp);
	}
	if (nsectors == 0) {
		nsectors = dp->d_nsectors;
		if (nsectors < 0)
			fatal("%s: no default #sectors/track", argv[1]);
	}
	if (ntracks == 0) {
		ntracks = dp->d_ntracks;
		if (ntracks < 0)
			fatal("%s: no default #tracks", argv[1]);
	}
	if (sectorsize == 0) {
		sectorsize = dp->d_secsize;
		if (sectorsize < 0)
			fatal("%s: no default sector size", argv[1]);
	}
	if (bsize == 0) {
		bsize = pp->p_bsize;
		if (bsize < 0)
			fatal("%s: no default block size for `%c' partition",
				argv[1], *cp);
	}
	if (fsize == 0) {
		fsize = pp->p_fsize;
		if (fsize < 0)
			fatal("%s: no default frag size for `%c' partition",
				argv[1], *cp);
	}
	i = 0;
	av[i++] = sprintf(a2, "%d", fssize);
	av[i++] = sprintf(a3, "%d", nsectors);
	av[i++] = sprintf(a4, "%d", ntracks);
	av[i++] = sprintf(a5, "%d", bsize);
	av[i++] = sprintf(a6, "%d", fsize);
	if (cpg != 0)
		av[i++] = sprintf(a7, "%d", cpg);
	av[i++] = 0;
	sprintf(cmd, "/etc/mkfs %s", special);
	for (i = 0; av[i] != 0; i++) {
		strcat(cmd, " ");
		strcat(cmd, av[i]);
	}
	if (verbose)
		printf("%s\n", cmd);
#ifdef notdef
	if (status = system(cmd))
		exit(status);
#endif
	if (*cp == 'a') {
		char type[3];

		cp = rindex(special, '/');
		if (cp == NULL)
			fatal("%s: can't figure out disk type from name",
				special);
		type[0] = *++cp;
		type[1] = *++cp;
		type[2] = '\0';
		installboot(special, type);
	}
	exit(0);
}

installboot(dev, type)
	char *dev, *type;
{
	int fd;
	char bootblock[MAXPATHLEN], standalonecode[MAXPATHLEN];
	char bootimage[BBSIZE];

	sprintf(bootblock, "/sys/mdec/%sboot", type);
	sprintf(standalonecode, "/sys/stand/boot%s", type);
	if (verbose) {
		printf("installing boot code\n");
		printf("sector 0 boot = %s\n", bootblock);
		printf("1st level boot = %s\n", standalonecode);
	}
	fd = open(bootblock, 0);
	if (fd < 0) {
		fprintf(stderr, "makefs: "); perror(bootblock);
		exit(1);
	}
	if (read(fd, bootimage, DEV_BSIZE) < 0) {
		fprintf(stderr, "makefs: "); perror(bootblock);
		exit(2);
	}
	close(fd);
	fd = open(standalonecode, 0);
	if (fd < 0) {
		fprintf(stderr, "makefs: "); perror(standalonecode);
		exit(1);
	}
	if (read(fd, &bootimage[DEV_BSIZE], BBSIZE - DEV_BSIZE) < 0) {
		fprintf(stderr, "makefs: "); perror(standalonecode);
		exit(2);
	}
	close(fd);
#ifdef notdef
	fd = open(special, 1);
	if (fd < 0) {
		fprintf(stderr, "makefs: "); perror(special);
		exit(1);
	}
	if (write(fd, bootimage, BBSIZE) != BBSIZE) {
		fprintf(stderr, "makefs: "); perror(special);
		exit(2);
	}
	close(fd);
#endif
}

/*VARARGS*/
fatal(fmt, arg1, arg2)
	char *fmt;
{

	fprintf(stderr, "makefs: ");
	fprintf(stderr, fmt, arg1, arg2);
	putc('\n', stderr);
	exit(10);
}
