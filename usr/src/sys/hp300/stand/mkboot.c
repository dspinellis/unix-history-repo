/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mkboot.c	7.5 (Berkeley) %G%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkboot.c	7.2 (Berkeley) 12/16/90";
#endif /* not lint */

#include <sys/param.h>
#include <sys/exec.h>
#include <sys/file.h>

#include <hp300/stand/volhdr.h>

#include <stdio.h>
#include <ctype.h>

#define LIF_NUMDIR	8

#define LIF_VOLSTART	0
#define LIF_VOLSIZE	sizeof(struct lifvol)
#define LIF_DIRSTART	512
#define LIF_DIRSIZE	(LIF_NUMDIR * sizeof(struct lifdir))
#define LIF_FILESTART	8192

#define btolifs(b)	(((b) + (SECTSIZE - 1)) / SECTSIZE)
#define lifstob(s)	((s) * SECTSIZE)

int lpflag;
int loadpoint;
struct load ld;
struct lifvol lifv;
struct lifdir lifd[LIF_NUMDIR];
struct exec ex;
char buf[10240];

/*
 * Old Format:
 *	sector 0:	LIF volume header (40 bytes)
 *	sector 1:	<unused>
 *	sector 2:	LIF directory (8 x 32 == 256 bytes)
 *	sector 3-:	LIF file 0, LIF file 1, etc.
 * where sectors are 256 bytes.
 *
 * New Format:
 *	sector 0:	LIF volume header (40 bytes)
 *	sector 1:	<unused>
 *	sector 2:	LIF directory (8 x 32 == 256 bytes)
 *	sector 3:	<unused>
 *	sector 4-31:	disklabel (~300 bytes right now)
 *	sector 32-:	LIF file 0, LIF file 1, etc.
 */
main(argc, argv)
	char **argv;
{
	int ac;
	char **av;
	int from1, from2, to;
	register int n;
	char *n1, *n2, *lifname();

	ac = --argc;
	av = ++argv;
	if (ac == 0)
		usage();
	if (!strcmp(av[0], "-l")) {
		av++;
		ac--;
		if (ac == 0)
			usage();
		sscanf(av[0], "0x%x", &loadpoint);
		lpflag++;
		av++;
		ac--;
	}
	if (ac == 0)
		usage();
	from1 = open(av[0], O_RDONLY, 0);
	if (from1 < 0) {
		perror("open");
		exit(1);
	}
	n1 = av[0];
	av++;
	ac--;
	if (ac == 0)
		usage();
	if (ac == 2) {
		from2 = open(av[0], O_RDONLY, 0);
		if (from2 < 0) {
			perror("open");
			exit(1);
		}
		n2 = av[0];
		av++;
		ac--;
	} else
		from2 = -1;
	to = open(av[0], O_WRONLY | O_TRUNC | O_CREAT, 0644);
	if (to < 0) {
		perror("open");
		exit(1);
	}
	/* clear possibly unused directory entries */
	strncpy(lifd[1].dir_name, "          ", 10);
	lifd[1].dir_type = -1;
	lifd[1].dir_addr = 0;
	lifd[1].dir_length = 0;
	lifd[1].dir_flag = 0xFF;
	lifd[1].dir_exec = 0;
	lifd[7] = lifd[6] = lifd[5] = lifd[4] = lifd[3] = lifd[2] = lifd[1];
	/* record volume info */
	lifv.vol_id = VOL_ID;
	strncpy(lifv.vol_label, "BOOT43", 6);
	lifv.vol_addr = btolifs(LIF_DIRSTART);
	lifv.vol_oct = VOL_OCT;
	lifv.vol_dirsize = btolifs(LIF_DIRSIZE);
	lifv.vol_version = 1;
	/* output bootfile one */
	lseek(to, LIF_FILESTART, 0);
	putfile(from1, to);
	n = btolifs(ld.count + sizeof(ld));
	strcpy(lifd[0].dir_name, lifname(n1));
	lifd[0].dir_type = DIR_TYPE;
	lifd[0].dir_addr = btolifs(LIF_FILESTART);
	lifd[0].dir_length = n;
	bcddate(from1, lifd[0].dir_toc);
	lifd[0].dir_flag = DIR_FLAG;
	lifd[0].dir_exec = lpflag? loadpoint + ex.a_entry : ex.a_entry;
	lifv.vol_length = lifd[0].dir_addr + lifd[0].dir_length;
	/* if there is an optional second boot program, output it */
	if (from2 >= 0) {
		lseek(to, LIF_FILESTART+lifstob(n), 0);
		putfile(from2, to);
		n = btolifs(ld.count + sizeof(ld));
		strcpy(lifd[1].dir_name, lifname(n2));
		lifd[1].dir_type = DIR_TYPE;
		lifd[1].dir_addr = lifv.vol_length;
		lifd[1].dir_length = n;
		bcddate(from2, lifd[1].dir_toc);
		lifd[1].dir_flag = DIR_FLAG;
		lifd[1].dir_exec = lpflag? loadpoint + ex.a_entry : ex.a_entry;
		lifv.vol_length = lifd[1].dir_addr + lifd[1].dir_length;
	}
	/* output volume/directory header info */
	lseek(to, LIF_VOLSTART, 0);
	write(to, &lifv, LIF_VOLSIZE);
	lseek(to, LIF_DIRSTART, 0);
	write(to, lifd, LIF_DIRSIZE);
	exit(0);
}

putfile(from, to)
{
	register int n, tcnt, dcnt;

	n = read(from, &ex, sizeof(ex));
	if (n != sizeof(ex)) {
		fprintf(stderr, "error reading file header\n");
		exit(1);
	}
	if (ex.a_magic == OMAGIC) {
		tcnt = ex.a_text;
		dcnt = ex.a_data;
	}
	else if (ex.a_magic == NMAGIC) {
		tcnt = (ex.a_text + PGOFSET) & ~PGOFSET;
		dcnt = ex.a_data;
	}
	else {
		fprintf(stderr, "bad magic number\n");
		exit(1);
	}
	ld.address = lpflag ? loadpoint : ex.a_entry;
	ld.count = tcnt + dcnt;
	write(to, &ld, sizeof(ld));
	while (tcnt) {
		n = sizeof(buf);
		if (n > tcnt)
			n = tcnt;
		n = read(from, buf, n);
		if (n < 0) {
			perror("read");
			exit(1);
		}
		if (n == 0) {
			fprintf(stderr, "short read\n");
			exit(1);
		}
		if (write(to, buf, n) < 0) {
			perror("write");
			exit(1);
		}
		tcnt -= n;
	}
	while (dcnt) {
		n = sizeof(buf);
		if (n > dcnt)
			n = dcnt;
		n = read(from, buf, n);
		if (n < 0) {
			perror("read");
			exit(1);
		}
		if (n == 0) {
			fprintf(stderr, "short read\n");
			exit(1);
		}
		if (write(to, buf, n) < 0) {
			perror("write");
			exit(1);
		}
		dcnt -= n;
	}
}

usage()
{
	fprintf(stderr,
		"usage:  mkboot [-l loadpoint] prog1 [ prog2 ] outfile\n");
	exit(1);
}

char *
lifname(str)
 char *str;
{
	static char lname[10] = "SYS_XXXXX";
	register int i;

	for (i = 4; i < 9; i++) {
		if (islower(*str))
			lname[i] = toupper(*str);
		else if (isalnum(*str) || *str == '_')
			lname[i] = *str;
		else
			break;
		str++;
	}
	for ( ; i < 10; i++)
		lname[i] = '\0';
	return(lname);
}

#include <sys/stat.h>
#include <time.h>	/* XXX */

bcddate(fd, toc)
	int fd;
	char *toc;
{
	struct stat statb;
	struct tm *tm;

	fstat(fd, &statb);
	tm = localtime(&statb.st_ctime);
	*toc = ((tm->tm_mon+1) / 10) << 4;
	*toc++ |= (tm->tm_mon+1) % 10;
	*toc = (tm->tm_mday / 10) << 4;
	*toc++ |= tm->tm_mday % 10;
	*toc = (tm->tm_year / 10) << 4;
	*toc++ |= tm->tm_year % 10;
	*toc = (tm->tm_hour / 10) << 4;
	*toc++ |= tm->tm_hour % 10;
	*toc = (tm->tm_min / 10) << 4;
	*toc++ |= tm->tm_min % 10;
	*toc = (tm->tm_sec / 10) << 4;
	*toc |= tm->tm_sec % 10;
}
