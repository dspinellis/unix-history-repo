/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)savecore.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * savecore
 */

#include <stdio.h>
#include <nlist.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <sys/time.h>

#define	DAY	(60L*60L*24L)
#define	LEEWAY	(3*DAY)

#define eq(a,b) (!strcmp(a,b))
#ifdef vax
#define ok(number) ((number)&0x7fffffff)
#else
#define ok(number) (number)
#endif

#define SHUTDOWNLOG "/usr/adm/shutdownlog"

struct nlist current_nl[] = {	/* namelist for currently running system */
#define X_DUMPDEV	0
	{ "_dumpdev" },
#define X_DUMPLO	1
	{ "_dumplo" },
#define X_TIME		2
	{ "_time" },
#define	X_DUMPSIZE	3
	{ "_dumpsize" },
#define X_VERSION	4
	{ "_version" },
#define X_PANICSTR	5
	{ "_panicstr" },
#define	X_DUMPMAG	6
	{ "_dumpmag" },
	{ "" },
};

struct nlist dump_nl[] = {	/* name list for dumped system */
	{ "_dumpdev" },		/* entries MUST be the same as */
	{ "_dumplo" },		/*	those in current_nl[]  */
	{ "_time" },
	{ "_dumpsize" },
	{ "_version" },
	{ "_panicstr" },
	{ "_dumpmag" },
	{ "" },
};

char	*system;
char	*dirname;			/* directory to save dumps in */
char	*ddname;			/* name of dump device */
char	*find_dev();
dev_t	dumpdev;			/* dump device */
time_t	dumptime;			/* time the dump was taken */
int	dumplo;				/* where dump starts on dumpdev */
int	dumpsize;			/* amount of memory dumped */
int	dumpmag;			/* magic number in dump */
time_t	now;				/* current date */
char	*path();
unsigned malloc();
char	*ctime();
char	vers[80];
char	core_vers[80];
char	panic_mesg[80];
int	panicstr;
off_t	lseek();
off_t	Lseek();
int	Verbose;

main(argc, argv)
	char **argv;
	int argc;
{

	while ((argc > 1) && (argv[1][0] == '-')) {
		switch (argv[1][1]) {
		case 'v':
		case 'd':
			Verbose = 1;
			break;
		default:
			fprintf(stderr, "savecore: illegal flag -%c\n",
				argv[1][1]);
			fprintf(stderr,
				"usage: savecore [-v] dirname [ system ]\n");
			exit(1);
		}
		argc--;
		argv++;
	}

	if (argc != 2 && argc != 3) {
		fprintf(stderr, "usage: savecore [-v] dirname [ system ]\n");
		exit(1);
	}
	dirname = argv[1];
	if (argc == 3)
		system = argv[2];
	if (access(dirname, 2) < 0) {
		perror(dirname);
		exit(1);
	}
	read_kmem();
}

int
dump_exists()
{
	register int dumpfd;
	int word;

	dumpfd = Open(ddname, 0);
	Lseek(dumpfd, (off_t)(dumplo + ok(dump_nl[X_DUMPMAG].n_value)), 0);
	Read(dumpfd, (char *)&word, sizeof word);
	close(dumpfd);
	if (Verbose && (word != dumpmag)) {
		printf("dumplo = %d (%d bytes)\n", dumplo/512, dumplo);
		printf("magic number mismatch: %x != %x\n", word, dumpmag);
	}
	return (word == dumpmag);
}

clear_dump()
{
	register int dumpfd;
	int zero = 0;

	dumpfd = Open(ddname, 1);
	Lseek(dumpfd, (off_t)(dumplo + ok(dump_nl[X_DUMPMAG].n_value)), 0);
	Write(dumpfd, (char *)&zero, sizeof zero);
	close(dumpfd);
}

char *
find_dev(dev, type)
	register dev_t dev;
	register int type;
{
	struct stat statb;
	char *dp;

	strcpy(devname, "/dev/");
		if (stat(devname, &statb)) {
			perror(devname);
			continue;
		}
		if ((statb.st_mode&S_IFMT) != type)
			continue;
		if (dev == statb.st_rdev) {
			dp = (char *)malloc(strlen(devname)+1);
			strcpy(dp, devname);
			return dp;
		}
	}
	fprintf(stderr, "savecore: Can't find device %d,%d\n",
		major(dev), minor(dev));
	exit(1);
	/*NOTREACHED*/
}

read_kmem()
{
	int kmem;
	FILE *fp;
	register char *cp;
	char *dump_sys;
	
	dump_sys = system ? system : "/vmunix";

	nlist("/vmunix", current_nl);
	nlist(dump_sys, dump_nl);

	/*
	 * Some names we need for the currently running system,
	 * others for the system that was running when the dump was made.
	 * The values obtained from the current system are used
	 * to look for things in /dev/kmem that cannot be found
	 * in the dump_sys namelist, but are presumed to be the same
	 * (since the disk partitions are probably the same!)
	 */
	if (current_nl[X_DUMPDEV].n_value == 0) {
		fprintf(stderr, "savecore: /vmunix: dumpdev not in namelist\n");
		exit(1);
	}
	if (current_nl[X_DUMPLO].n_value == 0) {
		fprintf(stderr, "savecore: /vmunix: dumplo not in namelist\n");
		exit(1);
	}
	if (dump_nl[X_TIME].n_value == 0) {
		fprintf(stderr, "savecore: %s: time not in namelist\n",
				dump_sys);
		exit(1);
	}
	if (dump_nl[X_DUMPSIZE].n_value == 0) {
		fprintf(stderr, "savecore: %s: dumpsize not in namelist\n",
				dump_sys);
		exit(1);
	}
	/* we need VERSION in both images */
	if (current_nl[X_VERSION].n_value == 0) {
		fprintf(stderr, "savecore: /vmunix: version not in namelist\n",
				dump_sys);
		exit(1);
	}
	if (dump_nl[X_VERSION].n_value == 0) {
		fprintf(stderr, "savecore: %s: version not in namelist\n",
				dump_sys);
		exit(1);
	}
	if (dump_nl[X_PANICSTR].n_value == 0) {
		fprintf(stderr, "savecore: %s: panicstr not in namelist\n",
				dump_sys);
		exit(1);
	}
	/* we need DUMPMAG in both images */
	if (current_nl[X_DUMPMAG].n_value == 0) {
		fprintf(stderr, "savecore: /vmunix: dumpmag not in namelist\n");
		exit(1);
	}
	if (dump_nl[X_DUMPMAG].n_value == 0) {
		fprintf(stderr, "savecore: %s: dumpmag not in namelist\n",
				dump_sys);
		exit(1);
	}
	kmem = Open("/dev/kmem", 0);
	Lseek(kmem, (long)current_nl[X_DUMPDEV].n_value, 0);
	Read(kmem, (char *)&dumpdev, sizeof (dumpdev));
	Lseek(kmem, (long)current_nl[X_DUMPLO].n_value, 0);
	Read(kmem, (char *)&dumplo, sizeof (dumplo));
	Lseek(kmem, (long)current_nl[X_DUMPMAG].n_value, 0);
	Read(kmem, (char *)&dumpmag, sizeof (dumpmag));
	dumplo *= 512L;
	ddname = find_dev(dumpdev, S_IFBLK);
	if ((fp = fdopen(kmem, "r")) == NULL) {
		fprintf(stderr, "savecore: Couldn't fdopen kmem\n");
		exit(1);
	}
	if (system)
		return;
	fseek(fp, (long)current_nl[X_VERSION].n_value, 0);
	fgets(vers, sizeof vers, fp);
	fclose(fp);
}

check_kmem()
{
	FILE *fp;
	register char *cp;

	if ((fp = fopen(ddname, "r")) == NULL) {
		perror(ddname);
		exit(1);
	}
	fseek(fp, (off_t)(dumplo+ok(dump_nl[X_VERSION].n_value)), 0);
	fgets(core_vers, sizeof core_vers, fp);
	fclose(fp);
	if (!eq(vers, core_vers) && (system == 0))
		fprintf(stderr,
		   "savecore: Warning: vmunix version mismatch:\n\t%sand\n\t%s",
		   vers, core_vers);
	fp = fopen(ddname, "r");
	fseek(fp, (off_t)(dumplo + ok(dump_nl[X_PANICSTR].n_value)), 0);
	fread((char *)&panicstr, sizeof panicstr, 1, fp);
	if (panicstr) {
		fseek(fp, dumplo + ok(panicstr), 0);
		cp = panic_mesg;
		do
			*cp = getc(fp);
		while (*cp++);
	}
	fclose(fp);
}

get_crashtime()
{
	int dumpfd;
	time_t clobber = (time_t)0;

	Lseek(dumpfd, (off_t)(dumplo + ok(dump_nl[X_TIME].n_value)), 0);
	Read(dumpfd, (char *)&dumptime, sizeof dumptime);
	close(dumpfd);
	if (dumptime == 0) {
		if (Verbose)
			printf("dump time not found\n");
		return (0);
	}
	printf("System went down at %s", ctime(&dumptime));
	if (dumptime < now - LEEWAY || dumptime > now + LEEWAY) {
		printf("Dump time is unreasonable\n");
		return (0);
	}
	return (1);
}

char *
path(file)
	char *file;
{
	register char *cp = (char *)malloc(strlen(file) + strlen(dirname) + 2);

	(void) strcpy(cp, dirname);
	(void) strcat(cp, "/");
	(void) strcat(cp, file);
	return (cp);
}

check_space()
{
	struct stat dsb;
	register char *ddev;
	int dfd, spacefree;
	struct fs fs;

	if (stat(dirname, &dsb) < 0) {
		perror(dirname);
		exit(1);
	}
	ddev = find_dev(dsb.st_dev, S_IFBLK);
	dfd = Open(ddev, 0);
	Lseek(dfd, (long)(SBLOCK * DEV_BSIZE), 0);
	Read(dfd, (char *)&fs, sizeof fs);
	close(dfd);
	spacefree = fs.fs_cstotal.cs_nbfree * fs.fs_bsize / 1024;
	if (read_number("minfree") > spacefree) {
		fprintf(stderr,
		   "savecore: Dump omitted, not enough space on device\n");
		return (0);
	}
	if (fs.fs_cstotal.cs_nbfree * fs.fs_frag + fs.fs_cstotal.cs_nffree <
	    fs.fs_dsize * fs.fs_minfree / 100)
		fprintf(stderr,
			"Dump performed, but free space threshold crossed\n");
	return (1);
}

read_number(fn)
	char *fn;
{
	char lin[80];
	register FILE *fp;

	if ((fp = fopen(path(fn), "r")) == NULL)
		return (0);
	if (fgets(lin, 80, fp) == NULL) {
		fclose(fp);
		return (0);
	}
	fclose(fp);
	return (atoi(lin));
}

save_core()
{
	register int n;
	char buffer[32*NBPG];
	register char *cp = buffer;
	register int ifd, ofd, bounds;
	register FILE *fp;

	bounds = read_number("bounds");
	ifd = Open(system?system:"/vmunix", 0);
	while((n = Read(ifd, cp, BUFSIZ)) > 0)
		Write(ofd, cp, n);
	close(ifd);
	close(ofd);
	ifd = Open(ddname, 0);
	Lseek(ifd, (off_t)(dumplo + ok(dump_nl[X_DUMPSIZE].n_value)), 0);
	Read(ifd, (char *)&dumpsize, sizeof (dumpsize));
	sprintf(cp, "vmcore.%d", bounds);
	ofd = Create(path(cp), 0644);
	Lseek(ifd, (off_t)dumplo, 0);
	printf("Saving %d bytes of image in vmcore.%d\n", NBPG*dumpsize,
		bounds);
	while (dumpsize > 0) {
		n = Read(ifd, cp, (dumpsize > 32 ? 32 : dumpsize) * NBPG);
		Write(ofd, cp, n);
		dumpsize -= n/NBPG;
	}
	close(ifd);
	close(ofd);
	fp = fopen(path("bounds"), "w");
	fprintf(fp, "%d\n", bounds+1);
	fclose(fp);
}

char *days[] = {
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};

char *months[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
	"Oct", "Nov", "Dec"
};

log_entry()
{
	FILE *fp;
	struct tm *tm, *localtime();

	tm = localtime(&now);
	fp = fopen("/usr/adm/shutdownlog", "a");
	if (fp == 0)
		return;
	fseek(fp, 0L, 2);
	fprintf(fp, "%02d:%02d  %s %s %2d, %4d.  Reboot", tm->tm_hour,
		tm->tm_min, days[tm->tm_wday], months[tm->tm_mon],
		tm->tm_mday, tm->tm_year + 1900);
	if (panicstr)
		fprintf(fp, " after panic: %s\n", panic_mesg);
	else
		putc('\n', fp);
	fclose(fp);
}

/*
 * Versions of std routines that exit on error.
 */

Open(name, rw)
	char *name;
	int rw;
{
	int fd;

	if ((fd = open(name, rw)) < 0) {
		perror(name);
		exit(1);
	}
	return fd;
}

Read(fd, buff, size)
	int fd, size;
	char *buff;
{
	int ret;

	if ((ret = read(fd, buff, size)) < 0) {
		perror("read");
		exit(1);
	}
	return ret;
}

off_t
Lseek(fd, off, flag)
	int fd, flag;
	long off;
{
	long ret;

	if ((ret = lseek(fd, off, flag)) == -1L) {
		perror("lseek");
		exit(1);
	}
	return ret;
}

Create(file, mode)
	char *file;
	int mode;
{
	register int fd;

	if ((fd = creat(file, mode)) < 0) {
		perror(file);
		exit(1);
	}
	return fd;
}

Write(fd, buf, size)
	int fd, size;
	char *buf;
{

	if (write(fd, buf, size) < size) {
		perror("write");
		exit(1);
	}
}
