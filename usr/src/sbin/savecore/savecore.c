static	char *sccsid = "@(#)savecore.c	4.5 (Berkeley) 81/05/14";
/*
 * savecore
 */

#include <stdio.h>
#include <nlist.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <sys/filsys.h>
#include <time.h>

#define	DAY	(60L*60L*24L)
#define	LEEWAY	(3*DAY)

#define eq(a,b) (!strcmp(a,b))
#define ok(number) ((number)&0x7fffffff)

#define SHUTDOWNLOG "/usr/adm/shutdownlog"

struct nlist nl[] = {
#define X_DUMPDEV	0
	{ "_dumpdev" },
#define X_DUMPLO	1
	{ "_dumplo" },
#define X_TIME		2
	{ "_time" },
#define X_PHYSMEM	3
	{ "_physmem" },
#define X_VERSION	4
	{ "_version" },
#define X_PANICSTR	5
	{ "_panicstr" },
	{ 0 },
};

char	*dirname;			/* directory to save dumps in */
char	*ddname;			/* name of dump device */
char	*find_dev();
dev_t	dumpdev;			/* dump device */
time_t	dumptime;			/* time the dump was taken */
int	dumplo;				/* where dump starts on dumpdev */
int	physmem;			/* amount of memory in machine */
time_t	now;				/* current date */
char	*path();
unsigned malloc();
char	*ctime();
char	vers[80];
char	core_vers[80];
char	panic_mesg[80];
int	panicstr;
int	do_the_dump = 1;
off_t	lseek();
off_t	Lseek();

main(argc, argv)
	char **argv;
	int argc;
{

	if (argc != 2) {
		fprintf(stderr, "usage: savecore dirname\n");
		exit(1);
	}
	dirname = argv[1];
	if (access(dirname, 2) < 0) {
		perror(dirname);
		exit(1);
	}
	(void) time(&now);
	read_kmem();
	log_entry();
	if (do_the_dump && get_crashtime() && check_space())
		save_core();
}

char *
find_dev(dev, type)
	register dev_t dev;
	register int type;
{
	register int dfd = Open("/dev", 0);
	struct direct dir;
	struct stat statb;
	static char devname[DIRSIZ + 1];
	char *dp;

	strcpy(devname, "/dev/");
	while(Read(dfd, (char *)&dir, sizeof dir) > 0) {
		if (dir.d_ino == 0)
			continue;
		strncpy(devname + 5, dir.d_name, DIRSIZ);
		devname[DIRSIZ] = '\0';
		if (stat(devname, &statb)) {
			perror(devname);
			continue;
		}
		if ((statb.st_mode&S_IFMT) != type)
			continue;
		if (dev == statb.st_rdev) {
			close(dfd);
			dp = (char *)malloc(strlen(devname)+1);
			strcpy(dp, devname);
			return dp;
		}
	}
	close(dfd);
	fprintf(stderr, "Can't find device %d,%d\n", major(dev), minor(dev));
	exit(1);
	/*NOTREACHED*/
}

read_kmem()
{
	int kmem;
	FILE *fp;
	register char *cp;

	nlist("/vmunix", nl);
	if (nl[X_DUMPDEV].n_value == 0) {
		fprintf(stderr, "/vmunix: dumpdev not in namelist\n");
		exit(1);
	}
	if (nl[X_DUMPLO].n_value == 0) {
		fprintf(stderr, "/vmunix: dumplo not in namelist\n");
		exit(1);
	}
	if (nl[X_TIME].n_value == 0) {
		fprintf(stderr, "/vmunix: time not in namelist\n");
		exit(1);
	}
	if (nl[X_PHYSMEM].n_value == 0) {
		fprintf(stderr, "/vmunix: physmem not in namelist\n");
		exit(1);
	}
	if (nl[X_VERSION].n_value == 0) {
		fprintf(stderr, "/vmunix: version not in namelist\n");
		exit(1);
	}
	if (nl[X_PANICSTR].n_value == 0) {
		fprintf(stderr, "/vmunix: panicstr not in namelist\n");
		exit(1);
	}
	kmem = Open("/dev/kmem", 0);
	Lseek(kmem, (long)nl[X_DUMPDEV].n_value, 0);
	Read(kmem, (char *)&dumpdev, sizeof dumpdev);
	Lseek(kmem, (long)nl[X_DUMPLO].n_value, 0);
	Read(kmem, (char *)&dumplo, sizeof dumplo);
	Lseek(kmem, (long)nl[X_PHYSMEM].n_value, 0);
	Read(kmem, (char *)&physmem, sizeof physmem);
	dumplo *= 512L;
	ddname = find_dev(dumpdev, S_IFBLK);
	if ((fp = fdopen(kmem, "r")) == NULL) {
		fprintf(stderr, "Couldn't fdopen kmem\n");
		exit(1);
	}
	fseek(fp, (long)nl[X_VERSION].n_value, 0);
	fgets(vers, sizeof vers, fp);
	fclose(fp);
	if ((fp = fopen(ddname, "r")) == NULL) {
		perror(ddname);
		exit(1);
	}
	fseek(fp, (off_t)(dumplo+ok(nl[X_VERSION].n_value)), 0);
	fgets(core_vers, sizeof core_vers, fp);
	fclose(fp);
	if (!eq(vers, core_vers))
		fprintf(stderr, "Warning: vmunix version mismatch:\n\t%sand\n\t%s",
		    vers,core_vers);
	fp = fopen(ddname, "r");
	fseek(fp, (off_t)(dumplo + ok(nl[X_PANICSTR].n_value)), 0);
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

	dumpfd = Open(ddname, 2);
	Lseek(dumpfd, (off_t)(dumplo + ok(nl[X_TIME].n_value)), 0);
	Read(dumpfd, (char *)&dumptime, sizeof dumptime);
	Lseek(dumpfd, (off_t)(dumplo + ok(nl[X_TIME].n_value)), 0);
	Write(dumpfd, (char *)&clobber, sizeof clobber);
	close(dumpfd);
	if (dumptime == 0)
		return 0;
	printf("System went down at %s", ctime(&dumptime));
	if (dumptime < now - LEEWAY || dumptime > now + LEEWAY) {
		printf("Dump time is unreasonable\n");
		return 0;
	}
	return 1;
}

char *
path(file)
	char *file;
{
	register char *cp = (char *)malloc(strlen(file) + strlen(dirname) + 2);

	(void) strcpy(cp, dirname);
	(void) strcat(cp, "/");
	(void) strcat(cp, file);
	return cp;
}

check_space()
{
	struct stat dsb;
	register char *ddev;
	register int dfd;
	struct filsys sblk;

	if (stat(dirname, &dsb) < 0) {
		perror(dirname);
		exit(1);
	}
	ddev = find_dev(dsb.st_dev, S_IFBLK);
	dfd = Open(ddev, 0);
	Lseek(dfd, 1L<<BSHIFT, 0);
	Read(dfd, (char *)&sblk, sizeof sblk);
	close(dfd);
	if (read_number("minfree") > sblk.s_tfree) {
		fprintf(stderr, "Dump omitted, not enough space on device\n");
		return (0);
	}
	return (1);
}

read_number(fn)
	char *fn;
{
	char lin[80];
	register FILE *fp;

	if ((fp = fopen(path(fn), "r")) == NULL)
		return 0;
	if (fgets(lin, 80, fp) == NULL) {
		fclose(fp);
		return 0;
	}
	fclose(fp);
	return atoi(lin);
}

save_core()
{
	register int n;
	char buffer[32*NBPG];
	register char *cp = buffer;
	register int ifd, ofd, bounds;
	register FILE *fp;

	bounds = read_number("bounds");
	ifd = Open("/vmunix", 0);
	ofd = Create(path(sprintf(cp, "vmunix.%d", bounds)), 0666);
	while((n = Read(ifd, cp, BUFSIZ)) > 0)
		Write(ofd, cp, n);
	close(ifd);
	close(ofd);
	ifd = Open(ddname, 0);
	ofd = Create(path(sprintf(cp, "vmcore.%d", bounds)), 0666);
	Lseek(ifd, (off_t)dumplo, 0);
	printf("Saving %d bytes of image in vmcore.%d\n", NBPG*physmem, bounds);
	while(physmem > 0) {
		n = Read(ifd, cp, (physmem > 32 ? 32 : physmem) * NBPG);
		Write(ofd, cp, n);
		physmem -= n/NBPG;
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
