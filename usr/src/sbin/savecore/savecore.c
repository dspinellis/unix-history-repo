/*
 *	savecore.c	4.2	81/04/03
 * savecore dirname
 *	Written by Michael Toy (UCB)
 *	Program meant to be called from the /etc/rc file for saving the
 * dump of a crashed system.  If the core file has not already been saved
 * then save it in dirname (if there is at least minfree blocks on the
 * device the directory is on.)
 *	1) Make certain "dirname" exists
 *	2) Get dumpdev and dumplo from vmunix/kmem
 *	3) Find dump device name get time from core image
 *	4) Look in "dirname" generate a name se
 *		vmunix.n
 *		vmcore.n
 *	5) Check in "dirname"/minfree to be certain there is space
 *	6) Make entry in shutdown log with date and cause of crash
 */

#include <stdio.h>
#include <nlist.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <sys/filsys.h>
#include <time.h>

#define LEEWAY (60L*60L*24L*3L)	/* Maximum reasonable dump age diff (3 days )*/
#define eq(a,b) (strcmp(a,b)==0)
#define ok(number) (number&0x7fffffff)
#define SHUTDOWNLOG "/usr/adm/shutdownlog"
#define TRUE (1)
#define FALSE (0)

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

char *dirname;				/* Directory to save dumps in */
char *ddname;				/* Name of dump device */
char *find_dev();
int minfree;				/* Minimum free blocks on device */
dev_t dumpdev;				/* Dump device */
time_t dumptime;			/* Time the dump was taken */
int dumplo;				/* Where dump starts on dumpdev */
int physmem;				/* Amount of memory in machine */
time_t now;				/* Current date */
char *path(), *malloc();
char vers[80], core_vers[80];
char panic_mesg[80];
int panicstr;
int do_the_dump = TRUE;

main(argc, argv)
char **argv;
int argc;
{
	if (argc != 2)
	{
		fprintf(stderr, "usage: savecore dirname\n");
		exit(1);
	}
	dirname = argv[1];
	if (access(dirname, 2) < 0)
	{
		perror(dirname);
		exit(4);
	}
	/*
	 * Now invoke the local dieties so that things get done
	 */
	time(&now);
	read_kmem();
	log_entry();
	if (do_the_dump && get_crashtime() && check_space())
		save_core();
}

/*
 * find_dev
 *	Lookup a dev in the /dev directory, return the dev name
 */

char *find_dev(dev, type)
register dev_t dev;
register int type;
{
	register int dfd = Open("/dev", 0);
	struct direct dir;
	struct stat statb;
	static char devname[DIRSIZ + 1];

	strcpy(devname, "/dev/");
	while(Read(dfd, &dir, sizeof dir) > 0)
	{
		if (dir.d_ino == 0)
			continue;
		strncpy(devname + 5, dir.d_name, DIRSIZ);
		devname[DIRSIZ] = '\0';
		if (stat(devname, &statb))
			perror(devname);
		else
		{
			if ((statb.st_mode&S_IFMT) != type)
				continue;
			if (dev == statb.st_rdev)
			{
				close(dfd);
				return devname;
			}
		}
	}
	close(dfd);
	fprintf(stderr, "Can't find device %d,%d\n", major(dev), minor(dev));
	exit(7);
}

/*
 * Open
 *	Open and exit if open fails
 */

Open(name, rw)
char *name;
int rw;
{
	int fd;

	if ((fd = open(name, rw)) < 0)
	{
		perror(name);
		exit(2);
	}
	return fd;
}

/*
 * Read, like read but checks bad return codes
 */

Read(fd, buff, size)
int fd, size;
char *buff;
{
	int ret;

	if ((ret = read(fd, buff, size)) < 0)
	{
		perror("reading");
		exit(3);
	}
	return ret;
}

/*
 * Lseek
 *	A "safe" lseek
 */

long Lseek(fd, off, flag)
int fd, flag;
long off;
{
	long ret;

	if ((ret = lseek(fd, off, flag)) == -1L)
	{
		perror("lseek");
		exit(5);
	}
	return ret;
}

Create(file, mode)
char *file;
int mode;
{
	register int fd;

	if ((fd = creat(file, mode)) < 0)
	{
		perror(file);
		exit(9);
	}
	return fd;
}

Write(fd, buf, size)
int fd, size;
char *buf;
{
	if (write(fd, buf, size) < size)
	{
		perror("Writing");
		exit(10);
	}
}

/*
 * Get dumpdev and dumplo from kmem/vmunix
 */

read_kmem()
{
	int kmem;
	FILE *fp;
	register char *cp;

	nlist("/vmunix", nl);
	if (nl[X_DUMPDEV].n_value == 0)
	{
	    fprintf(stderr, "savecore: dumpdev not in namelist\n");
	    exit(6);
	}
	if (nl[X_DUMPLO].n_value == 0)
	{
	    fprintf(stderr, "savecore: dumplo not in namelist\n");
	    exit(6);
	}
	if (nl[X_TIME].n_value == 0)
	{
	    fprintf(stderr, "savecore: time not in namelist\n");
	    exit(6);
	}
	if (nl[X_PHYSMEM].n_value == 0)
	{
		fprintf("savecore: physmem not in namelist\n");
		exit(6);
	}
	if (nl[X_VERSION].n_value == 0)
	{
		fprintf("savecore: version not in namelist\n");
		exit(6);
	}
	if (nl[X_PANICSTR].n_value == 0)
	{
		fprintf("savecore: panicstr not in namelist\n");
		exit(6);
	}
	kmem = Open("/dev/kmem", 0);
	Lseek(kmem, nl[X_DUMPDEV].n_value, 0);
	Read(kmem, &dumpdev, sizeof dumpdev);
	Lseek(kmem, nl[X_DUMPLO].n_value, 0);
	Read(kmem, &dumplo, sizeof dumplo);
	Lseek(kmem, nl[X_PHYSMEM].n_value, 0);
	Read(kmem, &physmem, sizeof physmem);
	Lseek(kmem, nl[X_PANICSTR].n_value, 0);
	Read(kmem, &panicstr, sizeof panicstr);
	dumplo *= 512L;
	ddname = find_dev(dumpdev, S_IFBLK);
	/*
	 * Check for version mismatch
	 */
	if ((fp = fdopen(kmem, "r")) == NULL)
	{
		fprintf(stderr, "Couldn't fdopen kmem\n");
		exit(11);
	}
	fseek(fp, nl[X_VERSION].n_value, 0);
	fgets(vers, sizeof vers, fp);
	fclose(fp);
	if ((fp = fopen(ddname, "r")) == NULL)
	{
		perror(ddname);
		exit(12);
	}
	fseek(fp, dumplo+ok(nl[X_VERSION].n_value), 0);
	fgets(core_vers, sizeof core_vers, fp);
	fclose(fp);
	if (!eq(vers, core_vers))
	{
		fprintf(stderr, "Version mismatch:\n\t%sand\n\t%s",
				vers,core_vers);
		fprintf(stderr, "Core not saved\n");
		do_the_dump = FALSE;
		return;
	}
	/*
	 * Now check the panic string
	 */
	if (panicstr)
	{
		fp = fopen(ddname, "r");
		fseek(fp, dumplo + ok(panicstr));
		cp = panic_mesg;
		do
			*cp = getc(fp);
		while (*cp++);
		fclose(fp);
	}
}	

/*
 * Now get the time of the crash
 */

 get_crashtime()
 {
	int dumpfd;
	time_t clobber = (time_t)0;

	dumpfd = Open(ddname, 2);
	Lseek(dumpfd, dumplo + ok(nl[X_TIME].n_value), 0);
	Read(dumpfd, &dumptime, sizeof dumptime);
	Lseek(dumpfd, dumplo + ok(nl[X_TIME].n_value), 0);
	Write(dumpfd, &clobber, sizeof clobber);
	close(dumpfd);
	printf("System went down at %s", ctime(&dumptime));
	if (dumptime < now - LEEWAY || dumptime > now + LEEWAY) {
		printf("Dump time is unreasonable\n");
		return FALSE;
	} else {
		printf("Dump time ok.\n");
		return TRUE;
	}
}

/*
 * Put a file name in the proper perspective
 */

char *path(file)
{
	register char *cp = malloc(strlen(file) + strlen(dirname) + 2);

	strcpy(cp, dirname);
	strcat(cp, "/");
	strcat(cp, file);
	return cp;
}

/*
 * Check to make certain that there is enough space for this dump
 */

check_space()
{
	struct stat dsb;
	register char *ddev;
	register int dfd;
	struct filsys sblk;

	/*
	 * First find the number of free blocks
	 */
	stat(dirname, &dsb);
	ddev = find_dev(dsb.st_dev, S_IFBLK);
	dfd = Open(ddev, 0);
	Lseek(dfd, 1L<<BSHIFT, 0);
	Read(dfd, &sblk, sizeof sblk);
	close(dfd);
	/*
	 * Now check against maximum allowed
	 */
	if (read_number("minfree") > sblk.s_tfree)
	{
		fprintf(stderr, "*** Dump not done, not enough space ***\n");
		return FALSE;
	}
	else
		return TRUE;
}

/*
 * Read a number from a file
 */

read_number(fn)
char *fn;
{
	char lin[80];
	register FILE *fp;

	if ((fp = fopen(path(fn), "r")) == NULL)
		return 0;
	else
	{
		if (fgets(lin, 80, fp) == NULL)
		{
			fclose(fp);
			return 0;
		}
		else
		{
			fclose(fp);
			return atoi(lin);
		}
	}
}

save_core()
{
	register int n;
	char buffer[BUFSIZ];
	register char *cp = buffer;
	register int ifd, ofd, bounds;
	register FILE *fp;

	bounds = read_number("bounds");
	/*
	 * Copy the vmunix file
	 */
	ifd = Open("/vmunix", 0);
	ofd = Create(path(sprintf(cp, "vmunix.%d", bounds)), 0666);
	while((n = Read(ifd, cp, BUFSIZ)) > 0)
		Write(ofd, cp, n);
	close(ifd);
	close(ofd);
	/*
	 * Make the core file
	 */
	ifd = Open(ddname, 0);
	ofd = Create(path(sprintf(cp, "vmcore.%d", bounds)), 0666);
	Lseek(ifd, dumplo, 0);
	printf("Vmcore should be %d bytes long\n", NBPG * physmem);
	while(physmem > 0)
	{
		n = Read(ifd, cp, BUFSIZ);
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
	fp = fopen(SHUTDOWNLOG, "a");
	fseek(fp, 0L, 2);
	fprintf(fp, "%02d:%02d  %s %s %2d, %4d.  Reboot", tm->tm_hour,
		tm->tm_min, days[tm->tm_wday], months[tm->tm_mon],
		tm->tm_mday, tm->tm_year + 1900);
	if (panicstr)
		fprintf(fp, " -- panic %s\n", panic_mesg);
	else
		putc('\n', fp);
	fclose(fp);
}
