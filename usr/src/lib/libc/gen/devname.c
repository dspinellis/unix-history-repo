/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)devname.c	5.8 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <paths.h>

/*
 * Routine to convert a major+minor device number (st_rdev field)
 * plus a mode (S_IFCHR or S_IFBLK) into a name relative to /dev.
 *
 * We build a hash table of everything in /dev, with the hash being
 * a function of the number and mode.
 */

#define	HASHSIZ	512		/* MUST BE A POWER OF 2 */
#define	hash(x, t)	((((t) >> 14) + 4*minor(x) + major(x)) & (HASHSIZ-1))

struct devs {
	struct	devs *next;
	dev_t	dev;
	mode_t	type;
	char	*name;
};

static struct devs *devhash[HASHSIZ];

#ifdef TEST
int	chainlen[HASHSIZ];
int	verbose;
#endif

static int
add(type, dev, name)
	mode_t type;
	dev_t dev;
	char *name;
{
	register struct devs *devp, **p;
	int h;

	devp = (struct devs *)malloc(sizeof *devp);
	if (devp == NULL)
		return (0);
	devp->next = NULL;
	devp->dev = dev;
	devp->type = type;
	devp->name = strdup(name);
	h = hash(dev, type);
	for (p = &devhash[h]; *p; p = &(*p)->next)
		/* void */;
	*p = devp;
#ifdef TEST
	chainlen[h]++;
	if (verbose)
		(void) printf("adding %c %d,%d %s (hash=%d)\n",
		    type == S_IFBLK ? 'b': 'c', major(dev), minor(dev),
		    name, h);
#endif
	return (1);
}

static int
init_by_stat()
{
	register struct dirent *entry;
	struct stat sb;
	DIR *dp;
	int savewd;
	mode_t specialtype;

	if ((savewd = open(".", O_RDONLY, 0)) == -1)
		return (0);
	if (chdir(_PATH_DEV) == -1) {
		(void) close(savewd);
		return (0);
	}
	if ((dp = opendir(".")) == NULL) {
		(void) fchdir(savewd);
		(void) close(savewd);
		return (0);
	}
	while ((entry = readdir(dp)) != NULL) {
		if (stat(entry->d_name, &sb) == -1)
			continue;
		switch (sb.st_mode & S_IFMT) {
		case S_IFCHR:
			specialtype = S_IFCHR;
			break;
		case S_IFBLK:
			specialtype = S_IFBLK;
			break;
		default:
			continue;
		}
		if (!add(specialtype, sb.st_rdev, entry->d_name))
			break;
	}
	(void) fchdir(savewd);
	(void) close(savewd);
	(void) closedir(dp);
	return (1);
}

static int
init_by_db()
{
	register FILE *fp;
	char type, name[MAXNAMLEN + 1];
	int maj, min;
#define specialtype(c) ((c) == 'b' ? (mode_t)S_IFBLK : (mode_t)S_IFCHR)

	if ((fp = fopen("/var/run/devdatabase", "r")) == NULL)
		return (0);
	while (fscanf(fp, " %c %d,%d %s", &type, &maj, &min, name) == 4)
		if (!add(specialtype(type), makedev(maj, min), name))
			break;
	(void) fclose(fp);
	return (1);
#undef specialtype
}

char *
devname(dev, type)
	dev_t dev;
	mode_t type;
{
	register struct devs *devp;
	static int devinit;

	if (!devinit) {
		if (!init_by_db() && !init_by_stat())
			return (NULL);
		devinit = 1;
	}
	for (devp = devhash[hash(dev, type)]; devp != NULL; devp = devp->next)
		if (dev == devp->dev && type == devp->type)
			return (devp->name);

	return (NULL);
}

#ifdef TEST
main(argc, argv)
	int argc;
	char **argv;
{
	register int i, sum, longest;
	struct stat st;
	char *p, *ttyname();

	if (argc > 1 && strcmp(argv[1], "-v") == 0)
		verbose = 1, argc--, argv++;
	p = argc > 1 ? argv[1] : ttyname(0);
	(void) stat(p, &st);
	(void) printf(" %s \n", devname(st.st_rdev, (mode_t)S_IFCHR));
	longest = sum = 0;
	for (i = 0; i < HASHSIZ; i++) {
		sum += chainlen[i];
		if (chainlen[i] > longest)
			longest = chainlen[i];
	}
	(void) printf("average hash chain length %.2f, longest %d\n",
	    (double)sum / HASHSIZ, longest);
}
#endif
