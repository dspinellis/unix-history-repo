/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)devname.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <dirent.h>
#include <paths.h>

static struct devs {
	struct	devs *next;
	dev_t	dev;
	char	name[MAXNAMLEN+1];
	mode_t	type;
};

#define	hash(x)	((x)&0xff)
static struct devs *devhash[0xff];

static int devinit;

char *
devname(dev, type)
	dev_t dev;
	mode_t type;
{
	struct devs *devp;

	if (devinit == 0) {
		register struct devs *devpp;
		register struct dirent *entry;
		struct stat sb;
		DIR *dp = opendir(_PATH_DEV);
		int savewd = open(".", O_RDONLY, 0);
		mode_t specialtype;

		if (savewd == -1 || dp == NULL || chdir(_PATH_DEV) == -1)
			return (NULL);
		while ((entry = readdir(dp)) != NULL) {
			if (stat(entry->d_name, &sb) == -1)
				continue;
			switch(sb.st_mode&S_IFMT) {
			case S_IFCHR:
				specialtype = S_IFCHR;
				break;
			case S_IFBLK:
				specialtype = S_IFBLK;
				break;
			default:
				continue;
			}
			devp = (struct devs *)malloc(sizeof (struct devs));
			if (devp == NULL)
				return (NULL);
			devp->type = specialtype;
			devp->dev = sb.st_rdev;
			strcpy(devp->name, entry->d_name);
			devp->next = NULL;
			if ((devpp = devhash[hash(sb.st_rdev)]) == NULL)
				devhash[hash(sb.st_rdev)] = devp;
			else {
				for (;devpp->next != NULL; devpp = devpp->next)
					;
				devpp->next = devp;
			}
		}
		fchdir(savewd);
		close(savewd);
		closedir(dp);
		devinit = 1;
	}
	for (devp = devhash[hash(dev)]; devp != NULL; devp = devp->next)
		if (dev == devp->dev && type == devp->type)
			return(devp->name);

	return (NULL);
}

#ifdef TEST
main() {
	printf(" %s \n", devname(0));
}
#endif
