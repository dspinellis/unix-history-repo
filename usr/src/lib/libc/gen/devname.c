#include <sys/types.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include "pathnames.h"
#include <stdio.h>

struct devs {
	dev_t	dev;
	char	name[MAXNAMLEN+1];
	struct	devs *next;
};

struct devs *devhash[minor(~0)];

int devinit;

/* TODO - use a database file stored in /dev, - just check mod time on /dev 
	hide data structures 
*/

char *
devname(dev)
	dev_t dev;
{
	struct devs *devp;

	if (devinit == 0) {
		DIR *dp = opendir(_PATH_DEV);
		struct direct *entry;
		struct stat sb;
		struct devs *devpp;

		/* XXX XXX - can't chdir as a library routine (add flag?) */
		if (dp == NULL || chdir(_PATH_DEV) == -1)
			return (NULL);
		while ((entry = readdir(dp)) != NULL) {
			if (stat(entry->d_name, &sb) == -1)
				continue;
			if ((sb.st_mode&S_IFMT) != S_IFCHR)
				continue;
			devp = (struct devs *)malloc(sizeof (struct devs));
			if (devp == NULL)
				return (NULL);
			devp->dev = sb.st_rdev;
			strcpy(devp->name, entry->d_name);
			devp->next = NULL;
			if ((devpp = devhash[minor(sb.st_rdev)]) == NULL)
				devhash[minor(sb.st_rdev)] = devp;
			else {
				for (;devpp->next != NULL; devpp = devpp->next)
					;
				devpp->next = devp;
			}
		}
		devinit = 1; 	/* XXX - should have way to invalidate cache ?*/
	}
	for (devp = devhash[minor(dev)]; devp != NULL; devp = devp->next)
		if (dev == devp->dev)
			return(devp->name);

	return (NULL);
}

#ifdef TEST
main() {
	printf(" %s \n", devname(0));
}
#endif
