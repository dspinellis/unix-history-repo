static	char *sccsid = "@(#)version.c	2.2 (Berkeley) 84/04/02";

#include <sys/time.h>
#include <stdio.h>

extern char	version[];

main()
{
    long	time();
    long	clock;
    struct tm	*localtime();
    struct tm	*tmp;
    int		major;
    int		minor;

    time(&clock);
    tmp = localtime(&clock);
    sscanf(version, "%d.%d", &major, &minor);
    minor += 1;
    printf("char	version[] = \"%d.%d (%d/%d/%d)\";\n",
	    major, minor, tmp->tm_mon+1, tmp->tm_mday, tmp->tm_year);
}
