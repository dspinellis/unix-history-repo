#ifndef lint
    static	char *sccsid = "@(#)hertz.c	1.1 (Berkeley) %G%";
#endif lint

    /*
     *	discover the tick frequency of the machine
     */
#include <nlist.h>
#include <stdio.h>

struct nlist	nl[] =	{{"_hz"},	/* clock ticks per second */
			 {0}};

hertz()
{
    int		kmem;			/* file descriptor for /dev/kmem */
    long	lseek();
    long	seeked;			/* return value from lseek() */
    long	hz;			/* buffer for reading from system */
    int		red;			/* return value from read() */
    int		closed;			/* return value from close() */

#   define	VMUNIX	"/vmunix"	/* location of the system namelist */
    nlist(VMUNIX, nl);
    if (nl[0].n_type == 0) {
	fprintf(stderr, "no %s namelist entry for _hz\n", VMUNIX);
	return 0;
    }
#   define	KMEM	"/dev/kmem"	/* location of the system data space */
    kmem = open(KMEM, 0);
    if (kmem == -1) {
	perror("hertz()");
	fprintf(stderr, "open(\"%s\", 0)", KMEM);
	return 0;
    }
    seeked = lseek(kmem, nl[0].n_value, 0);
    if (seeked == -1) {
	fprintf(stderr, "can't lseek(kmem, 0x%x, 0)\n", nl[0].n_value);
	return 0;
    }
    red = read(kmem, &hz, sizeof hz);
    if (red != sizeof hz) {
	fprintf(stderr, "read(kmem, 0x%x, %d) returned %d\n",
		&hz, sizeof hz, red);
	return 0;
    }
    closed = close(kmem);
    if (closed != 0) {
	perror("hertz()");
	fprintf(stderr, "close(\"%s\")", KMEM);
	return 0;
    }
    return hz;
}
