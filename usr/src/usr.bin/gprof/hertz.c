#ifndef lint
    static	char *sccsid = "@(#)hertz.c	1.3 (Berkeley) %G%";
#endif lint

    /*
     *	discover the tick frequency of the machine
     *	if something goes wrong, we return 0, an impossible hertz.
     */
#include <nlist.h>
#include <stdio.h>

#define	HZ_SYMBOL	"_hz"
#define	HZ_WRONG	0

struct nlist	nl[] =	{{HZ_SYMBOL},	/* clock ticks per second */
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
	fprintf(stderr, "no %s namelist entry for %s\n", VMUNIX, HZ_SYMBOL);
	return HZ_WRONG;
    }
#   define	KMEM	"/dev/kmem"	/* location of the system data space */
    kmem = open(KMEM, 0);
    if (kmem == -1) {
	perror("hertz()");
	fprintf(stderr, "open(\"%s\", 0)", KMEM);
	return HZ_WRONG;
    }
    seeked = lseek(kmem, nl[0].n_value, 0);
    if (seeked == -1) {
	fprintf(stderr, "can't lseek(kmem, 0x%x, 0)\n", nl[0].n_value);
	return HZ_WRONG;
    }
    red = read(kmem, &hz, sizeof hz);
    if (red != sizeof hz) {
	fprintf(stderr, "read(kmem, 0x%x, %d) returned %d\n",
		&hz, sizeof hz, red);
	return HZ_WRONG;
    }
    closed = close(kmem);
    if (closed != 0) {
	perror("hertz()");
	fprintf(stderr, "close(\"%s\")", KMEM);
	return HZ_WRONG;
    }
    return hz;
}
