#ifndef lint
    static	char *sccsid = "@(#)hertz.c	1.2 (Berkeley) %G%";
#endif lint

    /*
     *	discover the tick frequency of the machine
     *	if something goes wrong, we return HZ_DEFAULT.
     */
#include <nlist.h>
#include <stdio.h>

#define	HZ_SYMBOL	"_hz"
#define	HZ_DEFAULT	1

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
wrong:
	fprintf(stderr, "times are in units of %d tick%s, not seconds\n",
			HZ_DEFAULT, HZ_DEFAULT==1?"":"s");
	return HZ_DEFAULT;
    }
#   define	KMEM	"/dev/kmem"	/* location of the system data space */
    kmem = open(KMEM, 0);
    if (kmem == -1) {
	perror("hertz()");
	fprintf(stderr, "open(\"%s\", 0)", KMEM);
	goto wrong;
    }
    seeked = lseek(kmem, nl[0].n_value, 0);
    if (seeked == -1) {
	fprintf(stderr, "can't lseek(kmem, 0x%x, 0)\n", nl[0].n_value);
	goto wrong;
    }
    red = read(kmem, &hz, sizeof hz);
    if (red != sizeof hz) {
	fprintf(stderr, "read(kmem, 0x%x, %d) returned %d\n",
		&hz, sizeof hz, red);
	goto wrong;
    }
    closed = close(kmem);
    if (closed != 0) {
	perror("hertz()");
	fprintf(stderr, "close(\"%s\")", KMEM);
	goto wrong;
    }
    return hz;
}
