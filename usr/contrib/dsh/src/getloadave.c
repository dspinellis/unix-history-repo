/*(@(#)getloadave.c	1.2		/ra/csr/presotto/hacks/src/worm/sccs/s.getloadave.c)*/
#include <sys/types.h>
#include <nlist.h>
#include <sys/stat.h>

#define MAXRETRIES 20

struct	nlist nl[] = {
#define	NL_AVENRUN	0
	{ "_avenrun" },
	0
};
int	kmemf = -1;		/* fd for kmem */
time_t	vmunixdate = 0;		/* update time for vmunix */

initnlist ()
{
    int			i;
    struct nlist	*nlp;
    struct stat		sbuf;

    i = stat ("/vmunix", &sbuf);
    if (i == -1) {
	error ("can't read /vmunix");
    }
    if (vmunixdate != sbuf.st_mtime) {
	for (i = 0; i < MAXRETRIES; sleep (5), i++) {
	    for (nlp = &nl[sizeof (nl) / sizeof (nl[0])]; --nlp >= nl; ) {
		nlp->n_value = 0;
		nlp->n_type = 0;
	    }
	    nlist("/vmunix", nl);
	    if (nl[0].n_value == 0) {
		warn ("/vmunix namelist botch");
		continue;
	    }
	    break;
	}
	if (i >= MAXRETRIES)
	    exit (-1);
	vmunixdate = sbuf.st_mtime;
    }

    if (kmemf == -1) {
	kmemf = open("/dev/kmem", 0);
	if (kmemf < 0) {
	    error ("opening /dev/kmem");
	}
    }
}


/*
 *	Get the current load average 
 */
getloadave (avenrun)
double	*avenrun;
{
    double		template[3];

    initnlist ();
    (void) lseek(kmemf, (long)nl[NL_AVENRUN].n_value, 0);
    (void) read(kmemf, (char *)avenrun, sizeof (template));
}
