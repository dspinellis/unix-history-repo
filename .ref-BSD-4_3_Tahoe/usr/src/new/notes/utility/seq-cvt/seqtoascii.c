#include "../../src/parms.h"
#include "../../src/structs.h"

main (argc, argv)
int     argc;
char  **argv;
{
    int     i,
            j,
            k;
    char   *p,
           *q,
           *r;
    FILE * fp;
    struct seq_f    buf;

    i = open (argv[1], 0);

    while (read (i, &buf, sizeof buf) == sizeof buf)
    {
#ifdef	DEBUG
	/*
	 * normally we don't print the gmttime field...
	 * the seqtobinary program doesn't expect to see it...
	 *
	 * You are welcome to use it if you want...
	 */
	printf ("%-20s: %d:%d %d/%d/%d (%ld)\n", buf.nfname,
		buf.lastin.w_hours, buf.lastin.w_mins,
		buf.lastin.w_day, buf.lastin.w_month, buf.lastin.w_year,
		buf.lastin.w_gmttime);
#else
	printf ("%-20s: %d:%d %d/%d/%d\n", buf.nfname,
		buf.lastin.w_hours, buf.lastin.w_mins,
		buf.lastin.w_day, buf.lastin.w_month, buf.lastin.w_year);
#endif
    }

}
