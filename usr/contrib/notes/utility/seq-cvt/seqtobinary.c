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
    char    line[128];

    i = open (argv[1], 1);
    if (i < 0)
    {
	i = creat (argv[1], 0666);
	if (i < 0)
	{
	    printf ("Couldn't open/create %d\n", argv[1]);
	    exit (1);
	}
    }

    while (fgets (line, sizeof line, stdin) != NULL)
    {
/*
 *	printf("Hey I read line: %d", line);
 */

	buf.lastin.w_gmttime = 0;		/* no unixtime */
	j=sscanf (line, "%s : %hd:%hd %hd/%hd/%hd",
		&buf.nfname,
		&buf.lastin.w_hours, &buf.lastin.w_mins,
		&buf.lastin.w_day, &buf.lastin.w_month, &buf.lastin.w_year);
/*
 *	printf("sscanf returned %d\n", j);
 *	printf ("%-20s: %d:%d %d/%d/%d\n", buf.nfname,
 *		buf.lastin.w_hours, buf.lastin.w_mins,
 *		buf.lastin.w_day, buf.lastin.w_month, buf.lastin.w_year);
 */

	write (i, &buf, sizeof buf);
    }

}
