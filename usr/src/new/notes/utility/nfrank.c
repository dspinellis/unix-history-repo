#include <stdio.h>
#include <ctype.h>
/*	Using data gleaned from nfstats, prints notesfile popularity
**	sorted by number of entries. The largest value is to be ignored
**	'cause it's really the total for all notesfiles.
**	Charley Kline, Tue May 31 09:59:57 CDT 1983
 *
 * $Header: /mntb/3/srg/notes/work/utility/RCS/nfrank.c,v 1.6 84/03/07 19:05:21 notes Exp $
*/
#define NAMEPOS 13
char    buf[80];

main ()
{
    FILE * popen (), *fdns, *fdtemp;
    char   *fgets (), *rindex (), nfname[30], entries[20];
    char    hostname[32];

    if ((fdns = popen ("nfstats net.\\*", "r")) == NULL)
	perror ("nfstats"),
	    exit (-1);
    fdtemp = fopen ("/tmp/top20junk", "w");
    gethostname (hostname, sizeof hostname);
    while (fgets (buf, sizeof buf, fdns) != NULL)
    {
	if (Index (buf, hostname))
	    bufcopy (buf, NAMEPOS, nfname);
	if (!strncmp (buf, "Orphan", 6))
	    bufcopy (rindex (buf, ':'), 2, entries),
		fprintf (fdtemp, "%8d\t%s\n", atoi (entries), nfname);
    }
    fclose (fdtemp);
    pclose (fdns);
    system ("sort -r /tmp/top20junk");
    unlink ("/tmp/top20junk");
}


Index (a, b) char  *a,
                   *b;
{
    char   *p,
           *index ();

next: 
    if ((p = index (a, *b++)) == 0)
	return (0);
    p++;
    while (*b)
	if (*p++ != *b++)
	    goto next;
    return (1);
}


bufcopy (buf, start, to) char  *buf,
                               *to;
int     start;
{
    char   *p = buf + start;

    while (isprint (*p))
	*to++ = *p++;
    *to = '\0';
    return;
}

