#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: savtxt.c,v 1.7 85/01/18 15:39:14 notes Rel $";
#endif	RCSIDENT

/*
 *	savtxt - will format and write the note/response handed to
 *	it in a specified file in the users working directory.
 *	We have to do some forking in this case because the directory
 *	may be protected against us. 
 *	first save the text in a /tmp/nfs file, then fork and
 *	setuid to the user. The file is then copied to the end
 *	of the ultimate destination.
 *		-- Thanks to Rick Spickelmier (tekcad!ricks)
 *
 *	returns the number of lines written to the file.
 *
 *	Original Coding:	Ray Essick	December 1981
 */

long    lseek ();					/* declare for lint */
FILE * popen ();					/* for lint */
#include <signal.h>					/* define signal processing */

savtxt (io, where, author, date, txtwhere, title)
struct io_f *io;
char   *where;
struct auth_f  *author;
struct when_f  *date;
struct daddr_f *txtwhere;
char   *title;
{
    int     (*p) (),
            (*q) (),
            (*r) ();
    int     retcode,
            lines;
    int     c;						/* character scratch */
    register    pid,
                rpid;
    FILE * txtfile;
    FILE * outfile;
    char    tmpfile[20];				/* hold scratch file name */

    sprintf (tmpfile, "/tmp/nfs%d", getpid ());
    if ((txtfile = fopen (tmpfile, "w")) == NULL)
	return 0;					/* no lines saved */
    lines = preptxt (io, txtfile, author, date, txtwhere, title);
    fclose (txtfile);

/*
 *	open it now so we don't have to hassle with permissions and
 *	such.  the file is passed across the fork.
 */
    if ((txtfile = fopen (tmpfile, "r")) == NULL)
	return 0;					/* couldn't reopen */

    if ((pid = fork ()) == 0)				/* also makes him own file */
    {
#ifdef	RUNSUID
	x (setuid (globuid) < 0, "savtxt: couldn't setuid(your uid)");
#else
	x (setgid (getgid ()) < 0, "savtxt: couldn't setgid(your gid)");
#endif	RUNSUID
	if (where[0] == '|')				/* make it a pipe */
	    outfile = popen (&where[1], "w");		/* writing */
	else
	    outfile = fopen (where, "a");		/* a simple file */
	if (outfile == NULL)				/* no way jose */
	    exit (1);					/* error */
/*
 *	copy the file over
 *		there are faster ways... this works.
 */
	while ((c = getc (txtfile)) != EOF)
	    putc (c, outfile);				/* copy it */
	if (where[0] == '|')
	    pclose (outfile);				/* pipes wait */
	else
	    fclose (outfile);
	fclose (txtfile);
	exit (0);					/* exit ok */
    }							/* end of forked process code */

    /* wait here for the son to finish */
    p = signal (1, SIG_IGN);
    q = signal (2, SIG_IGN);
    r = signal (3, SIG_IGN);
    while ((rpid = wait (&retcode)) != pid && rpid != -1);
    if (rpid == -1)
	retcode = -1;
    signal (1, p);
    signal (2, q);
    signal (3, r);

    fclose (txtfile);					/* close it in this one too */
    x (unlink (tmpfile) < 0, "savtxt: bad unlink of text file");
    if (retcode)					/* something wrong */
	return 0;					/* no lines saved */
    else
	return lines;
}
