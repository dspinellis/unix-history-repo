#include	"parms.h"
#include	"structs.h"
#include	<pwd.h>
#ifdef	RCSIDENT
static char rcsid[] = "$Header: startup.c,v 1.7.0.3 85/03/18 20:56:44 notes Rel $";
#endif	RCSIDENT

static char *Bigversion = "Notesfiles: $Revision: 1.7.0.3 $";

/*
 *	this file contains code and declarations that are common to
 *	all main lines. This routine should be called by all
 *	mainline routines immediately.
 *
 *	We also take care of the problem of some systems not honoring
 *	the setuid bit when root runs a program.
 *
 *	Ray Essick	May 7, 1982
 */

static char SysName[32];				/* holds system name */
static char authsys[32];				/* author's system */

#if	defined(UNAME)
#include <sys/utsname.h>				/* uname syscall */
#endif	defined(UNAME)

char   *hised = DFLTED;					/* his favorite editor */
char   *hisshell = DFLTSH;				/* his favorite shell */
char   *hispager;
char   *hismailer;
int     nrows = 24;					/* rows on screen */
int     ncols = 80;					/* width of screen */
char   *histty = (char *) NULL;				/* tty as on command */
int     intflag = 0;					/* DEL hit recently */
int     globuid = ANONUID;				/* his true user id */
int     Notesuid = NOTESUID;				/* default to root? */
int     Anonuid = ANONUID;
int     Nindex = 12;					/* # on index page */
int     ignoresigs = 0;					/* critical section */
char   *Mstdir = MSTDIR;				/* store nf's here */
char   *System = SysName;				/* point to it */
char   *Authsystem = authsys;				/* init pointer */
char   *Invokedas = "Unknown";				/* argv[0] */
char    Seqname[WDLEN];					/* sequencer name */
struct when_f   Basetime;				/* zero time */


struct passwd  *getpwnam ();

startup (argc, argv)
int     argc;
char  **argv;
{

    struct passwd  *pw;
    struct auth_f   whoami;
    char   *p;

/*
 *	Grab "notes" uid from /etc/passwd and also grab uid of
 *	user running this process.
 *	ditto for "anon".
 */

    if ((pw = getpwnam (NOTES)) == NULL)		/* find notes uid */
    {
	fprintf (stderr, "Can't find uid for %s, assuming %d\n",
		NOTES, NOTESUID);
#ifdef	DYNADIR
	fprintf (stderr, "Assuming Notesfiles live in %s\n",
		Mstdir);
#endif	DYNADIR
    }
    else
    {
	Notesuid = pw -> pw_uid & UIDMASK;
#ifdef	DYNADIR
/*
 *	Select Mstdir based on the home directory for the "notes"
 *	owner.  Make it one directory "above" the note's home directory.
 *	(which in many places will be /usr/spool/notes/.utilities
 */
	if ((p = rindex (pw -> pw_dir, '/')) != NULL)	/* get end */
	{
	    *p = '\0';					/* terminate */
	    if (strlen (pw -> pw_dir) > 0)		/* non-empty */
		Mstdir = strsave (pw -> pw_dir);	/* save it */
	}
#endif	DYNADIR
    }

    if ((pw = getpwnam (ANON)) == NULL)			/* (prohibited) anon uid */
    {
	fprintf (stderr, "Can't find uid for %s, assuming %d\n",
		ANON, ANONUID);
    }
    else
    {
	Anonuid = pw -> pw_uid & UIDMASK;
    }
    endpwent ();

/*
 *	now resolve who is running the program, make sure that
 *	we are setuid'ed away from root which breaks our locks
 *	at this time (need a better locking mechanism).
 *
 *	We really want seteuid(), but V7 and a number of others
 *	don't offer this.  4.2 Bsd does, so we use it there.
 */

    globuid = getuid () & UIDMASK;			/* set this */
#ifdef	RUNSUID
    if (geteuid () == 0)				/* root access? */
    {
#ifdef	BSD42
	seteuid (Notesuid);				/* we'll fix that */
#else
	setuid (Notesuid);				/* we'll fix that */
#endif	BSD42
    }
#endif	RUNSUID

    getname (&whoami, 0);				/* get my name */
    strcpy (Seqname, &whoami.aname);			/* load seq name */

/*
 *	and now decide where we are 
 */

#ifdef	PORTBINARY					/* to have portable binaries */
    gethostname (SysName, sizeof (SysName));		/* load it */
#endif	defined(PORTBINARY)

#ifdef	UNAME						/* like PORTBINARY */
    {
	struct utsname  name;
	uname (&name);					/* UNIX 4.0 syscall */
	strcpy (SysName, name.nodename);
    }
#endif	defined(UNAME)

#ifdef	WHOAMI
    {
	char    buf[BUFSIZ];
	FILE * fd;

	if ((fd = fopen (WHOAMIFILE, "r")) == NULL)	/* get file */
	{
	    fprintf (stderr, "Cannot open %s\n", WHOAMIFILE);
	    exit (1);
	}
	for (;;)
	{
	    if (fgets (buf, sizeof buf, fd) == NULL)
	    {
		fprintf (stderr, "no sysname in %s\n", WHOAMIFILE);
		fclose (fd);
		exit (2);
	    }
	    if (sscanf (buf, "#define sysname \"%[^\"]\"", SysName) == 1)
	    {
		fclose (fd);
		break;
	    }
	}
    }
#endif	WHOAMI

    /* 
     * Now that we have a copy of the System name loaded into
     * the "SysName" array, let's make a copy to use for the 
     * user as his system. The "System" will get used for things
     * like the generation of Unique ID's.
     */
    strcpy (Authsystem, System);			/* copy it */

#ifdef	FULLDOMAIN
    /* 
     * append the domain information (if it isn't already there)
     * to the Author system name.
     */
    if (index (Authsystem, '.') == (char *) NULL)
    {
	strcat (Authsystem, ".");
	strcat (Authsystem, FULLDOMAIN);
    }

#ifdef	IDDOMAIN
    /* 
     *	append domain information if needed
     *  to the system name as it is used in the unique id.
     */
    if (index (System, '.') == (char *) NULL)		/* if not there */
    {
	strcat (SysName, ".");				/* separator */
	strcat (SysName, FULLDOMAIN);			/* append it */
    }
#endif	IDDOMAIN

#endif	FULLDOMAIN

/*
 *	Decide what we were invoked as
 *	and set up a few other values that don't fit anywhere.
 */

    Invokedas = argv[0];
    Basetime.w_year = 1970;				/* unix time 0 */
    Basetime.w_month = 1;
    Basetime.w_day = 1;
    Basetime.w_hours = 0;
    Basetime.w_mins = 0;
    Basetime.w_gmttime = 0;

}
