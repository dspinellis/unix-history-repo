# include <pwd.h>
# include <stdio.h>
# include <sysexits.h>
# include <ctype.h>
# include "useful.h"
# include "userdbm.h"

SCCSID(@(#)vacation.c	4.1.1.1		%G%);

/*
**  VACATION -- return a message to the sender when on vacation.
**
**	This program could be invoked as a message receiver
**	when someone is on vacation.  It returns a message
**	specified by the user to whoever sent the mail, taking
**	care not to return a message too often to prevent
**	"I am on vacation" loops.
**
**	For best operation, this program should run setuid to
**	root or uucp or someone else that sendmail will believe
**	a -f flag from.  Otherwise, the user must be careful
**	to include a header on his .vacation.msg file.
**
**	Positional Parameters:
**		the user to collect the vacation message from.
**
**	Flag Parameters:
**		-I	initialize the database.
**		-tT	set the timeout to T.  messages arriving more
**			often than T will be ignored to avoid loops.
**
**	Side Effects:
**		A message is sent back to the sender.
**
**	Author:
**		Eric Allman
**		UCB/INGRES
*/

# define MAXLINE	256	/* max size of a line */

# define ONEWEEK	(60L*60L*24L*7L)

time_t	Timeout = ONEWEEK;	/* timeout between notices per user */

struct dbrec {
	time_t	sentdate;
};

main(argc, argv)
	char **argv;
{
	char *from;
	register char *p;
	struct passwd *pw;
	char *homedir;
	char *myname;
	char *shortfrom;
	char buf[MAXLINE];
	extern struct passwd *getpwnam();
	extern char *newstr();
	extern char *getfrom();
	extern bool knows();
	extern time_t convtime();

	/* process arguments */
	while (--argc > 0 && (p = *++argv) != NULL && *p == '-')
	{
		switch (*++p)
		{
		  case 'I':	/* initialize */
			initialize();
			exit(EX_OK);

		  case 't':	/* set timeout */
			Timeout = convtime(++p);
			break;

		  default:
			usrerr("Unknown flag -%s", p);
			exit(EX_USAGE);
		}
	}

	/* verify recipient argument */
	if (argc != 1)
	{
		usrerr("Usage: vacation username (or) vacation -I");
		exit(EX_USAGE);
	}

	myname = p;

	/* find user's home directory */
	pw = getpwnam(myname);
	if (pw == NULL)
	{
		usrerr("Unknown user %s", myname);
		exit(EX_NOUSER);
	}
	homedir = newstr(pw->pw_dir);
	(void) strcpy(buf, homedir);
	(void) strcat(buf, "/.vacation");
	dbminit(buf);

	/* read message from standard input (just from line) */
	from = getfrom(&shortfrom);
#ifdef VDEBUG
	printf("from='%s'\nshortfrom='%s'\n", from, shortfrom);
	exit(0);
#endif

	/* check if this person is already informed */
	if (!knows(shortfrom))
	{
		/* mark this person as knowing */
		setknows(shortfrom);

		/* send the message back */
		(void) strcpy(buf, homedir);
		(void) strcat(buf, "/.vacation.msg");
		sendmessage(buf, from, myname);
		/* NOTREACHED */
	}
	exit (EX_OK);
}
/*
**  GETFROM -- read message from standard input and return sender
**
**	Parameters:
**		none.
**
**	Returns:
**		pointer to the sender address.
**
**	Side Effects:
**		Reads first line from standard input.
*/

char *
getfrom(shortp)
char **shortp;
{
	static char line[MAXLINE];
	register char *p, *start, *at, *bang;
	char saveat;

	/* read the from line */
	if (fgets(line, sizeof line, stdin) == NULL ||
	    strncmp(line, "From ", 5) != NULL)
	{
		usrerr("No initial From line");
		exit(EX_USAGE);
	}

	/* find the end of the sender address and terminate it */
	start = &line[5];
	p = index(start, ' ');
	if (p == NULL)
	{
		usrerr("Funny From line '%s'", line);
		exit(EX_USAGE);
	}
	*p = '\0';

	/*
	 * Strip all but the rightmost UUCP host
	 * to prevent loops due to forwarding.
	 * Start searching leftward from the leftmost '@'.
	 *	a!b!c!d yields a short name of c!d
	 *	a!b!c!d@e yields a short name of c!d@e
	 *	e@a!b!c yields the same short name
	 */
#ifdef VDEBUG
printf("start='%s'\n", start);
#endif
	*shortp = start;			/* assume whole addr */
	if ((at = index(start, '@')) == NULL)	/* leftmost '@' */
		at = p;				/* if none, use end of addr */
	saveat = *at;
	*at = '\0';
	if ((bang = rindex(start, '!')) != NULL) {	/* rightmost '!' */
		char *bang2;

		*bang = '\0';
		if ((bang2 = rindex(start, '!')) != NULL) /* 2nd rightmost '!' */
			*shortp = bang2 + 1;		/* move past ! */
		*bang = '!';
	}
	*at = saveat;
#ifdef VDEBUG
printf("place='%s'\n", *shortp);
#endif

	/* return the sender address */
	return start;
}
/*
**  KNOWS -- predicate telling if user has already been informed.
**
**	Parameters:
**		user -- the user who sent this message.
**
**	Returns:
**		TRUE if 'user' has already been informed that the
**			recipient is on vacation.
**		FALSE otherwise.
**
**	Side Effects:
**		none.
*/

bool
knows(user)
char *user;
{
	DATUM k, d;
	struct dbrec ldbrec;

	k.dptr = user;
	k.dsize = strlen(user) + 1;
	d = fetch(k);
	if (d.dptr == NULL)
		return FALSE;
	bcopy(d.dptr, (char *)&ldbrec, sizeof ldbrec);	/* realign data */
	return ldbrec.sentdate + Timeout >= time((time_t *)0);
}

#ifndef VMUNIX
bcopy(from, to, size)
register char *to, *from;
register unsigned size;
{
	while (size-- > 0)
		*to++ = *from++;
}
#endif
/*
**  SETKNOWS -- set that this user knows about the vacation.
**
**	Parameters:
**		user -- the user who should be marked.
**
**	Returns:
**		none.
**
**	Side Effects:
**		The dbm file is updated as appropriate.
*/

setknows(user)
	char *user;
{
	DATUM k, d;
	struct dbrec xrec;

	k.dptr = user;
	k.dsize = strlen(user) + 1;
	time(&xrec.sentdate);
	d.dptr = (char *) &xrec;
	d.dsize = sizeof xrec;
	store(k, d);
}
/*
**  SENDMESSAGE -- send a message to a particular user.
**
**	Parameters:
**		msgf -- filename containing the message.
**		user -- user who should receive it.
**
**	Returns:
**		none.
**
**	Side Effects:
**		sends mail to 'user' using /usr/lib/sendmail.
*/

sendmessage(msgf, user, myname)
	char *msgf;
	char *user;
	char *myname;
{
	FILE *f;

	/* find the message to send */
	f = freopen(msgf, "r", stdin);
	if (f == NULL)
	{
		f = freopen("/usr/lib/vacation.def", "r", stdin);
		if (f == NULL)
			syserr("No message to send");
	}

	execl("/usr/lib/sendmail", "sendmail", "-f", myname, user, NULL);
	syserr("Cannot exec /usr/lib/sendmail");
}
/*
**  INITIALIZE -- initialize the database before leaving for vacation
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Initializes the files .vacation.{pag,dir} in the
**		caller's home directory.
*/

initialize()
{
	char *homedir;
	char buf[MAXLINE];

	setgid(getgid());
	setuid(getuid());
	homedir = getenv("HOME");
	if (homedir == NULL)
		syserr("No home!");
	(void) strcpy(buf, homedir);
	(void) strcat(buf, "/.vacation.dir");
	if (close(creat(buf, 0644)) < 0)
		syserr("Cannot create %s", buf);
	(void) strcpy(buf, homedir);
	(void) strcat(buf, "/.vacation.pag");
	if (close(creat(buf, 0644)) < 0)
		syserr("Cannot create %s", buf);
}
/*
**  USRERR -- print user error
**
**	Parameters:
**		f -- format.
**		p -- first parameter.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

/* VARARGS 1 */
usrerr(f, p)
	char *f;
	char *p;
{
	fprintf(stderr, "vacation: ");
	_doprnt(f, &p, stderr);
	fprintf(stderr, "\n");
}
/*
**  SYSERR -- print system error
**
**	Parameters:
**		f -- format.
**		p -- first parameter.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

/* VARARGS 1 */
syserr(f, p)
	char *f;
	char *p;
{
	fprintf(stderr, "vacation: ");
	_doprnt(f, &p, stderr);
	fprintf(stderr, "\n");
	exit(EX_USAGE);
}
/*
**  NEWSTR -- copy a string
**
**	Parameters:
**		s -- the string to copy.
**
**	Returns:
**		A copy of the string.
**
**	Side Effects:
**		none.
*/

char *
newstr(s)
	char *s;
{
	char *p;

	p = malloc((unsigned)strlen(s) + 1);
	if (p == NULL)
	{
		syserr("newstr: cannot alloc memory");
		exit(EX_OSERR);
	}
	strcpy(p, s);
	return p;
}
