/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)conf.c	5.26 (Berkeley) 6/1/90";
#endif /* not lint */

# include <sys/ioctl.h>
# include <sys/param.h>
# include <pwd.h>
# include "sendmail.h"
# include "pathnames.h"

/*
**  CONF.C -- Sendmail Configuration Tables.
**
**	Defines the configuration of this installation.
**
**	Compilation Flags:
**		VMUNIX -- running on a Berkeley UNIX system.
**
**	Configuration Variables:
**		HdrInfo -- a table describing well-known header fields.
**			Each entry has the field name and some flags,
**			which are described in sendmail.h.
**
**	Notes:
**		I have tried to put almost all the reasonable
**		configuration information into the configuration
**		file read at runtime.  My intent is that anything
**		here is a function of the version of UNIX you
**		are running, or is really static -- for example
**		the headers are a superset of widely used
**		protocols.  If you find yourself playing with
**		this file too much, you may be making a mistake!
*/




/*
**  Header info table
**	Final (null) entry contains the flags used for any other field.
**
**	Not all of these are actually handled specially by sendmail
**	at this time.  They are included as placeholders, to let
**	you know that "someday" I intend to have sendmail do
**	something with them.
*/

struct hdrinfo	HdrInfo[] =
{
		/* originator fields, most to least significant  */
	"resent-sender",	H_FROM|H_RESENT,
	"resent-from",		H_FROM|H_RESENT,
	"resent-reply-to",	H_FROM|H_RESENT,
	"sender",		H_FROM,
	"from",			H_FROM,
	"reply-to",		H_FROM,
	"full-name",		H_ACHECK,
	"return-receipt-to",	H_FROM,
	"errors-to",		H_FROM,
		/* destination fields */
	"to",			H_RCPT,
	"resent-to",		H_RCPT|H_RESENT,
	"cc",			H_RCPT,
	"resent-cc",		H_RCPT|H_RESENT,
	"bcc",			H_RCPT|H_ACHECK,
	"resent-bcc",		H_RCPT|H_ACHECK|H_RESENT,
		/* message identification and control */
	"message-id",		0,
	"resent-message-id",	H_RESENT,
	"message",		H_EOH,
	"text",			H_EOH,
		/* date fields */
	"date",			0,
	"resent-date",		H_RESENT,
		/* trace fields */
	"received",		H_TRACE|H_FORCE,
	"via",			H_TRACE|H_FORCE,
	"mail-from",		H_TRACE|H_FORCE,

	NULL,			0,
};


/*
**  ARPANET error message numbers.
*/

char	Arpa_Info[] =		"050";	/* arbitrary info */
char	Arpa_TSyserr[] =	"451";	/* some (transient) system error */
char	Arpa_PSyserr[] =	"554";	/* some (permanent) system error */
char	Arpa_Usrerr[] =		"554";	/* some (fatal) user error */



/*
**  Location of system files/databases/etc.
*/

char	*ConfFile =	_PATH_SENDMAILCF;	/* runtime configuration */
char	*FreezeFile =	_PATH_SENDMAILFC;	/* frozen version of above */



/*
**  Miscellaneous stuff.
*/

int	DtableSize =	50;		/* max open files; reset in 4.2bsd */
extern int la;				/* load average */
/*
**  SETDEFAULTS -- set default values
**
**	Because of the way freezing is done, these must be initialized
**	using direct code.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Initializes a bunch of global variables to their
**		default values.
*/

setdefaults()
{
	QueueLA = 8;
	QueueFactor = 10000;
	RefuseLA = 12;
	SpaceSub = ' ';
	WkRecipFact = 1000;
	WkClassFact = 1800;
	WkTimeFact = 9000;
	FileMode = 0644;
	DefUid = 1;
	DefGid = 1;
	setdefuser();
}


/*
**  SETDEFUSER -- set/reset DefUser using DefUid (for initgroups())
*/

setdefuser()
{
	struct passwd *defpwent;

	if (DefUser != NULL)
		free(DefUser);
	if ((defpwent = getpwuid(DefUid)) != NULL)
		DefUser = newstr(defpwent->pw_name);
	else
		DefUser = newstr("nobody");
}


/*
**  GETRUID -- get real user id (V7)
*/

getruid()
{
	if (OpMode == MD_DAEMON)
		return (RealUid);
	else
		return (getuid());
}


/*
**  GETRGID -- get real group id (V7).
*/

getrgid()
{
	if (OpMode == MD_DAEMON)
		return (RealGid);
	else
		return (getgid());
}

/*
**  USERNAME -- return the user id of the logged in user.
**
**	Parameters:
**		none.
**
**	Returns:
**		The login name of the logged in user.
**
**	Side Effects:
**		none.
**
**	Notes:
**		The return value is statically allocated.
*/

char *
username()
{
	static char *myname = NULL;
	extern char *getlogin();
	register struct passwd *pw;
	extern struct passwd *getpwuid();

	/* cache the result */
	if (myname == NULL)
	{
		myname = getlogin();
		if (myname == NULL || myname[0] == '\0')
		{

			pw = getpwuid(getruid());
			if (pw != NULL)
				myname = newstr(pw->pw_name);
		}
		else
		{

			myname = newstr(myname);
			if ((pw = getpwnam(myname)) == NULL ||
			      getuid() != pw->pw_uid)
			{
				pw = getpwuid(getuid());
				if (pw != NULL)
					myname = newstr(pw->pw_name);
			}
		}
		if (myname == NULL || myname[0] == '\0')
		{
			syserr("Who are you?");
			myname = "postmaster";
		}
	}

	return (myname);
}
/*
**  TTYPATH -- Get the path of the user's tty
**
**	Returns the pathname of the user's tty.  Returns NULL if
**	the user is not logged in or if s/he has write permission
**	denied.
**
**	Parameters:
**		none
**
**	Returns:
**		pathname of the user's tty.
**		NULL if not logged in or write permission denied.
**
**	Side Effects:
**		none.
**
**	WARNING:
**		Return value is in a local buffer.
**
**	Called By:
**		savemail
*/

# include <sys/stat.h>

char *
ttypath()
{
	struct stat stbuf;
	register char *pathn;
	extern char *ttyname();
	extern char *getlogin();

	/* compute the pathname of the controlling tty */
	if ((pathn = ttyname(2)) == NULL && (pathn = ttyname(1)) == NULL &&
	    (pathn = ttyname(0)) == NULL)
	{
		errno = 0;
		return (NULL);
	}

	/* see if we have write permission */
	if (stat(pathn, &stbuf) < 0 || !bitset(02, stbuf.st_mode))
	{
		errno = 0;
		return (NULL);
	}

	/* see if the user is logged in */
	if (getlogin() == NULL)
		return (NULL);

	/* looks good */
	return (pathn);
}
/*
**  CHECKCOMPAT -- check for From and To person compatible.
**
**	This routine can be supplied on a per-installation basis
**	to determine whether a person is allowed to send a message.
**	This allows restriction of certain types of internet
**	forwarding or registration of users.
**
**	If the hosts are found to be incompatible, an error
**	message should be given using "usrerr" and FALSE should
**	be returned.
**
**	'NoReturn' can be set to suppress the return-to-sender
**	function; this should be done on huge messages.
**
**	Parameters:
**		to -- the person being sent to.
**
**	Returns:
**		TRUE -- ok to send.
**		FALSE -- not ok.
**
**	Side Effects:
**		none (unless you include the usrerr stuff)
*/

bool
checkcompat(to)
	register ADDRESS *to;
{
# ifdef lint
	if (to == NULL)
		to++;
# endif lint
# ifdef EXAMPLE_CODE
	/* this code is intended as an example only */
	register STAB *s;

	s = stab("arpa", ST_MAILER, ST_FIND);
	if (s != NULL && CurEnv->e_from.q_mailer != LocalMailer &&
	    to->q_mailer == s->s_mailer)
	{
		usrerr("No ARPA mail through this machine: see your system administration");
		/* NoReturn = TRUE; to supress return copy */
		return (FALSE);
	}
# endif EXAMPLE_CODE
	return (TRUE);
}
/*
**  HOLDSIGS -- arrange to hold all signals
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Arranges that signals are held.
*/

holdsigs()
{
}
/*
**  RLSESIGS -- arrange to release all signals
**
**	This undoes the effect of holdsigs.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Arranges that signals are released.
*/

rlsesigs()
{
}
/*
**  GETLA -- get the current load average
**
**	This code stolen from la.c.
**
**	Parameters:
**		none.
**
**	Returns:
**		The current load average as an integer.
**
**	Side Effects:
**		none.
*/

#ifndef sun

getla()
{
	double avenrun[3];

	if (getloadavg(avenrun, sizeof(avenrun) / sizeof(avenrun[0])) < 0)
		return (0);
	return ((int) (avenrun[0] + 0.5));
}

#else /* sun */

#include <nlist.h>

struct	nlist Nl[] =
{
	{ "_avenrun" },
#define	X_AVENRUN	0
	{ 0 },
};


extern int la;

getla()
{
	static int kmem = -1;
	long avenrun[3];
	extern off_t lseek();

	if (kmem < 0)
	{
		kmem = open("/dev/kmem", 0, 0);
		if (kmem < 0)
			return (-1);
		(void) ioctl(kmem, (int) FIOCLEX, (char *) 0);
		nlist("/vmunix", Nl);
		if (Nl[0].n_type == 0)
			return (-1);
	}
	if (lseek(kmem, (off_t) Nl[X_AVENRUN].n_value, 0) == -1 ||
	    read(kmem, (char *) avenrun, sizeof(avenrun)) < sizeof(avenrun))
	{
		/* thank you Ian */
		return (-1);
	}
	return ((int) (avenrun[0] + FSCALE/2) >> FSHIFT);
}

#endif /* sun */
/*
**  SHOULDQUEUE -- should this message be queued or sent?
**
**	Compares the message cost to the load average to decide.
**
**	Parameters:
**		pri -- the priority of the message in question.
**
**	Returns:
**		TRUE -- if this message should be queued up for the
**			time being.
**		FALSE -- if the load is low enough to send this message.
**
**	Side Effects:
**		none.
*/

bool
shouldqueue(pri)
	long pri;
{
	if (la < QueueLA)
		return (FALSE);
	return (pri > (QueueFactor / (la - QueueLA + 1)));
}
/*
**  SETPROCTITLE -- set process title for ps
**
**	Parameters:
**		fmt -- a printf style format string.
**		a, b, c -- possible parameters to fmt.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Clobbers argv of our main procedure so ps(1) will
**		display the title.
*/

/*VARARGS1*/
setproctitle(fmt, a, b, c)
	char *fmt;
{
# ifdef SETPROCTITLE
	register char *p;
	register int i;
	extern char **Argv;
	extern char *LastArgv;
	char buf[MAXLINE];

	(void) sprintf(buf, fmt, a, b, c);

	/* make ps print "(sendmail)" */
	p = Argv[0];
	*p++ = '-';

	i = strlen(buf);
	if (i > LastArgv - p - 2)
	{
		i = LastArgv - p - 2;
		buf[i] = '\0';
	}
	(void) strcpy(p, buf);
	p += i;
	while (p < LastArgv)
		*p++ = ' ';
# endif SETPROCTITLE
}
/*
**  REAPCHILD -- pick up the body of my child, lest it become a zombie
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Picks up extant zombies.
*/

# ifdef VMUNIX
# include <sys/wait.h>
# endif VMUNIX

reapchild()
{
# ifdef WNOHANG
	union wait status;

	while (wait3(&status, WNOHANG, (struct rusage *) NULL) > 0)
		continue;
# else WNOHANG
	auto int status;

	while (wait(&status) > 0)
		continue;
# endif WNOHANG
}
