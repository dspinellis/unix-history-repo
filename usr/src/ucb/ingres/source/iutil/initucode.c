# include	<stdio.h>
# include	<ingres.h>
# include	<aux.h>
# include	<version.h>
# include	<opsys.h>
# include	<access.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)initucode.c	7.1	2/5/81)

/*
**  INITUCODE -- initialize standalone process
**
**	This function initializes a standalone process, initializing
**	a lot of global variables, scanning the argument vector for
**	some special flags (-u and +-w), seperating flags and
**	parameters, and so forth.
**
**	Every standalone program should begin with the lines:
**			i = initucode(argc, argv, ...);
**			switch (i)
**				...
**
**	On a return of 2, 3, or 4, essentially none of the processing
**	is done (particularly true with return 4).  Virtually nothing
**	can be done in the calling program except print a "usage"
**	message and exit.  The exception to this is that 'Pathname'
**	is set, so that it can be used in the error printing.  For
**	example, ingres.c cats file .../files/usage on this sort of
**	error.
**
**	If it is preferable to not lock the database at this time,
**	the 'waitmode' parameter should be passed as -1.  This still
**	causes the 'Wait_action' variable to be initialized, but the
**	database is not actually locked.  It can be locked by calling:
**		db_lock(Dbpath, M_EXCL);
**	at the proper time.
**
**	For the main effects of this routine, see the "Side Effects"
**	section below.
**
**	Parameters:
**		argc -- argc from main.
**		argv -- argv from main.
**		dbflag -- TRUE -- take the first parameter as the
**				database name.
**			FALSE -- don't take the first parameter as
**				the database name.
**		paramlist -- a pointer to an array[4] of pointers
**			to character; set to the extra fields of
**			the users file entry for the real user
**			executing the code (not the user on the
**			-u flag).  If NULL, this is ignored.
**		waitmode -- M_EXCL -- set an exclusive lock on the
**				database.
**			M_SHARE -- set a shared lock on the database.
**			-1 -- don't set a lock on the database.
**				However, other stuff (Wait_action) is
**				still set up so that the lock can be
**				placed later by calling 'db_lock'.
**
**	Returns:
**		0 -- everything is ok.
**		1 -- the database does not exist.
**		2 -- you are not authorized to access this database.
**		3 -- you are not a valid INGRES user.
**		4 -- no database name was specified (only if dbflag
**			== TRUE).
**		5 -- everything is ok, but there was an indirect
**			taken.
**		6 -- there was an indirect taken, but there was no
**			database there.
**
**		If dbflag == FALSE, you can only get returns 0 and
**			3.
**
**	Side Effects:
**		A lot of variables are set, as follows:
**
**		Dbpath -- set to the pathname of the database (only
**			if dbflag == TRUE).  It is set even if the
**			database does not exist.
**		Parmvect -- set to the parameters from argv, that is,
**			anything not beginning with '+' or '-'.
**		Flagvect -- set to the flags from argv, that is,
**			everything beginning with '+' or '-'.  The
**			flags '+w', '-w', and '-u' are stripped out,
**			however.
**		Wait_action -- set to the appropriate action (A_SLP
**			or A_RTN) based on the +-w flags and whether
**			we are running in background or not.
**			This is automatically used by 'db_lock()'.
**		Usercode -- set to the persons effective user code
**			(that is, after the -u processing).  Only
**			the INGRES user or the DBA can use the -u
**			flag.
**		Pathname -- set to the pathname of the INGRES subtree.
**		Status -- an integer set to the user status field
**			of the users file for the real user.
**		Ing_uid -- set to the user id of the INGRES user.
**
**		The rubout signal (signal 2) is caught, and refered
**		to the standard rubout processor (see rub.c); thus,
**		a routine called 'rubproc' must be defined in the
**		standalone code (which will just call exit, in the
**		normal case).
**
**		The 'adminhdr' part of the 'Admin' struct is filled
**		in.  This is not done with readadmin() and is not
**		equivalent to an 'admininit()', but it does make
**		the DBA and database status available.
**
**		This routine can also exit immediately with an
**		error message.
**
**	Defined Constants:
**		MAXPARGS -- the maximum number of parameter type
**			arguments to any standalone program.
**		MAXFARGS -- the maximum number of flag type arg-
**			uments to any standalong program (not inclu-
**			ding flags in the users file, and the +-w
**			and -u flags).
**
**	Files:
**		/etc/passwd -- to get the pathname for user "ingres".
**		.../files/users -- to get all the per-user information,
**			and to process the -u flag.
**
**	Compilation Flags:
**		xB_UNIX, xV6_UNIX -- see comments in aux.h
**
**	Trace Flags:
**		none
*/


# define	MAXFARGS	15	/* maximum flag-type arguments */
# define	MAXPARGS	20	/* maximum parameter-type args */

char	*Usercode;	/* the usercode of the effective user */
char	*Pathname;	/* path of INGRES subtree */
int	Status;		/* the user status of the real user */
int	Rubignored;	/* set if rubouts ignored */
			/* (also in initproc for system processes) */
int	Wait_action;	/* the action on the db_lock */
char	*Dbpath;	/* the pathname of the database */
char	*Flagvect[MAXFARGS+1];	/* the flags from argv */
char	*Parmvect[MAXPARGS+1];	/* the parameters from argv */
int	Ing_uid;	/* the user id of the INGRES user */

initucode(argc, argv, dbflag, paramlist, waitmode)
int	argc;
char	**argv;
int	dbflag;
char	*paramlist[4];
int	waitmode;
{
	register char	*p;
	char		*q;
	char		c;
	FILE		*iop;
	static char	sbuf[MAXLINE * 2];
	register char	*sbufp;
	char		buf[MAXLINE+1];
	register int	i;
	int		npermit;
	int		rtval;
	char		*field[UF_NFIELDS];
	int		actualuid;
	auto int	uid;
	auto int	gid;
	int		waitflag;
	char		*userflag;
	struct sgttyb	gttydummy;
	int		fvi, pvi;
	char		**avp;
	char		usr_ovrd[3];
	static int	reenter;
	extern		rubcatch();
	static short	tvect[100];
	bool		nobuffer;
# ifdef xV7_UNIX
	extern char	*getenv();
# endif xV7_UNIX

	/*
	**  Set up interrupts.
	*/

	reenter = 0;
	setexit();
	if (reenter++)
		exit(-1);
	if (signal(2, 1) == 0)
		signal(2, rubcatch);
# ifdef xV6_UNIX
	for (avp = argv; *avp != 0 && *avp != (char *) -1; avp++)
		continue;
	*avp = NULL;
# endif

	/*
	**  Do basic initialization, such as setting trace flags.
	*/

	nobuffer = tTrace(argv, 'T', tvect, 100);
	if (!nobuffer)
		set_so_buf();
	sbufp = sbuf;

	/*
	**  Get pathname of INGRES subtree from /etc/passwd file
	**  entry for USERINGRES (presumably "ingres") and save it
	**  in 'Pathname'.
	**
	**  This algorithm suggested by Jim Popa.
	*/

# ifdef xV7_UNIX
	Pathname = getenv("INGPATH");
	if (Pathname == NULL)
	{
# endif xV7_UNIX
		if ((iop = fopen("/etc/passwd", "r")) == NULL)
			syserr("initucode: passwd");

		do
		{
			if (fgets(buf, MAXLINE, iop) == NULL)
				syserr("initucode: no INGRES");
			
			/* decode passwd entry */
			i = 0;
			for (p = buf; *p != '\n' && *p != '\0'; p++)
			{
				if (*p == ':')
				{
					*p = 0;
					i++;
					field[i] = p + 1;
				}
			}
			*p = '\0';

			/* check for enough fields for valid entry */
			if (i < 3)
				syserr("initucode: passwd fmt %s", buf);
		} while (!sequal(buf, USERINGRES));

		/* we now have the INGRES passwd file entry in 'buf' */
		fclose(iop);

		/* copy pathname entry into 'Pathname' variable */
		Pathname = sbufp;
		sbufp += smove(field[i - 1], sbufp) + 1;
# ifdef PATHEXT
		sbufp += smove(PATHEXT, sbufp - 1);
# endif PATHEXT
# ifdef xV7_UNIX
	}
# endif xV7_UNIX

	/* create the INGRES user id */
	if (atoi(p = field[2], &Ing_uid) != 0)
		syserr("initucode: bad Ing_uid \"%s\"", p);
# ifdef xV6_UNIX
	Ing_uid &= 0377;
# endif
# ifdef xB_UNIX
	if (atoi(p = field[3], &gid) != 0)
		syserr("initucode: bad Ing_gid %s", p);
	Ing_uid = (Ing_uid & 0377) | ((gid & 0377) << 8);
# endif

	/*
	**  Scan the argument vector.  The following flags are pulled
	**  out of the vector (and argc and argv are adjusted so it
	**  looks like they never existed):
	**	+w, -w -- (don't) wait for the database to be free.
	**	-uxxx -- run as user xxx.  If first character is a
	**	colon, the format must be '-u:xx' where 'xx' is the
	**	internal user code.
	*/

	avp = argv;
	fvi = 0;
	pvi = 0;
	waitflag = 0;
	userflag = NULL;
	usr_ovrd[0] = 0;

	for (i = argc; --i > 0; )
	{
		p = *++avp;
		if (p[0] == '+')
		{
			if (p[1] == 'w')
				waitflag = 1;
			else
				goto boring;
		}
		else if (p[0] == '-')
		{
			switch (p[1])
			{
			  case 'w':
				waitflag = -1;
				break;
			
			  case 'u':
				if (p[2] == ':')
				{
					if (p[3] == 0 || p[4] == 0 || p[5] != 0)
					{
						printf("Bad flag %s\n", p);
						exit(-1);
					}
					smove(&p[3], usr_ovrd);
				}
				else
					userflag = &p[2];
				break;

			  default:
				/* not an interesting flag */
			boring:
				if (fvi >= MAXFARGS)
				{
					printf("Too many flags\n");
					exit(-1);
				}
				Flagvect[fvi++] = p;
				break;
			}
		}
		else
		{
			/* not a flag: save in Parmvect */
			if (pvi >= MAXPARGS)
			{
				printf("Too many parmameters\n");
				exit(-1);
			}
			Parmvect[pvi++] = p;
		}
	}

	if (pvi <= 0 && dbflag)
	{
		return (4);	/* no database name specified */
	}

	/*
	**  Scan the "users" file.
	*/

	if ((iop = fopen(ztack(Pathname, "/files/users"), "r")) == NULL)
		syserr("initucode: open error");
	
	/* get uid (out of loop) for test */
#	ifdef xV6_UNIX
	actualuid = getuid() & 0377;
#	endif
#	ifndef xV6_UNIX
	actualuid = getuid();
#	endif
	
	/* scan users file, one line at a time */
	rtval = 3;
	while ((Usercode == NULL || userflag != NULL) && fgets(buf, MAXLINE, iop) != NULL)
	{
	
		/* decode users file entry */
		i = 0;
		field[0] = buf;
		for (p = buf; *p != '\n' && *p != '\0'; p++)
		{
			if (*p == ':')
			{
				*p = 0;
				i++;
				field[i] = p + 1;
			}
		}
		*p = '\0';

		/* check for correct number of fields */
		if (i != UF_NFIELDS - 1)
			syserr("initucode: users fmt %s", buf);

		/*
		**  Check to see if this entry is the override user.
		**  If so, save his user code in usr_ovrd.
		*/

		if (userflag != NULL && sequal(userflag, field[UF_NAME]))
		{
			smove(field[UF_UCODE], usr_ovrd);
			userflag = NULL;
		}

		/* don't bother with this shit if not needed */
		if (Usercode != NULL)
			continue;
		
		/*
		**  Build the user id of this entry into 'uid'
		**  and see if it is this user.
		*/

		if (atoi(p = field[UF_UID], &uid) != 0)
			syserr("initucode: users: bad UID %s", p);

#		ifdef xB_UNIX
		if (atoi(p = field[UF_GID], &gid) != 0)
			syserr("initucode: users: bad GID %s", p);
		uid = (uid & 0377) | ((gid & 0377) << 8);
#		endif

#		ifdef xV6_UNIX
		if ((uid & 0377) != actualuid)
			continue;
#		endif
#		ifndef xV6_UNIX
		if (uid != actualuid)
			continue;
#		endif

		/*
		**  We now have the real user entry.
		**	Fetch the usercode, the status bits, and other
		**	fields from the users file, and save them in
		**	a safe place (sbuf).
		*/

		Usercode = sbufp;
		sbufp += smove(field[UF_UCODE], sbufp) + 1;
		Status = oatoi(field[UF_STAT]);
		if (paramlist != NULL)
		{
			for (i = 0; i < 4; i++)
			{
				paramlist[i] = sbufp;
				sbufp += smove(field[UF_FLAGS + i], sbufp) + 1;
			}
		}

		/* validate access permission */
		rtval = 0;
		if (!dbflag || (Status & U_SUPER) != 0)
			continue;
		p = field[UF_DBLIST];
		if (*p == 0)
			continue;

		/* select permission/no-permission */
		npermit = 0;
		if (*p == '-')
		{
			p++;
			npermit++;
		}

		/* scan for database listed */
		if (!npermit)
			rtval = 2;
		for (c = *p; c != 0; p = q + 1)
		{
			for (q = p; *q != ',' && *q != 0; q++)
				continue;
			c = *q;
			*q = 0;
			if (sequal(Parmvect[0], p))
			{
				rtval = npermit ? 2 : 0;
				break;
			}
		}
	}
	fclose(iop);

	if (rtval != 0)
		return (rtval);

	/*
	**  Check for existance of the database.  This is done by
	**	first building the pathname of the database into
	**	'Dbpath', and then reading the admin file (just
	**	the adhdr part).
	*/

	if (dbflag)
	{
		Dbpath = sbufp;
		switch (i = initdbpath(Parmvect[0], Dbpath, TRUE))
		{
		  case 0:
			rtval = 0;
			break;

		  case 1:
			rtval = 5;
			break;

		  case 2:
			rtval = 1;
			break;

		  case 3:
			rtval = 6;
			break;

		  default:
			syserr("initucode: initdbpath %d", i);
		}
		sbufp += length(Dbpath) + 1;

		if (rtval == 0 || rtval == 5)
		{
			i = open(ztack(Dbpath, "/admin"), 0);
			if (i < 0)
				rtval += 1;
			else
			{
				/* open and check admin file */
				checkadmin(i);
				close(i);
			}
		}
	}

	/*
	**  Check to see if the name on the -u flag is valid, and
	**	that this user is allowed to use it.
	*/

	if (userflag != NULL)
	{
		printf("Invalid user name %s\n", userflag);
		exit(-1);
	}
	if (usr_ovrd[0] != '\0')
	{
		if ((Status & U_SUPER) == 0)
		{
			if (!dbflag || !bequal(Admin.adhdr.adowner, Usercode, 2))
			{
				printf("You may not use the -u flag\n");
				exit(-1);
			}
		}
		bmove(usr_ovrd, Usercode, 2);
	}

	/*
	**  Process the +-w flag.
	**	First, determine the locking mode.  If +w, always
	**	wait; if -w, never wait; if unspecified, wait if in
	**	background, but print error and exit if running
	**	interactive.
	*/

	if (waitflag > 0 || (waitflag == 0 && gtty(0, &gttydummy) < 0))
		Wait_action = A_SLP;
	else
		Wait_action = A_RTN;
	if (dbflag && waitmode >= 0)
		db_lock(waitmode);
	
	/*
	**  Return authorization value.
	*/

	return (rtval);
}
/*
**  DB_LOCK -- lock database
**
**	Locks the database.  Everyone should do this before using any
**	database.
**
**	Parameters:
**		database -- the pathname of the database.
**		mode -- M_EXCL -- get an exclusive lock.
**			M_SHARE -- get a shared lock.
**
**	Returns:
**		none
**
**	Side Effects:
**		Alockdes is opened.
*/

struct lockreq	Lock;	/* the database lock structure */

db_lock(mode)
int	mode;
{
	if ((Admin.adhdr.adflags & A_DBCONCUR) == 0)
		return;
	if (Alockdes < 0)
		Alockdes = open("/dev/lock", 1);
	if (setdbl(Wait_action, mode) < 0)
	{
		printf("Database temporarily unavailable\n");
		exit(1);
	}
}
/*
**  INITDBPATH -- initialize the pathname of the database
**
**	The pathname of a specified database is created.  Indirection
**	via a file is supported, so that if the pathname is a file,
**	the first line of the file is read and used as the pathname
**	of the real database.
**
**	Parameters:
**		database -- the name of the database.  If NULL,
**			the pathname of datadir is returned.
**		dbbuf -- a buffer into which the pathname should
**			be dumped.
**		follow -- if set, follow the indirect chain of
**			database pathnames.
**
**	Returns:
**		0 -- database exists in datadir
**		1 -- database exists, but I followed a pointer.
**		2 -- database doesn't exist in datadir.
**		3 -- databae doesn't exist, but I followed a pointer.
**
**	Side Effects:
**		none.
*/

initdbpath(database, dbpath, follow)
char	*database;
char	*dbpath;
int	follow;
{
	struct stat	ibuf;
	register char	*d;
	register FILE	*f;
	register int	phase;
	int		retval;
	int		uid;
	extern char	*index();

	d = dbpath;

	if (database == NULL)
	{
# ifndef xDBPATH
		concat(Pathname, "/data/base/", d);
# else
		smove(xDBPATH, d);
# endif
		return (0);
	}

	/* get the basic pathname */
	concat(ztack(Pathname, "/datadir/"), database, d);

	/*
	** Iterate looking for database.
	**	"Phase" is what we are trying:
	**	   -1 -- looking in datadir
	**	    0 -- looking in data/base
	**	    1 -- following indirect.
	*/

	retval = 2;
	for (phase = -1;;)
	{
		/* find out what sort of filesystem node this is */
		if (stat(d, &ibuf) < 0)
		{
			if (phase < 0)
			{
# ifdef xDBPATH
				concat(xDBPATH, database, d);
# else
				concat(ztack(Pathname, "/data/base/"), database, d);
# endif
				phase = 0;
				continue;
			}
			else
				return (retval);
		}
		
		/* set up the lock structure for future use */
		bmove(&ibuf, Lock.dbnode, 4);

		retval -= 2;
		if ((ibuf.st_mode & S_IFMT) == S_IFDIR)
			return (retval);
		
		/* if second time through, the database must be a directory */
		if (phase > 0)
			syserr("initdbpath: not direc");
		
		/* if we shouldn't follow the chain, say it exists */
		if (!follow)
			return (3);
		
		/* it's a file -- see if we can use it */
		uid = ibuf.st_uid;
#		ifdef xB_UNIX
		uid = (uid & 0377) | ((ibuf.st_gid & 0377) << 8);
#		endif
#		ifdef xV6_UNIX
		uid &= 0377;
#		endif
		if (uid != Ing_uid || (ibuf.st_mode & 0777) != 0600)
			return (3);
		
		f = fopen(d, "r");
		if (f == NULL)
			syserr("initdbpath: fopen");
	
		/* read the pathname of the database */
		if (fgets(d, MAXLINE, f) == NULL || d[0] != '/')
			syserr("initdbpath: bad indirect");
		*index(d, '\n') = '\0';
		fclose(f);

		/* prepare for next iteration */
		retval = 3;
		phase = 1;
	}
}
