
/*  This is a finger program.  It prints out useful information about users
 *  by digging it up from various system files.  It is not very portable
 *  because the most useful parts of the information (the full user name,
 *  office, and phone numbers) are all stored in the VAX-unused gecos field
 *  of /etc/passwd, which, unfortunately, other UNIXes use for other things.
 *
 *  There are three output formats, all of which give login name, teletype
 *  line number, and login time.  The short output format is reminiscent
 *  of finger on ITS, and gives one line of information per user containing
 *  in addition to the minimum basic requirements (MBR), the full name of
 *  the user, his idle time and office location and phone number.  The
 *  quick style output is UNIX who-like, giving only name, teletype and
 *  login time.  Finally, the long style output give the same information
 *  as the short (in more legible format), the home directory and shell
 *  of the user, and, if it exits, a copy of the file .plan in the users
 *  home directory.  Finger may be called with or without a list of people
 *  to finger -- if no list is given, all the people currently logged in
 *  are fingered.
 *
 *  The program is validly called by one of the following:
 *
 *	finger			{short form list of users}
 *	finger -l		{long form list of users}
 *	finger -b		{briefer long form list of users}
 *	finger -q		{quick list of users}
 *	finger -i		{quick list of users with idle times}
 *	finger namelist		{long format list of specified users}
 *	finger -s namelist	{short format list of specified users}
 *	finger -w namelist	{narrow short format list of specified users}
 *
 *  where 'namelist' is a list of users login names.
 *  The other options can all be given after one '-', or each can have its
 *  own '-'.  The -f option disables the printing of headers for short and
 *  quick outputs.  The -b option briefens long format outputs.  The -p
 *  option turns off plans for long format outputs.
 */

#include	<sys/types.h>
#include	<sys/stat.h>
#include	<sgtty.h>
#include	<utmp.h>
#include	<signal.h>
#include	<pwd.h>
#include	<stdio.h>
#include	<sccs.h>
#include	<lastlog.h>
#include	<time.h>

#define		ASTERISK	'*'	/* ignore this in real name */
#define		BLANK		' '	/* blank character (i.e. space) */
#define		CAPITALIZE	0137&	/* capitalize character macro */
#define		COMMA		','	/* separator in pw_gecos field */
#define		COMMAND		'-'	/* command line flag char */
#define		CORY		'C'	/* cory hall office */
#define		EVANS		'E'	/* evans hall office */
#define		LINEBREAK	012	/* line feed */
#define		NULLSTR		""	/* the null string, opposed to NULL */
#define		SAMENAME	'&'	/* repeat login name in real name */
#define		TALKABLE	0222	/* tty is writeable if 222 mode */

struct  person  {			/* one for each person fingered */
	char		name[ 9 ];	/* login name */
	char		tty[ 9 ];	/* NULL terminated tty line */
	long		loginat;	/* time of login (possibly last) */
	long		idletime;	/* how long idle (if logged in) */
	short int	loggedin;	/* flag for being logged in */
	short int	writeable;	/* flag for tty being writeable */
	char		*realname;	/* pointer to full name */
	char		*office;	/* pointer to office name */
	char		*officephone;	/* pointer to office phone no. */
	char		*homephone;	/* pointer to home phone no. */
	char		*random;	/* for any random stuff in pw_gecos */
	struct  passwd	*pwd;		/* structure of /etc/passwd stuff */
	struct  person	*link;		/* link to next person */
};

struct  passwd			*NILPWD = 0;
struct  person			*NILPERS = 0;

int		persize		= sizeof( struct person );
int		pwdsize		= sizeof( struct passwd );

char		LASTLOG[]	= "/usr/adm/lastlog";	/* last login info */
char		USERLOG[]	= "/etc/utmp";		/* who is logged in */
char		outbuf[BUFSIZ];				/* output buffer */
char		*ctime();

int		unbrief		= 1;		/* -b option default */
int		header		= 1;		/* -f option default */
int		hack		= 1;		/* -h option default */
int		idle		= 0;		/* -i option default */
int		large		= 0;		/* -l option default */
int		match		= 1;		/* -m option default */
int		plan		= 1;		/* -p option default */
int		unquick		= 1;		/* -q option default */
int		small		= 0;		/* -s option default */
int		wide		= 1;		/* -w option default */

int		lf;
int		llopenerr;

long		tloc;				/* current time */



main( argc, argv )

    int		argc;
    char	*argv[];

{
	FILE			*fp,  *fopen();		/* for plans */
	struct  passwd		*getpwent();		/* read /etc/passwd */
	struct  person		*person1,  *p,  *pend;	/* people */
	struct  passwd		*pw;			/* temporary */
	struct  utmp		user;			/*   ditto   */
	char			*malloc();
	char			*s,  *pn,  *ln;
	char			c;
	char			*PLAN = "/.plan";	/* what plan file is */
	char			*PROJ = "/.project";	/* what project file */
	int			PLANLEN = strlen( PLAN );
	int			PROJLEN = strlen( PROJ );
	int			numnames = 0;
	int			orgnumnames;
	int			uf;
	int			usize = sizeof user;
	int			unshort;
	int			i, j;
	int			fngrlogin;

	setbuf( stdout, outbuf );			/* buffer output */

    /*  parse command line for (optional) arguments */

	i = 1;
	if(  strcmp( *argv, "sh" )  )  {
	    fngrlogin = 0;
	    while( i++ < argc  &&  (*++argv)[0] == COMMAND )  {
		for( s = argv[0] + 1; *s != NULL; s++ )  {
			switch  (*s)  {

			    case 'b':
				    unbrief = 0;
				    break;

			    case 'f':
				    header = 0;
				    break;

			    case 'h':
				    hack = 0;
				    break;

			    case 'i':
				    idle = 1;
				    unquick = 0;
				    break;

			    case 'l':
				    large = 1;
				    break;

			    case 'm':
				    match = 0;
				    break;

			    case 'p':
				    plan = 0;
				    break;

			    case 'q':
				    unquick = 0;
				    break;

			    case 's':
				    small = 1;
				    break;

			    case 'w':
				    wide = 0;
				    break;

			    default:
				fprintf( stderr, "finger: Usage -- 'finger [-bfhilmpqsw] [login1 [login2 ...] ]'\n" );
				exit( 1 );
			}
		}
	    }
	}
	else  {
	    fngrlogin = 1;
	}
	if( unquick )  {
	    time( &tloc );
	}
	else  {
	    if( idle )  {
		time( &tloc );
	    }
	}

    /*  i > argc means no login names given so get them by reading USERLOG */

	if(  (i > argc)  ||  fngrlogin  )  {
	    unshort = large;
	    if(  ( uf = open(USERLOG, 0) ) >= 0  )  {
		user.ut_name[0] = NULL;
		while( user.ut_name[0] == NULL )  {
		    if( read( uf, (char *) &user, usize ) != usize )  {
			printf( "\nNo one logged on\n" );
			exit( 0 );
		    }
		}
		person1 = (struct person  *) malloc( persize );
		for( j = 0; j < 8; j++ )  {
		    person1->tty[j] = user.ut_line[j];
		    person1->name[j] = user.ut_name[j];
		}
		person1->name[8] = NULL;
		person1->tty[8] = NULL;
		person1->loginat = user.ut_time;
		person1->pwd = NILPWD;
		person1->loggedin = 1;
		numnames++;
		p = person1;
		while( read( uf, (char *) &user, usize ) == usize )  {
		    if( user.ut_name[0] == NULL )  continue;
		    p->link = (struct person  *) malloc( persize );
		    p = p->link;
		    for( j = 0; j < 8; j++ )  {
			p->tty[j] = user.ut_line[j];
			p->name[j] = user.ut_name[j];
		    }
		    p->name[8] = NULL;
		    p->tty[8] = NULL;
		    p->loginat = user.ut_time;
		    p->pwd = NILPWD;
		    p->loggedin = 1;
		    numnames++;
		}
		p->link = NILPERS;
		close( uf );
	    }
	    else  {
		fprintf( stderr, "finger: error opening %s\n", USERLOG );
		exit( 2 );
	    }

		/*  if we are doing it, read /etc/passwd for the useful info */

	    if( unquick )  {
		setpwent();
		fwopen();
		i = numnames;
		while(  ( (pw = getpwent()) != NILPWD )  &&  ( i > 0 )  )  {
		    p = person1;
		    do  {
			if( p->pwd == NILPWD )  {
			    if(  strcmp( p->name, pw->pw_name ) == 0  )  {
				p->pwd = (struct passwd  *) malloc( pwdsize );
				pwdcopy( p->pwd, pw );
				decode( p );
				i--;
			    }
			}
			p = p->link;
		    }  while( p != NILPERS );
		}
		fwclose();
		endpwent();
	    }
	}

    /* get names from command line and check to see if they're  logged in */

	else  {
	    unshort = ( small == 1 ? 0 : 1 );
	    i++;
	    person1 = (struct person  *) malloc( persize );
	    strcpy(  person1->name, (argv++)[ 0 ]  );
	    person1->loggedin = 0;
	    person1->pwd = NILPWD;
	    numnames++;
	    p = person1;
	    while( i++ <= argc )  {
		p->link = (struct person  *) malloc( persize );
		p = p->link;
		strcpy(  p->name, (argv++)[ 0 ]  );
		p->loggedin = 0;
		p->pwd = NILPWD;
		numnames++;
	    }
	    p->link = NILPERS;
	    pend = p;

		/*  if we are doing it, read /etc/passwd for the useful info */

	    orgnumnames = numnames;
	    if( unquick )  {
		setpwent();
		while(  ( pw = getpwent() ) != NILPWD  )  {
		    p = person1;
		    i = 0;
		    do  {
			if( strcmp( p->name, pw->pw_name ) == 0    ||
			    matchcmp( pw->pw_gecos, pw->pw_name, p->name ) )  {
			    if( p->pwd == NILPWD )  {
				p->pwd = (struct passwd  *) malloc( pwdsize );
				pwdcopy( p->pwd, pw );
			    }
			    else  {
				pend->link = (struct person  *) malloc(persize);
				pend = pend->link;
				pend->link = NILPERS;
				strcpy( pend->name, p->name );
				pend->pwd = (struct passwd  *) malloc(pwdsize);
				pwdcopy( pend->pwd, pw );
				numnames++;
			    }
			}
			p = p->link;
		    }  while( ++i < orgnumnames );
		}
		endpwent();
	    }

		/*  Now get login information */

	    if(  ( uf = open(USERLOG, 0) ) >= 0  )  {
		while( read( uf, (char *) &user, usize ) == usize )  {
		    if( user.ut_name[0] == NULL )  continue;
		    p = person1;
		    do  {
			pw = p->pwd;
			if( pw == NILPWD )  {
			    p = p->link;
			    continue;
			}
			i = 0;
			while((i < 8) && (pw->pw_name[i] == user.ut_name[i]))  {
			    if( pw->pw_name[i] == NULL )  {
				i = 8;
				break;
			    }
			    i++;
			}
			if( i == 8 )  {
			    if( p->loggedin == 1 )  {
				pend->link = (struct person  *) malloc(persize);
				pend = pend->link;
				pend->link = NILPERS;
				strcpy( pend->name, p->name );
				for( j = 0; j < 8; j++ )  {
				    pend->tty[j] = user.ut_line[j];
				}
				pend->tty[ 8 ] = NULL;
				pend->loginat = user.ut_time;
				pend->loggedin = 2;
				pend->pwd = (struct passwd  *) malloc(pwdsize);
				pwdcopy( pend->pwd, pw );
				numnames++;
			    }
			    else  {
				if( p->loggedin != 2 )  {
				    for( j = 0; j < 8; j++ )  {
					p->tty[j] = user.ut_line[j];
				    }
				    p->tty[ 8 ] = NULL;
				    p->loginat = user.ut_time;
				    p->loggedin = 1;
				}
			    }
			}
			p = p->link;
		    }  while( p != NILPERS );
		}
		fwopen();
		p = person1;
		while( p != NILPERS )  {
		    if( p->loggedin == 2 )  {
			p->loggedin = 1;
		    }
		    decode( p );
		    p = p->link;
		}
		fwclose();
		close( uf );
	    }
	    else  {
		fprintf( stderr, "finger: error opening %s\n", USERLOG );
		exit( 2 );
	    }
	}

    /* print out what we got */

	if( header )  {
	    if( unquick )  {
		if( !unshort )  {
		    if( wide )  {
			printf(
"Login       Name              TTY Idle    When            Office\n" );
		    }
		    else  {
			printf(
"Login    TTY Idle    When            Office\n" );
		    }
		}
	    }
	    else  {
		printf( "Login      TTY            When" );
		if( idle )  {
		    printf( "             Idle" );
		}
		printf( "\n" );
	    }
	}
	p = person1;
	do  {
	    if( unquick )  {
		if( unshort )  {
		    personprint( p );
		    if( p->pwd != NILPWD )  {
			if( hack )  {
			    s = malloc(strlen((p->pwd)->pw_dir) + PROJLEN + 1 );
			    strcpy(  s, (p->pwd)->pw_dir  );
			    strcat( s, PROJ );
			    if(  ( fp = fopen( s, "r") )  != NULL  )  {
				printf( "Project: " );
				while(  ( c = getc(fp) )  !=  EOF  )  {
				    if( c == LINEBREAK )  {
					break;
				    }
				    putc( c, stdout );
				}
				fclose( fp );
				printf( "\n" );
			    }
			}
			if( plan )  {
			    s = malloc( strlen( (p->pwd)->pw_dir ) + PLANLEN + 1 );
			    strcpy(  s, (p->pwd)->pw_dir  );
			    strcat( s, PLAN );
			    if(  ( fp = fopen( s, "r") )  == NULL  )  {
				printf( "No Plan.\n" );
			    }
			    else  {
				printf( "Plan:\n" );
				while(  ( c = getc(fp) )  !=  EOF  )  {
				    putc( c, stdout );
				}
				fclose( fp );
			    }
			}
		    }
		    if( p->link != NILPERS )  {
			printf( "\n" );
		    }
		}
		else  {
		    shortprint( p );
		}
	    }
	    else  {
		quickprint( p );
	    }
	    p = p->link;
	}  while( p != NILPERS );
	exit(1);
}


/*  given a pointer to a pwd (pfrom) copy it to another one, allocating
 *  space for all the stuff in it.  Note: Only the useful (what the
 *  program currently uses) things are copied.
 */

pwdcopy( pto, pfrom )		/* copy relevant fields only */

    struct  passwd		*pto,  *pfrom;
{
	pto->pw_name = malloc(  strlen( pfrom->pw_name ) + 1  );
	strcpy( pto->pw_name, pfrom->pw_name );
	pto->pw_uid = pfrom->pw_uid;
	pto->pw_gecos = malloc(  strlen( pfrom->pw_gecos ) + 1  );
	strcpy( pto->pw_gecos, pfrom->pw_gecos );
	pto->pw_dir = malloc(  strlen( pfrom->pw_dir ) + 1  );
	strcpy( pto->pw_dir, pfrom->pw_dir );
	pto->pw_shell = malloc(  strlen( pfrom->pw_shell ) + 1  );
	strcpy( pto->pw_shell, pfrom->pw_shell );
}


/*  print out information on quick format giving just name, tty, login time
 *  and idle time if idle is set.
 */

quickprint( pers )

    struct  person		*pers;
{
	int			idleprinted;

	printf( "%-8.8s", pers->name );
	printf( "  " );
	if( pers->loggedin )  {
	    if( idle )  {
		findidle( pers );
		if( pers->writeable )  {
		    printf(  " %-8.8s %-16.16s",
			pers->tty, ctime( &pers->loginat )  );
		}
		else  {
		    printf(  "*%-8.8s %-16.16s",
			pers->tty, ctime( &pers->loginat )  );
		}
		printf( "   " );
		idleprinted = ltimeprint( &pers->idletime );
	    }
	    else  {
		printf(  " %-8.8s %-16.16s",
		    pers->tty, ctime( &pers->loginat )  );
	    }
	}
	else  {
	    printf( "          Not Logged In" );
	}
	printf( "\n" );
}


/*  print out information in short format, giving login name, full name,
 *  tty, idle time, login time, office location and phone.
 */

shortprint( pers )

    struct  person	*pers;

{
	struct  passwd		*pwdt = pers->pwd;
	char			buf[ 26 ];
	int			i,  len,  offset,  dialup;

	if( pwdt == NILPWD )  {
	    printf( "%-8.8s", pers->name );
	    printf( "       ???\n" );
	    return;
	}
	printf( "%-8.8s", pwdt->pw_name );
	dialup = 0;
	if( wide )  {
	    if(  strlen( pers->realname ) > 0  )  {
		printf( " %-20.20s", pers->realname );
	    }
	    else  {
		printf( "        ???          " );
	    }
	}
	if( pers->loggedin )  {
	    if( pers->writeable )  {
		printf( "  " );
	    }
	    else  {
		printf( " *" );
	    }
	}
	else  {
	    printf( "  " );
	}
	if(  strlen( pers->tty ) > 0  )  {
	    strcpy( buf, pers->tty );
	    if(  (buf[0] == 't')  &&  (buf[1] == 't')  &&  (buf[2] == 'y')  )  {
		offset = 3;
		for( i = 0; i < 2; i++ )  {
		    buf[i] = buf[i + offset];
		}
	    }
	    if(  (buf[0] == 'd')  &&  pers->loggedin  )  {
		dialup = 1;
	    }
	    printf( "%-2.2s ", buf );
	}
	else  {
	    printf( "   " );
	}
	strcpy( buf, ctime( &pers->loginat ) );
	if( pers->loggedin )  {
	    stimeprint( &pers->idletime );
	    offset = 7;
	    for( i = 4; i < 19; i++ )  {
		buf[i] = buf[i + offset];
	    }
	    printf( " %-9.9s ", buf );
	}
	else  {
	    printf( " " );
	    offset = 4;
	    for( i = 0; i <22; i++ )  {
		buf[i] = buf[i + offset];
	    }
	    printf( "<%-12.12s>", buf );
	}
	len = strlen( pers->homephone );
	if(  dialup  &&  (len > 0)  )  {
	    if( len == 8 )  {
		printf( "             " );
	    }
	    else  {
		if( len == 12 )  {
		    printf( "         " );
		}
		else {
		    for( i = 1; i <= 21 - len; i++ )  {
			printf( " " );
		    }
		}
	    }
	    printf( "%s", pers->homephone );
	}
	else  {
	    if(  strlen( pers->office ) > 0  )  {
		printf( " %-11.11s", pers->office );
		if(  strlen( pers->officephone ) > 0  )  {
		    printf( " %8.8s", pers->officephone );
		}
		else  {
		    if( len == 8 )  {
			printf( " %8.8s", pers->homephone );
		    }
		}
	    }
	    else  {
		if(  strlen( pers->officephone ) > 0  )  {
		    printf( "             %8.8s", pers->officephone );
		}
		else  {
		    if( len == 8 )  {
			printf( "             %8.8s", pers->homephone );
		    }
		    else  {
			if( len == 12 )  {
			    printf( "         %12.12s", pers->homephone );
			}
		    }
		}
	    }
	}
	printf( "\n" );
}


/*  print out a person in long format giving all possible information.
 *  directory and shell are inhibited if unbrief is clear.
 */

personprint( pers )

    struct  person	*pers;

{
	struct  passwd		*pwdt = pers->pwd;
	int			idleprinted;

	if( pwdt == NILPWD )  {
	    printf( "Login name: %-10s", pers->name );
	    printf( "			" );
	    printf( "In real life: ???\n");
	    return;
	}
	printf( "Login name: %-10s", pwdt->pw_name );
	if( pers->loggedin )  {
	    if( pers->writeable )  {
		printf( "			" );
	    }
	    else  {
		printf( "	(messages off)	" );
	    }
	}
	else  {
	    printf( "			" );
	}
	if(  strlen( pers->realname ) > 0  )  {
	    printf( "In real life: %-s", pers->realname );
	}
	if(  strlen( pers->office ) > 0  )  {
	    printf( "\nOffice: %-.11s", pers->office );
	    if(  strlen( pers->officephone ) > 0  )  {
		printf( ", %s", pers->officephone );
		if(  strlen( pers->homephone ) > 0  )  {
		    printf( "		Home phone: %s", pers->homephone );
		}
		else  {
		    if(  strlen( pers->random ) > 0  )  {
			printf( "	%s", pers->random );
		    }
		}
	    }
	    else  {
		if(  strlen( pers->homephone ) > 0  )  {
		    printf("			Home phone: %s",pers->homephone);
		}
		if(  strlen( pers->random ) > 0  )  {
		    printf( "			%s", pers->random );
		}
	    }
	}
	else  {
	    if(  strlen( pers->officephone ) > 0  )  {
		printf( "\nPhone: %s", pers->officephone );
		if(  strlen( pers->homephone ) > 0  )  {
		    printf( "\n, %s", pers->homephone );
		    if(  strlen( pers->random ) > 0  )  {
			printf( ", %s", pers->random );
		    }
		}
		else  {
		    if(  strlen( pers->random ) > 0  )  {
			printf( "\n, %s", pers->random );
		    }
		}
	    }
	    else  {
		if(  strlen( pers->homephone ) > 0  )  {
		    printf( "\nPhone: %s", pers->homephone );
		    if(  strlen( pers->random ) > 0  )  {
			printf( "%s", pers->random );
		    }
		}
		else  {
		    if(  strlen( pers->random ) > 0  )  {
			printf( "\n%s", pers->random );
		    }
		}
	    }
	}
	if( unbrief )  {
	    printf( "\n" );
	    printf( "Directory: %-25s", pwdt->pw_dir );
	    if(  strlen( pwdt->pw_shell ) > 0  )  {
		printf( "	Shell: %-s", pwdt->pw_shell );
	    }
	}
	if( pers->loggedin )  {
	    register char *ep = ctime( &pers->loginat );
	    printf("\nOn since %15.15s on %-8.8s	", &ep[4], pers->tty );
	    idleprinted = ltimeprint( &pers->idletime );
	    if( idleprinted )  {
		printf( " Idle Time" );
	    }
	}
	else  {
	    register char *ep = ctime( &pers->loginat );
	    printf("\nLast login %16.16s on %.8s", ep, pers->tty );
	}
	printf( "\n" );
}


/*
 *  very hacky section of code to print phone numbers.  filled with
 *  magic constants like 4, 7 and 10.
 */

char  *phone( s, len )

    char		*s;
    int			len;
{
	char		*strsave();
	char		fonebuf[ 15 ];
	int		i;

	switch  (len)  {

	    case  4:
		fonebuf[ 0 ] = ' ';
		fonebuf[ 1 ] = 'x';
		fonebuf[ 2 ] = '2';
		fonebuf[ 3 ] = '-';
		for( i = 0; i <= 3; i++ )  {
		    fonebuf[ 4 + i ] = *s++;
		}
		fonebuf[ 8 ] = NULL;
		return( strsave( &fonebuf[0] ) );
		break;

	    case  7:
		for( i = 0; i <= 2; i++ )  {
		    fonebuf[ i ] = *s++;
		}
		fonebuf[ 3 ] = '-';
		for( i = 0; i <= 3; i++ )  {
		    fonebuf[ 4 + i ] = *s++;
		}
		fonebuf[ 8 ] = NULL;
		return( strsave( &fonebuf[0] ) );
		break;

	    case 10:
		for( i = 0; i <= 2; i++ )  {
		    fonebuf[ i ] = *s++;
		}
		fonebuf[ 3 ] = '-';
		for( i = 0; i <= 2; i++ )  {
		    fonebuf[ 4 + i ] = *s++;
		}
		fonebuf[ 7 ] = '-';
		for( i = 0; i <= 3; i++ )  {
		    fonebuf[ 8 + i ] = *s++;
		}
		fonebuf[ 12 ] = NULL;
		return( strsave( &fonebuf[0] ) );
		break;

	    default:
		fprintf( stderr, "finger: error in phone numbering\n" );
		return( strsave(s) );
		break;
	}
}


/*  decode the information in the gecos field of /etc/passwd
 *  another hacky section of code, but given the format the stuff is in...
 */

decode( pers )

    struct  person	*pers;

{
	struct  passwd		*pwdt = pers->pwd;
	char			buffer[ 40 ],  *bp,  *gp,  *lp;
	char			*phone();
	int			alldigits;
	int			len;
	int			i;

	pers->realname = NULLSTR;
	pers->office = NULLSTR;
	pers->officephone = NULLSTR;
	pers->homephone = NULLSTR;
	pers->random = NULLSTR;
	if(  pwdt != NILPWD )  {
	    gp = pwdt->pw_gecos;
	    bp = &buffer[ 0 ];
	    if( *gp == ASTERISK )  {
		gp++;
	    }
	    while(  (*gp != NULL)  &&  (*gp != COMMA)  )  {
		if( *gp == SAMENAME )  {
		    lp = pwdt->pw_name;
		    *bp++ = CAPITALIZE(*lp++);
		    while( *lp != NULL )  {
			*bp++ = *lp++;
		    }
		}
		else  {
		    *bp++ = *gp;
		}
		gp++;
	    }
	    *bp = NULL;
	    pers->realname = malloc( strlen( &buffer[0] ) + 1 );
	    strcpy( pers->realname, &buffer[0] );
	    if( *gp++ == COMMA )  {
		alldigits = 1;
		bp = &buffer[ 0 ];
		while(  (*gp != NULL)  &&  (*gp != COMMA)  )  {
		    *bp = *gp++;
		    alldigits = alldigits && ('0' <= *bp) && (*bp <= '9');
		    bp++;
		}
		*bp = NULL;
		len = strlen( &buffer[0] );
		if( buffer[ len - 1 ]  ==  CORY )  {
		    strcpy( &buffer[ len - 1 ], " Cory" );
		    pers->office = malloc( len + 5 );
		    strcpy( pers->office, &buffer[0] );
		}
		else  {
		    if( buffer[ len - 1 ] == EVANS )  {
			strcpy( &buffer[ len - 1 ], " Evans" );
			pers->office = malloc( len + 6 );
			strcpy( pers->office, &buffer[0] );
		    }
		    else  {
			if( buffer[ len - 1 ] == 'L' )  {
			    strcpy( &buffer[ len - 3 ], " LBL" );
			    pers->office = malloc( len + 2 );
			    strcpy( pers->office, &buffer[0] );
			}
			else  {
			    if( alldigits )  {
				if( len == 4 )  {
				    pers->officephone = phone(&buffer[0], len);
				}
				else  {
				    if(  (len == 7) || (len == 10)  )  {
					pers->homephone = phone(&buffer[0],len);
				    }
				}
			    }
			    else  {
				pers->random = malloc( len + 1 );
				strcpy( pers->random, &buffer[0] );
			    }
			}
		    }
		}
		if( *gp++ == COMMA )  {
		    bp = &buffer[ 0 ];
		    alldigits = 1;
		    while(  (*gp != NULL)  &&  (*gp != COMMA)  )  {
			*bp = *gp++;
			alldigits = alldigits && ('0' <= *bp) && (*bp <= '9');
			bp++;
		    }
		    *bp = NULL;
		    len = strlen( &buffer[0] );
		    if( alldigits )  {
			if(  len != 4  )  {
			    if(  (len == 7) || (len == 10)  )  {
				pers->homephone = phone( &buffer[0], len );
			    }
			    else  {
				pers->random = malloc( len + 1 );
				strcpy( pers->random, &buffer[0] );
			    }
			}
			else  {
				pers->officephone = phone( &buffer[0], len );
			}
		    }
		    else  {
			pers->random = malloc( len + 1 );
			strcpy( pers->random, &buffer[0] );
		    }
		    if( *gp++ == COMMA )  {
			bp = &buffer[ 0 ];
			alldigits = 1;
			    while(  (*gp != NULL)  &&  (*gp != COMMA)  )  {
				*bp = *gp++;
				alldigits = alldigits && ('0' <= *bp) &&
							(*bp <= '9');
				bp++;
			    }
			*bp = NULL;
			len = strlen( &buffer[0] );
			if( alldigits  &&  ( (len == 7) || (len == 10) )  )  {
			    if( pers->homephone != NULL )  {
				pers->officephone = pers->homephone;
			    }
			    pers->homephone = phone( &buffer[0], len );
			}
			else  {
			    pers->random = malloc( strlen( &buffer[0] ) + 1 );
			    strcpy( pers->random, &buffer[0] );
			}
		    }
		}
	    }
	    if( pers->loggedin == 0 )  {
		findwhen( pers );
	    }
	    else  {
		findidle( pers );
	    }
	}
}


/*  find the last log in of a user by checking the LASTLOG file.
 *  the entry is indexed by the uid, so this can only be done if
 *  the uid is known (which it isn't in quick mode)
 */

fwopen()
{
	if(  ( lf = open(LASTLOG, 0) ) >= 0  )  {
	    llopenerr = 0;
	}
	else  {
	    fprintf( stderr, "finger: lastlog open error\n" );
	    llopenerr = 1;
	}
}


findwhen( pers )

    struct  person	*pers;
{
	struct  passwd		*pwdt = pers->pwd;
	struct  lastlog		ll;
	int			llsize = sizeof ll;
	int			i;

	if( !llopenerr )  {
	    lseek( lf, pwdt->pw_uid*llsize, 0 );
	    if( read( lf, (char *) &ll, llsize ) == llsize )  {
		    for( i = 0; i < 8; i++ )  {
			pers->tty[ i ] = ll.ll_line[ i ];
		    }
		    pers->tty[ 8 ] = NULL;
		    pers->loginat = ll.ll_time;
	    }
	    else  {
		fprintf( stderr, "finger: lastlog read error\n" );
		pers->tty[ 0 ] = NULL;
		pers->loginat = 0L;
	    }
	}
	else  {
	    pers->tty[ 0 ] = NULL;
	    pers->loginat = 0L;
	}
}


fwclose()
{
	if( !llopenerr )  {
	    close( lf );
	}
}


/*  find the idle time of a user by doing a stat on /dev/histty,
 *  where histty has been gotten from USERLOG, supposedly.
 */

findidle( pers )

    struct  person	*pers;
{
	struct  stat		ttystatus;
	struct  passwd		*pwdt = pers->pwd;
	char			buffer[ 20 ];
	char			*TTY = "/dev/";
	int			TTYLEN = strlen( TTY );
	int			i;

	strcpy( &buffer[0], TTY );
	i = 0;
	do  {
	    buffer[ TTYLEN + i ] = pers->tty[ i ];
	}  while( ++i <= 8 );
	if(  stat( &buffer[0], &ttystatus ) >= 0  )  {
	    time( &tloc );
	    if( tloc < ttystatus.st_atime )  {
		pers->idletime = 0L;
	    }
	    else  {
		pers->idletime = tloc - ttystatus.st_atime;
	    }
	    if(  (ttystatus.st_mode & TALKABLE) == TALKABLE  )  {
		pers->writeable = 1;
	    }
	    else  {
		pers->writeable = 0;
	    }
	}
	else  {
	    fprintf( stderr, "finger: error STATing %s\n", &buffer[0] );
	    exit( 4 );
	}
}


/*  print idle time in short format; this program always prints 4 characters;
 *  if the idle time is zero, it prints 4 blanks.
 */

stimeprint( dt )

    long	*dt;
{
	struct  tm		*gmtime();
	struct  tm		*delta;

	delta = gmtime( dt );
	if( delta->tm_yday == 0 )  {
	    if( delta->tm_hour == 0 )  {
		if( delta->tm_min >= 10 )  {
		    printf( " %2.2d ", delta->tm_min );
		}
		else  {
		    if( delta->tm_min == 0 )  {
			printf( "    " );
		    }
		    else  {
			printf( "  %1.1d ", delta->tm_min );
		    }
		}
	    }
	    else  {
		if( delta->tm_hour >= 10 )  {
		    printf( "%3.3d:", delta->tm_hour );
		}
		else  {
		    printf( "%1.1d:%02.2d", delta->tm_hour, delta->tm_min );
		}
	    }
	}
	else  {
	    printf( "%3dd", delta->tm_yday );
	}
}


/*  print idle time in long format with care being taken not to pluralize
 *  1 minutes or 1 hours or 1 days.
 */

ltimeprint( dt )

    long	*dt;
{
	struct  tm		*gmtime();
	struct  tm		*delta;
	int			printed = 1;

	delta = gmtime( dt );
	if( delta->tm_yday == 0 )  {
	    if( delta->tm_hour == 0 )  {
		if( delta->tm_min >= 10 )  {
		    printf( "%2d minutes", delta->tm_min );
		}
		else  {
		    if( delta->tm_min == 0 )  {
			if( delta->tm_sec > 10 )  {
			    printf( "%2d seconds", delta->tm_sec );
			}
			else  {
			    printed = 0;
			}
		    }
		    else  {
			if( delta->tm_min == 1 )  {
			    if( delta->tm_sec == 1 )  {
				printf( "%1d minute %1d second",
				    delta->tm_min, delta->tm_sec );
			    }
			    else  {
				printf( "%1d minute %d seconds",
				    delta->tm_min, delta->tm_sec );
			    }
			}
			else  {
			    if( delta->tm_sec == 1 )  {
				printf( "%1d minutes %1d second",
				    delta->tm_min, delta->tm_sec );
			    }
			    else  {
				printf( "%1d minutes %d seconds",
				    delta->tm_min, delta->tm_sec );
			    }
			}
		    }
		}
	    }
	    else  {
		if( delta->tm_hour >= 10 )  {
		    printf( "%2d hours", delta->tm_hour );
		}
		else  {
		    if( delta->tm_hour == 1 )  {
			if( delta->tm_min == 1 )  {
			    printf( "%1d hour %1d minute",
				delta->tm_hour, delta->tm_min );
			}
			else  {
			    printf( "%1d hour %2d minutes",
				delta->tm_hour, delta->tm_min );
			}
		    }
		    else  {
			if( delta->tm_min == 1 )  {
			    printf( "%1d hours %1d minute",
				delta->tm_hour, delta->tm_min );
			}
			else  {
			    printf( "%1d hours %2d minutes",
				delta->tm_hour, delta->tm_min );
			}
		    }
		}
	    }
	}
	else  {
		if( delta->tm_yday >= 10 )  {
		    printf( "%2d days", delta->tm_yday );
		}
		else  {
		    if( delta->tm_yday == 1 )  {
			if( delta->tm_hour == 1 )  {
			    printf( "%1d day %1d hour",
				delta->tm_yday, delta->tm_hour );
			}
			else  {
			    printf( "%1d day %2d hours",
				delta->tm_yday, delta->tm_hour );
			}
		    }
		    else  {
			if( delta->tm_hour == 1 )  {
			    printf( "%1d days %1d hour",
				delta->tm_yday, delta->tm_hour );
			}
			else  {
			    printf( "%1d days %2d hours",
				delta->tm_yday, delta->tm_hour );
			}
		    }
		}
	}
	return( printed );
}


matchcmp( gname, login, given )

    char		*gname;
    char		*login;
    char		*given;
{
	char		buffer[ 20 ];
	char		c;
	int		flag,  i,  unfound;

	if( !match )  {
	    return( 0 );
	}
	else  {
	    if(  namecmp( login, given )  )  {
		return( 1 );
	    }
	    else  {
		if( *gname == ASTERISK )  {
		    gname++;
		}
		flag = 1;
		i = 0;
		unfound = 1;
		while(  unfound  )  {
		    if( flag )  {
			c = *gname++;
			if( c == SAMENAME )  {
			    flag = 0;
			    c = *login++;
			}
			else  {
			    unfound = (*gname != COMMA)  &&  (*gname != NULL);
			}
		    }
		    else {
			c = *login++;
			if( c == NULL )  {
			    if(  (*gname == COMMA)  ||  (*gname == NULL)  )  {
				break;
			    }
			    else  {
				flag = 1;
				continue;
			    }
			}
		    }
		    if( c == BLANK )  {
			buffer[i++] = NULL;
			if(  namecmp( buffer, given )  )  {
			    return( 1 );
			}
			i = 0;
			flag = 1;
		    }
		    else  {
			buffer[ i++ ] = c;
		    }
		}
		buffer[i++] = NULL;
		if(  namecmp( buffer, given )  )  {
		    return( 1 );
		}
		else  {
		    return( 0 );
		}
	    }
	}
}


namecmp( name1, name2 )

    char		*name1;
    char		*name2;
{
	char		c1,  c2;

	c1 = *name1;
	if( (('A' <= c1) && (c1 <= 'Z')) || (('a' <= c1) && (c1 <= 'z')) )  {
	    c1 = CAPITALIZE( c1 );
	}
	c2 = *name2;
	if( (('A' <= c2) && (c2 <= 'Z')) || (('a' <= c2) && (c2 <= 'z')) )  {
	    c2 = CAPITALIZE( c2 );
	}
	while( c1 == c2 )  {
	    if( c1 == NULL )  {
		return( 1 );
	    }
	    c1 = *++name1;
	    if( (('A'<=c1) && (c1<='Z')) || (('a'<=c1) && (c1<='z')) )  {
		c1 = CAPITALIZE( c1 );
	    }
	    c2 = *++name2;
	    if( (('A'<=c2) && (c2<='Z')) || (('a'<=c2) && (c2<='z')) )  {
		c2 = CAPITALIZE( c2 );
	    }
	}
	if( *name1 == NULL )  {
	    while(  ('0' <= *name2)  &&  (*name2 <= '9')  )  {
		name2++;
	    }
	    if( *name2 == NULL )  {
		return( 1 );
	    }
	}
	else  {
	    if( *name2 == NULL )  {
		while(  ('0' <= *name1)  &&  (*name1 <= '9')  )  {
		    name1++;
		}
		if( *name1 == NULL )  {
		    return( 1 );
		}
	    }
	}
	return( 0 );
}


char  *strsave( s )

    char		*s;
{
	char		*malloc();
	char		*p;

	p = malloc( strlen( s ) + 1 );
	strcpy( p, s );
}
