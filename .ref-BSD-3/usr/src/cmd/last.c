#
/*
 * NAME: last
 *
 * SYNOPSIS: last [list]
 *
 * DESCRIPTION: Displays login history of named users or tty's.
 *    		Last with no argument prints history for all users.
 *
 * AUTHOR - Howard P. Katseff
 */

# include <sys/types.h>
# include <stdio.h>
# include <stat.h>
# include <utmp.h>

char	yes =	1,
	no  =	0,

	*wtmp = "/usr/adm/wtmp",
	b [512],

	Arg	  [25] [9],
	tty_names [48] [9],

	*ctime  (),
	*move   (),
	*rmchar ();


long	logouts	[48],
	bl,
	rec,
	nblock;

struct utmp buf [128]; /* buf takes exactly 5 blocks */

main (argc, argv)
char **argv;
{
	char	f,
		narg,

		*bend,
		*p,
		*q;

	short	n_byte,
		n_record;

	long	i,
		k,
		ntime,
		otime,

		intrp ();

	struct	stat sbuf;
 
	for (i = 1; i < argc; i++)
	{
		if
		(
			length (argv [i]) > 2 /* long tty or user name */
			||
			equal (argv [i], "~") /* tilde */
			||
			getpwnam (argv [i]) /* user name */
		)
		{
			move (argv [i], Arg [narg++]);
		}
		else /* short tty name */
		{
			move (argv [i], move ("tty", Arg [narg++]));
		}
	}
	f = open (wtmp, 0);
	if (f < 0)
	{
		perror (wtmp);
		fflush (stdout);
		exit ();
	}
	if (fstat (f, &sbuf) < 0)
	{
		perror ("/usr/adm/wtmp");
		fflush (stdout);
		exit ();
	}
	nblock = (sbuf.st_size + 2559) / 2560;
	signal (2, intrp);
	for (bl = nblock - 1; bl >= 0; bl--)
	{
		lseek (f, bl * 2560, 0);
		n_byte = read (f, buf, 2560);
		n_record = n_byte / sizeof buf [0];
		for (rec = n_record - 1; rec >= 0; rec--)
		{
			
			if (should_print ())
			{
				q = ctime (&buf[rec].ut_time);
				printf
				(
					"%-8.8s  %-8.8s  %10.10s %5.5s ",
					buf[rec].ut_name, buf[rec].ut_line, q, 11+q
				);
				otime = buf[rec].ut_time;
			/*
			 * look up the logout time for the tty
			 */
				for (i = 0;; i++)
				{
					if (!*tty_names [i])
					/* not in the table, therefore add it */
					{
						move
						(
							buf[rec].ut_line,
							tty_names [i]
						);
						ntime = 0;
						break;
					}
					if
					(
						equal
						(
							tty_names [i],
							buf [rec].ut_line
						)
					)
					{
						ntime = logouts [i];
						break;
					}
				}
				if (ntime == 0)
				{
					printf ("  still logged in\n");
				}
				else
				{
					if (ntime < 0)
					{
						ntime = -ntime;
						printf ("- crash");
					}
					else 
					{
						printf ("- %5.5s", ctime (&ntime) + 11);
					}
				/*
			 	 * calculate how long logged in
			 	 */
					otime = ntime - otime;
					otime += 231220830 + 10800;
					if (otime < 231220830 + 86400 + 10800)
					{
						printf
						(
							"  (%5.5s)\n",
						 	ctime (&otime) + 11
						);
					}
					else
					{
						printf
						(
							" (%ld+%5.5s)\n",
							(otime -
							(231330830-86400-10800))/86400,
							ctime (&otime) + 11
						);
					}
				}
				fflush (stdout);
			}
			if
			(
				equal (buf[rec].ut_line, "~")
				||
				equal (buf[rec].ut_line, "tty~")
			)
			{
				for (i = 0; *tty_names [i]; i++)
				{
					logouts [i] = -buf[rec].ut_time;
				}
			}
			else
			{
				for (k = 0;; k++)
				{
					if (!*tty_names [k])
					{
						move
						(
							buf[rec].ut_line,
							tty_names [k]
						);
						logouts [k] = buf[rec].ut_time;
						break;
					}
					if (equal (tty_names [k], buf[rec].ut_line))
					{
						logouts [k] = buf[rec].ut_time;
						break;
					}
				}
			}
		}
	}
	q = ctime (&buf [0].ut_time);
	printf
	(
		"\nwtmp begins %10.10s %5.5s \n",
		q, q + 11
	);
}

equal (a, b)
char *a, *b;
{
	char i;

	for (i = 0; i < 8; i++)
	{
		if (!*a) return (!*b);
		if (*a++ != *b++) return (0);
	}
	return (1);
}


intrp ()
{
	char *q;

	signal (2, 1); /* ignore further interrupts */
	q = ctime (&buf[rec].ut_time);
	printf
	(
		"\ninterrupted %10.10s %5.5s \n",
		q, q + 11
	);
	exit ();
}

char *
rmchar (c, s)
char c, *s;
{
	for (; *s; s++)
	{
		if (*s == c)
		{
			*s = 0;
			return (s);
		}
	}
	return (0);
}

length (a)
char *a;
{
	char *b;

	for (b = a; *b; b++);
	return (b - a);
}

char *
move (a, b)
char *a, *b;
{
	while (*b++ = *a++);
	return (b - 1);
}

should_print ()
{
	short i;

	if (buf [rec].ut_name [0] == no) return no; /* a logout entry */
	if (!**Arg) return yes; /* no arguments? Print all login entries */
	for (i = 0; i < *Arg [i]; i++)
	{
		if 
		(
			equal (Arg [i], buf[rec].ut_name)
			||
			equal (Arg [i], buf[rec].ut_line)
		)
		return yes;
	}
	return no;
}
