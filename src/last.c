/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * NAME: last
 *
 * SYNOPSIS: last [list]
 *
 * DESCRIPTION: Displays login history of named users or tty's.
 *    		Last with no argument prints history for all users.
 *
 * EXTERNAL ROUTINES USED: bread.c blseek.c
 *
 * AUTHOR - Howard P. Katseff
 */

# include <sys/types.h>
# include <stdio.h>
# include <sys/stat.h>
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


long	logouts	[48];

struct utmp buf;

main (argc, argv)
char **argv;
{
	char	f,
		narg,

		*bend,
		*p,
		*q;

	short	i,
		nbyte;

	int	lx,
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
	lx = sbuf.st_size;
	blseek (f, lx, 0);
	lx -= sizeof buf;
	signal (2, intrp);
	for (;;)	/* the large loop */
	{
		if (lx < sizeof buf)
		{
			q = ctime (&buf.ut_time);
			printf
			(
				"\nwtmp begins %10.10s  %5.5s \n",
				q, q + 11
			);
			fflush (stdout);
			exit ();
		}
	/*
	 * read the next login-logout record
	 */
		if (bread (f, &buf+1, -sizeof buf) < sizeof buf)
		{
			perror ("impossible error\n");
			exit ();
		}
		if (should_print ())
		{
			q = ctime (&buf.ut_time);
			printf
			(
				"%-8.8s  %-8.8s  %10.10s %5.5s ",
				buf.ut_name, buf.ut_line, q, 11+q
			);
			otime = buf.ut_time;
		/*
		 * look up the logout time for the tty
		 */
			for (i = 0;; i++)
			{
				if (!*tty_names [i])
				{ /* not in the table, therefore add it */
					move (buf.ut_line, tty_names [i]);
					ntime = 0;
					break;
				}
				if (equal (tty_names [i], buf.ut_line))
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
		lx -= sizeof buf;
		if (equal (buf.ut_line, "~"))
		{
			for (i = 0; *tty_names [i]; i++)
			{
				logouts [i] = -buf.ut_time;
			}
		}
		else
		{
			for (i = 0;; i++)
			{
				if (!*tty_names [i])
				{
					move (buf.ut_line, tty_names [i]);
					break;
				}
				if (equal (tty_names [i], buf.ut_line))
				{
					logouts [i] = buf.ut_time;
					break;
				}
			}
		}
	}
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
	q = ctime (&buf.ut_time);
	printf
	(
		"\ninterrupted %10.10s %5.5s \n",
		q, q + 11
	);
	exit ();
}

/*
 * NAMES:
 *	bread (), brseek (), blseek ()
 *
 * DESCRIPTION:
 *	This is a buffered read package which simulates
 *	read (), lseek () and lseek ().
 *
 *      Bread may be called with a negative nbytes which causes it to
 *      read backwards.  In this case, buffer should point to the first
 *      byte following the buffer.  If only a partial read is possible
 *      (due to beginning of file), only the last bytes of the buffer
 *      will be filled.
 */

char	*next;

int	nl, nr, i, j, k;

bread (file, buff, nbytes)
char *buff;
{
	register nb;

	if (nbytes > 0)
	{
		for (nb = nbytes; nb > 0; nb--)
		{
			if (!nr)
			{
				nr = read (file, next=b, 512);
				nl = 0;
				if (nr < 0) return (-1);
				if (nr == 0) return (nbytes - nb);
			}
			*buff++ = *next++;
			nl++;
			nr--;
		}
	}
	else
	{
		nbytes = -nbytes;
		for (nb = nbytes; nb > 0; nb--)
		{
			if (!nl)
			{
				lseek (file, (long) - (512 + nr), 1);
				nl = read (file, b, 512);
				if (nl < 0)
				{
					for (k = 511; k > 0; k--)
					{
						lseek (file, (long) 1, 1);
						nl = read (file, b, k);
						if (nl >= 0) break;
					}
					if (nl < 0) return (nbytes-nb);
				}
				if (nl == 0) return (nbytes-nb);
				next = b + nl;
				nr = 0;
			}
			*--buff = *--next;
			nr++;
			nl--;
		}
	}
	return (nbytes);
}

brseek (file, offset, flag)
{
	nl = nr = 0;
	return (lseek (file, (long) offset, flag));
}

blseek (file, offset, flag) 
long offset;
{
	nl = nr = 0;
	return (lseek (file, offset, flag));
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
	char i;

	if (buf.ut_name [0] == no) return no; /* a logout entry */
	if (!**Arg) return yes;
	for (i = 0; i < *Arg [i]; i++)
	{
		if 
		(
			equal (Arg [i], buf.ut_name)
			||
			equal (Arg [i], buf.ut_line)
		)
		return yes;
	}
	return no;
}
