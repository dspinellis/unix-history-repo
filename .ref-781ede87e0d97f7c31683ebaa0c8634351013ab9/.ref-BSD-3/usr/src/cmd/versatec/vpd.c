#

/*
 * Versatec printer daemon
 */

#include <stdio.h>
#include <sys/types.h>
#include <dir.h>
#include <signal.h>
#include <stat.h>
#include <sgtty.h>
#include <errno.h>

extern	int errno;

#define	SETSTATE	(('v'<<8)|1)
#define VRAST		"/usr/local/lib/vrast"
int	prtmode[] =	{0100, 0, 0};

char	line[128];
char    linep[127];
char	banbuf[64];
char	printflag;
int	linel;
FILE	*dfb;
char	dfname [27] = "/usr/vpd/";
int	waittm = 6;
struct	dir dbuf;
int	onalrm ();
char	fonts[4][50] =
{
	"/usr/lib/vfont/R",
	"/usr/lib/vfont/I",
	"/usr/lib/vfont/B",
	"/usr/lib/vfont/S"
};

main (argc, argv)
{
	char dp, n;
	register char *p1, *p2;
	register int df;
	struct stat stb;
	int offline = 0;
	int i;

	signal (SIGHUP, SIG_IGN);
	signal (SIGINT, SIG_IGN);
	signal (SIGQUIT, SIG_IGN);
	signal (SIGTERM, SIG_IGN);
/*
 * Close all files, open root as 0, 1, 2
 * to assure standard environment
 */
	for (df=0; df<=15; df++)
		close (df);
	open ("/", 0);
	dup (0);
	dup (0);
	if (chdir ("/usr/vpd") < 0)
		exit (1);
	if (stat ("lock", &stb) >= 0)
		exit ();
	if ((df=creat ("lock", 0)) < 0)
		exit ();
	close (df);
	i = fork ();
	if (i < 0)
	{
		unlink ("lock");
		exit ();
	}
	if (i != 0)
		exit ();
	for (;;)
	{
		if (open ("/dev/vp0", 1) == 3)
			break;
		if (errno != EIO)
		{
			perror ("/dev/vp0");
			unlink ("lock");
			exit ();
		}
		if (offline == 0)
		{
			int f = open ("/dev/tty", 1);
			offline++;
			if (f > 0)
			{
				write (f, "Versatec is offline\n", 20);
				close (f);
			}
		}
		sleep (10);
	}
	dp = open (".", 0);
	/*
	 * the main loop
	 */
	for (;;)
	{
		/*
		 * find the first queueable directory entry
		 */
		lseek (dp, 0, 0);
		for (;;)
		{
			n = read (dp, &dbuf, sizeof dbuf);
			if (n <= 0)
			{
				/*
				 * if there was a job printed
				 * pull it out of the toner bath
				 */
				if (printflag)
					feedpage ();
				unlink ("lock");
				exit ();
			}
			if
			(
				dbuf.d_ino
				&&
				dbuf.d_name[0] == 'd'
				&&
				dbuf.d_name[1] == 'f'
			)
			{
				break;
			}
		}
		strcpy (&dfname [9], dbuf.d_name);
		if (trysend (dfname) == 0) /* everything was ok */
		{
			write (3, "\n\n\n", 3);
			printflag++;
		}
	}
}

trysend (file)
char *file;
{
	register int i;
	extern int badexit ();
	char plot;

	resfonts();
	dfb = fopen (file, "r");
	if (dfb == NULL)
		return (0);
	for (*banbuf = plot = 0; getline ();)
	{
		switch (line[0])
		{
			case 'L':
				strcpy (banbuf, line + 1);
				continue;

			case '1':
			case '2':
			case '3':
			case '4':
				strcpy (fonts[line[0]-'1'], line + 1);
				continue;

			case 'F':
			case 'T':
				if (send (line[0]))
				{
					ioctl (3, SETSTATE, prtmode);
					write (3, "\nJOB ABORTED***\n", 16);
				}
				continue;

			case 'P':
				if (plot)
				{
					plot = 0;
					if (send (line[0]))
					{
						ioctl (3, SETSTATE, prtmode);
						write (3, "\nJOB ABORTED***\n", 16);
					}
					else
						continue;
				}
				else
				{
					strcpy (linep, line + 1);
					plot++;
					continue;
				}

			case 'U':
				continue;

			case 'M':
				continue;
		}
	}
	/*
	 * Second pass.
	 * Unlink files and send mail.
	 */
	fseek (dfb, 0, 0);
	while (getline ())
	{
		switch (*line)
		{
			default:
				continue;

			case 'U':
				unlink (line + 1);
				continue;

			case 'M':
				sendmail ();
				continue;
		}
	}
	fclose (dfb);
	unlink (file);
	return (0);
}

static char	ifonts[4][50] =
{
	"/usr/lib/vfont/R",
	"/usr/lib/vfont/I",
	"/usr/lib/vfont/B",
	"/usr/lib/vfont/S"
};

resfonts()
{
	int i;
	for (i = 0; i < 4; i++)
		strcpy(fonts[i], ifonts[i]);
}
sendmail ()
{
	static int p[2];
	register i;
	int stat;

	pipe (p);
	if (fork ()==0)
	{
		alarm (0);
		close (0);
		dup (p[0]);
		for (i=3; i<=15; i++)
			close (i);
		execl ("/bin/mail", "mail", &line[1], 0);
		exit ();
	}
	close (1);
	dup (p[1]);
	printf ("Your versatec job is done\n");
	close (1);
	close (p[0]);
	close (p[1]);
	open ("/", 0);
	wait (&stat);
}

getline ()
{
	register char *lp;
	register int c;

	lp = line;
	linel = 0;
	while ((c = getc (dfb)) != '\n')
	{
		if (c<0)
			return (0);
		if (c=='\t')
		{
			do
			{
				*lp++ = ' ';
				linel++;
			} while ((linel & 07) != 0);
			continue;
		}
		*lp++ = c;
		linel++;
	}
	*lp++ = 0;
	return (1);
}

int	pid;

send (c)
char c;
{
	int p;

	if (pid = fork ())
	{
		if (pid == -1)
			return (1);
		setexit ();
		signal (SIGALRM, onalrm);
		alarm (30);
		wait (&p);
		alarm (0);
		return (p);
	}
	ioctl (3, SETSTATE, prtmode);
	if (c == 'F')
	{
		if (banbuf[0])
		{
			execl ("/usr/lib/vpf", "vpf", "-b", banbuf, line+1, 0);
			return (1);
		}
		execl ("/usr/lib/vpf", "vpf", line, 0);
	}
	else if (c == 'T')
	{
		int i;
		int rm;
		unlink (".railmag");
		rm = creat (".railmag", 0666);
		for (i = 0; i < 4; i++)
		{
			if (fonts[i][0] != '/')
				write (rm, "/usr/lib/vfont/", strlen ("/usr/lib/vfont/"));
			write (rm, fonts[i], strlen (fonts[i]));
			write (rm, "\n", 1);
		}
		close (rm);
		if (banbuf[0])
		{
			execl ("/usr/lib/vcat", "vcat", "-3", "-b", banbuf, line+1, 0);
			return (1);
		}
		execl ("/usr/lib/vcat", "vcat", "-3", line+1, 0);
	}
	else if (c == 'P')
	{
     		close (1);
		dup (3);
		if (banbuf[0])
		{
			execl (VRAST, "vrast", "-v", "-b", banbuf, line+1, linep, 0);
			return (1);
		}
		execl (VRAST, "vrast", "-v", line+1, linep, 0);
	}

	return (1);
}

onalrm ()
{
	struct stat stb;

	signal (SIGALRM, onalrm);
	if (stat (dfname, &stb) < 0)
		kill (pid, SIGEMT);
	reset ();
}

struct	sgttyb ttyb =
{
	B1200, B1200,
	0, 0,
	XTABS|ANYP|NL2|CR2
};

/*
 * skip 16 inches
 */
feedpage ()
{
	char i;

	ioctl (3, SETSTATE, prtmode);
	for (i = 0; i < 18; i++)
		write (3, "\n\n\n\n\n\n\n\n", 8);
}
