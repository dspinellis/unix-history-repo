static char *sccsid = "@(#)lastcomm.c	4.4 (Berkeley) 82/07/17";

/*
 * last command
 */

# include <stdio.h>
# include <sys/param.h>
# include <sys/acct.h>
# include <sys/dir.h>
# include <signal.h>
# include <pwd.h>
# include <stat.h>
# include <utmp.h>
# include <struct.h>

# define N_USER		4000		/* highest alloc user # */
# define N_DEVS		43		/* hash value for device names */
# define NDEVS		500		/* max number of file names in /dev */

struct acct	acct_buff [BUFSIZ / sizeof (struct acct)];

char	user_list [N_USER][fldsiz(utmp, ut_name) + 1];

struct	devhash {
	dev_t	dev_dev;
	char	dev_name [fldsiz(utmp, ut_line) + 1];
	struct	devhash * dev_nxt;
}
	* dev_hash [ N_DEVS ],
	* dev_chain ;
# define HASH(d)	(((int) d) % N_DEVS)

time_t	expand ();
char	* flagbits();
char	* tername();

struct passwd
	*passwd,
	*getpwent ();

struct stat stat_buff;

# define equal(a, b)		(strcmp(a, b) == 0)

main (argc, argv)
char **argv;
{
	char	acct_desc,
		*p;

	long	i,
		j,
		i_block,
		n_blocks,
		n_byte,
		n_entry;

	float	x;

/*
 * set up user names
 */
	while (passwd = getpwent ())
	{
		if (user_list[passwd->pw_uid][0]==0)
			move (passwd->pw_name, user_list [passwd->pw_uid]);
	}

 /*
  * find dev numbers corresponding to names in /dev
  */
	setupdevs();

	acct_desc = open ("/usr/adm/acct", 0);
	if (acct_desc < 0)
	{
		perror ("/usr/adm/acct");
		return;
	}
	fstat (acct_desc, &stat_buff);
	n_blocks = (stat_buff.st_size + BUFSIZ - 1) / BUFSIZ;

	/*
	 * read one block's worth
	 */
	for (i_block = n_blocks - 1; i_block >= 0; i_block--)
	{
		lseek (acct_desc, i_block * BUFSIZ, 0);
		n_byte = read (acct_desc, acct_buff, BUFSIZ);
		n_entry = n_byte / sizeof acct_buff [0];
		for (i = n_entry - 1; i >= 0; i--)
		{
			if (!*user_list [acct_buff [i].ac_uid])
				continue;
			/*
			 * get the times
			 */
			x =	expand (acct_buff [i].ac_utime)
				+
				expand (acct_buff [i].ac_stime);
			/*
			 * null terminate the command name
			 */
			acct_buff [i].ac_comm [10] = 0;
			/*
			 * replace missing command names with question marks
			 */
			if (!*acct_buff [i].ac_comm)
			{
				move ("?", acct_buff [i].ac_comm);
			}
			/*
			 * replace control characters with question marks
			 */
			for (p = acct_buff [i].ac_comm; *p; p++)
			{
				if (*p < '!' || '~' < *p)
					*p = '?';
			}
			for (j = 1; j < argc; j++)
			{
				if
				(
					equal (acct_buff [i].ac_comm, argv [j])
					||
					equal
					(
						user_list[acct_buff[i].ac_uid],
						argv [j]
					)
					||
					equal
					(
						tername(acct_buff[i].ac_tty),
						argv[j]
					)
				)
				{
					break;
				}
			}
			if (argc == 1 || j != argc)
			{
				printf
				(
					"%-*s %s %-*s %-*s %6.2f     %.16s\n"
					, fldsiz(acct, ac_comm)
					, acct_buff [i].ac_comm
					, flagbits(acct_buff [i].ac_flag)
					, fldsiz(utmp, ut_name)
					, user_list [acct_buff [i].ac_uid]
					, fldsiz(utmp, ut_line)
					, tername(acct_buff [i].ac_tty)
					, x / 60.0
					, ctime (&acct_buff [i].ac_btime)
				);
			}
		}
	}
}

time_t
expand (t)
unsigned t;
{
	register time_t nt;

	nt = t & 017777;
	t >>= 13;
	while (t)
	{
		t--;
		nt <<= 3;
	}
	return (nt);
}

move (a, b)
char *a, *b;
{
	while (*b++ = *a++)
		;
}

char *
flagbits(f)
register int f;
{
	register int i = 0;
	static char flags[20];

# define BIT(flag, ch)	flags[i++] = ( f & flag ) ? ch : ' '

	BIT( ASU,	'S');
	BIT( AFORK,	'F');
	BIT( ACOMPAT,	'C');
	BIT( ACORE,	'D');
	BIT( AXSIG,	'X');

	flags[i] = '\0';

	return(flags);
}

setupdevs()
{
	register DIR * fd;
	register struct devhash * hashtab;
	register ndevs = NDEVS;
	struct direct * dp;

	if ((fd = opendir("/dev")) == NULL) {
		perror("/dev");
		return;
	}

	if ((hashtab = (struct devhash *)malloc(NDEVS * sizeof(struct devhash)))
	    == (struct devhash *) 0) {
		fprintf(stderr, "No mem for dev table\n");
		return;
	}

	while (dp = readdir(fd)) {
		if (dp->d_ino == 0)
			continue;
#ifdef	MELB
		if (dp->d_name[0] != 't' && strcmp(dp->d_name, "console"))
			continue;
#endif
		strncpy(hashtab->dev_name, dp->d_name, fldsiz(utmp, ut_line));
		hashtab->dev_name[fldsiz(utmp, ut_line)] = 0;
		hashtab->dev_nxt = dev_chain;
		dev_chain = hashtab;
		hashtab++;
		if (--ndevs <= 0)
			break;
	}
	closedir(fd);
}

char *
tername(dev)
dev_t dev;
{
	register struct devhash *hp, *nhp;
	struct stat statb;
	char name [fldsiz(devhash, dev_name) + 6];
	static dev_t lastdev = (dev_t) -1;
	static char *lastname;

	if (dev == NODEV)
		return("__");

	if (dev == lastdev)
		return(lastname);
	
	for (hp = dev_hash[HASH(dev)]; hp; hp = hp->dev_nxt)
		if (hp->dev_dev == dev) {
			lastdev = dev;
			return(lastname = hp->dev_name);
		}

	for (hp = dev_chain; hp; hp = nhp) {
		nhp = hp->dev_nxt;
		strcpy(name, "/dev/");
		strcat(name, hp->dev_name);
		if (stat(name, &statb) < 0)	/* name truncated usually */
			continue;
		if ((statb.st_mode & S_IFMT) != S_IFCHR)
			continue;
		hp->dev_dev = statb.st_rdev;
		hp->dev_nxt = dev_hash[HASH(hp->dev_dev)];
		dev_hash[HASH(hp->dev_dev)] = hp;
		if (hp->dev_dev == dev) {
			dev_chain = nhp;
			lastdev = dev;
			return(lastname = hp->dev_name);
		}
	}

	dev_chain = (struct devhash *) 0;
	return("??");
}
