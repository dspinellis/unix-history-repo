#ifndef lint
static char *sccsid = "@(#)lastcomm.c	4.5 (Berkeley) 83/04/04";
#endif

/*
 * last command
 */
#include <stdio.h>
#include <sys/param.h>
#include <sys/acct.h>
#include <sys/dir.h>
#include <signal.h>
#include <pwd.h>
#include <stat.h>
#include <utmp.h>
#include <struct.h>
#include <ctype.h>

#define N_USER		4000		/* highest alloc user # */
#define N_DEVS		43		/* hash value for device names */
#define NDEVS		500		/* max number of file names in /dev */

struct	acct acct_buff[BUFSIZ / sizeof (struct acct)];
char	user_list[N_USER][fldsiz(utmp, ut_name) + 1];

struct	devhash {
	dev_t	dev_dev;
	char	dev_name [fldsiz(utmp, ut_line) + 1];
	struct	devhash * dev_nxt;
};
struct	devhash *dev_hash[N_DEVS];
struct	devhash *dev_chain ;
#define HASH(d)	(((int) d) % N_DEVS)

time_t	expand ();
char	*flagbits();
char	*ttyname();

struct	passwd *passwd, *getpwent ();
struct stat stat_buff;

main(argc, argv)
	char *argv[];
{
	char acct_desc, *p;
	long i, j, i_block, n_blocks, n_byte, n_entry, x;
	register struct acct *acp;

	/*
	 * Set up user names
	 */
	while (passwd = getpwent())
		if (user_list[passwd->pw_uid][0] == 0)
			strcpy(user_list[passwd->pw_uid], passwd->pw_name);
	/*
	 * Find dev numbers corresponding to names in /dev
	 */
	setupdevs();
	acct_desc = open("/usr/adm/acct", 0);
	if (acct_desc < 0) {
		perror("/usr/adm/acct");
		exit(1);
	}
	fstat(acct_desc, &stat_buff);
	n_blocks = (stat_buff.st_size + BUFSIZ - 1) / BUFSIZ;

	/*
	 * Read one block's worth
	 */
	for (i_block = n_blocks - 1; i_block >= 0; i_block--) {
		lseek(acct_desc, i_block * BUFSIZ, 0);
		n_byte = read(acct_desc, acct_buff, BUFSIZ);
		n_entry = n_byte / sizeof acct_buff[0];
		for (acp = acct_buff + n_entry - 1; acp >= acct_buff; acp--) {
			if (*user_list[acp->ac_uid] == '\0')
				continue;
			x = expand(acp->ac_utime) + expand(acp->ac_stime);
			acp->ac_comm[10] = '\0';
			if (*acp->ac_comm == '\0')
				strcpy(acp->ac_comm, "?");
			for (p = acp->ac_comm; *p; p++)
				if (iscntrl(*p))
					*p = '?';
			if (!ok(argc, argv, acp) && argc != 1)
				continue;
			printf("%-*s %s %-*s %-*s %4d sec%s %.16s\n",
				fldsiz(acct, ac_comm), acp->ac_comm,
				flagbits(acp->ac_flag),
				fldsiz(utmp, ut_name), user_list[acp->ac_uid],
				fldsiz(utmp, ut_line), ttyname(acp->ac_tty),
				x, x > 1 || x == 0 ? "s" : " ",
				ctime(&acp->ac_btime));
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
	while (t) {
		t--;
		nt <<= 3;
	}
	return (nt);
}

char *
flagbits(f)
	register int f;
{
	register int i = 0;
	static char flags[20];

#define BIT(flag, ch)	flags[i++] = (f & flag) ? ch : ' '
	BIT(ASU, 'S');
	BIT(AFORK, 'F');
	BIT(ACOMPAT, 'C');
	BIT(ACORE, 'D');
	BIT(AXSIG, 'X');
	flags[i] = '\0';
	return (flags);
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
ttyname(dev)
	dev_t dev;
{
	register struct devhash *hp, *nhp;
	struct stat statb;
	char name[fldsiz(devhash, dev_name) + 6];
	static dev_t lastdev = (dev_t) -1;
	static char *lastname;

	if (dev == NODEV)
		return ("__");
	if (dev == lastdev)
		return (lastname);
	for (hp = dev_hash[HASH(dev)]; hp; hp = hp->dev_nxt)
		if (hp->dev_dev == dev) {
			lastdev = dev;
			return (lastname = hp->dev_name);
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
			return (lastname = hp->dev_name);
		}
	}
	dev_chain = (struct devhash *) 0;
	return ("??");
}

ok(argc, argv, acp)
	register int argc;
	register char *argv[];
	register struct acct *acp;
{
	register int j;

	for (j = 1; j < argc; j++)
		if (strcmp(user_list[acp->ac_uid], argv[j]) &&
		    strcmp(ttyname(acp->ac_tty), argv[j]) &&
		    strcmp(acp->ac_comm, argv[j]))
			break;
	return (j == argc);
}
