/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)lastcomm.c	5.8 (Berkeley) %G%";
#endif /* not lint */

/*
 * last command
 */
#include <sys/param.h>
#include <sys/acct.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <utmp.h>
#include <struct.h>
#include <ctype.h>
#include <stdio.h>
#include "pathnames.h"

struct	acct buf[DEV_BSIZE / sizeof (struct acct)];

time_t	expand();
char	*flagbits();
char	*getname();
char	*getdev();

main(argc, argv)
	int argc;
	char *argv[];
{
	extern int optind;
	extern char *optarg;
	register struct acct *acp;
	register int bn, cc;
	struct stat sb;
	int ch, fd;
	char *acctfile, *strcpy(), *ctime();
	long lseek();

	acctfile = _PATH_ACCT;
	while ((ch = getopt(argc, argv, "f:")) != EOF)
		switch((char)ch) {
		case 'f':
			acctfile = optarg;
			break;
		case '?':
		default:
			fputs("lastcomm [ -f file ]\n", stderr);
			exit(1);
		}
	argv += optind;

	fd = open(acctfile, O_RDONLY);
	if (fd < 0) {
		perror(acctfile);
		exit(1);
	}
	(void)fstat(fd, &sb);
	setpassent(1);
	for (bn = btodb(sb.st_size); bn >= 0; bn--) {
		(void)lseek(fd, (off_t)dbtob(bn), L_SET);
		cc = read(fd, buf, DEV_BSIZE);
		if (cc < 0) {
			perror("read");
			break;
		}
		acp = buf + (cc / sizeof (buf[0])) - 1;
		for (; acp >= buf; acp--) {
			register char *cp;
			time_t x;

			if (acp->ac_comm[0] == '\0')
				(void)strcpy(acp->ac_comm, "?");
			for (cp = &acp->ac_comm[0];
			     cp < &acp->ac_comm[fldsiz(acct, ac_comm)] && *cp;
			     cp++)
				if (!isascii(*cp) || iscntrl(*cp))
					*cp = '?';
			if (*argv && !ok(argv, acp))
				continue;
			x = expand(acp->ac_utime) + expand(acp->ac_stime);
			printf("%-*.*s %s %-*s %-*s %6.2f secs %.16s\n",
				fldsiz(acct, ac_comm),
				fldsiz(acct, ac_comm),
				acp->ac_comm, flagbits(acp->ac_flag),
				fldsiz(utmp, ut_name),
				(cp = getname(acp->ac_uid)) ? cp : "",
				fldsiz(utmp, ut_line), getdev(acp->ac_tty),
				x / (double)AHZ, ctime(&acp->ac_btime));
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
	static char flags[20];
	char *p, *strcpy();

#define	BIT(flag, ch)	if (f & flag) *p++ = ch;
	p = strcpy(flags, "-    ");
	BIT(ASU, 'S');
	BIT(AFORK, 'F');
	BIT(ACOMPAT, 'C');
	BIT(ACORE, 'D');
	BIT(AXSIG, 'X');
	return (flags);
}

ok(argv, acp)
	register char *argv[];
	register struct acct *acp;
{
	register char *cp;

	do {
		if ((cp = getname(acp->ac_uid)) && !strcmp(cp, *argv) ||
		    (cp = getdev(acp->ac_tty)) && !strcmp(cp, *argv) ||
		    !strncmp(acp->ac_comm, *argv, fldsiz(acct, ac_comm)))
			return(1);
	} while (*++argv);
	return(0);
}

/* should be done with nameserver or database */

#include <pwd.h>

struct	utmp utmp;
#define	NMAX	(sizeof (utmp.ut_name))
#define SCPYN(a, b)	strncpy(a, b, NMAX)

#define NCACHE	64		/* power of 2 */
#define CAMASK	NCACHE - 1

char *
getname(uid)
	uid_t uid;
{
	static struct ncache {
		uid_t	uid;
		char	name[NMAX+1];
	} c_uid[NCACHE];
	register struct passwd *pw;
	register struct ncache *cp;

	cp = c_uid + (uid & CAMASK);
	if (cp->uid == uid && *cp->name)
		return(cp->name);
	if (!(pw = getpwuid(uid)))
		return((char *)0);
	cp->uid = uid;
	SCPYN(cp->name, pw->pw_name);
	return(cp->name);
}

#include <sys/dir.h>

#define N_DEVS		43		/* hash value for device names */
#define NDEVS		500		/* max number of file names in /dev */

struct	devhash {
	dev_t	dev_dev;
	char	dev_name [fldsiz(utmp, ut_line) + 1];
	struct	devhash * dev_nxt;
};
struct	devhash *dev_hash[N_DEVS];
struct	devhash *dev_chain;
#define HASH(d)	(((int) d) % N_DEVS)

setupdevs()
{
	register DIR * fd;
	register struct devhash * hashtab;
	register ndevs = NDEVS;
	struct direct * dp;
	char *malloc();

	/*NOSTRICT*/
	hashtab = (struct devhash *)malloc(NDEVS * sizeof(struct devhash));
	if (hashtab == (struct devhash *)0) {
		fputs("No mem for dev table\n", stderr);
		return;
	}
	if ((fd = opendir(_PATH_DEV)) == NULL) {
		perror(_PATH_DEV);
		return;
	}
	while (dp = readdir(fd)) {
		if (dp->d_ino == 0)
			continue;
		if (dp->d_name[0] != 't' && strcmp(dp->d_name, "console"))
			continue;
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
getdev(dev)
	dev_t dev;
{
	register struct devhash *hp, *nhp;
	struct stat statb;
	char name[fldsiz(devhash, dev_name) + 6];
	static dev_t lastdev = (dev_t) -1;
	static char *lastname;
	static int init = 0;
	char *strcpy(), *strcat();

	if (dev == NODEV)
		return ("__");
	if (dev == lastdev)
		return (lastname);
	if (!init) {
		setupdevs();
		init++;
	}
	for (hp = dev_hash[HASH(dev)]; hp; hp = hp->dev_nxt)
		if (hp->dev_dev == dev) {
			lastdev = dev;
			return (lastname = hp->dev_name);
		}
	for (hp = dev_chain; hp; hp = nhp) {
		nhp = hp->dev_nxt;
		strcpy(name, _PATH_DEV);
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
