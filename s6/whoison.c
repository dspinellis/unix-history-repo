#
/*
 * whoison - who is on the system
 *
 * Author: Bill Joy UCB May/July 1977
 *
 * Inspired by a shell script written by Howard Katseff
 * and the who program and the program ttyfree by Chuck Haley.
 *
 * Whoison summarizes one or more of the following
 *
 *	Users who aren't students in a class
 *	Distribution of class users by class
 *	Used, free, and unavail terminals grouped by
 *	    Evans, Cory, Phones, 11/10, and Private
 *
 * Options are:
 *
 *	p	print people
 *	u	print used
 *	f	print free
 *	n	print unavail
 *	d	print class distribution
 *	s	print summary
 *
 * Option "-" is equivalent to "-pufnd"
 * Default is "-pds".
 */

struct alias {
	char	*aname;
	char	auid, agid;
} aliases[50];

struct dist {
	char	*dname;
	struct dist2 *link;
	int	cnt;
} distrib[50];

struct dist2 {
	char *patt;
	char *next;
};
#define	USED	0
#define	FREE	1
#define	UNAVAIL	2

int	total[3];
struct	ttyw {
	char	*tname;
	int	tcnts[3];
	char	*ttys;
} ttywhere[50];

#define	NTTY	128

int	count;

int	obuf[259];

struct utmp {
	char	name[8];
	char	tty;
	char	fill;
	long	timeon;
	char	uid;
	char	gid;
} utmpbuf[NTTY];

int	status[NTTY];

#define	P	1	/* people */
#define	U	2	/* used */
#define	F	4	/* free */
#define	N	8	/* not available */
#define	D	16	/* distribution */
#define	S	32	/* summary */

int flags	P|D|S|F;

char	usagestr[]	"usage: %s [ -pufnds ]\n";
char	*progname;

char	utmp[]	"/etc/utmp";
char	*winfo	"/etc/winfo";
int	qucmp();

int	xargc;
char	**xargv;

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *argp;
	int i;
	register struct utmp *up;
	char c, d, e;
	int ibuf[259];

	argc--;
	progname = *argv++;
	if (progname[0] == 'a')
		winfo = "winfo";
	if (argc > 0) {
		flags = 0;
		while (argc > 0) {
			argp = argv[0];
			if (*argp++ != '-')
				break;
			argc--, argv++;
			if (*argp == 0)
				flags = P|U|F|N|D;
			else
				while (*argp)
					switch(*argp++) {
						case 'p':
							flags =| P;
							continue;
						case 'u':
							flags =| U;
							continue;
						case 'f':
							flags =| F;
							continue;
						case 'n':
							flags =| N;
							continue;
						case 'd':
							flags =| D;
							continue;
						case 's':
							flags =| S;
							continue;
						default:
usage:
							printf(usagestr, progname);
							exit(1);
					}
		}
	}
	xargc = argc;
	if (xargc > 0)
		flags =| P;
	xargv = argv;
	obuf[0] = 1;
	initdata();
	close(0);
	if (open("/etc/utmp", 0) < 0) {
		printf("/etc/utmp: Cannot open\n");
		fflush(obuf);
		exit(1);
	}
	read(0, utmpbuf, sizeof(utmpbuf));
	close(0);
	for (i = 0, up = &utmpbuf; up < &utmpbuf[NTTY]; i++, up++)
		up->tty = i;
	qsort(utmpbuf, NTTY, sizeof utmpbuf[0], qucmp);
	if (open("/etc/ttys", 0) < 0) {
		printf("/etc/ttys: Cannot open\n");
		fflush(obuf);
		exit(1);
	}
	while ((c = getc(ibuf)) > 0) {
		d = getc(ibuf);
		status[d] = c - '0';
		while ((c = getc(ibuf)) > 0 && c != '\n')
			continue;
	}
	close(0);
	for (up = &utmpbuf; up < &utmpbuf[NTTY]; up++)
		process(up);
	if (count && (flags & P))
		printf("\n");
	count = 0;
	if (flags & D)
		if ((flags & U) || !(flags & S))
			if (pdist("") == 0)
				printf("\n");
	count = 0;
	pwhere(USED, "used", flags & U);
	if (flags & D)
		if (!(flags & U) && (flags & S))
			if (pdist(" (") == 0)
				printf(")\r");
	if ((flags & U) || (flags & S) && (flags & D) && (flags & F))
		if (count > 2) {
			printf("\n");
			count = 0;
		}
	pwhere(FREE, "free", flags & F);
	if (flags & (U | F) && (flags & N))
		if (count) {
			printf("\n");
			count = 0;
		}
	if (flags & N)
		pwhere(UNAVAIL, "unavail", flags & N);
	if (count)
		printf("\n");
	fflush(obuf);
	exit(0);
}

/*
 * Process a utmp entry, accounting for
 * this teletype in the statistics and printing
 * an entry for this tty via puser if this tty
 * is in use. Status negative indicates
 * that this tty has already been processed out
 * of order.
 */
process(up)
	register struct utmp *up;
{

	if (status[up->tty] < 0)
		return;
	if (status[up->tty] == 0) {
		account(up->tty, UNAVAIL);
		return;
	}
	if (up->name[0] != 0) {
		account(up->tty, USED);
		puser(up);
		return;
	}
	account(up->tty, FREE);
}

char	*whereat;
/*
 * account for the argument tty to be in the
 * given state.  We use the ttywhere information
 * to locate the place for this tty's statistics.
 * It is an error for a tty to be in any state but
 * unavail if it is not in any of the tty lists
 * (this likely corresponds to a need to update
 * the data in winfo).
 */
account(tty, state)
	int tty, state;
{
	register struct ttyw *tp;

	status[tty] = -1;
	for (tp = ttywhere; tp->tname; tp++)
		if (any(tty, tp->ttys)) {
			tp->tcnts[state]++;
			total[state]++;
			whereat = tp->tname;
			return;
		}
	whereat = "";
	if (state == UNAVAIL)
		return;
	printf("tty%c is %s???, \r", tty, state == USED ? "used" : "free");
	total[state]++;
}

/*
 * print out a user if he is not a student.
 * We call rest on such users to do "and ...".
 */
puser(up)
	struct utmp *up;
{
	register char *cp;
	register int i;
	register struct dist *dp;

	cp = up->name;
	for (i = 8; i != 0; i--) {
		if (*cp <= ' ' || *cp >= 0177) {
			*cp = 0;
			break;
		}
		cp++;
	}
	if (xargc > 0) {
		for (i = 0; i < xargc; i++)
			if (clmatch(xargv[i], up) || clmatch(xargv[i], whereat) || xargv[i][1] == 0
				&& xargv[i][0] == up->tty)
				break;
		if (i == xargc)
			return;
	} else {
		for (dp = distrib; dp->dname; dp++)
			if (clmatch(dp, up)) {
				dodist(up);
				return;
			}
	}
	if (count)
		if (flags & P)
			printf(", \r");
	count++;
	if (flags & P)
		printf("%0.8s on %c", up->name, ttyof(up->tty));
	if (up->fill)
		pas(up);
	dodist(up);
	rest(up);
}

/*
 * find the rest of the instances of e.g. "root"
 * to do like "root on 5 and 8"
 */
rest(up)
	register struct utmp *up;
{
	register struct utmp *aup;

	for (aup = up + 1; aup < &utmpbuf[NTTY]; aup++)
		if (same(up->name, aup->name)) {
			dodist(aup);
			aup->name[0] = 0;
			if (flags & P)
				printf(" and %c", ttyof(aup->tty));
			if (aup->fill)
				pas(aup);
			account(aup->tty, USED);
		}
}

ttyof(t)
	char t;
{

	return (t < ' ' ? '^' | ((t + 'a' - 1) << 8) : t);
}

/*
 * print "(as name)" when root is su'd to
 * someone else.
 */
pas(up)
	register struct utmp *up;
{
	register struct alias *ap;

	for (ap = aliases; ap->aname; ap++)
		if (ap->auid == up->uid && ap->agid == up->gid) {
			if (flags & P)
				printf(" (as %s)", ap->aname);
			return;
		}
	if (flags & P)
		printf(" (as ???)");
}

/*
 * dodist distributes up into the correct
 * "class" for the class summary.
 */
dodist(up)
	struct utmp *up;
{
	register struct dist *dp;

	for (dp = distrib; dp->dname; dp++)
		if (clmatch(dp, up)) {
			dp->cnt++;
			return;
		}
}

clmatch(dp, up)
	register struct dist *dp;
	register struct utmp *up;
{
	char clname[9];
	register int i;
	register struct dist2 *lp;

	for (i = 0; i < 8; i++)
		clname[i] = up->name[i];
	clname[8] = 0;
	for (i--; i >= 0 && clname[i] == ' '; i--)
		clname[i] = 0;
	if (dp > &clname)	/* kludge to recognize members of argv */
		return(amatch(clname, dp));
	for (lp = dp->link; lp; lp = lp->next)
		if (amatch(clname, lp->patt))
			return (1);
	return (0);
}

amatch(cp, pp)
	register char *cp, *pp;
{
	while (*pp)
		switch (*pp) {
			case '?':
				if (*cp == 0)
					return (0);
				cp++;
				pp++;
				continue;
			case '*':
				pp++;
				for (;;) {
					if (amatch(cp, pp))
						return (1);
					if (*cp++ == 0)
						return (0);
				}
			default:
				if (*cp++ != *pp++)
					return (0);
		}
	return (*cp == 0);
}

/*
 * pdist prints the classwise distribution
 */
pdist(l)
	char *l;
{
	register struct dist *dp;

	for (dp = distrib; dp->dname; dp++)
		if (dp->cnt) {
			if (l) {
				printf(l);
				l = 0;
			} else
				printf(", \r");
			printf("%d %s", dp->cnt, dp->dname);
			count++;
		}
	return (l);
}

/*
 * pwhere breaks down the teletypes in state
 * (which is called sname) by location.
 * nothing is printed if there are no such,
 * otherwise we terminate with \n.
 */
pwhere(state, sname, allinfo)
	int state, allinfo;
	char *sname;
{
	register struct ttyw *tp;

	if ((flags & S) == 0 && allinfo == 0)
		return;
	if (count)
		printf(", \r");
	count++;
	printf("%d %s\r", total[state], sname);
	if (allinfo == 0)
		return;
	sname = " (";
	for (tp = ttywhere; tp->tname; tp++)
		if (tp->tcnts[state]) {
			printf("%s%d %s", sname, tp->tcnts[state], tp->tname);
			sname = ", \r";
			count++;
		}
	if (sname[0] == ',')
		printf(")\r");
}

/*
 * SUBROUTINES
 */

/*
 * pref tells whether cp is a prefix of dp
 */
pref(cp, dp)
	register char *cp, *dp;
{

	while (*cp && *cp == *dp)
		cp++, dp++;
	return (*cp == 0);
}

same(cp, dp)
	char *cp, *dp;
{

	return (ucmp(cp, dp) == 0);
}

qucmp(cp, dp)
	struct utmp *cp, *dp;
{
	register int i;

	i = ucmp(cp, dp);
	if (i == 0)
		i = cp->tty - dp->tty;
	return (i);
}

ucmp(cp, dp)
	register char *cp, *dp;
{
	register int i;

	i = 8;
	while (*cp++ == *dp++)
		if (--i == 0)
			return (0);
	--cp, --dp;
	if ((*cp == ' ' || *cp == 0) && (*dp == ' ' || *dp == 0))
		return (0);
	return (*cp - *dp);
}
ucmpA(cp0, dp0)
	struct utmp *cp0, *dp0;
{
	register char *cp, *dp;
	register int i;

	cp = &cp0->name[0];
	dp = &dp0->name[0];
	i = 8;
	while (*cp++ == *dp++)
		if (--i == 0)
			return (cp0->tty - dp0->tty);
	--cp, --dp;
	if ((*cp == ' ' || *cp == 0) && (*dp == ' ' || *dp == 0))
		return (cp0->tty - dp0->tty);
	return (*cp - *dp);
}

/*
 * is c any of the characters in s ?
 */
any(c, s)
	char c;
	register char *s;
{

	while (*s)
		if (c == *s++)
			return (1);
	return (0);
}

/*
 * The following definitions of putchar and item and the sav buffer
 * implement breakover formatting of the output.
 * Each carriage return character in the putchar stream '\r'
 * indicates that an item has come to an end and that a reasonable
 * place for a breakover is occuring.
 *
 * If this item will fit on the current line it is placed there
 * after output of the saved trailing blanks of the previous item.
 * If it will not fit there then the trailing blanks of the previous
 * line and the leading blanks of this line are discarded and the
 * item is placed on the next line.
 *
 * In any case a newline character causes the item buffer to be emptied.
 */
char	sav[100];
char	*savp sav;

putchar(c)
	char c;
{

	if (c != '\r')
		*savp++ = c;
	if (c == '\r' || c == '\n')
		item();
}

item()
{
	static int outcol, oweblanks;
	register char *cp;

	*savp = 0;
	cp = sav;
	if (outcol != 0 && outcol + oweblanks + (savp - sav) >= 79) {
		outcol = 0;
		putc('\n', obuf);
	}
	if (outcol == 0) {
		oweblanks = 0;
		while (*cp == ' ')
			cp++;
	} else
		for (; oweblanks > 0; oweblanks--)
			putc(' ', obuf);
	while (savp >= cp && *savp == ' ')
		oweblanks++, savp--;
	while (*cp) {
		putc(*cp, obuf);
		if (*cp++ == '\n')
			outcol = 0;
		else
			outcol++;
	}
	savp = sav;
	fflush(obuf);
}

char	lastc;
int	ibuf[259];
int	inline 1;

initdata()
{

	close(0);
	if (fopen(winfo, ibuf) < 0) {
		perror(winfo);
		fflush(obuf);
		exit (1);
	}
	inittty();
	initdist();
	initalias();
	eof();
}

inittty()
{
	register struct ttyw *tp;

	for (tp = &ttywhere[0]; tp->tname = field(); tp++) {
		tp->ttys = tail();
		eol();
	}
	eol();
}

initdist()
{
	register struct dist *dp;
	register struct dist2 *lp;
	register char *cp;

	for (dp = &distrib[0]; dp->dname = field(); dp++) {
		while (lastc != '\n' && lastc != -1) {
			cp = tail();
			lp = dp->link;
			dp->link = alloc(sizeof *lp);
			dp->link->patt = cp;
			dp->link->next = lp;
		}
		eol();
	}
	eol();
}

initalias()
{
	register struct alias *ap;

	for (ap = &aliases[0]; ap->aname = field(); ap++) {
		ap->auid = number(1);
		ap->agid = number(0);
		eol();
	}
}

inpanic(cp)
	char *cp;
{

	printf("%s: %s, line %d\n", winfo, cp, inline);
	fflush(obuf);
	exit(1);
}

eol()
{

	if (lastc != '\n')
		inpanic("Too many fields in line");
	inline++;
}

eof()
{
	if (getc(ibuf) != -1)
		inpanic("Too much data in file");
}

field()
{

	return (field1(1));
}

tail()
{

	return (field1(0));
}

number(f)
	int f;
{
	register char *cp;
	register int i;

	cp = field1(f);
	if (cp == 0)
		inpanic("Number expected");
	i = 0;
	while (*cp >= '0' && *cp <= '9')
		i =* 10, i =+ *cp++ - '0';
	if (*cp)
		inpanic("Badly formed number");
	return (i);
}

field1(f)
	int f;
{
	char buf[100];
	register char *bp, c;

	bp = buf;
	for (;;) {
		c = getc(ibuf);
		switch (c) {
			case -1:
			case '\n':
				lastc = c;
				if (f) {
					if (bp == buf)
						return (0);
					inpanic("Missing ':'");
				}
			case '|':
			case ':':
				lastc = c;
				if (f && bp == buf)
					inpanic("Empty field");
				*bp++ = 0;
				return (savestr(buf));
			default:
				if (bp >= &buf[99])
					inpanic("Field too long");
				*bp++ = c;
		}
	}
}

savestr(cp)
	char *cp;
{

	return (strcpy(alloc(strlen(cp) + 1), cp));
}
strcpy(oto, from)
	char *oto;
	register char *from;
{
	register char *to;

	to = oto;
	while (*to++ = *from++)
		continue;
	return (oto);
}
