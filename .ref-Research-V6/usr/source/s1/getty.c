#
/*
 * getty -- adapt to terminal speed on dialup, and call login
 */

/*
 * tty flags
 */
#define	HUPCL 01
#define	XTABS	02
#define	LCASE	04
#define	ECHO	010
#define	CRMOD	020
#define	RAW	040
#define	ODDP	0100
#define	EVENP	0200
#define	ANYP	0300

/*
 * Delay algorithms
 */
#define	CR1	010000
#define	CR2	020000
#define	CR3	030000
#define	NL1	000400
#define	NL2	001000
#define	NL3	001400
#define	TAB1	002000
#define	TAB2	004000
#define	TAB3	006000
#define	FF1	040000

#define	ERASE	'#'
#define	KILL	'@'

/*
 * speeds
 */
#define	B110	3
#define	B150	5
#define	B300	7
#define	B9600	13

#define	SIGINT	2
#define	SIGQIT	3

struct	sgtty {
	char	sgispd, sgospd;
	char	sgerase, sgkill;
	int	sgflag;
} tmode;

struct	tab {
	int	tname;		/* this table name */
	int	nname;		/* successor table name */
	int	iflags;		/* initial flags */
	int	fflags;		/* final flags */
	int	ispeed;		/* input speed */
	int	ospeed;		/* output speed */
	char	*message;	/* login message */
} itab[] {

/* table '0'-1-2 300,150,110 */

	'0', 1,
	ANYP+RAW+NL1+CR1, ANYP+ECHO+CR1,
	B300, B300,
	"\n\r\033;\007login: ",

	1, 2,
	ANYP+RAW+NL1+CR1, EVENP+ECHO+FF1+CR2+TAB1+NL1,
	B150, B150,
	"\n\r\033:\006\006\017login: ",

	2, '0',
	ANYP+RAW+NL1+CR1, ANYP+ECHO+CRMOD+XTABS+LCASE+CR1,
	B110, B110,
	"\n\rlogin: ",

/* table '-' -- Console TTY 110 */
	'-', '-',
	ANYP+RAW+NL1+CR1, ANYP+ECHO+CRMOD+XTABS+LCASE+CR1,
	B110, B110,
	"\n\rlogin: ",

/* table '1' -- 150 */
	'1', '1',
	ANYP+RAW+NL1+CR1, EVENP+ECHO+FF1+CR2+TAB1+NL1,
	B150, B150,
	"\n\r\033:\006\006\017login: ",

/* table '2' -- 9600 */
	'2', '2',
	ANYP+RAW+NL1+CR1, ANYP+XTABS+ECHO+CRMOD+FF1,
	B9600, B9600,
	"\n\r\033;login: ",
};

#define	NITAB	sizeof itab/sizeof itab[0]

char	name[16];
int	crmod;
int	upper;
int	lower;

main(argc, argv)
char **argv;
{
	register struct tab *tabp;
	register tname;

/*
	signal(SIGINT, 1);
	signal(SIGQIT, 0);
*/
	tname = '0';
	if (argc > 1)
		tname = *argv[1];
	for (;;) {
		for(tabp = itab; tabp < &itab[NITAB]; tabp++)
			if(tabp->tname == tname)
				break;
		if(tabp >= &itab[NITAB])
			tabp = itab;
		tmode.sgispd = tabp->ispeed;
		tmode.sgospd = tabp->ospeed;
		tmode.sgflag = tabp->iflags;
		tmode.sgispd = tabp->ispeed;
		tmode.sgospd = tabp->ospeed;
		stty(0, &tmode);
		puts(tabp->message);
		stty(0, &tmode);
		if(getname()) {
			tmode.sgerase = ERASE;
			tmode.sgkill = KILL;
			tmode.sgflag = tabp->fflags;
			if(crmod)
				tmode.sgflag =| CRMOD;
			if(upper)
				tmode.sgflag =| LCASE;
			if(lower)
				tmode.sgflag =& ~LCASE;
			stty(0, &tmode);
			execl("/bin/login", "login", name, 0);
			exit(1);
		}
		tname = tabp->nname;
	}
}

getname()
{
	register char *np;
	register c;
	static cs;

	crmod = 0;
	upper = 0;
	lower = 0;
	np = name;
	do {
		if (read(0, &cs, 1) <= 0)
			exit(0);
		if ((c = cs&0177) == 0)
			return(0);
		write(1, &cs, 1);
		if (c>='a' && c <='z')
			lower++;
		else if (c>='A' && c<='Z') {
			upper++;
			c =+ 'a'-'A';
		} else if (c==ERASE) {
			if (np > name)
				np--;
			continue;
		} else if (c==KILL) {
			np = name;
			continue;
		}
		*np++ = c;
	} while (c!='\n' && c!='\r' && np <= &name[16]);
	*--np = 0;
	if (c == '\r') {
		write(1, "\n", 1);
		crmod++;
	} else
		write(1, "\r", 1);
	return(1);
}

puts(as)
char *as;
{
	register char *s;

	s = as;
	while (*s)
		write(1, s++, 1);
}
