/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)tip.c	5.4 (Berkeley) 4/3/86";
#endif not lint

/*
 * tip - UNIX link to other systems
 *  tip [-v] system-name [-option ....]
 * or
 *  tip [-v] phone-number [-option ....]
 * or
 *  tip [-v] [-option ....]
 */
#include "tip.h"

int	disc = OTTYDISC;		/* tip normally runs this way */
int	intprompt();
int	timeout();
int	cleanup();
char	*sname();
struct phonenumberent pent;
char	PNbuf[256];			/* This limits the size of a number */
extern char *sprintf();
int	dataset;
char optionbuf[1024] ;
struct connectdomain cd;

main(argc, argv)
	char *argv[];
{
	char *system = NOSTR;
	register int i;
	register char *p;
	char sbuf[12];
	register char *optp ;

	if (argc > 4) {
		fprintf(stderr,
		"usage: tip [-v] [system or number] [-options ...]\n");
		exit(1);
	}
	if (!isatty(0)) {
		fprintf(stderr, "tip: must be interactive\n");
		exit(1);
	}

	optp = optionbuf ;
	for (; argc > 1; argv++, argc--) {
		if (argv[1][0] != '-')
			system = argv[1];
		else switch (argv[1][1]) {

		case 'v':
			vflag++;
			break;

		default:
			strcpy (optp, &argv[1][1]) ;
			optp += strlen (&argv[1][1]) + 1 ;
			break;
		}
	}

#ifdef notdef
	/*if (system == NOSTR)
		goto notnumber;
	if (isalpha(*system))
		goto notnumber;*/
	if () {
		fprintf(stderr, "tip: can't find phone number for %s\n",
			system) ;
		exit(1);
	}
	strncpy( PNbuf, system, sizeof PNbuf - 1 );
	/*
	 * System name is really a phone number...
	 * Copy the number then stomp on the original (in case the number
	 *	is private, we don't want 'ps' or 'w' to find it).
	 */
	if (strlen(system) > sizeof PNbuf - 1) {
		fprintf(stderr, "tip: phone number too long (max = %d bytes)\n",
			sizeof PNbuf - 1);
		exit(1);
	}
	strncpy( PNbuf, system, sizeof PNbuf - 1 );
	for (p = system; *p; p++)
		*p = '\0';
	PN = PNbuf;
	system = sprintf(sbuf, "tip%d", BR);

notnumber:
#endif
strcpy (cd.cd_address, system);
cd.cd_alen = strlen (cd.cd_address) +1 ;


	signal(SIGINT, cleanup);
	signal(SIGQUIT, cleanup);
	signal(SIGHUP, cleanup);
	signal(SIGTERM, cleanup);

	setbuf(stdout, NULL);

	vinit();				/* init variables */
	setparity("even");			/* set the parity table */
	if ((FD = externalconnect (&cd, optionbuf, optp - optionbuf, 2)) <= 0){
		printf("\07%s\n[EOT]\n", p);
		exit(1);
	}

	dataset = 1;
	ioctl(0, TIOCGETP, (char *)&defarg);
	ioctl(0, TIOCGETC, (char *)&defchars);
	ioctl(0, TIOCGLTC, (char *)&deflchars);
	ioctl(0, TIOCGETD, (char *)&odisc);
	arg = defarg;
	arg.sg_flags = ANYP | CBREAK;
	tchars = defchars;
	tchars.t_intrc = tchars.t_quitc = -1;
	ltchars = deflchars;
	ltchars.t_suspc = ltchars.t_dsuspc = ltchars.t_flushc
		= ltchars.t_lnextc = -1;
	raw();

	pipe(fildes); pipe(repdes);
	signal(SIGALRM, timeout);

	/*
	 * Everything's set up now:
	 *	connection established (hardwired or dialup)
	 *	line conditioned (baud rate, mode, etc.)
	 *	internal data structures (variables)
	 * so, fork one process for local side and one for remote.
	 */
	printf(cumode ? "Connected\r\n" : "\07connected\r\n");
	if (pid = fork())
		tipin();
	else
		tipout();
	/*NOTREACHED*/
}

cleanup()
{

	if (odisc)
		ioctl(0, TIOCSETD, (char *)&odisc);
	unraw() ;
	externalfinish (FD) ;
	exit(0);
}

/*
 * put the controlling keyboard into raw mode
 */
raw()
{

	ioctl(0, TIOCSETP, &arg);
	ioctl(0, TIOCSETC, &tchars);
	ioctl(0, TIOCSLTC, &ltchars);
	ioctl(0, TIOCSETD, (char *)&disc);
}


/*
 * return keyboard to normal mode
 */
unraw()
{
	if(!dataset) return;	/* premature death blasts unset tty state */
	ioctl(0, TIOCSETD, (char *)&odisc);
	ioctl(0, TIOCSETP, (char *)&defarg);
	ioctl(0, TIOCSETC, (char *)&defchars);
	ioctl(0, TIOCSLTC, (char *)&deflchars);
}

static	jmp_buf promptbuf;

/*
 * Print string ``s'', then read a string
 *  in from the terminal.  Handles signals & allows use of
 *  normal erase and kill characters.
 */
prompt(s, p)
	char *s;
	register char *p;
{
	register char *b = p;
	int (*oint)(), (*oquit)();

	stoprompt = 0;
	oint = signal(SIGINT, intprompt);
	oint = signal(SIGQUIT, SIG_IGN);
	unraw();
	printf("%s", s);
	if (setjmp(promptbuf) == 0)
		while ((*p = getchar()) != EOF && *p != '\n')
			p++;
	*p = '\0';

	raw();
	signal(SIGINT, oint);
	signal(SIGQUIT, oint);
	return (stoprompt || p == b);
}

/*
 * Interrupt service routine during prompting
 */
intprompt()
{

	signal(SIGINT, SIG_IGN);
	stoprompt = 1;
	printf("\r\n");
	longjmp(promptbuf, 1);
}

/*
 * ****TIPIN   TIPIN****
 */
tipin()
{
	char gch, bol = 1;

	/*
	 * Kinda klugey here...
	 *   check for scripting being turned on from the .tiprc file,
	 *   but be careful about just using setscript(), as we may
	 *   send a SIGEMT before tipout has a chance to set up catching
	 *   it; so wait a second, then setscript()
	 */
	if (boolean(value(SCRIPT))) {
		sleep(1);
		setscript();
	}

	while (1) {
		gch = getchar()&0177;
		if ((gch == character(value(ESCAPE))) && bol) {
			if (!(gch = escape()))
				continue;
		} else if (!cumode && gch == character(value(RAISECHAR))) {
			boolean(value(RAISE)) = !boolean(value(RAISE));
			continue;
		} else if (gch == '\r') {
			bol = 1;
			pwrite(FD, &gch, 1);
			if (boolean(value(HALFDUPLEX)))
				printf("\r\n");
			continue;
		} else if (!cumode && gch == character(value(FORCE)))
			gch = getchar()&0177;
		bol = any(gch, value(EOL));
		if (boolean(value(RAISE)) && islower(gch))
			gch = toupper(gch);
		pwrite(FD, &gch, 1);
		if (boolean(value(HALFDUPLEX)))
			printf("%c", gch);
	}
}

/*
 * Escape handler --
 *  called on recognition of ``escapec'' at the beginning of a line
 */
escape()
{
	register char gch;
	register esctable_t *p;
	char c = character(value(ESCAPE));
	extern esctable_t etable[];

	gch = (getchar()&0177);
	for (p = etable; p->e_char; p++)
		if (p->e_char == gch) {
			if ((p->e_flags&PRIV) && getuid())
				continue;
			printf("%s", ctrl(c));
			(*p->e_func)(gch);
			return (0);
		}
	/* ESCAPE ESCAPE forces ESCAPE */
	if (c != gch)
		pwrite(FD, &c, 1);
	return (gch);
}

any(c, p)
	register char c, *p;
{
	while (p && *p)
		if (*p++ == c)
			return (1);
	return (0);
}

size(s)
	register char	*s;
{
	register int i = 0;

	while (s && *s++)
		i++;
	return (i);
}

char *
interp(s)
	register char *s;
{
	static char buf[256];
	register char *p = buf, c, *q;

	while (c = *s++) {
		for (q = "\nn\rr\tt\ff\033E\bb"; *q; q++)
			if (*q++ == c) {
				*p++ = '\\'; *p++ = *q;
				goto next;
			}
		if (c < 040) {
			*p++ = '^'; *p++ = c + 'A'-1;
		} else if (c == 0177) {
			*p++ = '^'; *p++ = '?';
		} else
			*p++ = c;
	next:
		;
	}
	*p = '\0';
	return (buf);
}

char *
ctrl(c)
	char c;
{
	static char s[3];

	if (c < 040 || c == 0177) {
		s[0] = '^';
		s[1] = c == 0177 ? '?' : c+'A'-1;
		s[2] = '\0';
	} else {
		s[0] = c;
		s[1] = '\0';
	}
	return (s);
}

/*
 * Help command
 */
help(c)
	char c;
{
	register esctable_t *p;
	extern esctable_t etable[];

	printf("%c\r\n", c);
	for (p = etable; p->e_char; p++) {
		if ((p->e_flags&PRIV) && getuid())
			continue;
		printf("%2s", ctrl(character(value(ESCAPE))));
		printf("%-2s %c   %s\r\n", ctrl(p->e_char),
			p->e_flags&EXP ? '*': ' ', p->e_help);
	}
}

/*
 * Return "simple" name from a file name,
 * strip leading directories.
 */
char *
sname(s)
	register char *s;
{
	register char *p = s;

	while (*s)
		if (*s++ == '/')
			p = s;
	return (p);
}

static char partab[0200];

/*
 * Do a write to the remote machine with the correct parity.
 * We are doing 8 bit wide output, so we just generate a character
 * with the right parity and output it.
 */
pwrite(fd, buf, n)
	int fd;
	char *buf;
	register int n;
{
	register int i;
	register char *bp;
	extern int errno;

	bp = buf;
	for (i = 0; i < n; i++) {
		*bp = partab[(*bp) & 0177];
		bp++;
	}
	if (write(fd, buf, n) < 0) {
		if (errno == EIO)
			abort("Lost carrier.");
		/* this is questionable */
		perror("write");
	}
}

/*
 * Build a parity table with appropriate high-order bit.
 */
setparity(defparity)
	char *defparity;
{
	register int i;
	char *parity;
	extern char evenpartab[];

	if (value(PARITY) == NOSTR)
		value(PARITY) = defparity;
	parity = value(PARITY);
	for (i = 0; i < 0200; i++)
		partab[i] = evenpartab[i];
	if (equal(parity, "even"))
		return;
	if (equal(parity, "odd")) {
		for (i = 0; i < 0200; i++)
			partab[i] ^= 0200;	/* reverse bit 7 */
		return;
	}
	if (equal(parity, "none") || equal(parity, "zero")) {
		for (i = 0; i < 0200; i++)
			partab[i] &= ~0200;	/* turn off bit 7 */
		return;
	}
	if (equal(parity, "one")) {
		for (i = 0; i < 0200; i++)
			partab[i] |= 0200;	/* turn on bit 7 */
		return;
	}
	fprintf(stderr, "%s: unknown parity value\n", PA);
	fflush(stderr);
}
