/*
 *	$Source: /u1/X/xterm/RCS/resize.c,v $
 *	$Header: resize.c,v 10.101 86/12/02 10:35:30 swick Exp $
 */

#ifndef lint
static char *rcsid_resize_c = "$Header: resize.c,v 10.101 86/12/02 10:35:30 swick Exp $";
#endif	lint

#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984	*/

/* resize.c */

#include <stdio.h>
#include <sgtty.h>
#include <strings.h>
#include <ctype.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <sys/time.h>

#ifndef lint
static char sccs_id[] = "@(#)resize.c\tX10/6.6B\t12/26/86";
#endif

#define	EMULATIONS	2
#define	SUN		1
#define	TIMEOUT		10
#define	VT100		0

char *emuname[EMULATIONS] = {
	"VT100",
	"Sun",
};
char *myname;
int stdsh;
char *getsize[EMULATIONS] = {
	"\0337\033[r\033[999;999H\033[6n",
	"\033[18t",
};
#ifndef sun
#ifdef TIOCSWINSZ
char *getwsize[EMULATIONS] = {
	0,
	"\033[14t",
};
#endif TIOCSWINSZ
#endif sun
char *restore[EMULATIONS] = {
	"\0338",
	0,
};
char *setname = "";
char *setsize[EMULATIONS] = {
	0,
	"\033[8;%s;%st",
};
struct sgttyb sgorig;
char *size[EMULATIONS] = {
	"\033[%d;%dR",
	"\033[8;%d;%dt",
};
char sunname[] = "sunsize";
int tty;
FILE *ttyfp;
#ifndef sun
#ifdef TIOCSWINSZ
char *wsize[EMULATIONS] = {
	0,
	"\033[4;%hd;%hdt",
};
#endif TIOCSWINSZ
#endif sun

char *strindex (), *index (), *rindex();

main (argc, argv)
char **argv;
/*
   resets termcap string to reflect current screen size
 */
{
	register char *ptr, *env;
	register int emu = VT100;
	int rows, cols;
	struct sgttyb sg;
	char termcap [1024];
	char newtc [1024];
	char buf[BUFSIZ];
#ifdef sun
#ifdef TIOCSSIZE
	struct ttysize ts;
#endif TIOCSSIZE
#else sun
#ifdef TIOCSWINSZ
	struct winsize ws;
#endif TIOCSWINSZ
#endif sun
	char *getenv();
	int onintr();

	if(ptr = rindex(myname = argv[0], '/'))
		myname = ptr + 1;
	if(strcmp(myname, sunname) == 0)
		emu = SUN;
	for(argv++, argc-- ; argc > 0 && **argv == '-' ; argv++, argc--) {
		switch((*argv)[1]) {
		 case 's':	/* Sun emulation */
			if(emu == SUN)
				Usage();	/* Never returns */
			emu = SUN;
			break;
		 case 'u':	/* Bourne (Unix) shell */
		  	stdsh++;
			break;
		 default:
			Usage();	/* Never returns */
		}
	}
	if(argc == 2) {
		if(!setsize[emu]) {
			fprintf(stderr,
			 "%s: Can't set window size under %s emulation\n",
			 myname, emuname[emu]);
			exit(1);
		}
		if(!checkdigits(argv[0]) || !checkdigits(argv[1]))
			Usage();	/* Never returns */
	} else if(argc != 0)
		Usage();	/* Never returns */
	if((ttyfp = fopen("/dev/tty", "r+")) == NULL) {
		fprintf(stderr, "%s: Can't open /dev/tty\n", myname);
		exit(1);
	}
	tty = fileno(ttyfp);
	if((env = getenv("TERMCAP")) && *env)
		strcpy(termcap, env);
	else {
		if(!(env = getenv("TERM")) || !*env) {
			env = "xterm";
			if(stdsh)
				setname = "TERM=xterm;\n";
			else	setname = "setenv TERM xterm;\n";
		}
		if(tgetent (termcap, env) <= 0) {
			fprintf(stderr, "%s: Can't get entry \"%s\"\n",
			 myname, env);
			exit(1);
		}
	}

 	ioctl (tty, TIOCGETP, &sgorig);
	sg = sgorig;
	sg.sg_flags |= RAW;
	sg.sg_flags &= ~ECHO;
	signal(SIGINT, onintr);
	signal(SIGQUIT, onintr);
	signal(SIGTERM, onintr);
	ioctl (tty, TIOCSETP, &sg);

	if (argc == 2) {
		sprintf (buf, setsize[emu], argv[0], argv[1]);
		write(tty, buf, strlen(buf));
	}
	write(tty, getsize[emu], strlen(getsize[emu]));
	readstring(ttyfp, buf, size[emu]);
	if(sscanf (buf, size[emu], &rows, &cols) != 2) {
		fprintf(stderr, "%s: Can't get rows and columns\r\n", myname);
		onintr();
	}
	if(restore[emu])
		write(tty, restore[emu], strlen(restore[emu]));
#ifdef sun
#ifdef TIOCGSIZE
	/* finally, set the tty's window size */
	if (ioctl (tty, TIOCGSIZE, &ts) != -1) {
		ts.ts_lines = rows;
		ts.ts_cols = cols;
		ioctl (tty, TIOCSSIZE, &ts);
	}
#endif TIOCGSIZE
#else sun
#ifdef TIOCGWINSZ
	/* finally, set the tty's window size */
	if(getwsize[emu]) {
	    /* get the window size in pixels */
	    write (tty, getwsize[emu], strlen (getwsize[emu]));
	    readstring(ttyfp, buf, wsize[emu]);
	    if(sscanf (buf, wsize[emu], &ws.ws_xpixel, &ws.ws_ypixel) != 2) {
		fprintf(stderr, "%s: Can't get window size\r\n", myname);
		onintr();
	    }
	    ws.ws_row = rows;
	    ws.ws_col = cols;
	    ioctl (tty, TIOCSWINSZ, &ws);
	} else if (ioctl (tty, TIOCGWINSZ, &ws) != -1) {
	    /* we don't have any way of directly finding out
	       the current height & width of the window in pixels.  We try
	       our best by computing the font height and width from the "old"
	       struct winsize values, and multiplying by these ratios...*/
	    if (ws.ws_xpixel != 0)
	        ws.ws_xpixel = cols * (ws.ws_xpixel / ws.ws_col);
	    if (ws.ws_ypixel != 0)
	        ws.ws_ypixel = rows * (ws.ws_ypixel / ws.ws_row);
	    ws.ws_row = rows;
	    ws.ws_col = cols;
	    ioctl (tty, TIOCSWINSZ, &ws);
	}
#endif TIOCGWINSZ
#endif sun

	ioctl (tty, TIOCSETP, &sgorig);
	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
	signal(SIGTERM, SIG_DFL);

	/* update termcap string */
	/* first do columns */
	if ((ptr = strindex (termcap, "co#")) == NULL) {
		fprintf(stderr, "%s: No `co#'\n", myname);
		exit (1);
	}
	strncpy (newtc, termcap, ptr - termcap + 3);
	sprintf (newtc + strlen (newtc), "%d", cols);
	ptr = index (ptr, ':');
	strcat (newtc, ptr);

	/* now do lines */
	if ((ptr = strindex (newtc, "li#")) == NULL) {
		fprintf(stderr, "%s: No `li#'\n", myname);
		exit (1);
	}
	strncpy (termcap, newtc, ptr - newtc + 3);
	sprintf (termcap + ((int) ptr - (int) newtc + 3), "%d", rows);
	ptr = index (ptr, ':');
	strcat (termcap, ptr);

	if(stdsh)
		printf ("%sTERMCAP='%s'\n",
		 setname, termcap);
	else	printf ("set noglob;\n%ssetenv TERMCAP '%s';\nunset noglob;\n",
		 setname, termcap);
	exit(0);
}

char *strindex (s1, s2)
/*
   returns a pointer to the first occurrence of s2 in s1, or NULL if there are
   none.
 */
register char *s1, *s2;
{
	register char *s3;

	while ((s3 = index (s1, *s2)) != NULL)
	{
		if (strncmp (s3, s2, strlen (s2)) == 0) return (s3);
		s1 = ++s3;
	}
	return (NULL);
}

checkdigits(str)
register char *str;
{
	while(*str) {
		if(!isdigit(*str))
			return(0);
		str++;
	}
	return(1);
}

readstring(fp, buf, str)
register FILE *fp;
register char *buf;
char *str;
{
	register int i, last;
	struct itimerval it;
	int timeout();

	signal(SIGALRM, timeout);
	bzero((char *)&it, sizeof(struct itimerval));
	it.it_value.tv_sec = TIMEOUT;
	setitimer(ITIMER_REAL, &it, (struct itimerval *)NULL);
	if((*buf++ = getc(fp)) != *str) {
		fprintf(stderr, "%s: unknown character, exiting.\r\n", myname);
		onintr();
	}
	last = str[i = strlen(str) - 1];
	while((*buf++ = getc(fp)) != last);
	bzero((char *)&it, sizeof(struct itimerval));
	setitimer(ITIMER_REAL, &it, (struct itimerval *)NULL);
	*buf = 0;
}

Usage()
{
	fprintf(stderr, strcmp(myname, sunname) == 0 ?
	 "Usage: %s [rows cols]\n" :
	 "Usage: %s [-u] [-s [rows cols]]\n", myname);
	exit(1);
}

timeout()
{
	fprintf(stderr, "%s: Time out occurred\r\n", myname);
	onintr();
}

onintr()
{
	ioctl (tty, TIOCSETP, &sgorig);
	exit(1);
}
