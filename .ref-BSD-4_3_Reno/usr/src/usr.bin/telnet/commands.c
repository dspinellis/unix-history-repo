/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)commands.c	1.32 (Berkeley) 7/28/90";
#endif /* not lint */

#include <sys/types.h>
#if	defined(unix)
#include <sys/file.h>
#endif	/* defined(unix) */
#include <sys/socket.h>
#include <netinet/in.h>
#ifdef	CRAY
#include <sys/fcntl.h>
#endif	/* CRAY */

#include <signal.h>
#include <netdb.h>
#include <ctype.h>
#include <pwd.h>
#include <varargs.h>

#include <arpa/telnet.h>

#include "general.h"

#include "ring.h"

#include "externs.h"
#include "defines.h"
#include "types.h"

#ifdef	SRCRT
# ifndef CRAY
# include <netinet/in_systm.h>
#  if (defined(vax) || defined(tahoe) || defined(hp300)) && !defined(ultrix)
#  include <machine/endian.h>
#  endif /* vax */
# endif /* CRAY */
#include <netinet/ip.h>
#endif /* SRCRT */

#if defined(CRAY) && defined(IP_TOS) && !defined(HAS_IP_TOS)
# define HAS_IP_TOS
#endif


char	*hostname;
extern char *getenv();

#define Ambiguous(s)	((char **)s == &ambiguous)
static char *ambiguous;		/* special return value for command routines */
static call();

typedef struct {
	char	*name;		/* command name */
	char	*help;		/* help string (NULL for no help) */
	int	(*handler)();	/* routine which executes command */
	int	needconnect;	/* Do we need to be connected to execute? */
} Command;

static char line[256];
static char saveline[256];
static int margc;
static char *margv[20];

/*
 * Various utility routines.
 */

#if	!defined(BSD) || (BSD <= 43)

char	*h_errlist[] = {
	"Error 0",
	"Unknown host",				/* 1 HOST_NOT_FOUND */
	"Host name lookup failure",		/* 2 TRY_AGAIN */
	"Unknown server error",			/* 3 NO_RECOVERY */
	"No address associated with name",	/* 4 NO_ADDRESS */
};
int	h_nerr = { sizeof(h_errlist)/sizeof(h_errlist[0]) };

int h_errno;		/* In some version of SunOS this is necessary */

/*
 * herror --
 *	print the error indicated by the h_errno value.
 */
herror(s)
	char *s;
{
	if (s && *s) {
		fprintf(stderr, "%s: ", s);
	}
	if ((h_errno < 0) || (h_errno >= h_nerr)) {
		fprintf(stderr, "Unknown error\n");
	} else if (h_errno == 0) {
#if	defined(sun)
		fprintf(stderr, "Host unknown\n");
#endif	/* defined(sun) */
	} else {
		fprintf(stderr, "%s\n", h_errlist[h_errno]);
	}
}
#endif	/* !define(BSD) || (BSD <= 43) */

static void
makeargv()
{
    register char *cp, *cp2, c;
    register char **argp = margv;

    margc = 0;
    cp = line;
    if (*cp == '!') {		/* Special case shell escape */
	strcpy(saveline, line);	/* save for shell command */
	*argp++ = "!";		/* No room in string to get this */
	margc++;
	cp++;
    }
    while (c = *cp) {
	register int inquote = 0;
	while (isspace(c))
	    c = *++cp;
	if (c == '\0')
	    break;
	*argp++ = cp;
	margc += 1;
	for (cp2 = cp; c != '\0'; c = *++cp) {
	    if (inquote) {
		if (c == inquote) {
		    inquote = 0;
		    continue;
		}
	    } else {
		if (c == '\\') {
		    if ((c = *++cp) == '\0')
			break;
		} else if (c == '"') {
		    inquote = '"';
		    continue;
		} else if (c == '\'') {
		    inquote = '\'';
		    continue;
		} else if (isspace(c))
		    break;
	    }
	    *cp2++ = c;
	}
	*cp2 = '\0';
	if (c == '\0')
	    break;
	cp++;
    }
    *argp++ = 0;
}


static char **
genget(name, table, next)
char	*name;		/* name to match */
char	**table;		/* name entry in table */
char	**(*next)();	/* routine to return next entry in table */
{
	register char *p, *q;
	register char **c, **found;
	register int nmatches, longest;

	if (name == 0) {
	    return 0;
	}
	longest = 0;
	nmatches = 0;
	found = 0;
	for (c = table; (p = *c) != 0; c = (*next)(c)) {
		for (q = name;
		    (*q == *p) || (isupper(*q) && tolower(*q) == *p); p++, q++)
			if (*q == 0)		/* exact match? */
				return (c);
		if (!*q) {			/* the name was a prefix */
			if (q - name > longest) {
				longest = q - name;
				nmatches = 1;
				found = c;
			} else if (q - name == longest)
				nmatches++;
		}
	}
	if (nmatches > 1)
		return &ambiguous;
	return (found);
}

/*
 * Make a character string into a number.
 *
 * Todo:  1.  Could take random integers (12, 0x12, 012, 0b1).
 */

static
special(s)
register char *s;
{
	register char c;
	char b;

	switch (*s) {
	case '^':
		b = *++s;
		if (b == '?') {
		    c = b | 0x40;		/* DEL */
		} else {
		    c = b & 0x1f;
		}
		break;
	default:
		c = *s;
		break;
	}
	return c;
}

/*
 * Construct a control character sequence
 * for a special character.
 */
static char *
control(c)
	register cc_t c;
{
	static char buf[5];

	if (c == 0x7f)
		return ("^?");
	if (c == (cc_t)-1) {
		return "off";
	}
	if (c >= 0x80) {
		buf[0] = '\\';
		buf[1] = ((c>>6)&07) + '0';
		buf[2] = ((c>>3)&07) + '0';
		buf[3] = (c&07) + '0';
		buf[4] = 0;
	} else if (c >= 0x20) {
		buf[0] = c;
		buf[1] = 0;
	} else {
		buf[0] = '^';
		buf[1] = '@'+c;
		buf[2] = 0;
	}
	return (buf);
}



/*
 *	The following are data structures and routines for
 *	the "send" command.
 *
 */
 
struct sendlist {
    char	*name;		/* How user refers to it (case independent) */
    char	*help;		/* Help information (0 ==> no help) */
#if	defined(NOT43)
    int		(*handler)();	/* Routine to perform (for special ops) */
#else	/* defined(NOT43) */
    void	(*handler)();	/* Routine to perform (for special ops) */
#endif	/* defined(NOT43) */
    int		what;		/* Character to be sent (<0 ==> special) */
};

#define	SENDQUESTION	-1
#define	SENDESCAPE	-3

static struct sendlist Sendlist[] = {
    { "ao",	"Send Telnet Abort output",		0,	AO },
    { "ayt",	"Send Telnet 'Are You There'",		0,	AYT },
    { "brk",	"Send Telnet Break",			0,	BREAK },
    { "ec",	"Send Telnet Erase Character",		0,	EC },
    { "el",	"Send Telnet Erase Line",		0,	EL },
    { "escape",	"Send current escape character",	0,	SENDESCAPE },
    { "ga",	"Send Telnet 'Go Ahead' sequence",	0,	GA },
    { "ip",	"Send Telnet Interrupt Process",	0,	IP },
    { "nop",	"Send Telnet 'No operation'",		0,	NOP },
    { "eor",	"Send Telnet 'End of Record'",		0,	EOR },
    { "abort",	"Send Telnet 'Abort Process'",		0,	ABORT },
    { "susp",	"Send Telnet 'Suspend Process'",	0,	SUSP },
    { "eof",	"Send Telnet End of File Character",	0,	xEOF },
    { "synch",	"Perform Telnet 'Synch operation'",	dosynch, SYNCH },
    { "getstatus", "Send request for STATUS",		get_status, 0 },
    { "?",	"Display send options",			0,	SENDQUESTION },
    { 0 }
};

static struct sendlist Sendlist2[] = {		/* some synonyms */
    { "break",		0, 0, BREAK },

    { "intp",		0, 0, IP },
    { "interrupt",	0, 0, IP },
    { "intr",		0, 0, IP },

    { "help",		0, 0, SENDQUESTION },

    { 0 }
};

static char **
getnextsend(name)
char *name;
{
    struct sendlist *c = (struct sendlist *) name;

    return (char **) (c+1);
}

static struct sendlist *
getsend(name)
char *name;
{
    struct sendlist *sl;

    if ((sl = (struct sendlist *)
			genget(name, (char **) Sendlist, getnextsend)) != 0) {
	return sl;
    } else {
	return (struct sendlist *)
				genget(name, (char **) Sendlist2, getnextsend);
    }
}

static
sendcmd(argc, argv)
int	argc;
char	**argv;
{
    int what;		/* what we are sending this time */
    int count;		/* how many bytes we are going to need to send */
    int i;
    int question = 0;	/* was at least one argument a question */
    struct sendlist *s;	/* pointer to current command */

    if (argc < 2) {
	printf("need at least one argument for 'send' command\n");
	printf("'send ?' for help\n");
	return 0;
    }
    /*
     * First, validate all the send arguments.
     * In addition, we see how much space we are going to need, and
     * whether or not we will be doing a "SYNCH" operation (which
     * flushes the network queue).
     */
    count = 0;
    for (i = 1; i < argc; i++) {
	s = getsend(argv[i]);
	if (s == 0) {
	    printf("Unknown send argument '%s'\n'send ?' for help.\n",
			argv[i]);
	    return 0;
	} else if (Ambiguous(s)) {
	    printf("Ambiguous send argument '%s'\n'send ?' for help.\n",
			argv[i]);
	    return 0;
	}
	switch (s->what) {
	case SENDQUESTION:
	    question = 1;
	    break;
	case SENDESCAPE:
	    count += 1;
	    break;
	case SYNCH:
	    count += 2;
	    break;
	default:
	    count += 2;
	    break;
	}
    }
    if (!connected) {
	if (count)
	    printf("?Need to be connected first.\n");
	if (question) {
	    for (s = Sendlist; s->name; s++)
		if (s->help)
		    printf("%-15s %s\n", s->name, s->help);
	} else
	    printf("'send ?' for help\n");
	return !question;
    }
    /* Now, do we have enough room? */
    if (NETROOM() < count) {
	printf("There is not enough room in the buffer TO the network\n");
	printf("to process your request.  Nothing will be done.\n");
	printf("('send synch' will throw away most data in the network\n");
	printf("buffer, if this might help.)\n");
	return 0;
    }
    /* OK, they are all OK, now go through again and actually send */
    for (i = 1; i < argc; i++) {
	if ((s = getsend(argv[i])) == 0) {
	    fprintf(stderr, "Telnet 'send' error - argument disappeared!\n");
	    (void) quit();
	    /*NOTREACHED*/
	}
	if (s->handler) {
	    (*s->handler)(s);
	} else {
	    switch (what = s->what) {
	    case SYNCH:
		dosynch();
		break;
	    case SENDQUESTION:
		for (s = Sendlist; s->name; s++) {
		    if (s->help)
			printf("%-15s %s\n", s->name, s->help);
		}
		question = 1;
		break;
	    case SENDESCAPE:
		NETADD(escape);
		break;
	    default:
		NET2ADD(IAC, what);
		break;
	    }
	}
    }
    return !question;
}

/*
 * The following are the routines and data structures referred
 * to by the arguments to the "toggle" command.
 */

static
lclchars()
{
    donelclchars = 1;
    return 1;
}

static
togdebug()
{
#ifndef	NOT43
    if (net > 0 &&
	(SetSockOpt(net, SOL_SOCKET, SO_DEBUG, debug)) < 0) {
	    perror("setsockopt (SO_DEBUG)");
    }
#else	/* NOT43 */
    if (debug) {
	if (net > 0 && SetSockOpt(net, SOL_SOCKET, SO_DEBUG, 0, 0) < 0)
	    perror("setsockopt (SO_DEBUG)");
    } else
	printf("Cannot turn off socket debugging\n");
#endif	/* NOT43 */
    return 1;
}


static int
togcrlf()
{
    if (crlf) {
	printf("Will send carriage returns as telnet <CR><LF>.\n");
    } else {
	printf("Will send carriage returns as telnet <CR><NUL>.\n");
    }
    return 1;
}

int binmode;

static int
togbinary(val)
int val;
{
    donebinarytoggle = 1;

    if (val >= 0) {
	binmode = val;
    } else {
	if (my_want_state_is_will(TELOPT_BINARY) &&
				my_want_state_is_do(TELOPT_BINARY)) {
	    binmode = 1;
	} else if (my_want_state_is_wont(TELOPT_BINARY) &&
				my_want_state_is_dont(TELOPT_BINARY)) {
	    binmode = 0;
	}
	val = binmode ? 0 : 1;
    }

    if (val == 1) {
	if (my_want_state_is_will(TELOPT_BINARY) &&
					my_want_state_is_do(TELOPT_BINARY)) {
	    printf("Already operating in binary mode with remote host.\n");
	} else {
	    printf("Negotiating binary mode with remote host.\n");
	    tel_enter_binary(3);
	}
    } else {
	if (my_want_state_is_wont(TELOPT_BINARY) &&
					my_want_state_is_dont(TELOPT_BINARY)) {
	    printf("Already in network ascii mode with remote host.\n");
	} else {
	    printf("Negotiating network ascii mode with remote host.\n");
	    tel_leave_binary(3);
	}
    }
    return 1;
}

static int
togrbinary(val)
int val;
{
    donebinarytoggle = 1;

    if (val == -1)
	val = my_want_state_is_do(TELOPT_BINARY) ? 0 : 1;

    if (val == 1) {
	if (my_want_state_is_do(TELOPT_BINARY)) {
	    printf("Already receiving in binary mode.\n");
	} else {
	    printf("Negotiating binary mode on input.\n");
	    tel_enter_binary(1);
	}
    } else {
	if (my_want_state_is_dont(TELOPT_BINARY)) {
	    printf("Already receiving in network ascii mode.\n");
	} else {
	    printf("Negotiating network ascii mode on input.\n");
	    tel_leave_binary(1);
	}
    }
    return 1;
}

static int
togxbinary(val)
int val;
{
    donebinarytoggle = 1;

    if (val == -1)
	val = my_want_state_is_will(TELOPT_BINARY) ? 0 : 1;

    if (val == 1) {
	if (my_want_state_is_will(TELOPT_BINARY)) {
	    printf("Already transmitting in binary mode.\n");
	} else {
	    printf("Negotiating binary mode on output.\n");
	    tel_enter_binary(2);
	}
    } else {
	if (my_want_state_is_wont(TELOPT_BINARY)) {
	    printf("Already transmitting in network ascii mode.\n");
	} else {
	    printf("Negotiating network ascii mode on output.\n");
	    tel_leave_binary(2);
	}
    }
    return 1;
}


extern int togglehelp();
extern int slc_check();

struct togglelist {
    char	*name;		/* name of toggle */
    char	*help;		/* help message */
    int		(*handler)();	/* routine to do actual setting */
    int		*variable;
    char	*actionexplanation;
};

static struct togglelist Togglelist[] = {
    { "autoflush",
	"flushing of output when sending interrupt characters",
	    0,
		&autoflush,
		    "flush output when sending interrupt characters" },
    { "autosynch",
	"automatic sending of interrupt characters in urgent mode",
	    0,
		&autosynch,
		    "send interrupt characters in urgent mode" },
    { "binary",
	"sending and receiving of binary data",
	    togbinary,
		0,
		    0 },
    { "inbinary",
	"receiving of binary data",
	    togrbinary,
		0,
		    0 },
    { "outbinary",
	"sending of binary data",
	    togxbinary,
		0,
		    0 },
    { "crlf",
	"sending carriage returns as telnet <CR><LF>",
	    togcrlf,
		&crlf,
		    0 },
    { "crmod",
	"mapping of received carriage returns",
	    0,
		&crmod,
		    "map carriage return on output" },
    { "localchars",
	"local recognition of certain control characters",
	    lclchars,
		&localchars,
		    "recognize certain control characters" },
#ifdef	KERBEROS
    { "kerberos",
	"toggle use of Kerberos authentication",
	    0,
		&kerberized,
		    "use Kerberos authentication" },
#endif
    { " ", "", 0 },		/* empty line */
#if	defined(unix) && defined(TN3270)
    { "apitrace",
	"(debugging) toggle tracing of API transactions",
	    0,
		&apitrace,
		    "trace API transactions" },
    { "cursesdata",
	"(debugging) toggle printing of hexadecimal curses data",
	    0,
		&cursesdata,
		    "print hexadecimal representation of curses data" },
#endif	/* defined(unix) && defined(TN3270) */
    { "debug",
	"debugging",
	    togdebug,
		&debug,
		    "turn on socket level debugging" },
    { "netdata",
	"printing of hexadecimal network data (debugging)",
	    0,
		&netdata,
		    "print hexadecimal representation of network traffic" },
    { "prettydump",
	"output of \"netdata\" to user readable format (debugging)",
	    0,
		&prettydump,
		    "print user readable output for \"netdata\"" },
    { "options",
	"viewing of options processing (debugging)",
	    0,
		&showoptions,
		    "show option processing" },
#if	defined(unix)
    { "termdata",
	"(debugging) toggle printing of hexadecimal terminal data",
	    0,
		&termdata,
		    "print hexadecimal representation of terminal traffic" },
#endif	/* defined(unix) */
    { "?",
	0,
	    togglehelp },
    { "help",
	0,
	    togglehelp },
    { 0 }
};

static
togglehelp()
{
    struct togglelist *c;

    for (c = Togglelist; c->name; c++) {
	if (c->help) {
	    if (*c->help)
		printf("%-15s toggle %s\n", c->name, c->help);
	    else
		printf("\n");
	}
    }
    printf("\n");
    printf("%-15s %s\n", "?", "display help information");
    return 0;
}

static
settogglehelp(set)
int set;
{
    struct togglelist *c;

    for (c = Togglelist; c->name; c++) {
	if (c->help) {
	    if (*c->help)
		printf("%-15s %s %s\n", c->name, set ? "enable" : "disable",
						c->help);
	    else
		printf("\n");
	}
    }
}

static char **
getnexttoggle(name)
char *name;
{
    struct togglelist *c = (struct togglelist *) name;

    return (char **) (c+1);
}

static struct togglelist *
gettoggle(name)
char *name;
{
    return (struct togglelist *)
			genget(name, (char **) Togglelist, getnexttoggle);
}

static
toggle(argc, argv)
int	argc;
char	*argv[];
{
    int retval = 1;
    char *name;
    struct togglelist *c;

    if (argc < 2) {
	fprintf(stderr,
	    "Need an argument to 'toggle' command.  'toggle ?' for help.\n");
	return 0;
    }
    argc--;
    argv++;
    while (argc--) {
	name = *argv++;
	c = gettoggle(name);
	if (Ambiguous(c)) {
	    fprintf(stderr, "'%s': ambiguous argument ('toggle ?' for help).\n",
					name);
	    return 0;
	} else if (c == 0) {
	    fprintf(stderr, "'%s': unknown argument ('toggle ?' for help).\n",
					name);
	    return 0;
	} else {
	    if (c->variable) {
		*c->variable = !*c->variable;		/* invert it */
		if (c->actionexplanation) {
		    printf("%s %s.\n", *c->variable? "Will" : "Won't",
							c->actionexplanation);
		}
	    }
	    if (c->handler) {
		retval &= (*c->handler)(-1);
	    }
	}
    }
    return retval;
}

/*
 * The following perform the "set" command.
 */

#ifdef	USE_TERMIO
struct termio new_tc = { 0 };
#endif

struct setlist {
    char *name;				/* name */
    char *help;				/* help information */
    void (*handler)();
    cc_t *charp;			/* where it is located at */
};

static struct setlist Setlist[] = {
#ifdef	KLUDGELINEMODE
    { "echo", 	"character to toggle local echoing on/off", 0, &echoc },
#endif
    { "escape",	"character to escape back to telnet command mode", 0, &escape },
    { "tracefile", "file to write trace intormation to", SetNetTrace, (cc_t *)NetTraceFile},
    { " ", "" },
    { " ", "The following need 'localchars' to be toggled true", 0, 0 },
    { "flushoutput", "character to cause an Abort Oubput", 0, termFlushCharp },
    { "interrupt", "character to cause an Interrupt Process", 0, termIntCharp },
    { "quit",	"character to cause an Abort process", 0, termQuitCharp },
    { "eof",	"character to cause an EOF ", 0, termEofCharp },
    { " ", "" },
    { " ", "The following are for local editing in linemode", 0, 0 },
    { "erase",	"character to use to erase a character", 0, termEraseCharp },
    { "kill",	"character to use to erase a line", 0, termKillCharp },
    { "lnext",	"character to use for literal next", 0, termLiteralNextCharp },
    { "susp",	"character to cause a Suspend Process", 0, termSuspCharp },
    { "reprint", "character to use for line reprint", 0, termRprntCharp },
    { "worderase", "character to use to erase a word", 0, termWerasCharp },
    { "start",	"character to use for XON", 0, termStartCharp },
    { "stop",	"character to use for XOFF", 0, termStopCharp },
    { "forw1",	"alternate end of line character", 0, termForw1Charp },
    { "forw2",	"alternate end of line character", 0, termForw2Charp },
    { 0 }
};

#ifdef	CRAY
/* Work around compiler bug */
_setlist_init()
{
#ifndef	KLUDGELINEMODE
#define	N 4
#else
#define	N 5
#endif
	Setlist[N+0].charp = &termFlushChar;
	Setlist[N+1].charp = &termIntChar;
	Setlist[N+2].charp = &termQuitChar;
	Setlist[N+3].charp = &termEofChar;
	Setlist[N+6].charp = &termEraseChar;
	Setlist[N+7].charp = &termKillChar;
	Setlist[N+8].charp = &termLiteralNextChar;
	Setlist[N+9].charp = &termSuspChar;
	Setlist[N+10].charp = &termRprntChar;
	Setlist[N+11].charp = &termWerasChar;
	Setlist[N+12].charp = &termStartChar;
	Setlist[N+13].charp = &termStopChar;
	Setlist[N+14].charp = &termForw1Char;
	Setlist[N+15].charp = &termForw2Char;
#undef	N
}
#endif	/* CRAY */

static char **
getnextset(name)
char *name;
{
    struct setlist *c = (struct setlist *)name;

    return (char **) (c+1);
}

static struct setlist *
getset(name)
char *name;
{
    return (struct setlist *) genget(name, (char **) Setlist, getnextset);
}

set_escape_char(s)
char *s;
{
	escape = (s && *s) ? special(s) : -1;
	printf("escape character is '%s'.\n", control(escape));
}

static
setcmd(argc, argv)
int	argc;
char	*argv[];
{
    int value;
    struct setlist *ct;
    struct togglelist *c;

    if (argc < 2 || argc > 3) {
	printf("Format is 'set Name Value'\n'set ?' for help.\n");
	return 0;
    }
    if ((argc == 2) &&
		((!strcmp(argv[1], "?")) || (!strcmp(argv[1], "help")))) {
	for (ct = Setlist; ct->name; ct++)
	    printf("%-15s %s\n", ct->name, ct->help);
	printf("\n");
	settogglehelp(1);
	printf("%-15s %s\n", "?", "display help information");
	return 0;
    }

    ct = getset(argv[1]);
    if (ct == 0) {
	c = gettoggle(argv[1]);
	if (c == 0) {
	    fprintf(stderr, "'%s': unknown argument ('set ?' for help).\n",
			argv[1]);
	    return 0;
	} else if (Ambiguous(c)) {
	    fprintf(stderr, "'%s': ambiguous argument ('set ?' for help).\n",
			argv[1]);
	    return 0;
	}
	if (c->variable) {
	    if ((argc == 2) || (strcmp("on", argv[2]) == 0))
		*c->variable = 1;
	    else if (strcmp("off", argv[2]) == 0)
		*c->variable = 0;
	    else {
		printf("Format is 'set togglename [on|off]'\n'set ?' for help.\n");
		return 0;
	    }
	    if (c->actionexplanation) {
		printf("%s %s.\n", *c->variable? "Will" : "Won't",
							c->actionexplanation);
	    }
	}
	if (c->handler)
	    (*c->handler)(1);
    } else if (argc != 3) {
	printf("Format is 'set Name Value'\n'set ?' for help.\n");
	return 0;
    } else if (Ambiguous(ct)) {
	fprintf(stderr, "'%s': ambiguous argument ('set ?' for help).\n",
			argv[1]);
	return 0;
    } else if (ct->handler) {
	(*ct->handler)(argv[2]);
	printf("%s set to \"%s\".\n", ct->name, (char *)ct->charp);
    } else {
	if (strcmp("off", argv[2])) {
	    value = special(argv[2]);
	} else {
	    value = -1;
	}
	*(ct->charp) = (cc_t)value;
	printf("%s character is '%s'.\n", ct->name, control(*(ct->charp)));
    }
    slc_check();
    return 1;
}

static
unsetcmd(argc, argv)
int	argc;
char	*argv[];
{
    struct setlist *ct;
    struct togglelist *c;
    register char *name;

    if (argc < 2) {
	fprintf(stderr,
	    "Need an argument to 'unset' command.  'unset ?' for help.\n");
	return 0;
    }
    if ((!strcmp(argv[1], "?")) || (!strcmp(argv[1], "help"))) {
	for (ct = Setlist; ct->name; ct++)
	    printf("%-15s %s\n", ct->name, ct->help);
	printf("\n");
	settogglehelp(0);
	printf("%-15s %s\n", "?", "display help information");
	return 0;
    }

    argc--;
    argv++;
    while (argc--) {
	name = *argv++;
	ct = getset(name);
	if (ct == 0) {
	    c = gettoggle(name);
	    if (c == 0) {
		fprintf(stderr, "'%s': unknown argument ('unset ?' for help).\n",
			name);
		return 0;
	    } else if (Ambiguous(c)) {
		fprintf(stderr, "'%s': ambiguous argument ('unset ?' for help).\n",
			name);
		return 0;
	    }
	    if (c->variable) {
		*c->variable = 0;
		if (c->actionexplanation) {
		    printf("%s %s.\n", *c->variable? "Will" : "Won't",
							c->actionexplanation);
		}
	    }
	    if (c->handler)
		(*c->handler)(0);
	} else if (Ambiguous(ct)) {
	    fprintf(stderr, "'%s': ambiguous argument ('unset ?' for help).\n",
			name);
	    return 0;
	} else if (ct->handler) {
	    (*ct->handler)(0);
	    printf("%s reset to \"%s\".\n", ct->name, (char *)ct->charp);
	} else {
	    *(ct->charp) = -1;
	    printf("%s character is '%s'.\n", ct->name, control(*(ct->charp)));
	}
    }
    return 1;
}

/*
 * The following are the data structures and routines for the
 * 'mode' command.
 */
#ifdef	KLUDGELINEMODE
extern int kludgelinemode;

dokludgemode()
{
    kludgelinemode = 1;
    send_wont(TELOPT_LINEMODE, 1);
    send_dont(TELOPT_SGA, 1);
    send_dont(TELOPT_ECHO, 1);
}
#endif

static
dolinemode()
{
#ifdef	KLUDGELINEMODE
    if (kludgelinemode)
	send_dont(TELOPT_SGA, 1);
#endif
    send_will(TELOPT_LINEMODE, 1);
    send_dont(TELOPT_ECHO, 1);
    return 1;
}

static
docharmode()
{
#ifdef	KLUDGELINEMODE
    if (kludgelinemode)
	send_do(TELOPT_SGA, 1);
    else
#endif
    send_wont(TELOPT_LINEMODE, 1);
    send_do(TELOPT_ECHO, 1);
    return 1;
}

setmode(bit)
{
    return dolmmode(bit, 1);
}

clearmode(bit)
{
    return dolmmode(bit, 0);
}

dolmmode(bit, on)
int bit, on;
{
    char c;
    extern int linemode;

    if (my_want_state_is_wont(TELOPT_LINEMODE)) {
	printf("?Need to have LINEMODE option enabled first.\n");
	printf("'mode ?' for help.\n");
	return 0;
    }

    if (on)
	c = (linemode | bit);
    else
	c = (linemode & ~bit);
    lm_mode(&c, 1, 1);
    return 1;
}

struct modelist {
	char	*name;		/* command name */
	char	*help;		/* help string */
	int	(*handler)();	/* routine which executes command */
	int	needconnect;	/* Do we need to be connected to execute? */
	int	arg1;
};

extern int modehelp();

static struct modelist ModeList[] = {
    { "character", "Disable LINEMODE option",	docharmode, 1 },
#ifdef	KLUDGELINEMODE
    { "",	"(or disable obsolete line-by-line mode)", 0 },
#endif
    { "line",	"Enable LINEMODE option",	dolinemode, 1 },
#ifdef	KLUDGELINEMODE
    { "",	"(or enable obsolete line-by-line mode)", 0 },
#endif
    { "", "", 0 },
    { "",	"These require the LINEMODE option to be enabled", 0 },
    { "isig",	"Enable signal trapping",	setmode, 1, MODE_TRAPSIG },
    { "+isig",	0,				setmode, 1, MODE_TRAPSIG },
    { "-isig",	"Disable signal trapping",	clearmode, 1, MODE_TRAPSIG },
    { "edit",	"Enable character editing",	setmode, 1, MODE_EDIT },
    { "+edit",	0,				setmode, 1, MODE_EDIT },
    { "-edit",	"Disable character editing",	clearmode, 1, MODE_EDIT },
    { "softtabs", "Enable tab expansion",	setmode, 1, MODE_SOFT_TAB },
    { "+softtabs", 0,				setmode, 1, MODE_SOFT_TAB },
    { "-softtabs", "Disable character editing",	clearmode, 1, MODE_SOFT_TAB },
    { "litecho", "Enable literal character echo", setmode, 1, MODE_LIT_ECHO },
    { "+litecho", 0,				setmode, 1, MODE_LIT_ECHO },
    { "-litecho", "Disable literal character echo", clearmode, 1, MODE_LIT_ECHO },
    { "help",	0,				modehelp, 0 },
#ifdef	KLUDGELINEMODE
    { "kludgeline", 0,				dokludgemode, 1 },
#endif
    { "", "", 0 },
    { "?",	"Print help information",	modehelp, 0 },
    { 0 },
};

static char **
getnextmode(name)
char *name;
{
    return (char **) (((struct modelist *)name)+1);
}

static struct modelist *
getmodecmd(name)
char *name;
{
    return (struct modelist *) genget(name, (char **) ModeList, getnextmode);
}

modehelp()
{
    struct modelist *mt;

    printf("format is:  'mode Mode', where 'Mode' is one of:\n\n");
    for (mt = ModeList; mt->name; mt++) {
	if (mt->help) {
	    if (*mt->help)
		printf("%-15s %s\n", mt->name, mt->help);
	    else
		printf("\n");
	}
    }
    return 0;
}

static
modecmd(argc, argv)
int	argc;
char	*argv[];
{
    struct modelist *mt;

    if (argc != 2) {
	printf("'mode' command requires an argument\n");
	printf("'mode ?' for help.\n");
    } else if ((mt = getmodecmd(argv[1])) == 0) {
	fprintf(stderr, "Unknown mode '%s' ('mode ?' for help).\n", argv[1]);
    } else if (Ambiguous(mt)) {
	fprintf(stderr, "Ambiguous mode '%s' ('mode ?' for help).\n", argv[1]);
    } else if (mt->needconnect && !connected) {
	printf("?Need to be connected first.\n");
	printf("'mode ?' for help.\n");
    } else if (mt->handler) {
	return (*mt->handler)(mt->arg1);
    }
    return 0;
}

/*
 * The following data structures and routines implement the
 * "display" command.
 */

static
display(argc, argv)
int	argc;
char	*argv[];
{
#define	dotog(tl)	if (tl->variable && tl->actionexplanation) { \
			    if (*tl->variable) { \
				printf("will"); \
			    } else { \
				printf("won't"); \
			    } \
			    printf(" %s.\n", tl->actionexplanation); \
			}

#define	doset(sl)   if (sl->name && *sl->name != ' ') { \
			if (sl->handler == 0) \
			    printf("%-15s [%s]\n", sl->name, control(*sl->charp)); \
			else \
			    printf("%-15s \"%s\"\n", sl->name, (char *)sl->charp); \
		    }

    struct togglelist *tl;
    struct setlist *sl;

    if (argc == 1) {
	for (tl = Togglelist; tl->name; tl++) {
	    dotog(tl);
	}
	printf("\n");
	for (sl = Setlist; sl->name; sl++) {
	    doset(sl);
	}
    } else {
	int i;

	for (i = 1; i < argc; i++) {
	    sl = getset(argv[i]);
	    tl = gettoggle(argv[i]);
	    if (Ambiguous(sl) || Ambiguous(tl)) {
		printf("?Ambiguous argument '%s'.\n", argv[i]);
		return 0;
	    } else if (!sl && !tl) {
		printf("?Unknown argument '%s'.\n", argv[i]);
		return 0;
	    } else {
		if (tl) {
		    dotog(tl);
		}
		if (sl) {
		    doset(sl);
		}
	    }
	}
    }
/*@*/optionstatus();
    return 1;
#undef	doset
#undef	dotog
}

/*
 * The following are the data structures, and many of the routines,
 * relating to command processing.
 */

/*
 * Set the escape character.
 */
static
setescape(argc, argv)
	int argc;
	char *argv[];
{
	register char *arg;
	char buf[50];

	printf(
	    "Deprecated usage - please use 'set escape%s%s' in the future.\n",
				(argc > 2)? " ":"", (argc > 2)? argv[1]: "");
	if (argc > 2)
		arg = argv[1];
	else {
		printf("new escape character: ");
		(void) gets(buf);
		arg = buf;
	}
	if (arg[0] != '\0')
		escape = arg[0];
	if (!In3270) {
		printf("Escape character is '%s'.\n", control(escape));
	}
	(void) fflush(stdout);
	return 1;
}

/*VARARGS*/
static
togcrmod()
{
    crmod = !crmod;
    printf("Deprecated usage - please use 'toggle crmod' in the future.\n");
    printf("%s map carriage return on output.\n", crmod ? "Will" : "Won't");
    (void) fflush(stdout);
    return 1;
}

/*VARARGS*/
suspend()
{
#ifdef	SIGTSTP
    setcommandmode();
    {
	long oldrows, oldcols, newrows, newcols, err;

	err = TerminalWindowSize(&oldrows, &oldcols);
	(void) kill(0, SIGTSTP);
	err += TerminalWindowSize(&newrows, &newcols);
	if (connected && !err &&
	    ((oldrows != newrows) || (oldcols != newcols))) {
		sendnaws();
	}
    }
    /* reget parameters in case they were changed */
    TerminalSaveState();
    setconnmode(0);
#else
    printf("Suspend is not supported.  Try the '!' command instead\n");
#endif
    return 1;
}

#if	!defined(TN3270)
/*ARGSUSED*/
shell(argc, argv)
int argc;
char *argv[];
{
    extern char *rindex();

    setcommandmode();
    switch(vfork()) {
    case -1:
	perror("Fork failed\n");
	break;

    case 0:
	{
	    /*
	     * Fire up the shell in the child.
	     */
	    register char *shell, *shellname;

	    shell = getenv("SHELL");
	    if (shell == NULL)
		shell = "/bin/sh";
	    if ((shellname = rindex(shell, '/')) == 0)
		shellname = shell;
	    else
		shellname++;
	    if (argc > 1)
		execl(shell, shellname, "-c", &saveline[1], 0);
	    else
		execl(shell, shellname, 0);
	    perror("Execl");
	    _exit(1);
	}
    default:
	    (void)wait((int *)0);	/* Wait for the shell to complete */
    }
}
#endif	/* !defined(TN3270) */

/*VARARGS*/
static
bye(argc, argv)
int	argc;		/* Number of arguments */
char	*argv[];	/* arguments */
{
    if (connected) {
	(void) shutdown(net, 2);
	printf("Connection closed.\n");
	(void) NetClose(net);
	connected = 0;
	/* reset options */
	tninit();
#if	defined(TN3270)
	SetIn3270();		/* Get out of 3270 mode */
#endif	/* defined(TN3270) */
    }
    if ((argc != 2) || (strcmp(argv[1], "fromquit") != 0)) {
	longjmp(toplevel, 1);
	/* NOTREACHED */
    }
    return 1;			/* Keep lint, etc., happy */
}

/*VARARGS*/
quit()
{
	(void) call(bye, "bye", "fromquit", 0);
	Exit(0);
	/*NOTREACHED*/
}

/*
 * The SLC command.
 */

struct slclist {
	char	*name;
	char	*help;
	int	(*handler)();
	int	arg;
};

extern int slc_help();
extern int slc_mode_export(), slc_mode_import(), slcstate();

struct slclist SlcList[] = {
    { "export",	"Use local special character definitions",
						slc_mode_export,	0 },
    { "import",	"Use remote special character definitions",
						slc_mode_import,	1 },
    { "check",	"Verify remote special character definitions",
						slc_mode_import,	0 },
    { "help",	0,				slc_help,		0 },
    { "?",	"Print help information",	slc_help,		0 },
    { 0 },
};

static
slc_help()
{
    struct slclist *c;

    for (c = SlcList; c->name; c++) {
	if (c->help) {
	    if (*c->help)
		printf("%-15s %s\n", c->name, c->help);
	    else
		printf("\n");
	}
    }
}

static char **
getnextslc(name)
char *name;
{
    return (char **)(((struct slclist *)name)+1);
}

static struct slclist *
getslc(name)
char *name;
{
    return (struct slclist *)genget(name, (char **) SlcList, getnextslc);
}

static
slccmd(argc, argv)
int	argc;
char	*argv[];
{
    struct slclist *c;

    if (argc != 2) {
	fprintf(stderr,
	    "Need an argument to 'slc' command.  'slc ?' for help.\n");
	return 0;
    }
    c = getslc(argv[1]);
    if (c == 0) {
        fprintf(stderr, "'%s': unknown argument ('slc ?' for help).\n",
    				argv[1]);
        return 0;
    }
    if (Ambiguous(c)) {
        fprintf(stderr, "'%s': ambiguous argument ('slc ?' for help).\n",
    				argv[1]);
        return 0;
    }
    (*c->handler)(c->arg);
    slcstate();
    return 1;
}

/*
 * The ENVIRON command.
 */

struct envlist {
	char	*name;
	char	*help;
	int	(*handler)();
	int	narg;
};

extern struct env_lst *env_define();
extern int env_undefine();
extern int env_export(), env_unexport();
extern int env_list(), env_help();

struct envlist EnvList[] = {
    { "define",	"Define an environment variable",
						(int (*)())env_define,	2 },
    { "undefine", "Undefine an environment variable",
						env_undefine,	1 },
    { "export",	"Mark an environment variable for automatic export",
						env_export,	1 },
    { "unexport", "Dont mark an environment variable for automatic export",
						env_unexport,	1 },
    { "list",	"List the current environment variables",
						env_list,	0 },
    { "help",	0,				env_help,		0 },
    { "?",	"Print help information",	env_help,		0 },
    { 0 },
};

static
env_help()
{
    struct envlist *c;

    for (c = EnvList; c->name; c++) {
	if (c->help) {
	    if (*c->help)
		printf("%-15s %s\n", c->name, c->help);
	    else
		printf("\n");
	}
    }
}

static char **
getnextenv(name)
char *name;
{
    return (char **)(((struct envlist *)name)+1);
}

static struct envlist *
getenvcmd(name)
char *name;
{
    return (struct envlist *)genget(name, (char **) EnvList, getnextenv);
}

env_cmd(argc, argv)
int	argc;
char	*argv[];
{
    struct envlist *c;

    if (argc < 2) {
	fprintf(stderr,
	    "Need an argument to 'environ' command.  'environ ?' for help.\n");
	return 0;
    }
    c = getenvcmd(argv[1]);
    if (c == 0) {
        fprintf(stderr, "'%s': unknown argument ('environ ?' for help).\n",
    				argv[1]);
        return 0;
    }
    if (Ambiguous(c)) {
        fprintf(stderr, "'%s': ambiguous argument ('environ ?' for help).\n",
    				argv[1]);
        return 0;
    }
    if (c->narg + 2 != argc) {
	fprintf(stderr,
	    "Need %s%d argument%s to 'environ %s' command.  'environ ?' for help.\n",
		c->narg < argc + 2 ? "only " : "",
		c->narg, c->narg == 1 ? "" : "s", c->name);
	return 0;
    }
    (void)(*c->handler)(argv[2], argv[3]);
    return 1;
}

struct env_lst {
	struct env_lst *next;	/* pointer to next structure */
	struct env_lst *prev;	/* pointer to next structure */
	char *var;		/* pointer to variable name */
	char *value;		/* pointer to varialbe value */
	int export;		/* 1 -> export with default list of variables */
};

struct env_lst envlisthead;

struct env_lst *
env_find(var)
{
	register struct env_lst *ep;

	for (ep = envlisthead.next; ep; ep = ep->next) {
		if (strcmp(ep->var, var) == 0)
			return(ep);
	}
	return(NULL);
}

env_init()
{
	extern char **environ, *index();
	register char **epp, *cp;
	register struct env_lst *ep;

	for (epp = environ; *epp; epp++) {
		if (cp = index(*epp, '=')) {
			*cp = '\0';
			ep = env_define(*epp, cp+1);
			ep->export = 0;
			*cp = '=';
		}
	}
	/*
	 * Special case for DISPLAY variable.  If it is ":0.0" or
	 * "unix:0.0", we have to get rid of "unix" and insert our
	 * hostname.
	 */
	if ((ep = env_find("DISPLAY")) &&
	    ((*ep->value == ':') || (strncmp(ep->value, "unix:", 5) == 0))) {
		char hbuf[256+1];
		char *cp2 = index(ep->value, ':');

		gethostname(hbuf, 256);
		hbuf[256] = '\0';
		cp = (char *)malloc(strlen(hbuf) + strlen(cp2) + 1);
		sprintf(cp, "%s%s", hbuf, cp2);
		free(ep->value);
		ep->value = cp;
	}
#ifdef notdef
	/*
	 * If USER is not defined, but LOGNAME is, then add
	 * USER with the value from LOGNAME.
	 */
	if ((env_find("USER") == NULL) && (ep = env_find("LOGNAME")))
		env_define("USER", ep->value);
	env_export("USER");
#endif
	env_export("DISPLAY");
	env_export("PRINTER");
}

struct env_lst *
env_define(var, value)
char *var, *value;
{
	register struct env_lst *ep;
	extern char *savestr();

	if (ep = env_find(var)) {
		if (ep->var)
			free(ep->var);
		if (ep->value)
			free(ep->value);
	} else {
		ep = (struct env_lst *)malloc(sizeof(struct env_lst));
		ep->next = envlisthead.next;
		envlisthead.next = ep;
		ep->prev = &envlisthead;
		if (ep->next)
			ep->next->prev = ep;
	}
	ep->export = 1;
	ep->var = savestr(var);
	ep->value = savestr(value);
	return(ep);
}

env_undefine(var)
char *var;
{
	register struct env_lst *ep;

	if (ep = env_find(var)) {
		ep->prev->next = ep->next;
		ep->next->prev = ep->prev;
		if (ep->var)
			free(ep->var);
		if (ep->value)
			free(ep->value);
		free(ep);
	}
}

env_export(var)
char *var;
{
	register struct env_lst *ep;

	if (ep = env_find(var))
		ep->export = 1;
}

env_unexport(var)
char *var;
{
	register struct env_lst *ep;

	if (ep = env_find(var))
		ep->export = 0;
}

env_list()
{
	register struct env_lst *ep;

	for (ep = envlisthead.next; ep; ep = ep->next) {
		printf("%c %-20s %s\n", ep->export ? '*' : ' ',
					ep->var, ep->value);
	}
}

char *
env_default(init)
{
	static struct env_lst *nep = NULL;

	if (init) {
		nep = &envlisthead;
		return;
	}
	if (nep) {
		while (nep = nep->next) {
			if (nep->export)
				return(nep->var);
		}
	}
	return(NULL);
}

char *
env_getvalue(var)
char *var;
{
	register struct env_lst *ep;

	if (ep = env_find(var))
		return(ep->value);
	return(NULL);
}

char *
savestr(s)
register char *s;
{
	register char *ret;
	if (ret = (char *)malloc(strlen(s)+1))
		strcpy(ret, s);
	return(ret);
}

#if	defined(unix)
#ifdef notdef
/*
 * Some information about our file descriptors.
 */

char *
decodeflags(mask)
int mask;
{
    static char buffer[100];
#define do(m,s) \
	if (mask&(m)) { \
	    strcat(buffer, (s)); \
	}

    buffer[0] = 0;			/* Terminate it */

#ifdef FREAD
    do(FREAD, " FREAD");
#endif
#ifdef FWRITE
    do(FWRITE, " FWRITE");
#endif
#ifdef F_DUPFP
    do(F_DUPFD, " F_DUPFD");
#endif
#ifdef FNDELAY
    do(FNDELAY, " FNDELAY");
#endif
#ifdef FAPPEND
    do(FAPPEND, " FAPPEND");
#endif
#ifdef FMARK
    do(FMARK, " FMARK");
#endif
#ifdef FDEFER
    do(FDEFER, " FDEFER");
#endif
#ifdef FASYNC
    do(FASYNC, " FASYNC");
#endif
#ifdef FSHLOCK
    do(FSHLOCK, " FSHLOCK");
#endif
#ifdef FEXLOCK
    do(FEXLOCK, " FEXLOCK");
#endif
#ifdef FCREAT
    do(FCREAT, " FCREAT");
#endif
#ifdef FTRUNC
    do(FTRUNC, " FTRUNC");
#endif
#ifdef FEXCL
    do(FEXCL, " FEXCL");
#endif

    return buffer;
}
#undef do
#endif	/* notdef */

#if defined(TN3270)
static void
filestuff(fd)
int fd;
{
    int res;

#ifdef	F_GETOWN
    setconnmode(0);
    res = fcntl(fd, F_GETOWN, 0);
    setcommandmode();

    if (res == -1) {
	perror("fcntl");
	return;
    }
    printf("\tOwner is %d.\n", res);
#endif

    setconnmode(0);
    res = fcntl(fd, F_GETFL, 0);
    setcommandmode();

    if (res == -1) {
	perror("fcntl");
	return;
    }
    printf("\tFlags are 0x%x: %s\n", res, decodeflags(res));
}
#endif /* defined(TN3270) */


#endif	/* defined(unix) */

/*
 * Print status about the connection.
 */
/*ARGSUSED*/
static
status(argc, argv)
int	argc;
char	*argv[];
{
    if (connected) {
	printf("Connected to %s.\n", hostname);
	if ((argc < 2) || strcmp(argv[1], "notmuch")) {
	    int mode = getconnmode();

	    if (my_want_state_is_will(TELOPT_LINEMODE)) {
		printf("Operating with LINEMODE option\n");
		printf("%s line editing\n", (mode&MODE_EDIT) ? "Local" : "No");
		printf("%s catching of signals\n",
					(mode&MODE_TRAPSIG) ? "Local" : "No");
		slcstate();
#ifdef	KLUDGELINEMODE
	    } else if (kludgelinemode && my_want_state_is_dont(TELOPT_SGA)) {
		printf("Operating in obsolete linemode\n");
#endif
	    } else {
		printf("Operating in single character mode\n");
		if (localchars)
		    printf("Catching signals locally\n");
	    }
	    printf("%s character echo\n", (mode&MODE_ECHO) ? "Local" : "Remote");
	    if (my_want_state_is_will(TELOPT_LFLOW))
		printf("%s flow control\n", (mode&MODE_FLOW) ? "Local" : "No");
	}
    } else {
	printf("No connection.\n");
    }
#   if !defined(TN3270)
    printf("Escape character is '%s'.\n", control(escape));
    (void) fflush(stdout);
#   else /* !defined(TN3270) */
    if ((!In3270) && ((argc < 2) || strcmp(argv[1], "notmuch"))) {
	printf("Escape character is '%s'.\n", control(escape));
    }
#   if defined(unix)
    if ((argc >= 2) && !strcmp(argv[1], "everything")) {
	printf("SIGIO received %d time%s.\n",
				sigiocount, (sigiocount == 1)? "":"s");
	if (In3270) {
	    printf("Process ID %d, process group %d.\n",
					    getpid(), getpgrp(getpid()));
	    printf("Terminal input:\n");
	    filestuff(tin);
	    printf("Terminal output:\n");
	    filestuff(tout);
	    printf("Network socket:\n");
	    filestuff(net);
	}
    }
    if (In3270 && transcom) {
       printf("Transparent mode command is '%s'.\n", transcom);
    }
#   endif /* defined(unix) */
    (void) fflush(stdout);
    if (In3270) {
	return 0;
    }
#   endif /* defined(TN3270) */
    return 1;
}


#if	defined(NEED_GETTOS)
struct tosent {
	char	*t_name;	/* name */
	char	**t_aliases;	/* alias list */
	char	*t_proto;	/* protocol */
	int	t_tos;		/* Type Of Service bits */
};

struct tosent *
gettosbyname(name, proto)
char *name, *proto;
{
	static struct tosent te;
	static char *aliasp = 0;

	te.t_name = name;
	te.t_aliases = &aliasp;
	te.t_proto = proto;
	te.t_tos = 020; /* Low Delay bit */
	return(&te);
}
#endif

extern	int autologin;

int
tn(argc, argv)
	int argc;
	char *argv[];
{
    register struct hostent *host = 0;
    struct sockaddr_in sin;
    struct servent *sp = 0;
    static char	hnamebuf[32];
    unsigned long temp, inet_addr();
    extern char *inet_ntoa();
#if	defined(SRCRT) && defined(IPPROTO_IP)
    char *srp = 0, *strrchr();
    unsigned long sourceroute(), srlen;
#endif
#if defined(HAS_IP_TOS) || defined(NEED_GETTOS)
    struct tosent *tp;
#endif /* defined(HAS_IP_TOS) || defined(NEED_GETTOS) */
    char *cmd, *hostp = 0, *portp = 0, *user = 0;


    if (connected) {
	printf("?Already connected to %s\n", hostname);
	return 0;
    }
    if (argc < 2) {
	(void) strcpy(line, "Connect ");
	printf("(to) ");
	(void) gets(&line[strlen(line)]);
	makeargv();
	argc = margc;
	argv = margv;
    }
    cmd = *argv;
    --argc; ++argv;
    while (argc) {
	if (strcmp(*argv, "-l") == 0) {
	    --argc; ++argv;
	    if (argc == 0)
		goto usage;
	    user = *argv++;
	    --argc;
	    continue;
	}
	if (hostp == 0) {
	    hostp = *argv++;
	    --argc;
	    continue;
	}
	if (portp == 0) {
	    portp = *argv++;
	    --argc;
	    continue;
	}
    usage:
	printf("usage: %s [-l user] host-name [port]\n", cmd);
	return 0;
    }
#if	defined(SRCRT) && defined(IPPROTO_IP)
    if (hostp[0] == '@' || hostp[0] == '!') {
	if ((hostname = strrchr(hostp, ':')) == NULL)
	    hostname = strrchr(hostp, '@');
	hostname++;
	srp = 0;
	temp = sourceroute(hostp, &srp, &srlen);
	if (temp == 0) {
	    herror(srp);
	    return 0;
	} else if (temp == -1) {
	    printf("Bad source route option: %s\n", hostp);
	    return 0;
	} else {
	    sin.sin_addr.s_addr = temp;
	    sin.sin_family = AF_INET;
	}
    } else {
#endif
	temp = inet_addr(hostp);
	if (temp != (unsigned long) -1) {
	    sin.sin_addr.s_addr = temp;
	    sin.sin_family = AF_INET;
	    (void) strcpy(hnamebuf, hostp);
	    hostname = hnamebuf;
	} else {
	    host = gethostbyname(hostp);
	    if (host) {
		sin.sin_family = host->h_addrtype;
#if	defined(h_addr)		/* In 4.3, this is a #define */
		memcpy((caddr_t)&sin.sin_addr,
				host->h_addr_list[0], host->h_length);
#else	/* defined(h_addr) */
		memcpy((caddr_t)&sin.sin_addr, host->h_addr, host->h_length);
#endif	/* defined(h_addr) */
		hostname = host->h_name;
	    } else {
		herror(hostp);
		return 0;
	    }
	}
#if	defined(SRCRT) && defined(IPPROTO_IP)
    }
#endif
    if (portp) {
	if (*portp == '-') {
	    portp++;
	    telnetport = 1;
	} else
	    telnetport = 0;
	sin.sin_port = atoi(portp);
	if (sin.sin_port == 0) {
	    sp = getservbyname(portp, "tcp");
	    if (sp)
		sin.sin_port = sp->s_port;
	    else {
		printf("%s: bad port number\n", portp);
		return 0;
	    }
	} else {
#if	!defined(htons)
	    u_short htons();
#endif	/* !defined(htons) */
	    sin.sin_port = htons(sin.sin_port);
	}
    } else {
	if (sp == 0) {
	    sp = getservbyname("telnet", "tcp");
	    if (sp == 0) {
		fprintf(stderr, "telnet: tcp/telnet: unknown service\n");
		return 0;
	    }
	    sin.sin_port = sp->s_port;
	}
	telnetport = 1;
    }
    printf("Trying %s...\n", inet_ntoa(sin.sin_addr));
    do {
	net = socket(AF_INET, SOCK_STREAM, 0);
	if (net < 0) {
	    perror("telnet: socket");
	    return 0;
	}
#if	defined(SRCRT) && defined(IPPROTO_IP)
	if (srp && setsockopt(net, IPPROTO_IP, IP_OPTIONS, (char *)srp, srlen) < 0)
		perror("setsockopt (IP_OPTIONS)");
#endif
#if	defined(HAS_IP_TOS) || defined(NEED_GETTOS)
	if ((tp = gettosbyname("telnet", "tcp")) &&
	    (setsockopt(net, IPPROTO_IP, IP_TOS, &tp->t_tos, sizeof(int)) < 0))
		perror("telnet: setsockopt TOS (ignored)");
#endif	/* defined(HAS_IP_TOS) || defined(NEED_GETTOS) */

	if (debug && SetSockOpt(net, SOL_SOCKET, SO_DEBUG, 1) < 0) {
		perror("setsockopt (SO_DEBUG)");
	}

	if (connect(net, (struct sockaddr *)&sin, sizeof (sin)) < 0) {
#if	defined(h_addr)		/* In 4.3, this is a #define */
	    if (host && host->h_addr_list[1]) {
		int oerrno = errno;

		fprintf(stderr, "telnet: connect to address %s: ",
						inet_ntoa(sin.sin_addr));
		errno = oerrno;
		perror((char *)0);
		host->h_addr_list++;
		memcpy((caddr_t)&sin.sin_addr, 
			host->h_addr_list[0], host->h_length);
		(void) NetClose(net);
		continue;
	    }
#endif	/* defined(h_addr) */
	    perror("telnet: Unable to connect to remote host");
	    return 0;
	}
	connected++;
    } while (connected == 0);
    cmdrc(hostp, hostname);
    if (autologin && user == NULL) {
	struct passwd *pw;
	uid_t uid = getuid();

	user = getlogin();
	if (user == NULL ||
	    (pw = getpwnam(user)) && pw->pw_uid != uid)
		if (pw = getpwuid(uid))
			user = pw->pw_name;
		else
			user = NULL;
    }
    if (user) {
	env_define("USER", user);
	env_export("USER");
    }
    (void) call(status, "status", "notmuch", 0);
    if (setjmp(peerdied) == 0)
	telnet();
    (void) NetClose(net);
    ExitString("Connection closed by foreign host.\n",1);
    /*NOTREACHED*/
}


#define HELPINDENT (sizeof ("connect"))

static char
	openhelp[] =	"connect to a site",
	closehelp[] =	"close current connection",
	quithelp[] =	"exit telnet",
	statushelp[] =	"print status information",
	helphelp[] =	"print help information",
	sendhelp[] =	"transmit special characters ('send ?' for more)",
	sethelp[] = 	"set operating parameters ('set ?' for more)",
	unsethelp[] = 	"unset operating parameters ('unset ?' for more)",
	togglestring[] ="toggle operating parameters ('toggle ?' for more)",
	slchelp[] =	"change state of special charaters ('slc ?' for more)",
	displayhelp[] =	"display operating parameters",
#if	defined(TN3270) && defined(unix)
	transcomhelp[] = "specify Unix command for transparent mode pipe",
#endif	/* defined(TN3270) && defined(unix) */
#if	defined(unix)
	zhelp[] =	"suspend telnet",
#endif	/* defined(unix) */
	shellhelp[] =	"invoke a subshell",
	envhelp[] =	"change environment variables ('environ ?' for more)",
	modestring[] = "try to enter line or character mode ('mode ?' for more)";

extern int	help(), shell();

static Command cmdtab[] = {
	{ "close",	closehelp,	bye,		1 },
	{ "display",	displayhelp,	display,	0 },
	{ "mode",	modestring,	modecmd,	0 },
	{ "open",	openhelp,	tn,		0 },
	{ "quit",	quithelp,	quit,		0 },
	{ "send",	sendhelp,	sendcmd,	0 },
	{ "set",	sethelp,	setcmd,		0 },
	{ "unset",	unsethelp,	unsetcmd,	0 },
	{ "status",	statushelp,	status,		0 },
	{ "toggle",	togglestring,	toggle,		0 },
	{ "slc",	slchelp,	slccmd,		0 },
#if	defined(TN3270) && defined(unix)
	{ "transcom",	transcomhelp,	settranscom,	0 },
#endif	/* defined(TN3270) && defined(unix) */
#if	defined(unix)
	{ "z",		zhelp,		suspend,	0 },
#endif	/* defined(unix) */
#if	defined(TN3270)
	{ "!",		shellhelp,	shell,		1 },
#else
	{ "!",		shellhelp,	shell,		0 },
#endif
	{ "environ",	envhelp,	env_cmd,	0 },
	{ "?",		helphelp,	help,		0 },
	0
};

static char	crmodhelp[] =	"deprecated command -- use 'toggle crmod' instead";
static char	escapehelp[] =	"deprecated command -- use 'set escape' instead";

static Command cmdtab2[] = {
	{ "help",	0,		help,		0 },
	{ "escape",	escapehelp,	setescape,	0 },
	{ "crmod",	crmodhelp,	togcrmod,	0 },
	0
};


/*
 * Call routine with argc, argv set from args (terminated by 0).
 */

/*VARARGS1*/
static
call(va_alist)
va_dcl
{
    va_list ap;
    typedef int (*intrtn_t)();
    intrtn_t routine;
    char *args[100];
    int argno = 0;

    va_start(ap);
    routine = (va_arg(ap, intrtn_t));
    while ((args[argno++] = va_arg(ap, char *)) != 0) {
	;
    }
    va_end(ap);
    return (*routine)(argno-1, args);
}


static char **
getnextcmd(name)
char *name;
{
    Command *c = (Command *) name;

    return (char **) (c+1);
}

static Command *
getcmd(name)
char *name;
{
    Command *cm;

    if ((cm = (Command *) genget(name, (char **) cmdtab, getnextcmd)) != 0) {
	return cm;
    } else {
	return (Command *) genget(name, (char **) cmdtab2, getnextcmd);
    }
}

void
command(top, tbuf, cnt)
	int top;
	char *tbuf;
	int cnt;
{
    register Command *c;

    setcommandmode();
    if (!top) {
	putchar('\n');
#if	defined(unix)
    } else {
	(void) signal(SIGINT, SIG_DFL);
	(void) signal(SIGQUIT, SIG_DFL);
#endif	/* defined(unix) */
    }
    for (;;) {
	printf("%s> ", prompt);
	if (tbuf) {
	    register char *cp;
	    cp = line;
	    while (cnt > 0 && (*cp++ = *tbuf++) != '\n')
		cnt--;
	    tbuf = 0;
	    if (cp == line || *--cp != '\n' || cp == line)
		goto getline;
	    *cp = '\0';
	    printf("%s\n", line);
	} else {
	getline:
	    if (gets(line) == NULL) {
		if (feof(stdin) || ferror(stdin)) {
		    (void) quit();
		    /*NOTREACHED*/
		}
		break;
	    }
	}
	if (line[0] == 0)
	    break;
	makeargv();
	if (margv[0] == 0) {
	    break;
	}
	c = getcmd(margv[0]);
	if (Ambiguous(c)) {
	    printf("?Ambiguous command\n");
	    continue;
	}
	if (c == 0) {
	    printf("?Invalid command\n");
	    continue;
	}
	if (c->needconnect && !connected) {
	    printf("?Need to be connected first.\n");
	    continue;
	}
	if ((*c->handler)(margc, margv)) {
	    break;
	}
    }
    if (!top) {
	if (!connected) {
	    longjmp(toplevel, 1);
	    /*NOTREACHED*/
	}
#if	defined(TN3270)
	if (shell_active == 0) {
	    setconnmode(0);
	}
#else	/* defined(TN3270) */
	setconnmode(0);
#endif	/* defined(TN3270) */
    }
}

/*
 * Help command.
 */
static
help(argc, argv)
	int argc;
	char *argv[];
{
	register Command *c;

	if (argc == 1) {
		printf("Commands may be abbreviated.  Commands are:\n\n");
		for (c = cmdtab; c->name; c++)
			if (c->help) {
				printf("%-*s\t%s\n", HELPINDENT, c->name,
								    c->help);
			}
		return 0;
	}
	while (--argc > 0) {
		register char *arg;
		arg = *++argv;
		c = getcmd(arg);
		if (Ambiguous(c))
			printf("?Ambiguous help command %s\n", arg);
		else if (c == (Command *)0)
			printf("?Invalid help command %s\n", arg);
		else
			printf("%s\n", c->help);
	}
	return 0;
}

static char *rcname = 0;
static char rcbuf[128];

cmdrc(m1, m2)
	char *m1, *m2;
{
    register Command *c;
    FILE *rcfile;
    int gotmachine = 0;
    int l1 = strlen(m1);
    int l2 = strlen(m2);
    char m1save[64];

    strcpy(m1save, m1);
    m1 = m1save;

    if (rcname == 0) {
	rcname = getenv("HOME");
	if (rcname)
	    strcpy(rcbuf, rcname);
	else
	    rcbuf[0] = '\0';
	strcat(rcbuf, "/.telnetrc");
	rcname = rcbuf;
    }

    if ((rcfile = fopen(rcname, "r")) == 0) {
	return;
    }

    for (;;) {
	if (fgets(line, sizeof(line), rcfile) == NULL)
	    break;
	if (line[0] == 0)
	    break;
	if (line[0] == '#')
	    continue;
	if (gotmachine == 0) {
	    if (isspace(line[0]))
		continue;
	    if (strncasecmp(line, m1, l1) == 0)
		strncpy(line, &line[l1], sizeof(line) - l1);
	    else if (strncasecmp(line, m2, l2) == 0)
		strncpy(line, &line[l2], sizeof(line) - l2);
	    else
		continue;
	    gotmachine = 1;
	} else {
	    if (!isspace(line[0])) {
		gotmachine = 0;
		continue;
	    }
	}
	makeargv();
	if (margv[0] == 0)
	    continue;
	c = getcmd(margv[0]);
	if (Ambiguous(c)) {
	    printf("?Ambiguous command: %s\n", margv[0]);
	    continue;
	}
	if (c == 0) {
	    printf("?Invalid command: %s\n", margv[0]);
	    continue;
	}
	/*
	 * This should never happen...
	 */
	if (c->needconnect && !connected) {
	    printf("?Need to be connected first for %s.\n", margv[0]);
	    continue;
	}
	(*c->handler)(margc, margv);
    }
    fclose(rcfile);
}

#if	defined(SRCRT) && defined(IPPROTO_IP)

/*
 * Source route is handed in as
 *	[!]@hop1@hop2...[@|:]dst
 * If the leading ! is present, it is a
 * strict source route, otherwise it is
 * assmed to be a loose source route.
 *
 * We fill in the source route option as
 *	hop1,hop2,hop3...dest
 * and return a pointer to hop1, which will
 * be the address to connect() to.
 *
 * Arguments:
 *	arg:	pointer to route list to decipher
 *
 *	cpp: 	If *cpp is not equal to NULL, this is a
 *		pointer to a pointer to a character array
 *		that should be filled in with the option.
 *
 *	lenp:	pointer to an integer that contains the
 *		length of *cpp if *cpp != NULL.
 *
 * Return values:
 *
 *	Returns the address of the host to connect to.  If the
 *	return value is -1, there was a syntax error in the
 *	option, either unknown characters, or too many hosts.
 *	If the return value is 0, one of the hostnames in the
 *	path is unknown, and *cpp is set to point to the bad
 *	hostname.
 *
 *	*cpp:	If *cpp was equal to NULL, it will be filled
 *		in with a pointer to our static area that has
 *		the option filled in.  This will be 32bit aligned.
 * 
 *	*lenp:	This will be filled in with how long the option
 *		pointed to by *cpp is.
 *	
 */
unsigned long
sourceroute(arg, cpp, lenp)
char	*arg;
char	**cpp;
int	*lenp;
{
	static char lsr[44];
	char *cp, *cp2, *lsrp, *lsrep, *index();
	register int tmp;
	struct in_addr sin_addr;
	register struct hostent *host = 0;
	register char c;

	/*
	 * Verify the arguments, and make sure we have
	 * at least 7 bytes for the option.
	 */
	if (cpp == NULL || lenp == NULL)
		return((unsigned long)-1);
	if (*cpp != NULL && *lenp < 7)
		return((unsigned long)-1);
	/*
	 * Decide whether we have a buffer passed to us,
	 * or if we need to use our own static buffer.
	 */
	if (*cpp) {
		lsrp = *cpp;
		lsrep = lsrp + *lenp;
	} else {
		*cpp = lsrp = lsr;
		lsrep = lsrp + 44;
	}

	cp = arg;

	/*
	 * Next, decide whether we have a loose source
	 * route or a strict source route, and fill in
	 * the begining of the option.
	 */
	if (*cp == '!') {
		cp++;
		*lsrp++ = IPOPT_SSRR;
	} else
		*lsrp++ = IPOPT_LSRR;

	if (*cp != '@')
		return((unsigned long)-1);

	lsrp++;		/* skip over length, we'll fill it in later */
	*lsrp++ = 4;

	cp++;

	sin_addr.s_addr = 0;

	for (c = 0;;) {
		if (c == ':')
			cp2 = 0;
		else for (cp2 = cp; c = *cp2; cp2++) {
			if (c == ',') {
				*cp2++ = '\0';
				if (*cp2 == '@')
					cp2++;
			} else if (c == '@') {
				*cp2++ = '\0';
			} else if (c == ':') {
				*cp2++ = '\0';
			} else
				continue;
			break;
		}
		if (!c)
			cp2 = 0;

		if ((tmp = inet_addr(cp)) != -1) {
			sin_addr.s_addr = tmp;
		} else if (host = gethostbyname(cp)) {
#if	defined(h_addr)
			memcpy((caddr_t)&sin_addr,
				host->h_addr_list[0], host->h_length);
#else
			memcpy((caddr_t)&sin_addr, host->h_addr, host->h_length);
#endif
		} else {
			*cpp = cp;
			return(0);
		}
		memcpy(lsrp, (char *)&sin_addr, 4);
		lsrp += 4;
		if (cp2)
			cp = cp2;
		else
			break;
		/*
		 * Check to make sure there is space for next address
		 */
		if (lsrp + 4 > lsrep)
			return((unsigned long)-1);
	}
	if ((*(*cpp+IPOPT_OLEN) = lsrp - *cpp) <= 7) {
		*cpp = 0;
		*lenp = 0;
		return((unsigned long)-1);
	}
	*lsrp++ = IPOPT_NOP; /* 32 bit word align it */
	*lenp = lsrp - *cpp;
	return(sin_addr.s_addr);
}
#endif

#if	defined(NOSTRNCASECMP)
strncasecmp(p1, p2, len)
register char *p1, *p2;
int len;
{
    while (len--) {
	if (tolower(*p1) != tolower(*p2))
	   return(tolower(*p1) - tolower(*p2));
	if (*p1 == '\0')
	    return(0);
	p1++, p2++;
    }
    return(0);
}
#endif
