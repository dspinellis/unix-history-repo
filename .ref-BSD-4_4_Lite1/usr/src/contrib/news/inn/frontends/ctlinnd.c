/*  $Revision: 1.24 $
**
**  Send control messages to the InterNetNews daemon.
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "libinn.h"
#include "clibrary.h"
#include "inndcomm.h"
#include "paths.h"
#include "macros.h"


/*
**  Datatype for an entry in the command table.
*/
typedef struct _COMMAND {
    STRING	Command;
    STRING	Text;
    int		argc;
    char	Letter;
    BOOL	Glue;
} COMMAND;


STATIC COMMAND	Commands[] = {
    {	"addhist",	"id arr exp post path...\tAdd history line",
	5,	SC_ADDHIST,	TRUE	},
    {	"allow",	"reason...\t\t\tAllow remote connections",
	1,	SC_ALLOW,	TRUE	},
    {	"begin",	"site\t\t\tStart newly-added site",
	1,	SC_BEGIN,	FALSE	},
    {	"cancel",	"id\t\t\tCancel message locally",
	1,	SC_CANCEL,	FALSE	},
    {	"changegroup",	"group rest\tChange mode of group",
	2,	SC_CHANGEGROUP,	FALSE	},
    {	"checkfile",	"\t\t\tCheck syntax of newsfeeds file",
	0,	SC_CHECKFILE,	FALSE	},
    {	"drop",		"site\t\t\tStop feeding site",
	1,	SC_DROP,	FALSE		},
    {	"flush",	"site\t\t\tFlush feed for site*",
	1,	SC_FLUSH,	FALSE	},
    {	"flushlogs",	"\t\t\tFlush log files",
	0,	SC_FLUSHLOGS,	FALSE	},
    {	"go",		"reason...\t\t\tRestart after pause or throttle",
	1,	SC_GO,		TRUE	},
    {	"hangup",	"channel\t\tHangup specified incoming channel",
	1,	SC_HANGUP,	FALSE	},
    {	"mode",		"\t\t\t\tPrint operating mode",
	0,	SC_MODE,	FALSE		},
    {	"name",		"nnn\t\t\tPrint name of specified channel*",
	1,	SC_NAME,	FALSE		},
    {	"newgroup",	"group rest creator\tCreate new group",
	3,	SC_NEWGROUP,	FALSE	},
    {	"param",	"letter value\t\tChange command-line parameters",
	2,	SC_PARAM,	FALSE	},
    {	"pause",	"reason...\t\tShort-term pause in accepting articles",
	1,	SC_PAUSE,	TRUE	},
    {	"readers",	"flag text...\t\tEnable or disable newsreading",
	2,	SC_READERS,	TRUE	},
    {	"refile",	"path group\t\tRefile an article",
	2,	SC_REFILE,	FALSE	},
    {	"reject",	"reason...\t\t\tReject remote connections",
	1,	SC_REJECT,	TRUE	},
    {	"reload",	"what reason...\t\tRe-read config files*",
	2,	SC_RELOAD,	TRUE	},
    {	"renumber",	"group\t\tRenumber the active file*",
	1,	SC_RENUMBER,	FALSE	},
    {	"reserve",	"reason...\t\tReserve the next pause or throttle",
	1,	SC_RESERVE,	TRUE	},
    {	"rmgroup",	"group\t\t\tRemove named group",
	1,	SC_RMGROUP,	FALSE	},
    {	"send",		"feed text...\t\tSend text to exploder feed",
	2,	SC_SEND,	TRUE	},
    {	"shutdown",	"reason...\t\tShut down server",
	1,	SC_SHUTDOWN,	TRUE	},
    {	"kill",	"signal site\t\tSend signal to site's process",
	2,	SC_SIGNAL,	FALSE	},
    {	"throttle",	"reason...\t\tStop accepting articles",
	1,	SC_THROTTLE,	TRUE	},
    {	"trace",	"innd|#|nnrpd flag\tTurn tracing on or off",
	2,	SC_TRACE,	FALSE	},
    {	"xabort",	"text...\t\tAbort the server",
	1,	SC_XABORT,	TRUE	},
    {	"xexec",	"path\t\t\tExec new server",
	1,	SC_XEXEC,	FALSE	}
};



/*
**  Print a help summary.
*/
STATIC NORETURN
Help(p)
    char		*p;
{
    register COMMAND	*cp;

    if (p == NULL) {
	(void)printf("Command summary:\n");
	for (cp = Commands; cp < ENDOF(Commands); cp++)
	    (void)printf("  %s %s\n", cp->Command, cp->Text);
	(void)printf("*   Empty string means all sites/groups/etc.\n");
	(void)printf("... All trailing words are glued together.\n");
	exit(0);
    }
    for (cp = Commands; cp < ENDOF(Commands); cp++)
	if (EQ(p, cp->Command)) {
	    (void)printf("Command usage:\n");
	    (void)printf("  %s %s\n", cp->Command, cp->Text);
	    exit(0);
	}
    (void)printf("No such command.\n");
    exit(0);
}


/*
**  Print a command-usage message and exit.
*/
STATIC NORETURN
WrongArgs(cp)
    COMMAND	*cp;
{
    (void)printf("Wrong number of arguments -- usage:\n");
    (void)printf("  %s %s\n", cp->Command, cp->Text);
    exit(1);
}


/*
**  Print an error message and exit.
*/
STATIC NORETURN
Failed(p)
    char	*p;
{
    if (ICCfailure)
	(void)fprintf(stderr, "Can't %s (%s failure) %s.\n",
		p, ICCfailure, strerror(errno));
    else
	(void)fprintf(stderr, "Can't %s, %s.\n", p, strerror(errno));
    (void)ICCclose();
    exit(1);
}


/*
**  Print an error reporting incorrect usage.
*/
STATIC NORETURN
Usage(what)
    char	*what;
{
    (void)fprintf(stderr, "Usage error (%s) -- try -h for help.\n", what);
    exit(1);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    static char		Y[] = "y";
    static char		EMPTY[] = "";
    register COMMAND	*cp;
    register char	*p;
    register int	i;
    BOOL		Silent;
    BOOL		NeedHelp;
    char		*reply;
    char		*new;
    int			length;
    char		*nv[4];
    struct stat		Sb;
    char		buff[SMBUF];

    /* Set defaults. */
    Silent = FALSE;
    NeedHelp = FALSE;
    ICCsettimeout(CTLINND_TIMEOUT);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "hst:")) != EOF)
	switch (i) {
	default:
	    Usage("bad flags");
	    /* NOTREACHED */
	case 'h':		/* Get help			*/
	    NeedHelp = TRUE;
	    break;
	case 's':		/* Silent -- no output		*/
	    Silent = TRUE;
	    break;
	case 't':		/* Time to wait for reply	*/
	    ICCsettimeout(atoi(optarg));
	    break;
	}
    ac -= optind;
    av += optind;
    if (NeedHelp)
	Help(av[0]);
    if (ac == 0)
	Usage("missing command");

    /* Look up the command word and move to the arguments. */
    if (EQ(av[0], "help"))
	Help(av[1]);
    for (cp = Commands; cp < ENDOF(Commands); cp++)
	if (EQ(av[0], cp->Command))
	    break;
    if (cp == ENDOF(Commands))
	Usage("unknown command");
    ac--;
    av++;

    /* Check argument count. */
    if (cp->Letter == SC_NEWGROUP) {
	/* Newgroup command has defaults. */
	switch (ac) {
	default:
	    WrongArgs(cp);
	    /* NOTREACHED */
	case 1:
	    nv[0] = av[0];
	    nv[1] = Y;
	    nv[2] = EMPTY;
	    nv[3] = NULL;
	    av = nv;
	    break;
	case 2:
	    nv[0] = av[0];
	    nv[1] = av[1];
	    nv[2] = EMPTY;
	    nv[3] = NULL;
	    av = nv;
	    break;
	case 3:
	    break;
	}
	switch (av[1][0]) {
	default:
	    Usage("Bad group mode");
	    /* NOTREACHED */
	case NF_FLAG_ALIAS:
	case NF_FLAG_EXCLUDED:
	case NF_FLAG_MODERATED:
	case NF_FLAG_OK:
	case NF_FLAG_NOLOCAL:
	case NF_FLAG_IGNORE:
	    break;
	}
	ac = 3;
    }
    else if (ac > cp->argc && cp->Glue) {
	/* Glue any extra words together. */
	for (length = 0, i = cp->argc - 1; (p = av[i++]) != NULL; )
	    length += strlen(p) + 1;
	for (new = p = NEW(char, length), i = cp->argc - 1; av[i]; i++) {
	    if (i >= cp->argc)
		*p++ = ' ';
	    p += strlen(strcpy(p, av[i]));
	}
	av[cp->argc - 1] = new;
	av[cp->argc] = NULL;
    }
    else if (ac != cp->argc)
	/* All other commands must have the right number of arguments. */
	WrongArgs(cp);

    /* Make sure there are no separators in the parameters. */
    for (i = 0; (p = av[i++]) != NULL; )
	if (strchr(p, SC_SEP) != NULL) {
	    (void)fprintf(stderr, "Illegal character '\\%03o' in \"%s\"\n",
		    SC_SEP, p);
	    exit(1);
	}

    /* Do the real work. */
    if (ICCopen() < 0)
	Failed("setup communication");
    i = ICCcommand(cp->Letter, av, &reply);
    if (i < 0) {
	i = errno;
	if (stat(_PATH_SERVERPID, &Sb) < 0)
	    (void)fprintf(stderr, "No innd.pid file; did server die?\n");
	(void)sprintf(buff, "send \"%s\" command", cp->Command);
	errno = i;
	Failed(buff);
    }

    if (reply) {
	/* Skip "<exitcode><space>" part of reply. */
	for (p = reply; *p && CTYPE(isdigit, *p); p++)
	    continue;
	while (*p && ISWHITE(*p))
	    p++;
	if (i != 0)
	    (void)fprintf(stderr, "%s\n", p);
	else if (!Silent)
	    (void)printf("%s\n", p);
    }

    if (ICCclose() < 0)
	Failed("end communication");

    exit(i);
    /* NOTREACHED */
}
