#include <sys/types.h>
#include <stdio.h>
#include <errno.h>
#include <sys/wait.h>
#include "dsh.h"

int	errno;		/* global error location */

/* options */
bool	fflg = FALSE;		/* if TRUE fanout stdin */
bool	hflg = FALSE;		/* if TRUE use specified host */
bool	vflg = FALSE;		/* tell hosts */
bool	nflg = FALSE;		/* same as for rsh */
bool	aflg = FALSE;		/* true if all nodes to be used */
bool	sflg = FALSE;		/* like -n for make */

/* files to be rcp'd for input or output */
struct rcpfile {
    char	*r_name;	/* name of the file */
    struct rcpfile *r_next;
};
struct rcpfile	*rcpin = 0;	/* list of files to copy to */
struct rcpfile	*rcpout = 0;	/* list of files to copy from */

char		*av[100];	/* the command */
union wait	pstatus;	/* the return status of the job */
struct hostdef	*thehost;	/* the hosts */
char		*spechost;	/* the specified host */

/* external routines */
int		error();
struct hostdef	*highest();
int		getbids();
double		atof();
bool		aresynonyms();

main(argc, argv)
int	argc;
char	*argv[];
{
    int	inda, indc, ind;
    struct rcpfile *lin, *lout, *next;
    struct hostdef *hp;
    
    /* now worry about the commands */
    for (inda = 1; inda < argc; inda++) {
	
	/* process an opton */
	if (argv[inda][0] == '-') {
	    ind = inda;
	    for (indc = 1; argv[ind][indc] != NULL; indc++) {
		switch (argv[ind][indc]) {

		/* all nodes are specified */
		case 'a':
		    aflg = TRUE;
		    break;

		case 's':
		    sflg = TRUE;
		    break;
		
		/* fanout input */
		case 'f':
		    fflg = TRUE;
		    break;

		/* specify  hosts */
		case 'h':
		    inda++;
		    if (inda < argc) {
			hflg = TRUE;
			spechost = argv[inda];
		    } else {
			fprintf (stderr, "%s: no host after -h\n", argv[0]);
			exit (-1);
		    }
		    break;

		/* specify input files */
		case 'i':
		    inda++;
		    if (inda < argc) {
			next = new (struct rcpfile);
			if (rcpin == 0) {
			    rcpin = next;
			} else {
			    lin->r_next = next;
			}
			lin = next;
			lin->r_next = 0;
			lin->r_name = argv[inda];
		    } else {
			fprintf (stderr, "%s: no input file after -i\n", argv[0]);
			exit (-1);
		    }
		    break;

		/* specify output files */
		case 'o':
		    inda++;
		    if (inda < argc) {
			next = new (struct rcpfile);
			if (rcpout == 0) {
			    rcpout = next;
			} else {
			    lout->r_next = next;
			}
			lout = next;
			lout->r_next = 0;
			lout->r_name = argv[inda];
		    } else {
			fprintf (stderr, "%s: no output file after -o\n", argv[0]);
			exit (-1);
		    }
		    break;

		/* pipe from /dev/null */
		case 'n':
		    nflg = TRUE;
		    break;

		/* tell which machine we're using */
		case 'v':
		    vflg = TRUE;
		    break;

		default:
		    fprintf (stderr, "usage: %s [-anv][-io file] <command>\n", argv[0]);
		    exit (-1);
		}
	    }
	} else {
	    break;
	}
    }

    /* pick up the command */
    for (ind = 0; inda < argc; inda++, ind++) {
	av[ind] = argv[inda];
    }
    av[ind] = 0;

    /* process the defaults file */
    getnodes();

    /* see if anyone wants to bid */
    getbids(av, thehost);

    /* execute the command */
    hp = highest ();
    if (hp == 0) {
	error ("no machine bid for the command");
    }
    if (aflg) {
	do {
	    rexecute (hp);
	    hp = highest ();
	} while (hp != 0);
    } else {
	rexecute (hp);
    }

    if (pstatus.w_T.w_Termsig != 0) {
	fprintf (stderr, "(signal %d)", pstatus.w_T.w_Termsig);
	pstatus.w_T.w_Retcode = 0;
    }
    if (pstatus.w_T.w_Coredump == 1) {
	fprintf (stderr, "(core dumped)\n");
	pstatus.w_T.w_Retcode = 0;
    } 

    exit (pstatus.w_T.w_Retcode);
}

/*
 *	find out which nodes to use 
 */
char *
skipgrey(p)
char	*p;
{
    while (*p == ' ' || *p == '\t' || *p == ',' || *p == ')' || *p == '*')
	p++;
    return (p);
}

char *
token (to, sp)
char *to;
char *sp;
{
    while (*sp != ' ' && *sp != '\t' && *sp != ',' && *sp !=')' && *sp != 0) {
	*to++ = *sp++;
    }
    return (sp);
}

getnodes()
{
    char *sp;
    struct hostdef *last, *next;
    bool account;
    double weight;
    char buf[132];
    int rv;

    thehost = 0;
    rv = getstringrc (".dshrc", "hosts", buf);
    if (rv < 0) {
	rv = getstringrc ("/usr/lib/dshrc", "hosts", buf);
	if (rv < 0) {
	    error ("dsh: no hosts in rc files");
	}
    }

    /* convert to reasonable format */
    sp = buf;
    while (*sp != 0) {
	sp = skipgrey (sp);

	/* get the multiplier */
	weight = 1.0;
	if ((*sp >= '0' && *sp <= '9') || *sp == '.') {
	    weight = atof (sp);
	    for (;*sp != '*' && *sp != 0; sp++);
	    sp = skipgrey (sp);
	}

	if (*sp != 0) {

	    /* allocate some space and chain it in */
	    next = new (struct hostdef);
	    if (thehost == 0) {
		thehost = next;
	    } else {
		last->h_next = next;
	    }
	    last = next;
	    last->h_next = 0;
	    last->h_weight = weight;

	    /* pick up the entry */
	    if (*sp == '(') {
		sp++;
		sp = skipgrey (sp);
		account = TRUE;
	    } else {
		account = FALSE;
	    }
	    sp = token (last->h_name, sp);
	    if (account) {
		sp = skipgrey (sp);
		sp = token (last->h_user, sp);
	    } else {
		*(last->h_user) = 0;
	    }
	}
    }
}

/*
 *	execute a command
 */
execute (argv, block, justtell, ignore)
char	*argv[];	/* the command */
bool	block;		/* if true, block till the command is done */
bool	justtell;	/* true if we shouldn't execute when debuging */
bool	ignore;		/* ignore output */
{
    int		argc;
    int		pid, rv;
    int		status, fd;

    if (sflg) {

	/* just say what we'll do */
	for (argc = 0;argv[argc] != 0; argc++) {
	    printf ("%s ", argv[argc]);
	}
	printf ("\n");
    }
    if (!(justtell && sflg)) {

	/* really do it */
	if (pid = fork()) {
	    if (block) {
		do {
		    rv = wait (&status);
		} while (rv != -1 && rv != pid);
	    }
	} else {
	    if (ignore) {
		fd = open ("/dev/null", 2);
		dup2 (fd, 1);
		dup2 (fd, 2);
	    }
	    execvp (argv[0], argv);
	    _exit (0);
	}
    }
}

/*
 *	remotely execute the command
 */
rexecute (hp)
struct hostdef	*hp;		/* the host to execute on */
{
    struct rcpfile *fp;
    int		rv;
    int		argc, ac;
    char	*argv[200];
    char	mydir[PATHSIZE];
    bool	local;
	
    if (vflg || aflg) {
	fprintf (stderr, ">>%s<<\n", hp->h_name);
    }
    argc = 0;
    local = aresynonyms (hp->h_name, myhostname());
    if (!local) {
	/* get our directory if we're going to copy files */
	if (rcpin != 0 || rcpout != 0) {
	    getwd (mydir);
	}

	/* make the directory we're going to use */
	argv[argc++] = "(";
	argv[argc++] = "mkdir";
	argv[argc++] = hp->h_dir;
	argv[argc++] = ";";

	/* and hop to it */
	argv[argc++] = "cd";
	argv[argc++] = hp->h_dir;
	argv[argc++] = ";";

	/* copy over any files */
	if (rcpin != 0) {
	    argv[argc++] = "rcp";
	    for (fp = rcpin; fp != 0; fp = fp->r_next) {
		argv[argc] = (char *) malloc (HOSTNAMESIZE+2*PATHSIZE);
		if (fp->r_name[0] == '/' || fp->r_name[0] == '~') {
		    sprintf (argv[argc++], "%s:%s", myhostname(), fp->r_name);
		} else {
		    sprintf (argv[argc++], "%s:%s/%s", myhostname(),
			mydir, fp->r_name);
		}
	    }
	    argv[argc++] = ".";
	    argv[argc++] = ";";
	}
    }

    /* execute the command */
    for (ac = 0; av[ac] != 0; ac++) {
	argv[argc++] = av[ac];
    }
    argv[argc++] = ";";

    if (!local) {

	/* copy back any files */
	if (rcpout != 0) {
	    argv[argc++] = "rcp";
	    for (fp = rcpout; fp != 0; fp = fp->r_next) {
		argv[argc++] = fp->r_name;
	    }
	    argv[argc] = (char *) malloc (HOSTNAMESIZE+2*PATHSIZE);
	    sprintf (argv[argc++], "%s:%s", myhostname(), mydir);
	    argv[argc++] = ";";
	}

	/* clean up the directory */
	argv[argc++] = "cd";
	argv[argc++] = "..";
	argv[argc++] = ";";
	argv[argc++] = "/bin/rm";
	argv[argc++] = "-fr";
	argv[argc++] = hp->h_dir;
	argv[argc++] = ")";
    }
    argv[argc] = 0;
    
    rshell (hp, argv, TRUE, nflg, TRUE, FALSE);
}

rshell (hp, av, block, usenflg, justtell, ignore)
struct hostdef	*hp;		/* all about the host */
char		*av[];		/* the command */
bool		block;		/* true if we should block */
bool		usenflg;	/* true if we should use the n flag */
bool		justtell;	/* true if we shouldn't execute when debuging */
bool		ignore;		/* ignore the output from the command */
{
    int		rv;
    int		argc, ac;
    char	*argv[100];
    char	command[256];
    char	*p, *p1;
    bool	local;
	
    argc = 0;
    local = aresynonyms (hp->h_name, myhostname());
    if (local) {
	argv[argc++] = "csh";
	argv[argc++] = "-c";
	argv[argc++] = command;
	p = command;
	for (ac = 0; av[ac] != 0; ac++) {
	    *p++ = ' ';
	    for (p1 = av[ac]; *p1 != 0;){
		*p++ = *p1++;
	    }
	}
	*p = 0;
    } else {
	argv[argc++] = "rsh";
	argv[argc++] = hp->h_name;
	if (usenflg) {
	    argv[argc++] = "-n";
	}
	if (*(hp->h_user) != 0) {
	    argv[argc++] = "-l";
	    argv[argc++] = hp->h_user;
	}
	for (ac = 0; av[ac] != 0; ac++) {
	    argv[argc++] = av[ac];
	}
    }
    argv[argc] = 0;
    execute (argv, block, justtell, ignore);
}
