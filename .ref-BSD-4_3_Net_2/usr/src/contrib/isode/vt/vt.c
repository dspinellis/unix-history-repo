/* vt.c - VT initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/vt/RCS/vt.c,v 7.4 91/02/22 09:48:23 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/vt/RCS/vt.c,v 7.4 91/02/22 09:48:23 mrose Interim $
 *
 *
 * $Log:	vt.c,v $
 * Revision 7.4  91/02/22  09:48:23  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/12/23  18:43:29  mrose
 * update
 * 
 * Revision 7.2  90/01/11  18:38:13  mrose
 * real-sync
 * 
 * Revision 7.1  89/11/30  23:51:38  mrose
 * pa2str
 * 
 * Revision 7.0  89/11/23  22:31:49  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <signal.h>
#include "vtpm.h"
#include "sector1.h"
#include "tailor.h"

#include <sys/ioctl.h>
#ifdef BSD44
#include <sys/termios.h>
#endif
#include <ctype.h>
#include <setjmp.h>
#include <varargs.h>

#define	strip(x)	((x)&0177)
#define TBUFSIZ		1024

char	ttyobuf[TBUFSIZ], *tfrontp = ttyobuf, *tbackp = ttyobuf;
char	netobuf[BUFSIZ], *nfrontp = netobuf, *nbackp = netobuf;

int	connected;
int	net;
int	showoptions = 0;
int	options;
int	debug = 0;
int	crmod = 0;
char	escape = ']' & 037;
static char *escapestr = "^]";

VT_PROFILE vtp_profile;
int rflag = 0;
char erase_char;	/*Unix Erase*/
char erase_line;	/*Unix Kill*/
char intr_char;		/*Unix Interrupt*/
char *my_displayobj = "K";	/*Initiator's Display Object Name*/
char *my_echo_obj = "NI";	/*Initiator's Negotiation Control Object*/
char *my_signal_obj = "KB";	/*Initiator's Signal Control Object*/
char *his_echo_obj = "NA";	/*Acceptor's Negotiation Control Object*/
char *his_signal_obj = "DI";	/*Acceptor's Signal Control Object*/
int my_right = INITIATOR;
char kb_image;			/*KB Control Object Image*/
char di_image;			/*DI Control Image*/
char ni_image;			/*NI Control Object Image*/
char na_image;			/*NA Control Object Image*/
char nego_state;		/*Current state of NI affected options*/
char sync_image;		/*SY Control Object*/
char ga_image;
int transparent = 0;		/*Transparent Repertoire switch*/
int telnet_profile = 1;
int do_break = 1;		/*If VT-BREAK Functional Unit agreed to*/
char *myhostname;
char peerhost[BUFSIZ];
struct PSAPaddr ts_bound;
int pty;			/*Kludge for single map.c (sorry)*/

char	line[BUFSIZ];

jmp_buf	toplevel;
jmp_buf	peerdied;

extern	int errno;


struct dispatch {
    char   *ds_name;
    IFP	    ds_fnx;

    int	    ds_flags;
#define	DS_NULL		0x00
#define	DS_OPEN		0x01	/* association required */
#define	DS_CLOSE	0x02	/* association avoided */

    char   *ds_help;
};

struct dispatch *getds ();


int	vt_open (), vt_close (), vt_quit (), vt_status (), vt_suspend ();
int	vt_ayt (), vt_break (), vt_escape ();
int	vt_set (), vt_help ();


static struct dispatch dispatches[] = {
    "ayt", vt_ayt, DS_OPEN,
    "send \"are you there?\"",

    "break", vt_break, DS_OPEN,
    "send break",

    "close", vt_close, DS_OPEN,
    "release association with terminal service",

    "escape", vt_escape, DS_NULL,
    "set escape character (depreciated)",

    "help", vt_help, DS_NULL,
    "print help information",

    "open", vt_open, DS_CLOSE,
    "associate with terminal service",

    "quit", vt_quit, DS_NULL,
    "release association with terminal service and exit",

    "set", vt_set, DS_NULL,
    "display or change variables",

    "status", vt_status, DS_OPEN,
    "show current status",

    "suspend", vt_suspend, DS_OPEN,
    "suspend vtp",

    NULL
};


SFD	intr(), deadpeer();
char	*control(), *strdup ();

#ifdef BSD44
struct	termios oterm;
#else
struct	tchars otc;
struct	ltchars oltc;
struct	sgttyb ottyb;
#endif

static int runcom = 0;
char	*myname;
static char *myhome;
int	tmode();

LLog    _vt_log = {
    "./vt.log", NULLCP, NULLCP,
    LLOG_NONE, LLOG_NONE, -1, LLOGCLS | LLOGCRT | LLOGZER, NOTOK 
};
LLog   *vt_log = &_vt_log;

main(argc, argv)
	int argc;
	char *argv[];
{
	int	i,
		fflag;
	char   *logname,
		buffer[BUFSIZ],
	       *vec[NVEC + 1];
	FILE   *fp;

    if (myname = rindex (*argv, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *argv;

    isodetailor (myname, 1);

    ll_hdinit (vt_log, myname);

	fflag = 0;
	logname = 0;
	myhostname = PLocalHostName ();
	peerhost[0] = NULL;
	acc = &accs;
	acr = &acrs;
	aci = &acis;

#ifdef BSD44
	if (tcgetattr(0, &oterm) == -1)
		perror("tcgetattr");
	erase_char = oterm.c_cc[VERASE];
	erase_line = oterm.c_cc[VKILL];
	intr_char = oterm.c_cc[VINTR];
#else
	if (ioctl(0, TIOCGETP, (char *)&ottyb) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed");
	}
	if (ioctl(0, TIOCGETC, (char *)&otc) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed");
	}
	if (ioctl(0, TIOCGLTC, (char *)&oltc) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed");
	}
	erase_char = ottyb.sg_erase;
	erase_line = ottyb.sg_kill;
	intr_char = otc.t_intrc;
#endif


	setbuf(stdin, NULLCP);
	setbuf(stdout, NULLCP);

	bzero ((char *) &vtp_profile, sizeof vtp_profile);
	vtp_profile.profile_name = "telnet";
	vtp_profile.arg_val.tel_arg_list.x_window = ncols (stdin);
	vtp_profile.arg_val.tel_arg_list.full_ascii = 1;

	for(i=1; i<argc; i++)
	{
		if (peerhost[0] == NULL && (*argv[i] != '-'))
		{
			(void) strcpy(peerhost,argv[i]);
		}
		else if(!strcmp(argv[i], "-g"))
		{
			vtp_profile.arg_val.tel_arg_list.full_ascii = 0;
			advise(LLOG_DEBUG,NULLCP,"using ASCII GO repertoire");
		}
		else if(!strcmp(argv[i], "-D"))
		{
			vtp_profile.profile_name = "default";
			telnet_profile = 0;
			my_displayobj = "DISPLAY-OBJECT-2";
			advise(LLOG_DEBUG,NULLCP,"using default profile");
		}
		else if(!strcmp(argv[i],"-B"))
		{
			advise(LLOG_DEBUG,NULLCP,"VT-BREAK not chosen");
			do_break = 0;
		}
		else if(!strcmp(argv[i], "-f"))
		    fflag++;
		else if(!strcmp(argv[i], "-F"))
		{
			if ((logname = argv[++i]) == NULL || *logname == '-')
			    adios (NULLCP, "usage: %s -F logfile", myname);
			vt_log -> ll_file = logname;
			(void) ll_close (vt_log);
			advise(LLOG_DEBUG,NULLCP, "logging to %s",logname);
		}
		else
		    adios("usage: %s [-g] [-D] [-B] [-f] [-F logfile] [hostname]",
			  myname);
	}


    runcom = 1;

    rcinit ();
    (void) sprintf (buffer, "%s/.vtrc", myhome);
    if (!fflag && (fp = fopen (buffer, "r"))) {
	register char   *bp;
	
	while (fgets (buffer, sizeof buffer, fp)) {
	    if (bp = index (buffer, '\n'))
		*bp = NULL;

	    bzero ((char *) vec, sizeof vec);
	    if (str2vec (buffer, vec) < 1)
		continue;

	    if (vtploop (vec, NOTOK) == NOTOK && peerhost[0])
		exit (1);
	}

	(void) fclose (fp);
    }

    runcom = 0;

	if (peerhost[0] != NULL) {
		if (setjmp(toplevel) != 0)
			exit(0);
		do_vt();
	}
	(void) setjmp(toplevel);
	for (;;)
		command(1);
}

/*    DISPATCH */

command(top)
	int top;
{
	int eof,oldmode;
	char *vec[NVEC + 1];

	oldmode = tmode(0);
	if (!top)
		(void) putchar('\n');
	else
	    (void) signal (SIGINT, SIG_DFL);
	eof = 0;
	for (;;) {
		if (getline ("%s> ", line) == NOTOK) {
		    if (eof) {
			if (!connected)
			    exit (0);
			(void) vt_status (NULLVP);
			break;
		    }

		    eof = 1;
		    continue;
		}
		eof = 0;

		bzero ((char *) vec, sizeof vec);
		if (str2vec (line, vec) < 1)
		    break;

		if (vtploop (vec, NOTOK) != DONE)
		    break;
	}
	if (!top) {
		if (!connected)
			longjmp(toplevel, 1);
		(void) fflush (stdout);
		(void) fflush (stderr);
		(void) tmode(oldmode);
	}
}

/*  */

static int vtploop (vec, error)
char  **vec;
int	error;
{
    register struct dispatch *ds;

    if ((ds = getds (strcmp (*vec, "?") ? *vec : "help")) == NULL)
	return error;

    if (!connected) {
	if (ds -> ds_flags & DS_OPEN) {
	    advise (LLOG_NOTICE,NULLCP,  "not associated with terminal service");
	    return error;
	    }
    }
    else
	if (ds -> ds_flags & DS_CLOSE) {
	    advise (LLOG_NOTICE,NULLCP, 
		    "already associated with terminal service");
	    return error;
	}

    switch ((*ds -> ds_fnx) (vec)) {
	case NOTOK:
	    return error;

	case OK:
	default:
	    return OK;

	case DONE:
	    return DONE;
	}
}

/*  */

int	getline (prompt, buffer)
char   *prompt,
       *buffer;
{
    register int    i;
    register char  *cp,
                   *ep;
    static int  sticky = 0;

    if (sticky) {
	sticky = 0;
	return NOTOK;
    }

    (void)printf (prompt, connected ? peerhost : myname);
    (void) fflush (stdout);

    for (ep = (cp = buffer) + BUFSIZ - 1; (i = getchar ()) != '\n';) {
	if (i == EOF) {
	    (void)printf ("\n");
	    clearerr (stdin);
	    if (cp == buffer)
		return NOTOK;

	    sticky++;
	    break;
	}

	if (cp < ep)
	    *cp++ = i;
    }
    *cp = NULL;
    
    return OK;
}

/*  */

struct dispatch *getds (name)
register char *name;
{
    register int    longest,
                    nmatches;
    register char  *p,
                   *q;
    char    buffer[BUFSIZ];
    register struct dispatch   *ds,
                               *fs;

    longest = nmatches = 0;
    for (ds = dispatches; p = ds -> ds_name; ds++) {
	for (q = name; *q == *p++; q++)
	    if (*q == NULL)
		return ds;
	if (*q == NULL)
	    if (q - name > longest) {
		longest = q - name;
		nmatches = 1;
		fs = ds;
	    }
	    else
		if (q - name == longest)
		    nmatches++;
    }

    switch (nmatches) {
	case 0: 
	    advise (LLOG_NOTICE,NULLCP,  "unknown operation \"%s\"", name);
	    return NULL;

	case 1: 
	    return fs;

	default: 
	    for (ds = dispatches, p = buffer; q = ds -> ds_name; ds++)
		if (strncmp (q, name, longest) == 0) {
		    (void) sprintf (p, "%s \"%s\"", p != buffer ? "," : "", q);
		    p += strlen (p);
		}
	    advise (LLOG_NOTICE,NULLCP, 
		    "ambiguous operation, it could be one of:%s",
			buffer);
	    return NULL;
    }
}

/*    OPERATIONS */

static int  vt_open (vec)
char  **vec;
{
    if (*++vec == NULL) {
	if (getline ("host: ", line) == NOTOK
	        || str2vecX (line, vec, 0, NULLIP, NULL, 0) < 1)
	    return NOTOK;
    }

    (void) strcpy (peerhost, *vec);
    do_vt ();

    return OK;
}


do_vt()
{
	(void) signal(SIGINT, intr);
	(void) signal(SIGPIPE, deadpeer);
	(void)printf("Trying...\n");
	(void)fflush(stdout);

	if ((fd = con_req()) < 0)
	    return;

	connected++;
	(void) vt_status (NULLVP);
	(void)printf ("escape character is '%s'\n", escapestr);
	if (setjmp(peerdied) == 0)
		vt(fd);
	adios (NULLCP, "association terminated by peer");
}

/*  */

/* ARGSUSED */

static int  vt_close (vec)
char  **vec;
{
    (void) tmode(0);
    vrelreq();
    if (getch () >= -1) {
	advise (LLOG_DEBUG,NULLCP,  "flushing input queue...");
	while (getch () >= -1)
	    continue;
    }

    /* read network events until the release sequence reached
       the point where the other side shuts down
     */

    (void)printf ("association released\n");
    (void)fflush (stdout);
    connected = 0;
    /* reset his options */

    return OK;
}

/*  */

/* ARGSUSED */

static int  vt_quit (vec)
char  *vec;
{
    if (connected)
	(void) vt_close (NULLVP);

    exit(0);	/* NOTREACHED */
}

/*  */

/* ARGSUSED */

static int  vt_status (vec)
char  **vec;
{
    (void) printf ("associated with terminal service on \"%s\"\n  at %s\n",
		   peerhost, pa2str (&ts_bound));
    (void) printf ("  using %s profile\n", vtp_profile.profile_name);

    return OK;
}

/*  */

/* ARGSUSED */

static int  vt_suspend (vec)
char  **vec;
{
	register int save;

	save = tmode(0);
	(void)kill(0, SIGTSTP);

	/* reget parameters in case they were changed */
#ifdef BSD44
	if (tcgetattr(0, &oterm) == -1)
		perror("tcgetattr");
#else
	if (ioctl(0, TIOCGETP, (char *)&ottyb) == -1) {
		perror("ioctl"); 
		adios(NULLCP, "ioctl failed");
	}
	if (ioctl(0, TIOCGETC, (char *)&otc) == -1) {
		perror("ioctl"); 
		adios(NULLCP, "ioctl failed"); 
	} 
	if (ioctl(0, TIOCGLTC, (char *)&oltc) == -1) { 
		perror("ioctl"); 
		adios(NULLCP, "ioctl failed");
	}
#endif
	(void) tmode(save);

	return OK;
}

/*  */

static int  vt_escape (vec)
char  **vec;
{
    char   c;

    if (*++vec == NULL) {
	if (getline ("new escape character: ", line) == NOTOK
	        || str2vec (line, vec) < 1)
	    return NOTOK;
    }

    if ((c = *vec[0]) != NULL) {
	char   *cp = control (escape = c);

	free (escapestr);
	escapestr = strdup (cp);
    }
    (void)printf ("escape character is '%s'\n", escapestr);

    return OK;
}

/*    VARIABLES */

static char *debug_val[] = {
	"0", "1", "2", "3", "4", "5", "6", "7", NULL
};

static char *bool[] = {
    "off", "on", NULL
};

static char *emodes[] = {
    "local", "remote", NULL
};

static char *rmodes[] = {
    "ascii", "transparent", NULL
};

static char *xsaplevels[] = {
    "none", "fatal", "exceptions", "notice", "pdus", "trace", "debug", NULL
};


struct var {
    char   *v_name;
    IP	    v_value;

    char   *v_dname;
    char  **v_dvalue;
    char   *v_mask;

    IFP	    v_hook;
};

struct var *getvar ();


static int   echo = 0;
static int   repertoire = 0;
static int   verbose = 0;

int	set_debug (), set_echo (), set_escape (), set_repertoire ();


static struct var vars[] = {
    "acsaplevel", &_acsap_log.ll_events, "ACSAP logging", xsaplevels,
	LLOG_MASK, NULLIFP,
    "acsapfile", NULLIP, "ACSAP trace file", &_acsap_log.ll_file, NULLCP,
	NULLIFP,

    "addrlevel", &_addr_log.ll_events, "address logging", xsaplevels,
	LLOG_MASK, NULLIFP,
    "addrfile", NULLIP, "address trace file", &_addr_log.ll_file, NULLCP,
	NULLIFP,

    "compatlevel", &_compat_log.ll_events, "COMPAT logging", xsaplevels,
	LLOG_MASK, NULLIFP,
    "compatfile", NULLIP, "COMPAT trace file", &_compat_log.ll_file, NULLCP,
	NULLIFP,

    "crmod", &crmod, "map CR on output", bool, NULLCP, NULLIFP,

    "debug", &debug, "debug VT", debug_val, NULLCP, set_debug,

    "echo", &echo, "local or remote echoing", emodes, NULLCP, set_echo,

    "escape", NULLIP, "escape character", &escapestr, NULLCP, set_escape,

    "options", &showoptions, "show option processing", bool, NULLCP, NULLIFP,

    "psaplevel", &_psap_log.ll_events, "PSAP logging", xsaplevels,
	LLOG_MASK, NULLIFP,
    "psapfile", NULLIP, "PSAP trace file", &_psap_log.ll_file, NULLCP,
	NULLIFP,

    "psap2level", &_psap2_log.ll_events, "PSAP2 logging", xsaplevels,
	LLOG_MASK, NULLIFP,
    "psap2file", NULLIP, "PSAP2 trace file", &_psap2_log.ll_file, NULLCP,
	NULLIFP,

    "repertoire", &repertoire, "terminal repertoire", rmodes, NULLCP,
	set_repertoire,

    "ssaplevel", &_ssap_log.ll_events, "SSAP logging", xsaplevels,
	LLOG_MASK, NULLIFP,
    "ssapfile", NULLIP, "SSAP trace file", &_ssap_log.ll_file, NULLCP,
	NULLIFP,

    "tracelevel", &_vt_log.ll_events, "VT logging", xsaplevels,
	LLOG_MASK, NULLIFP,
    "tracefile", NULLIP, "VT trace file", &_vt_log.ll_file, NULLCP,
	NULLIFP,

    "tsaplevel", &_tsap_log.ll_events, "TSAP logging", xsaplevels,
	LLOG_MASK, NULLIFP,
    "tsapfile", NULLIP, "TSAP trace file", &_tsap_log.ll_file, NULLCP,
	NULLIFP,

    "verbose", &verbose, "verbose interaction", bool, NULLCP, NULLIFP,

    NULL
};


static int varwidth1;
static int varwidth2;

char    **getval ();

/*  */

static int  vt_set (vec)
char  **vec;
{
    register int    i,
		    j;
    int     value,
	    vflag;
    register char **cp,
		   *dp;
    register struct var *v;

    if (*++vec == NULL) {
	register int    w;
	int     columns,
	        width,
	        lines;
	register struct var *u;

	for (u = vars; u -> v_name; u++)
	    continue;
	width = varwidth1;

	if ((columns = ncols (stdout) / (width = (width + 8) & ~7)) == 0)
	    columns = 1;
	lines = ((u - vars) + columns - 1) / columns;

	(void)printf ("Variables:\n");
	for (i = 0; i < lines; i++)
	    for (j = 0; j < columns; j++) {
		v = vars + j * lines + i;
		(void)printf ("%s", v -> v_name);
		if (v + lines >= u) {
		    (void)printf ("\n");
		    break;
		}
		for (w = strlen (v -> v_name); w < width; w = (w + 8) & ~7)
		    (void) putchar ('\t');
	    }

	return DONE;
    }

    echo = (nego_state & ECHO_OBJ) ? 1 : 0;
    repertoire = transparent ? 1 : 0;

    if (strcmp (*vec, "?") == 0) {
	for (v = vars; v -> v_name; v++)
	    printvar (v);

	return DONE;
    }

    if ((v = getvar (*vec)) == NULL)
	return DONE;

    if (*++vec == NULL) {
	printvar (v);

	return DONE;
    }

    if (strcmp (*vec, "?") == 0) {
	if (v -> v_value && (cp = v -> v_dvalue)) {
	    printf ("use %s of:", v -> v_mask ? "any" : "one");
	    for (i = 0; *cp; cp++)
		printf ("%s \"%s\"", i++ ? "," : "", *cp);
	    if (v -> v_mask)
		printf (";\n\tor  \"all\";\n\tor a hexadecimal number from 0 to 0x%x\n",
		    (1 << (i - 1)) - 1);
	    else
		printf (";\n\tor a number from 0 to %d\n",
		    cp - v -> v_dvalue - 1);
	}
	else
	    (void)printf ("use any %s value\n",
		    v -> v_value ? "integer" : "string");

	return DONE;
    }

    if (v -> v_value == NULLIP) {
	register int    w;

	if (*v -> v_dvalue)
	    free (*v -> v_dvalue);
	*v -> v_dvalue = strdup (*vec);
	if ((w = strlen (*v -> v_dvalue) + 2) > varwidth2)
	    varwidth2 = w;
	if (v -> v_hook)
	    (*v -> v_hook) (v);
	if (verbose)
	    printvar (v);
	return DONE;
    }

    if (v -> v_mask) {
	if (strcmp (dp = *vec, "all") == 0 && (cp = v -> v_dvalue)) {
	    i = 1;
	    while (*++cp)
		i <<= 1;
	    value = i - 1;
	    j = 1;
	}
	else {
	    if (strncmp (dp = *vec, "0x", 2) == 0)
		dp += 2;
	    for (j = sscanf (dp, "%x", &value); *dp; dp++)
		if (!isxdigit (*dp)) {
		    j = 0;
		    break;
		}
	}
    }
    else
	j = sscanf (*vec, "%d", &value);

    if (j == 1) {
	if (cp = v -> v_dvalue) {
	    if (v -> v_mask) {
		i = 1;
		while (*++cp)
		    i <<= 1;
		if (value >= i)
		    goto out_of_range;
	    }
	    else {
		for (; *cp; cp++)
		    continue;
		if (value >= cp - v -> v_dvalue) {
out_of_range: ;
		    advise (LLOG_NOTICE,NULLCP, 
			    "value out of range \"%s\"", *vec);

		    return DONE;
		}
	    }
	}

	vflag = verbose;
	*v -> v_value = value;
	if (v -> v_hook)
	    (*v -> v_hook) (v);
	if (vflag)
	    printvar (v);

	return DONE;
    }

    if (v -> v_mask) {
	i = 0;
	for (; *vec; vec++) {
	    if (!(cp = getval (*vec, v -> v_dvalue))) {
		advise (LLOG_NOTICE,NULLCP,  "bad value \"%s\"", *vec);

		return DONE;
	    }
	    if ((j = cp - v -> v_dvalue) <= 0)
		continue;

	    i |= 1 << (j - 1);
	}

	vflag = verbose;
	*v -> v_value = i;
	if (v -> v_hook)
	    (*v -> v_hook) (v);
	if (vflag)
	    printvar (v);

	return DONE;
    }

    if (v -> v_dvalue && (cp = getval (*vec, v -> v_dvalue))) {
	vflag = verbose;
	*v -> v_value = cp - v -> v_dvalue;
	if (v -> v_hook)
	    (*v -> v_hook) (v);
	if (vflag)
	    printvar (v);
    }
    else
	if (!v -> v_dvalue)
	    advise (LLOG_NOTICE,NULLCP,  "bad value \"%s\"", *vec);

    return DONE;
}

/*  */

static printvar (v)
register struct var *v;
{
    int	    i;
    char    buffer[BUFSIZ];

    if (runcom)
	return;

    (void)printf ("%-*s = ", varwidth1, v -> v_name);
    if (v -> v_value) {
	i = *v -> v_value;

	if (v -> v_mask) {
	    if (v -> v_dvalue) {
		if (i == 0)
		    (void)printf ("%-*s", varwidth2, v -> v_dvalue[i]);
		else {
		    (void) strcpy (buffer, sprintb (i, v -> v_mask));
		    if (strlen (buffer) <= varwidth2)
			(void)printf ("%-*s", varwidth2, buffer);
		    else
			(void)printf ("%s\n%*s", buffer, varwidth1 + varwidth2 + 3,
				"");
		}
	    }
	    else
		(void)printf ("0x%-*x", varwidth2 - 2, i);
	}
	else {
	    if (v -> v_dvalue)
		(void)printf ("%-*s", varwidth2, v -> v_dvalue[i]);
	    else
		(void)printf ("%-*d", varwidth2, i);
	}
    }
    else
	if (*v -> v_dvalue) {
	    (void) sprintf (buffer, "\"%s\"", *v -> v_dvalue);
	    (void)printf ("%-*s", varwidth2, buffer);
	}
    (void)printf ("    - %s\n", v -> v_dname);
}

/*  */

/* ARGSUSED */

static int  set_debug (v)
struct var *v;
{
    if (debug)
	ll_dbinit (vt_log, myname);
    else
	vt_log -> ll_stat &= ~LLOGTTY;
}


/* ARGSUSED */

static int  set_echo (v)
struct var *v;
{
    if (!connected) {
	advise (LLOG_NOTICE,NULLCP,  "not associated with terminal service");
	return;
    }

    vt_echo (echo);
}


/* ARGSUSED */

static int  set_escape (v)
struct var *v;
{
    if (*escapestr) {
	char   *cp = control (escape = *escapestr);

	free (escapestr);
	escapestr = strdup (cp);
    }
}


/* ARGSUSED */

static int  set_repertoire (v)
struct var *v;
{
    if (!connected) {
	advise (LLOG_NOTICE,NULLCP,  "not associated with terminal service");
	return;
    }

    vt_repertoire (repertoire);
}

/*  */

static char **getval (name, choices)
register char *name;
char   **choices;
{
    register int    longest,
                    nmatches;
    register char  *p,
                   *q,
                  **cp,
                  **fp;
    char    buffer[BUFSIZ];

    longest = nmatches = 0;
    for (cp = choices; p = *cp; cp++) {
	for (q = name; *q == *p++; q++)
	    if (*q == NULL)
		return cp;
	if (*q == NULL)
	    if (q - name > longest) {
		longest = q - name;
		nmatches = 1;
		fp = cp;
	    }
	    else
		if (q - name == longest)
		    nmatches++;
    }

    switch (nmatches) {
	case 0: 
	    advise (LLOG_NOTICE,NULLCP,  "unknown value \"%s\"", name);
	    return NULL;

	case 1: 
	    return fp;

	default: 
	    for (cp = choices, p = buffer; q = *cp; cp++)
		if (strncmp (q, name, longest) == 0) {
		    (void) sprintf (p, "%s \"%s\"", p != buffer ? "," : "", q);
		    p += strlen (p);
		}
	    advise (LLOG_NOTICE,NULLCP,  "ambiguous value, it could be one of:%s",
		    buffer);
	    return NULL;
    }
}

/*  */

static struct var *getvar (name)
register char *name;
{
    register int    longest,
                    nmatches;
    register char  *p,
                   *q;
    char    buffer[BUFSIZ];
    register struct var *v,
			*f;

    longest = nmatches = 0;
    for (v = vars; p = v -> v_name; v++) {
	for (q = name; *q == *p++; q++)
	    if (*q == NULL)
		return v;
	if (*q == NULL)
	    if (q - name > longest) {
		longest = q - name;
		nmatches = 1;
		f = v;
	    }
	    else
		if (q - name == longest)
		    nmatches++;
    }

    switch (nmatches) {
	case 0: 
	    advise (LLOG_NOTICE,NULLCP,  "unknown variable \"%s\"", name);
	    return NULL;

	case 1: 
	    return f;

	default: 
	    for (v = vars, p = buffer; q = v -> v_name; v++)
		if (strncmp (q, name, longest) == 0) {
		    (void) sprintf (p, "%s \"%s\"", p != buffer ? "," : "", q);
		    p += strlen (p);
		}
	    advise (LLOG_NOTICE,NULLCP, 
		    "ambiguous variable, it could be one of:%s", buffer);
	    return NULL;
    }
}

/*    HELP */

static int helpwidth = 0;


static int  vt_help (vec)
char  **vec;
{
    register int    i,
                    j,
                    w;
    int     columns,
            width,
            lines;
    register struct dispatch   *ds,
                               *es;

    for (es = dispatches; es -> ds_name; es++)
	continue;
    width = helpwidth;

    if (*++vec == NULL) {
	if ((columns = ncols (stdout) / (width = (width + 8) & ~7)) == 0)
	    columns = 1;
	lines = ((es - dispatches) + columns - 1) / columns;

	(void)printf ("Operations:\n");
	for (i = 0; i < lines; i++)
	    for (j = 0; j < columns; j++) {
		ds = dispatches + j * lines + i;
		(void)printf ("%s", ds -> ds_name);
		if (ds + lines >= es) {
		    (void)printf ("\n");
		    break;
		}
		for (w = strlen (ds -> ds_name); w < width; w = (w + 8) & ~7)
		    (void) putchar ('\t');
	    }

	(void)printf ("\n");

	return DONE;
    }

    for (; *vec; vec++)
	if (strcmp (*vec, "?") == 0) {
	    for (ds = dispatches; ds -> ds_name; ds++)
		(void)printf ("%-*s\t- %s\n", width, ds -> ds_name, ds -> ds_help);

	    break;
	}
	else
	    if (ds = getds (*vec))
		(void)printf ("%-*s\t- %s\n", width, ds -> ds_name, ds -> ds_help);

    return DONE;
}


#ifndef	TIOCGWINSZ
/* ARGSUSED */
#endif

static int    ncols (fp)
FILE *fp;
{
#ifdef	TIOCGWINSZ
    int	    i;
    struct winsize win;

    if (ioctl (fileno (fp), TIOCGWINSZ, (char *) &win) != NOTOK
	    && (i = win.ws_col) > 0)
	return i;
#endif

    return 80;
}

/*  */

char   *strdup (s)
char   *s;
{
    char    *p;

    if ((p = malloc((unsigned) (strlen (s) + 1))) == NULL)
	adios (NULLCP, "out of memory");

    (void) strcpy (p, s);

    return p;
}

/*  */

static rcinit ()
{
    register int    w;
    register char **cp;
    register struct dispatch *ds;
    register struct var *v;

    if ((myhome = getenv ("HOME")) == NULL)
	myhome = ".";		/* could do passwd search... */

    escapestr = strdup (control (escape));
    for (ds = dispatches, helpwidth = 0; ds -> ds_name; ds++)
	if ((w = strlen (ds -> ds_name)) > helpwidth)
	    helpwidth = w;

    for (v = vars, varwidth1 = 0; v -> v_name; v++) {
	if ((w = strlen (v -> v_name)) > varwidth1)
	    varwidth1 = w;

	if (v -> v_value) {
	    if (cp = v -> v_dvalue) {
		if (v -> v_mask) {
#ifdef	notdef
		    w = 1;
		    while (*++cp)
			w <<= 1;
		    w--;
		    if ((w = strlen (sprintb (w, v -> v_mask))) > varwidth2)
			varwidth2 = w;
#endif
		}
		else
		    for (; *cp; cp++)
			if ((w = strlen (*cp)) > varwidth2)
			    varwidth2 = w;
	    }
	}
	else
	    if (*v -> v_dvalue) {
		*v -> v_dvalue = strdup (*v -> v_dvalue);
		if ((w = strlen (*v -> v_dvalue) + 2) > varwidth2)
		    varwidth2 = w;
	    }
    }
}

char	sibuf[BUFSIZ << 3], *sbp;
char	tibuf[BUFSIZ], *tbp;
int	tcc;

/*
 * Select from tty and network...
 */
vt(s)
	int s;
{
	register int c;
	int tin = fileno(stdin), tout = fileno(stdout);
	int nfds, result;

	if ((nfds = (tin > tout ? tin : tout)) < s)
	    nfds = s;
	nfds++;

	nego_state = 0;
	if(telnet_profile)
	{
		(void) tmode(2);
		vt_rem_echo(&ni_image);		/*Request Remote Echo*/
		vt_sup_ga(&ni_image);		/*Request Suppress Go Ahead*/
		repertoire = 1;
		vt_repertoire(repertoire);
	}
	else (void) tmode(1);

	for (;;) {
	    	fd_set    ibits, obits;

		FD_ZERO (&ibits);

		FD_ZERO (&obits);
		FD_SET (tout, &obits);
		FD_SET (s, &obits);

		if (nfrontp - nbackp)
		    FD_SET (s, &obits);
		else
		    FD_SET (tin, &ibits);

		if (tfrontp - tbackp)
		    FD_SET (tout, &obits);
		else
		    FD_SET (s, &ibits);
		if (FD_ISSET (s, &ibits) && data_pending()) {
		        FD_CLR (s, &ibits);
			result = xselect(nfds, &ibits, &obits,
					 (fd_set *)NULL, OK);
			if (result == -1)
			    adios ("failed", "xselect");
			FD_SET (s, &ibits);
		}
		else {			
			result = xselect(nfds, &ibits, &obits,
					 (fd_set *)NULL, NOTOK);
			if (result == -1)
			    adios ("failed", "xselect");
		}
		if (!FD_ISSET (s, &ibits)
		        && !FD_ISSET (tin, &ibits)
		        && !FD_ISSET (s, &obits)
		        && !FD_ISSET (tout, &obits)) {
			sleep(5);
			continue;
		}

		/*
		 * Something to read from the network...
		 */
		if (FD_ISSET (s, &ibits)) {

			while ( (c = getch()) > 0){
				*tfrontp++ = c;
				if(tfrontp >= &ttyobuf[TBUFSIZ-1]) break;
			}

			if (c == E_EOF) {
				break;
			}
		}

		/*
		 * Something to read from the tty...
		 */
		if (FD_ISSET (tin, &ibits)) {
			tcc = read(tin, tibuf, sizeof (tibuf));
			if (tcc < 0 && errno == EWOULDBLOCK)
				tcc = 0;
			else {
				if (tcc <= 0) {
					advise(LLOG_NOTICE,NULLCP,  "error: read from terminal returned %d", tcc);
					break;
				}
				tbp = tibuf;
			}
		}

		while (tcc > 0) {
			register int ch;

			if ((&netobuf[BUFSIZ] - nfrontp) < 2)
				break;
			ch = *tbp++ & 0377, tcc--;
			if (strip(ch) == escape) {
				command(0);
				tcc = 0;
				break;
			}
			*nfrontp++ = ch;
		}
		if (FD_ISSET (s, &obits) && (nfrontp - nbackp) > 0)
			netflush(s);
		if (FD_ISSET (tout, &obits) && (tfrontp - tbackp) > 0)
			ttyflush(tout);
	}
	(void) tmode(0);
}

/*
 * Construct a control character sequence
 * for a special character.
 */
char *
control(c)
	register int c;
{
	static char buf[3];

	if (c == 0177)
		return ("^?");
	if (c >= 040) {
		buf[0] = c;
		buf[1] = 0;
	} else {
		buf[0] = '^';
		buf[1] = '@'+c;
		buf[2] = 0;
	}
	return (buf);
}

SFD	deadpeer()
{
	(void) tmode(0);
	longjmp(peerdied, -1);
}

SFD	intr()
{
	(void) tmode(0);
	longjmp(toplevel, -1);
}

ttyflush(dd)
int	dd;
{
	int n;

	if ((n = tfrontp - tbackp) > 0) {

		n = write(dd, tbackp, n);

	}
	if (n < 0)
	{
		advise(LLOG_NOTICE,NULLCP,  "ttyflush(): Negative returned from write");
		return;
	}
	tbackp += n;
	if (tbackp == tfrontp)
		tbackp = tfrontp = ttyobuf;
}

netflush(dd)
int	dd;
{
	register char *cp;
	int n, i, j;
	int nl_flag;		/*If current PDU includes newline, follow it
				  with a Deliver Request*/

	nl_flag = 0;
	if ((n = nfrontp - nbackp) > 0) {
		if(transparent)
		{
			if (vt_text(nbackp,n) != OK)
				advise(LLOG_NOTICE,NULLCP,  "vt_text failed");
			vtsend();
			cp = nbackp;
			for(i=0; i<n; i++)
			{
				if( (*cp == '\r') ||
				    (*cp == '\n') )
				{
					vdelreq(FALSE);
					break;
				}
				++cp;
			}
			nbackp += n;
		}
		else
		{
		    cp = nbackp;
		    for(i=0,j=0; i<n; i++)
		    {
			if(*cp == '\r')
			{
			    if(j) 
					if (vt_text(nbackp,j) != OK) 
						advise(LLOG_NOTICE,NULLCP,  "vt_text failed");
			    nbackp += (j+1);
			    cp = nbackp;
			    j = 0;
			    rflag = 1;
			    vt_newline();
			    ++nl_flag;
			}
			else if(*cp == '\n')
			{
			    if(!rflag) /*If preceeding char was not CR*/
			    {
				if(j) 
					if (vt_text(nbackp,j) != OK)
						advise(LLOG_NOTICE,NULLCP,  "vt_text failed");
				nbackp += (j+1);
				cp = nbackp;
				j = 0;
				vt_newline();
				++nl_flag;
			    }
			    else /*Preceeding char was CR so already sent
				   the Update.  Remove this LF from buffer*/
			    {
				++nbackp;
				++cp;
			        rflag = 0;
			    }
			} 
			else if(telnet_profile)
			{
			    rflag = 0;
			    if(*cp == erase_char)
			    {
				    if(j) 
						if (vt_text(nbackp,j) != OK)
							advise(LLOG_NOTICE,NULLCP,  "vt_text failed");
				    nbackp += (j+1);
				    cp = nbackp;
				    j = 0;
			 	    vt_char_erase();
			    }
			    else if(*cp == erase_line)
			    {
				    if(j) 
						if (vt_text(nbackp,j) != OK)
							advise(LLOG_NOTICE,NULLCP,  "vt_text failed");
				    nbackp += (j+1);
				    cp = nbackp;
				    j = 0;
				    vt_line_erase();
			    }
			    else if(*cp == intr_char)
			    {
				    if(j) 
						if (vt_text(nbackp,j) != OK)
							advise(LLOG_NOTICE,NULLCP,  "vt_text failed");
				    nbackp += (j+1);
				    cp = nbackp;
				    j = 0;
				    vt_interrupt();
			    }
			    else if(!vtp_profile.arg_val.tel_arg_list.full_ascii)
					/*If ASCII GO, dump ctrl chars*/
			    {
				if((*cp < 0x20) || (*cp > 0x7e))
				{
				    if(j) 
						if (vt_text(nbackp,j) != OK)
							advise(LLOG_NOTICE,NULLCP,  "vt_text failed");
				    nbackp += (j+1);
				    cp = nbackp;
				    j = 0;
				}
				else
				{
				    ++j;
				    ++cp;
				}
			    }
			    else
			    {
				++j;
				++cp;
			    }
			}
			else	/*Else Default Profile*/
			{
			    if((*cp < 0x20) || (*cp > 0x7e))
			    {
				if(j) 
					if (vt_text(nbackp,j) != OK)
							advise(LLOG_NOTICE,NULLCP,  "vt_text failed");
				nbackp += (j+1);
				cp = nbackp;
				j = 0;
			    }
			    else
			    {
				++j; ++cp;
			    }
			}
		    }		/*End for loop*/
		    if(j) 
				if (vt_text(nbackp,j) != OK) /*Load anything left if CR or LF
							wasn't last char in buffer*/
						advise(LLOG_NOTICE,NULLCP,  "vt_text failed");
		    nbackp += j;
		    vtsend();	/*Send the whole NDQ*/
		    if(nl_flag && telnet_profile) vdelreq(FALSE);
		    rflag = 0;
		}

	}
	if (n < 0) {
		if (errno != ENOBUFS && errno != EWOULDBLOCK) {
			(void) tmode(0);
			perror(peerhost);
			(void)close(dd);
			longjmp(peerdied, -1);
			/*NOTREACHED*/
		}
		n = 0;
	}
	if (nbackp == nfrontp)
		nbackp = nfrontp = netobuf;
}

flushbufs()
{
	tcc = 0;
	tbp = tibuf;
	nfrontp = nbackp = netobuf;
	while (getch() > 0)
	    continue;
	tfrontp = tbackp = ttyobuf;
}
	
/*    ERRORS */

void	finalbye ()
{
    (void) tmode (0);
}


#ifndef	lint
void	adios (va_alist)
va_dcl
{
    int	    code;
    va_list ap;
    static int latched = 0;

    va_start (ap);

    code = va_arg (ap, int);

    (void) _ll_log (vt_log, code, ap);

    va_end (ap);

    if (connected && latched++ == 0)
	(void) vt_close (NULLVP);

    _exit (1);
}
#else
/* VARARGS2 */

void	adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
void	advise (va_alist)
va_dcl
{
    int	    code,
	    flags;
    char    buffer[BUFSIZ];
    va_list ap;

    va_start (ap);

    code = va_arg (ap, int);

    asprintf (buffer, ap);

    flags = vt_log -> ll_stat;

    if (code & (LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE)) {
	(void) fflush (stdout);

	fprintf (stderr, "%s: ", myname);
	(void) fputs (buffer, stderr);
	(void) fputc ('\n', stderr);

	(void) fflush (stderr);

	vt_log -> ll_stat &= ~LLOGTTY;
    }

    (void) ll_log (vt_log, code, NULLCP, "%s", buffer);

    vt_log -> ll_stat = flags;

    va_end (ap);
}
#else
/* VARARGS3 */

void	advise (code, what, fmt)
int	code;
char   *what,
       *fmt;
{
    advise (code, what, fmt);
}
#endif

/* XXX -- why is this stubbed ? */
#ifdef BSD44
ptyecho(on)
{
}
#else
/*ARGSUSED*/
setmode(on, off)
{
}
#endif
