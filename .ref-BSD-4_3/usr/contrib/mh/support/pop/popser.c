/* popser.c - the POP service */

#include "../h/mh.h"
#include "../h/dropsbr.h"
#include "../zotnet/bboards.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include <ctype.h>
#include <errno.h>
#include <pwd.h>
#include <signal.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/stat.h>


#define	TRUE	1
#define	FALSE	0

#define	NVEC	4

/*  */

extern int  errno;

extern int  debug;
extern char myhost[];
extern char *myname;

static enum state {
    auth1, auth2, trans, update, halt, error
} mystate;


int     user (), pass ();
#ifdef	RPOP
int	rpop ();
#endif	RPOP
int     status (), list (), retrieve (), delete (), reset ();
int	top ();
#ifdef	BPOP
int	xtnd ();
#endif	BPOP
int     quit ();

static struct vector {
    char   *v_cmd;
    int     v_min, v_max;
    int     (*v_vec) ();
    enum state v_valid;
    enum state v_win, v_lose;
}               vectors[] = {
    "user", 1, 1, user, auth1, auth2, auth1,
    "pass", 1, 1, pass, auth2, trans, auth1,
#ifdef	RPOP
    "rpop", 1, 1, rpop, auth2, trans, auth1,
#endif	RPOP
    "quit", 0, 0, NULL, auth1, halt, halt,
    "quit", 0, 0, NULL, auth2, halt, halt,

    "stat", 0, 0, status, trans, trans, trans,
    "list", 0, 1, list, trans, trans, trans,
    "retr", 1, 1, retrieve, trans, trans, trans,
    "dele", 1, 1, delete, trans, trans, trans,
    "noop", 0, 0, NULL, trans, trans, trans,
    "rset", 0, 0, reset, trans, trans, trans,

    "top",  2, 2, top,  trans, trans, trans,
#ifdef	BPOP
    "xtnd", 1, 2, xtnd, trans, trans, trans,
#endif	BPOP

    "quit", 0, 0, quit, trans, halt, halt,

    NULL
};

struct vector  *getvector ();

/*  */

#ifdef	DPOP
static int pop_uid;
static int pop_gid;
#endif	DPOP

static int  rproto;
static char *hostname;
static char server[BUFSIZ];

static char username[BUFSIZ];

static char maildrop[BUFSIZ];
static int  mode;
static time_t mtime;
static FILE *dp;

#ifdef	BPOP
static int xtnded;

static int guest_uid;
static int guest_gid;

static struct bboard *BBhead = NULL;
static struct bboard *BBtail = NULL;

static long BBtime = 0L;

struct bboard *getbbaux ();
#endif	BPOP


struct Msg {			/* Msgs[0] contains info for entire maildrop */
    struct drop m_drop;
#define	m_id	m_drop.d_id
#define	m_size	m_drop.d_size
#define	m_start	m_drop.d_start
#define	m_stop	m_drop.d_stop

    unsigned    m_flags;
#define	MNULL	0x00
#define	MDELE	0x01
#define	MREAD	0x02
};

static int nMsgs = 0;
static struct Msg *Msgs = NULL;

static int  nmsgs;
static int  dmsgs;


#define	TRM	"."
#define	TRMLEN	(sizeof TRM - 1)
#define	IAC	255

int    pipeser ();

FILE   *input;
FILE   *output;


void	padvise (), padios ();
long	lseek ();
char   *crypt ();

/*  */

popinit () {
#ifdef	BPOP
    padvise (NULLCP, LOG_INFO, "initialize list of BBoards");

    BBhead = BBtail = NULL;
    while (getbbaux (NULLCP))
	continue;
#endif	BPOP
}

popassert () {
#ifdef	BPOP
    register char **p;
    register struct bboard *bb,
                           *bp;

    if (BBtime == getbbtime ())
	return;

    padvise (NULLCP, LOG_INFO, "list of BBoards has changed");

    for (bb = BBhead; bb; bb = bp) {
	bp = bb -> bb_next;

	if (bb -> bb_name)
	    free (bb -> bb_name);
	if (bb -> bb_file)
	    free (bb -> bb_file);
	if (bb -> bb_archive)
	    free (bb -> bb_archive);
	if (bb -> bb_info)
	    free (bb -> bb_info);
	if (bb -> bb_map)
	    free (bb -> bb_map);
	if (bb -> bb_passwd)
	    free (bb -> bb_passwd);
	if (bb -> bb_date)
	    free (bb -> bb_date);
	if (bb -> bb_addr)
	    free (bb -> bb_addr);
	if (bb -> bb_request)
	    free (bb -> bb_request);
	if (bb -> bb_relay)
	    free (bb -> bb_relay);

	for (p = bb -> bb_aka; *p; p++)
	    free (*p);
	free ((char *) bb -> bb_aka);

	for (p = bb -> bb_leader; *p; p++)
	    free (*p);
	free ((char *) bb -> bb_leader);

	for (p = bb -> bb_dist; *p; p++)
	    free (*p);
	free ((char *) bb -> bb_dist);

	free ((char *) bb);
    }

    BBhead = BBtail = NULL;
    while (getbbaux (NULLCP))
	continue;
#endif	BPOP
}

/*  */

pop (in, out, priv, rhost)
int	in,
	out,
	priv;
char   *rhost;
{
    char    buffer[BUFSIZ],
           *vec[NVEC + 1];
#if	defined (DPOP) || defined (BPOP)
    register struct passwd *pw;
#endif	defined (DPOP) || defined (BPOP)
    register struct vector *v;

    m_foil (NULLCP);
    mts_init (myname);

    rproto = priv;
    hostname = rhost;
    (void) sprintf (server, "%s %s server", myhost, priv ? "RPOP" : "POP");

    if ((input = fdopen (in, "r")) == NULL
	    || (output = fdopen (out, "w")) == NULL) {/* you lose big */
	(void) respond (NOTOK, "%s loses on initialization", server);
	return;
    }
    (void) signal (SIGPIPE, pipeser);

#ifdef	DPOP
    if ((pw = getpwnam (POPUID)) == NULL || !setpwinfo (pw, POPDB, 1)) {
	(void) respond (NOTOK, "%s loses on DB initialization -- %s",
		    server, pw ? getbberr () : "POP user-id unknown");
	return;
    }
    pop_uid = pw -> pw_uid;
    pop_gid = pw -> pw_gid;
#endif	DPOP
#ifdef	BPOP
    if ((pw = getpwnam (popbbuser)) && pw -> pw_uid) {
	guest_uid = pw -> pw_uid;
	guest_gid = pw -> pw_gid;
    }
    else
	guest_uid = guest_gid = 0;
#endif	BPOP

    (void) respond (OK, "%s ready (Comments to: PostMaster@%s)",
	    server, myhost);

    for (mystate = auth1; mystate != halt && mystate != error;)
	switch (getline (buffer, sizeof buffer, input)) {
	    case OK: 
		if ((v = getvector (buffer, vec)) == NULL)
		    continue;
		mystate = (v -> v_vec ? (v -> v_vec) (vec)
			: respond (OK, NULLCP)) == OK
		    ? v -> v_win
		    : v -> v_lose;
		break;

	    case NOTOK: 
	    case DONE: 
		mystate = error;
		(void) respond (NOTOK, "%s signing off", server);
		break;
	}
}

/*  */

static int  user (vec)
register char  **vec;
{
    make_lower (username, vec[1]);

    return respond (OK, "password required for %s", username);
}

/*  */

static int  pass (vec)
register char  **vec;
{
    int	guest = 0;
#ifndef	DPOP
    register struct passwd *pw;
#else	DPOP
    register struct bboard *pw;
#endif	DPOP

#ifndef	DPOP
#ifdef	BPOP
    if (isguest ()) {
#ifdef	TRUSTED
	static passwd gw;

	gw.pw_name = popbbuser;
	gw.pw_uid = guest_uid;
	pw = &gw;
#endif	TRUSTED
	guest = 1;
	goto anonymous;
    }
#endif	BPOP
    if ((pw = getpwnam (username)) == NULL
	    || *pw -> pw_passwd == NULL
	    || strcmp (crypt (vec[1], pw -> pw_passwd), pw -> pw_passwd)) {
#ifdef	TRUSTED
	trusted (0, hostname, NULLCP, 0, pw ? pw -> pw_name : username,
	    pw && pw -> pw_uid == 0, "pop", "tcp", NULL);
#endif	TRUSTED
	return respond (NOTOK, "login incorrect");
    }
#else	DPOP
#ifdef	BPOP
    if (isguest ()) {
#ifdef	TRUSTED
	static bboard gw;

	gw.bb_name = popbbuser;
	pw = &gw;
#endif	TRUSTED
	guest = 1;
	goto anonymous;
    }
#endif	BPOP
    if (((pw = getbbnam (username)) == NULL
		&& (pw = getbbaka (username)) == NULL)
	    || *pw -> bb_passwd == NULL
	    || strcmp (crypt (vec[1], pw -> bb_passwd), pw -> bb_passwd)) {
#ifdef	TRUSTED
	trusted (0, hostname, NULLCP, 0, pw ? pw -> bb_name : username,
	    0, "pop", "tcp", NULL);
#endif	TRUSTED
	return respond (NOTOK, "login incorrect");
    }
#endif	DPOP

#ifdef	BPOP
anonymous: ;
#endif	BPOP
#ifdef	TRUSTED
    if (trusted (1, hostname, NULLCP, 0, myhost,
#ifndef	DPOP
		pw -> pw_name, pw -> pw_uid == 0,
#else	DPOP
		pw -> bb_name, 0,
#endif	DPOP
		"pop", "tcp", NULL)
	    == 0)
	return respond (NOTOK, "permission denied");
#endif	TRUSTED
    return setup (pw, guest);
}

/*  */

#ifdef	BPOP
static  isguest () {
    int	    i;
    register char  *cp;
    char    buffer[BUFSIZ];
    register FILE  *fp;

    if (strcmp (username, popbbuser) || !guest_uid)
	return FALSE;
    if (popbblist == NULL || (fp = fopen (popbblist, "r")) == NULL)
	return TRUE;

    i = FALSE;
    if (hostname)
	while (fgets (buffer, sizeof buffer, fp)) {
	    if (cp = index (buffer, '\n'))
		*cp = NULL;
	    if (strcmp (buffer, hostname) == 0) {
		i = TRUE;
		break;
	    }
	}

    (void) fclose (fp);

    return i;
}
#endif	BPOP

/*  */

#ifdef	RPOP
static int rpop (vec)
register char  **vec;
{
#ifndef	DPOP
    register struct passwd *pw;
#else	DPOP
    register int hostok = 0;
    register char  *bp,
		   *cp;
    char    buffer[BUFSIZ];
    register struct bboard *pw;
#endif	DPOP

#ifndef	DPOP
    if (!rproto || (pw = getpwnam (username)) == NULL) {
#ifdef	TRUSTED
	trusted (0, hostname, vec[1], 0, username, 0, "rpop", "tcp",
	    NULL);
#endif	TRUSTED
	return respond (NOTOK, "login incorrect");
    }
    if (chdir (pw -> pw_dir) == NOTOK && chdir ("/") == NOTOK)
	return respond (NOTOK, "no remote directory");
    if (ruserok (hostname, pw -> pw_uid == 0, vec[1], username) == NOTOK) {
#ifdef	TRUSTED
	trusted (0, hostname, vec[1], 0, pw -> pw_name,
	   pw -> pw_uid == 0, "rpop", "tcp", NULL);
#endif	TRUSTED
	return respond (NOTOK, "permission denied");
    }
#else	DPOP
    if (!rproto
	    || ((pw = getbbnam (username)) == NULL
		&& (pw = getbbaka (username)) == NULL)) {
#ifdef	TRUSTED
	trusted (0, hostname, vec[1], 0, username, 0, "rpop", "tcp",
	    NULL);
#endif	TRUSTED
	return respond (NOTOK, "login incorrect");
    }
/*
 * hacked by Dave Cohrs Tue Feb  4 14:12:15 CST 1986
 *   to allow the hostname to be a list: user@host1,user@host2
 *   NOTE: the separator must be a comma -- no spaces are allowed
 */
    (void) sprintf (buffer, "%s@%s", vec[1], hostname);
    for (bp = pw -> bb_addr; bp; bp = cp) {
	if ((cp = index (bp, ',')))
	    *cp = NULL;
	hostok = strcmp (bp, buffer) == 0;
	if (cp)
	    *cp++ = ',';
	if (hostok)
	    break;
    }
    if (!hostok) {
#ifdef	TRUSTED
	trusted (0, hostname, vec[1], 0, pw -> bb_name, 0, "rpop",
	    "tcp", NULL);
#endif	TRUSTED
	return respond (NOTOK, "permission denied");
    }
#endif	DPOP

#ifdef	TRUSTED
    if (trusted (1, hostname, vec[1], 0, username,
#ifndef	DPOP
		pw -> pw_uid == 0,
#else	DPOP
		0,
#endif	DPOP
		"rpop", "tcp", NULL)
	    == 0)
	return respond (NOTOK, "permission denied");
#endif	TRUSTED
    return setup (pw, FALSE);
}
#endif	RPOP

/*  */

static int setup (pw, guest)
#ifndef	DPOP
register struct passwd *pw;
#else	DPOP
register struct bboard *pw;
#endif	DPOP
int	guest;
{
#ifdef	BPOP
    if (guest) {
	(void) setgid (guest_gid);
	(void) initgroups (popbbuser, guest_gid);
	(void) setuid (guest_uid);
    }
    else {
#endif	BPOP
#ifndef	DPOP
	(void) setgid (pw -> pw_gid);
	(void) initgroups (pw -> pw_name, pw -> pw_gid);
	(void) setuid (pw -> pw_uid);
#else	DPOP
	(void) setgid (pop_gid);
	(void) initgroups (POPUID, pop_gid);
	(void) setuid (pop_uid);
#endif	DPOP
#ifdef	BPOP
    }
#endif	BPOP

#ifndef	DPOP
    (void) sprintf (maildrop, "%s/%s",
	    mmdfldir && *mmdfldir ? mmdfldir : pw -> pw_dir,
	    mmdflfil && *mmdflfil ? mmdflfil : pw -> pw_name);
#else	DPOP
    (void) strcpy (maildrop, pw -> bb_file);
#endif	DPOP

    if (setupaux (guest) == NOTOK)
	return NOTOK;

    return respond (OK,
	    nmsgs ? "maildrop has %d message%s (%d octets)" : "maildrop empty",
	    nmsgs, nmsgs != 1 ? "s" : NULL, Msgs[0].m_size);
}

/*  */

static int  setupaux (readonly)
int	readonly;
{
    register int    i,
                    msgp;
    struct stat st;

#ifdef	BPOP
    xtnded = 0;
#endif	BPOP
    if ((dp = readonly ? fopen (maildrop, "r") : lkfopen (maildrop, "r"))
	    == NULL)
	switch (errno) {
	    case ENOENT: 
		m_gMsgs (msgp = 0);
		goto no_mail;

	    default: 
		nmsgs = dmsgs = 0;
		return respond (NOTOK, "unable to %s maildrop: \"%s\"",
			readonly ? "read" : "lock", maildrop);
	}

    if (fstat (fileno (dp), &st) != NOTOK) {
	mode = (int) (st.st_mode & 0777), mtime = st.st_mtime;
	msgp = read_map (maildrop, (long) st.st_size);
    }
    else {
	mode = 0600, mtime = 0;
	msgp = 0;
    }

    if ((msgp = read_file (msgp ? Msgs[msgp].m_stop : 0L, msgp + 1)) < 1)
	m_gMsgs (0);

no_mail: ;
    dmsgs = 0;
    nmsgs = msgp;

    Msgs[0].m_flags = readonly ? MREAD : MNULL;
    Msgs[0].m_size = 0;
    for (i = 1; i <= nmsgs; i++) {
	if (Msgs[i].m_size == 0)
	    Msgs[i].m_size = mbx_size (i);
	Msgs[0].m_size += Msgs[i].m_size;
	Msgs[i].m_flags = MNULL;
    }

    return OK;
}

/*  */

static int  read_map (file, pos)
char   *file;
long	pos;
{
    register int    i,
                    msgp;
    register struct drop   *pp,
                           *mp;
    struct drop *rp;

    if (debug)
	padvise (NULLCP, LOG_DEBUG, "read_map (%s, %ld)", file, pos);

    if ((i = map_read (file, pos, &rp, debug)) == 0)
	return 0;

    m_gMsgs (i);

    msgp = 1;
    for (pp = rp; i-- > 0; msgp++, pp++) {
	mp = &Msgs[msgp].m_drop;
	mp -> d_id = pp -> d_id;
	mp -> d_size = pp -> d_size;
	mp -> d_start = pp -> d_start;
	mp -> d_stop = pp -> d_stop;
    }
    free ((char *) rp);

    return (msgp - 1);
}

/*  */

static int  read_file (pos, msgp)
register long	pos;
register int	msgp;
{
    register int    i;
    register struct drop   *pp,
                           *mp;
    struct drop *rp;

    if (debug)
	padvise (NULLCP, LOG_DEBUG, "read_file (%ld, %d)",
		pos, msgp);

    if ((i = mbx_read (dp, pos, &rp, debug)) <= 0)
	return (msgp - 1);

    m_gMsgs ((msgp - 1) + i);

    for (pp = rp; i-- > 0; msgp++, pp++) {
	mp = &Msgs[msgp].m_drop;
	mp -> d_id = 0;
	mp -> d_size = pp -> d_size;
	mp -> d_start = pp -> d_start;
	mp -> d_stop = pp -> d_stop;
    }
    free ((char *) rp);

    return (msgp - 1);
}

/*  */

static m_gMsgs (n)
int	n;
{
    if (debug)
	padvise (NULLCP, LOG_DEBUG, "m_gMsgs (%d) 0x%x %d",
		n, Msgs, nMsgs);

    if (Msgs == NULL) {
	nMsgs = n + MAXFOLDER / 2;
	Msgs = (struct Msg *) calloc ((unsigned) (nMsgs + 2), sizeof *Msgs);
	if (Msgs == NULL)
	    padios (NULLCP, "unable to allocate Msgs structure");
	return;
    }

    if (nMsgs >= n)
	return;

    nMsgs = n + MAXFOLDER / 2;
    Msgs = (struct Msg *) realloc ((char *) Msgs,
				(unsigned) (nMsgs + 2) * sizeof *Msgs);
    if (Msgs == NULL)
	padios (NULLCP, "unable to reallocate Msgs structure");
}

/*  */

static int  mbx_size (m)
register int     m;
{
    register int    i;
    register long   pos;

    (void) fseek (dp, Msgs[m].m_start, 0);
    for (i = 0, pos = Msgs[m].m_stop - Msgs[m].m_start; pos > 0; i++, pos--)
	if (fgetc (dp) == '\n')
	    i++;

    return i;
}

/*  */

/* ARGSUSED */

static int  status (vec)
char  **vec;
{
    return respond (OK, "%d %d", nmsgs - dmsgs, Msgs[0].m_size);
}


static int  list (vec)
register char  **vec;
{
    register int    i;

    if (vec[1]) {
	if ((i = atoi (vec[1])) <= 0 || i > nmsgs)
	    return respond (NOTOK, "no such message: \"%s\"", vec[1]);
	if (Msgs[i].m_flags & MDELE)
	    return respond (NOTOK, "message %d is deleted", i);

#ifndef	BPOP
	return respond (OK, "%d %d", i, Msgs[i].m_size);
#else	BPOP
	return respond (OK, xtnded ? "%d %d %d" : "%d %d",
		i, Msgs[i].m_size, Msgs[i].m_id);
#endif	BPOP
    }

    (void) respond (OK, "%d message%s (%d octets)",
	    nmsgs - dmsgs, nmsgs - dmsgs != 1 ? "s" : NULL,
	    Msgs[0].m_size);
    for (i = 1; i <= nmsgs; i++)
	if (!(Msgs[i].m_flags & MDELE))
#ifndef BPOP
	    multiline ("%d %d", i, Msgs[i].m_size);
#else	BPOP
	    multiline (xtnded ? "%d %d %d" : "%d %d",
		    i, Msgs[i].m_size, Msgs[i].m_id);
#endif	BPOP
    multiend ();

    return OK;
}

/*  */

static int  retrieve (vec)
register char  **vec;
{
    register int    i;
    register long   pos;
    register char  *cp;
    char    buffer[BUFSIZ];

    if ((i = atoi (vec[1])) <= 0 || i > nmsgs)
	return respond (NOTOK, "no such message: \"%s\"", vec[1]);
    if (Msgs[i].m_flags & MDELE)
	return respond (NOTOK, "message %d is deleted", i);

    (void) respond (OK, "%d octets", Msgs[i].m_size);

    for ((void) fseek (dp, pos = Msgs[i].m_start, 0);
	    fgets (buffer, sizeof buffer, dp) != NULL && pos < Msgs[i].m_stop;
	    pos += (long) (cp - buffer + 1)) {
	if (*(cp = buffer + strlen (buffer) - 1) == '\n')
	    *cp = NULL;
	multiline ("%s", buffer);
    }
    multiend ();

    return OK;
}

/*  */

static int  delete (vec)
register char   **vec;
{
    register int    i;

    if (Msgs[0].m_flags & MREAD)
	return respond (NOTOK, "maildrop is read-only");

    if ((i = atoi (vec[1])) <= 0 || i > nmsgs)
	return respond (NOTOK, "no such message: \"%s\"", vec[1]);
    if (Msgs[i].m_flags & MDELE)
	return respond (NOTOK, "message %d is deleted", i);

    Msgs[i].m_flags |= MDELE;
    Msgs[0].m_size -= Msgs[i].m_size;
    dmsgs++;

    return respond (OK, "message %d deleted (%d octets)", i, Msgs[i].m_size);
}


static int  reset (vec)
char   **vec;
{
    register int    i;

    for (i = 1; i <= nmsgs; i++)
	if (Msgs[i].m_flags & MDELE) {
	    Msgs[i].m_flags &= ~MDELE;
	    Msgs[0].m_size += Msgs[i].m_size;
	    dmsgs--;
	}

    return status (vec);
}

/*  */

static int  top (vec)
register char  **vec;
{
    register int    i,
                    j,
                    body,
                    lines;
    register long   pos;
    register char  *cp;
    char    buffer[BUFSIZ];

    if ((i = atoi (vec[1])) <= 0 || i > nmsgs)
	return respond (NOTOK, "no such message: \"%s\"", vec[1]);
    if (Msgs[i].m_flags & MDELE)
	return respond (NOTOK, "message %d is deleted", i);
    if ((j = atoi (vec[2])) <= 0)
	return respond (NOTOK, "bad number: \"%s\"", vec[2]);

    (void) respond (OK, vec[0]);

    body = lines = 0;
    for ((void) fseek (dp, pos = Msgs[i].m_start, 0);
	    fgets (buffer, sizeof buffer, dp) != NULL && pos < Msgs[i].m_stop;
	    pos += (long) (cp - buffer + 1)) {
	if (*(cp = buffer + strlen (buffer) - 1) == '\n')
	    *cp = NULL;
	if (body) {
	    if (lines++ >= j)
		break;
	}
	else
	    if (*buffer == NULL)
		body++;
	multiline ("%s", buffer);
    }
    multiend ();

    return OK;
}

/*  */

#ifdef	BPOP
static int  xtnd (vec)
register char    **vec;
{
    make_lower (vec[1], vec[1]);

    if (strcmp (vec[1], "bboards") == 0 || strcmp (vec[1], "archive") == 0)
	return xtnd1 (vec);
    if (strcmp (vec[1], "x-bboards") == 0)
	return xtnd2 (vec);

    return respond (NOTOK, "unknown XTND command: \"%s\"", vec[1]);
}


static int  xtnd1 (vec)
register char    **vec;
{
    register struct bboard *bb;

    if (vec[2]) {
	make_lower (vec[2], vec[2]);
	if ((bb = getbbaux (vec[2])) == NULL)
	    return respond (NOTOK, "unknown BBoard: \"%s\"", vec[2]);

	if (quitaux (NULLVP) == NOTOK)
	    return NOTOK;
	(void) strcpy (maildrop,
		strcmp (vec[1], "bboards") ? bb -> bb_archive : bb -> bb_file);
	if (setupaux (TRUE) == NOTOK)
	    return NOTOK;
	xtnded++;
	(void) respond (OK, "%s", vec[1]);
	multiline ("%s %d", bb -> bb_name, bb -> bb_maxima);
    }
    else {
	if (strcmp (vec[1], "bboards"))
	    return respond (NOTOK, "too few arguments to XTND \"%s\"", vec[1]);

	(void) respond (OK, "%s", vec[1]);
	for (bb = BBhead; bb; bb = bb -> bb_next) {
	    getbbmax (bb);
	    if (!(bb -> bb_flags & BB_INVIS))
		multiline ("%s %d", bb -> bb_name, bb -> bb_maxima);
	}
	while (bb = getbbaux (NULLCP))
	    if (!(bb -> bb_flags & BB_INVIS))
		multiline ("%s %d", bb -> bb_name, bb -> bb_maxima);
    }
    multiend ();

    return OK;
}

/*  */

static int  xtnd2 (vec)
register char     **vec;
{
    register char  *cp,
                  **ap;
    char    buffer[BUFSIZ];
    register struct bboard *bb;

    if (vec[2] == NULL)
	return respond (NOTOK, "too few arguments to XTND \"%s\"", vec[1]);

    make_lower (vec[2], vec[2]);
    if ((bb = getbbaux (vec[2])) == NULL)
	return respond (NOTOK, "unknown BBoard: \"%s\"", vec[2]);

    (void) respond (OK, "%s", vec[1]);
    multiline ("%s", bb -> bb_name);

    cp = buffer;
    for (ap = bb -> bb_aka; *ap; ap++) {
	(void) sprintf (cp, cp != buffer ? " %s" : "%s", *ap);
	cp += strlen (cp);
    }
    multiline ("%s", buffer);

    multiline ("%s", bb -> bb_file);
    multiline ("%s", bb -> bb_archive);
    multiline ("%s", bb -> bb_info);
    multiline ("%s", bb -> bb_map);
    multiline ("%s", bb -> bb_passwd);

    cp = buffer;
    for (ap = bb -> bb_leader; *ap; ap++) {
	(void) sprintf (cp, cp != buffer ? " %s" : "%s", *ap);
	cp += strlen (cp);
    }
    multiline ("%s", buffer);

    multiline ("%s", bb -> bb_addr);
    multiline ("%s", bb -> bb_request);
    multiline ("%s", bb -> bb_relay);

    cp = buffer;
    for (ap = bb -> bb_dist; *ap; ap++) {
	(void) sprintf (cp, cp != buffer ? " %s" : "%s", *ap);
	cp += strlen (cp);
    }
    multiline ("%s", buffer);

    getbbmax (bb);
    multiline ("0%o %d", bb -> bb_flags, bb -> bb_maxima);
    multiline ("%s", bb -> bb_date);

    multiend ();

    return OK;
}

/*  */

static struct bboard *getbbaux (s)
register char   *s;
{
    register struct bboard *bb;
    struct stat st;

    if (BBhead == NULL)
	if (setbbinfo (BBOARDS, BBDB, 1))
	    BBtime = getbbtime ();
	else
	    return NULL;

    if (s != NULLCP)
	for (bb = BBhead; bb; bb = bb -> bb_next)
	    if (strcmp (bb -> bb_name, s) == 0) {
		if (debug)
		    padvise (NULLCP, LOG_DEBUG, "getbbaux: \"%s\" from cache",
			    bb -> bb_name);
		getbbmax (bb);
		return bb;
	    }

    while (bb = getbbent ()) {
	if ((bb = getbbcpy (bb)) == NULL)
	    return NULL;

	if (access (bb -> bb_file, 04) == NOTOK && errno == EACCES)
	    bb -> bb_flags |= BB_INVIS;
	bb -> bb_mtime = stat (bb -> bb_info, &st) != NOTOK ? st.st_mtime : 0L;

	if (BBtail != NULL)
	    BBtail -> bb_next = bb;
	if (BBhead == NULL)
	    BBhead = bb;
	BBtail = bb;

	if (s == NULL || strcmp (bb -> bb_name, s) == 0) {
	    if (s && debug)
		padvise (NULLCP, LOG_DEBUG, "getbbaux: \"%s\" from scratch",
			bb -> bb_name);
	    return bb;
	}
    }

    return NULL;
}

/*  */

static  getbbmax (bb)
register struct bboard *bb;
{
    int     i;
    register char  *cp;
    char    buffer[BUFSIZ];
    struct stat st;
    register    FILE * fp;

    if (debug)
	padvise (NULLCP, LOG_DEBUG, "getbbmax: \"%s\", 0%o, %d, %s",
		bb -> bb_name, bb -> bb_flags, bb -> bb_maxima, bb -> bb_date);

    if (!(bb -> bb_flags & BB_INVIS)
	    && access (bb -> bb_file, 04) == NOTOK && errno == EACCES)
	bb -> bb_flags |= BB_INVIS;

    if (stat (bb -> bb_info, &st) == NOTOK
	    || bb -> bb_mtime == st.st_mtime
	    || (fp = fopen (bb -> bb_info, "r")) == NULL)
	return;
    bb -> bb_mtime = st.st_mtime;

    if (fgets (buffer, sizeof buffer, fp) && (i = atoi (buffer)) > 0)
	bb -> bb_maxima = i;
    if (!feof (fp) && fgets (buffer, sizeof buffer, fp)) {
	if (bb -> bb_date)
	    free (bb -> bb_date);
	if (cp = index (buffer, '\n'))
	    *cp = NULL;
	bb -> bb_date = getcpy (buffer);
    }

    (void) fclose (fp);

    if (debug)
	padvise (NULLCP, LOG_DEBUG, "updated: \"%s\", 0%o, %d, %s",
		bb -> bb_name, bb -> bb_flags, bb -> bb_maxima, bb -> bb_date);
}
#endif	BPOP

/*  */

static int  quit (vec)
char   **vec;
{
    int     d,
            n;

    d = dmsgs, n = nmsgs;

    if (quitaux (vec) == NOTOK)
	return NOTOK;

#ifdef	BPOP
    if (xtnded)
	return respond (OK, "%s signing off", server);
#endif	BPOP

    if (n == d)
	return respond (OK, "%s signing off (maildrop empty)", server);

    return respond (OK,
	    n ? "%s signing off (%d message%s, %d octets left)"
	    : "%s signing off (maildrop empty)",
	    server, n - d, n - d != 1 ? "s" : NULL, Msgs[0].m_size);
}


static int  quitaux (vec)
char   **vec;
{
    int     i;

    if (dp == NULL)
	return OK;

    i = quitfile (vec);

    nmsgs = dmsgs = 0;
    (void) lkfclose (dp, maildrop);
    dp = NULL;

    return i;
}

/*  */

/* ARGSUSED */

static int  quitfile (vec)
char   **vec;
{
    register int    i,
                    md;
    char    tmpfil[BUFSIZ],
            map1[BUFSIZ],
            map2[BUFSIZ];
    struct stat st;

    if (dmsgs == 0 || (Msgs[0].m_flags & MREAD))
	return OK;

    if (fstat (fileno (dp), &st) == NOTOK)
	return respond (NOTOK, "unable to stat file");
    if (mtime != st.st_mtime)
	return respond (NOTOK, "new messages have arrived, no update");
    mode = (int) (st.st_mode & 0777);

    if (nmsgs == dmsgs) {
	i = truncate (maildrop, 0);
	(void) unlink (map_name (maildrop));/* XXX */
	if (i == NOTOK)
	    return respond (NOTOK, "unable to zero %s", maildrop);
	return OK;
    }

    (void) strcpy (tmpfil, m_backup (maildrop));
    if ((md = mbx_open (tmpfil, st.st_uid, st.st_gid, mode)) == NOTOK)
	return respond (NOTOK, "unable to create temporary file");

    for (i = 1; i <= nmsgs; i++)
	if (!(Msgs[i].m_flags & MDELE)
	    &&  mbx_write (tmpfil, md, dp, Msgs[i].m_id, Msgs[i].m_start,
			Msgs[i].m_stop, TRUE, debug) == NOTOK) {
	    (void) mbx_close (tmpfil, md);
	    (void) unlink (tmpfil);
	    return respond (NOTOK, "error writing temporary file");
	}
    (void) mbx_close (tmpfil, md);

    if ((i = rename (tmpfil, maildrop)) == OK) {
	(void) strcpy (map1, map_name (tmpfil));
	(void) strcpy (map2, map_name (maildrop));
	if (rename (map1, map2) == NOTOK) {
	    (void) unlink (map1);
	    (void) unlink (map2);
	}
    }

    if (i == NOTOK)
	return respond (NOTOK, "unable to rename maildrop");

    return OK;
}

/*  */

static struct vector   *getvector (bp, vec)
register char   *bp,
	      **vec;
{
    register int    i;
    register struct vector *v;

    for (i = 0; i < NVEC; i++) {
	while (isspace (*bp))
	    *bp++ = NULL;
	if (*bp == NULL) {
	    vec[i] = NULL;
	    break;
	}
	vec[i] = bp;
	while (!isspace (*bp))
	    bp++;
    }
    i--;
    vec[NVEC] = NULL;

    if (*bp != NULL) {
	(void) respond (NOTOK, "too many arguments");
	return NULL;
    }
    if (*vec[0] == NULL) {
	(void) respond (NOTOK, "null command");
	return NULL;
    }
    make_lower (vec[0], vec[0]);

    for (v = vectors; v -> v_cmd; v++)
	if (strcmp (v -> v_cmd, vec[0]) == 0 && v -> v_valid == mystate) {
	    if (i < v -> v_min || v -> v_max < i) {
		(void) respond (NOTOK, "too %s arguments to \"%s\"",
			i < v -> v_min ? "few" : "many", vec[0]);
		return NULL;
	    }
	    else
		return v;
	}

    (void) respond (NOTOK, "unknown command: \"%s\"", vec[0]);
    return NULL;
}

/*  */

/* VARARGS2 */

static int  respond (code, fmt, a, b, c, d)
char   *fmt,
       *a,
       *b,
       *c,
       *d;
int     code;
{
    register char  *bp;
    char    buffer[BUFSIZ];

    bp = buffer;
    bp += strlen (sprintf (bp, "%s%s", code == OK ? "+OK" : "-ERR",
		fmt ? " " : NULL));
    if (fmt)
	bp += strlen (sprintf (bp, fmt, a, b, c, d));
    putline (buffer, output);

    return code;
}


/* VARARGS1 */

static  multiline (fmt, a, b, c, d)
char   *fmt,
       *a,
       *b,
       *c,
       *d;
{
    register char  *cp;
    char    buffer[BUFSIZ + TRMLEN];

    (void) strcpy (buffer, TRM);
    cp = sprintf (buffer + TRMLEN, fmt, a, b, c, d);
    if (strncmp (cp, TRM, TRMLEN) == 0)
	cp = buffer;

    putline (cp, output);
}


static  multiend () {
    putline (TRM, output);
}

/*  */

static int  getline (s, n, iop)
register char  *s;
register int	n;
register FILE  *iop;
{
    register int    c;
    register char  *p;

    p = s;
    while (--n > 0 && (c = fgetc (iop)) != EOF) {
	while (c == IAC) {
	    (void) fgetc (iop);
	    c = fgetc (iop);
	}
	if ((*p++ = c) == '\n')
	    break;
    }
    if (ferror (iop))
	return NOTOK;
    if (c == EOF && p == s)
	return DONE;
    *p++ = NULL;
    if (debug)
	padvise (NULLCP, LOG_DEBUG, "<--- %s", s);

    return OK;
}


static  putline (s, iop)
register char   *s;
register FILE   *iop;
{
    (void) fprintf (iop, "%s\r\n", s);
    if (debug)
	padvise (NULLCP, LOG_DEBUG, "---> %s", s);

    (void) fflush (iop);
}


/* ARGSUSED */

static int pipeser (sig, code, sc)
int	sig;
long    code;
struct sigcontext *sc;
{
    padvise (NULLCP, LOG_WARNING, "lost connection");

    _exit (NOTOK);
}
