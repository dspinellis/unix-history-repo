/* popser.c - the POP service */
#ifndef	lint
static char ident[]="@(#)$Id: popser.c,v 1.31 1993/08/25 17:23:14 jromine Exp $";
#endif

#include "../h/mh.h"
#include "../h/dropsbr.h"
#ifdef	MPOP
#ifdef	BPOP
#include "../h/formatsbr.h"
#include "../h/scansbr.h"
#endif
#endif /* MPOP */
#include "../zotnet/bboards.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include <ctype.h>
#include <errno.h>
#include <pwd.h>
#include <signal.h>
#include "syslog.h"
#include <sys/types.h>
#include <sys/stat.h>
#ifdef KPOP
#include <krb.h>
#endif	/* KPOP */
#ifdef	SYS5
#include <fcntl.h>
#endif	/* SYS5 */
#ifdef	SHADOW
#include <shadow.h>
#endif	/* SHADOW */


#define	TRUE	1
#define	FALSE	0

#define	NVEC	5

#ifndef	POPSERVICE
#define	POPSERVICE	"pop"
#endif

/*  */

extern int  errno;

extern int  debug;
extern char myhost[];
extern char *myname;

#ifndef	POP2
static enum state {
    auth1, auth2, trans, update, halt, error
} mystate;
#else
static enum state {
    auth1, auth2, trans, mbox, item, ack, update, halt, error
} mystate;
#endif


static int     user (), pass ();
#ifdef	BPOP
static		isguest(), getbbmax();
#ifndef	MPOP
static	int	xtnd1(), xtnd2();
#else
static	int	xtnd1(), xtnd2(), xtnd3 ();
#endif /* MPOP */
#endif	/* BPOP */
#ifdef	RPOP
static int	rpop ();
#endif	/* RPOP */
#ifdef	APOP
static	int	apop ();
#endif
static int     status (), list (), retrieve (), delete (), reset ();
static int	top (), last ();
#ifdef	BPOP
static int	xtnd ();
#endif	/* BPOP */
static int     quit ();
#ifdef	POP2
static int	helo (), rdp2 (), acks (), ack2 (), fold (), nack ();
#endif	/* POP2 */

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
#endif	/* RPOP */
#ifdef	APOP
    "apop", 2, 2, apop, auth1, trans, auth1,
#endif
    "quit", 0, 0, NULL, auth1, halt, halt,
    "quit", 0, 0, NULL, auth2, halt, halt,

    "stat", 0, 0, status, trans, trans, trans,
    "list", 0, 1, list, trans, trans, trans,
    "retr", 1, 1, retrieve, trans, trans, trans,
    "dele", 1, 1, delete, trans, trans, trans,
    "noop", 0, 0, NULL, trans, trans, trans,
    "rset", 0, 0, reset, trans, trans, trans,

    "top",  2, 2, top,  trans, trans, trans,
    "last", 0, 0, last, trans, trans, trans,
#ifdef	BPOP
#ifndef	MPOP
    "xtnd", 1, 2, xtnd, trans, trans, trans,
#else
    "xtnd", 1, 3, xtnd, trans, trans, trans,
#endif /* MPOP */
#endif	/* BPOP */
    "quit", 0, 0, quit, trans, halt, halt,

#ifdef	POP2
    "helo", 2, 2, helo, auth1, mbox, auth1,

    "fold", 1, 1, fold, mbox, mbox, mbox,
    "quit", 0, 0, quit, mbox, halt, halt,
    "read", 0, 1, rdp2, mbox, item, error,

    "fold", 1, 1, fold, item, mbox, mbox,
    "read", 0, 1, rdp2, item, item, error,
    "quit", 0, 0, quit, item, halt, halt,
    "retr", 0, 0, retrieve, item, ack, error,

    "acks", 0, 0, ack2, ack, item, error,
    "ackd", 0, 0, ack2, ack, item, error,
    "nack", 0, 0, rdp2, ack, item, error,
    "quit", 0, 0, NULL, ack, halt, halt,

#endif	/* POP2 */
    NULL
};

static struct vector  *getvector ();

/*  */

#ifdef	POP2
static int pop2 = NOTOK;	/* current pop2 msg, or NOTOK if pop3 */
#endif	/* POP2 */
#ifdef	DPOP
static int pop_uid;
static int pop_gid;
#endif	/* DPOP */

static int  rproto;
static char *hostname;
static char server[BUFSIZ];
static char timestamp[BUFSIZ];

static char username[BUFSIZ];

static char maildrop[BUFSIZ];
static int  mode;
static time_t mtime;
static FILE *dp;

static long lastseen;
static int  rmsgs;

#ifdef	BPOP
static int xtnded;

static int guest_uid;
static int guest_gid;

static struct bboard *BBhead = NULL;
static struct bboard *BBtail = NULL;

static long BBtime = 0L;

static struct bboard *getbbaux ();
#endif	/* BPOP */


struct Msg {			/* Msgs[0] contains info for entire maildrop */
    struct drop m_drop;
#define	m_id	m_drop.d_id
#define	m_size	m_drop.d_size
#define	m_last	m_drop.d_start	/* Msgs[i = 0] */
#define	m_start	m_drop.d_start	/* Msgs[i > 0] */
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
#ifdef	MPOP
#ifdef	BPOP
static int   _sc_width = 0;
static char *nfs = NULL;
#endif
#endif /* MPOP */


#define	TRM	"."
#define	TRMLEN	(sizeof TRM - 1)
#define	IAC	255

static TYPESIG    pipeser ();

FILE   *input;
FILE   *output;

#ifndef	__STDC__
#ifdef	SYS5
struct passwd *getpwnam();
#endif
#endif

void	padvise (), padios ();
char   *crypt ();

#ifdef	POPUUMBOX
#define	MBX_READ	pmbx_read
static	int	pmbx_read ();
static	char   *p_copy(), *p_copyin(), *p_nextword();
static		p_cmatch(), p_isdate(), p_ishead(), p_parse(), any();
#else
#define	MBX_READ	mbx_read
#endif
extern	int	mbx_read ();

static int	setup(), setupaux(), read_map(), read_file(), pmbx_size();
static int	quitaux(), quitfile(), respond(), getline();
static	m_gMsgs(), multiline(), multiend(), putline();
/*  */

popinit () {
#ifdef	BPOP
    padvise (NULLCP, LOG_INFO, "initialize list of BBoards");

    BBhead = BBtail = NULL;
    while (getbbaux (NULLCP))
	continue;
#endif	/* BPOP */
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
#endif	/* BPOP */
}

/*  */

#ifdef KPOP
static char *kusername;

kpop (in, out, principal, rhost, auth)
int   in,
      out;
char  *principal, *rhost;
int auth;
#else	/* KPOP */
pop (in, out, priv, rhost)
int	in,
	out,
	priv;
char   *rhost;
#endif	/* KPOP */
{
    char    buffer[BUFSIZ],
           *vec[NVEC + 1];
#if	defined (DPOP) || defined (BPOP)
    register struct passwd *pw;
#endif	/* defined (DPOP) || defined (BPOP) */
    register struct vector *v;

    m_foil (NULLCP);
    mts_init (myname);

    hostname = rhost;
#ifdef KPOP
    rproto = 1;
    (void) sprintf (server, "%s KPOP server", myhost);
#else
    rproto = priv;
    (void) sprintf (server, "%s server", priv ? "RPOP" : "POP");
#endif	/* KPOP */

    if ((input = fdopen (in, "r")) == NULL
	    || (output = fdopen (out, "w")) == NULL) {/* you lose big */
	(void) respond (NOTOK, "%s loses on initialization", server);
	return;
    }
    (void) signal (SIGPIPE, pipeser);
#ifdef KPOP
    if (principal == NULLCP) {
	char buf[512];
	strcpy(buf,  "Authentication failed: ");
	strcat(buf, krb_err_txt[auth]);
	(void) respond (NOTOK, buf);
	return;
    }
    kusername = principal;
#endif	/* KPOP */

#ifdef	DPOP
    if ((pw = getpwnam (POPUID)) == NULL || !setpwinfo (pw, POPDB, 1)) {
	(void) respond (NOTOK, "%s loses on DB initialization -- %s",
		    server, pw ? getbberr () : "POP user-id unknown");
	return;
    }
    pop_uid = pw -> pw_uid;
    pop_gid = pw -> pw_gid;
#endif	/* DPOP */
#ifdef	BPOP
    if ((pw = getpwnam (popbbuser)) && pw -> pw_uid) {
	guest_uid = pw -> pw_uid;
	guest_gid = pw -> pw_gid;
    }
    else
	guest_uid = guest_gid = 0;
#endif	/* BPOP */

    {
	long	clock;

	(void) time (&clock);
	(void) sprintf (timestamp, "<%d.%ld@%s>", getpid (), clock, myhost);
    }
    (void) respond (OK, "%s ready %s", server, timestamp);

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
#ifdef	POP2
static int  helo (vec)		/* sort of "user" and "pass" */
register char  **vec;
{
    pop2 = 0;				/* now we're talkin' pop2! */
    make_lower (username, vec[1]);	/* helo user pass */
    return pass (++vec);		/* user pass */
}
#endif

static int  user (vec)
register char  **vec;
{
    make_lower (username, vec[1]);
#ifdef KPOP
    if (!strcmp(username, kusername))
      return respond (OK, "Kerberos authentication succeeded. Send username as password (%s)", username);
    else {
	respond (NOTOK, "Wrong username supplied (%s vs. %s)",
		 kusername, username);
	return (NOTOK);
    }
#else
    return respond (OK, "password required for %s", username);
#endif
}

/*  */

static int  pass (vec)
register char  **vec;
{
    int	guest = 0;
#ifndef	DPOP
    register struct passwd *pw;
#ifdef	SHADOW
    register struct spwd *shpw;
#endif	/* SHADOW */
#else	/* DPOP */
    register struct bboard *pw;
#endif	/* DPOP */

#ifdef KPOP
#ifndef DPOP
    if ((pw = getpwnam (username)) != NULL)
      return setup(pw, FALSE);
    else
      return respond (NOTOK, "no local password entry");
#else
    {
      static struct bboard entry;
      static char entry_file[BUFSIZ] = "/usr/spool/pop";
      
      pw = &entry;
      pw->bb_name = username;
      strcat(entry_file, username);
      pw->bb_file = entry_file;
      return setup(pw, FALSE);
    }
#endif
#else	/* KPOP */

#ifndef	DPOP
#ifdef	BPOP
    if (isguest ()) {
#ifdef	TRUSTED
	static passwd gw;

	gw.pw_name = popbbuser;
	gw.pw_uid = guest_uid;
	pw = &gw;
#endif	/* TRUSTED */
	guest = 1;
	goto anonymous;
    }
#endif	/* BPOP */
    if ((pw = getpwnam (username)) == NULL
#ifndef	SHADOW
	    || *pw -> pw_passwd == NULL
	    || strcmp (crypt (vec[1], pw -> pw_passwd), pw -> pw_passwd)) {
#else	/* SHADOW */
	    || (shpw = getspnam (username)) == NULL
	    || *shpw -> sp_pwdp == NULL
	    || strcmp (crypt (vec[1], shpw -> sp_pwdp), shpw -> sp_pwdp)) {
#endif	/* SHADOW */
#ifdef	TRUSTED
	trusted (0, hostname, NULLCP, 0, pw ? pw -> pw_name : username,
	    pw && pw -> pw_uid == 0, POPSERVICE, "tcp", NULL);
#endif	/* TRUSTED */
	return respond (NOTOK, "login incorrect");
    }
#else	/* DPOP */
#ifdef	BPOP
    if (isguest ()) {
	static struct bboard gw;

	gw.bb_name = popbbuser;
	pw = &gw;
	guest = 1;
	goto anonymous;
    }
#endif	/* BPOP */
    if (((pw = getbbnam (username)) == NULL
		&& (pw = getbbaka (username)) == NULL)
	    || *pw -> bb_passwd == NULL
	    || strcmp (crypt (vec[1], pw -> bb_passwd), pw -> bb_passwd)) {
#ifdef	TRUSTED
	trusted (0, hostname, NULLCP, 0, pw ? pw -> bb_name : username,
	    0, POPSERVICE, "tcp", NULL);
#endif	/* TRUSTED */
	return respond (NOTOK, "login incorrect");
    }
#endif	/* DPOP */

#ifdef	BPOP
anonymous: ;
#endif	/* BPOP */
#ifdef	TRUSTED
    if (trusted (1, hostname, NULLCP, 0, myhost,
#ifndef	DPOP
		pw -> pw_name, pw -> pw_uid == 0,
#else	/* DPOP */
		pw -> bb_name, 0,
#endif	/* DPOP */
		POPSERVICE, "tcp", NULL)
	    == 0)
	return respond (NOTOK, "permission denied");
#endif	/* TRUSTED */
    return setup (pw, guest);
#endif /* KPOP */
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
#endif	/* BPOP */

/*  */

#ifdef	RPOP
static int rpop (vec)
register char  **vec;
{
#ifndef	DPOP
    register struct passwd *pw;
#else	/* DPOP */
    register int hostok = 0;
    register char  *bp,
		   *cp;
    char    buffer[BUFSIZ];
    register struct bboard *pw;
#endif	/* DPOP */

#ifndef	DPOP
    if (!rproto || (pw = getpwnam (username)) == NULL) {
#ifdef	TRUSTED
	trusted (0, hostname, vec[1], 0, username, 0, "rpop", "tcp",
	    NULL);
#endif	/* TRUSTED */
	return respond (NOTOK, "login incorrect");
    }
    if (chdir (pw -> pw_dir) == NOTOK && chdir ("/") == NOTOK)
	return respond (NOTOK, "no remote directory");
    if (ruserok (hostname, pw -> pw_uid == 0, vec[1], username) == NOTOK) {
#ifdef	TRUSTED
	trusted (0, hostname, vec[1], 0, pw -> pw_name,
	   pw -> pw_uid == 0, "rpop", "tcp", NULL);
#endif	/* TRUSTED */
	return respond (NOTOK, "permission denied");
    }
#else	/* DPOP */
    if (!rproto
	    || ((pw = getbbnam (username)) == NULL
		&& (pw = getbbaka (username)) == NULL)) {
#ifdef	TRUSTED
	trusted (0, hostname, vec[1], 0, username, 0, "rpop", "tcp",
	    NULL);
#endif	/* TRUSTED */
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
	    *cp = 0;
	hostok = uleq (bp, buffer);
	if (cp)
	    *cp++ = ',';
	if (hostok)
	    break;
    }
    if (!hostok) {
#ifdef	TRUSTED
	trusted (0, hostname, vec[1], 0, pw -> bb_name, 0, "rpop",
	    "tcp", NULL);
#endif	/* TRUSTED */
	return respond (NOTOK, "permission denied");
    }
#endif	/* DPOP */

#ifdef	TRUSTED
    if (trusted (1, hostname, vec[1], 0, username,
#ifndef	DPOP
		pw -> pw_uid == 0,
#else	/* DPOP */
		0,
#endif	/* DPOP */
		"rpop", "tcp", NULL)
	    == 0)
	return respond (NOTOK, "permission denied");
#endif	/* TRUSTED */
    return setup (pw, FALSE);
}
#endif	/* RPOP */

/*  */

#ifdef	APOP
#include "popauth.h"
#include "../../uip/md5.c"
#include <ndbm.h>
#include <sys/file.h>
#ifdef	SYS5
#include <fcntl.h>
#endif


static int apop (vec)
register char  **vec;
{
    register char *cp;
    char    buffer[BUFSIZ];
    register unsigned char *dp;
    unsigned char *ep,
		   digest[16];
#ifndef	DPOP
    register struct passwd *pw;
#else
    register struct bboard *pw;
#endif
    struct stat st;
    datum   key,
	    value;
    DBM	   *db;
    MD5_CTX mdContext;
    struct authinfo auth;

    (void) strcpy (username, vec[1]);

#ifndef	DPOP
    if ((pw = getpwnam (username)) == NULL
	    || *pw -> pw_passwd == NULL) {
	return respond (NOTOK, "user invalid");
    }
#else
    if (((pw = getbbnam (username)) == NULL
		&& (pw = getbbaka (username)) == NULL)
	    || *pw -> bb_passwd == NULL) {
	return respond (NOTOK, "subscriber invalid");
    }
#endif

    if ((db = dbm_open (APOP, O_RDONLY, 0)) == NULL)
	return respond (NOTOK, "POP authorization DB not available (%d)",
			errno);
    if (fstat (dbm_pagfno (db), &st) != NOTOK
	    && (st.st_mode & 0777) != 0600) {
	dbm_close (db);
	return respond (NOTOK, "POP authorization DB has wrong mode (0%o)",
			st.st_mode & 0777);
    }
    if (flock (dbm_pagfno (db), LOCK_SH) == NOTOK) {
	dbm_close (db);
	return respond (NOTOK, "unable to lock POP authorization DB (%d)",
			errno);
    }
    key.dsize = strlen (key.dptr = vec[1]) + 1;
    value = dbm_fetch (db, key);
    if (value.dptr == NULL) {
	dbm_close (db);
	return respond (NOTOK, "not authorized");
    }
    bcopy (value.dptr, (char *) &auth, sizeof auth);
    (void) sprintf (cp = buffer, "%s%*.*s", timestamp, auth.auth_secretlen,
		    auth.auth_secretlen, auth.auth_secret);

    dbm_close (db);

    MD5Init (&mdContext);
    MD5Update (&mdContext, (unsigned char *) buffer,
	       (unsigned int) (strlen (timestamp) + auth.auth_secretlen));
    MD5Final (digest, &mdContext);

    cp = buffer;
    for (ep = (dp = digest) + sizeof digest / sizeof digest[0];
	     dp < ep;
	     cp += 2)
	(void) sprintf (cp, "%02x", *dp++ & 0xff);
    *cp = NULL;

    if (strcmp (vec[2], buffer))
	return respond (NOTOK, "authentication failure");

    return setup (pw, 0);
}
#endif

/*  */

static int setup (pw, guest)
#ifndef	DPOP
register struct passwd *pw;
#else	/* DPOP */
register struct bboard *pw;
#endif	/* DPOP */
int	guest;
{
#ifdef	BPOP
    if (guest) {
	(void) setgid (guest_gid);
#ifndef	SYS5
	(void) initgroups (popbbuser, guest_gid);
#endif	/* SYS5 */
	(void) setuid (guest_uid);
    }
    else {
#endif	/* BPOP */
#ifndef	DPOP
	(void) setgid (pw -> pw_gid);
#ifndef	SYS5
	(void) initgroups (pw -> pw_name, pw -> pw_gid);
#endif	/* SYS5 */
	(void) setuid (pw -> pw_uid);
#else	/* DPOP */
	(void) setgid (pop_gid);
#ifndef	SYS5
	(void) initgroups (POPUID, pop_gid);
#endif	/* SYS5 */
	(void) setuid (pop_uid);
#endif	/* DPOP */
#ifdef	BPOP
    }
#endif	/* BPOP */

#ifndef	DPOP
    (void) sprintf (maildrop, "%s/%s",
	    mmdfldir && *mmdfldir ? mmdfldir : pw -> pw_dir,
	    mmdflfil && *mmdflfil ? mmdflfil : pw -> pw_name);
#else	/* DPOP */
    (void) strcpy (maildrop, pw -> bb_file);
#endif	/* DPOP */

    if (setupaux (guest) == NOTOK)
	return NOTOK;

#ifdef	POP2
    if (pop2 != NOTOK) {		/* in response to pop2 "helo" */
	pop2 = nmsgs > 0 ? 1 : 0;
	return respond ('#', "%d message%s (%d octets)",
		nmsgs, nmsgs != 1 ? "s" : "", Msgs[0].m_size);
    }
    else
#endif	/* POP2 */
    return respond (OK,
	    nmsgs ? "maildrop has %d message%s (%d octets)" : "maildrop empty",
	    nmsgs, nmsgs != 1 ? "s" : "", Msgs[0].m_size);
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
#endif	/* BPOP */
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
    lastseen = Msgs[0].m_last;
if(debug)padvise(NULLCP,LOG_DEBUG,"XXX: lastseen=%d",lastseen);
    dmsgs = rmsgs = 0;
    nmsgs = msgp;

    Msgs[0].m_flags = readonly ? MREAD : MNULL;
    Msgs[0].m_size = 0;
    for (i = 1; i <= nmsgs; i++) {
	if (Msgs[i].m_size == 0)
	    Msgs[i].m_size = pmbx_size (i);
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

    Msgs[0].m_last = rp -> d_start;

    msgp = 1;
    for (pp = rp + 1; i-- > 0; msgp++, pp++) {
	mp = &Msgs[msgp].m_drop;
	mp -> d_id = pp -> d_id;
	mp -> d_size = pp -> d_size;
	mp -> d_start = pp -> d_start;
	mp -> d_stop = pp -> d_stop;
    }
    free ((char *) rp);

    if (Msgs[0].m_last > msgp) {
	if (debug)
	    padvise (NULLCP, LOG_DEBUG, "lastseen adjusted from %d to %d",
		Msgs[0].m_last, msgp);
	Msgs[0].m_last = msgp;
    }

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

    if ((i = MBX_READ (dp, pos, &rp, debug)) <= 0)
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

static int  pmbx_size (m)
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


#ifdef	POP2
static int  rdp2 (vec)		/* always returns OK */
char  **vec;
{
    if (vec[1]) {
	if ((pop2 = atoi (vec[1])) <= 0)
	    pop2 = 0;
    }
    else if (pop2 == 0)
	return NOTOK;		/* close 'em down */

    if (pop2 <= 0 || pop2 > nmsgs) {
	pop2 = 0;
	return respond ('=', "0 no message"); 
    }
    if (Msgs[pop2].m_flags & MDELE) {
	pop2 = 0;
	return respond ('=', "0 message %d is deleted", pop2);
    }

    return respond ('=', "%d (message %d)", Msgs[pop2].m_size, pop2);
}

static int  ack2 (vec)
char   **vec;
{
    if (strcmp (vec[0], "ackd") == 0) {
	Msgs[pop2].m_flags |= MDELE;	/* ignored later if MREAD */
	Msgs[0].m_size -= Msgs[pop2].m_size;
	dmsgs++;
    }

    if (pop2) {		/* a current msg */
	rmsgs++;			/* mark this one as read */
	if (++pop2 > nmsgs)
	    pop2 = -1;			/* let rdp2 reset */
	else if (Msgs[pop2].m_flags & MDELE)
	    pop2 = -1;			/* let rdp2 reset */
	if (pop2 > Msgs[0].m_last)
	    Msgs[0].m_last = pop2;
    }
    return rdp2 (vec);		/* vec = { "acks", 0 } */
}

static int  fold (vec)
register char  **vec;
{
    pop2 = 0;

#ifdef	notdef		

/* This might work, or it might be a huge security hole.  For my purpose,
 * it doesn't need to work, so I'm not going to make sure it's OK.
 * 16Nov90/JLR
 */
   
    if (quitaux (NULLVP) == NOTOK)
	return respond ('#', "0 unable to close folder");
    
    (void) sprintf (maildrop, vec[1]);
    if (setupaux (access (maildrop, 2) ? 1 : 0) == NOTOK)
	return respond ('#', "0 unable to read %s", maildrop);

    pop2 = nmsgs > 0 ? 1 : 0;
    return respond ('#', "%d message%s in %s (%d octets)",
	    nmsgs, nmsgs != 1 ? "s" : "", maildrop, Msgs[0].m_size);
    
#endif

    respond ('#', "0 unable to change folders");
    return NOTOK;
}
#endif	/* POP2 */

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
#else	/* BPOP */
#ifdef	MPOP
	if (nfs && !xtnded) {
	    char   *cp;

	    (void) fseek (dp, Msgs[i].m_start, 0);

	    switch (scan (dp, i, 0, nfs, 0, 0, NULLCP,
			  (long) Msgs[i].m_size, 0)) {
		case SCNMSG:
		case SCNENC:
		case SCNERR:
		    if (cp = index (scanl, '\n'))
			*cp = NULL;
		    return respond (OK, "%d %d #%s",
				    i, Msgs[i].m_size, scanl);

		case SCNEOF:
		    return respond (OK, "%d %d #%*d  empty",
				    i, Msgs[i].m_size, DMAXFOLDER, i);

		default:
		    break;
	    }
	}
#endif /* MPOP */
	return respond (OK, xtnded ? "%d %d %d" : "%d %d",
			i, Msgs[i].m_size, Msgs[i].m_id);
#endif	/* BPOP */
    }

    (void) respond (OK, "%d message%s (%d octets)",
	    nmsgs - dmsgs, nmsgs - dmsgs != 1 ? "s" : "",
	    Msgs[0].m_size);
    for (i = 1; i <= nmsgs; i++)
	if (!(Msgs[i].m_flags & MDELE)) {
#ifndef BPOP
	    multiline ("%d %d", i, Msgs[i].m_size);
#else	/* BPOP */
#ifdef	MPOP
	    if (nfs && !xtnded) {
		char   *cp;

		(void) fseek (dp, Msgs[i].m_start, 0);

		switch (scan (dp, i, 0, nfs, 0, 0, NULLCP,
			      (long) Msgs[i].m_size, 0)) {
		    case SCNMSG:
		    case SCNENC:
		    case SCNERR:
		        if (cp = index (scanl, '\n'))
			    *cp = NULL;
		        multiline ("%d %d #%s",
				   i, Msgs[i].m_size, scanl);
			continue;

		    case SCNEOF:
			multiline ("%d %d #%*d  empty",
				   i, Msgs[i].m_size, DMAXFOLDER, i);
			continue;

		    default:
			break;
		}
	    }
#endif /* MPOP */
	    multiline (xtnded ? "%d %d %d" : "%d %d",
		       i, Msgs[i].m_size, Msgs[i].m_id);
#endif	/* BPOP */
	}
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

#ifdef	POP2
  if (pop2 == 0)
    return NOTOK;
  else if (pop2 == NOTOK) {
#endif
    if ((i = atoi (vec[1])) <= 0 || i > nmsgs)
	return respond (NOTOK, "no such message: \"%s\"", vec[1]);
    if (Msgs[i].m_flags & MDELE)
	return respond (NOTOK, "message %d is deleted", i);

    (void) respond (OK, "%d octets", Msgs[i].m_size);
#ifdef	POP2
  }
  else		/* if called by pop2, vec = { "retr", 0 } */
    i = pop2;
#endif

    for ((void) fseek (dp, pos = Msgs[i].m_start, 0);
	    fgets (buffer, sizeof buffer, dp) != NULL && pos < Msgs[i].m_stop;
	    pos += (long) (cp - buffer + 1)) {
	if (*(cp = buffer + strlen (buffer) - 1) == '\n')
	    *cp = 0;
	multiline ("%s", buffer);
    }
#ifdef	POP2
  if (pop2 == NOTOK) {		/* then multiend */
#endif
    multiend ();

    if (i > Msgs[0].m_last) {
	Msgs[0].m_last = i; 
	rmsgs++;
    }
#ifdef	POP2
  }
#endif

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

    if (i > Msgs[0].m_last)
	Msgs[0].m_last = i;

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

    Msgs[0].m_last = lastseen;

#ifdef	MPOP
#ifdef	BPOP
    if (nfs) {
	if (scanl)
	    free (scanl), scanl = NULL;
	free (nfs), nfs = NULL;
    }
#endif
#endif /* MPOP */

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
    if ((j = atoi (vec[2])) < 0)
	return respond (NOTOK, "bad number: \"%s\"", vec[2]);

    (void) respond (OK, vec[0]);

    body = lines = 0;
    for ((void) fseek (dp, pos = Msgs[i].m_start, 0);
	    fgets (buffer, sizeof buffer, dp) != NULL && pos < Msgs[i].m_stop;
	    pos += (long) (cp - buffer + 1)) {
	if (*(cp = buffer + strlen (buffer) - 1) == '\n')
	    *cp = 0;
	if (body) {
	    if (lines++ >= j)
		break;
	}
	else
	    if (*buffer == 0)
		body++;
	multiline ("%s", buffer);
    }
    multiend ();

    return OK;
}

/*  */

/* ARGSUSED */

static int  last (vec)  
char  **vec;
{
    return respond (OK, "%d is the last msg seen", Msgs[0].m_last);
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
#ifdef	MPOP
    if (strcmp (vec[1], "scan") == 0)
	return xtnd3 (vec);
#endif /* MPOP */

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

/*  */

#ifdef	MPOP
static int xtnd3 (vec)
register char **vec;
{
    if (vec[2] == NULL)
	return respond (NOTOK, "too few arguments to XTND \"%s\"", vec[1]);
    if ((_sc_width = atoi (vec[2])) < WIDTH / 2)
	_sc_width = WIDTH / 2;
    nfs = new_fs (NULLCP, vec[3], FORMAT);
    if (scanl)
	(void) free (scanl), scanl = NULL;

    return respond (OK, vec[1]);
}

int	sc_width () { return _sc_width; }
#endif /* MPOP */
#endif	/* BPOP */

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
#endif	/* BPOP */

    if (n == d)
	return respond (OK, "%s signing off (maildrop empty)", server);

    return respond (OK,
	    n ? "%s signing off (%d message%s, %d octets left)"
	    : "%s signing off (maildrop empty)",
	    server, n - d, n - d != 1 ? "s" : "", Msgs[0].m_size);
}


static int  quitaux (vec)
char   **vec;
{
    int     i;

    if (dp == NULL)
	return OK;

    i = quitfile (vec);

    nmsgs = dmsgs = rmsgs = 0;
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
		    j,
		    tmpDR,
                    md;
    char    tmpfil[BUFSIZ],
            map1[BUFSIZ],
            map2[BUFSIZ];
    struct stat st;

if(debug)padvise(NULLCP,LOG_DEBUG,"XXX: dmsgs=%d rmsgs=%d readonly=%d",
		 dmsgs, rmsgs, Msgs[0].m_flags & MREAD);

    if (dmsgs == 0 || (Msgs[0].m_flags & MREAD))
	return OK;

    if (fstat (fileno (dp), &st) == NOTOK)
	return respond (NOTOK, "unable to stat file");
    if (mtime != st.st_mtime)
	return respond (NOTOK, "new messages have arrived, no update");
    mode = (int) (st.st_mode & 0777);

    if (nmsgs == dmsgs) {
#ifndef	SYS5
	i = truncate (maildrop, 0);
#else	/* SYS5 */
	i = open (maildrop, O_WRONLY | O_TRUNC);
	if (i != NOTOK) (void) close (i);
#endif	/* SYS5 */
	(void) unlink (map_name (maildrop));/* XXX */
	if (i == NOTOK)
	    return respond (NOTOK, "unable to zero %s", maildrop);
	return OK;
    }

    (void) strcpy (tmpfil, m_backup (maildrop));
    if ((md = mbx_open (tmpfil, st.st_uid, st.st_gid, mode)) == NOTOK)
	  { char msgbuf0[256];
	    sprintf(msgbuf0,"unable to create temporary file (%s)",tmpfil);
	    return respond (NOTOK, msgbuf0);
          }

    j = 0, tmpDR = Msgs[0].m_last;
if(debug)padvise(NULLCP,LOG_DEBUG,"XXX: last=%d",Msgs[0].m_last);
    for (i = 1; i <= nmsgs; i++) {
	if (!(Msgs[i].m_flags & MDELE))
	    j++;
	if (i == tmpDR)
	    Msgs[0].m_last = j;
    }
if(debug)padvise(NULLCP,LOG_DEBUG,"XXX: last=%d",Msgs[0].m_last);

    for (i = 1; i <= nmsgs; i++)
	if (!(Msgs[i].m_flags & MDELE)
	    &&  mbx_write (tmpfil, md, dp, Msgs[i].m_id, Msgs[0].m_last,
			Msgs[i].m_start, Msgs[i].m_stop, TRUE, debug)
				== NOTOK) {
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
	    *bp++ = 0;
	if (*bp == 0) {
	    vec[i] = NULL;
	    break;
	}

	if (*bp == '"') {
	    for (vec[i] = ++bp; *bp != '\0' && *bp != '"'; bp++)
		if (*bp == '\\') {
		    switch (*++bp) {
			case 'n':
			    (void) strcpy (bp, bp + 1);
			    *--bp = '\n';
			    break;

			case '\\':
			case '"':
			    (void) strcpy (bp - 1, bp);
			    /* and fall... */
			default:
			    bp--;
			    break;
		    }
		}
	    if (*bp == '"')
		*bp++ = '\0';
	    continue;
	}
	
	vec[i] = bp;
	while (!isspace (*bp))
	    bp++;
    }
    i--;
    vec[NVEC] = NULL;

    if (*bp != 0) {
	(void) respond (NOTOK, "too many arguments");
	return NULL;
    }
    if (*vec[0] == 0) {
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
#ifndef	POP2
    (void) sprintf (bp, "%s%s", code == OK ? "+OK" : "-ERR", fmt ? " " : "");
    bp += strlen (bp);
#else
    switch (code) {
	case OK:
	case NOTOK:
	    (void) sprintf (bp, "%s%s", code == OK ? "+OK" : "-ERR",
		    fmt ? " " : "");
	    bp += strlen (bp);
	    break;

	default:		/* only happens in pop2 */
	    *bp++ = code;
	    code = OK;
    }
#endif
    if (fmt) {
	(void) sprintf (bp, fmt, a, b, c, d);
	bp += strlen (bp);
    }
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
    (void) sprintf (cp = (buffer + TRMLEN), fmt, a, b, c, d);
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
    if (debug) {
	if (*--p == '\n')
	    *p = 0;
	padvise (NULLCP, LOG_DEBUG, "<--- %s", s);
	if (*p == 0)
	    *p = '\n';
	p++;
    }
    *p++ = 0;

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

static TYPESIG pipeser (sig, code, sc)
int	sig;
long    code;
struct sigcontext *sc;
{
    padvise (NULLCP, LOG_WARNING, "lost connection");

    _exit (NOTOK);
}

/*  */

/* Some people don't want to use the POP delivery agent with Sendmail
 * if they're going to run POP.  Sendmail writes maildrops in the old
 * UUCP format, and popd doesn't know how to read them.  These people
 * really should do what the MH manual says -- run the pop delivery
 * agent and be done with it.  Some things never die.
 *
 * A real fix would be to make uip/dropsbr.c should use the same methods
 * as sbr/m_getfld.c to determine the format of maildrops and read &
 * write them.  Unfortunately, it'll take a lot of work to bring it into
 * the fold.  20Mar90/JLR
 * 
 * I really really hate to add this, but this lets stuff popd read
 * UUCP style maildrops as well as MMDF (ctrl/A) style maildrops.  It was
 * contributed by Steve Dempsey <steved@longs.LANCE.ColoState.Edu>.
 *
 * Here's what he says:
 * 
 * Ideally, one should be able to do it with the mmdelim strings, but
 * the MH parser is not intelligent enough to do this.  You have at
 * least a couple of choices:
 * 
 *   - use aliases to deliver mail to POP users (user: user@pop) and
 *     install the POP delivery agent - should work well with sendmail.
 *   - fix the POP server!
 * 
 * We have all mail sent to one machine and users are given two options:
 * 
 *   - MH on any machine.
 *   - any user agent on the postoffice machine.
 * 
 * Most of our workstations run xmh and users find that to be sufficient.
 * New users are only taught to use MH, and a very few old timers stay
 * with BSD mail.  In any case, several agents are available at the cost
 * of a telnet/rlogin if a user does not like MH.
 * 
 * I have made the changes to the POP server (MH-6.6/support/pop/popser.c)
 * to look for the `\n\nFrom ' delimiter instead of the ^A's, using some
 * code from the BSD agent.  Context diff is included below.  When this
 * is installed, you just go back to the normal localmail and get rid of
 * slocal completely.
 * 
 * I have not tried this modification with anything but the MH client,
 * but it should work.  Nothing in the POP protocol changes; the server
 * just has different criteria for delimiting messages in the mailbox.
 * If you decide to use this, I'd like to know what happens.
 * 
 *         Steve Dempsey,  Center for Computer Assisted Engineering
 *   Colorado State University, Fort Collins, CO  80523    +1 303 491 0630
 * INET: steved@longs.LANCE.ColoState.Edu, dempsey@handel.CS.ColoState.Edu
 * boulder!ccncsu!longs.LANCE.ColoState.Edu!steved, ...!ncar!handel!dempsey
 */
/* From:    Jim Reid <jim@computer-science.strathclyde.ac.UK>
 * 
 * MH-6.7 does not support MMDF-style mailboxes with POP as claimed. It
 * appears that when code was added to popser.c to support UNIX-style
 * mailboxes, the old behaviour was lost. i.e. The new popd worked with
 * UNIX-style mailboxes, but not MMDF ones. Users would get "format error"
 * error messages if they tried to inc a remote MMDF-style mailbox because
 * the pop daemon didn't want to know or like the MMDF message delimiters.
 */

/* So... Now there's an incredible hack in mhconfig.c to define POPUUMBOX
 * in support/pop/Makefile if we're using Sendmail.  This causes this
 * UUCP-mbox reading code to be used here.  Ugh.  05Nov90/JLR
 */

/*  */
#ifdef	POPUUMBOX
/* from dropsbr.c - read from a mailbox - pop server version */

/* ALMOST IDENTICAL to mbx_read */

static	int	pmbx_read (fp, pos, drops, noisy)
register FILE  *fp;
register long	pos;
struct drop **drops;
int	noisy;
{
    register int    len,
                    size;
    register char  *bp;
    char    buffer[BUFSIZ];
    register struct drop   *cp,
                           *dp,
                           *ep,
                           *pp;

/* MTR: tsk, tsk, tsk... */
    (void) fseek (fp, pos, 0);
    if (fgets (buffer, sizeof buffer, fp)
	    && strcmp (buffer, mmdlm1) == 0)
	return mbx_read (fp, pos, drops, noisy);

    /* get drop storage */
    pp = (struct drop  *) calloc ((unsigned) (len = MAXFOLDER), sizeof *dp);

    if (debug)
	padvise (NULLCP, LOG_DEBUG, "pmbx_read (%d, %ld, %d, %d)",
		fp, pos,drops,noisy);

    if (pp == NULL) {
	if (noisy)
	    admonish (NULLCP, "unable to allocate drop storage");
	return NOTOK;
    }

    /* rewind drop file */
    (void) fseek (fp, pos, 0);

    if (debug)
	padvise (NULLCP, LOG_DEBUG, "rewind maildrop");

    /* read a buffer */
    for (ep = (dp = pp) + len - 1; fgets (buffer, sizeof buffer, fp);) {
	size = 0;

	/* if beginning of msg then mark it */

	if (p_ishead(buffer)) /*(strcmp (buffer, mmdlm1) == 0)*/ {
	    /* (don't) inc pos to msg start, mark it */
	    /*pos += ld1;*/
	    dp -> d_start = pos;
	    pos += strlen(buffer);  /* inc pos after marking head */
	}
	else {
	    /* didn't find it; mark it anyway */
	    dp -> d_start = pos, pos += (long) strlen (buffer);

	    /* count newlines and inc size if any found */
	    for (bp = buffer; *bp; bp++, size++)
		if (*bp == '\n')
		    size++;
	}

	/* read more lines... */
	while (fgets (buffer, sizeof buffer, fp) != NULL)

	    /* found end? */
	    if (p_ishead(buffer)) /*(strcmp (buffer, mmdlm2) == 0)*/ {

		/* out of loop */
	        (void) fseek (fp, pos, 0);
		break;

            }
	    else {
		/* add buffer size to pos */
		pos += (long) strlen (buffer);

		/* count newlines.... */
		for (bp = buffer; *bp; bp++, size++)
		    if (*bp == '\n')
			size++;
	    }

	if (dp -> d_start != pos) {
	    /* do this if pos was actually incremented; got some text */
	    dp -> d_id = 0;
	    dp -> d_size = size;  /* save the stuff we got */
	    dp -> d_stop = pos;
	    dp++;
	}

	/* (don't) advance pos */
	/* pos += ld2; */

	/* need more storage.... */
	if (dp >= ep) {
	    register int    curlen = dp - pp;

	    cp = (struct drop  *) realloc ((char *) pp,
		                    (unsigned) (len += MAXFOLDER) * sizeof *pp);
	    if (cp == NULL) {
		if (noisy)
		    admonish (NULLCP, "unable to allocate drop storage");
		free ((char *) pp);
		return 0;
	    }
	    dp = cp + curlen, ep = (pp = cp) + len - 1;
	}
    }

    /* return unused stuff */
    if (dp == pp)
	free ((char *) pp);
    else
	*drops = pp;
    return (dp - pp);
}

/*
 * The remainder of this file adapted from:
 *
 *	head.c	5.2 (Berkeley) 6/21/85
 */

struct p_hdline {
	char	*l_from;	/* The name of the sender */
	char	*l_tty;		/* His tty string (if any) */
	char	*l_date;	/* The entire date string */
};

/*
 *
 * See if position in a file is a mail header.
 * Return true if yes.  Note the extreme pains to
 * accomodate all funny formats.
 */

#define	NOSTR		((char *) 0)	/* Null string pointer */
static	char *p_copyin();
static	char *p_copy();


static	p_ishead(buffer)
char buffer[];
{
register char *cp;
struct p_hdline hl;
char linebuf[BUFSIZ];
char parbuf[BUFSIZ];

	strcpy(linebuf,buffer);
	cp = linebuf;

	if (linebuf[0]=='F')
		  padvise (NULLCP, LOG_DEBUG, "ishead: '%s'",linebuf);

	if (strncmp("From ", cp, 5) != 0)
		return(0);

	padvise (NULLCP, LOG_DEBUG, "Fromline...");

	/* get full header */
	p_parse(cp, &hl, parbuf);

	if (hl.l_from == NOSTR || hl.l_date ==  NOSTR) {
		  padvise (NULLCP, LOG_DEBUG, "Fromline...NODATE");
		return(0);
		}

	if (!p_isdate(hl.l_date)) {
		  padvise (NULLCP, LOG_DEBUG, "Fromline...BADDATE %s",
			hl.l_date);
		return(0);
		}

	/* I guess we got it! */
	padvise (NULLCP, LOG_DEBUG, "got a head.. ");

	return(1);
}

/*
 * Split a headline into its useful components.
 * Copy the line into dynamic string space, then set
 * pointers into the copied line in the passed headline
 * structure.  Actually, it scans.
 */

static	p_parse(line, hl, pbuf)
	char line[], pbuf[];
	struct p_hdline *hl;
{
	register char *cp, *dp;
	char *sp;
	char word[BUFSIZ];
	char * p_nextword();

	hl->l_from = NOSTR;
	hl->l_tty = NOSTR;
	hl->l_date = NOSTR;
	cp = line;
	sp = pbuf;

	/*
	 * Skip the first "word" of the line, which should be "From"
	 * anyway.
	 */
	cp = p_nextword(cp, word);
	dp = p_nextword(cp, word);
	if (!(strcmp(word, "")==0))
		hl->l_from = p_copyin(word, &sp);

	/* UNLIKELY */
	if (strncmp(dp, "tty", 3) == 0) {
		cp = p_nextword(dp, word);
		hl->l_tty = p_copyin(word, &sp);
		if (cp != NOSTR)
			hl->l_date = p_copyin(cp, &sp);
	}

	/* USUAL */
	else
		if (dp != NOSTR)
			hl->l_date = p_copyin(dp, &sp);
}

/*
 * Copy the string on the left into the string on the right
 * and bump the right (reference) string pointer by the length.
 * Thus, dynamically allocate space in the right string, copying
 * the left string into it.
 */

static	char *
p_copyin(src, space)
	char src[];
	char **space;
{
	register char *cp, *top;
	register int s;

	s = strlen(src);
	cp = *space;
	top = cp;
	strcpy(cp, src);
	cp += s + 1;
	*space = cp;
	return(top);
}

/*
 * Collect a liberal (space, tab delimited) word into the word buffer
 * passed.  Also, return a pointer to the next word following that,
 * or (empty) if none follow.
 */

static	char *
p_nextword(wp, wbuf)
	char wp[], wbuf[];
{
	register char *cp, *cp2;

	if ((cp = wp) == NOSTR) {
		p_copy("", wbuf);
		return(NOSTR);
	}
	cp2 = wbuf;
	while (!any(*cp, " \t") && *cp != '\0')
		if (*cp == '"') {
 			*cp2++ = *cp++;
 			while (*cp != '\0' && *cp != '"')
 				*cp2++ = *cp++;
 			if (*cp == '"')
 				*cp2++ = *cp++;
 		} else
 			*cp2++ = *cp++;
	*cp2 = '\0';
	while (any(*cp, " \t"))
		cp++;
	if (*cp == '\0')
		return(NOSTR);
	return(cp);
}

/*
 * Copy str1 to str2, return pointer to null in str2.
 */

static	char *
p_copy(str1, str2)
	char *str1, *str2;
{
	register char *s1, *s2;

	s1 = str1;
	s2 = str2;
	while (*s1)
		*s2++ = *s1++;
	*s2 = 0;
	return(s2);
}

#define	L	1		/* A lower case char */
#define	S	2		/* A space */
#define	D	3		/* A digit */
#define	O	4		/* An optional digit or space */
#define	C	5		/* A colon */
#define	N	6		/* A new line */
#define U	7		/* An upper case char */

static	char p_ctypes[] = 
	{U,L,L,S,U,L,L,S,O,D,S,D,D,C,D,D,C,D,D,S,D,D,D,D,0};
/*       T h u   S e p   2 9   1 5 : 2 0 : 1 9   1 9 8 8 */

static	char p_tmztyp[] = 
	{U,L,L,S,U,L,L,S,O,D,S,D,D,C,D,D,C,D,D,S,U,U,U,S,D,D,D,D,0};
/*       T h u   S e p   2 9   1 5 : 2 0 : 1 9   M S T   1 9 8 8 */

static	p_isdate(date)
	char date[];
{
	register char *cp;

	cp = date;
	if (p_cmatch(cp, p_ctypes))
		return(1);

	return(p_cmatch(cp, p_tmztyp));
}

/*
 * Match the given string against the given template.
 * Return 1 if they match, 0 if they don't
 */

static	p_cmatch(str, temp)
	char str[], temp[];
{
	register char *cp, *tp;
	register int c;

	cp = str;
	tp = temp;
	while (*cp != '\0' && *tp != 0) {
		c = *cp++;
		switch (*tp++) {
		case L:
			if (c < 'a' || c > 'z')
				return(0);
			break;

		case U:
			if (c < 'A' || c > 'Z')
				return(0);
			break;

		case S:
			if (c != ' ')
				return(0);
			break;

		case D:
			if (!isdigit(c))
				return(0);
			break;

		case O:
			if (c != ' ' && !isdigit(c))
				return(0);
			break;

		case C:
			if (c != ':')
				return(0);
			break;

		case N:
			if (c != '\n')
				return(0);
			break;
		}
	}
	if ((*cp != '\0' && *cp != '\n') || *tp != 0)
		return(0);
	return(1);
}

static	any(ch, str)
	char *str;
{
	register char *f;
	register c;

	f = str;
	c = ch;
	while (*f)
		if (c == *f++)
			return(1);
	return(0);
}
#endif
