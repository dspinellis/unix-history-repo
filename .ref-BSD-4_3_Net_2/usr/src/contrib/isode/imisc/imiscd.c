/* imiscd.c - miscellaneous network service -- responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/imisc/RCS/imiscd.c,v 7.4 91/02/22 09:26:21 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/imisc/RCS/imiscd.c,v 7.4 91/02/22 09:26:21 mrose Interim $
 *
 *
 * $Log:	imiscd.c,v $
 * Revision 7.4  91/02/22  09:26:21  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/11/21  11:37:04  mrose
 * update
 * 
 * Revision 7.2  90/07/09  14:38:48  mrose
 * sync
 * 
 * Revision 7.1  90/07/01  21:04:05  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:57:39  mrose
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


#include <ctype.h>
#include <stdio.h>
#include "IMISC-types.h"	/* IMISC type definitions */
#include "ryresponder.h"	/* for generic idempotent responders */
#include "IMISC-ops.h"		/* IMISC operation definitions */
#include <utmp.h>
#ifdef	SYS5
#include <sys/times.h>
#endif
#include <sys/stat.h>

/*    DATA */

static char *myservice = "isode miscellany";	/* should be something else */

static int execuid = 1;
static int execgid = 1;


					/* OPERATIONS */
int	op_utcTime (), op_genTime (), op_timeOfDay (), op_users (),
	op_charGen (), op_pwdGen (), op_exec (), op_tellUser (), op_data ();

static struct dispatch dispatches[] = {
    "utcTime", operation_IMISC_utcTime, op_utcTime,

    "genTime", operation_IMISC_genTime, op_genTime,

    "timeOfDay", operation_IMISC_timeOfDay, op_timeOfDay,

    "users", operation_IMISC_users, op_users,

    "chargen", operation_IMISC_charGen, op_charGen,

    "pwdGen", operation_IMISC_pwdGen, op_pwdGen,

    "qotd", operation_IMISC_qotd, op_exec,

    "finger", operation_IMISC_finger, op_exec,

    "tellUser", operation_IMISC_tellUser, op_tellUser,

    "ping", operation_IMISC_ping, op_data,

    "sink", operation_IMISC_sink, op_data,

    "echo", operation_IMISC_echo, op_data,

    NULL
};


					/* TYPES */
struct type_IMISC_IA5List *str2ia5list ();


extern int  errno;


long	time ();
char   *ctime ();

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    ryresponder (argc, argv, PLocalHostName (), myservice, NULLCP,
		 dispatches, table_IMISC_Operations, NULLIFP, NULLIFP);

    exit (0);			/* NOTREACHED */
}

/*    OPERATIONS */

/* ARGSUSED */

static int  op_utcTime (sd, ryo, rox, in, roi)
int	sd;
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t	in;
struct RoSAPindication *roi;
{
    long    clock;
    char   *cp;
    register struct tm *tm;
    struct UTCtime  uts;
    register struct UTCtime *ut = &uts;
    register struct type_IMISC_UTCResult *ur;

    if (rox -> rox_nolinked == 0) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"RO-INVOKE.INDICATION/%d: %s, unknown linkage %d",
		sd, ryo -> ryo_name, rox -> rox_linkid);
	return ureject (sd, ROS_IP_LINKED, rox, roi);
    }
    advise (LLOG_NOTICE, NULLCP, "RO-INVOKE.INDICATION/%d: %s",
	    sd, ryo -> ryo_name);

    if (time (&clock) == NOTOK || (tm = gmtime (&clock)) == NULL)
	return error (sd, error_IMISC_unableToDetermineTime, (caddr_t) NULL,
		    rox, roi);

    tm2ut (tm, ut);

    if ((cp = utct2str (ut)) == NULLCP
	    || (ur = str2qb (cp, strlen (cp), 1)) == NULL)
	return error (sd, error_IMISC_congested, (caddr_t) NULL, rox, roi);

    if (RyDsResult (sd, rox -> rox_id, (caddr_t) ur, ROS_NOPRIO, roi) == NOTOK)
	ros_adios (&roi -> roi_preject, "RESULT");

    free_IMISC_UTCResult (ur);

    return OK;
}

/*  */

/* ARGSUSED */

static int  op_genTime (sd, ryo, rox, in, roi)
int	sd;
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t	in;
struct RoSAPindication *roi;
{
    long    clock;
    char   *cp;
#if	defined(BSD42) || defined (HPUX)
    struct timeval  tvs;
#endif
    register struct tm *tm;
    struct UTCtime  uts;
    register struct UTCtime *ut = &uts;
    register struct type_IMISC_GenResult *gr;

    if (rox -> rox_nolinked == 0) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"RO-INVOKE.INDICATION/%d: %s, unknown linkage %d",
		sd, ryo -> ryo_name, rox -> rox_linkid);
	return ureject (sd, ROS_IP_LINKED, rox, roi);
    }
    advise (LLOG_NOTICE, NULLCP, "RO-INVOKE.INDICATION/%d: %s",
	    sd, ryo -> ryo_name);

#if	defined(BSD42) || defined (HPUX)
    if (gettimeofday (&tvs, (struct timezone *) 0) == NOTOK)
	return error (sd, error_IMISC_unableToDetermineTime, (caddr_t) NULL,
		    rox, roi);
    clock = tvs.tv_sec;
#else
    if (time (&clock) == NOTOK)
	return error (sd, error_IMISC_unableToDetermineTime, (caddr_t) NULL,
		    rox, roi);
#endif
    if ((tm = gmtime (&clock)) == NULL)
	return error (sd, error_IMISC_unableToDetermineTime, (caddr_t) NULL,
		    rox, roi);
    tm2ut (tm, ut);
#ifdef	BSD42
    ut -> ut_flags |= UT_USEC;
    ut -> ut_usec = tvs.tv_usec;
#endif

    if ((cp = gent2str (ut)) == NULLCP
	    || (gr = str2qb (cp, strlen (cp), 1)) == NULL)
	return error (sd, error_IMISC_congested, (caddr_t) NULL, rox, roi);

    if (RyDsResult (sd, rox -> rox_id, (caddr_t) gr, ROS_NOPRIO, roi) == NOTOK)
	ros_adios (&roi -> roi_preject, "RESULT");

    free_IMISC_GenResult (gr);

    return OK;
}

/*  */

/* Return the number of seconds since 00:00 (midnight) 1 January 1900 GMT */

/* ARGSUSED */

static int  op_timeOfDay (sd, ryo, rox, in, roi)
int	sd;
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t	in;
struct RoSAPindication *roi;
{
    long	clock;
    struct type_IMISC_TimeResult trs;
    register struct type_IMISC_TimeResult *tr = &trs;

    if (rox -> rox_nolinked == 0) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"RO-INVOKE.INDICATION/%d: %s, unknown linkage %d",
		sd, ryo -> ryo_name, rox -> rox_linkid);
	return ureject (sd, ROS_IP_LINKED, rox, roi);
    }
    advise (LLOG_NOTICE, NULLCP, "RO-INVOKE.INDICATION/%d: %s",
	    sd, ryo -> ryo_name);

    if (time (&clock) == NOTOK)
	return error (sd, error_IMISC_unableToDetermineTime, (caddr_t) NULL,
		    rox, roi);
    tr -> parm = clock + 2208988800;

    if (RyDsResult (sd, rox -> rox_id, (caddr_t) tr, ROS_NOPRIO, roi) == NOTOK)
	ros_adios (&roi -> roi_preject, "RESULT");

    return OK;
}

/*  */

#ifdef	sun
#define	BSD42
#undef	SYS5
#endif

#ifdef	bsd43_ut_host
#undef	BSD42
#define	SYS5
#endif

#ifdef	BSD42
#define	HMAX	(sizeof (ut -> ut_host))
#endif
#define	LMAX	(sizeof (ut -> ut_line))
#define	NMAX	(sizeof (ut -> ut_name))

#ifdef	SYS5
struct utmp *getutent ();
#endif


/* ARGSUSED */

static int  op_users (sd, ryo, rox, in, roi)
int	sd;
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t	in;
struct RoSAPindication *roi;
{
#ifndef	SYS5
    int	    ud;
#endif
    register char  *dp;
    char    buffer[BUFSIZ];
    struct utmp uts;
    register struct utmp   *ut = &uts;
    struct type_IMISC_IA5List *ia5;
    register struct type_IMISC_IA5List **ia5p;

    if (rox -> rox_nolinked == 0) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"RO-INVOKE.INDICATION/%d: %s, unknown linkage %d",
		sd, ryo -> ryo_name, rox -> rox_linkid);
	return ureject (sd, ROS_IP_LINKED, rox, roi);
    }
    advise (LLOG_NOTICE, NULLCP, "RO-INVOKE.INDICATION/%d: %s",
	    sd, ryo -> ryo_name);

    ia5 = NULL;
    ia5p = &ia5;

#ifndef	SYS5
    if ((ud = open ("/etc/utmp", 0)) == NOTOK) {
	int	result;

	(void) sprintf (buffer, "/etc/utmp: %s", sys_errname (errno));
	if ((*ia5p = str2ia5list (buffer)) == NULL)
	    goto congested;
	ia5p = &((*ia5p) -> next);

	result = error (sd, error_IMISC_unableToOpenFile, (caddr_t) ia5, rox,
		    roi);

	free_IMISC_IA5List (ia5);

	return result;
    }

    while (read (ud, (char *) ut, sizeof *ut) == sizeof *ut) {
	if (ut -> ut_name[0] == NULL)
	    continue;
	if ((dp = ctime (&ut -> ut_time)) == NULL)
	    goto congested;
	(void) sprintf (buffer, "%-*.*s %-*.*s %.12s",
		NMAX, NMAX, ut -> ut_name, LMAX, LMAX, ut -> ut_line, dp + 4);
#ifdef	BSD42
	if (ut -> ut_host[0])
	    (void) sprintf (buffer + strlen (buffer), "\t(%.*s)",
		    HMAX, ut -> ut_host);
#endif

	if ((*ia5p = str2ia5list (buffer)) == NULL)
	    goto congested;
	ia5p = &((*ia5p) -> next);
    }
    (void) close (ud);
#else
    setutent ();
    while (ut = getutent ()) {
	if (ut -> ut_type != USER_PROCESS)
	    continue;
	if ((dp = ctime (&ut -> ut_time)) == NULL)
	    goto congested;
	(void) sprintf (buffer, "%-*.*s %-*.*s %.12s",
		NMAX, NMAX, ut -> ut_name, LMAX, LMAX, ut -> ut_line,
		dp + 4);

	if ((*ia5p = str2ia5list (buffer)) == NULL)
	    goto congested;
	ia5p = &((*ia5p) -> next);
    }
    endutent ();
#endif

    if (RyDsResult (sd, rox -> rox_id, (caddr_t) ia5, ROS_NOPRIO, roi)
	    == NOTOK)
	ros_adios (&roi -> roi_preject, "RESULT");
    free_IMISC_IA5List (ia5);

    return OK;

congested: ;
    free_IMISC_IA5List (ia5);

    return error (sd, error_IMISC_congested, (caddr_t) NULL, rox, roi);
}


#ifdef	bsd43_ut_host
#define	BSD42
#undef	SYS5
#endif

/*  */

#define	NBYTES	512
#define	LINSIZ	72


/* ARGSUSED */

static int  op_charGen (sd, ryo, rox, in, roi)
int	sd;
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t	in;
struct RoSAPindication *roi;
{
    register int    i,
                    j;
    register char  *dp,
                   *de,
                   *rs,
                   *rp,
                   *re;
    char    line[LINSIZ + 1],
            ring[BUFSIZ];
    struct type_IMISC_IA5List *ia5;
    register struct type_IMISC_IA5List **ia5p;

    if (rox -> rox_nolinked == 0) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"RO-INVOKE.INDICATION/%d: %s, unknown linkage %d",
		sd, ryo -> ryo_name, rox -> rox_linkid);
	return ureject (sd, ROS_IP_LINKED, rox, roi);
    }
    advise (LLOG_NOTICE, NULLCP, "RO-INVOKE.INDICATION/%d: %s",
	    sd, ryo -> ryo_name);

    re = ring;
    for (i = 0; i < 0x80; i++)
	if (isprint ((u_char) i))
	    *re++ = i;

    ia5 = NULL;
    ia5p = &ia5;

    for (rs = ring, i = NBYTES; i > 0; rs++, i -= j) {
	if (rs >= re)
	    rs = ring;
	j = i > LINSIZ ? LINSIZ : i;
	for (de = (dp = line) + j, rp = rs; dp < de; dp++, rp++) {
	    if (rp >= re)
		rp = ring;
	    *dp = *rp;
	}

	*dp = NULL;
	if ((*ia5p = str2ia5list (line)) == NULL)
	    goto congested;
	ia5p = &((*ia5p) -> next);
    }

    if (RyDsResult (sd, rox -> rox_id, (caddr_t) ia5, ROS_NOPRIO, roi)
	    == NOTOK)
	ros_adios (&roi -> roi_preject, "RESULT");
    free_IMISC_IA5List (ia5);

    return OK;

congested: ;
    free_IMISC_IA5List (ia5);

    return error (sd, error_IMISC_congested, (caddr_t) NULL, rox, roi);
}

/*  */

#define	NPASS	6


/* ARGSUSED */

static int  op_pwdGen (sd, ryo, rox, in, roi)
int	sd;
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t	in;
struct RoSAPindication *roi;
{
    register int    i;
    char    buffer[BUFSIZ];
    struct type_IMISC_IA5List *ia5;
    register struct type_IMISC_IA5List **ia5p;

    if (rox -> rox_nolinked == 0) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"RO-INVOKE.INDICATION/%d: %s, unknown linkage %d",
		sd, ryo -> ryo_name, rox -> rox_linkid);
	return ureject (sd, ROS_IP_LINKED, rox, roi);
    }
    advise (LLOG_NOTICE, NULLCP, "RO-INVOKE.INDICATION/%d: %s",
	    sd, ryo -> ryo_name);

    ia5 = NULL;
    ia5p = &ia5;

    for (i = NPASS; i > 0; i--) {
	if (pwdgen (buffer) == NOTOK)
	    goto congested;

	if ((*ia5p = str2ia5list (buffer)) == NULL)
	    goto congested;
	ia5p = &((*ia5p) -> next);
    }

    if (RyDsResult (sd, rox -> rox_id, (caddr_t) ia5, ROS_NOPRIO, roi)
	    == NOTOK)
	ros_adios (&roi -> roi_preject, "RESULT");
    free_IMISC_IA5List (ia5);

    return OK;

congested: ;
    free_IMISC_IA5List (ia5);

    return error (sd, error_IMISC_congested, (caddr_t) NULL, rox, roi);
}

/*  */

/* Based on an f77 algorithm supplied by Frank Wancho <Wancho@SIMTEL20>,
   which was based on a basic algorithm by Paul D. Merillat and Arthur A. Key.

   Strings returned are built by alternating vowels and consonants.
   However, there are "Digraphs", and these are presorted according to
   END, MIDDLE, and START positions.

   Not going into combinatorial analysis (with 7 characters the "possible"
   combinations exceed 20 million).
 */


#define	TOT	54636577

static struct pair {
    char   *p_form;
    int     p_value;
} pairs[] = {
	"cvcvcvce", 0,
	"vcvcvcvc", 10827189,
	"cpcpce", 13725209,
	"dvcpce", 14036249,
	"vcvcpce", 14327369,
	"cvcpme", 15296369,
	"dvcvcvc", 15746954,
	"cpcvcvc", 16617618,
	"cvmvcvc", 17547858,
	"cvcpcvc", 23616474,
	"cvcvmvc", 24546714,
	"dpcvce", 30618378,
	"dvmvce", 30908898,
	"cpmvce", 32807107,
	"vmvcvce", 34838173,
	"vcpcvce", 41163763,
	"vcvmvce", 42132163,
	"vmvmvc", 48449078,
	"vmpcvc", 51995642,
	"vmvcpc", 52539386,
	"vcvmpc", 52962661,
	"vmpme", 53385611,
	"dvmpc", 53648987,
	"cpmpc", 53776002,
	"cvcvcpc", 53912072,

	NULL, TOT
};
	     

static char *Mx;
static char *Nx =		/* XXX */
    "BBBCBFBKBLBMBNBPBTBVCCCDCFCMCPDBDCDDDFDGDKDLDMDNDPDTDVDWDZFBFCFDFGFKFMFNF\
PFZGBGDGFGJGMGNGPGTGVKBKDKFKMKNKPKTKVKZLBLCLFLGLMLNLRLVLZMBMCMDMFMGMJMKMLMMMNM\
RMTMVMZNBNCNFNOSZTBTCTDTFTGTJTKTLTMTNTPTVVCVGVLVPNJNLNPNRPCPDPFPGPKPMPNPVPZRBR\
CRDRFRGRJRLSBSDSFSGSJSRSV";

static char *Cx =		/* consonants */
    "BCDFGHJKLMNPRSTVWZ";

static char *Vx =		/* vowels */
    "AEIOU";

static char *Dx =		/* consonant pairs */
    "BRCHCLCRDRFLFRGLGRKLKRPHPLPRQUSCSHSKSLSMSNSPSTSWTHTRTW";

static char *Lx =		/* end-consonant pairs */
    "BSCKCSCTDSFSFTGSHSKSLLLDLKLPLSLTMPMSNDNKNNNSNTPPPSPTRKRMRNRPRSSSTSVSWS";

static char *Px =		/* vowel pairs */
    "AIEAEEIEIOOIOOOU";

static char *Ex =		/* end-vowels */
    "EOAY";

static char *Zx =		/* end-vowel pairs */
    "EEOOAYEYOY";

static struct web {
    char    w_key;
    int	    w_length;
    int	    w_factor;
    char  **w_string;
    char   *w_special;
} webs[] = {
    'm', 189, 2, &Mx, NULL,
    'c', 18, 1, &Cx, "HWJ",
    'v', 5, 1, &Vx, NULL,
    'd', 27, 2, &Dx, NULL,
    'l', 35, 2, &Lx, NULL,
    'p', 8, 2, &Px, NULL,
    'e', 4, 1, &Ex, NULL,
    'z', 5, 2, &Zx, NULL,

    NULL, 0, 0, NULL, NULL
};

/*  */

#define	ifix(f)		((int) ((float) (f) + 0.5))
#ifndef	SYS5
#define	nrand()		(((float) (random ()) / (float) 2147483647))

long	random ();
#else
#define	nrand()		(((float) (rand ()) / (float) 2147483647))

int	rand (), srand ();
#endif
#define	rng(a,b)	if (((i = ifix (a * nrand ()) * b) ? i -= b : i) < 0 \
				|| i >= a * b + (1 - b)) \
			    return NOTOK;


static int  pwdgen (pw)
char   *pw;
{
    register int    i,
                    j;
    register char   c,
                   *f,
                   *s;
    register struct pair   *pair;
    register struct web *web;
    static int  latch = 0;

    if (!latch) {
	if ((Mx = malloc
		    (((unsigned) (strlen (Dx) + strlen (Lx) + strlen (Nx)))))
		== NULL)
	    return NOTOK;
	(void) strcpy (s = Mx, Dx);
	s += strlen (s);
	(void) strcpy (s, Lx);
	s += strlen (s);
	(void) strcpy (s, Nx);
	s += strlen (s);

#ifndef	SYS5
	(void) srandom ((int) time ((long *) 0));
#else
	(void) srand ((unsigned int) time ((long *) 0));
#endif

	latch++;
    }

    rng (TOT, 1.0);
    for (pair = pairs; pair -> p_form; pair++)
	if (pair -> p_value < i)
	    f = pair -> p_form;
	else
	    break;

    do {
	for (s = pw; c = *f++;) {
	    for (web = webs; web -> w_key != c; web++)
		if (web -> w_key == c)
		    break;
	    if (!web -> w_key)
		return NOTOK;

	    rng (web -> w_length, web -> w_factor);

	    for (j = web -> w_factor; j > 0; j--)
		*s++ = (*web -> w_string)[i++];
	    if (web -> w_special && *f == NULL) {
		s--, i--;
		while (index (web -> w_special, *s))
		    *s = (*web -> w_string)[--i];
		s++;
	    }
	}

	*s = NULL;
    } while (object (pw));

    return OK;
}

/*  */

static struct obj {
    char   *o_string;
    int     o_advance;
} objects[] = {
    "TRAFLLEHPARCTIHS", 4,
    "SIPSSATITDOGCUFKUF", 3,

    NULL, 0
};


static int  object (pw)
register char   *pw;
{
    register int    n;
    register char  *f,
                   *s;
    char    buffer[BUFSIZ];
    register struct obj *o;

    for (f = buffer + strlen (s = pw), *f = NULL; *s; s++)
	*--f = *s;

    for (o = objects; s = o -> o_string; o++)
	for (n = o -> o_advance; *s; s += n)
	    if (strncmp (f, s, n) == 0)
		return NOTOK;

    return OK;
}

/*  */

#ifndef	FORTUNE
#define	FORTUNE	"/usr/games/fortune"
#endif
#ifndef	RFINGER
#ifndef	SYS5
#define	RFINGER	"/usr/ucb/finger"
#else
#define	RFINGER	"/usr/bin/finger"
#endif
#endif


static int  op_exec (sd, ryo, rox, in, roi)
int	sd;
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t	in;
struct RoSAPindication *roi;
{
    int     fd,
            i,
            result,
            vecp,
	    vecq,
            pd[2];
    register char  *bp,
                   *dp;
    char    buffer[BUFSIZ],
            data[BUFSIZ],
	   *pgm,
           *vec[NVEC + 1];
    struct type_IMISC_IA5List *ia5;
    register struct type_IMISC_IA5List **ia5p;

    vecp = vecq = 0;
    if (rox -> rox_nolinked == 0) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"RO-INVOKE.INDICATION/%d: %s, unknown linkage %d",
		sd, ryo -> ryo_name, rox -> rox_linkid);
	result = ureject (sd, ROS_IP_LINKED, rox, roi);
	goto out;
    }
    advise (LLOG_NOTICE, NULLCP, "RO-INVOKE.INDICATION/%d: %s",
	    sd, ryo -> ryo_name);

    if (ryo -> ryo_op == operation_IMISC_qotd) {
	vec[vecp++] = pgm = FORTUNE;
	vecq = vecp;
    }
    else {
	vec[vecp++] = pgm = RFINGER;
#ifdef	RFOPT1
	vec[vecp++] = RFOPT1;
#endif
#ifdef	RFOPT2
	vec[vecp++] = RFOPT2;
#endif
	vecq = vecp;
	for (ia5 = (struct type_IMISC_IA5List *) in; ia5; ia5 = ia5 -> next)
	    if (vecp >= NVEC
		    || (vec[vecp++] = qb2str (ia5 -> IA5String)) == NULLCP) {
		result = error (sd, error_IMISC_congested, (caddr_t) NULL, rox,
			    roi);
		goto out;
	    }
    }
    vec[vecp] = NULLCP;

    ia5 = NULL;
    ia5p = &ia5;

    if (access (pgm, 1) == NOTOK) {
	result = error_IMISC_unableToAccessFile;

oops: ;
	free_IMISC_IA5List (ia5);
	ia5 = NULL;
	ia5p = &ia5;

	(void) sprintf (buffer, "%s: %s", pgm, sys_errname (errno));
	if ((*ia5p = str2ia5list (buffer)) == NULL)
	    goto congested;
	ia5p = &((*ia5p) -> next);

	result = error (sd, result, (caddr_t) ia5, rox, roi);

	free_IMISC_IA5List (ia5);

	goto out;
    }
    if (pipe (pd) == NOTOK) {
	result = error_IMISC_unableToPipe;
	goto oops;
    }

    switch (vfork ()) {
	case NOTOK: 
	    (void) close (pd[0]);
	    (void) close (pd[1]);
	    result = error_IMISC_unableToFork;
	    goto oops;

	case OK: 
	    if ((fd = open ("/dev/null", 2)) != NOTOK) {
		if (fd != 0)
		    (void) dup2 (fd, 0), (void) close (fd);
	    }
	    (void) dup2 (pd[1], 1);
	    (void) dup2 (pd[1], 2);
	    (void) close (pd[0]);
	    (void) close (pd[1]);
	    if (execuid != 0) {
		(void) setgid (execgid);
		(void) setuid (execuid);
	    }
	    execvp (pgm, vec);
	    _exit (1);

	default: 
	    (void) close (pd[1]);
	    for (vecp = vecq; bp = vec[vecp]; vecp++) {
		free (bp);
		vec[vecp] = NULL;
	    }

	    for (; vecq < vecp; vecq++)
		if (bp = vec[vecq])
		    free (bp);
	    for (dp = data;;)
		switch (i = read (pd[0], buffer, sizeof buffer)) {
		    case NOTOK: 
			i = errno;
			(void) close (pd[0]);
			errno = i;
			result = error_IMISC_errorReading;
			goto oops;

		    case OK: 
			(void) close (pd[0]);
			if (dp != data) {
			    *dp = NULL;
			    if ((*ia5p = str2ia5list (data)) == NULL)
				goto congested;
			    ia5p = &((*ia5p) -> next);
			}
			if (RyDsResult (sd, rox -> rox_id, (caddr_t) ia5,
				ROS_NOPRIO, roi) == NOTOK)
			    ros_adios (&roi -> roi_preject, "RESULT");
			free_IMISC_IA5List (ia5);
			result = OK;
			goto out;

		    default: 
			for (bp = buffer; i > 0; bp++, i--)
			    switch (*bp) {
				case '\n': 
				    *dp = NULL;
				    if ((*ia5p = str2ia5list (data)) == NULL)
					goto congested;
				    ia5p = &((*ia5p) -> next);
				    dp = data;
				    break;

				case NULL: 
				    break;

				default: 
				    *dp++ = *bp;
				    break;
			    }
			continue;
		}
    }

congested: ;
    free_IMISC_IA5List (ia5);
    result = error (sd, error_IMISC_congested, (caddr_t) NULL, rox, roi);

out: ;
    for (vecp = vecq; bp = vec[vecp]; vecp++)
	free (bp);

    return result;
}

/*  */

/* ARGSUSED */

static int  op_tellUser (sd, ryo, rox, in, roi)
int	sd;
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t	in;
struct RoSAPindication *roi;
{
#ifndef	SYS5
    int     ud;
#endif
    int     hit,
            result,
            vecp;
    char   *bp,
	   *fromuser,
           *touser,
            buffer[BUFSIZ],
          **vec,
           *vecl[NVEC + 1];
    struct utmp uts;
    register struct utmp   *ut = &uts;
    struct type_IMISC_IA5List  *ia5;

    vecp = 0;
    if (rox -> rox_nolinked == 0) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"RO-INVOKE.INDICATION/%d: %s, unknown linkage %d",
		sd, ryo -> ryo_name, rox -> rox_linkid);
	result = ureject (sd, ROS_IP_LINKED, rox, roi);
	goto out;
    }
    advise (LLOG_NOTICE, NULLCP, "RO-INVOKE.INDICATION/%d: %s",
	    sd, ryo -> ryo_name);

    for (ia5 = (struct type_IMISC_IA5List  *) in; ia5; ia5 = ia5 -> next)
	if (vecp >= NVEC
		|| (vecl[vecp++] = qb2str (ia5 -> IA5String)) == NULLCP)
	    goto congested;
    vecl[vecp] = NULLCP;

    if (vecp < 3) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"too few arguments (got %d, wanted at least 3)", vecp);
	result = ureject (sd, ROS_IP_MISTYPED, rox, roi);
	goto out;
    }
    fromuser = vecl[0];
    touser = vecl[1];
    vec = &vecl[2], vecp -= 2;

    hit = 0;
    result = error_IMISC_userNotLoggedIn;
#ifndef	SYS5
    if ((ud = open ("/etc/utmp", 0)) == NOTOK) {
	(void) sprintf (buffer, "/etc/utmp: %s", sys_errname (errno));
	if ((ia5 = str2ia5list (buffer)) == NULL)
	    goto congested;

	result = error (sd, error_IMISC_unableToOpenFile, (caddr_t) ia5, rox,
		roi);

	free_IMISC_IA5List (ia5);

	goto out;
    }

    while (read (ud, (char *) ut, sizeof *ut) == sizeof *ut) {
	if (ut -> ut_name[0] == NULL)
	    continue;
	if (strncmp (ut -> ut_name, touser, sizeof ut -> ut_name) == 0)
	    if (do_the_tell (ut, fromuser, vec, vecp) != NOTOK)
		hit++;
	    else
		result = error_IMISC_unableToOpenFile;
    }
    (void) close (ud);
#else
    setutent ();
    while (ut = getutent ()) {
	if (ut -> ut_type != USER_PROCESS)
	    continue;
	if (strncmp (ut -> ut_name, touser, sizeof ut -> ut_name) == 0)
	    if (do_the_tell (ut, fromuser, vec, vecp) != NOTOK)
		hit++;
	    else
		result = error_IMISC_unableToOpenFile;
    }
    endutent ();
#endif
    if (hit == 0) {
	result = error (sd, result, (caddr_t) NULL, rox, roi);
	goto out;
    }

    if (RyDsResult (sd, rox -> rox_id, (caddr_t) NULL, ROS_NOPRIO, roi)
	    == NOTOK)
	ros_adios (&roi -> roi_preject, "RESULT");

    result = OK;
    goto out;

congested: ;
    result = error (sd, error_IMISC_congested, (caddr_t) NULL, rox, roi);

out: ;
    for (vecp = 0; bp = vecl[vecp]; vecp++)
	free (bp);

    return result;
}

/*  */

static int  do_the_tell (ut, from, vec, vecp)
struct	utmp	*ut;
char	*from;
char	*vec[];
int	vecp;
{
    int     i,
            pid;
    char   *bp,
	    tty[5 + LMAX + 1];
    struct stat st;
    FILE   *fp;

    (void) strcpy (bp = tty, "/dev/");
    bp += strlen (bp);
    (void) strncpy (bp, ut -> ut_line, LMAX);
    bp += LMAX;
    *bp = NULL;
    if (stat (tty, &st) == NOTOK
	    || (st.st_mode & (S_IWRITE >> 3)) != (S_IWRITE >> 3))
	return NOTOK;

    for (i = 0; i < 5; i++) {
	switch (pid = fork ()) {
	    case NOTOK: 
		continue;

	    case 0: 
		break;

	    default: 
		return OK;
	}
	break;
    }
    if (pid == NOTOK)
	return NOTOK;
    if ((fp = fopen (tty, "w")) == NULL)
	_exit (1);
    fprintf (fp, "\r\nmessage from %s:\r\n\007", from);
    for (i = 0, bp = NULL; i < vecp; i++, bp = " ") {
	if (bp)
	    fputs (bp, fp);
	fputs (vec[i], fp);
    }
    fputs ("\r\n", fp);
    (void) fclose (fp);
    _exit (0);			/* NOTREACHED */
}

/*  */

static int  op_data (sd, ryo, rox, in, roi)
int	sd;
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t	in;
struct RoSAPindication *roi;
{
    if (rox -> rox_nolinked == 0) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"RO-INVOKE.INDICATION/%d: %s, unknown linkage %d",
		sd, ryo -> ryo_name, rox -> rox_linkid);
	return ureject (sd, ROS_IP_LINKED, rox, roi);
    }
    if (debug)
	advise (LLOG_DEBUG, NULLCP, "RO-INVOKE.INDICATION/%d: %s",
	    sd, ryo -> ryo_name);

    if (RyDsResult (sd, rox -> rox_id, ryo -> ryo_op == operation_IMISC_echo
			    ? in : (caddr_t) NULL, ROS_NOPRIO, roi) == NOTOK)
	ros_adios (&roi -> roi_preject, "RESULT");

    return OK;
}

/*    ERROR */

static int  error (sd, err, param, rox, roi)
int	sd,
	err;
caddr_t	param;
struct RoSAPinvoke *rox;
struct RoSAPindication *roi;
{
    if (RyDsError (sd, rox -> rox_id, err, param, ROS_NOPRIO, roi) == NOTOK)
	ros_adios (&roi -> roi_preject, "ERROR");

    return OK;
}

/*    U-REJECT */

static int  ureject (sd, reason, rox, roi)
int	sd,
	reason;
struct RoSAPinvoke *rox;
struct RoSAPindication *roi;
{
    if (RyDsUReject (sd, rox -> rox_id, reason, ROS_NOPRIO, roi) == NOTOK)
	ros_adios (&roi -> roi_preject, "U-REJECT");

    return OK;
}

/*    TYPES */

struct type_IMISC_IA5List *str2ia5list (s)
char   *s;
{
    register struct type_IMISC_IA5List *ia5;

    if ((ia5 = (struct type_IMISC_IA5List  *) calloc (1, sizeof *ia5)) == NULL)
	return NULL;

    if ((ia5 -> IA5String = str2qb (s, strlen (s), 1)) == NULL) {
	free ((char *) ia5);
	return NULL;
    }

    return ia5;
}
