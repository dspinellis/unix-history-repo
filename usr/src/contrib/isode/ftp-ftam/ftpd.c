/* ftpd.c - FTAM/FTP gateway */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftp-ftam/RCS/ftpd.c,v 7.1 91/02/22 09:24:24 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftp-ftam/RCS/ftpd.c,v 7.1 91/02/22 09:24:24 mrose Interim $
 *
 * Author:	John A. Scott		<Scott@GATEWAY.MITRE.ORG>
 *		The MITRE Corporation
 *		Washington C3I Division
 *		7525 Colshire Drive
 *		Mclean, Virginia 22102
 *		+1-703-883-5915
 *
 * $Log:	ftpd.c,v $
 * Revision 7.1  91/02/22  09:24:24  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:55:21  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *	The MITRE Corporation (hereafter MITRE) makes this software available 
 *	on an "as is" basis.  No guarantees, either explicit or implied, are 
 *	given as to performance or suitability.  
 *
 */

/*
 *	Shamelessly taken from UCB
 */

/*
 * FTP server.
 */
#include "config.h"
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/file.h>

#include <netinet/in.h>

#include <arpa/ftp.h>
#include <arpa/inet.h>

#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <setjmp.h>
#include <netdb.h>
#include <errno.h>
#include "general.h"
#include "manifest.h"
#include "logger.h"
extern LLog _ftam_log, *ftam_log;
#include <varargs.h>

char *ctime();
time_t time();
void adios (), advise ();

/*
 * File containing login names
 * NOT to be used on this machine.
 * Commonly used to disallow uucp.
 */
/*
 * This may be rework to provide bridge access control. JAS
 */
#define	FTPUSERS	"/usr/etc/ftpusers"

extern	int errno;
extern	char *sys_errlist[];
extern  char ftam_error[];
extern	char version[];

struct	sockaddr_in ctrl_addr;
struct	sockaddr_in data_source;
struct	sockaddr_in data_dest;
struct	sockaddr_in his_addr;

int	data;
jmp_buf	errcatch;
int	logged_in;
int	debug = 0;
int	watch = 1;
int	timeout;
int	logging = 0;
int	type;
int	form;
int	stru;			/* avoid C keyword */
int	mode;
int	usedefault = 1;		/* for data transfers */
char	hostname[32];
char	remotehost[32];
char	*osi_host = NULL;
char	*ftp_user;
char	*ftp_account;
char	*ftp_passwd;
int	verbose = 0;
#define NVEC 100
char *vec[NVEC];

/*
 * Timeout intervals for retrying connections
 * to hosts that don't accept PORT cmds.  This
 * is a kludge, but given the problems with TCP...
 */
#define	SWAITMAX	90	/* wait at most 90 seconds */
#define	SWAITINT	5	/* interval between retries */

int	swaitmax = SWAITMAX;
int	swaitint = SWAITINT;

SFD	lostconn();

main(argc, argv)
	int argc;
	char *argv[];
{
	int	addrlen;
	char *ptr, *index();
	struct servent *sp;

	isodetailor (ptr = argv[0], 0);
	argc--, argv++;
	if (verbose = isatty (fileno (stderr)))
	    ll_dbinit (ftam_log, ptr);
	else {
	    ftam_log -> ll_stat &= ~LLOGCLS;
	    ll_hdinit (ftam_log, ptr);
	}

	advise (NULLCP, "starting");

	addrlen = sizeof his_addr;
	if (getpeername (0, (struct sockaddr *) &his_addr, &addrlen) == NOTOK)
	    adios ("failed", "getpeername");
	sp = getservbyname("ftp", "tcp");
	if (sp == 0) {
		advise(NULLCP, "ftp/tcp: unknown service");
abort();
		exit(1);
	}
	ctrl_addr.sin_port = sp->s_port;
	data_source.sin_port = htons(ntohs((u_short) sp->s_port) - 1);
	(void)signal(SIGPIPE, lostconn);
	(void)signal(SIGCHLD, SIG_IGN);
	(void)dup2(0, 1);
	/* do telnet option negotiation here */
	/*
	 * Set up default state
	 */
	rcinit(); /* FTAM state initialize */
	logged_in = 0;
	data = -1;
	type = TYPE_A;
	form = FORM_N;
	stru = STRU_F;
	mode = MODE_S;
	addrlen = sizeof ctrl_addr;
	if (getsockname(0, (struct sockaddr *) &ctrl_addr, &addrlen) == NOTOK)
	    adios ("failed", "getsockname");
	(void)gethostname(hostname, sizeof (hostname));
	ptr = index(hostname,'.'); /* strip off domain name */
	if (ptr) *ptr = '\0';
	reply(220, "%s FTP/FTAM gateway (%s) ready.",
		hostname, version);
	for (;;) {
		(void)setjmp(errcatch);
		(void)yyparse();
	}
}

SFD
lostconn()
{

	advise (NULLCP,"lost connection");
	dologout(-1);
}

char *
savestr(s)
	char *s;
{
	char *malloc();
	char *new = malloc((unsigned) (strlen(s) + 1));
	
	if (new != NULL)
		(void)strcpy(new, s);
	return (new);
}

retrieve(name)
	char *name;
{
	int result;

 /* FTAM file retrieval block function.  Return values:
  * OK    -- file transfered without error
  * NOTOK -- file transfer error
  * DONE  -- Problem opening TCP connection for transfer
  *          Error response made by dataconn routine.
  */
	vec[0] = "f_get";
	vec[1] = name;
	vec[2] = NULL;

	if ((result = f_get(vec)) == NOTOK){
		reply(550, "%s: %s.", name, ftam_error);
	} else if (result == OK)
		reply(226, "Transfer complete.");

	data = -1;
	return;
}

ftp_store(name, modeX)
	char *name, *modeX;
{
	int result;
/*
 * f_put is FTAM file storage block function.  First arguement
 * controls file overwrite or append selection.
 * OK    -- file transfered without error
 * NOTOK -- file transfer error
 * DONE  -- Problem opening TCP connection for transfer
 *          Error response made by dataconn routine.
 */

	vec[0] = strcmp(modeX,"a") ? "put" : "append";
	vec[1] = name;
	vec[2] = NULL;
	if ((result = f_put(vec)) == NOTOK)
		reply(550, "%s: %s.", name, ftam_error);
	else if (result == OK)
		reply(226, "Transfer complete.");
	data = -1;
}

int
getdatasock()
{
/* UCB data socket creation routine */
	int s;
#ifdef	BSD43
	int on = 1;
#endif

	if (data >= 0)
		return (data);
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0)
		return (NOTOK);
#ifndef	BSD43
	if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *) 0, 0) < 0)
#else
	if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *) &on, sizeof on) < 0)
#endif
		goto bad;
	/* anchor socket to avoid multi-homing problems */
	data_source.sin_family = AF_INET;
	data_source.sin_addr = ctrl_addr.sin_addr;
	if (bind(s, (struct sockaddr *) &data_source, sizeof (data_source)) < 0)
		goto bad;
	return (s);
bad:
	(void)close(s);
	return (NOTOK);
}

int
dataconn(name)
	char *name;
{
/* UCB data connection routine */
	int retry = 0;

	if (data >= 0) {
		reply(125, "Using existing data connection for %s.",
		    name);
		usedefault = 1;
		return (data);
	}
	if (usedefault)
		data_dest = his_addr;
	usedefault = 1;
	data = getdatasock();
	if (data == NOTOK){
		reply(425, "Can't create data socket (%s,%d): %s.",
		    inet_ntoa(data_source.sin_addr),
		    ntohs(data_source.sin_port),
		    sys_errlist[errno]);
		return (NOTOK);
	}
	reply(150, "Opening data connection for %s (%s,%d).",
	    name, inet_ntoa(data_dest.sin_addr),
	    ntohs(data_dest.sin_port));
	while (connect(data, (struct sockaddr *)&data_dest, sizeof (data_dest)) < 0) {
		if (errno == EADDRINUSE && retry < swaitmax) {
			sleep((unsigned) swaitint);
			retry += swaitint;
			continue;
		}
		reply(425, "Can't build data connection: %s.",
		    sys_errlist[errno]);
		(void) close(data);
		data = -1;
		return (NOTOK);
	}
	return (data);
}

fatal(s)
	char *s;
{
	reply(451, "Error in server: %s\n", s);
	/* reply(221, "Closing connection due to server error.");*/
	dologout(0);
}

#ifndef	lint
reply(va_alist)
va_dcl
{
    int	n;
    va_list ap;

    va_start (ap);

    n = va_arg (ap, int);

    _reply (n, ' ', ap);

    va_end (ap);
}

lreply(va_alist)
va_dcl
{
    int	n;
    va_list ap;

    va_start (ap);

    n = va_arg (ap, int);

    _reply (n, '-', ap);

    va_end (ap);
}

static _reply (n, c, ap)
int	n;
char    c;
va_list ap;
{
    char    buffer[BUFSIZ];

    _asprintf (buffer, NULLCP, ap);

    printf ("%d%c%s\r\n", n, c, buffer);
    (void)fflush (stdout);

    if (verbose)
	advise (NULLCP,"<--- %d%c%s", n, c, buffer);
}
#else
/* VARARGS2 */

reply(n,fmt)
int	n;
char   *fmt;
{
    reply(n,fmt);
}
/* VARARGS2 */

lreply(n,fmt)
int	n;
char   *fmt;
{
    lreply(n,fmt);
}
#endif

replystr(s)
	char *s;
{
	printf("%s\r\n", s);
	(void)fflush(stdout);
	if (verbose)
	    advise(NULLCP,"<--- %s", s);
}

ack(s)
	char *s;
{
	reply(200, "%s command okay.", s);
}

nack(s)
	char *s;
{
	reply(502, "%s command not implemented.", s);
}

/*ARGSUSED*/
yyerror(s)
char *s;
{
	reply(500, "Command not understood.");
}

ftp_delete(name)
	char *name;
{
/* f_rm is the general purpose FTAM file/directory deletion routine.
 * Change information is formatted in ftam_error.
 */
	vec[0] = "f_rm";
	vec[1] = name;
	vec[2] = NULL;

	if (f_rm(vec) == NOTOK){
		reply(550, "%s: %s.", name, ftam_error);
		return;
	}
	ack("DELE");
}

makedir(name)
	char *name;
{
	
/* f_mkdir is the FTAM directory creation routine */

	vec[0] = "f_mkdir";
	vec[1] = name;
	vec[2] = NULL;
	
	if (f_mkdir(vec) == NOTOK){
		reply(550, "%s: %s.", name, ftam_error);
		return;
	}
	ack("MKDIR");
}

removedir(name)
	char *name;
{

/* f_rm is the general purpose FTAM file/directory deletion routine.
 */
	vec[0] = "f_rm";
	vec[1] = name;
	vec[2] = NULL;

	if (f_rm(vec) == NOTOK){
		reply(550, "%s: %s.", name, ftam_error);
		return;
	}
	ack("RMDIR");
}

char *
renamefrom(name)
	char *name;
{
	reply(350, "Ready for destination name");
	return (name);
}

renamecmd(from, to)
	char *from, *to;
{

/* f_mv is FTAM block function to select and change attributes 
 * (i.e. file name)
 */
	vec[0] ="f_mv";
	vec[1] = from;
	vec[2] = to;
	vec[3] = NULL;

	if (f_mv(vec) == NOTOK){
		reply(550, "rename: %s.", ftam_error);
		return;
	}
	ack("RNTO");
}

dolog(sin)
	struct sockaddr_in *sin;
{
#ifdef	notanymore
	struct hostent *hp = gethostbyaddr((char*)&sin->sin_addr,
		sizeof (struct in_addr), AF_INET);
#endif
	time_t t;

#ifdef	notanymore
	if (hp) {
		(void)strncpy(remotehost, hp->h_name, sizeof (remotehost));
		endhostent();
	} else
#endif
		(void)strncpy(remotehost, inet_ntoa(sin->sin_addr),
		    sizeof (remotehost));
	t = time((time_t*)0);
	if (!logging)
		return;
	advise(NULLCP,"connection from %s at %s", remotehost, ctime(&t));
}
directory(how,name)
char *how, *name;
{

	int result;
/* f_ls does a directory contents transfer.  The first arguement
 * determines whether a name list (NLST) or long list (LIST) is returned.
 * Results:
 * OK    -- list transfered without error
 * NOTOK -- list transfer error
 * DONE  -- Problem opening TCP connection for transfer
 *          Error response made by dataconn routine.
 */

	vec[0] = strcmp(how,"NLST") ? "dir" : "ls";
	vec[1] = name;
	vec[2] = NULL;

	if ((result = f_ls(vec)) == OK)
		reply(226, "Transfer complete.");
	else if (result == NOTOK)
		reply(500, ftam_error);
	data = -1;

}
/*
 * Execute FTAM login if all necessary arguements present
 */
dologin()
{

	if (!ftp_user) {
		reply(500,"Send USER command first");
		return(0);
	}
	if (!ftp_passwd) {
		reply(330,"Send PASS command");
		return(0);
	}
	if (!osi_host){
	/* Success is returned since most user FTP response scanners
         * are not prepared for a continue at this point.  The osi
         * host may be specified by encoding it with the user name
         * (i.e. user@osihost) or using the SITE command (SITE osihost).
	 * Gateway users are expected to know if the remote site
	 * requires account charging information.  The bridge makes
         * ACCT optional.
         */
		reply(200,"Specify OSI filestore with SITE command");
		return(0);
	}
	vec[0] = "f_open";
	vec[1] = osi_host;
	vec[2] = ftp_user;
	vec[3] = (ftp_account) ? "" : ftp_account;
	vec[4] = ftp_passwd;

	advise (NULLCP,
		"attempting association with OSI filestore \"%s\" as initiator \"%s\"",
	       osi_host, ftp_user);

	/* f_open performs the FTAM initialization (including login) */
	if (f_open(vec) == NOTOK){
		reply(500,"Login failed");
		return(0);
	}

	reply(200,"Associated with OSI filestore %s", osi_host);
	return(1);

}

/*
 * exit with supplied status.
 */
dologout(status)
	int status;
{

	vec[0] = "f_close";
	vec[1] = NULL;
	/* f_close performs the logout sequence and receives charging
         * information 
         */
	(void) f_close(vec);
	if (status>=0)
		reply(221,"Logged off. %s",ftam_error);
	/* beware of flushing buffers after a SIGPIPE */
	_exit(status);
}


/*
 * Check user requesting login priviledges.
 * Disallow anyone mentioned in the file FTPUSERS
 * to allow people such as uucp to be avoided.
 */
checkuser(name)
	register char *name;
{
	char line[BUFSIZ], *index();
	FILE *fd, *fopen();
	int found = 0;

	fd = fopen(FTPUSERS, "r");
	if (fd == NULL)
		return (1);
	while (fgets(line, sizeof (line), fd) != NULL) {
		register char *cp = index(line, '\n');

		if (cp)
			*cp = '\0';
		if (strcmp(line, name) == 0) {
			found++;
			break;
		}
	}
	(void)fclose(fd);
	return (!found);
}

