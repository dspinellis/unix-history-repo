/*  $Revision: 1.10 $
**
**  Open the privileged port, then exec innd.
*/
#include "configdata.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>
#include "paths.h"
#include "logging.h"
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"
#if	NOFILE_LIMIT > 0
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>
#include <sys/resource.h>
#endif	/* NOFILE_LIMIT > 0 */

/* #define DEBUGGER "/usr/ucb/dbx" */


#if	NOFILE_LIMIT > 0
/*
**  Set the limit on the number of open files we can have.  I don't
**  like having to do this.
*/
STATIC void
SetDescriptorLimit(i)
    int			i;
{
    struct rlimit	rl;

    if (getrlimit(RLIMIT_NOFILE, &rl) < 0) {
	syslog(L_ERROR, "inndstart cant getrlimit(NOFILE) %m");
	return;
    }
    rl.rlim_cur = i;
    if (setrlimit(RLIMIT_NOFILE, &rl) < 0) {
	syslog(L_ERROR, "inndstart cant setrlimit(NOFILE) %d %m", i);
	return;
    }
}
#endif	/* NOFILE_LIMIT > 0 */


int
main(ac, av)
    int			ac;
    char		*av[];
{
    static char		INNDDIR[] = _PATH_INNDDIR;
    GID_T		NewsGID;
    UID_T		NewsUID;
    struct sockaddr_in	server;
    register int	i;
    register int	j;
#if	defined(SO_REUSEADDR)
    int			on;
#endif	/* defined(SO_REUSEADDR) */
    STRING		*argv;
    char		*p;
    char		pflag[SMBUF];
    char		buff[BUFSIZ];
    STRING		env[8];
    struct stat		Sb;

    (void)openlog("inndstart", L_OPENLOG_FLAGS, LOG_INN_PROG);

    /* Make sure INND directory exists. */
    if (stat(INNDDIR, &Sb) < 0 || !S_ISDIR(Sb.st_mode)) {
	syslog(L_FATAL, "inndstart cant stat %s %m", INNDDIR);
	exit(1);
    }
    NewsUID = Sb.st_uid;
    NewsGID = Sb.st_gid;

#if	NOFILE_LIMIT > 0
    SetDescriptorLimit(NOFILE_LIMIT);
#endif	/* NOFILE_LIMIT > 0 */

    /* Create a socket and name it. */
    if ((i = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	syslog(L_FATAL, "inndstart cant socket %m");
	exit(1);
    }
#if	defined(SO_REUSEADDR)
    on = 1;
    if (setsockopt(i, SOL_SOCKET, SO_REUSEADDR, (caddr_t)&on, sizeof on) < 0)
	syslog(L_ERROR, "inndstart cant setsockopt %m");
#endif	/* defined(SO_REUSEADDR) */
    (void)memset((POINTER)&server, 0, sizeof server);
    server.sin_port = htons(NNTP_PORT);
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = htonl(INADDR_ANY);
    if (bind(i, (struct sockaddr *)&server, sizeof server) < 0) {
	syslog(L_FATAL, "inndstart cant bind %m");
	exit(1);
    }
    (void)sprintf(pflag, "-p%d", i);

    /* Build the new argument vector. */
    argv = NEW(STRING, 2 + ac + 1);
    j = 0;
#if	defined(DEBUGGER)
    argv[j++] = DEBUGGER;
    argv[j++] = _PATH_INND;
    argv[j] = NULL;
    (void)printf("Use -dp%d\n", i);
#else
    argv[j++] = _PATH_INND;
    argv[j++] = pflag;
    for (i = 1; av[i]; )
	argv[j++] = av[i++];
    argv[j] = NULL;
#endif	/* defined(DEBUGGER) */

    /* Set our user and group id. */
    (void)setgid(NewsGID);
    if (getgid() != NewsGID)
	syslog(L_ERROR, "inndstart cant setgid to %d %m", NewsGID);
    (void)setuid(NewsUID);
    if (getuid() != NewsUID)
	syslog(L_ERROR, "inndstart cant setuid to %d %m", NewsUID);

    /* Set up the environment. */
    (void)sprintf(buff, "PATH=%s:%s:/bin:/usr/bin:/usr/ucb",
	    _PATH_NEWSBIN, _PATH_NEWSLIB);
    env[0] = COPY(buff);
    (void)sprintf(buff, "TMPDIR=%s", _PATH_TMP);
    env[1] = COPY(buff);
    (void)sprintf(buff, "SHELL=%s", _PATH_SH);
    env[2] = COPY(buff);
    (void)sprintf(buff, "LOGNAME=%s", NEWSMASTER);
    env[3] = COPY(buff);
    (void)sprintf(buff, "USER=%s", NEWSMASTER);
    env[4] = COPY(buff);
    (void)sprintf(buff, "HOME=%s", _PATH_NEWSLIB);
    env[5] = COPY(buff);
    i = 6;
    if ((p = getenv("TZ")) != NULL) {
	(void)sprintf(buff, "TZ=%s", p);
	env[i++] = COPY(buff);
    }
    env[i++] = NULL;

    /* Go exec innd. */
    (void)execve(argv[0], (CSTRING *)argv, (CSTRING *)env);
    syslog(L_FATAL, "inndstart cant exec %s %m", argv[0]);
    _exit(1);
    /* NOTREACHED */
}
