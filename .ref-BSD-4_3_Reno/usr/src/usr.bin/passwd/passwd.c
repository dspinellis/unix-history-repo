/*
 * Copyright (c) 1988 The Regents of the University of California.
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
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)passwd.c	4.42 (Berkeley) 6/19/90";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <errno.h>
#include <pwd.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#ifdef	KERBEROS
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <kerberosIV/des.h>
#include <kerberosIV/krb.h>
#include "kpasswd_proto.h"
int use_kerberos = 1;
#define ARGSTR "l"
#else
#define ARGSTR ""
#endif

uid_t uid;

main(argc, argv)
	int argc;
	char **argv;
{
	register int ch;
	extern int errno, optind;
	extern char *optarg;
	struct passwd *pw;
	struct rlimit rlim;
	FILE *temp_fp;
	int fd;
	char *fend, *np, *passwd, *temp, *tend, *uname;
	char from[MAXPATHLEN], to[MAXPATHLEN];
	char *getnewpasswd(), *getlogin();

	uid = getuid();
	uname = getlogin();

#ifdef	KERBEROS
	while ((ch = getopt(argc, argv, ARGSTR)) != EOF)
		switch (ch) {
		/* change local password file */
		case 'l':
			use_kerberos = 0;
			break;
		default:
		case '?':
			usage();
			exit(1);
		}

	argc -= optind;
	argv += optind;
#endif

	switch(argc) {
	case 0:
		break;
	case 1:
#ifdef	KERBEROS
		if (use_kerberos && (strcmp(argv[1],uname) != 0)) { 
			fprintf(stderr,
				"must kinit to change another's password\n");
			exit(1);
		}
#endif
		uname = argv[0];
		break;
	default:
		usage();
		exit(1);
	}

#ifdef	KERBEROS
	if (use_kerberos) {
		exit(do_krb_passwd());
		/* NOTREACHED */
	}
#endif

	if (!(pw = getpwnam(uname))) {
		fprintf(stderr, "passwd: unknown user %s.\n", uname);
		exit(1);
	}
	if (uid && uid != pw->pw_uid) {
		fprintf(stderr, "passwd: %s\n", strerror(EACCES));
		exit(1);
	}

	(void)signal(SIGHUP, SIG_IGN);
	(void)signal(SIGINT, SIG_IGN);
	(void)signal(SIGQUIT, SIG_IGN);
	(void)signal(SIGTSTP, SIG_IGN);

	rlim.rlim_cur = rlim.rlim_max = RLIM_INFINITY;
	(void)setrlimit(RLIMIT_CPU, &rlim);
	(void)setrlimit(RLIMIT_FSIZE, &rlim);

	(void)umask(0);

	temp = _PATH_PTMP;
	if ((fd = open(temp, O_WRONLY|O_CREAT|O_EXCL, 0600)) < 0) {
		if (errno == EEXIST) {
			fprintf(stderr,
			    "passwd: password file busy -- try again later.\n");
			exit(0);
		}
		fprintf(stderr, "passwd: %s: %s", temp, strerror(errno));
		goto bad;
	}
	if (!(temp_fp = fdopen(fd, "w"))) {
		fprintf(stderr, "passwd: can't write %s", temp);
		goto bad;
	}
	passwd = _PATH_MASTERPASSWD;
	if (!freopen(passwd, "r", stdin)) {
		fprintf(stderr, "passwd: can't read %s", passwd);
		goto bad;
	}

	printf("Changing local password for %s.\n", pw->pw_name);
	np = getnewpasswd(pw, temp);

	if (!copy(pw->pw_name, np, temp_fp, pw))
		goto bad;

	(void)fclose(temp_fp);
	(void)fclose(stdin);

	switch(fork()) {
	case 0:
		break;
	case -1:
		fprintf(stderr, "passwd: can't fork");
		goto bad;
		/* NOTREACHED */
	default:
		exit(0);
		/* NOTREACHED */
	}

	if (makedb(temp)) {
		fprintf(stderr, "passwd: mkpasswd failed");
bad:		fprintf(stderr, "; password unchanged.\n");
		(void)unlink(temp);
		exit(1);
	}

	/*
	 * possible race; have to rename four files, and someone could slip
	 * in between them.  LOCK_EX and rename the ``passwd.dir'' file first
	 * so that getpwent(3) can't slip in; the lock should never fail and
	 * it's unclear what to do if it does.  Rename ``ptmp'' last so that
	 * passwd/vipw/chpass can't slip in.
	 */
	(void)setpriority(PRIO_PROCESS, 0, -20);
	fend = strcpy(from, temp) + strlen(temp);
	tend = strcpy(to, _PATH_PASSWD) + strlen(_PATH_PASSWD);
	bcopy(".dir", fend, 5);
	bcopy(".dir", tend, 5);
	if ((fd = open(from, O_RDONLY, 0)) >= 0)
		(void)flock(fd, LOCK_EX);
	/* here we go... */
	(void)rename(from, to);
	bcopy(".pag", fend, 5);
	bcopy(".pag", tend, 5);
	(void)rename(from, to);
	bcopy(".orig", fend, 6);
	(void)rename(from, _PATH_PASSWD);
	(void)rename(temp, passwd);
	/* done! */
	exit(0);
}

copy(name, np, fp, pw)
	char *name, *np;
	FILE *fp;
	struct passwd *pw;
{
	register int done;
	register char *p;
	char buf[1024];

	for (done = 0; fgets(buf, sizeof(buf), stdin);) {
		/* skip lines that are too big */
		if (!index(buf, '\n')) {
			fprintf(stderr, "passwd: line too long.\n");
			return(0);
		}
		if (done) {
			fprintf(fp, "%s", buf);
			continue;
		}
		if (!(p = index(buf, ':'))) {
			fprintf(stderr, "passwd: corrupted entry.\n");
			return(0);
		}
		*p = '\0';
		if (strcmp(buf, name)) {
			*p = ':';
			fprintf(fp, "%s", buf);
			continue;
		}
		if (!(p = index(++p, ':'))) {
			fprintf(stderr, "passwd: corrupted entry.\n");
			return(0);
		}
		/*
		 * reset change time to zero; when classes are implemented,
		 * go and get the "offset" value for this class and reset
		 * the timer.
		 */
		fprintf(fp, "%s:%s:%d:%d:%s:%ld:%ld:%s:%s:%s\n",
		    pw->pw_name, np, pw->pw_uid, pw->pw_gid,
		    pw->pw_class, 0L, pw->pw_expire, pw->pw_gecos,
		    pw->pw_dir, pw->pw_shell);
		done = 1;
	}
	return(1);
}

char *
getnewpasswd(pw, temp)
	register struct passwd *pw;
	char *temp;
{
	register char *p, *t;
	char buf[_PASSWORD_LEN+1], salt[2], *crypt(), *getpass();
	int tries = 0;
	time_t time();

	if (uid && pw->pw_passwd &&
	    strcmp(crypt(getpass("Old password:"), pw->pw_passwd),
	    pw->pw_passwd)) {
		(void)printf("passwd: %s.\n", strerror(EACCES));
		(void)unlink(temp);
		exit(1);
	}

	for (buf[0] = '\0';;) {
		p = getpass("New password:");
		if (!*p) {
			(void)printf("Password unchanged.\n");
			(void)unlink(temp);
			exit(0);
		}
		if (strlen(p) <= 5 && (uid != 0 || tries++ < 2)) {
			printf("Please enter a longer password.\n");
			continue;
		}
		for (t = p; *t && islower(*t); ++t);
		if (!*t && (uid != 0 || tries++ < 2)) {
			printf("Please don't use an all-lower case password.\nUnusual capitalization, control characters or digits are suggested.\n");
			continue;
		}
		(void)strcpy(buf, p);
		if (!strcmp(buf, getpass("Retype new password:")))
			break;
		printf("Mismatch; try again, EOF to quit.\n");
	}
	/* grab a random printable character that isn't a colon */
	(void)srandom((int)time((time_t *)NULL));
#ifdef NEWSALT
	salt[0] = '_';
	to64(&salt[1], (long)(29*25), 4);
	to64(&salt[5], (long)random(), 4);
#else
	to64(&salt[0], (long)random(), 2);
#endif
	return(crypt(buf, salt));
}

static unsigned char itoa64[] =		/* 0..63 => ascii-64 */
	"./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

to64(s, v, n)
	register char *s;
	register long v;
	register int n;
{
	while (--n >= 0) {
		*s++ = itoa64[v&0x3f];
		v >>= 6;
	}
}

makedb(file)
	char *file;
{
	union wait pstat;
	pid_t pid, waitpid();

	if (!(pid = vfork())) {
		execl(_PATH_MKPASSWD, "mkpasswd", "-p", file, NULL);
		_exit(127);
	}
	return(waitpid(pid, &pstat, 0) == -1 ? -1 : pstat.w_status);
}

usage()
{
#ifdef	KERBEROS
	fprintf(stderr, "usage: passwd [-l] user\n");
#else
	fprintf(stderr, "usage: passwd user\n");
#endif
}


#ifdef	KERBEROS 
KTEXT_ST	ticket;
long		authopts = 0L;
Key_schedule	random_schedule;
char		realm[REALM_SZ], krbhst[MAX_HSTNM];
static		struct	kpasswd_data	proto_data;
static		des_cblock		okey;
static		Key_schedule	osched;
int		sock;
int		finish();
#define		PROTO	"tcp"

static struct timeval	timeout = { CLIENT_KRB_TIMEOUT, 0 };

do_krb_passwd()
{
	struct servent *se;
	struct hostent *host;
	struct sockaddr_in sin;
	int rval;
	char pass[_PASSWORD_LEN], password[_PASSWORD_LEN];
	fd_set readfds;
	CREDENTIALS	cred;

	static struct rlimit rl = { 0, 0 };

	(void)signal(SIGHUP, SIG_IGN);
	(void)signal(SIGINT, SIG_IGN);
	(void)signal(SIGTSTP, SIG_IGN);

	if (setrlimit(RLIMIT_CORE, &rl) < 0) {
		perror("setrlimit");
		return(1);
	}

	if ((se = getservbyname(SERVICE, PROTO)) == NULL) {
		fprintf(stderr, "couldn't find entry for service %s/%s\n",
			SERVICE, PROTO);
		return(1);
	}

	if ((rval = krb_get_lrealm(realm,1)) != KSUCCESS) {
		fprintf(stderr, "couldn't get local Kerberos realm: %s\n",
			krb_err_txt[rval]);
		return(1);
	}

	if ((rval = krb_get_krbhst(krbhst, realm, 1)) != KSUCCESS) {
		fprintf(stderr, "couldn't get Kerberos host: %s\n",
			krb_err_txt[rval]);
		return(1);
	}

	if ((host = gethostbyname(krbhst)) == NULL) {
		fprintf(stderr, "couldn't get host entry for krb host %s\n",
			krbhst);
		return(1);
	}

	sin.sin_family = host->h_addrtype;
	bcopy(host->h_addr, (char *) &sin.sin_addr, host->h_length);
	sin.sin_port = se->s_port;

	if ((sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
		perror("socket");
		return(1);
	}

	if (connect(sock, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
		perror("connect");
		close(sock);
		return(1);
	}

	rval = krb_sendauth(
		authopts,		/* NOT mutual */
		sock,
		&ticket,		/* (filled in) */
		SERVICE,
		krbhst,			/* instance (krbhst) */
		realm,			/* dest realm */
		(u_long) getpid(),	/* checksum */
		NULL,			/* msg data */
		NULL,			/* credentials */ 
		NULL,			/* schedule */
		NULL,			/* local addr */
		NULL,			/* foreign addr */
		"KPWDV0.1"
	);


	if (rval != KSUCCESS) {
		fprintf(stderr, "Kerberos sendauth error: %s\n",
			krb_err_txt[rval]);
		return(1);
	}

	krb_get_cred("krbtgt", realm, realm, &cred);

	printf("Changing Kerberos password for %s.%s@%s.\n",
		cred.pname, cred.pinst, realm);

	if (des_read_pw_string(pass,
	    sizeof(pass)-1, "Old Kerberos password:", 0)) {
		fprintf(stderr,
			"error reading old Kerberos password\n");
		return(1);
	}

	(void)des_string_to_key(pass, okey);
	(void)des_key_sched(okey, osched);
	(void)des_set_key(okey, osched);

	/* wait on the verification string */

	FD_ZERO(&readfds);
	FD_SET(sock, &readfds);

	rval =
	  select(sock + 1, &readfds, (fd_set *) 0, (fd_set *) 0, &timeout);

	if ((rval < 1) || !FD_ISSET(sock, &readfds)) {
		if(rval == 0) {
			fprintf(stderr, "timed out (aborted)\n");
			cleanup();
			return(1);
		}
		fprintf(stderr, "select failed (aborted)\n");
		cleanup();
		return(1);
	}

	/* read verification string */

	if (des_read(sock, &proto_data, sizeof(proto_data)) !=
	    sizeof(proto_data)) {
		fprintf(stderr,
		    "couldn't read verification string (aborted)\n");
		cleanup();
		return(1);
	}

	(void)signal(SIGHUP, finish);
	(void)signal(SIGINT, finish);

	if (strcmp(SECURE_STRING, proto_data.secure_msg) != 0) {
		cleanup();
		/* don't complain loud if user just hit return */
		if (pass == NULL || (!*pass))
			return(0);
		fprintf(stderr, "Sorry\n");
		return(1);
	}

	(void)des_key_sched(proto_data.random_key, random_schedule);
	(void)des_set_key(proto_data.random_key, random_schedule);
	(void)bzero(pass, sizeof(pass));

	if (des_read_pw_string(pass,
	    sizeof(pass)-1, "New Kerberos password:", 0)) {
		fprintf(stderr,
			"error reading new Kerberos password (aborted)\n");
		cleanup();
		return(1);
	}

	if (des_read_pw_string(password,
	    sizeof(password)-1, "Retype new Kerberos password:", 0)) {
		fprintf(stderr,
			"error reading new Kerberos password (aborted)\n");
		cleanup();
		return(1);
	}

	if (strcmp(password, pass) != 0) {
		fprintf(stderr, "password mismatch (aborted)\n");
		cleanup();
		return(1);
	}

	if (strlen(pass) == 0)
		printf("using NULL password\n");

	send_update(sock, password, SECURE_STRING);

	/* wait for ACK */

	FD_ZERO(&readfds);
	FD_SET(sock, &readfds);

	rval =
	  select(sock + 1, &readfds, (fd_set *) 0, (fd_set *) 0, &timeout);
	if ((rval < 1) || !FD_ISSET(sock, &readfds)) {
		if(rval == 0) {
			fprintf(stderr, "timed out reading ACK (aborted)\n");
			cleanup();
			exit(1);
		}
		fprintf(stderr, "select failed (aborted)\n");
		cleanup();
		exit(1);
	}

	recv_ack(sock);
	cleanup();
	exit(0);
}

send_update(dest, pwd, str)
	int	dest;
	char	*pwd, *str;
{
	static struct	update_data	ud;
	strncpy(ud.secure_msg, str, _PASSWORD_LEN);
	strncpy(ud.pw, pwd, sizeof(ud.pw));
	if (des_write(dest, &ud, sizeof(ud)) != sizeof(ud)) {
		fprintf(stderr, "couldn't write pw update (abort)\n");
		bzero(ud, sizeof(ud));
		cleanup();
		exit(1);
	}
}

recv_ack(remote)
	int	remote;
{
	int	cc;
	char	buf[BUFSIZ];
	cc = des_read(remote, buf, sizeof(buf));
	if (cc <= 0) {
		fprintf(stderr, "error reading acknowledgement (aborted)\n");
		cleanup();
		exit(1);
	}
	printf("%s", buf);
}

cleanup()
{
	(void)bzero(&proto_data, sizeof(proto_data));
	(void)bzero(okey, sizeof(okey));
	(void)bzero(osched, sizeof(osched));
	(void)bzero(random_schedule, sizeof(random_schedule));
}

finish()
{
	(void)close(sock);
	exit(1);
}

#endif /* KERBEROS */
