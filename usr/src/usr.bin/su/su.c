/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)su.c	5.20 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <syslog.h>
#include <stdio.h>
#include <pwd.h>
#include <grp.h>
#include <string.h>
#include <unistd.h>
#include "pathnames.h"

#ifdef KERBEROS
#include <kerberosIV/des.h>
#include <kerberosIV/krb.h>
#include <netdb.h>

#define	ARGSTR	"-Kflm"

int use_kerberos = 1;
#else
#define	ARGSTR	"-flm"
#endif

main(argc, argv)
	int argc;
	char **argv;
{
	extern char **environ;
	extern int errno, optind;
	register struct passwd *pwd;
	register char *p, **g;
	struct group *gr;
	uid_t ruid, getuid();
	int asme, ch, asthem, fastlogin, prio;
	enum { UNSET, YES, NO } iscsh = UNSET;
	char *user, *shell, *username, *cleanenv[2], *nargv[4], **np;
	char shellbuf[MAXPATHLEN];
	char *crypt(), *getpass(), *getenv(), *getlogin(), *mytty();

	np = &nargv[3];
	*np-- = NULL;
	asme = asthem = fastlogin = 0;
	while ((ch = getopt(argc, argv, ARGSTR)) != EOF)
		switch((char)ch) {
#ifdef KERBEROS
		case 'K':
			use_kerberos = 0;
			break;
#endif
		case 'f':
			fastlogin = 1;
			break;
		case '-':
		case 'l':
			asme = 0;
			asthem = 1;
			break;
		case 'm':
			asme = 1;
			asthem = 0;
			break;
		case '?':
		default:
			(void)fprintf(stderr, "usage: su [%s] [login]\n",
			    ARGSTR);
			exit(1);
		}
	argv += optind;

	errno = 0;
	prio = getpriority(PRIO_PROCESS, 0);
	if (errno)
		prio = 0;
	(void)setpriority(PRIO_PROCESS, 0, -2);

	/* get current login name and shell */
	if ((pwd = getpwuid(ruid = getuid())) == NULL) {
		fprintf(stderr, "su: who are you?\n");
		exit(1);
	}
	username = strdup(pwd->pw_name);
	if (asme)
		if (pwd->pw_shell && *pwd->pw_shell)
			shell = strcpy(shellbuf,  pwd->pw_shell);
		else {
			shell = _PATH_BSHELL;
			iscsh = NO;
		}

	/* get target login information, default to root */
	user = *argv ? *argv : "root";
	if ((pwd = getpwnam(user)) == NULL) {
		fprintf(stderr, "su: unknown login %s\n", user);
		exit(1);
	}

	/* only allow those in group zero to su to root. */
	if (pwd->pw_uid == 0 && (gr = getgrgid((gid_t)0)))
		for (g = gr->gr_mem;; ++g) {
			if (!*g) {
				(void)fprintf(stderr,
				    "su: you are not in the correct group to su %s.\n", user);
				exit(1);
			}
			if (!strcmp(username, *g))
				break;
		}
	openlog("su", LOG_CONS, 0);

	if (ruid) {
#ifdef KERBEROS
		if (!use_kerberos || kerberos(username, user, pwd->pw_uid))
#endif
		/* if target requires a password, verify it */
		if (*pwd->pw_passwd) {
			p = getpass("Password:");
			if (strcmp(pwd->pw_passwd, crypt(p, pwd->pw_passwd))) {
				fprintf(stderr, "Sorry\n");
				if (pwd->pw_uid == 0)
					syslog(LOG_AUTH|LOG_CRIT,
					    "BAD SU %s on %s", username,
					    mytty());
				exit(1);
			}
		}
	}

	if (asme) {
		/* if asme and non-standard target shell, must be root */
		if (!chshell(pwd->pw_shell) && ruid) {
			(void)fprintf(stderr, "su: permission denied.\n");
			exit(1);
		}
	} else if (pwd->pw_shell && *pwd->pw_shell) {
		shell = pwd->pw_shell;
		iscsh = UNSET;
	} else {
		shell = _PATH_BSHELL;
		iscsh = NO;
	}

	/* if we're forking a csh, we want to slightly muck the args */
	if (iscsh == UNSET) {
		if (p = rindex(shell, '/'))
			++p;
		else
			p = shell;
		iscsh = strcmp(p, "csh") ? NO : YES;
	}

	/* set permissions */
	if (setgid(pwd->pw_gid) < 0) {
		perror("su: setgid");
		exit(1);
	}
	if (initgroups(user, pwd->pw_gid)) {
		(void)fprintf(stderr, "su: initgroups failed.\n");
		exit(1);
	}
	if (setuid(pwd->pw_uid) < 0) {
		perror("su: setuid");
		exit(1);
	}

	if (!asme) {
		if (asthem) {
			p = getenv("TERM");
			cleanenv[0] = _PATH_SEARCHPATH;
			cleanenv[1] = NULL;
			environ = cleanenv;
			(void)setenv("TERM", p, 1);
			if (chdir(pwd->pw_dir) < 0) {
				fprintf(stderr, "su: no directory\n");
				exit(1);
			}
		}
		if (asthem || pwd->pw_uid)
			(void)setenv("USER", pwd->pw_name, 1);
		(void)setenv("HOME", pwd->pw_dir, 1);
		(void)setenv("SHELL", shell, 1);
	}

	if (iscsh == YES) {
		if (fastlogin)
			*np-- = "-f";
		if (asme)
			*np-- = "-m";
	}

	/* csh strips the first character... */
	*np = asthem ? "-su" : iscsh == YES ? "_su" : "su";

	if (pwd->pw_uid == 0)
		syslog(LOG_NOTICE|LOG_AUTH, "%s on %s", username, mytty());

	(void)setpriority(PRIO_PROCESS, 0, prio);

	execv(shell, np);
	(void)fprintf(stderr, "su: %s not found.\n", shell);
	exit(1);
}

chshell(sh)
	char *sh;
{
	register char *cp;
	char *getusershell();

	while ((cp = getusershell()) != NULL)
		if (!strcmp(cp, sh))
			return(1);
	return(0);
}

char *
mytty()
{
	char *p, *ttyname();

	return((p = ttyname(STDERR_FILENO)) ? p : "UNKNOWN TTY");
}

#ifdef KERBEROS
kerberos(username, user, uid)
	char *username, *user;
	int uid;
{
	extern char *krb_err_txt[];
	KTEXT_ST ticket;
	AUTH_DAT authdata;
	struct hostent *hp;
	register char *p;
	int kerno;
	u_long faddr;
	char lrealm[REALM_SZ], krbtkfile[MAXPATHLEN], pw_buf[_PASSWORD_LEN];
	char hostname[MAXHOSTNAMELEN], savehost[MAXHOSTNAMELEN];
	char *mytty();

	if (krb_get_lrealm(lrealm, 1) != KSUCCESS) {
		(void)fprintf(stderr, "su: couldn't get local realm.\n");
		return(1);
	}
	if (koktologin(username, lrealm, user) && !uid) {
		(void)fprintf(stderr, "kerberos su: not in %s's ACL.\n", user);
		return(1);
	}
	(void)sprintf(krbtkfile, "%s_%s_%d", TKT_ROOT, user, getuid());
	/* setuid(uid); */

	if (read_pw_string(pw_buf, sizeof(pw_buf) - 1,
	    "Kerberos password: ", 0)) {
		(void)fprintf(stderr, "su: error reading password.\n");
		return(1);
	}

	(void)setenv("KRBTKFILE", krbtkfile, 1);
	/* short lifetime for root tickets */
	if (setuid(0) < 0) {
		perror("su: setuid");
		return(1);
	}
	(void)unlink(krbtkfile);
	/* POLICY: short ticket lifetime for root */
	kerno = krb_get_pw_in_tkt(username, (uid == 0 ? "root" : ""), lrealm,
	    "krbtgt", lrealm, (uid == 0 ? 2 : DEFAULT_TKT_LIFE), pw_buf);

	bzero(pw_buf, sizeof(pw_buf));

	if (kerno != KSUCCESS) {
		if (kerno == KDC_PR_UNKNOWN)
			return(1);
		(void)printf("su: unable to su: %s\n", krb_err_txt[kerno]);
		syslog(LOG_NOTICE|LOG_AUTH,
		    "su: BAD Kerberos SU: %s on %s: %s", username, mytty(),
		    krb_err_txt[kerno]);
		return(1);
	}

	if (chown(krbtkfile, uid, -1) < 0) {
		perror("su: chown:");
		(void)unlink(krbtkfile);
		return(1);
	}

	(void)setpriority(PRIO_PROCESS, 0, -2);

	if (gethostname(hostname, sizeof(hostname)) == -1) {
		perror("su: hostname");
		dest_tkt();
		return(1);
	}

	(void)strncpy(savehost, krb_get_phost(hostname), sizeof(savehost));
	savehost[sizeof(savehost) - 1] = '\0';

	kerno = krb_mk_req(&ticket, "rcmd", savehost, lrealm, 33);

	if (kerno == KDC_PR_UNKNOWN) {
		(void)printf("Warning: tgt not verified.\n");
		syslog(LOG_NOTICE|LOG_AUTH, "su: %s on %s, tgt not verified",
		    username, mytty());
	} else if (kerno != KSUCCESS) {
		(void)printf("Unable to use tgt: %s\n", krb_err_txt[kerno]);
		syslog(LOG_NOTICE|LOG_AUTH, "su: failed su: %s on %s: %s",
		    username, mytty(), krb_err_txt[kerno]);
		dest_tkt();
		return(1);
	} else {
		if (!(hp = gethostbyname(hostname))) {
			(void)printf("su: can't get addr of %s\n", hostname);
			dest_tkt();
			return(1);
		}
		(void)bcopy((char *)hp->h_addr, (char *)&faddr, sizeof(faddr));

		if ((kerno = krb_rd_req(&ticket, "rcmd", savehost, faddr,
		    &authdata, "")) != KSUCCESS) {
			(void)printf("su: unable to verify rcmd ticket: %s\n",
			    krb_err_txt[kerno]);
			syslog(LOG_NOTICE|LOG_AUTH,
			    "su: failed su: %s on %s: %s", username,
			    mytty(), krb_err_txt[kerno]);
			dest_tkt();
			return(1);
		}
	}
	return(0);
}

koktologin(name, realm, toname)
	char *name, *realm, *toname;
{
	register AUTH_DAT *kdata;
	AUTH_DAT kdata_st;

	kdata = &kdata_st;
	bzero((caddr_t) kdata, sizeof(*kdata));
	(void)strcpy(kdata->pname, name);
	(void)strcpy(kdata->pinst,
	    ((strcmp(toname, "root") == 0) ? "root" : ""));
	(void)strcpy(kdata->prealm, realm);
	return(kuserok(kdata, toname));
}
#endif
