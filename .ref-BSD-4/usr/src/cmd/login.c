static	char *sccsid = "@(#)login.c	4.6 (Berkeley) 11/11/80";
/*
 * login [ name ]
 */

#include <sys/types.h>
#include <sgtty.h>
#include <utmp.h>
#include <signal.h>
#include <pwd.h>
#include <stdio.h>
#include <sys/stat.h>
#include <lastlog.h>
#include <whoami.h>
#define SCPYN(a, b)	strncpy(a, b, sizeof(a))

#define NMAX sizeof(utmp.ut_name)
#define LMAX sizeof(utmp.ut_line)

char	user[20];
char	maildir[30] =	"/usr/spool/mail/";
char	lastlog[] =	"/usr/adm/lastlog";
struct	passwd nouser = {"", "nope"};
struct	sgttyb ttyb;
struct	utmp utmp;
char	minusnam[16] = "-";
char	homedir[64] = "HOME=";
char	shell[64] = "SHELL=";
char	term[64] = "TERM=";
char	*envinit[] = {homedir, shell, "PATH=:/usr/ucb:/bin:/usr/bin", term, user,0};
struct	passwd *pwd;

struct	passwd *getpwnam();
char	*strcat();
int	setpwent();
char	*ttyname();
char	*crypt();
char	*getpass();
char	*rindex();
char	*stypeof();
extern	char **environ;

#define	CTRL(c)	('c'&037)
#define	CERASE	'#'
#define	CEOT	CTRL(d)
#define	CKILL	'@'
#define	CQUIT	034		/* FS, cntl shift L */
#define	CINTR	0177		/* DEL */
#define	CSTOP	CTRL(s)
#define	CSTART	CTRL(q)
#define	CBRK	0377
struct	tchars tc = {
	CINTR, CQUIT, CSTART, CSTOP, CEOT, CBRK
};
struct	ltchars ltc = {
	CTRL(z), CTRL(y), CTRL(r), CTRL(o), CTRL(w), CTRL(v)
};

main(argc, argv)
char **argv;
{
	register char *namep;
	int t, f, c;
	char *ttyn;
	int ldisc = 0;

	alarm(60);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	nice(-100);
	nice(20);
	nice(0);
	ioctl(0, TIOCLSET, 0);
	ioctl(0, TIOCNXCL, 0);
	gtty(0, &ttyb);
	ttyb.sg_erase = '#';
	ttyb.sg_kill = '@';
	stty(0, &ttyb);
	ioctl(0, TIOCSETC, &tc);
	ioctl(0, TIOCSLTC, &ltc);
	for (t=3; t<20; t++)
		close(t);
	ttyn = ttyname(0);
	if (ttyn==0)
		ttyn = "/dev/tty??";

    loop:
	ldisc = 0;
	ioctl(0, TIOCSETD, &ldisc);
	SCPYN(utmp.ut_name, "");
	if (argc>1) {
		SCPYN(utmp.ut_name, argv[1]);
		argc = 0;
	}
	while (utmp.ut_name[0] == '\0') {
		namep = utmp.ut_name;
		printf("login: ");
		while ((c = getchar()) != '\n') {
			if(c == ' ')
				c = '_';
			if (c == EOF)
				exit(0);
			if (namep < utmp.ut_name+NMAX)
				*namep++ = c;
		}
	}
	setpwent();
	if ((pwd = getpwnam(utmp.ut_name)) == NULL)
		pwd = &nouser;
	endpwent();
	if (!strcmp(pwd->pw_shell, "/bin/csh")) {
		ldisc = NTTYDISC;
		ioctl(0, TIOCSETD, &ldisc);
	}
	if (*pwd->pw_passwd != '\0') {
		nice(-4);
		namep = crypt(getpass("Password:"),pwd->pw_passwd);
		nice(4);
		if (strcmp(namep, pwd->pw_passwd)) {
bad:
			printf("Login incorrect\n");
			if (ttyn[LMAX] == 'd') {
				FILE *console = fopen("/dev/console", "w");
				if (console != NULL) {
					fprintf(console, "\r\nBADDIALUP %s %s\r\n", ttyn+5, utmp.ut_name);
					fclose(console);
				}
			}
			goto loop;
		}
	}
	sprintf(user, "USER=%.*s", NMAX, pwd->pw_name);
#ifdef ERNIE
	if (pwd->pw_uid == 0 && ttyn[5] != 'c')
		goto bad;
#endif
	if (ttyn[LMAX] == 'd') {
		FILE *console = fopen("/dev/console", "w");
		if (console != NULL) {
			fprintf(console, "\r\nDIALUP %s %s\r\n", ttyn+5, pwd->pw_name);
			fclose(console);
		}
	}
	if((f = open(lastlog, 2)) >= 0) {
		struct lastlog ll;

		lseek(f, (long) pwd->pw_uid * sizeof (struct lastlog), 0);
		if (read(f, (char *) &ll, sizeof ll) == sizeof ll && ll.ll_time != 0) {
			register char *ep = (char *) ctime(&ll.ll_time);
			printf("Last login: ");
			ep[24 - 5] = 0;
			printf("%s on %.*s\n", ep, LMAX, ll.ll_line);
		}
		lseek(f, (long) pwd->pw_uid * sizeof (struct lastlog), 0);
		time(&ll.ll_time);
		strcpyn(ll.ll_line, ttyn+5, LMAX);
		write(f, (char *) &ll, sizeof ll);
		close(f);
	}
	if(chdir(pwd->pw_dir) < 0) {
		printf("No directory\n");
		goto loop;
	}
	time(&utmp.ut_time);
	t = ttyslot();
	if (t>0 && (f = open("/etc/utmp", 1)) >= 0) {
		lseek(f, (long)(t*sizeof(utmp)), 0);
		SCPYN(utmp.ut_line, rindex(ttyn, '/')+1);
		write(f, (char *)&utmp, sizeof(utmp));
		close(f);
	}
	if (t>0 && (f = open("/usr/adm/wtmp", 1)) >= 0) {
		lseek(f, 0L, 2);
		write(f, (char *)&utmp, sizeof(utmp));
		close(f);
	}
	chown(ttyn, pwd->pw_uid, pwd->pw_gid);
	setgid(pwd->pw_gid);
	setuid(pwd->pw_uid);
	if (*pwd->pw_shell == '\0')
		pwd->pw_shell = "/bin/sh";
	environ = envinit;
	strncat(homedir, pwd->pw_dir, sizeof(homedir)-6);
	strncat(shell, pwd->pw_shell, sizeof(shell)-7);
	strncat(term, stypeof(ttyn), sizeof(term)-6);
	if ((namep = rindex(pwd->pw_shell, '/')) == NULL)
		namep = pwd->pw_shell;
	else
		namep++;
	strcat(minusnam, namep);
	alarm(0);
	umask(022);
	showmotd();
	strcat(maildir, pwd->pw_name);
	if(access(maildir,4)==0) {
		struct stat statb;
		stat(maildir, &statb);
		if (statb.st_size)
			printf("You have mail.\n");
	}
	signal(SIGQUIT, SIG_DFL);
	signal(SIGINT, SIG_DFL);
	execlp(pwd->pw_shell, minusnam, 0);
	printf("No shell\n");
	exit(0);
}

int	stopmotd;
catch()
{
	signal(SIGINT, SIG_IGN);
	stopmotd++;
}

showmotd()
{
	FILE *mf;
	register c;

	signal(SIGINT, catch);
	if((mf = fopen("/etc/motd","r")) != NULL) {
		while((c = getc(mf)) != EOF && stopmotd == 0)
			putchar(c);
		fclose(mf);
	}
	signal(SIGINT, SIG_IGN);
}

#define UNKNOWN "su"

char *
stypeof(ttyid)
char	*ttyid;
{
	static char	typebuf[16];
	char		buf[50];
	register FILE	*f;
	register char	*p, *t, *q;

	if (ttyid == NULL)
		return (UNKNOWN);
	f = fopen("/etc/ttytype", "r");
	if (f == NULL)
		return (UNKNOWN);
	/* split off end of name */
	for (p = q = ttyid; *p != 0; p++)
		if (*p == '/')
			q = p + 1;

	/* scan the file */
	while (fgets(buf, sizeof buf, f) != NULL)
	{
		for (t=buf; *t!=' '; t++)
			;
		*t++ = 0;
		for (p=t; *p>' '; p++)
			;
		*p = 0;
		if (strcmp(q,t)==0) {
			strcpy(typebuf, buf);
			fclose(f);
			return (typebuf);
		}
	}
	fclose (f);
	return (UNKNOWN);
}
