static char *sccsid = "@(#)who.c	4.4 (Berkeley) %G%";
/*
 * who
 */

#include <stdio.h>
#include <utmp.h>
#include <pwd.h>
#include <ctype.h>

#define NMAX sizeof(utmp.ut_name)
#define LMAX sizeof(utmp.ut_line)

struct	utmp utmp;
struct	passwd *pw;
struct	passwd *getpwuid();
char	hostname[32];

char	*ttyname(), *rindex(), *ctime(), *strcpy();

main(argc, argv)
	int argc;
	char **argv;
{
	register char *tp, *s;
	register FILE *fi;
	extern char _sobuf[];

	setbuf(stdout, _sobuf);
	s = "/etc/utmp";
	if(argc == 2)
		s = argv[1];
	if (argc==3) {
		tp = ttyname(0);
		if (tp)
			tp = rindex(tp, '/') + 1;
		else {	/* no tty - use best guess from passwd file */
			pw = getpwuid(getuid());
			strcpy(utmp.ut_name, pw?pw->pw_name: "?");
			strcpy(utmp.ut_line, "tty??");
			time(&utmp.ut_time);
			putline();
			exit(0);
		}
	}
	if ((fi = fopen(s, "r")) == NULL) {
		puts("who: cannot open utmp");
		exit(1);
	}
	while (fread((char *)&utmp, sizeof(utmp), 1, fi) == 1) {
		if(argc==3) {
			gethostname(hostname, sizeof (hostname));
			if (strcmp(utmp.ut_line, tp))
				continue;
			printf("%s!", hostname);
			putline();
			exit(0);
		}
		if(utmp.ut_name[0] == '\0' && argc==1)
			continue;
		putline();
	}
}

putline()
{
	register char *cbuf;

	printf("%-*.*s %-*.*s", NMAX, NMAX, utmp.ut_name, LMAX, LMAX, utmp.ut_line);
	cbuf = ctime(&utmp.ut_time);
	printf("%.12s\n", cbuf+4);
}
