static	char sccsid[] = "@(#)cc.c 4.21 6/30/90";
/*
 * cc - front end for C compiler
 */
#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <sys/dir.h>
#include "pathnames.h"

char	*cpp = _PATH_CPP;
char	*ccom = _PATH_CCOM;
char	*sccom = _PATH_SCCOM;
char	*c2 = _PATH_C2;
char	*as = _PATH_AS;
char	*ld = _PATH_LD;
char	*crt0 = _PATH_CRT0;

char	tmp0[MAXPATHLEN];
char	*tmp1, *tmp2, *tmp3, *tmp4, *tmp5;
char	*outfile;
char	*savestr(), *strspl(), *setsuf();
int	idexit();
char	**av, **clist, **llist, **plist;
int	cflag, eflag, oflag, pflag, sflag, wflag, Rflag, exflag, proflag;
int	fflag, gflag, Gflag, Mflag, debug;
char	*dflag;
int	exfail;
char	*chpass;
char	*npassname;

int	nc, nl, np, nxo, na;

#define	cunlink(s)	if (s) unlink(s)

main(argc, argv)
	char **argv;
{
	char *t;
	char *assource;
	int i, j, c;

	/* ld currently adds upto 5 args; 10 is room to spare */
	av = (char **)calloc(argc+10, sizeof (char **));
	clist = (char **)calloc(argc, sizeof (char **));
	llist = (char **)calloc(argc, sizeof (char **));
	plist = (char **)calloc(argc, sizeof (char **));
	for (i = 1; i < argc; i++) {
		if (*argv[i] == '-') switch (argv[i][1]) {

		case 'S':
			sflag++;
			cflag++;
			continue;
		case 'o':
			if (++i < argc) {
				outfile = argv[i];
				switch (getsuf(outfile)) {

				case 'c':
					error("-o would overwrite %s",
					    outfile);
					exit(8);
				}
			}
			continue;
		case 'R':
			Rflag++;
			continue;
		case 'O':
			oflag++;
			continue;
		case 'p':
			proflag++;
			crt0 = _PATH_MCRT0;
			if (argv[i][2] == 'g')
				crt0 = _PATH_GCRT0;
			continue;
		case 'f':
			fflag++;
			continue;
		case 'g':
			if (argv[i][2] == 'o') {
			    Gflag++;	/* old format for -go */
			} else {
			    gflag++;	/* new format for -g */
			}
			continue;
		case 'w':
			wflag++;
			continue;
		case 'E':
			exflag++;
		case 'P':
			pflag++;
			if (argv[i][1]=='P')
				fprintf(stderr,
	"cc: warning: -P option obsolete; you should use -E instead\n");
			plist[np++] = argv[i];
		case 'c':
			cflag++;
			continue;
		case 'M':
			exflag++;
			pflag++;
			Mflag++;
			/* and fall through */
		case 'D':
		case 'I':
		case 'U':
		case 'C':
			plist[np++] = argv[i];
			continue;
		case 'L':
			llist[nl++] = argv[i];
			continue;
		case 't':
			if (chpass)
				error("-t overwrites earlier option", 0);
			chpass = argv[i]+2;
			if (chpass[0]==0)
				chpass = "012p";
			continue;
		case 'B':
			if (npassname)
				error("-B overwrites earlier option", 0);
			npassname = argv[i]+2;
			if (npassname[0]==0)
				error("-B requires an argument", 0);
			continue;
		case 'd':
			if (argv[i][2] == '\0') {
				debug++;
				continue;
			}
			dflag = argv[i];
			continue;
		}
		t = argv[i];
		c = getsuf(t);
		if (c=='c' || c=='s' || exflag) {
			clist[nc++] = t;
			t = setsuf(t, 'o');
		}
		if (nodup(llist, t)) {
			llist[nl++] = t;
			if (getsuf(t)=='o')
				nxo++;
		}
	}
	if (gflag || Gflag) {
		if (oflag)
			fprintf(stderr, "cc: warning: -g disables -O\n");
		oflag = 0;
	}
	if (npassname && chpass ==0)
		chpass = "012p";
	if (chpass && npassname==0)
		npassname = _PATH_USRNEW;
	if (chpass)
	for (t=chpass; *t; t++) {
		switch (*t) {

		case '0':
			if (fflag)
				sccom = strspl(npassname, "sccom");
			else
				ccom = strspl(npassname, "ccom");
			continue;
		case '2':
			c2 = strspl(npassname, "c2");
			continue;
		case 'p':
			cpp = strspl(npassname, "cpp");
			continue;
		}
	}
	if (nc==0)
		goto nocom;
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, idexit);
	if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
		signal(SIGTERM, idexit);
	if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
		signal(SIGHUP, idexit);
	if (pflag==0)
		(void)sprintf(tmp0, "%s/ctm%05.5d", _PATH_TMP, getpid());
	tmp1 = strspl(tmp0, "1");
	tmp2 = strspl(tmp0, "2");
	tmp3 = strspl(tmp0, "3");
	if (pflag==0)
		tmp4 = strspl(tmp0, "4");
	if (oflag)
		tmp5 = strspl(tmp0, "5");
	for (i=0; i<nc; i++) {
		if (nc > 1 && !Mflag) {
			printf("%s:\n", clist[i]);
			fflush(stdout);
		}
		if (!Mflag && getsuf(clist[i]) == 's') {
			assource = clist[i];
			goto assemble;
		} else
			assource = tmp3;
		if (pflag)
			tmp4 = setsuf(clist[i], 'i');
		av[0] = "cpp"; av[1] = clist[i];
		na = 2;
		if (!exflag)
			av[na++] = tmp4;
		for (j = 0; j < np; j++)
			av[na++] = plist[j];
		av[na++] = 0;
		if (callsys(cpp, av)) {
			exfail++;
			eflag++;
			cflag++;
			continue;
		}
		if (pflag) {
			cflag++;
			continue;
		}
		if (sflag) {
			if (nc==1 && outfile)
				tmp3 = outfile;
			else
				tmp3 = setsuf(clist[i], 's');
			assource = tmp3;
		}
		av[0] = fflag ? "sccom" : "ccom";
		av[1] = tmp4; av[2] = oflag?tmp5:tmp3; na = 3;
		if (proflag)
			av[na++] = "-XP";
		if (gflag) {
			av[na++] = "-Xg";
		} else if (Gflag) {
			av[na++] = "-XG";
		}
		if (wflag)
			av[na++] = "-w";
		av[na] = 0;
		if (callsys(fflag ? sccom : ccom, av)) {
			cflag++;
			eflag++;
			continue;
		}
		if (oflag) {
			av[0] = "c2"; av[1] = tmp5; av[2] = tmp3; av[3] = 0;
			if (callsys(c2, av)) {
				unlink(tmp3);
				tmp3 = assource = tmp5;
			} else
				unlink(tmp5);
		}
		if (sflag)
			continue;
	assemble:
		cunlink(tmp1); cunlink(tmp2); cunlink(tmp4);
		av[0] = "as"; av[1] = "-o";
		if (cflag && nc==1 && outfile)
			av[2] = outfile;
		else
			av[2] = setsuf(clist[i], 'o');
		na = 3;
		if (Rflag)
			av[na++] = "-R";
		if (dflag)
			av[na++] = dflag;
		av[na++] = assource;
		av[na] = 0;
		if (callsys(as, av) > 1) {
			cflag++;
			eflag++;
			continue;
		}
	}
nocom:
	if (cflag==0 && nl!=0) {
		i = 0;
		av[0] = "ld"; av[1] = "-X"; av[2] = crt0; na = 3;
		if (outfile) {
			av[na++] = "-o";
			av[na++] = outfile;
		}
		while (i < nl)
			av[na++] = llist[i++];
		if (proflag)
			av[na++] = "-lc_p";
		else
			av[na++] = "-lc";
		av[na++] = 0;
		eflag |= callsys(ld, av);
		if (nc==1 && nxo==1 && eflag==0)
			unlink(setsuf(clist[0], 'o'));
	}
	dexit();
}

idexit()
{

	eflag = 100;
	dexit();
}

dexit()
{

	if (!pflag) {
		cunlink(tmp1);
		cunlink(tmp2);
		if (sflag==0)
			cunlink(tmp3);
		cunlink(tmp4);
		cunlink(tmp5);
	}
	exit(eflag);
}

error(s, x)
	char *s, *x;
{
	FILE *diag = exflag ? stderr : stdout;

	fprintf(diag, "cc: ");
	fprintf(diag, s, x);
	putc('\n', diag);
	exfail++;
	cflag++;
	eflag++;
}

getsuf(as)
char as[];
{
	register int c;
	register char *s;
	register int t;

	s = as;
	c = 0;
	while (t = *s++)
		if (t=='/')
			c = 0;
		else
			c++;
	s -= 3;
	if (c <= MAXNAMLEN && c > 2 && *s++ == '.')
		return (*s);
	return (0);
}

char *
setsuf(as, ch)
	char *as;
{
	register char *s, *s1;

	s = s1 = savestr(as);
	while (*s)
		if (*s++ == '/')
			s1 = s;
	s[-1] = ch;
	return (s1);
}

callsys(f, v)
	char *f, **v;
{
	int t, status;
	char **cpp;

	if (debug) {
		fprintf(stderr, "%s:", f);
		for (cpp = v; *cpp != 0; cpp++)
			fprintf(stderr, " %s", *cpp);
		fprintf(stderr, "\n");
	}
	t = vfork();
	if (t == -1) {
		printf("No more processes\n");
		return (100);
	}
	if (t == 0) {
		execv(f, v);
		printf("Can't find %s\n", f);
		fflush(stdout);
		_exit(100);
	}
	while (t != wait(&status))
		;
	if ((t=(status&0377)) != 0 && t!=14) {
		if (t!=2) {
			printf("Fatal error in %s\n", f);
			eflag = 8;
		}
		dexit();
	}
	return ((status>>8) & 0377);
}

nodup(l, os)
	char **l, *os;
{
	register char *t, *s;
	register int c;

	s = os;
	if (getsuf(s) != 'o')
		return (1);
	while (t = *l++) {
		while (c = *s++)
			if (c != *t++)
				break;
		if (*t==0 && c==0)
			return (0);
		s = os;
	}
	return (1);
}

#define	NSAVETAB	1024
char	*savetab;
int	saveleft;

char *
savestr(cp)
	register char *cp;
{
	register int len;

	len = strlen(cp) + 1;
	if (len > saveleft) {
		saveleft = NSAVETAB;
		if (len > saveleft)
			saveleft = len;
		savetab = (char *)malloc(saveleft);
		if (savetab == 0) {
			fprintf(stderr, "ran out of memory (savestr)\n");
			exit(1);
		}
	}
	strncpy(savetab, cp, len);
	cp = savetab;
	savetab += len;
	saveleft -= len;
	return (cp);
}

char *
strspl(left, right)
	char *left, *right;
{
	char buf[BUFSIZ];

	strcpy(buf, left);
	strcat(buf, right);
	return (savestr(buf));
}
