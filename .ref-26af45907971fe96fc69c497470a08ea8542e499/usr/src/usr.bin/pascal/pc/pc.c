/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pc.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/signal.h>
#include <sys/wait.h>
#include <stdio.h>
#include "pathnames.h"

/*
 * Pc - front end for Pascal compiler.
 */
char	*pc0 = _PATH_PC0;
char	*pc1 = _PATH_PC1;
char	*pc2 = _PATH_PC2;
char	*c2 = _PATH_C2;
char	*pc3 = _PATH_PC3;
char	*ld = _PATH_LD;
char	*as = _PATH_AS;
char	*lpc = "-lpc";
char	*crt0 = _PATH_CRT0;
char	*mcrt0 = _PATH_MCRT0;
char	*gcrt0 = _PATH_GCRT0;

char	*mktemp();
char	*tmpdir = _PATH_TMP;
char	tmp0[MAXPATHLEN], tmp1[MAXPATHLEN];
char	*tname[2];
char	*tfile[2];

char	*setsuf(), *savestr();

int	Jflag, Sflag, Oflag, Tlflag, cflag, gflag, pflag, wflag, tflag;
int	debug;

#define	NARGS	512
int	ldargx = 3;
int	pc0argx = 3;
char	*pc0args[NARGS] =	{ "pc0", "-o", "XXX" };
char	*pc1args[3] =		{ "pc1", 0, };
char	*pc2args[2] =		{ "pc2", 0 };
char	*c2args[4] =		{ "c2", 0, 0, 0 };
int	pc3argx = 1;
#define	pc3args	pc0args
#define	ldargs	pc0args
/* char	*pc3args[NARGS] =	{ "pc3", 0 }; */
/* char	*ldargs[NARGS] =	{ "ld", "-X", _PATH_CRT0, 0, }; */

				/* as -J -t tmpdir -o objfile srcfile \0 */
int	asargx;
char	*asargs[8] =		{ "as", 0, };

char *mesg[] = {
	0,
	"Hangup",
	"Interrupt",	
	"Quit",
	"Illegal instruction",
	"Trace/BPT trap",
	"IOT trap",
	"EMT trap",
	"Floating exception",
	"Killed",
	"Bus error",
	"Segmentation fault",
	"Bad system call",
	"Broken pipe",
	"Alarm clock",
	"Terminated",
	"Signal 16",
	"Stopped (signal)",
	"Stopped",
	"Continued",
	"Child exited",
	"Stopped (tty input)",
	"Stopped (tty output)",
	"Tty input interrupt",
	"Cputime limit exceeded",
	"Filesize limit exceeded",
	"Signal 26",
	"Signal 27",
	"Signal 28",
	"Signal 29",
	"Signal 30",
	"Signal 31",
	"Signal 32"
};

/*
 * If the number of .p arguments (np) is 1, and the number of .o arguments
 * (nxo) is 0, and we successfully create an ``a.out'', then we remove
 * the one .ps .o file (onepso).
 */
int	np, nxo;
char	*onepso;
int	errs;

int	onintr();

main(argc, argv)
	int argc;
	char **argv;
{
	register char *argp;
	register int i;
	int savargx;
	char *t, c;
	int j;

	argc--, argv++;
	if (argc == 0) {
		execl(_PATH_CAT, "cat", _PATH_HOWPC);
		exit(1);
	}
	if (signal(SIGINT, SIG_IGN) != SIG_IGN) {
		signal(SIGINT, onintr);
		signal(SIGTERM, onintr);
	}
	for (i = 0; i < argc; i++) {
		argp = argv[i];
		if (argp[0] != '-')
			continue;
		switch (argp[1]) {

		case 'd':
			if (argp[2] == 0)
				debug++;
			continue;
		case 'i':
			pc0args[pc0argx++] = "-i";
			while (i+1 < argc && argv[i+1][0] != '-' &&
			    getsuf(argv[i+1]) != 'p') {
				pc0args[pc0argx++] = argv[i+1];
				i++;
			}
			if (i+1 == argc) {
				fprintf(stderr, "pc: bad -i construction\n");
				exit(1);
			}
			continue;
		case 'o':
			i++;
			if (i == argc) {
				fprintf(stderr, "pc: -o must specify file\n");
				exit(1);
			}
			c = getsuf(argv[i]);
			if (c == 'o' || c == 'p' || c == 'c') {
				fprintf(stderr, "pc: -o would overwrite %s\n",
				    argv[i]);
				exit(1);
			}
			continue;
		case 't':
			i++;
			if (i == argc) {
				fprintf(stderr, "pc: -t but no directory\n");
				exit(1);
			}
			if (argp[2] != '\0') {
				fprintf(stderr, "pc: bad -t option\n");
				exit(1);
			}
			tmpdir = argv[i];
			if (tmpdir[0] == '-') {
				fprintf(stderr, "pc: bad -t option\n");
				exit(1);
			}
			tflag = 1;
			continue;
		case 'O':
			Oflag = 1;
			continue;
		case 'S':
			Sflag = 1;
			continue;
		case 'J':
			Jflag = 1;
			continue;
		case 'T':
			switch (argp[2]) {

			case '0':
				pc0 = _PATH_DPC0;
				if (argp[3] != '\0') {
					pc0 = &argp[3];
				}
				continue;
			case '1':
				pc1 = _PATH_DPC1;
				if (argp[3] != '\0') {
					pc1 = &argp[3];
				}
				continue;
			case '2':
				pc2 = _PATH_DPC2;
				if (argp[3] != '\0') {
					pc2 = &argp[3];
				}
				continue;
			case '3':
				pc3 = _PATH_DPC3;
				if (argp[3] != '\0') {
					pc3 = &argp[3];
				}
				continue;
			case 'l':
				Tlflag = 1;
				lpc = _PATH_DLPC;
				if (argp[3] != '\0') {
					lpc = &argp[3];
				}
				continue;
			}
			continue;
		case 'c':
			cflag = 1;
			continue;
		case 'l':
			if (argp[2])
				continue;
			/* fall into ... */
		case 'b':
		case 's':
		case 'z':
		case 'C':
			pc0args[pc0argx++] = argp;
			continue;
		case 'w':
			wflag = 1;
			pc0args[pc0argx++] = argp;
			continue;
		case 'g':
			gflag = 1;
			pc0args[pc0argx++] = argp;
			continue;
		case 'p':
			if (argp[2] == 'g')
				crt0 = gcrt0;
			else
				crt0 = mcrt0;
			if (!Tlflag)
				lpc = "-lpc_p";
			pflag = 1;
			continue;
		}
	}
	if (gflag && Oflag) {
		fprintf(stderr, "pc: warning: -g overrides -O\n");
		Oflag = 0;
	}
	sprintf(tmp0, "%s/%s", tmpdir, "p0XXXXXX");
	tname[0] = mktemp(tmp0);
	sprintf(tmp1, "%s/%s", tmpdir, "p1XXXXXX");
	tname[1] = mktemp(tmp1);
	savargx = pc0argx;
	for (i = 0; i < argc; i++) {
		argp = argv[i];
		if (argp[0] == '-')
			continue;
		if (suffix(argp) == 's') {
			asargx = 1;
			if (Jflag)
				asargs[asargx++] = "-J";
#			if defined(vax) || defined(tahoe)
				if (tflag) {
					asargs[asargx++] = "-t";
					asargs[asargx++] = tmpdir;
				}
#			endif vax || tahoe
			asargs[asargx++] = argp;
			asargs[asargx++] = "-o";
			tfile[1] = setsuf(argp, 'o');
			asargs[asargx++] = tfile[1];
			asargs[asargx] = 0;
			if (dosys(as, asargs, 0, 0))
				continue;
			tfile[1] = 0;
			continue;
		}
		if (suffix(argp) != 'p')
			continue;
		tfile[0] = tname[0];
		pc0args[2] = tfile[0];
		pc0argx = savargx;
		if (pflag)
			pc0args[pc0argx++] = "-p";
		if (Jflag)
			pc0args[pc0argx++] = "-J";
		pc0args[pc0argx++] = argp;
		pc0args[pc0argx] = 0;
		if (dosys(pc0, pc0args, 0, 0))
			continue;
		pc1args[1] = tfile[0];
		tfile[1] = tname[1];
		if (dosys(pc1, pc1args, 0, tfile[1]))
			continue;
		unlink(tfile[0]);
		tfile[0] = tname[0];
		if (Oflag) {
			if (dosys(c2, c2args, tfile[1], tfile[0]))
				continue;
			unlink(tfile[1]);
			tfile[1] = tfile[0];
			tfile[0] = tname[1];
		}
		if (Sflag)
			tfile[0] = setsuf(argp, 's');
		if (dosys(pc2, pc2args, tfile[1], tfile[0]))
			continue;
		unlink(tfile[1]);
		tfile[1] = 0;
		if (Sflag) {
			tfile[0] = 0;
			continue;
		}
		asargx = 1;
		if (Jflag)
			asargs[asargx++] = "-J";
#		if defined(vax) || defined(tahoe)
			if (tflag) {
				asargs[asargx++] = "-t";
				asargs[asargx++] = tmpdir;
			}
#		endif vax || tahoe
		asargs[asargx++] = tfile[0];
		asargs[asargx++] = "-o";
		tfile[1] = setsuf(argp, 'o');
		asargs[asargx++] = tfile[1];
		asargs[asargx] = 0;
		if (dosys(as, asargs, 0, 0))
			continue;
		tfile[1] = 0;
		removetemps();
	}
	if (errs || cflag || Sflag)
		done();
/* char	*pc3args[NARGS] =	{ "pc3", 0 }; */
	pc3args[0] = "pc3";
	if (wflag)
		pc3args[pc3argx++] = "-w";
	pc3args[pc3argx++] = _PATH_PCEXTERN;
	for (i = 0; i < argc; i++) {
		argp = argv[i];
		if (!strcmp(argp, "-o"))
			i++;
		if (argp[0] == '-')
			continue;
		switch (getsuf(argp)) {

		case 'o':
			pc3args[pc3argx++] = argp;
			nxo++;
			continue;
		case 's':
		case 'p':
			onepso = pc3args[pc3argx++] =
			    savestr(setsuf(argp, 'o'));
			np++;
			continue;
		}
	}
	pc3args[pc3argx] = 0;
	if (dosys(pc3, pc3args, 0, 0) > 1)
		done();
	errs = 0;
/* char	*ldargs[NARGS] =	{ "ld", "-X", _PATH_CRT0, 0, }; */
	ldargs[0] = "ld";
	ldargs[1] = "-X";
	ldargs[2] = crt0;
	for (i = 0; i < argc; i++) {
		argp = argv[i];
		if (argp[0] != '-') {
			switch (getsuf(argp)) {

			case 'p':
			case 's':
				ldargs[ldargx] = savestr(setsuf(argp, 'o'));
				break;
			default:
				ldargs[ldargx] = argp;
				break;
			}
			if (getsuf(ldargs[ldargx]) == 'o')
			for (j = 0; j < ldargx; j++)
				if (!strcmp(ldargs[j], ldargs[ldargx]))
					goto duplicate;
			ldargx++;
duplicate:
			continue;
		}
		switch (argp[1]) {

		case 'i':
			while (i+1 < argc && argv[i+1][0] != '-' &&
			    getsuf(argv[i+1]) != 'p')
				i++;
			continue;
		case 'd':
			if (argp[2] == 0)
				continue;
			ldargs[ldargx++] = argp;
			continue;
		case 'o':
			ldargs[ldargx++] = argp;
			i++;
			ldargs[ldargx++] = argv[i];
			continue;
		case 'l':
			if (argp[2])
				ldargs[ldargx++] = argp;
			continue;
		case 't':
			i++;
			continue;
		case 'c':
		case 'g':
		case 'w':
		case 'p':
		case 'S':
		case 'J':
		case 'T':
		case 'O':
		case 'C':
		case 'b':
		case 's':
		case 'z':
			continue;
		default:
			ldargs[ldargx++] = argp;
			continue;
		}
	}
	ldargs[ldargx++] = lpc;
	if (gflag)
		ldargs[ldargx++] = "-lg";
	if (pflag) {
		ldargs[ldargx++] = "-lm_p";
		ldargs[ldargx++] = "-lc_p";
	} else {
		ldargs[ldargx++] = "-lm";
		ldargs[ldargx++] = "-lc";
	}
	ldargs[ldargx] = 0;
	if (dosys(ld, ldargs, 0, 0)==0 && np == 1 && nxo == 0)
		unlink(onepso);
	done();
}

dosys(cmd, argv, in, out)
	char *cmd, **argv, *in, *out;
{
	union wait status;
	int pid;

	if (debug) {
		int i;
		printf("%s:", cmd);
		for (i = 0; argv[i]; i++)
			printf(" %s", argv[i]);
		if (in)
			printf(" <%s", in);
		if (out)
			printf(" >%s", out);
		printf("\n");
	}
	/*
	 * warning: vfork doesn't work here, because the call to signal() 
	 * done by the child process destroys the parent's SIGINT handler.
	 */
	pid = fork();
	if (pid < 0) {
		fprintf(stderr, "pc: No more processes\n");
		done();
	}
	if (pid == 0) {
		if (in) {
			close(0);
			if (open(in, 0) != 0) {
				perror(in);
				exit(1);
			}
		}
		if (out) {
			close(1);
			unlink(out);
			if (creat(out, 0666) != 1) {
				perror(out);
				exit(1);
			}
		}
		signal(SIGINT, SIG_DFL);
		execv(cmd, argv);
		perror(cmd);
		exit(1);
	}
	while (wait(&status) != pid)
		;
	if (WIFSIGNALED(status)) {
		if (status.w_termsig != SIGINT) {
			fprintf(stderr, "%s: %s", cmd, mesg[status.w_termsig]);
			if (status.w_coredump)
				fprintf(stderr, " (core dumped)");
			fprintf(stderr, "\n");
		}
		errs = 100;
		done();
		/*NOTREACHED*/
	}
	if (status.w_retcode) {
		errs = 1;
		removetemps();
	}
	return (status.w_retcode);
}

done()
{

	removetemps();
	exit(errs);
}

removetemps()
{

	if (tfile[0])
		unlink(tfile[0]);
	if (tfile[1])
		unlink(tfile[1]);
}

onintr()
{

	errs = 1;
	done();
}

getsuf(cp)
	char *cp;
{

	if (*cp == 0)
		return;
	while (cp[1])
		cp++;
	if (cp[-1] != '.')
		return (0);
	return (*cp);
}

char *
setsuf(as, ch)
	char *as;
	int ch;
{
	register char *s, *s1;

	s = s1 = savestr(as);
	while (*s)
		if (*s++ == '/')
			s1 = s;
	s[-1] = ch;
	return (s1);
}

#define	NSAVETAB	512
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
	return (cp);
}

suffix(cp)
	char *cp;
{

	if (cp[0] == 0 || cp[1] == 0)
		return (0);
	while (cp[1])
		cp++;
	if (cp[-1] == '.')
		return (*cp);
	return (0);
}
