static char sccsid[] = "@(#)vpr.c	1.4	(Berkeley)	%G%";

#include <signal.h>
#include <sys/param.h>
#define	BUFSIZ	MAXBSIZE

/*
 * 	vpr -- varian and versatec (as printers) spooler
 */

#define VAD		"/usr/lib/vad"
#define VPD		"/usr/lib/vpd"
char	VAtfname[] =	"/usr/spool/vad/tfaXXXXX";
char	VAcfname[] =	"/usr/spool/vad/cfaXXXXX";
char	VAdfname[] =	"/usr/spool/vad/dfaXXXXX";
char	VPtfname[] =	"/usr/spool/vpd/tfaXXXXX";
char	VPcfname[] =	"/usr/spool/vpd/cfaXXXXX";
char	VPdfname[] =	"/usr/spool/vpd/dfaXXXXX";
char	*tfname;
char	*cfname;
/* char	*lfname; */
char	*dfname;
int	wide;
int	literal;
int	nact;
int	tff;
int	mailflg;
char	person[10];
int	inchar;
int	maxrec	= 2000;
char	*width = "-w106";
int	troffit;
int     plotit;
char	*fonts[4];

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *arg, *remote;
	int c, f, fv, flag;
	int out();

	umask(0);
	if (signal(SIGINT, SIG_IGN) == SIG_DFL)
		signal(SIGINT, out);
	if (signal(SIGQUIT, SIG_IGN) == SIG_DFL)
		signal(SIGQUIT, out);
	if (signal(SIGHUP, SIG_IGN) == SIG_DFL)
		signal(SIGHUP, out);
	if (signal(SIGTERM, SIG_IGN) == SIG_DFL)
		signal(SIGTERM, out);
	remote = "$	remote	**,onl";
	flag = 0;
	wide = 0;
	tfname = VAtfname;
	cfname = VAcfname;
	dfname = VAdfname;
	while (argc>1 && (arg = argv[1])[0]=='-') {
		if (arg[1] && arg[2]) {
			remote[12] = arg[1];
			remote[13] = arg[2];
			remote[14] = 0;
		} else switch (arg[1]) {

		case 'W':
			wide++;
			tfname = VPtfname;
			cfname = VPcfname;
			dfname = VPdfname;
			break;

		case '-':
			remote[12] = 'r';
			remote[13] = '1';
			remote[14] = '\0';
			break;

		case 'c':
			flag = '+';
			break;

		case 'r':
			flag = '-';
			break;

                case 'v':
                        plotit = 1;
                        width = 0;
                        break;

		case 'm':
			mailflg = 1;
			break;

		case 't':
			troffit = 1;
			width = 0;
			break;

		case '4':
		case '3':
		case '2':
		case '1':
			fonts[arg[1] - '1'] = argv[2];
			troffit = 1;
			argv++;
			argc--;
			break;

		case 'w':
			if (troffit)
				break;
			if (arg[2] == 0)
				width = 0;
			else
				width = arg;
			break;

		case 'l':
			literal++;	/* Pass control chars through. */
			break;
		}
		argc--;
		argv++;
	}
	pidfn();
	tff = nfile(tfname);
	if (!wide)		/* varian doesn't use sidebyside. */
		width = 0;
	ident();
	if(argc == 1)
		copy(0);
	while(--argc > 0) {
		arg = *++argv;
/*
		if(flag == '+')
			goto cf;
 * This may not work because the daemon runs as someone else, so don't bother
		if(*arg == '/' && flag != '-') {
			card(literal ? 'G' : 'F', arg);
			nact++;
			continue;
		}
		if(link(arg, lfname) < 0)
			goto cf;
		card(literal ? 'G' : 'F', lfname);
		card('U', lfname);
		lfname[inchar]++;
		nact++;
		goto df;
 */

	cf:
		f = open(arg, 0);
		if(f < 0) {
			printf("Cannot open %s\n", arg);
			if (plotit) {
				--argc;
				arg = *++argv;
			}
			continue;
		}
                if (plotit) {
 			if (--argc > 0) {
				arg = *++argv;
				fv = open(arg, 0);
				if (fv < 0) {
					printf("Cannot open %s\n", arg);
					close(f);
					continue;
				}
			}
			else {
				printf("Versaplot requires parm and vector file\n");
				close(f);
				continue;
			}
			copy(fv);
			close(fv);
		}
		copy(f);
		close(f);

	df:
		if(flag == '-') {
			f = unlink(arg);
			if(f < 0)
				printf("Cannot remove %s\n", arg);
		}
	}

	if(nact) {
		tfname[inchar]--;
		f = link(tfname, dfname);
		if(f < 0) {
			printf("Cannot rename %s\n", dfname);
			tfname[inchar]++;
			out();
		}
		unlink(tfname);
		if (wide)
			execl(VPD, "vpd", "-n", "-3", 0);
		else
			execl(VAD, "vad", "-n", "-3", 0);
		dfname[inchar]++;
		printf("Daemon doesn't exist\n");
		exit(0);
	}
	out();
}

copy(f)
int f;
{
	int ff, i, nr, nc;
	static char buf[BUFSIZ];
	int status;

	for (i = 0; i < 3; i++)
		if (fonts[i])
			card('1' + i, fonts[i]);
	if (troffit)
		card('T', cfname);
	else if (plotit)
		card('P', cfname);
	else
		card(literal ? 'G' : 'F', cfname);
	card('U', cfname);
	ff = nfile(cfname);
	nc = 0;
	nr = 0;
	if (width) {
		int pvec[2];
		pipe(pvec);
		i = fork();
		if (i < 0) {
			printf("No more processes\n");
			out();
		}
		if (i == 0) {
			if (f != 0) {
				close(0);
				dup(f);
			}
			close(1);
			dup(pvec[1]);
			close(pvec[0]);
			close(pvec[1]);
			execl("/usr/lib/sidebyside", "sidebyside", width, 0);
			perror("/usr/lib/sidebyside");
			exit(1);
		}
		close(pvec[1]);
		close(f);
		f = pvec[0];
	}
	while((i = read(f, buf, BUFSIZ)) > 0) {
		write(ff, buf, i);
		nc += i;
		if(nc >= BUFSIZ) {
			nc -= BUFSIZ;
			nr++;
			if(nr > maxrec) {
				printf("Copy file is too large\n");
				break;
			}
		}
	}
	close(ff);
	nact++;
	wait(&status);
}

card(c, s)
int c;
char s[];
{
	char *p1, *p2;
	static char buf[BUFSIZ];
	int col;

	p1 = buf;
	p2 = s;
	col = 0;
	*p1++ = c;
	while((c = *p2++) != '\0') {
		*p1++ = c;
		col++;
	}
	*p1++ = '\n';
	write(tff, buf, col+2);
}

ident()
{
	int c, n;
	register char *b1p, *pp, *b2p;
	static char b1[100], b2[100];

	b1p = b1;
	if(getpw(getuid(), b1p)) {
		b1p = "pdp::::m0000,m000:";
	}
	n = 0;
	b2p = b2;
	while(*b2p++ = "$	ident	"[n++]);
	b2p--;
	n = 5;
	while(--n) while(*b1p++ != ':');
	while((*b2p++ = *b1p++) != ':');
	b2p[-1] = ',';
	b1p = b1;
	pp = person;
	while((c = *b1p++) != ':') {
		*b2p++ = c;
		*pp++ = c;
	}
	*b2p++ = 0;
	*pp++ = 0;
	card('L', person);
	if (mailflg)
		card('M', person);
}

pidfn()
{
	register i, j, c;
	int s;
	int p;

	s = p = getpid();
	p &= 077777;
	i = 0;
	while(tfname[i] != 'X')
		i++;
	i += 4;
	for(j=0; j<5; j++) {
		c = (p%10) + '0';
		if(s<0 && j==4)
			c += 4;
		p /= 10;
		tfname[i] = c;
		cfname[i] = c;
/*
		lfname[i] = c;
*/
		dfname[i] = c;
		i--;
	}
	inchar = i;
}

nfile(name)
char *name;
{
	register f;

	f = creat(name, 0644);
	if(f < 0) {
		printf("Cannot create %s\n", name);
		out();
	}
	name[inchar]++;
	return(f);
}

out()
{
	register i;

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGHUP, SIG_IGN);
	signal(SIGTERM, SIG_IGN);
	i = inchar;
	while(tfname[i] != 'a') {
		tfname[i]--;
		unlink(tfname);
	}
	while(cfname[i] != 'a') {
		cfname[i]--;
		unlink(cfname);
	}
/*
	while(lfname[i] != 'a') {
		lfname[i]--;
		unlink(lfname);
	}
*/
	while(dfname[i] != 'a') {
		dfname[i]--;
		unlink(dfname);
	}
	exit(0);
}
