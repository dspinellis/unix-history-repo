# include	"../hdr/defines.h"
# include	"../hdr/had.h"

static char Sccsid[] = "@(#)comb.c	4.4	%G%";
USXALLOC();

struct packet gpkt;
struct sid sid;
int	num_files;
char	had[26];
char	*clist;
int	*Cvec;
int	Cnt;
FILE	*iop;

main(argc,argv)
int argc;
register char *argv[];
{
	register int i;
	register char *p;
	char c;
	int testmore;
	extern comb();
	extern int Fcnt;

	Fflags = FTLEXIT | FTLMSG | FTLCLN;
	for(i = 1; i < argc; i++)
		if(argv[i][0] == '-' && (c=argv[i][1])) {
			p = &argv[i][2];
			testmore = 0;
			switch (c) {

			case 'p':
				if (!p[0]) {
					argv[i] = 0;
					continue;
				}
				chksid(sid_ab(p,&sid),&sid);
				break;
			case 'c':
				clist = p;
				break;
			case 'o':
				testmore++;
				break;
			case 's':
				testmore++;
				break;
			default:
				fatal("unknown key letter (cm1)");
			}

			if (testmore) {
				testmore = 0;
				if (*p) {
					sprintf(Error, "value after %c arg (cm7)",c);
					fatal(Error);
				}
			}
			if (had[c - 'a']++)
				fatal("key letter twice (cm2)");
			argv[i] = 0;
		}
		else num_files++;

	if(num_files == 0)
		fatal("missing file arg (cm3)");
	if (HADP && HADC)
		fatal("can't have both -p and -c (cb2)");
	setsig();
	Fflags &= ~FTLEXIT;
	Fflags |= FTLJMP;
	iop = stdout;
	for (i = 1; i < argc; i++)
		if (p=argv[i])
			do_file(p,comb);
	fclose(iop);
	exit(Fcnt ? 1 : 0);
}


comb(file)
{
	register int i, n;
	register struct idel *rdp;
	char *p;
	int succnt;
	struct sid *sp;
	extern char had_dir, had_standinp;
	extern char *Sflags[];
	struct stats stats;

	if (setjmp(Fjmp))
		return;
	sinit(&gpkt, file, 1);
	gpkt.p_verbose = -1;
	gpkt.p_stdout = stderr;
	if (gpkt.p_verbose && (num_files > 1 || had_dir || had_standinp))
		fprintf(gpkt.p_stdout,"\n%s:\n",gpkt.p_file);
	if (exists(auxf(gpkt.p_file, 'p')))
		fatal("p-file exists (cb1)");

	if (dodelt(&gpkt,&stats,0,0) == 0)
		fmterr(&gpkt);

	Cvec = alloc(n = ((maxser(&gpkt) + 1) * sizeof(*Cvec)));
	bzero(Cvec, n);
	Cnt = 0;

	if (HADP) {
		if (!(n = sidtoser(&sid, &gpkt)))
			fatal("sid doesn't exist (cb3)");
		while (n <= maxser(&gpkt))
			Cvec[Cnt++] = n++;
	}
	else if (HADC) {
		dolist(&gpkt, clist, 0);
	}
	else {
		rdp = gpkt.p_idel;
		for (i = 1; i <= maxser(&gpkt); i++) {
			succnt = 0;
			for (n = i + 1; n <= maxser(&gpkt); n++)
				if (rdp[n].i_pred == i)
					succnt++;
			if (succnt != 1)
				Cvec[Cnt++] = i;
		}
	}
	finduser(&gpkt);
	doflags(&gpkt);
	fclose(gpkt.p_iop);
	gpkt.p_iop = 0;
	if (!Cnt)
		fatal("nothing to do (cb4)");
	rdp = gpkt.p_idel;
	sp = prtget(rdp, Cvec[0], iop, gpkt.p_file);
	fprintf(iop, "admin -iCOMB -r%d s.COMB\n", sp->s_rel);
	fprintf(iop, "rm -f COMB\n");
	for (i = 1; i < Cnt; i++) {
		n = getpred(rdp, Cvec, i);
		if (HADO)
			fprintf(iop, "get -s -r%d -g -e -t s.COMB\n",
				rdp[Cvec[i]].i_sid.s_rel);
		else
			fprintf(iop, "get -s -a%d -r%d -g -e s.COMB\n",
				n + 1, rdp[Cvec[i]].i_sid.s_rel);
		prtget(rdp, Cvec[i], iop, gpkt.p_file);
		fprintf(iop, "delta -s '-yThis was COMBined' s.COMB\n");
	}
	fprintf(iop, "sed -n '/^%c%c$/,/^%c%c$/p' %s >comb${pid}\n",
		CTLCHAR, BUSERTXT, CTLCHAR, EUSERTXT, gpkt.p_file);
	fprintf(iop, "ed - comb${pid} <<\\!\n");
	fprintf(iop, "1d\n");
	fprintf(iop, "$c\n");
	fprintf(iop, " *** DELTA TABLE PRIOR TO COMBINE ***\n");
	fprintf(iop, ".\n");
	fprintf(iop, "w\n");
	fprintf(iop, "q\n");
	fprintf(iop, "!\n");
	fprintf(iop, "prt -a %s >>comb${pid}\n", gpkt.p_file);
	fprintf(iop, "admin -tcomb${pid} s.COMB\\\n");
	for (i = 0; i < NFLAGS; i++)
		if (p = Sflags[i])
			fprintf(iop, " -f%c%s\\\n", i + 'a', p);
	fprintf(iop, "\n");
	fprintf(iop, "sed -n '/^%c%c$/,/^%c%c$/p' %s >comb${pid}\n",
		CTLCHAR, BUSERNAM, CTLCHAR, EUSERNAM, gpkt.p_file);
	fprintf(iop, "ed - comb${pid} <<\\!\n");
	fprintf(iop, "v/^%c/s/.*/-a& \\\\/\n", CTLCHAR);
	fprintf(iop, "1c\n");
	fprintf(iop, "admin s.COMB\\\n");
	fprintf(iop, ".\n");
	fprintf(iop, "$c\n");
	fprintf(iop, "\n");
	fprintf(iop, ".\n");
	fprintf(iop, "w\n");
	fprintf(iop, "q\n");
	fprintf(iop, "!\n");
	fprintf(iop, "sh comb${pid}\n");
	fprintf(iop, "rm comb${pid}\n");
	if (!HADS) {
		fprintf(iop, "rm -f %s\n", gpkt.p_file);
		fprintf(iop, "mv s.COMB %s\n", gpkt.p_file);
	}
	else {
		fprintf(iop, "set a=`echo \\`ls -s s.COMB\\``\n");
		fprintf(iop, "set b=`echo \\`ls -s %s\\``\n",gpkt.p_file);
		fprintf(iop, "set c=`expr 100 - 100 '*' ${a} / ${b}`\n");
		fprintf(iop, "echo '%s\t' ${c}'%%\t' ${a}/${b}\n", gpkt.p_file);
		fprintf(iop, "rm -f s.COMB\n");
	}
}


enter(pkt,ch,n,sidp)
struct packet *pkt;
char ch;
int n;
struct sid *sidp;
{
	Cvec[Cnt++] = n;
}


prtget(idp, ser, iop, file)
struct idel *idp;
int ser;
FILE *iop;
char *file;
{
	char buf[32];
	struct sid *sp;

	sid_ba(sp = &idp[ser].i_sid, buf);
	fprintf(iop, ":\t/bin/bsh\n");
	fprintf(iop, "get -s -k -r%s -p %s > COMB\n", buf, file);
	return(sp);
}


getpred(idp, vec, i)
struct idel *idp;
int *vec;
int i;
{
	int ser, pred, acpred;

	ser = vec[i];
	while (--i) {
		pred = vec[i];
		for (acpred = idp[ser].i_pred; acpred; acpred = idp[acpred].i_pred)
			if (pred == acpred)
				break;
		if (pred == acpred)
			break;
	}
	return(i);
}


clean_up(n)
{
	xfreeall();
}


escdodelt()	/* dummy for dodelt() */
{
}
