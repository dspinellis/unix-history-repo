#ifndef lint
static	char *sccsid = "@(#)prof.c	4.4 (Berkeley) 3/24/85";
#endif
/*
 * prof
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <a.out.h>
#include <sys/time.h>

typedef	short UNIT;		/* unit of profiling */
#define	PCFUDGE		11
#define	A_OUTNAME	"a.out"
#define	MON_OUTNAME	"mon.out"
#define	MON_SUMNAME	"mon.sum"

/*
 * The symbol table;
 * for each external in the specified file we gather
 * its address, the number of calls and compute its share of cpu time.
 */
struct nl {
	char	*name;
	unsigned value;
	float	time;
	long	ncall;
} *nl;
int	nname;
struct	nl *np;
struct	nl *npe;

/*
 * The header on the mon.out file.
 * Mon.out consists of one of these headers, an array of ncount
 * cnt structures (as below) and then an array of samples
 * representing the discretized program counter values.
 */
struct hdr {
	UNIT	*lowpc, *highpc;
	int	ncount;
} h;

/*
 * Each counter has an address and a number of calls.
 */
struct cnt {
	unsigned cvalue;
	long	cncall;
} *cbuf;

/*
 * Each discretized pc sample has
 * a count of the number of samples in its range
 */
unsigned UNIT	*samples;

FILE	*pfile, *nfile;

unsigned lowpc, highpc;		/* range profiled */
double	ransca, ranoff;		/* scaling for blowing up plots */
unsigned sampbytes;		/* number of bytes of samples */
int	nsamples;		/* number of samples */
double	totime;			/* total time for all routines */
double	maxtime;		/* maximum time of any routine (for plot) */
double	scale;			/* scale factor converting samples to pc
				   values: each sample covers scale bytes */
char	*strtab;		/* string table in core */
off_t	ssiz;			/* size of the string table */
struct	exec xbuf;		/* exec header of a.out */

int	aflg;
int	nflg;
int	vflg;
int	lflg;
int	zflg;
int	sflag;

char	*namfil;

int	timcmp(), valcmp(), cntcmp();

main(argc, argv)
	char **argv;
{
	int lowpct, highpct;

	/*
	 * Use highpct and lowpc as percentages, temporarily
	 * for graphing options involving blow-up
	 */
	lowpct = -1;
	highpct = -1;
	argv++;
	while ( *argv != 0 && **argv == '-' ) {
		*argv += 1;
		if (**argv == 'l')
			lflg++;
		else if (**argv == 'a')
			aflg++;
		else if (**argv == 'n')
			nflg++;
		else if (**argv == 'z')
			zflg++;
		else if (**argv == 'v')
			vflg++;
		else if ( **argv == 's' )
			sflag++;
		else if (**argv >= '0' && **argv <= '9') {
			int i = atoi(*argv);
			if (lowpct == -1)
				lowpct = i;
			else
				highpct = i;
		}
		argv++;
	}
	if ( *argv != 0 ) {
		namfil = *argv;
		argv++;
	} else {
		namfil = A_OUTNAME;
	}
	if (lowpct >= 100)
		lowpct = 0;
	if (highpct <= lowpct || highpct > 100)
		highpct = 100;
	ransca = 100./(highpct-lowpct);
	ranoff = 2040. + 40.8*lowpc*ransca;
		/*
		 *	get information about a.out file.
		 */
	getnfile();
		/*
		 *	get information about mon.out file(s).
		 */
	if ( *argv == 0 ) {
		getpfile( MON_OUTNAME );
	} else {
		do {
			getpfile( *argv );
			argv++;
		} while ( *argv != 0 );
	}
	asgnsamples();		/* assign samples to procedures */
#ifdef plot
	if (vflg)
		plotprof();	/* a plotted or ... */
	else
#endif
		printprof();	/* a printed profile */
	if ( sflag != 0 ) {
		putprof();
	}
	done();
}

printprof()
{
	double time, actime, hz;

	actime = 0;
	hz = hertz();
	printf(" %%time  cumsecs  #call  ms/call  name\n");
	if (!lflg)
		qsort(nl, nname, sizeof(struct nl), timcmp);
	for (np = nl; np<npe-1; np++) {
		if (zflg == 0 && np->time == 0 && np->ncall == 0)
			continue;
		time = np->time/totime;
		actime += np->time;
		printf("%6.1f%9.2f", 100*time, actime/hz);
		if (np->ncall != 0)
			printf("%7ld %8.2f",
			    np->ncall, (np->time*1000/hz)/np->ncall);
		else
			printf("%7.7s %8.8s", "", "");
		printf("  %s\n", np->name);
	}
}

/*
 * Set up string and symbol tables from a.out.
 * On return symbol table is sorted by value.
 */
getnfile()
{

	nfile = fopen(namfil,"r");
	if (nfile == NULL) {
		perror(namfil);
		done();
	}
	fread(&xbuf, 1, sizeof(xbuf), nfile);
	if (N_BADMAG(xbuf)) {
		fprintf(stderr, "%s: bad format\n", namfil);
		done();
	}
	getstrtab();
	getsymtab();
	qsort(nl, nname, sizeof(struct nl), valcmp);
}

getstrtab()
{

	fseek(nfile, N_SYMOFF(xbuf) + xbuf.a_syms, 0);
	if (fread(&ssiz, sizeof (ssiz), 1, nfile) == 0) {
		fprintf(stderr, "%s: no string table (old format?)\n", namfil);
		done();
	}
	strtab = (char *)calloc(ssiz, 1);
	if (strtab == NULL) {
		fprintf(stderr, "%s: no room for %d bytes of string table",
		    namfil, ssiz);
		done();
	}
	if (fread(strtab+sizeof(ssiz), ssiz-sizeof(ssiz), 1, nfile) != 1) {
		fprintf(stderr, "%s: error reading string table\n", namfil);
		done();
	}
}

/*
 * Read in symbol table
 */
getsymtab()
{
	register int i;

	/* pass1 - count symbols */
	fseek(nfile, N_SYMOFF(xbuf), 0);
	nname = 0;
	for (i = xbuf.a_syms; i > 0; i -= sizeof(struct nlist)) {
		struct nlist nbuf;
		fread(&nbuf, sizeof(nbuf), 1, nfile);
		if (nbuf.n_type!=N_TEXT && nbuf.n_type!=N_TEXT+N_EXT)
			continue;
		if (aflg==0 && nbuf.n_type!=N_TEXT+N_EXT)
			continue;
		nname++;
	}
	if (nname == 0) {
		fprintf(stderr, "%s: no symbols\n", namfil);
		done();
	}
	nl = (struct nl *)calloc((nname+1), sizeof (struct nl));
	if (nl == 0) {
		fprintf(stderr, "prof: No room for %d bytes of symbol table\n",
		    (nname+1) * sizeof (struct nlist));
		done();
	}

	/* pass2 - read symbols */
	fseek(nfile, N_SYMOFF(xbuf), 0);
	npe = nl;
	nname = 0;
	for (i = xbuf.a_syms; i > 0; i -= sizeof(struct nlist)) {
		struct nlist nbuf;
		fread(&nbuf, sizeof(nbuf), 1, nfile);
		if (nbuf.n_type!=N_TEXT && nbuf.n_type!=N_TEXT+N_EXT)
			continue;
		if (aflg==0 && nbuf.n_type!=N_TEXT+N_EXT)
			continue;
		npe->value = nbuf.n_value/sizeof(UNIT);
		npe->name = strtab+nbuf.n_un.n_strx;
		npe++;
		nname++;
	}
	npe->value = -1;
	npe++;
}

/*
 * information from a mon.out file is in two parts:
 * the counters of how many times each procedure was called,
 * if it was called at all;
 * and an array of sampling hits within pc ranges.
 * the counters must be dealt with on a file-by-file basis,
 * since which procedures are represented may vary.
 * the samples ranges are fixed, but must be summed across
 * files, and then distributed among procedures, because
 * of the wierd way the plotting is done.
 */
getpfile(filename)
	char *filename;
{

	openpfile(filename);
	readcntrs();
	asgncntrs();		/* assign counts to procedures */
	readsamples();
	closepfile();
}

openpfile(filename)
	char *filename;
{
	struct stat stb;

	if((pfile = fopen(filename, "r")) == NULL) {
		perror(filename);
		done();
	}
	fstat(fileno(pfile), &stb);
	fread(&h, sizeof(struct hdr), 1, pfile);
	lowpc = h.lowpc - (UNIT *)0;
	highpc = h.highpc - (UNIT *)0;
	sampbytes =
	    stb.st_size - sizeof(struct hdr) - h.ncount*sizeof(struct cnt);
	nsamples = sampbytes / sizeof (unsigned UNIT);
}

closepfile()
{

	fclose(pfile);
	free(cbuf);
}

readcntrs()
{
	struct cnt *kp;

	cbuf = (struct cnt *)calloc((h.ncount+1), sizeof (struct cnt));
	if (cbuf == 0) {
		fprintf(stderr, "prof: No room for %d bytes of count buffer\n",
		    (h.ncount+1) * sizeof (struct cnt));
		exit(1);
	}
	fread(cbuf, sizeof(struct cnt), h.ncount, pfile);
	/* eliminate zero counters and scale counter pc values */
	if (h.ncount) {
		kp = &cbuf[h.ncount - 1];
		for (;;) {
			if (kp->cvalue==0) {
				h.ncount=kp-cbuf;
				++kp;
				break;
			}
			if (kp == cbuf) {
				h.ncount = 0;
				break;
			}
			--kp;
		}
		for (; --kp>=cbuf; )
			kp->cvalue /= sizeof(UNIT);
	}
	/* sort counters */
	qsort(cbuf, h.ncount, sizeof(struct cnt), cntcmp);
}

/*
 * Assign counters to the procedures to which they belong
 */
asgncntrs()
{
	register int i;
	struct cnt *kp;

	kp = &cbuf[h.ncount-1];
	np = npe;
	while (--np>=nl) {
		if (kp<cbuf || np->value > kp->cvalue)
			continue;
			/* skip ``static'' functions */
		while (kp >= cbuf && kp->cvalue > np->value + PCFUDGE)
			--kp;
		if (kp->cvalue >= np->value) {
			np->ncall += kp->cncall;
			--kp;
		}
	}
}

readsamples()
{
	register i;
	unsigned UNIT	sample;
	int totalt;
	
	if (samples == 0) {
		samples = (unsigned UNIT *)
		    calloc(sampbytes, sizeof (unsigned UNIT));
		if (samples == 0) {
			printf("prof: No room for %d sample pc's\n", 
			    sampbytes / sizeof (unsigned UNIT));
			done();
		}
	}
	for (i = 0; ; i++) {
		fread(&sample, sizeof (unsigned UNIT), 1, pfile);
		if (feof(pfile))
			break;
		samples[i] += sample;
		totalt += sample;
	}
	if (i != nsamples) {
		fprintf(stderr,
		    "prof: unexpected EOF after reading %d/%d samples\n",
			--i, nsamples);
		done();
	}
}

/*
 * Assign samples to the procedures to which they belong.
 */
asgnsamples()
{
	register j;
	unsigned UNIT	ccnt;
	double time;
	unsigned pcl, pch;
	register int i;
	int overlap;

	/* read samples and assign to namelist symbols */
	scale = highpc - lowpc;
	scale /= nsamples;
	for (i=0; i < nsamples; i++) {
		ccnt = samples[i];
		if (ccnt == 0)
			continue;
		pcl = lowpc + scale*i;
		pch = lowpc + scale*(i+1);
		time = ccnt;
		totime += time;
		if(time > maxtime)
			maxtime = time;
		for (j=0; j<nname; j++) {
			if (pch < nl[j].value)
				break;
			if (pcl >= nl[j+1].value)
				continue;
			overlap=(min(pch,nl[j+1].value)-max(pcl,nl[j].value));
			if (overlap>0)
				nl[j].time += overlap*time/scale;
		}
	}
	if (totime==0.0) {
		fprintf(stderr, "No time accumulated\n");
/*
		done();
 */
		totime=1.0;
	}
}

/*
 * dump what you have out to a mon.out style file.
 */
putprof()
{
	FILE *sfile;
	struct nl *np;
	struct cnt kp;
	int i;

	sfile = fopen(MON_SUMNAME, "w");
	if (sfile == NULL) {
		perror(MON_SUMNAME);
		done();
	}
	/*
	 * build a new header.
	 * h.lowpc and h.highpc are already fine.
	 * fix h.ncount to count non-zero calls,
	 * and the one zero call which marks the end.
	 */
	h.ncount = 0;
	for (np = nl; np < npe-1 ; np++)
		if (np->ncall > 0)
			h.ncount++;
	h.ncount++;
	fwrite(&h, sizeof (struct hdr), 1, sfile);
	for (np = nl; np < npe-1; np++) {
		if (np->ncall > 0) {
			kp.cvalue = np->value * sizeof (unsigned UNIT);
			kp.cncall = np->ncall;
			fwrite(&kp, sizeof (struct cnt), 1, sfile);
		}
	}
	kp.cvalue = 0;
	kp.cncall = 0;
	fwrite(&kp, sizeof (struct cnt), 1, sfile);
	fwrite(samples, sizeof (unsigned UNIT), nsamples, sfile);
	fclose(sfile);
}

/*
 *	discover the tick frequency of the machine
 *	if something goes wrong, we return 1.
 */
hertz()
{
	struct itimerval tim;

	tim.it_interval.tv_sec = 0;
	tim.it_interval.tv_usec = 1;
	tim.it_value.tv_sec = 0;
	tim.it_value.tv_usec = 0;
	setitimer(ITIMER_REAL, &tim, 0);
	setitimer(ITIMER_REAL, 0, &tim);
	if (tim.it_interval.tv_usec < 1)
		return (1);
	return (1000000 / tim.it_interval.tv_usec);
}

min(a, b)
{
	if (a<b)
		return(a);
	return(b);
}

max(a, b)
{
	if (a>b)
		return(a);
	return(b);
}

valcmp(p1, p2)
	struct nl *p1, *p2;
{

	return(p1->value - p2->value);
}

timcmp(p1, p2)
	struct nl *p1, *p2;
{
	float d;

	if (nflg && p2->ncall != p1->ncall)
		return (p2->ncall - p1->ncall);
	d = p2->time - p1->time;
	if (d > 0.0)
		return(1);
	if (d < 0.0)
		return(-1);
	return(strcmp(p1->name,p2->name));
}

cntcmp(p1, p2)
	struct cnt *p1, *p2;
{

	return(p1->cvalue - p2->cvalue);
}

done()
{

#ifdef plot
	if(vflg) {
		point(0, -2040);
		closepl();
	}
#endif
	exit(0);
}

#ifdef plot
plotprof()
{
	double time, lastx, lasty, lastsx;
	register i;

	openpl();
	erase();
	space(-2048, -2048, 2048, 2048);
	line(-2040, -2040, -2040, 2040);
	line(0, 2040, 0, -2040);
	for(i=0; i<11; i++)
		line(-2040, 2040-i*408, 0, 2040-i*408);
	lastx = 0.;
	lasty = ranoff;
	scale = (4080.*ransca)/(sampbytes/sizeof(UNIT));
	lastsx = 0.0;
	for(i = 0; i < nsamples; i++) {
		unsigned UNIT ccnt;
		double tx, ty;
		ccnt = samples[i];
		time = ccnt;
		tx = lastsx;
		ty = lasty;
		lastsx -= 2000.*time/totime;
		lasty -= scale;
		if(lasty >= -2040. && ty <= 2040.) {
			line((int)tx, (int)ty, (int)lastsx, (int)lasty);
			if (ccnt!=0 || lastx!=0.0) {
				tx = lastx;
				lastx = -time*2000./maxtime;
				ty += scale/2;
				line(0, (int)ty, (int)tx, (int)ty);
			}
		}
	}
	scale = (4080.*ransca)/(highpc-lowpc);
	lastx = 50.;
	for(np = nl; np<npe;  np++) {
		if(np->value < lowpc)
			continue;
		if(np->value >= highpc)
			continue;
		if(zflg == 0 && np->time == 0 && np->ncall == 0)
			continue;
		time = np->time/totime;
		lasty = ranoff - (np->value - lowpc)*scale;
		if(lasty >= -2040. && lasty <= 2040.) {
			char bufl[BUFSIZ], *namp;
			register j;
			line(0, (int)lasty, 50, (int)lasty);
			line((int)(lastx-50),(int)lasty,(int)lastx,(int)lasty);
			move((int)(lastx+30), (int)(lasty+10));
			sprintf(bufl, "%s", np->name + (np->name[0] == '_'));
			label(bufl);
		}
		lastx += 500.;
		if(lastx > 2000.)
			lastx = 50.;
	}
}
#endif
