/*
 *  Print execution profile
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <a.out.h>

typedef	short UNIT;		/* unit of profiling */

struct stat stbuf;
struct	nl {
	char	name[8];
	unsigned value;
	float	time;
	long	ncall;
};

struct hdr {
	short	*lowpc;
	short	*highpc;
	int	ncount;
};

struct nl nl[600];

struct cnt {
	unsigned cvalue;
	long	cncall;
} cbuf[350];

FILE	*pfile, *nfile;
unsigned highpc;
unsigned lowpc;
double	ransca;
double	ranoff;
int	pcl;
int	pch;
unsigned	bufs;
int	nname;
double	time;
double	actime;
double	totime;
double	maxtime;
double	scale;
double	lastx;
double	lasty;
struct nl *np;
struct nl *npe;
int	aflg;
int	vflg;
int	lflg;
long	symoff;

main(argc, argv)
char **argv;
{
	char *namfil;
	int timcmp(), valcmp(), cntcmp();
	int i, overlap;
	long pfpos;
	double lastsx;
	struct cnt *cp;
	double tx, ty;
	struct exec xbuf;
	struct hdr {
		UNIT	*lowpc;
		UNIT	*highpc;
		int	ncount;
	} h;

	lowpc = -1;
	highpc = -1;
	argv++;
	namfil = "a.out";
	while (argc>1) {
		if (**argv == '-') {
			if (*++*argv == 'l')
				lflg++;
			if (**argv == 'a')
				aflg = 040;
			if(**argv == 'v')
				vflg++;
			if(**argv >= '0' && **argv <= '9') {
				i = atoi(*argv);
				if(lowpc == -1)
					lowpc = i;
				else
					highpc = i;
			}
		} else
			namfil = *argv;
		argc--;
		argv++;
	}
	if (lowpc >= 100)
		lowpc = 0;
	if(highpc <= lowpc || highpc > 100)
		highpc = 100;
	ransca = 100./(highpc-lowpc);
	ranoff = 2040. + 40.8*lowpc*ransca;
	if((nfile=fopen(namfil,"r"))==NULL) {
		fprintf(stderr, "%s: not found\n", namfil);
		done();
	}
	fread(&xbuf, 1, sizeof(xbuf), nfile);
	if (xbuf.a_magic!=A_MAGIC1 && xbuf.a_magic!=A_MAGIC2 && xbuf.a_magic!=A_MAGIC3) {
		fprintf(stderr, "%s: bad format\n", namfil);
		done();
	}
	symoff = (long)xbuf.a_text + xbuf.a_data + xbuf.a_trsize + xbuf.a_drsize;
	fseek(nfile, symoff+sizeof(xbuf), 0);
	if((pfile = fopen("mon.out","r")) == NULL) {
		fprintf(stderr, "No mon.out\n");
		done();
	}
	fstat(fileno(pfile), &stbuf);
	fread(&h, sizeof(struct hdr), 1, pfile);
	lowpc = h.lowpc - (UNIT *)0;
	highpc = h.highpc - (UNIT *)0;
	bufs = stbuf.st_size - sizeof(struct hdr) - h.ncount*sizeof(struct cnt);
	fread(cbuf, sizeof(struct cnt), h.ncount, pfile);
	pfpos = ftell(pfile);
	npe = nl;
	for (nname = 0; xbuf.a_syms > 0; xbuf.a_syms -= sizeof(struct nlist)) {
		struct nlist nbuf;
		fread(&nbuf, sizeof(nbuf), 1, nfile);
		if (nbuf.n_type!=N_TEXT && nbuf.n_type!=N_TEXT+N_EXT)
			continue;
		if (aflg==0 && nbuf.n_type!=N_TEXT+N_EXT)
			continue;
		npe->value = nbuf.n_value/sizeof(UNIT);
		for (i=8; --i>=0;)
			npe->name[i] = nbuf.n_name[i];
		npe++;
		nname++;
	}
	if (nname == 0) {
		fprintf(stderr, "%s: no symbols\n", namfil);
		done();
	}
	npe->value = -1;
	npe++;
	cp = &cbuf[h.ncount]; while ((--cp)->cvalue==0); ++cp; h.ncount=cp-cbuf;
	for (;--cp>=cbuf;) cp->cvalue /= sizeof(UNIT);
	qsort(cbuf, h.ncount, sizeof(struct cnt), cntcmp);
	qsort(nl, nname, sizeof(struct nl), valcmp);
	cp = &cbuf[h.ncount-1]; np = npe;
	while (--np>=nl) {
		if (cp<cbuf || np->value > cp->cvalue) continue;
		while (cp>=cbuf && cp->cvalue - np->value >11) --cp;
		if (cp->cvalue >= np->value) {np->ncall = cp->cncall; --cp;}
	}
	scale = highpc - lowpc;
	scale /= bufs/sizeof(UNIT);
	for(i=0;;i++) {
		register j;
		unsigned UNIT ccnt;
		fread(&ccnt, sizeof(ccnt), 1, pfile);
		if(feof(pfile))
			break;
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
			if (overlap>0) nl[j].time += overlap*time/scale;
		}
	}
	if (totime==0.0) {
		fprintf(stderr, "No time accumulated\n");
/*
		done();
 */
		totime=1.0;
	}
#ifdef plot
	if(!vflg)
		goto print;
	openpl();
	erase();
	space(-2048, -2048, 2048, 2048);
	line(-2040, -2040, -2040, 2040);
	line(0, 2040, 0, -2040);
	for(i=0; i<11; i++)
		line(-2040, 2040-i*408, 0, 2040-i*408);
	lastx = 0.;
	lasty = ranoff;
	scale = (4080.*ransca)/(bufs/sizeof(UNIT));
	fclose(pfile);	/*to turn off eof*/
	pfile = fopen("mon.out", "r");
	fseek(pfile, pfpos, 0);
	lastsx = 0.0;
	for(;;) {
		unsigned UNIT ccnt;
		fread(&ccnt, sizeof(ccnt), 1, pfile);
		if(feof(pfile))
			break;
		time = ccnt;
		tx = lastsx;
		ty = lasty;
		lastsx =- 2000.*time/totime;
		lasty =- scale;
		if(lasty >= -2040. && ty <= 2040.) {
			line((int)tx, (int)ty, (int)lastsx, (int)lasty);
			if (ccnt!=0 || lastx!=0.0) {
				tx = lastx;
				lastx = -time*2000./maxtime;
				ty =+ scale/2;
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
		time = np->time/totime;
		lasty = ranoff - (np->value - lowpc)*scale;
		if(lasty >= -2040. && lasty <= 2040.) {
			char bufl[8+3], *namp;
			register j;
			line(0, (int)lasty, 50, (int)lasty);
			line((int)(lastx-50),(int)lasty,(int)lastx,(int)lasty);
			point((int)(lastx+30), (int)(lasty+10));
			namp = bufl;
			for(j=0; j<8; j++)
				if(np->name[j] != '_')
					*namp++ = np->name[j];
			*namp++ = '\n';
			*namp++ = 0;
			label(bufl);
		}
		lastx =+ 500.;
		if(lastx > 2000.)
			lastx = 50.;
	}
	done();

print:
#endif
	actime = 0;
	printf("    name %%time  cumsecs  #call  ms/call\n");
	if (!lflg)
		qsort(nl, nname, sizeof(struct nl), timcmp);
	for (np = nl; np<npe-1; np++) {
		time = np->time/totime;
		actime += np->time;
		printf("%8.8s%6.1f%9.2f", np->name, 100*time, actime/60);
		if(np->ncall!=0) {
			printf("%7ld", np->ncall);
			printf(" %8.2f\n", np->time/(np->ncall*.06));
		} else
			printf("\n");
	}
	done();
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

	d = p2->time - p1->time;
	if (d > 0.0)
		return(1);
	if (d < 0.0)
		return(-1);
	return(0);
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
