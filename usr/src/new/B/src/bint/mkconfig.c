/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: mkconfig.c,v 1.4 85/08/26 10:41:39 timo Exp $
*/

/* Generate constants for configuration file			*/

/* If your C system is not unix but does have signal/setjmp,	*/
/*    add a #define unix					*/
/* You may also need to add some calls to signal().		*/

#ifdef unix

#define SIGNAL

#include <signal.h>
#include <setjmp.h>

	jmp_buf lab;
	overflow(sig) int sig; { /* what to do on overflow/underflow */
		signal(sig, overflow);
		longjmp(lab, 1);
	}

#else
	/* Dummy routines instead */
	int lab=1;
	int setjmp(lab) int lab; { return(0); }

#endif

#define fabs(x) (((x)<0.0)?(-x):(x))
#define min(x,y) (((x)<(y))?(x):(y))

/* These routines are intended to defeat any attempt at optimisation */
Dstore(a, b) double a, *b; { *b=a; }
double Dsum(a, b) double a, b; { double r; Dstore(a+b, &r); return (r); }
double Ddiff(a, b) double a, b; { double r; Dstore(a-b, &r); return (r); }
double Dmul(a, b) double a, b; { double r; Dstore(a*b, &r); return (r); }
double Ddiv(a, b) double a, b; { double r; Dstore(a/b, &r); return (r); }

double power(x, n) int x, n; {
	double r=1.0;
	for (;n>0; n--) r*=x;
	return r;
}

int log(base, x) int base; double x; {
	int r=0;
	while (x>=base) { r++; x/=base; }
	return r;
}

main(argc, argv) int argc; char *argv[]; {
	char c;
	short newshort, maxshort, maxershort;
	int newint, maxint, shortbits, bits, mantbits,
	    *p, shortpower, intpower, longpower;
	long newlong, maxlong, count;

	int i, ibase, iexp, irnd, imant, iz, k, machep, maxexp, minexp,
	    mx, negeps, ngrd, normalised;
	double a, b, base, basein, basem1, eps, epsneg, xmax, newxmax,
	       xmin, xminner, y, y1, z, z1, z2;

	double BIG, Maxreal;
	int BASE, MAXNUMDIG, ipower, tenlogBASE, Maxexpo, Minexpo, Dblbits;

#ifdef SIGNAL
	signal(SIGFPE, overflow);
	if (setjmp(lab)!=0) { printf("Unexpected over/underflow\n"); exit(1); }
#endif

/****** Calculate max short *********************************************/
/*      Calculate 2**n-1 until overflow - then use the previous value	*/

	newshort=1; maxshort=0;

	if (setjmp(lab)==0)
		for(shortpower=0; newshort>maxshort; shortpower++) {
			maxshort=newshort;
			newshort=newshort*2+1;
		}

	/* Now for those daft Cybers: */

	maxershort=0; newshort=maxshort;

	if (setjmp(lab)==0)
		for(shortbits=shortpower; newshort>maxershort; shortbits++) {
			maxershort=newshort;
			newshort=newshort+newshort+1;
		}

	bits= (shortbits+1)/sizeof(short);
	c= (char)(-1);
	printf("/\* char=%d bits, %ssigned *\/\n", sizeof(c)*bits,
			((int)c)<0?"":"un");
	printf("/\* maxshort=%d (=2**%d-1) *\/\n", maxshort, shortpower);

	if (maxershort>maxshort) {
		printf("/\* There is a larger maxshort, %d (=2**%d-1), %s *\/\n",
			maxershort, shortbits, 
			"but only for addition, not multiplication");
	}

/****** Calculate max int by the same method ***************************/

	newint=1; maxint=0;

	if (setjmp(lab)==0)
		for(intpower=0; newint>maxint; intpower++) {
			maxint=newint;
			newint=newint*2+1;
		}

	printf("/\* maxint=%d (=2**%d-1) *\/\n", maxint, intpower);

/****** Calculate max long by the same method ***************************/

	newlong=1; maxlong=0;

	if (setjmp(lab)==0)
		for(longpower=0; newlong>maxlong; longpower++) {
			maxlong=newlong;
			newlong=newlong*2+1;
		}

	if (setjmp(lab)!=0) { printf("\nUnexpected under/overflow\n"); exit(1); }

	printf("/\* maxlong=%ld (=2**%d-1) *\/\n", maxlong, longpower);

/****** Pointers ********************************************************/
	printf("/\* pointers=%d bits%s *\/\n", sizeof(p)*bits,
		sizeof(p)>sizeof(int)?" BEWARE! larger than int!":"");

/****** Base and size of mantissa ***************************************/
	a=1.0;
	do { a=Dsum(a, a); } while (Ddiff(Ddiff(Dsum(a, 1.0), a), 1.0) == 0.0);
	b=1.0;
	do { b=Dsum(b, b); } while ((base=Ddiff(Dsum(a, b), a)) == 0.0);
	ibase=base;
	printf("/\* base=%d *\/\n", ibase);

	imant=0; b=1.0;
	do { imant++; b=Dmul(b, base); }
	while (Ddiff(Ddiff(Dsum(b,1.0),b),1.0) == 0.0);
	printf("/\* Significant base digits=%d *\/\n", imant);

/****** Various flavours of epsilon *************************************/
	basem1=Ddiff(base,1.0);
	if (Ddiff(Dsum(a, basem1), a) != 0.0) irnd=1; 
	else irnd=0;

	negeps=imant+imant;
	basein=1.0/base;
	a=1.0;
	for(i=1; i<=negeps; i++) a*=basein;

	b=a;
	while (Ddiff(Ddiff(1.0, a), 1.0) == 0.0) {
		a*=base;
		negeps--;
	}
	negeps= -negeps;
	printf("/\* Smallest x such that 1.0-base**x != 1.0=%d *\/\n", negeps);

	epsneg=a;
	if ((ibase!=2) && (irnd==1)) {
	/*	a=(a*(1.0+a))/(1.0+1.0); => */
		a=Ddiv(Dmul(a, Dsum(1.0, a)), Dsum(1.0, 1.0));
	/*	if ((1.0-a)-1.0 != 0.0) epsneg=a; => */
		if (Ddiff(Ddiff(1.0, a), 1.0) != 0.0) epsneg=a;
	}
	printf("/\* Small x such that 1.0-x != 1.0=%g *\/\n", epsneg);
	/* it may not be the smallest */

	machep= -imant-imant;
	a=b;
	while (Ddiff(Dsum(1.0, a), 1.0) == 0.0) { a*=base; machep++; }
	printf("/\* Smallest x such that 1.0+base**x != 1.0=%d *\/\n", machep);

	eps=a;
	if ((ibase!=2) && (irnd==1)) {
	/*	a=(a*(1.0+a))/(1.0+1.0); => */
		a=Ddiv(Dmul(a, Dsum(1.0, a)), Dsum(1.0, 1.0));
	/*	if ((1.0+a)-1.0 != 0.0) eps=a; => */
		if (Ddiff(Dsum(1.0, a), 1.0) != 0.0) eps=a;
	}
	printf("/\* Smallest x such that 1.0+x != 1.0=%g *\/\n", eps);

/****** Round or chop ***************************************************/
	ngrd=0;
	if (irnd == 1) { printf("/\* Arithmetic rounds *\/\n"); }
	else { 
		printf("/\* Arithmetic chops");
		if (Ddiff(Dmul(Dsum(1.0,eps),1.0),1.0) != 0.0) {
			ngrd=1;
			printf(" but uses guard digits");
		}
		printf(" *\/\n");
	}

/****** Size of and minimum normalised exponent ****************************/
	y=0; i=0; k=1; z=basein; z1=(1.0+eps)/base;

	/* Coarse search for the largest power of two */
	if (setjmp(lab)==0) /* in case of underflow trap */
		do {
			y=z; y1=z1;
			z=Dmul(y,y); z1=Dmul(z1, y);
			a=Dmul(z,1.0);
			z2=Ddiv(z1,y);
			if (z2 != y1) break;
			if ((Dsum(a,a) == 0.0) || (fabs(z) >= y)) break;
			i++;
			k+=k;
		} while(1);

	if (ibase != 10) {
		iexp=i+1; /* for the sign */
		mx=k+k;
	} else {
		iexp=2;
		iz=ibase;
		while (k >= iz) { iz*=ibase; iexp++; }
		mx=iz+iz-1;
	}

	/* Fine tune starting with y and y1 */
	if (setjmp(lab)==0) /* in case of underflow trap */
		do {
			xmin=y; z1=y1;
			y=Ddiv(y,base); y1=Ddiv(y1,base);
			a=Dmul(y,1.0);
			z2=Dmul(y1,base);
			if (z2 != z1) break;
			if ((Dsum(a,a) == 0.0) || (fabs(y) >= xmin)) break;
			k++;
		} while (1);

	if (setjmp(lab)!=0) { printf("Unexpected over/underflow\n"); exit(1); }

	minexp= (-k)+1;

	if ((mx <= k+k-3) && (ibase != 10)) { mx+=mx; iexp+=1; }
	printf("/\* Number of bits used for exponent=%d *\/\n", iexp);
	printf("/\* Minimum normalised exponent=%d *\/\n", minexp);
	printf("/\* Minimum normalised positive number=%g *\/\n", xmin);

/****** Minimum exponent ***************************************************/
	if (setjmp(lab)==0) /* in case of underflow trap */
		do {
			xminner=y;
			y=Ddiv(y,base);
			a=Dmul(y,1.0);
			if ((Dsum(a,a) == 0.0) || (fabs(y) >= xminner)) break;
		} while (1);

	if (setjmp(lab)!=0) { printf("Unexpected over/underflow\n"); exit(1); }

	normalised=1;
	if (xminner != 0.0 && xminner != xmin) {
		normalised=0;
		printf("/\* The smallest numbers are not kept normalised *\/\n");
		printf("/\* Smallest unnormalised positive number=%g *\/\n",
			xminner);
	}

/****** Maximum exponent ***************************************************/
	maxexp=2; xmax=1.0; newxmax=base+1.0;
	if (setjmp(lab) == 0) {
		while (xmax<newxmax) {
			xmax=newxmax;
			newxmax=Dmul(newxmax, base);
			if (Ddiv(newxmax, base) != xmax) break; /* ieee infinity */
			maxexp++;
		}
	}
	if (setjmp(lab)!=0) { printf("Unexpected over/underflow\n"); exit(1); }

	printf("/\* Maximum exponent=%d *\/\n", maxexp);

/****** Largest and smallest numbers ************************************/
	xmax=Ddiff(1.0, epsneg);
	if (Dmul(xmax,1.0) != xmax) xmax=Ddiff(1.0, Dmul(base,epsneg));
	for (i=1; i<=maxexp; i++) xmax=Dmul(xmax, base);
	printf("/\* Maximum number=%g *\/\n", xmax);

/****** Hidden bit + sanity check ***************************************/
	if (ibase != 10) {
		mantbits=log(2, (double)ibase)*imant;
		if (mantbits+iexp+1 == sizeof(double)*bits+1) {
			printf("/\* Double arithmetic uses a hidden bit *\/\n");
		} else if (mantbits+iexp+1 == sizeof(double)*bits) {
			printf("/\* Double arithmetic doesn't use a hidden bit *\/\n");
		} else {
			printf("/\* Something fishy here! %s %s *\/\n",
				"Exponent size + mantissa size doesn't match",
				"with the size of a double.");
		}
	}

/****** The point of it all: ********************************************/
	printf("\n/\* Numeric package constants *\/\n");

	BIG= power(ibase, imant)-1.0;
	MAXNUMDIG= log(10, BIG);
	ipower= log(10, (double)(maxint/2));
	Maxreal= xmax;
	Maxexpo= log(2, (double)ibase)*maxexp;
	Minexpo= log(2, (double)ibase)*minexp;
	Dblbits= log(2, (double)ibase)*imant;
	tenlogBASE= min(MAXNUMDIG/2, ipower);
	BASE=1; for(i=1; i<=tenlogBASE; i++) BASE*=10;

	printf("#define BIG %1.1f /\* Maximum integral double *\/\n", BIG);
	printf("#define LONG ");
		for(i=1; i<=MAXNUMDIG; i++) printf("9");
		printf(".5 /\* Maximum power of ten less than BIG *\/\n");
	printf("#define MAXNUMDIG %d /\* The number of 9's in LONG *\/\n",
		MAXNUMDIG);
	printf("#define MINNUMDIG 6 /\* Don't change: this is here for consistency *\/\n");

	printf("#define BASE %d /\* %s *\/\n",
		BASE, "BASE**2<=BIG && BASE*2<=Maxint && -2*BASE>=Minint");
	if (((double)BASE)*BASE > BIG || ((double)BASE)+BASE > maxint) {
		printf("*** BASE value wrong\n");
		exit(1);
	}
	printf("#define tenlogBASE %d /\*  = log10(BASE) *\/\n", tenlogBASE);
	printf("#define Maxreal %e /\* Maximum double *\/\n", Maxreal);
	printf("#define Maxexpo %d /\* Maximum value such that 2**Maxexpo<=Maxreal *\/\n",
		Maxexpo);
	printf("#define Minexpo (%d) /\* Minimum value such that -2**Minexpo>=Minreal *\/\n",
		Minexpo);
	printf("#define Dblbits %d /\* Maximum value such that 2**Dblbits<=BIG *\/\n",
		Dblbits);

	printf("#define Maxintlet %d /\* Maximum short *\/\n", maxshort);
	printf("#define Maxint %d /\* Maximum int *\/\n", maxint);

	printf("#define Maxsmallint %d /\* BASE-1 *\/\n", BASE-1);
	printf("#define Minsmallint (%d) /\* -BASE *\/\n", -BASE);

/* An extra goody: the approximate amount of data-space */
/* Put here because it is likely to be slower then the rest */

	/*Allocate blocks of 1000 until no more available*/
	/*Don't be tempted to change this to 1024: */
	/*we don't know how much header information there is*/

	for(count=0; (p=(int *)malloc(1000))!=0; count++) { }

	printf("\n/\* Memory~= %d000 *\/\n", count);

	exit(0);
}
