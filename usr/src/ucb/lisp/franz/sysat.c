#ifndef lint
static char *rcsid =
   "$Header: sysat.c,v 1.20 85/03/13 17:19:21 sklower Exp $";
#endif

/*					-[Thu Sep 29 14:05:32 1983 by jkf]-
 * 	sysat.c				$Locker:  $
 * startup data structure creation
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"
#include "lfuncs.h"
#define FIDDLE(z,b,c,y) z->a.clb=newdot(); (z->a.clb->d.car=newint())->i=b->i; \
	z->a.clb->d.cdr=newdot(); (z->a.clb->d.cdr->d.car=newint())->i=c->i; \
	z->a.clb->d.cdr->d.cdr=newdot(); (z->a.clb->d.cdr->d.cdr->d.car=newint())->i=y; \
	b = z->a.clb->d.car; c = z->a.clb->d.cdr->d.car; \
	copval(z,z->a.clb); z->a.clb = nil;

#define cforget(x) protect(x); Lforget(); unprot();

/*  The following array serves as the temporary counters of the items	*/
/*  and pages used in each space.					*/

long int tint[2*NUMSPACES];

extern int tgcthresh; 
extern int initflag; 	/*  starts off TRUE to indicate unsafe to gc  */

extern int *beginsweep;	/* place for garbage collector to begin sweeping */
extern int page_limit;  /* begin warning messages about running out of space */
extern char purepage[]; /* which pages should not be swept by gc */
extern int ttsize;	/* need to know how much of pagetable to set to other */

extern lispval Iaddstat(), Isstatus();
lispval inewatom();

makevals()
	{
	int i;
	lispval temp;

	/*  system list structure and atoms are initialized.  */

	/*  Before any lisp data can be created, the space usage */
	/*  counters must be set up, temporarily in array tint.  */

	atom_items = (lispval) &tint[0];
	atom_pages = (lispval) &tint[1];
	str_items = (lispval) &tint[2];
	str_pages = (lispval) &tint[3];
	int_items = (lispval) &tint[4];
	int_pages = (lispval) &tint[5];
	dtpr_items = (lispval) &tint[6];
	dtpr_pages = (lispval) &tint[7];
	doub_items = (lispval) &tint[8];
	doub_pages = (lispval) &tint[9];
	sdot_items = (lispval) &tint[10];
	sdot_pages = (lispval) &tint[11];
	array_items = (lispval) &tint[12];
	array_pages = (lispval) &tint[13];
	val_items = (lispval) &tint[14];
	val_pages = (lispval) &tint[15];
	funct_items = (lispval) &tint[16];
	funct_pages = (lispval) &tint[17];

	for (i=0; i < 7; i++)
	{
		hunk_pages[i] = (lispval) &tint[18+i*2];
		hunk_items[i] = (lispval) &tint[19+i*2];
	}

	vect_items = (lispval) &tint[34];
	vecti_items = (lispval) &tint[35];
	vect_pages = (lispval) &tint[36];
	vecti_pages = (lispval) &tint[37];
	other_items = (lispval) &tint[38];
	other_pages = (lispval) &tint[39];
	
	/*  This also applies to the garbage collection threshhold  */

	gcthresh = (lispval) &tgcthresh;

	/*  Now we commence constructing system lisp structures.  */

	/*  nil is a special case, constructed especially at location zero  */

	hasht[hashfcn("nil")] = (struct atom *)nil;


	/* allocate space for namestack and bindstack first
	 * then set up beginsweep variable so that the sweeper will
	 * ignore these `always in use' pages
	 */

	lbot = orgnp = np = ((struct argent *)csegment(VALUE,NAMESIZE,FALSE));
	orgbnp = bnp = ((struct nament *)csegment(DTPR,NAMESIZE,FALSE));
	/* since these dtpr pages will not be swept, we don't want them
	 * to show up in count of dtpr pages allocated or it will confuse
	 * gcafter when it tries to determine how much space is free
	 */
	dtpr_pages->i = 0;
	beginsweep = (int *) xsbrk(0);

	/*
	 *  patching up info in type and pure tables
	 */
#if unisys3botch
	/*
	 * This code is in here because Schriebman made Romberger tend
	 * more important things for too long for Apple and Fateman to
	 * wait
	 */
	{extern int dmpmode; int jj = ATOX(beginsweep);
	dmpmode = 407; for(i=19;i < jj; i++) typetable[i] = 0; }
#endif
	for(i=ATOX(beginsweep); i < ttsize; i++) (typetable+1)[i] = OTHER;
	purepage[ATOX(np)] = 1;  /* Mark these as non-gc'd arrays */
	purepage[ATOX(bnp)] = 1;

	/*
	 * Names of various spaces and things
	 */

	atom_name = inewatom("symbol");
	str_name = inewatom("string");
	int_name = inewatom("fixnum");
	dtpr_name = inewatom("list");
	doub_name = inewatom("flonum");
	sdot_name = inewatom("bignum");
	array_name = inewatom("array");
	val_name = inewatom("value");
	funct_name = inewatom("binary");
	port_name = inewatom("port");		/* not really a space */
	vect_name = inewatom("vector");
	vecti_name = inewatom("vectori");
	other_name = inewatom("other");

	{
	    char name[6], *strcpy();

	    strcpy(name, "hunk0");
	    for (i=0; i< 7; i++) {
		hunk_name[i] = matom(name);
		name[4]++;
	    }
	}
	
	/*  set up the name stack as an array of pointers */
	nplim = orgnp+NAMESIZE-6*NAMINC;
	temp = inewatom("namestack");
	nstack = temp->a.fnbnd = newarray();
	nstack->ar.data = (char *) (np);
	(nstack->ar.length = newint())->i = NAMESIZE;
	(nstack->ar.delta = newint())->i = sizeof(struct argent);
	Vnogbar = inewatom("unmarked_array");
	/* marking of the namestack will be done explicitly in gc1 */
	(nstack->ar.aux = newdot())->d.car = Vnogbar; 
						

	/* set up the binding stack as an array of dotted pairs */

	bnplim = orgbnp+NAMESIZE-5;
	temp = inewatom("bindstack");
	bstack = temp->a.fnbnd = newarray();
	bstack->ar.data = (char *) (bnp);
	(bstack->ar.length = newint())->i = NAMESIZE;
	(bstack->ar.delta = newint())->i = sizeof(struct nament);
	/* marking of the bindstack will be done explicitly in gc1 */
	(bstack->ar.aux = newdot())->d.car = Vnogbar; 

	/* more atoms */

	tatom = inewatom("t");
	tatom->a.clb = tatom;
	lambda = inewatom("lambda");
	nlambda = inewatom("nlambda");
	cara = inewatom("car");
	cdra = inewatom("cdr");
	Veval = inewatom("eval");
	quota = inewatom("quote");
	reseta = inewatom("reset");
	gcafter = inewatom("gcafter");	/* garbage collection wind-up */
	macro = inewatom("macro");
	ibase = inewatom("ibase");		/* base for input conversion */
	ibase->a.clb = inewint(10);
	(inewatom("base"))->a.clb = ibase->a.clb;
	fclosure = inewatom("fclosure");
	clos_marker = inewatom("int:closure-marker");
	Vpbv = inewatom("value-structure-argument");
	rsetatom = inewatom("*rset");
	rsetatom->a.clb = nil;
	Vsubrou = inewatom("subroutine");
	Vpiport = inewatom("piport");
	Vpiport->a.clb = P(piport = stdin);	/* standard input */
	Vpoport = inewatom("poport");
	Vpoport->a.clb = P(poport = stdout);	/* stand. output */
	inewatom("errport")->a.clb = (P(errport = stderr));/* stand. err. */
	ioname[PN(stdin)]  = (lispval) pinewstr("$stdin");
	ioname[PN(stdout)] = (lispval) pinewstr("$stdout");
	ioname[PN(stderr)] = (lispval) pinewstr("$stderr");
	inewatom("Standard-Input")->a.clb = Vpiport->a.clb;
	inewatom("Standard-Output")->a.clb = Vpoport->a.clb;
	inewatom("Standard-Error")->a.clb = P(errport);
	(Vreadtable = inewatom("readtable"))->a.clb  = Imkrtab(0);
	strtab = Imkrtab(0);
	Vptport = inewatom("ptport");
	Vptport->a.clb = nil;				/* protocal port */

	Vcntlw = inewatom("^w");	/* when non nil, inhibits output to term */
	Vcntlw->a.clb = nil;

	Vldprt = inewatom("$ldprint");	
			/* when nil, inhibits printing of fasl/autoload   */
						/* cfasl messages to term */
	Vldprt->a.clb = tatom;

	Vprinlevel = inewatom("prinlevel");	/* printer recursion count */
	Vprinlevel->a.clb = nil;		/* infinite recursion */

	Vprinlength = inewatom("prinlength");	/* printer element count */
	Vprinlength->a.clb = nil;		/* infinite elements */

	Vfloatformat = inewatom("float-format");
	Vfloatformat->a.clb = (lispval) pinewstr("%.16g");

	Verdepth = inewatom("Error-Depth");
	Verdepth->a.clb = inewint(0);		/* depth of error */

	Vpurcopylits = inewatom("$purcopylits");
	Vpurcopylits->a.clb = tatom;		/* tells fasl to purcopy
						 *  literals it reads
						 */
	Vdisplacemacros = inewatom("displace-macros");
        Vdisplacemacros->a.clb = nil;		/* replace macros calls
						 * with their expanded forms
						 */

	Vprintsym = inewatom("print");
	
	atom_buffer = (lispval) strbuf;
	Vlibdir = inewatom("lisp-library-directory");
	Vlibdir->a.clb = inewatom("/usr/lib/lisp");
	/*  The following atoms are used as tokens by the reader  */

	perda = inewatom(".");
	lpara = inewatom("(");
	rpara = inewatom(")");
	lbkta = inewatom("[");
	rbkta = inewatom("]");
	snqta = inewatom("'");
	exclpa = inewatom("!");


	(Eofa = inewatom("eof"))->a.clb = eofa;

	/*  The following few atoms have values the reader tokens.  */
	/*  Perhaps this is a kludge which should be abandoned.  */
	/*  On the other hand, perhaps it is an inspiration.	*/

	inewatom("perd")->a.clb = perda;
	inewatom("lpar")->a.clb = lpara;
	inewatom("rpar")->a.clb = rpara;
	inewatom("lbkt")->a.clb = lbkta;
	inewatom("rbkt")->a.clb = rbkta;

	noptop = inewatom("noptop");

	/*  atoms used in connection with comments.  */

	commta = inewatom("comment");
	rcomms = inewatom("readcomments");

	/*  the following atoms are used for lexprs */

	lexpr_atom = inewatom("last lexpr binding\7");
	lexpr = inewatom("lexpr");

	/* the following atom is used to reference the bind stack for eval */
	bptr_atom = inewatom("eval1 binding pointer\7");
	bptr_atom->a.clb = nil;

	/* the following atoms are used for evalhook hackery */
	evalhatom = inewatom("evalhook");
	evalhatom->a.clb = nil;
	evalhcallsw = FALSE;

	funhatom = inewatom("funcallhook");
	funhatom->a.clb = nil;
	funhcallsw = FALSE;

	Vevalframe = inewatom("evalframe");

	sysa = inewatom("sys");
	plima = inewatom("pagelimit");	/*  max number of pages  */


	startup = inewatom("startup");	/*  used by save and restore  */
	sysa = inewatom("sys");	/*  sys indicator for system variables  */
	splice = inewatom("splicing");


	
	/* vector stuff */

	odform = inewatom("odformat");	/* format for printf's used in od */
	rdrsdot = newsdot();		/* used in io conversions of bignums */
	rdrsdot2 = newsdot();		/* used in io conversions of bignums */
	rdrint = newint();		/* used as a temporary integer */
	(nilplist = newdot())->d.cdr = newdot();
					/* used as property list for nil,
					   since nil will eventually be put at
					   0 (consequently in text and not
					   writable) */

	/* error variables */
	(Vererr = inewatom("ER%err"))->a.clb = nil;
	(Vertpl = inewatom("ER%tpl"))->a.clb = nil;
	(Verall = inewatom("ER%all"))->a.clb = nil;
	(Vermisc = inewatom("ER%misc"))->a.clb = nil;
	(Verbrk = inewatom("ER%brk"))->a.clb = nil;
	(Verundef = inewatom("ER%undef"))->a.clb = nil;
	(Vlerall = newdot())->d.car = Verall;	/* list (ER%all) */
	(Veruwpt = inewatom("ER%unwind-protect"))->a.clb = nil;
	(Verrset = inewatom("errset"))->a.clb = nil;


	/* set up the initial status list */

	stlist = nil;			/* initially nil */
	{
	    lispval feature, dom;
	    Iaddstat(inewatom("features"),ST_READ,ST_NO,nil);
	    Iaddstat(feature = inewatom("feature"),ST_FEATR,ST_FEATW,nil);
	    Isstatus(feature,inewatom("franz"));
	    Isstatus(feature,inewatom("Franz"));
	    Isstatus(feature,inewatom(OS));
	    Isstatus(feature,inewatom("string"));
	    Isstatus(feature,dom = inewatom(DOMAIN));
	    Iaddstat(inewatom("domain"),ST_READ,ST_NO,dom);
	    Isstatus(feature,inewatom(MACHINE));
#ifdef PORTABLE
	    Isstatus(feature,inewatom("portable"));
#endif
#ifdef unisoft
	    Isstatus(feature,inewatom("unisoft"));
#endif
#ifdef sun
	    Isstatus(feature,inewatom("sun"));
#endif
#ifdef os_masscomp
	    Isstatus(feature,inewatom("mc500"));
#endif
#if os_4_1c | os_4_2 | os_4_3
	    Isstatus(feature,inewatom("long-filenames"));
#endif
	}
	Iaddstat(inewatom("nofeature"),ST_NFETR,ST_NFETW,nil);
	Iaddstat(inewatom("syntax"),ST_SYNT,ST_NO,nil);
	Iaddstat(inewatom("uctolc"),ST_READ,ST_TOLC,nil);
	Iaddstat(inewatom("dumpcore"),ST_READ,ST_CORE,nil);
	Isstatus(inewatom("dumpcore"),nil);	/*set up signals*/

	Iaddstat(inewatom("chainatom"),ST_RINTB,ST_INTB,inewint(0));
	Iaddstat(inewatom("dumpmode"),ST_DMPR,ST_DMPW,nil);
	Iaddstat(inewatom("appendmap"),ST_READ,ST_SET,nil);  /* used by fasl */
	Iaddstat(inewatom("debugging"),ST_READ,ST_SET,nil);  
	Iaddstat(inewatom("evalhook"),ST_RINTB,ST_INTB,inewint(3));
	Isstatus(inewatom("evalhook"),nil); /*evalhook switch off */
	Iaddstat(inewatom("bcdtrace"),ST_READ,ST_BCDTR,nil);
	Iaddstat(inewatom("ctime"),ST_CTIM,ST_NO,nil);
	Iaddstat(inewatom("localtime"),ST_LOCT,ST_NO,nil);
	Iaddstat(inewatom("isatty"),ST_ISTTY,ST_NO,nil);
	Iaddstat(inewatom("ignoreeof"),ST_READ,ST_SET,nil);
	Iaddstat(inewatom("version"),ST_READ,ST_NO,mstr("Franz Lisp, Opus 38"));
	Iaddstat(inewatom("automatic-reset"),ST_READ,ST_AUTR,nil);
	Iaddstat(inewatom("translink"),ST_READ,ST_TRAN,nil);
	Isstatus(inewatom("translink"),nil);		/* turn off tran links */
	Iaddstat(inewatom("undeffunc"),ST_UNDEF,ST_NO,nil); /* list undef funcs */
	Iaddstat(inewatom("gcstrings"),ST_READ,ST_GCSTR,nil); /* gc strings */

	/* garbage collector things */

	gcport = inewatom("gcport");	/* port for gc dumping */
	gccheck = inewatom("gccheck");	/* flag for checking during gc */
	gcdis = inewatom("gcdisable");	/* variable for disabling the gc */
	gcdis->a.clb = nil;
	gcload = inewatom("gcload");	/* option for gc while loading */
	loading = inewatom("loading");	/* flag--in loader if = t  */
	noautot = inewatom("noautotrace");	/* option to inhibit auto-trace */
	Vgcprint = inewatom("$gcprint");	/* if t then pring gc messages */
	Vgcprint->a.clb = nil;
	
	(gcthresh = newint())->i = tgcthresh;
	gccall1 = newdot();  gccall2 = newdot();  /* used to call gcafter */
	gccall1->d.car = gcafter;  /* start constructing a form for eval */

	arrayst = mstr("ARRAY");	/* array marker in name stack */
	bcdst = mstr("BINARY");		/* binary function marker */
	listst = mstr("INTERPRETED");	/* interpreted function marker */
	macrost = mstr("MACRO");	/* macro marker */
	protst = mstr("PROTECTED");	/* protection marker */
	badst = mstr("BADPTR");		/* bad pointer marker */
	argst = mstr("ARGST");		/* argument marker */
	hunkfree = mstr("EMPTY");	/* empty hunk cell value */

	/* type names */

	FIDDLE(atom_name,atom_items,atom_pages,ATOMSPP);
	FIDDLE(str_name,str_items,str_pages,STRSPP);
	FIDDLE(other_name,other_items,other_pages,STRSPP);
	FIDDLE(int_name,int_items,int_pages,INTSPP);
	FIDDLE(dtpr_name,dtpr_items,dtpr_pages,DTPRSPP);
	FIDDLE(doub_name,doub_items,doub_pages,DOUBSPP);
	FIDDLE(sdot_name,sdot_items,sdot_pages,SDOTSPP);
	FIDDLE(array_name,array_items,array_pages,ARRAYSPP);
	FIDDLE(val_name,val_items,val_pages,VALSPP);
	FIDDLE(funct_name,funct_items,funct_pages,BCDSPP);

	FIDDLE(hunk_name[0], hunk_items[0], hunk_pages[0], HUNK2SPP);
	FIDDLE(hunk_name[1], hunk_items[1], hunk_pages[1], HUNK4SPP);
	FIDDLE(hunk_name[2], hunk_items[2], hunk_pages[2], HUNK8SPP);
	FIDDLE(hunk_name[3], hunk_items[3], hunk_pages[3], HUNK16SPP);
	FIDDLE(hunk_name[4], hunk_items[4], hunk_pages[4], HUNK32SPP);
	FIDDLE(hunk_name[5], hunk_items[5], hunk_pages[5], HUNK64SPP);
	FIDDLE(hunk_name[6], hunk_items[6], hunk_pages[6], HUNK128SPP);
	
	FIDDLE(vect_name, vect_items, vect_pages, VECTORSPP)
	FIDDLE(vecti_name, vecti_items, vecti_pages, VECTORSPP)

	(plimit = newint())->i = page_limit;
	copval(plima,plimit);  /*  default value  */

	/* the following atom is used when reading caar, cdar, etc. */

	xatom = inewatom("??");
	dofuns();
#if sun_4_1c ||sun_4_2 || sun_4_2beta
	hookupcore();
#endif
	/*  now it is OK to collect garbage  */

	initflag = FALSE;
	}

/*  matom("name")  ******************************************************/
/*									*/
/*  simulates an atom being read in from the reader and returns a	*/
/*  pointer to it.							*/
/*									*/
/*  BEWARE:  if an atom becomes "truly worthless" and is collected,	*/
/*  the pointer becomes obsolete.					*/
/*									*/
lispval
matom(string)
char *string;
	{
	strbuf[0] = 0;
	strncat(strbuf,string,STRBLEN-1); /* strcpyn always pads to n */
	strbuf[STRBLEN-1] = 0;
	return(getatom(TRUE));
	}

/*  mstr  ***************************************************************/
/*									*/
/*  Makes a string.  Uses matom.					*/
/*  Not the most efficient but will do until the string from the code	*/
/*  itself can be used as a lispval.					*/

lispval mstr(string) char *string;
	{
	return((lispval)(pinewstr(string)));
	}

/*  mfun("name",start)  *************************************************/
/*									*/
/*  Same as matom, but entry point to c code is associated with		*/
/*  "name" as function binding.						*/
/*  A pointer to the atom is returned.					*/
/*									*/
lispval mfun(string,start,discip) char *string; lispval (*start)(), discip;
	{
	lispval v;
	v = inewatom(string);
	v->a.fnbnd = newfunct();
	v->a.fnbnd->bcd.start = start;
	v->a.fnbnd->bcd.discipline = discip;
	return(v);
	}

struct ftab {
	char *string;
	lispval (*start)();
	lispval *discip;
};

lispval
mftab(table)
register struct ftab *table;
{
	register lispval v;
	for(;table->string;table++) {
		v = inewatom(table->string);
		v = v->a.fnbnd = newfunct();
		v->bcd.start = table->start;
		v->bcd.discipline = *table->discip;
	}
}

static struct ftab cfuns[] = {
  {"car", Lcar, &(lambda)},
  {"cdr", Lcdr, &(lambda)},
  {"eval", Leval1, &(lambda)},
  {"asin", Lasin, &(lambda)},
  {"acos", Lacos, &(lambda)},
  {"atan", Latan, &(lambda)},
  {"cos", Lcos, &(lambda)},
  {"sin", Lsin, &(lambda)},
  {"sqrt", Lsqrt, &(lambda)},
  {"exp", Lexp, &(lambda)},
  {"log", Llog, &(lambda)},
  {"lsh", Llsh, &(lambda)},
  {"bignum-leftshift", Lbiglsh, &(lambda)},
  {"sticky-bignum-leftshift", Lsbiglsh, &(lambda)},
  {"frexp", Lfrexp, &(lambda)},
  {"rot", Lrot, &(lambda)},
  {"random", Lrandom, &(lambda)},
  {"atom", Latom, &(lambda)},
  {"apply", Lapply, &(lambda)},
  {"funcall", Lfuncal, &(lambda)},
  {"lexpr-funcall", Llexfun, &(lambda)},
  {"return", Lreturn, &(lambda)},
/* 	MK("cont",Lreturn,lambda),  */
  {"cons", Lcons, &(lambda)},
  {"scons", Lscons, &(lambda)},
  {"bignum-to-list", Lbigtol, &(lambda)},
  {"cadr", Lcadr, &(lambda)},
  {"caar", Lcaar, &(lambda)},
  {"cddr", Lc02r, &(lambda)},
  {"caddr", Lc12r, &(lambda)},
  {"cdddr", Lc03r, &(lambda)},
  {"cadddr", Lc13r, &(lambda)},
  {"cddddr", Lc04r, &(lambda)},
  {"caddddr", Lc14r, &(lambda)},
  {"nthelem", Lnthelem, &(lambda)},
  {"eq", Leq, &(lambda)},
  {"equal", Lequal, &(lambda)},
/**	MK("zqual",Zequal,lambda), 	*/
  {"numberp", Lnumberp, &(lambda)},
  {"dtpr", Ldtpr, &(lambda)},
  {"bcdp", Lbcdp, &(lambda)},
  {"portp", Lportp, &(lambda)},
  {"arrayp", Larrayp, &(lambda)},
  {"valuep", Lvaluep, &(lambda)},
  {"get_pname", Lpname, &(lambda)},
  {"ptr", Lptr, &(lambda)},
  {"arrayref", Larayref, &(lambda)},
  {"marray", Lmarray, &(lambda)},
  {"getlength", Lgetl, &(lambda)},
  {"putlength", Lputl, &(lambda)},
  {"getaccess", Lgeta, &(lambda)},
  {"putaccess", Lputa, &(lambda)},
  {"getdelta", Lgetdel, &(lambda)},
  {"putdelta", Lputdel, &(lambda)},
  {"getaux", Lgetaux, &(lambda)},
  {"putaux", Lputaux, &(lambda)},
  {"getdata", Lgetdata, &(lambda)},
  {"putdata", Lputdata, &(lambda)},
  {"mfunction", Lmfunction, &(lambda)},
  {"getentry", Lgtentry, &(lambda)},
  {"getdisc", Lgetdisc, &(lambda)},
  {"putdisc", Lputdisc, &(lambda)},
  {"segment", Lsegment, &(lambda)},
  {"rplaca", Lrplca, &(lambda)},
  {"rplacd", Lrplcd, &(lambda)},
  {"set", Lset, &(lambda)},
  {"replace", Lreplace, &(lambda)},
  {"infile", Linfile, &(lambda)},
  {"outfile", Loutfile, &(lambda)},
  {"terpr", Lterpr, &(lambda)},
  {"print", Lprint, &(lambda)},
  {"close", Lclose, &(lambda)},
  {"patom", Lpatom, &(lambda)},
  {"pntlen", Lpntlen, &(lambda)},
  {"read", Lread, &(lambda)},
  {"ratom", Lratom, &(lambda)},
  {"readc", Lreadc, &(lambda)},
  {"truename", Ltruename, &(lambda)},
  {"implode", Limplode, &(lambda)},
  {"maknam", Lmaknam, &(lambda)},
  {"deref", Lderef, &(lambda)},
  {"concat", Lconcat, &(lambda)},
  {"uconcat", Luconcat, &(lambda)},
  {"putprop", Lputprop, &(lambda)},
  {"monitor", Lmonitor, &(lambda)},
  {"get", Lget, &(lambda)},
  {"getd", Lgetd, &(lambda)},
  {"putd", Lputd, &(lambda)},
  {"prog", Nprog, &(nlambda)},
  {"quote", Nquote, &(nlambda)},
  {"function", Nfunction, &(nlambda)},
  {"go", Ngo, &(nlambda)},
  {"*catch", Ncatch, &(nlambda)},
  {"errset", Nerrset, &(nlambda)},
  {"status", Nstatus, &(nlambda)},
  {"sstatus", Nsstatus, &(nlambda)},
  {"err-with-message", Lerr, &(lambda)},
  {"*throw", Nthrow, &(lambda)},	/* this is a lambda now !! */
  {"reset", Nreset, &(nlambda)},
  {"break", Nbreak, &(nlambda)},
  {"exit", Lexit, &(lambda)},
  {"def", Ndef, &(nlambda)},
  {"null", Lnull, &(lambda)},
	  	/*{"framedump", Lframedump, &(lambda)},*/
  {"and", Nand, &(nlambda)},
  {"or", Nor, &(nlambda)},
  {"setq", Nsetq, &(nlambda)},
  {"cond", Ncond, &(nlambda)},
  {"list", Llist, &(lambda)},
  {"load", Lload, &(lambda)},
  {"nwritn", Lnwritn, &(lambda)},
  {"*process", Lprocess, &(lambda)},	/*  execute a shell command  */
  {"allocate", Lalloc, &(lambda)},	/*  allocate a page  */
  {"sizeof", Lsizeof, &(lambda)},	/*  size of one item of a data type  */
  {"dumplisp", Ndumplisp, &(nlambda)},	/*  NEW save the world  */
  {"top-level", Ntpl, &(nlambda)},	/*  top level eval-print read loop  */
  {"mapcar", Lmpcar, &(lambda)},
  {"maplist", Lmaplist, &(lambda)},
  {"mapcan", Lmapcan, &(lambda)},
  {"mapcon", Lmapcon, &(lambda)},
  {"assq", Lassq, &(lambda)},
  {"mapc", Lmapc, &(lambda)},
  {"map", Lmap, &(lambda)},
  {"flatc", Lflatsi, &(lambda)},
  {"alphalessp", Lalfalp, &(lambda)},
  {"drain", Ldrain, &(lambda)},
  {"killcopy", Lkilcopy, &(lambda)}, /*  forks aand aborts for adb */
  {"opval", Lopval, &(lambda)},	/*  sets and retrieves system variables  */
  {"ncons", Lncons, &(lambda)},
  {"remob", Lforget, &(lambda)},	/*  function to take atom out of hash table  */
  {"not", Lnull, &(lambda)},
  {"plus", Ladd, &(lambda)},
  {"add", Ladd, &(lambda)},
  {"times", Ltimes, &(lambda)},
  {"difference", Lsub, &(lambda)},
  {"quotient", Lquo, &(lambda)},
  {"+", Lfp, &(lambda)},
  {"-", Lfm, &(lambda)},
  {"*", Lft, &(lambda)},
  {"/", Lfd, &(lambda)},
  {"1+", Lfadd1, &(lambda)},
  {"1-", Lfsub1, &(lambda)},
  {"^", Lfexpt, &(lambda)},
  {"double-to-float", Ldbtofl, &(lambda)},
  {"float-to-double", Lfltodb, &(lambda)},
  {"<", Lflessp, &(lambda)},
  {"mod", Lmod, &(lambda)},
  {"minus", Lminus, &(lambda)},
  {"absval", Labsval, &(lambda)},
  {"add1", Ladd1, &(lambda)},
  {"sub1", Lsub1, &(lambda)},
  {"greaterp", Lgreaterp, &(lambda)},
  {"lessp", Llessp, &(lambda)},
  {"any-zerop", Lzerop, &(lambda)},   /* used when bignum arg possible */
  {"zerop", Lzerop, &(lambda)},
  {"minusp", Lnegp, &(lambda)},
  {"onep", Lonep, &(lambda)},
  {"sum", Ladd, &(lambda)},
  {"product", Ltimes, &(lambda)},
  {"do", Ndo, &(nlambda)},
  {"progv", Nprogv, &(nlambda)},
  {"progn", Nprogn, &(nlambda)},
  {"prog2", Nprog2, &(nlambda)},
  {"oblist", Loblist, &(lambda)},
  {"baktrace", Lbaktrace, &(lambda)},
  {"tyi", Ltyi, &(lambda)},
  {"tyipeek", Ltyipeek, &(lambda)},
  {"untyi", Luntyi, &(lambda)},
  {"tyo", Ltyo, &(lambda)},
  {"termcapinit", Ltci, &(lambda)},
  {"termcapexe", Ltcx, &(lambda)},
  {"int:setsyntax", Lsetsyn, &(lambda)},	/* an internal function */
  {"int:getsyntax", Lgetsyntax, &(lambda)},
  {"int:showstack", LIshowstack, &(lambda)},
  {"int:franz-call", LIfranzcall, &(lambda)},
  {"makereadtable", Lmakertbl, &(lambda)},
  {"zapline", Lzapline, &(lambda)},
  {"aexplode", Lxplda, &(lambda)},
  {"aexplodec", Lxpldc, &(lambda)},
  {"aexploden", Lxpldn, &(lambda)},
  {"hashtabstat", Lhashst, &(lambda)},
#ifdef METER
  {"gcstat", Lgcstat, &(lambda)},
#endif
  {"argv", Largv, &(lambda)},
  {"arg", Larg, &(lambda)},
  {"setarg", Lsetarg, &(lambda)},
  {"showstack", Lshostk, &(lambda)},
  {"freturn", Lfretn, &(lambda)},
  {"*rset", Lrset, &(lambda)},
  {"eval1", Leval1, &(lambda)},
  {"evalframe", Levalf, &(lambda)},
  {"evalhook", Levalhook, &(lambda)},
  {"funcallhook", Lfunhook, &(lambda)},
  {"int:fclosure-stack-stuff", LIfss, &(lambda)},
  {"resetio", Nioreset, &(nlambda)},
  {"chdir", Lchdir, &(lambda)},
  {"ascii", Lascii, &(lambda)},
  {"boole", Lboole, &(lambda)},
  {"type", Ltype, &(lambda)},	/* returns type-name of argument */
  {"fix", Lfix, &(lambda)},
  {"float", Lfloat, &(lambda)},
  {"fact", Lfact, &(lambda)},
  {"cpy1", Lcpy1, &(lambda)},
  {"Divide", LDivide, &(lambda)},
  {"Emuldiv", LEmuldiv, &(lambda)},
  {"readlist", Lreadli, &(lambda)},
  {"plist", Lplist, &(lambda)},	/* gives the plist of an atom */
  {"setplist", Lsetpli, &(lambda)},	/* get plist of an atom  */
  {"eval-when", Nevwhen, &(nlambda)},
  {"syscall", Lsyscall, &(lambda)},
  {"intern", Lntern, &(lambda)},
  {"ptime", Lptime, &(lambda)},	/* return process user time */
  {"fork", Lfork, &(lambda)},	/* turn on fork and wait */
  {"wait", Lwait, &(lambda)},
/*	MK("pipe",Lpipe,lambda),	*/
/*	MK("fdopen",Lfdopen,lambda), */
  {"exece", Lexece, &(lambda)},
  {"gensym", Lgensym, &(lambda)},
  {"remprop", Lremprop, &(lambda)},
  {"bcdad", Lbcdad, &(lambda)},
  {"symbolp", Lsymbolp, &(lambda)},
  {"stringp", Lstringp, &(lambda)},
  {"rematom", Lrematom, &(lambda)},
/**	MK("prname",Lprname,lambda),	*/
  {"getenv", Lgetenv, &(lambda)},
  {"I-throw-err", Lctcherr, &(lambda)}, /* directly force a throw or error */
  {"makunbound", Lmakunb, &(lambda)},
  {"haipart", Lhaipar, &(lambda)},
  {"haulong", Lhau, &(lambda)},
  {"signal", Lsignal, &(lambda)},
  {"fasl", Lfasl, &(lambda)},	/* NEW - new fasl loader */
  {"cfasl", Lcfasl, &(lambda)},	/* read in compiled C file */
  {"getaddress", Lgetaddress, &(lambda)},
  {"removeaddress", Lrmadd, &(lambda)}, 	/* unbind symbols    */
  {"make-c-thunk", Lmkcth, &(lambda)}, 	/* make wrappers    */
  {"boundp", Lboundp, &(lambda)},	/* tells if an atom is bound */
  {"fake", Lfake, &(lambda)},	/* makes a fake lisp pointer */
/***	MK("od",Lod,lambda),		/* dumps info */
  {"maknum", Lmaknum, &(lambda)},	/* converts a pointer to an integer */
  {"*mod", LstarMod, &(lambda)},		/* return fixnum modulus */
  {"*invmod", Lstarinvmod, &(lambda)},	/* return fixnum modulus ^-1 */
  {"fseek", Lfseek, &(lambda)},	/* seek to a specific byte in a file */
  {"fileopen",  Lfileopen, &( lambda)},
  {"pv%", Lpolyev, &(lambda)},	/* polynomial evaluation instruction*/
  {"cprintf", Lcprintf, &(lambda)},  /* formatted print 		    */
  {"sprintf", Lsprintf, &(lambda)},  /* formatted print to string	    */
  {"copyint*", Lcopyint, &(lambda)},	/* copyint*  */
  {"purcopy", Lpurcopy, &(lambda)},	/* pure copy */
  {"purep", Lpurep, &(lambda)},	/* check if pure */
  {"int:memreport", LImemory, &(lambda)}, /* dump memory stats */
/*
 * Hunk stuff
 */
  {"*makhunk", LMakhunk, &(lambda)},		/* special hunk creater */
  {"hunkp", Lhunkp, &(lambda)},		/* test a hunk */
  {"cxr", Lcxr, &(lambda)},			/* cxr of a hunk */
  {"rplacx", Lrplcx, &(lambda)},		/* replace element of a hunk */
  {"*rplacx", Lstarrpx, &(lambda)},		/* rplacx used by hunk */
  {"hunksize", Lhunksize, &(lambda)},	/* size of a hunk */
  {"hunk-to-list", Lhtol, &(lambda)},	/* hunk to list */
  {"new-vector", Lnvec, &(lambda)},
  {"new-vectori-byte", Lnvecb, &(lambda)},
  {"new-vectori-word", Lnvecw, &(lambda)},
  {"new-vectori-long", Lnvecl, &(lambda)},
  {"vectorp", Lvectorp, &(lambda)},
  {"vectorip", Lpvp, &(lambda)},
  {"int:vref", LIvref, &(lambda)},
  {"int:vset", LIvset, &(lambda)},
  {"int:vsize", LIvsize, &(lambda)},
  {"vsetprop", Lvsp, &(lambda)},
  {"vprop", Lvprop, &(lambda)},
  {"probef", Lprobef, &(lambda)},	/* test file existance */
  {"substring", Lsubstring, &(lambda)},
  {"substringn", Lsstrn, &(lambda)},
  {"character-index", Lcharindex, &(lambda)}, /* index of char in string */
  {"time-string", Ltymestr, &(lambda)},
  {"gc", Ngc, &(nlambda)},
  {"gcafter", Ngcafter, &(nlambda)},	/* garbage collection wind-up */
  {0}
};
static dofuns(){mftab(cfuns);}
