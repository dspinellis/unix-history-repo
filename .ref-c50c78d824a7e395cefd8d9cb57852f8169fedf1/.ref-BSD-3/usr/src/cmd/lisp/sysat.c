#include "global.h"
#include "lfuncs.h"
#define MK(x,y,z) mfun(x,y,z)
#define FIDDLE(a,b,c,d) a->clb=newdot(); (a->clb->car=newint())->i=b->i; \
	a->clb->cdr=newdot(); (a->clb->cdr->car=newint())->i=c->i; \
	a->clb->cdr->cdr=newdot(); (a->clb->cdr->cdr->car=newint())->i=d; \
	b = a->clb->car; c = a->clb->cdr->car; \
	copval(a,a->clb); a->clb = nil;

#define cforget(x) protect(x); Lforget(); unprot();

/*  The following array serves as the temporary counters of the items	*/
/*  and pages used in each space.					*/

long int tint[18]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

long int tgcthresh = 15;
int initflag = TRUE;	/*  starts off TRUE to indicate unsafe to gc  */

#define PAGE_LIMIT 3800

extern Iaddstat();

makevals()
	{
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

	/*  This also applies to the garbage collection threshhold  */

	gcthresh = (lispval) &tgcthresh;

	/*  Now we commence constructing system lisp structures.  */

	/*  nil is a special case, constructed especially at location zero  */

	hasht['n'^'i'^'l'] = (struct atom *)nil;


	atom_name = matom("symbol");
	str_name = matom("string");
	int_name = matom("fixnum");
	dtpr_name = matom("list");
	doub_name = matom("flonum");
	sdot_name = matom("bignum");
	array_name = matom("array");
	val_name = matom("value");
	funct_name = matom("binary");


	/*  set up the name stack as an array of pointers */

	lbot = orgnp = np = ((struct argent *)csegment(val_name,NAMESIZE));
	nplim = orgnp+NAMESIZE-5;
	temp = matom("namestack");
	nstack = temp->fnbnd = newarray();
	nstack->data = (char *) (np);
	(nstack->length = newint())->i = NAMESIZE;
	(nstack->delta = newint())->i = sizeof(struct argent);

	/* set up the binding stack as an array of dotted pairs */

	orgbnp = bnp = ((struct nament *)csegment(dtpr_name,NAMESIZE));
	bnplim = orgbnp+NAMESIZE-5;
	temp = matom("bindstack");
	bstack = temp->fnbnd = newarray();
	bstack->data = (char *) (bnp);
	(bstack->length = newint())->i = NAMESIZE;
	(nstack->delta = newint())->i = sizeof(struct nament);

	/* more atoms */

	tatom = matom("t");
	tatom->clb = tatom;
	lambda = matom("lambda");
	nlambda = matom("nlambda");
	macro = matom("macro");
	ibase = matom("ibase");		/* base for input conversion */
	ibase->clb = inewint(10);
	Vpiport = matom("piport");
	Vpiport->clb = P(piport = stdin);	/* standard input */
	Vpoport = matom("poport");
	Vpoport->clb = P(poport = stdout);	/* stand. output */
	matom("errport")->clb = (P(errport = stderr));/* stand. err. */
	(Vreadtable = matom("readtable"))->clb  = Imkrtab(0);
	strtab = Imkrtab(0);

	/*  The following atoms are used as tokens by the reader  */

	perda = matom(".");
	lpara = matom("(");
	rpara = matom(")");
	lbkta = matom("[");
	rbkta = matom("]");
	snqta = matom("'");
	exclpa = matom("!");


	(Eofa = matom("eof"))->clb = eofa;
	cara = MK("car",Lcar,lambda);
	cdra = MK("cdr",Lcdr,lambda);

	/*  The following few atoms have values the reader tokens.  */
	/*  Perhaps this is a kludge which should be abandoned.  */
	/*  On the other hand, perhaps it is an inspiration.	*/

	matom("perd")->clb = perda;
	matom("lpar")->clb = lpara;
	matom("rpar")->clb = rpara;
	matom("lbkt")->clb = lbkta;
	matom("rbkt")->clb = rbkta;

	noptop = matom("noptop");

	/*  atoms used in connection with comments.  */

	commta = matom("comment");
	rcomms = matom("readcomments");

	/*  the following atoms are used for lexprs */

	lexpr_atom = matom("last lexpr binding\7");
	lexpr = matom("lexpr");

	sysa = matom("sys");
	plima = matom("pagelimit");	/*  max number of pages  */
	Veval = MK("eval",Leval,lambda);
	MK("asin",Lasin,lambda);
	MK("acos",Lacos,lambda);
	MK("atan",Latan,lambda);
	MK("cos",Lcos,lambda);
	MK("sin",Lsin,lambda);
	MK("sqrt",Lsqrt,lambda);
	MK("exp",Lexp,lambda);
	MK("log",Llog,lambda);
	MK("random",Lrandom,lambda);
	MK("atom",Latom,lambda);
	MK("apply",Lapply,lambda);
	MK("funcall",Lfuncal,lambda);
	MK("return",Lreturn,lambda);
	MK("retbrk",Lretbrk,lambda);
	MK("cont",Lreturn,lambda);
	MK("cons",Lcons,lambda);
	MK("scons",Lscons,lambda);
	MK("cadr",Lcadr,lambda);
	MK("caar",Lcaar,lambda);
	MK("cddr",Lc02r,lambda);
	MK("caddr",Lc12r,lambda);
	MK("cdddr",Lc03r,lambda);
	MK("cadddr",Lc13r,lambda);
	MK("cddddr",Lc04r,lambda);
	MK("caddddr",Lc14r,lambda);
	MK("nthelem",Lnthelem,lambda);
	MK("eq",Leq,lambda);
	MK("equal",Lequal,lambda);
	MK("numberp",Lnumberp,lambda);
	MK("dtpr",Ldtpr,lambda);
	MK("bcdp",Lbcdp,lambda);
	MK("portp",Lportp,lambda);
	MK("arrayp",Larrayp,lambda);
	MK("valuep",Lvaluep,lambda);
	MK("get_pname",Lpname,lambda);
	MK("arrayref",Larrayref,lambda);
	MK("marray",Lmarray,lambda);
	MK("getlength",Lgetl,lambda);
	MK("putlength",Lputl,lambda);
	MK("getaccess",Lgeta,lambda);
	MK("putaccess",Lputa,lambda);
	MK("getdelta",Lgetdel,lambda);
	MK("putdelta",Lputdel,lambda);
	MK("getaux",Lgetaux,lambda);
	MK("putaux",Lputaux,lambda);
	MK("mfunction",Lmfunction,lambda);
	MK("getentry",Lgetentry,lambda);
	MK("getdisc",Lgetdisc,lambda);
	MK("segment",Lsegment,lambda);
	MK("rplaca",Lrplaca,lambda);
	MK("rplacd",Lrplacd,lambda);
	MK("set",Lset,lambda);
	MK("replace",Lreplace,lambda);
	MK("infile",Linfile,lambda);
	MK("outfile",Loutfile,lambda);
	MK("terpr",Lterpr,lambda);
	MK("print",Lprint,lambda);
	MK("close",Lclose,lambda);
	MK("patom",Lpatom,lambda);
	MK("pntlen",Lpntlen,lambda);
	MK("read",Lread,lambda);
	MK("ratom",Lratom,lambda);
	MK("readc",Lreadc,lambda);
	MK("implode",Limplode,lambda);
	MK("maknam",Lmaknam,lambda);
	MK("concat",Lconcat,lambda);
	MK("uconcat",Luconcat,lambda);
	MK("putprop",Lputprop,lambda);
	MK("get",Lget,lambda);
	MK("getd",Lgetd,lambda);
	MK("putd",Lputd,lambda);
	MK("prog",Nprog,nlambda);
	quota = MK("quote",Nquote,nlambda);
	MK("function",Nfunction,nlambda);
	MK("go",Ngo,nlambda);
	MK("*catch",Ncatch,nlambda);
	MK("errset",Nerrset,nlambda);
	MK("status",Nstatus,nlambda);
	MK("sstatus",Nsstatus,nlambda);
	MK("err",Lerr,lambda);
	MK("*throw",Nthrow,lambda);	/* this is a lambda now !! */
	MK("reset",Nreset,nlambda);
	MK("break",Nbreak,nlambda);
	MK("exit",Lexit,lambda);
	MK("def",Ndef,nlambda);
	MK("null",Lnull,lambda);
	MK("and",Nand,nlambda);
	MK("or",Nor,nlambda);
	MK("setq",Nsetq,nlambda);
	MK("cond",Ncond,nlambda);
	MK("list",Llist,lambda);
	MK("load",Lload,lambda);
	MK("nwritn",Lnwritn,lambda);
	MK("process",Nprocess,nlambda);	/*  execute a shell command  */
	MK("allocate",Lalloc,lambda);	/*  allocate a page  */
	MK("sizeof",Lsizeof,lambda);	/*  size of one item of a data type  */
	MK("dumplisp",Ndumpli,nlambda);	/*  save the world  */
	MK("top-level",Ntpl,nlambda);	/*  top level eval-print read loop  */
	startup = matom("startup");	/*  used by save and restore  */
	MK("mapcar",Lmapcar,lambda);
	MK("maplist",Lmaplist,lambda);
	MK("mapcan",Lmapcan,lambda);
	MK("mapcon",Lmapcon,lambda);
	MK("assq",Lassq,lambda);
	MK("mapc",Lmapc,lambda);
	MK("map",Lmap,lambda);
	MK("flatsize",Lflatsi,lambda);
	MK("alphalessp",Lalfalp,lambda);
	MK("drain",Ldrain,lambda);
	MK("killcopy",Lkilcopy,lambda); /*  forks aand aborts for adb */
	MK("opval",Lopval,lambda);	/*  sets and retrieves system variables  */
	MK("ncons",Lncons,lambda);
	sysa = matom("sys");	/*  sys indicator for system variables  */
	MK("remob",Lforget,lambda);	/*  function to take atom out of hash table  */
	splice = matom("splicing");
	MK("not",Lnull,lambda);
	MK("plus",Ladd,lambda);
	MK("add",Ladd,lambda);
	MK("times",Ltimes,lambda);
	MK("difference",Lsub,lambda);
	MK("quotient",Lquo,lambda);
	MK("mod",Lmod,lambda);
	MK("minus",Lminus,lambda);
	MK("absval",Labsval,lambda);
	MK("add1",Ladd1,lambda);
	MK("sub1",Lsub1,lambda);
	MK("greaterp",Lgreaterp,lambda);
	MK("lessp",Llessp,lambda);
	MK("zerop",Lzerop,lambda);
	MK("minusp",Lnegp,lambda);
	MK("onep",Lonep,lambda);
	MK("sum",Ladd,lambda);
	MK("product",Ltimes,lambda);
	MK("do",Ndo,nlambda);
	MK("progv",Nprogv,nlambda);
	MK("progn",Nprogn,nlambda);
	MK("prog2",Nprog2,nlambda);
	MK("oblist",Loblist,lambda);
	MK("baktrace",Lbaktra,lambda);
	MK("tyi",Ltyi,lambda);
	MK("tyipeek",Ltyipeek,lambda);
	MK("tyo",Ltyo,lambda);
	MK("setsyntax",Lsetsyn,lambda);
	MK("makereadtable",Lmakertbl,lambda);
	MK("zapline",Lzaplin,lambda);
	MK("aexplode",Lexplda,lambda);
	MK("aexplodec",Lexpldc,lambda);
	MK("aexploden",Lexpldn,lambda);
	MK("argv",Largv,lambda);
	MK("arg",Larg,lambda);
	MK("showstack",Lshostk,lambda);
	MK("resetio",Nreseti,nlambda);
	MK("chdir",Lchdir,lambda);
	MK("ascii",Lascii,lambda);
	MK("boole",Lboole,lambda);
	MK("type",Ltype,lambda);	/* returns type-name of argument */
	MK("fix",Lfix,lambda);
	MK("float",Lfloat,lambda);
	MK("fact",Lfact,lambda);
	MK("cpy1",Lcpy1,lambda);
	MK("Divide",LDivide,lambda);
	MK("Emuldiv",LEmuldiv,lambda);
	MK("readlist",Lreadli,lambda);
	MK("plist",Lplist,lambda);	/* gives the plist of an atom */
	MK("setplist",Lsetpli,lambda);	/* get plist of an atom  */
	MK("eval-when",Nevwhen,nlambda);
	MK("syscall",Nsyscall,nlambda);
	MK("ptime",Lptime,lambda);	/* return process user time */
/*
	MK("fork",Lfork,lambda);
	MK("wait",Lwait,lambda);
	MK("pipe",Lpipe,lambda);
	MK("fdopen",Lfdopen,lambda);
	MK("exece",Lexece,lambda);
 */
	MK("gensym",Lgensym,lambda);
	MK("remprop",Lremprop,lambda);
	MK("bcdad",Lbcdad,lambda);
	MK("symbolp",Lsymbolp,lambda);
	MK("stringp",Lstringp,lambda);
	MK("rematom",Lrematom,lambda);
	MK("prname",Lprname,lambda);
	MK("getenv",Lgetenv,lambda);
	MK("makunbound",Lmakunb,lambda);
	MK("haipart",Lhaipar,lambda);
	MK("haulong",Lhau,lambda);
	MK("signal",Lsignal,lambda);
	MK("fasl",Lfasl,lambda);	/* read in compiled file */
	MK("bind",Lbind,lambda);	/* like fasl but for functions
					   loaded in when the lisp system
					   was constructed by ld */
	MK("boundp",Lboundp,lambda);	/* tells if an atom is bound */
	MK("fake",Lfake,lambda);	/* makes a fake lisp pointer */
	MK("od",Lod,lambda);		/* dumps info */
	MK("what",Lwhat,lambda);	/* converts a pointer to an integer */
	MK("pv%",Lpolyev,lambda);	/* polynomial evaluation instruction */
	odform = matom("odformat");	/* format for printf's used in od */
	rdrsdot = newsdot();		/* used in io conversions of bignums */
	rdrint = newint();		/* used as a temporary integer */
	(nilplist = newdot())->cdr = newdot();
					/* used as property list for nil,
					   since nil will eventually be put at
					   0 (consequently in text and not
					   writable) */

	/* error variables */
	(Vererr = matom("ER%err"))->clb = nil;
	(Vertpl = matom("ER%tpl"))->clb = nil;
	(Verall = matom("ER%all"))->clb = nil;
	(Vermisc = matom("ER%misc"))->clb = nil;
	(Vlerall = newdot())->car = Verall;	/* list (ER%all) */


	/* set up the initial status list */

	stlist = nil;			/* initially nil */
	Iaddstat(matom("features"),ST_READ,ST_NO,nil);
	Iaddstat(matom("feature"),ST_FEATR,ST_FEATW,nil);
	Isstatus(matom("feature"),matom("franz"));

	Iaddstat(matom("nofeature"),ST_NFETR,ST_NFETW,nil);
	Iaddstat(matom("syntax"),ST_SYNT,ST_NO,nil);
	Iaddstat(matom("uctolc"),ST_READ,ST_TOLC,nil);
	Iaddstat(matom("dumpcore"),ST_READ,ST_CORE,nil);
	Isstatus(matom("dumpcore"),nil);	/*set up signals*/

	Iaddstat(matom("chainatom"),ST_RINTB,ST_INTB,inewint(0));
	Iaddstat(matom("dumpmode"),ST_DMPR,ST_DMPW,nil);
	/* garbage collector things */

	MK("gc",Ngc,nlambda);
	gcafter = MK("gcafter",Ngcafter,nlambda);	/* garbage collection wind-up */
	gcport = matom("gcport");	/* port for gc dumping */
	gccheck = matom("gccheck");	/* flag for checking during gc */
	gcdis = matom("gcdisable");	/* option for disabling the gc */
	gcload = matom("gcload");	/* option for gc while loading */
	loading = matom("loading");	/* flag--in loader if = t  */
	noautot = matom("noautotrace");	/* option to inhibit auto-trace */
	(gcthresh = newint())->i = tgcthresh;
	gccall1 = newdot();  gccall2 = newdot();  /* used to call gcafter */
	gccall1->car = gcafter;  /* start constructing a form for eval */

	arrayst = mstr("ARRAY");	/* array marker in name stack */
	bcdst = mstr("BINARY");		/* binary function marker */
	listst = mstr("INTERPRETED");	/* interpreted function marker */
	macrost = mstr("MACRO");	/* macro marker */
	protst = mstr("PROTECTED");	/* protection marker */
	badst = mstr("BADPTR");		/* bad pointer marker */
	argst = mstr("ARGST");		/* argument marker */

	/* type names */

	FIDDLE(atom_name,atom_items,atom_pages,ATOMSPP);
	FIDDLE(str_name,str_items,str_pages,STRSPP);
	FIDDLE(int_name,int_items,int_pages,INTSPP);
	FIDDLE(dtpr_name,dtpr_items,dtpr_pages,DTPRSPP);
	FIDDLE(doub_name,doub_items,doub_pages,DOUBSPP);
	FIDDLE(sdot_name,sdot_items,sdot_pages,SDOTSPP);
	FIDDLE(array_name,array_items,array_pages,ARRAYSPP);
	FIDDLE(val_name,val_items,val_pages,VALSPP);
	FIDDLE(funct_name,funct_items,funct_pages,BCDSPP);

	(plimit = newint())->i = PAGE_LIMIT;
	copval(plima,plimit);  /*  default value  */

	/* the following atom is used when reading caar, cdar, etc. */

	xatom = matom("??");

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
	strcpy(strbuf,string);
	return(getatom());
	}

/*  mstr  ***************************************************************/
/*									*/
/*  Makes a string.  Uses matom.					*/
/*  Not the most efficient but will do until the string from the code	*/
/*  itself can be used as a lispval.					*/

lispval mstr(string) char *string;
	{
	return((lispval)(inewstr(string)));
	}

/*  mfun("name",entry)  *************************************************/
/*									*/
/*  Same as matom, but entry point to c code is associated with		*/
/*  "name" as function binding.						*/
/*  A pointer to the atom is returned.					*/
/*									*/
lispval mfun(string,entry,discip) char *string; lispval (*entry)(), discip;
	{
	lispval v;
	v = matom(string);
	v -> fnbnd = newfunct();
	v->fnbnd->entry = entry;
	v->fnbnd->discipline = discip;
	return(v);
	}
