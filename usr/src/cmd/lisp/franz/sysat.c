static char *sccsid = "@(#)sysat.c	35.7 7/9/81";

#include "global.h"
#include "lfuncs.h"
#define MK(x,y,z) mfun(x,y,z)
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
#define PAGE_LIMIT 3800

extern Iaddstat();

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

	for (i=0; i < 8; i++)
	{
		hunk_pages[i] = (lispval) &tint[18+i*2];
		hunk_items[i] = (lispval) &tint[19+i*2];
	}

	/*  This also applies to the garbage collection threshhold  */

	gcthresh = (lispval) &tgcthresh;

	/*  Now we commence constructing system lisp structures.  */

	/*  nil is a special case, constructed especially at location zero  */

	hasht[hashfcn("nil")] = (struct atom *)nil;


	/* allocate space for namestack and bindstack first
	 * then set up beginsweep variable so that the sweeper will
	 * ignore these `always in use' pages
	 */

	lbot = orgnp = np = ((struct argent *)csegment(val_name,NAMESIZE,FALSE));
	orgbnp = bnp = ((struct nament *)csegment(dtpr_name,NAMESIZE,FALSE));
	beginsweep = (int *) xsbrk(0);

/*
 * Names of various spaces and things
 */

	atom_name = matom("symbol");
	str_name = matom("string");
	int_name = matom("fixnum");
	dtpr_name = matom("list");
	doub_name = matom("flonum");
	sdot_name = matom("bignum");
	array_name = matom("array");
	val_name = matom("value");
	funct_name = matom("binary");
	port_name = matom("port");		/* not really a space */

	{
	    char name[6];

	    strcpy(name, "hunk0");
	    for (i=0; i< 8; i++) {
		hunk_name[i] = matom(name);
		name[4]++;
	    }
	}
	/*  set up the name stack as an array of pointers */
	nplim = orgnp+NAMESIZE-6*NAMINC;
	temp = matom("namestack");
	nstack = temp->a.fnbnd = newarray();
	nstack->ar.data = (char *) (np);
	(nstack->ar.length = newint())->i = NAMESIZE;
	(nstack->ar.delta = newint())->i = sizeof(struct argent);
	Vnogbar = matom("unmarked_array");
	/* marking of the namestack will be done explicitly in gc1 */
	(nstack->ar.aux = newdot())->d.car = Vnogbar; 
						

	/* set up the binding stack as an array of dotted pairs */

	bnplim = orgbnp+NAMESIZE-5;
	temp = matom("bindstack");
	bstack = temp->a.fnbnd = newarray();
	bstack->ar.data = (char *) (bnp);
	(bstack->ar.length = newint())->i = NAMESIZE;
	(bstack->ar.delta = newint())->i = sizeof(struct nament);
	/* marking of the bindstack will be done explicitly in gc1 */
	(bstack->ar.aux = newdot())->d.car = Vnogbar; 

	/* more atoms */

	tatom = matom("t");
	tatom->a.clb = tatom;
	lambda = matom("lambda");
	nlambda = matom("nlambda");
	macro = matom("macro");
	ibase = matom("ibase");		/* base for input conversion */
	ibase->a.clb = inewint(10);
	rsetatom = matom("*rset");
	rsetatom->a.clb = nil;
	Vsubrou = matom("subroutine");
	Vpiport = matom("piport");
	Vpiport->a.clb = P(piport = stdin);	/* standard input */
	Vpoport = matom("poport");
	Vpoport->a.clb = P(poport = stdout);	/* stand. output */
	matom("errport")->a.clb = (P(errport = stderr));/* stand. err. */
	ioname[PN(stdin)]  = (lispval) inewstr("$stdin");
	ioname[PN(stdout)] = (lispval) inewstr("$stdout");
	ioname[PN(stderr)] = (lispval) inewstr("$stderr");
	(Vreadtable = matom("readtable"))->a.clb  = Imkrtab(0);
	strtab = Imkrtab(0);
	Vptport = matom("ptport");
	Vptport->a.clb = nil;				/* protocal port */

	Vcntlw = matom("^w");	/* when non nil, inhibits output to term */
	Vcntlw->a.clb = nil;

	Vldprt = matom("$ldprint");	
			/* when nil, inhibits printing of fasl/autoload   */
						/* cfasl messages to term */
	Vldprt->a.clb = tatom;

	Vprinlevel = matom("prinlevel");	/* printer recursion count */
	Vprinlevel->a.clb = nil;		/* infinite recursion */

	Vprinlength = matom("prinlength");	/* printer element count */
	Vprinlength->a.clb = nil;		/* infinite elements */

	Vfloatformat = matom("float-format");
	Vfloatformat->a.clb = (lispval) inewstr("%.16G");

	Verdepth = matom("Error-Depth");
	Verdepth->a.clb = inewint(0);		/* depth of error */

	/*  The following atoms are used as tokens by the reader  */

	perda = matom(".");
	lpara = matom("(");
	rpara = matom(")");
	lbkta = matom("[");
	rbkta = matom("]");
	snqta = matom("'");
	exclpa = matom("!");


	(Eofa = matom("eof"))->a.clb = eofa;
	cara = MK("car",Lcar,lambda);
	cdra = MK("cdr",Lcdr,lambda);

	/*  The following few atoms have values the reader tokens.  */
	/*  Perhaps this is a kludge which should be abandoned.  */
	/*  On the other hand, perhaps it is an inspiration.	*/

	matom("perd")->a.clb = perda;
	matom("lpar")->a.clb = lpara;
	matom("rpar")->a.clb = rpara;
	matom("lbkt")->a.clb = lbkta;
	matom("rbkt")->a.clb = rbkta;

	noptop = matom("noptop");

	/*  atoms used in connection with comments.  */

	commta = matom("comment");
	rcomms = matom("readcomments");

	/*  the following atoms are used for lexprs */

	lexpr_atom = matom("last lexpr binding\7");
	lexpr = matom("lexpr");

	/* the following atom is used to reference the bind stack for eval */
	bptr_atom = matom("eval1 binding pointer\7");
	bptr_atom->a.clb = nil;

	/* the following atoms are used for evalhook hackery */
	evalhatom = matom("evalhook");
	evalhatom->a.clb = nil;
	evalhcallsw = FALSE;

	funhatom = matom("funcallhook");
	funhatom->a.clb = nil;
	funhcallsw = FALSE;

	Vevalframe = matom("evalframe");

	sysa = matom("sys");
	plima = matom("pagelimit");	/*  max number of pages  */
	Veval = MK("eval",Leval1,lambda);
	MK("asin",Lasin,lambda);
	MK("acos",Lacos,lambda);
	MK("atan",Latan,lambda);
	MK("cos",Lcos,lambda);
	MK("sin",Lsin,lambda);
	MK("sqrt",Lsqrt,lambda);
	MK("exp",Lexp,lambda);
	MK("log",Llog,lambda);
	MK("lsh",Llsh,lambda);
	MK("bignum-leftshift",Lbiglsh,lambda);
	MK("sticky-bignum-leftshift",Lsbiglsh,lambda);
	MK("frexp",Lfrexp,lambda);
	MK("rot",Lrot,lambda);
	MK("random",Lrandom,lambda);
	MK("atom",Latom,lambda);
	MK("apply",Lapply,lambda);
	MK("funcall",Lfuncal,lambda);
	MK("return",Lreturn,lambda);
/* 	MK("cont",Lreturn,lambda);  */
	MK("cons",Lcons,lambda);
	MK("scons",Lscons,lambda);
	MK("bignum-to-list",Lbigtol,lambda);
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
	MK("zqual",Zequal,lambda);
	MK("numberp",Lnumberp,lambda);
	MK("dtpr",Ldtpr,lambda);
	MK("bcdp",Lbcdp,lambda);
	MK("portp",Lportp,lambda);
	MK("arrayp",Larrayp,lambda);
	MK("valuep",Lvaluep,lambda);
	MK("get_pname",Lpname,lambda);
	MK("ptr",Lptr,lambda);
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
	MK("getdata",Lgetdata,lambda);
	MK("putdata",Lputdata,lambda);
	MK("mfunction",Lmfunction,lambda);
	MK("getentry",Lgetentry,lambda);
	MK("getdisc",Lgetdisc,lambda);
	MK("putdisc",Lputdisc,lambda);
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
	MK("monitor",Lmonitor,lambda);
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
	reseta = MK("reset",Nreset,nlambda);
	MK("break",Nbreak,nlambda);
	MK("exit",Lexit,lambda);
	MK("def",Ndef,nlambda);
	MK("null",Lnull,lambda);
	/* debugging, remove when done */
	{ lispval Lframedump(); 
	  MK("framedump",Lframedump,lambda);
	}
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
	MK("odumplisp",Ndumplisp,nlambda);	/*  OLD save the world  */
	MK("dumplisp",Nndumplisp,nlambda);	/*  NEW save the world  */
#ifdef VMS
	MK("savelisp",Lsavelsp,lambda);	/*  save lisp data	*/
	MK("restorelisp",Lrestlsp,lambda);
#endif
	MK("top-level",Ntpl,nlambda);	/*  top level eval-print read loop  */
	startup = matom("startup");	/*  used by save and restore  */
	MK("mapcar",Lmapcar,lambda);
	MK("maplist",Lmaplist,lambda);
	MK("mapcan",Lmapcan,lambda);
	MK("mapcon",Lmapcon,lambda);
	MK("assq",Lassq,lambda);
	MK("mapc",Lmapc,lambda);
	MK("map",Lmap,lambda);
	MK("flatc",Lflatsi,lambda);
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
	MK("+",Lfp,lambda);
	MK("-",Lfm,lambda);
	MK("*",Lft,lambda);
	MK("/",Lfd,lambda);
	MK("mod",Lmod,lambda);
	MK("minus",Lminus,lambda);
	MK("absval",Labsval,lambda);
	MK("add1",Ladd1,lambda);
	MK("sub1",Lsub1,lambda);
	MK("greaterp",Lgreaterp,lambda);
	MK("lessp",Llessp,lambda);
	MK("any-zerop",Lzerop,lambda);   /* used when bignum arg possible */
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
	MK("baktrace",Lbaktrace,lambda);
	MK("tyi",Ltyi,lambda);
	MK("tyipeek",Ltyipeek,lambda);
	MK("tyo",Ltyo,lambda);
	MK("setsyntax",Lsetsyn,lambda);
	MK("makereadtable",Lmakertbl,lambda);
	MK("zapline",Lzapline,lambda);
	MK("aexplode",Lexplda,lambda);
	MK("aexplodec",Lexpldc,lambda);
	MK("aexploden",Lexpldn,lambda);
	MK("hashtabstat",Lhashst,lambda);
#ifdef METER
	MK("gcstat",Lgcstat,lambda);
#endif
	MK("argv",Largv,lambda);
	MK("arg",Larg,lambda);
	MK("setarg",Lsetarg,lambda);
	MK("showstack",Lshostk,lambda);
	MK("freturn",Lfretn,lambda);
	MK("*rset",Lrset,lambda);
	MK("eval1",Leval1,lambda);
	MK("evalframe",Levalf,lambda);
	MK("evalhook",Levalhook,lambda);
	MK("funcallhook",Lfunhook,lambda);
	MK("resetio",Nresetio,nlambda);
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
	MK("syscall",Lsyscall,lambda);
	MK("intern",Lintern,lambda);
	MK("ptime",Lptime,lambda);	/* return process user time */
	MK("fork",Lfork,lambda);	/* turn on fork and wait */
	MK("wait",Lwait,lambda);
/*
	MK("pipe",Lpipe,lambda);
	MK("fdopen",Lfdopen,lambda);
*/
	MK("exece",Lexece,lambda);
	MK("gensym",Lgensym,lambda);
	MK("remprop",Lremprop,lambda);
	MK("bcdad",Lbcdad,lambda);
	MK("symbolp",Lsymbolp,lambda);
	MK("stringp",Lstringp,lambda);
	MK("rematom",Lrematom,lambda);
	MK("prname",Lprname,lambda);
	MK("getenv",Lgetenv,lambda);
	MK("I-throw-err",Lctcherr,lambda); /* directly force a throw or error */
	MK("makunbound",Lmakunb,lambda);
	MK("haipart",Lhaipar,lambda);
	MK("haulong",Lhau,lambda);
	MK("signal",Lsignal,lambda);
	MK("fasl",Lnfasl,lambda);	/* NEW - new fasl loader */
	MK("cfasl",Lcfasl,lambda);	/* read in compiled C file */
	MK("getaddress",Lgetaddress,lambda);
					/* bind symbols without doing cfasl */
	MK("removeaddress",Lrmadd,lambda); 	/* unbind symbols    */
	MK("boundp",Lboundp,lambda);	/* tells if an atom is bound */
	MK("fake",Lfake,lambda);	/* makes a fake lisp pointer */
	MK("od",Lod,lambda);		/* dumps info */
	MK("maknum",Lmaknum,lambda);	/* converts a pointer to an integer */
	MK("*mod",LstarMod,lambda);		/* return fixnum modulus */

	MK("fseek",Lfseek,lambda);	/* seek to a specific byte in a file */
	MK("fileopen", Lfileopen, lambda);
					/* open a file for read/write/append */

	MK("pv%",Lpolyev,lambda);	/* polynomial evaluation instruction */
	MK("cprintf",Lcprintf,lambda);  /* formatted print 		     */
	MK("copyint*",Lcopyint,lambda);	/* copyint*  */
	MK("purcopy",Lpurcopy,lambda);	/* pure copy */
	MK("purep",Lpurep,lambda);	/* check if pure */

/*
 * Hunk stuff
 */

	MK("*makhunk",LMakhunk,lambda);		/* special hunk creater */
	MK("hunkp",Lhunkp,lambda);		/* test a hunk */
	MK("cxr",Lcxr,lambda);			/* cxr of a hunk */
	MK("rplacx",Lrplacx,lambda);		/* replace element of a hunk */
	MK("*rplacx",Lstarrpx,lambda);		/* rplacx used by hunk */
	MK("hunksize",Lhunksize,lambda);	/* size of a hunk */

	MK("probef",Lprobef,lambda);	/* test file existance */
	MK("substring",Lsubstring,lambda);
	MK("substringn",Lsubstringn,lambda);
	odform = matom("odformat");	/* format for printf's used in od */
	rdrsdot = newsdot();		/* used in io conversions of bignums */
	rdrsdot2 = newsdot();		/* used in io conversions of bignums */
	rdrint = newint();		/* used as a temporary integer */
	(nilplist = newdot())->d.cdr = newdot();
					/* used as property list for nil,
					   since nil will eventually be put at
					   0 (consequently in text and not
					   writable) */

	/* error variables */
	(Vererr = matom("ER%err"))->a.clb = nil;
	(Vertpl = matom("ER%tpl"))->a.clb = nil;
	(Verall = matom("ER%all"))->a.clb = nil;
	(Vermisc = matom("ER%misc"))->a.clb = nil;
	(Verbrk = matom("ER%brk"))->a.clb = nil;
	(Verundef = matom("ER%undef"))->a.clb = nil;
	(Vlerall = newdot())->d.car = Verall;	/* list (ER%all) */
	(Veruwpt = matom("ER%unwind-protect"))->a.clb = nil;
	(Verrset = matom("errset"))->a.clb = nil;


	/* set up the initial status list */

	stlist = nil;			/* initially nil */
	Iaddstat(matom("features"),ST_READ,ST_NO,nil);
	Iaddstat(matom("feature"),ST_FEATR,ST_FEATW,nil);
	Isstatus(matom("feature"),matom("franz"));
	Isstatus(matom("feature"),matom(OS));
	Isstatus(matom("feature"),matom("string"));
	Isstatus(matom("feature"),matom(MACHINE));
	Isstatus(matom("feature"),matom(SITE));

	Iaddstat(matom("nofeature"),ST_NFETR,ST_NFETW,nil);
	Iaddstat(matom("syntax"),ST_SYNT,ST_NO,nil);
	Iaddstat(matom("uctolc"),ST_READ,ST_TOLC,nil);
	Iaddstat(matom("dumpcore"),ST_READ,ST_CORE,nil);
	Isstatus(matom("dumpcore"),nil);	/*set up signals*/

	Iaddstat(matom("chainatom"),ST_RINTB,ST_INTB,inewint(0));
	Iaddstat(matom("dumpmode"),ST_DMPR,ST_DMPW,nil);
	Iaddstat(matom("appendmap"),ST_READ,ST_SET,nil);  /* used by fasl */
	Iaddstat(matom("debugging"),ST_READ,ST_SET,nil);  
	Iaddstat(matom("evalhook"),ST_RINTB,ST_INTB,inewint(3));
	Isstatus(matom("evalhook"),nil); /*evalhook switch off */
	Iaddstat(matom("bcdtrace"),ST_READ,ST_BCDTR,nil);
	Iaddstat(matom("ctime"),ST_CTIM,ST_NO,nil);
	Iaddstat(matom("localtime"),ST_LOCT,ST_NO,nil);
	Iaddstat(matom("isatty"),ST_ISTTY,ST_NO,nil);
	Iaddstat(matom("ignoreeof"),ST_READ,ST_SET,nil);
	Iaddstat(matom("version"),ST_READ,ST_NO,mstr("Franz Lisp, Opus 36"));
	Iaddstat(matom("automatic-reset"),ST_READ,ST_AUTR,nil);
	Iaddstat(matom("translink"),ST_READ,ST_TRAN,nil);
	Isstatus(matom("translink"),tatom);		/* turn on tran links */
	Iaddstat(matom("undeffunc"),ST_UNDEF,ST_NO,nil); /* list undef funcs */

	/* garbage collector things */

	MK("gc",Ngc,nlambda);
	gcafter = MK("gcafter",Ngcafter,nlambda);	/* garbage collection wind-up */
	gcport = matom("gcport");	/* port for gc dumping */
	gccheck = matom("gccheck");	/* flag for checking during gc */
	gcdis = matom("gcdisable");	/* variable for disabling the gc */
	gcdis->a.clb = nil;
	gcload = matom("gcload");	/* option for gc while loading */
	loading = matom("loading");	/* flag--in loader if = t  */
	noautot = matom("noautotrace");	/* option to inhibit auto-trace */
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
	strbuf[0] = 0;
	strcatn(strbuf,string,STRBLEN);
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
	v->a.fnbnd = newfunct();
	v->a.fnbnd->bcd.entry = entry;
	v->a.fnbnd->bcd.discipline = discip;
	return(v);
	}
