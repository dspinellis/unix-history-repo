/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/*
 * AT&T Bell Laboratories
 *
 */

#include	"defs.h"
#include	"io.h"
#include	"flags.h"
#include	"name.h"
#include	"shtype.h"
#include	"sym.h"
#include	"stak.h"
#include	"brkincr.h"
#include	"builtins.h"
#include	"history.h"
#include	"timeout.h"

/* This module defines the following routines */
NAMPTR	checkfor();
void	do_whence();
#ifdef ECHO_N
char	*echo_mode();
#endif	/* ECHO_N */
#ifdef apollo
void	ev_$set_var();
#endif	/* apollo */
int	genenv();
char	*heap();
void	meminit();
void	mem_scope();
void	mem_unscope();
char	*movstr();
void	name_unscope();
char	*qvalup();
void	prinscan();
int	printnam();
void	printflg();
int	readvar();
void	rmlocal();
char	**setenv();
void	setlist();
NAMPTR	setname();
int	syslook();

/* This module references the following external routines */
extern void	assign();
extern char	*bracket_match();
extern unsigned	chkid();
#ifdef ECHO_N
extern MSG	echo_bin;
extern MSG	echo_opt;
#endif	/* ECHO_N */
extern char	**environ;
extern void	exitsh();
extern void	failed();
extern void	free();
extern NAMPTR	findnod();
extern char	*fullname();
extern char	*getstak();
extern struct Amemory *gettree();
extern void	gsort();
extern long	hist_list();
extern void	hist_flush();
extern void	initf();
extern void	gscan_all();
extern void	gscan_some();
extern NAMPTR	lookup();
extern void	linknod();
extern char	*mactrim();
extern char	*malloc();
extern char	*movstr();
extern unsigned	rand();
extern void	p_num();
extern void	p_str();
extern void	p_sub();
extern void	p_setout();
extern void	scan_all();
extern char	*simple();
extern int	srand();
extern char	*strchr();
extern char	*strcpy();
extern void	rmnval();
extern void	unassign();
extern long	time();
extern char	*utos();
extern char	*valup();

static void	countnam();
#ifdef ECHO_N
static char	*echo_arg;
#endif	/* ECHO_N */
static long	get_rand();
static long	get_second();
static struct Amemory *inittree();
static void	no_export();
static void	pr_name();
static void	pushnam();
static void	pushnid();
static void	rehash();
static void	rm_node();
static int	set_second();
static int	set_rand();
static char	*staknam();


static rsflag;	/* used to see if "SHELL" has been set in the environment */
static int	namec;
static char	**argnam;
static struct Amemory	*namebase;

struct Bfunction seconds = { get_second, set_second};
struct Bfunction randnum = { get_rand, set_rand};



/* ========	variable and string handling	======== */

/*
 *  Table lookup routine
 *  The table <syswds> is searched for string <w> and corresponding value is returned
 */
syslook(w,syswds)
register char *w;
SYSTAB		syswds;
{
	register int first;
	register SYSPTR	syscan;
	register int c;
	if(w==0 || (first= *w)==0)
		return(0);
	syscan=syswds;
	while((c= *syscan->sysnam) && c <= first)
	{
		if(first == c && eq(w,syscan->sysnam))
			return(syscan->sysval);
		syscan++;
	}
	return(0);
}

/*
 * perform parameter assignment on an argument list
 */

void setlist(arg,xp)
register ARGPTR	arg;
register int xp;
{
	if(is_option(ALLEXP))
		xp |= N_EXPORT;
	while(arg)
	{
		register char *s;
		if(arg->argflag&A_MAC)
			s=mactrim(arg->argval,0);
		else
			s = arg->argval;
		setname(s, xp);
		arg=arg->argnxt;
		if(is_option(EXECPR))
		{
			p_setout(stderr);
			p_str(s,arg?SP:NL);
		}
	}
}

/*
 * Put <arg> into associative memory.
 * If <xp> & V_FLAG then the alias list is used instead
 * If <xp> & S_FLAG then use the current scope only
 */


NAMPTR	setname(argi, xp)
char *argi;
int 	xp;
{
	register char *argscan=argi;
	register NAMPTR	n;
	register int sep = *argscan;
	char *sim;
	if(isalpha(sep) || ((xp&V_FLAG) && !expchar(sep)))
	{
		do
		{
			sep = *++argscan;
		}
		while(isalnum(sep));
		/* check for subscript*/
		if(sep=='[')
		{
			argscan = bracket_match(argscan)+1;
		}
		if((sep = *argscan) && sep!='=')
			failed(argi,notid);
		*argscan = 0;
		if(xp&V_FLAG)
		{
			n = findnod(argi,alias,1);
			if(attest(n,T_FLAG|NO_ALIAS))
				pattrib(n,~(NO_ALIAS|T_FLAG|N_EXPORT));
		}
		else if(xp&S_FLAG)
			/* scoped name must be in first tree */
			n = findnod(argi,namep,1);
		else
			n = lookup(argi);
		*argscan++ = sep;
		if(sep == '=')
		{
			if(n==PATHNOD || n==ENVNOD || n==SHELLNOD)
			{
				if(is_option(RSHFLG))
					failed(argi,restricted);
				if(n==SHELLNOD)
				{
					sim = simple(argscan);
					if(strchr(sim,'r') != NULL)
						rsflag = 0;	/* restricted shell */
				}
			}
			assign (n, argscan);
			attrib(n, xp&~(S_FLAG|V_FLAG));
#ifdef apollo
			if(attest(n,N_EXPORT) && attest(n,N_IMPORT)==0 
				&& (xp&(S_FLAG|V_FLAG))==0)
			{
				short namlen,vallen;
				namlen =strlen(n->namid);
				vallen = strlen(argscan);
				ev_$set_var(n->namid,&namlen,argscan,&vallen);
			}
#endif /* apollo */
			if(n==PATHNOD)
			{
				gscan_some(rehash,alias,T_FLAG,T_FLAG);
#ifdef ECHO_N
				echo_arg = NIL;
#endif	/* ECHO_N */
			}
			if(n==VISINOD || ((n==EDITNOD)&&isnull(VISINOD)))
			{
				/* turn on vi or emacs option if editor name is either*/
				argscan = simple(argscan);
				if(gmatch(argscan,"*vi"))
				{
					off_option(EDITVI|EMACS|GMACS);
					on_option(EDITVI);
				}
				if(gmatch(argscan,"*macs"))
				{
					off_option(EDITVI|EMACS|GMACS);
					if(*argscan=='g')
						on_option(GMACS);
					else
						on_option(EMACS);
				}
			}
		}
		return(n);
	}
	failed (argi, notid);
	/* NOTREACHED */
}

/*
 * Mark each node is invalid
 */

static void rehash(np)
register NAMPTR np;
{
	attrib(np,NO_ALIAS);
}

/*
 *  alias each name to full path name
 *  realias returns the pathname or NULL if not found
 */

char *realias(np)
register NAMPTR np;
{
	register char *sp;
	register char *vp = np->value.namval.cp;
	int flag = namflag(np)&(N_EXPORT|NO_ALIAS|T_FLAG);
	/* turn of T_FLAG to avoid recursion */
	pattrib(np,~(NO_ALIAS|T_FLAG));
	sp = fullname(np->namid);
	if(sp==NIL)
	{
		unassign(np);
		return(NIL);
	}
	else if(*sp!= '/')
	{
		sattrib(np,flag);
		return(NIL);
	}
	if(vp==0 || strcmp(sp,vp)!=0)
		assign(np,sp);
	/* turn T_FLAG back on */
	attrib(np,T_FLAG|N_EXPORT);
	return(sp);
}


int readvar(names,fd,raw)
register char **names;
FILE *fd;
{
	FILEBLK	fb;
	SHFILE f;
	register int c;
	int issep;
	register NAMPTR	n;
	int checksep = 1;		/* set when looking for separators */
	STKPTR	rel;
	char *seps;
	char is_eol;
	FILE *savef;
	states |= RWAIT;
	/* save in history file if S_FLAG is set */
	if((raw&S_FLAG) && fc_fix)
		states |= FIXFLG;
	raw &= R_FLAG;
	f = &fb;
	if(*names)
	{
		if(seps=strchr(*names,'?'))
			*seps = 0;
		n = lookup(*names++);
		if(seps)
			*seps = '?';
	}
	else
		n = REPLYNOD;
	rel=(STKPTR)relstak();
	seps = qvalup(IFSNOD);
	if(seps==NULL)
		seps = sptbnl;
	savef = input;
	if(fd==NULL)
		failed(bread,noquery);
	if(fd != input)
	{
		/* buffer the input stream if possible */
		if(fnobuff(fd) && (fd!=stdin || !ispipe(stdin)))
			setbuf(fd,malloc((unsigned)BUFSIZ));
		push(f);
		initf(fd);
	}
	while(1)
	{
		c = (raw?readc():nextc());
		issep = (strchr(seps,c)!=0);
		is_eol = eolchar(c);
		if(checksep && issep && !is_eol)
		{
			/* visable adjacent separators signify null fields*/
			if(strchr(sptbnl,c)!=0)
				continue;
		}
		checksep = 0;
		if((issep && *names) || is_eol)	/* from non-separator to separator */
		{
			if(*names==NULL && staktop>stakbot)
			{
				/* remove trailing separators */
				while(strchr(seps,*--staktop));
				staktop++;
			}
			zerostak();
			assign(n,absstak(rel)); setstak(rel);
			if(is_option(ALLEXP))
				attrib(n,N_EXPORT);
			n = (*names?lookup(*names++):0);
			if(is_eol)
				break;
			checksep = 1;
		}
		else 		/* not a separator */
			pushstak(c);
	}
	while(n)
	{
		assign(n, nullstr);
		if(is_option(ALLEXP))
			attrib(n,N_EXPORT);
		n = (*names?lookup(*names++):0);
	}
	if(savef != fd)
		pop(1);
	if(states&FIXFLG)
		hist_flush();
	states &= ~(RWAIT|PROMPT);
	states |= is_option(INTFLG);
	return;
}

/*
 * put string v onto the heap and return the heap pointer
 */

char *heap(v)
register char *v;
{
	register char *p;
	if(v)
	{
		movstr(v,p=malloc((unsigned)strlen(v)+1));
		return(p);
	}
	else
		return(0);
}


/*
 * print out the name and value of a name-value pair <n>
 */

int printnam(n,flag)
register NAMPTR		n;
register int	flag;
{
	register FILE *fd;
	register char *s;
	union Namval *up= &n->value.namval;
	if(trapnote&SIGSET)
		exitsh(SIGFAIL);
	fd = output;
	if(attest(n,NO_ALIAS)==NO_ALIAS)
	{
		return(0);
	}
	if(is_afunction(n))
	{
		fputs(bltfn,fd);
		fputs(n->namid,fd);
		if(flag==0 && n->value.namval.rp->hoffset >=0 )
		{
			fputs(fn_hdr,fd);
			hist_list(n->value.namval.rp->hoffset,EOF,"\n");
		}
		else
			putc('\n',fd);
		return(n->namsz+1);
	}
	if(s=valup(n))
	{
		char numbuf[30];
		pr_name(n,0);
		flag = (flag?NL:'=');
	        if (attest (n, ARRAY))
		{
			if(attest(n,INT_GER))
			{
				/* copy to a save place */
				strcpy(numbuf,s);
				s = numbuf;
			}
			p_sub((int)up->aray->adot,flag);
		}
		else
			putc(flag,fd);
		if(flag != NL)
			p_str(s,NL);
		return(1);
	}
	return(0);
}

/*
 * print the name of a node followed by the character c
 */

static void pr_name(n,c)
register NAMPTR		n;
int c;
{
	register char *cp = strchr(n->namid,'=');
	if(cp)
		*cp = 0;
	p_str(n->namid,c);
	if(cp)
		*cp = '=';
}

static void pushnid(np)
NAMPTR np;
{
	*argnam++ = np->namid;
	namec++;
	if(attest(np,ARRAY))
		arayp(np)->adot = arayp(np)->maxi;
}

/*
 * print the nodes in tree <root> which have attributes <flag> set
 */

void prinscan(file,flag,root,option)
FILE *file;
struct Amemory *root;
{
	register char **argv;
	register NAMPTR np;
	p_setout(file);
	argv = argnam  = (char**)locstak();
	namec = 0;
	if(flag)
		gscan_some(pushnid,root,flag,flag);
	else
		gscan_all(pushnid,root);
	gsort(argv,namec);
	while(namec--)
	{
		{
			register char *cp;
			if(cp = strchr(*argv,'='))
				*cp = 0;
			np = checkfor(*argv++,root);
			if(cp)
				*cp = '=';
		}
		if(np)
		{
			if(attest(np,ARRAY))
			{
				register struct Namaray *ap = arayp (np);
				register int i, imax;
				i = ap->adot = 0;
				imax = ap->maxi;
				for (; i <= imax; i++)
				{
					ap->adot = i;
					if (ap->val[i])
						printnam(np,option);
				}
			}
			else
				printnam(np,option);
		}
	}
}

static char *staknam(n,value)
char *	value;
register NAMPTR	n;
{
	register char *p,*q;
	q = getstak(strlen(n->namid)+strlen(value)+2);
	p=movstr(n->namid,q);
	*p++ = '=';
	strcpy(p,value);
	return(q);
}


void	printflg(n)
register NAMPTR		n;
{
	register SYSPTR	syscan;
	register int val;
	if (namflag(n) != N_DEFAULT)
	{
		syscan=attributes;
		while(*syscan->sysnam)
		{
			val = syscan->sysval;
			if(attest(n,val)==val)
			{
				p_str(syscan->sysnam,SP);
		                if (attest (n, L_JUST|R_JUST|Z_FILL))
					p_num(n->namsz,SP);
				if(val == (BLT_NOD|INT_GER))
					break;
			}
		        if(val == INT_GER && attest(n,INT_GER))
			{
				if(n->namsz != 10)
				{
					p_str(intbase,SP);
					p_num(n->namsz,SP);
				}
				break;
			}
			syscan++;
		}
		pr_name(n,NL);
	}
}

int	genenv()
{
	register char **e=environ;
	register NAMPTR		n;
	rsflag = 1;
	if(e)
	{
		while(*e)
		{
			n = setname(*e, (N_IMPORT|N_EXPORT)); 
			n->namid = *e++;
		}
	}
	return(rsflag);
}


static void	countnam()
{
	namec++;
}


static void	pushnam(n)
register NAMPTR		n;
{
	register char *value;
	if(attest(n,N_IMPORT))
		*argnam++ = n->namid;
	else if(value=valup(n))
		*argnam++ = staknam(n,value);
}

/*
 * Generate the environment list for the child.
 */


char **setenv()
{
	register char **er;
	namec = 0;
	/* L_ARGNOD gets generated automatically as full path name of command */
	pattrib(L_ARGNOD,~N_EXPORT);
	gscan_some (countnam,namep, N_EXPORT|N_IMPORT, N_EXPORT);
	er = (char**)getstak((namec+2)*BYTESPERWORD);
	argnam = ++er;
	gscan_some (pushnam, namep, N_EXPORT|N_IMPORT, N_EXPORT);
	*argnam++ = 0;
	return(er);
}

/*
 * Initialize the shell name and alias table
 */

void meminit()
{
	register NAMPTR np;
	bltin_nodes = (NAMPTR)malloc((unsigned)(NNODES*sizeof(struct Namnod)));
	namebase = namep = inittree(node_names,bltin_nodes,0);
	/* set up random number generator */
#ifdef apollo
	(PPIDNOD)->value.namval.cp = (char*)(&ppid);
	(L_ARGNOD)->value.namval.cp = (char*)(&lastarg);
	(TMOUTNOD)->value.namval.cp = (char*)(&timeout);
	(SECONDS)->value.namval.cp = (char*)(&seconds);
	(MCHKNOD)->value.namval.cp = (char*)(&mailchk);
	(RANDNOD)->value.namval.cp = (char*)(&randnum);
#endif	/* apollo */
	namflag(RANDNOD) = N_FREE|INT_GER|BLT_NOD;
	/* set up the seconds clock */
	namflag(SECONDS) = N_FREE|INT_GER|BLT_NOD;
	set_second(0L);
	namflag(MCHKNOD) = N_FREE|INT_GER;
	namflag(TMOUTNOD) = INT_GER;
	namflag(PPIDNOD) = (N_FREE|INT_GER|N_RDONLY);
	namflag(L_ARGNOD) = N_FREE|IN_DIR;
	np = (NAMPTR)malloc(NALIAS*sizeof(struct Namnod));
	alias = inittree(alias_names,np,N_EXPORT);
	prnames = gettree(MEMSIZE/4);
}

/*
 * re-initialize name-value pairs after fork
 */


static struct Amemory *inittree(name_vals,nodes,atflag)
struct name_value *name_vals;
NAMPTR nodes;
{
	register struct Amemory *treep;
	register NAMPTR np;
	register struct name_value *nv;
	int flag;
	treep = gettree (MEMSIZE);
	for(np=nodes,nv=name_vals;*nv->nv_name;nv++,np++)
	{
		np->namid = nv->nv_name;
		np->value.namval.cp = nv->nv_value;
		flag = 0;
#ifdef apollo
		if(*nv->nv_value==0)
			np->value.namval.cp = 0;
		else
#else
		if(nv->nv_value)
#endif	/* apollo */
		{
			flag = atflag|N_FREE;
			if(atflag && *nv->nv_value=='/')
				flag |= T_FLAG;
		}
		sattrib(np,flag);
		np->namsz = 10;
		linknod (np, treep);
	}
	return(treep);
}

/*
 * create a new environment scope
 */

void mem_scope(envlist)
ARGPTR envlist;
{
	register struct Amemory *sav_namep = namep;
	register struct Amemory *newscope;
	newscope = gettree(MEMSIZE/8);
	newscope->nexttree = sav_namep;
	namep = newscope;
	setlist(envlist,N_EXPORT|S_FLAG);
	newscope->nexttree = NULL;
	namep = sav_namep;
	scan_all(no_export,newscope);
	newscope->nexttree = sav_namep;
	namep = newscope;
}

/*
 * Temporarily remove name from export list of previous scopes
 */

static void no_export(nnod)
register struct Namnod *nnod;
{
	register struct Namnod *np = checkfor(nnod->namid,namep);
	if(np && attest(np,N_EXPORT))
	{
		pattrib(np,~N_EXPORT);
		attrib(np,E_FLAG);
	}
}

/*
 * free up top environment scope
 */

void mem_unscope()
{
	register struct Amemory *ap = namep;
	if((namep = ap->nexttree)==NULL)
		namep = namebase;
	scan_all(rm_node,ap);
	free((char*)ap);
}

/*
 * free up all environment scopes except the first
 */

void name_unscope()
{
	while(namep->nexttree)
		mem_unscope();
}

/*
 * Remove a node and free up all the space
 * Restate export attribute for hidden nodes if necessary
 */
static void rm_node(nnod)
register struct Namnod *nnod;
{
	register struct Namnod *np = checkfor(nnod->namid,namep);
	if(np && attest(np,E_FLAG))
	{
		pattrib(np,~E_FLAG);
		attrib(np,N_EXPORT);
	}
	pattrib(nnod,~N_EXPORT);
	rmlocal(nnod);
	free((char*)nnod);
}

/* 
 * Remove freeable local space associated with the namval field
 * of nnod. This includes any strings representing the value(s) of the
 * node, as well as its dope vector, if it is an array.
 */

void	rmlocal (nnod)
register struct Namnod *nnod;
{
	register int i;
	register struct Nodval *nv;
	register struct Namaray *ap;
	register union Namval *up = &nnod->value.namval;

	/* return if the node is global or unfreeable */

	if (attest (nnod, N_EXPORT|N_FREE))
		return;
	/*
	 * If the node is a freeable array, then free both the stringspace
	 * associated with it and its dope vector.
	 */

	else if (attest (nnod, ARRAY))
	{
	        ap = up->aray;
		i = ap->maxi;
		while(i >= 0)
		{
			nv = ap->val[i--];
			if (nv)
			{
	                	if (freeble (nv))
					rmnval (unmark (nv));
				else 
				{
					up = &nv->namval;
					if ((up->cp) && ((nv->namflg & (N_FREE|N_ALLOC)) == 0))
						free ((nv->namflg & IN_DIR)?up->up->cp:up->cp);
				}
			}
		}
		free ((char*)(arayp(nnod)));
		nnod->value.namval.cp = NULL;
	}
	/* 
	 * otherwise node is a freeable scalar, so free the string 
	 * representing its value.
	 */
	else
	{
		unassign (nnod);
	}
	sattrib (nnod, N_DEFAULT);
}

/*
 * Get the value of a built-in node
 * A lookup may not be necessary
 */

char *qvalup(n)
register NAMPTR	n;
{
	if(namep->nexttree)
		n = lookup((node_names+(n-bltin_nodes))->nv_name);
	return(valup(n));
}

/*
 * lookup name in trees root and return Namnod pointer with this name.
 * If none exists, it will not be created.
 */

NAMPTR checkfor(name,root)
char *name;
struct Amemory *root;
{
	register struct Namnod *np = NULL;
	register struct Amemory *app = root;
	struct Namnod *findnod();
	while(app && np==NULL)
	{
		np = findnod(name,app,0);
		app = app->nexttree;
	}
	return((np==NULL||isnull(np))?NULL:np);
}

/*
 *  for the whence command
 */

void	do_whence(com,flag)
char **com;
register int flag;
{
	register char *a1;
	register struct Namnod *np;
	register char *cp;
	struct Namnod *fp;
	while(a1 = *++com)
	{
		if(flag)
			fputs(a1,output);
		np = NULL;
		/* reserved words first */
		if(syslook(a1,reserved))
		{
			if(flag)
				a1 = is_reserved;
		}
		/* non-tracked aliases */
		else if((np=findnod(a1,alias,CHK_FOR)) && !isnull(np)
			 && attest(np,T_FLAG)==0 && (a1=valup(np))) 
		{
			if(flag)
			{
				if(attest(np,N_EXPORT))
					cp = is_xalias;
				else
					cp = is_alias;
				fputs(cp,output);
			}
		}
		/* built-in commands next */
		else if(syslook(a1,commands))
		{
			if(flag)
				a1 = is_builtin;
		}
		/* functions next */
		else if((fp=findnod(a1,prnames,CHK_FOR))&& !isnull(fp))
		{
			if(flag)
		 		a1=attest(fp,N_EXPORT)?is_xfunction:is_function;
		}
		else
		{
			/* find full pathname */
			a1 = fullname(a1);
			if(a1)
			{
				if(flag)
				{
					/* tracked aliases next */
					if(np && attest(np,T_FLAG) && *a1 == '/')
						fputs(is_talias,output);
					else
						fputs(is_,output);
				}
			}
			else 
			{
				a1 = (flag?notfound:nullstr);
				exitval |= 1;
			}
		}
		p_str(a1,NL);
	}
}

/*
 * these functions are used to get and set the SECONDS variable
 */

static long sec_offset;

static int set_second(n)
long n;
{
	sec_offset =  time((long*)0) - n ;
}

static long get_second()
{
	return(time((long*)0)-sec_offset);
}

/*
 * These functions are used to get and set the RANDOM variable
 */

static int set_rand(n)
long n;
{
	srand((int)n);
}

static long get_rand()
{
	return((long)rand());
}

#ifdef ECHO_N
char *echo_mode()
{
	register char *cp;
	optflag savopts;
	if(echo_arg==0)
	{
#ifdef apollo
		register NAMPTR np = checkfor("SYSTYPE",namep);
		if(np && (cp=valup(np)))
		{
			echo_arg = (*cp=='b'?echo_opt:minus);
			return(echo_arg);
		}
#endif /* apollo */
		savopts = flags;
		off_option(HASHALL);
		cp = fullname(echo_bin+5);
		flags = savopts;
		if(eq(cp,echo_bin))
			echo_arg = echo_opt;
		else
			echo_arg = minus;
	}
	return(echo_arg);
}
#endif	/* ECHO_N */
