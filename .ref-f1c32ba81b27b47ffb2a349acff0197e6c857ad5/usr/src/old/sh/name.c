#ifndef lint
static char sccsid[] = "@(#)name.c	4.4 %G%";
#endif

#
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Bell Telephone Laboratories
 *
 */

#include	"defs.h"

PROC BOOL	chkid();


NAMNOD	ps2nod	= {	NIL,		NIL,		ps2name},
	fngnod	= {	NIL,		NIL,		fngname},
	pathnod = {	NIL,		NIL,		pathname},
	ifsnod	= {	NIL,		NIL,		ifsname},
	ps1nod	= {	&pathnod,	&ps2nod,	ps1name},
	homenod = {	&fngnod,	&ifsnod,	homename},
	mailnod = {	&homenod,	&ps1nod,	mailname};

NAMPTR		namep = &mailnod;


/* ========	variable and string handling	======== */

syslook(w,syswds)
	STRING		w;
	SYSTAB		syswds;
{
	REG CHAR	first;
	REG STRING	s;
	REG SYSPTR	syscan;

	syscan=syswds; first = *w;

	WHILE s=syscan->sysnam
	DO  IF first == *s
		ANDF eq(w,s)
	    THEN return(syscan->sysval);
	    FI
	    syscan++;
	OD
	return(0);
}

setlist(arg,xp)
	REG ARGPTR	arg;
	INT		xp;
{
	WHILE arg
	DO REG STRING	s=mactrim(arg->argval);
	   setname(s, xp);
	   arg=arg->argnxt;
	   IF flags&execpr
	   THEN prs(s);
		IF arg THEN blank(); ELSE newline(); FI
	   FI
	OD
}

VOID	setname(argi, xp)
	STRING		argi;
	INT		xp;
{
	REG STRING	argscan=argi;
	REG NAMPTR	n;

	IF letter(*argscan)
	THEN	WHILE alphanum(*argscan) DO argscan++ OD
		IF *argscan=='='
		THEN	*argscan = 0;
			n=lookup(argi);
			*argscan++ = '=';
			attrib(n, xp);
			IF xp&N_ENVNAM
			THEN	
				/*
				 * Importing IFS can be very dangerous
				 */
				IF !bcmp(argi, "IFS=", sizeof("IFS=") - 1)
				THEN 
					int uid;
					IF (uid = getuid())!=geteuid() ORF !uid
					THEN
						return;
					FI
				FI
				n->namenv = n->namval = argscan;
			ELSE	assign(n, argscan);
			FI
			return;
		FI
	FI
	failed(argi,notid);
}

replace(a, v)
	REG STRING	*a;
	STRING		v;
{
	free(*a); *a=make(v);
}

dfault(n,v)
	NAMPTR		n;
	STRING		v;
{
	IF n->namval==0
	THEN	assign(n,v)
	FI
}

assign(n,v)
	NAMPTR		n;
	STRING		v;
{
	IF n->namflg&N_RDONLY
	THEN	failed(n->namid,wtfailed);
	ELSE	replace(&n->namval,v);
	FI
}

INT	readvar(names)
	STRING		*names;
{
	FILEBLK		fb;
	REG FILE	f = &fb;
	REG CHAR	c;
	REG INT		rc=0;
	NAMPTR		n=lookup(*names++); /* done now to avoid storage mess */
	STKPTR		rel=relstak();

	push(f); initf(dup(0));
	IF lseek(0,0L,1)==-1
	THEN	f->fsiz=1;
	FI

	LOOP	c=nextc(0);
		IF (*names ANDF any(c, ifsnod.namval)) ORF eolchar(c)
		THEN	zerostak();
			assign(n,absstak(rel)); setstak(rel);
			IF *names
			THEN	n=lookup(*names++);
			ELSE	n=0;
			FI
			IF eolchar(c)
			THEN	break;
			FI
		ELSE	pushstak(c);
		FI
	POOL
	WHILE n
	DO assign(n, nullstr);
	   IF *names THEN n=lookup(*names++); ELSE n=0; FI
	OD

	IF eof THEN rc=1 FI
	lseek(0, (long)(f->fnxt-f->fend), 1);
	pop();
	return(rc);
}

assnum(p, i)
	STRING		*p;
	INT		i;
{
	itos(i); replace(p,numbuf);
}

STRING	make(v)
	STRING		v;
{
	REG STRING	p;

	IF v
	THEN	movstr(v,p=alloc(length(v)));
		return(p);
	ELSE	return(0);
	FI
}


NAMPTR		lookup(nam)
	REG STRING	nam;
{
	REG NAMPTR	nscan=namep;
	REG NAMPTR	*prev;
	INT		LR;

	IF !chkid(nam)
	THEN	failed(nam,notid);
	FI
	WHILE nscan
	DO	IF (LR=cf(nam,nscan->namid))==0
		THEN	return(nscan);
		ELIF LR<0
		THEN	prev = &(nscan->namlft);
		ELSE	prev = &(nscan->namrgt);
		FI
		nscan = *prev;
	OD

	/* add name node */
	nscan=alloc(sizeof *nscan);
	nscan->namlft=nscan->namrgt=NIL;
	nscan->namid=make(nam);
	nscan->namval=0; nscan->namflg=N_DEFAULT; nscan->namenv=0;
	return(*prev = nscan);
}

LOCAL BOOL	chkid(nam)
	STRING		nam;
{
	REG CHAR *	cp=nam;

	IF !letter(*cp)
	THEN	return(FALSE);
	ELSE	WHILE *++cp
		DO IF !alphanum(*cp)
		   THEN	return(FALSE);
		   FI
		OD
	FI
	return(TRUE);
}

LOCAL VOID (*namfn)();
namscan(fn)
	VOID		(*fn)();
{
	namfn=fn;
	namwalk(namep);
}

LOCAL VOID	namwalk(np)
	REG NAMPTR	np;
{
	IF np
	THEN	namwalk(np->namlft);
		(*namfn)(np);
		namwalk(np->namrgt);
	FI
}

VOID	printnam(n)
	NAMPTR		n;
{
	REG STRING	s;

	sigchk();
	IF s=n->namval
	THEN	prs(n->namid);
		prc('='); prs(s);
		newline();
	FI
}

LOCAL STRING	staknam(n)
	REG NAMPTR	n;
{
	REG STRING	p;

	p=movstr(n->namid,staktop);
	p=movstr("=",p);
	p=movstr(n->namval,p);
	return(getstak(p+1-ADR(stakbot)));
}

VOID	exname(n)
	REG NAMPTR	n;
{
	IF n->namflg&N_EXPORT
	THEN	free(n->namenv);
		n->namenv = make(n->namval);
	ELSE	free(n->namval);
		n->namval = make(n->namenv);
	FI
}

VOID	printflg(n)
	REG NAMPTR		n;
{
	IF n->namflg&N_EXPORT
	THEN	prs(export); blank();
	FI
	IF n->namflg&N_RDONLY
	THEN	prs(readonly); blank();
	FI
	IF n->namflg&(N_EXPORT|N_RDONLY)
	THEN	prs(n->namid); newline();
	FI
}

VOID	setupenv()
{
	REG STRING	*e=environ;

	WHILE *e
	DO setname(*e++, N_ENVNAM) OD
}

LOCAL INT	namec;

VOID	countnam(n)
	NAMPTR		n;
{
	namec++;
}

LOCAL STRING 	*argnam;

VOID	pushnam(n)
	NAMPTR		n;
{
	IF n->namval
	THEN	*argnam++ = staknam(n);
	FI
}

STRING	*setenv()
{
	REG STRING	*er;

	namec=0;
	namscan(countnam);
	argnam = er = getstak(namec*BYTESPERWORD+BYTESPERWORD);
	namscan(pushnam);
	*argnam++ = 0;
	return(er);
}
