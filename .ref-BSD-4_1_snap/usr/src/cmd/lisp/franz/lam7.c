static char *sccsid = "@(#)lam7.c	35.4 7/8/81";

#include "global.h"

lispval
Lfork() {
	register lispval temp;
	int pid;

	chkarg(0,"fork");
	if ((pid=fork())) {
		return(inewint(pid));
	} else
		return(nil);
}

lispval
Lwait()
{
	register lispval ret, temp;
	int status = -1, pid;
	Savestack(2);


	chkarg(0,"wait");
	pid = wait(&status);
	ret = newdot();
	protect(ret);
	temp = inewint(pid);
	ret->d.car = temp;
	temp = inewint(status);
	ret->d.cdr = temp;
	Restorestack();
	return(ret);
}

lispval
Lpipe()
{
	register lispval ret, temp;
	int pipes[2];
	Savestack(2);

	chkarg(0,"pipe");
	pipes[0] = -1;
	pipes[1] = -1;
	pipe(pipes);
	ret = newdot();
	protect(ret);
	temp = inewint(pipes[0]);
	ret->d.car = temp;
	temp = inewint(pipes[1]);
	ret->d.cdr = temp;
	Restorestack();
	return(ret);
}

lispval
Lfdopen()
{
	register lispval fd, type;
	FILE *ptr;

	chkarg(2,"fdopen");
	type = (np-1)->val;
	fd = lbot->val;
	if( TYPE(fd)!=INT )
		return(nil);
	if ( (ptr=fdopen((int)fd->i, (char *)type->a.pname))==NULL)
		return(nil);
	return(P(ptr));
}

lispval
Lexece()
{
	lispval fname, arglist, envlist, temp;
	char *args[100], *envs[100], estrs[1024];
	char *p, *cp, **sp;

	fname = nil;
	arglist = nil;
	envlist = nil;

	switch(np-lbot) {
	case 3: envlist = lbot[2].val;
	case 2: arglist = lbot[1].val;
	case 1: fname   = lbot[0].val;
	case 0: break;
	default:
		argerr("exece");
	}

	while (TYPE(fname)!=ATOM)
	   fname = error("exece: non atom function name",TRUE);
	while (TYPE(arglist)!=DTPR && arglist!=nil)
		arglist = error("exece: non list arglist",TRUE);
	for (sp=args; arglist!=nil; arglist=arglist->d.cdr) {
		temp = arglist->d.car;
		if (TYPE(temp)!=ATOM)
			error("exece: non atom argument seen",FALSE);
		*sp++ = temp->a.pname;
	}
	*sp = 0;
	if (TYPE(envlist)!=DTPR && envlist!=nil)
		return(nil);
	for (sp=envs,cp=estrs; envlist!=nil; envlist=envlist->d.cdr) {
		temp = envlist->d.car;
		if (TYPE(temp)!=DTPR || TYPE(temp->d.car)!=ATOM
		  || TYPE(temp->d.cdr)!=ATOM)
 		     error("exece: Bad enviroment list",FALSE);
		*sp++ = cp;
		for (p=temp->d.car->a.pname; (*cp++ = *p++);) ;
		*(cp-1) = '=';
		for (p=temp->d.cdr->a.pname; (*cp++ = *p++);) ;
	}
	*sp = 0;
	
	return(inewint(execve(fname->a.pname, args, envs)));
}
	
int gensymcounter = 0;  /* should really be in data.c */

lispval
Lgensym()
{
	lispval arg;
	char leader;

	switch(np-lbot)
	{
	    case 0: arg = nil;
		    break;
	    case 1: arg = lbot->val;
		    break;
	    default: argerr("gensym");
	}
	leader = 'g';
	if (arg != nil && TYPE(arg)==ATOM)
		leader = arg->a.pname[0];
	sprintf(strbuf, "%c%05d", leader, gensymcounter++);
	atmlen = 7;
	return((lispval)newatom());
}

extern struct types {
char	*next_free;
int	space_left,
	space,
	type,
	type_len;			/*  note type_len is in units of int */
lispval *items,
	*pages,
	*type_name;
struct heads
	*first;
} atom_str ;

lispval
Lremprop()
{
	register struct argent *argp;
	register lispval pptr, ind, opptr;
	lispval atm;
	int disemp = FALSE;

	chkarg(2,"remprop");
	argp = lbot;
	ind = argp[1].val;
	atm = argp->val;
	switch (TYPE(atm)) {
	case DTPR:
		pptr = atm->d.cdr;
		disemp = TRUE;
		break;
	case ATOM:
		if((lispval)atm==nil)
			pptr = nilplist;
		else
			pptr = atm->a.plist;
		break;
	default:
		errorh(Vermisc, "remprop: Illegal first argument :",
		       nil, FALSE, 0, atm);
	}
	opptr = nil;
	if (pptr==nil) 
		return(nil);
	while(TRUE) {
		if (TYPE(pptr->d.cdr)!=DTPR)
			errorh(Vermisc, "remprop: Bad property list",
			       nil, FALSE, 0,atm);
		if (pptr->d.car == ind) {
			if( opptr != nil)
				opptr->d.cdr = pptr->d.cdr->d.cdr;
			else if(disemp)
				atm->d.cdr = pptr->d.cdr->d.cdr;
			else if(atm==nil)
				nilplist = pptr->d.cdr->d.cdr;
			else
				atm->a.plist = pptr->d.cdr->d.cdr;
			return(pptr->d.cdr);
		}
		if ((pptr->d.cdr)->d.cdr == nil) return(nil);
		opptr = pptr->d.cdr;
		pptr = (pptr->d.cdr)->d.cdr;
	}
}

lispval
Lbcdad()
{
	lispval ret, temp;

	chkarg(1,"bcdad");
	temp = lbot->val;
	if (TYPE(temp)!=ATOM)
		error("ONLY ATOMS HAVE FUNCTION BINDINGS", FALSE);
	temp = temp->a.fnbnd;
	if (TYPE(temp)!=BCD)
		return(nil);
	ret = newint();
	ret->i = (int)temp;
	return(ret);
}

lispval
Lstringp()
{
	chkarg(1,"stringp");
	if (TYPE(lbot->val)==STRNG)
		return(tatom);
	return(nil);
}

lispval
Lsymbolp()
{
	chkarg(1,"symbolp");
	if (TYPE(lbot->val)==ATOM)
		return(tatom);
	return(nil);
}

lispval
Lrematom()
{
	register lispval temp;

	chkarg(1,"rematom");
	temp = lbot->val;
	if (TYPE(temp)!=ATOM)
		return(nil);
	temp->a.fnbnd = nil;
	temp->a.pname = (char *)CNIL;
	temp->a.plist = nil;
	(atom_items->i)--;
	(atom_str.space_left)++;
	temp->a.clb=(lispval)atom_str.next_free;
	atom_str.next_free=(char *) temp;
	return(tatom);
}

#define QUTMASK 0200
#define VNUM 0000

lispval
Lprname()
{
	lispval a, ret;
	register lispval work, prev;
	char	*front, *temp; int clean;
	char ctemp[100];
	extern unsigned char *ctable;
	Savestack(2);

	chkarg(1,"prname");
	a = lbot->val;
	switch (TYPE(a)) {
		case INT:
			sprintf(ctemp,"%d",a->i);
			break;

		case DOUB:
			sprintf(ctemp,"%f",a->r);
			break;
	
		case ATOM:
			temp = front = a->a.pname;
			clean = *temp;
			if (*temp == '-') temp++;
			clean = clean && (ctable[*temp] != VNUM);
			while (clean && *temp)
				clean = (!(ctable[*temp++] & QUTMASK));
			if (clean)
				strcpyn(ctemp, front, 99);
			else	
				sprintf(ctemp,"\"%s\"",front);
			break;
	
		default:
			error("prname does not support this type", FALSE);
	}
	temp = ctemp;
	protect(ret = prev = newdot());
	while (*temp) {
		prev->d.cdr = work = newdot();
		strbuf[0] = *temp++;
		strbuf[1] = 0;
		work->d.car = getatom();
		work->d.cdr = nil;
		prev = work;
	}
	Restorestack();
	return(ret->d.cdr);
}

Lexit()
{
	register lispval handy;
	if(np-lbot==0) franzexit(0);
	handy = lbot->val;
	if(TYPE(handy)==INT)
		franzexit(handy->i);
	franzexit(-1);
}
lispval
Iimplode(unintern)
{
	register lispval handy, work;
	register char *cp = strbuf;
	extern int atmlen;	/* used by newatom and getatom */

	chkarg(1,"implode");
	for(handy = lbot->val; handy!=nil; handy = handy->d.cdr)
	{
		work = handy->d.car;
		if(cp >= endstrb)
			errorh(Vermisc,"maknam/impode argument exceeds buffer",nil,FALSE,43,lbot->val);
	again:
		switch(TYPE(work))
		{
		case ATOM:
			*cp++ = work->a.pname[0];
			break;
		case SDOT:
		case INT:
			*cp++ = work->i;
			break;
		case STRNG:
			*cp++ = * (char *) work;
			break;
		default:
			work = errorh(Vermisc,"implode/maknam: Illegal type for this arg:",nil,FALSE,44,work);
			goto again;
		}
	}
	*cp = 0;
	if(unintern) return((lispval)newatom());
	else return((lispval) getatom());
}

lispval
Lmaknam()
{
	return(Iimplode(TRUE));		/* unintern result */
}

lispval
Limplode()
{
	return(Iimplode(FALSE));	/* intern result */
}

lispval
Lintern()
{
	register int hash;
	register lispval handy,atpr;
	register char *name;


	chkarg(1,"intern");
	if(TYPE(handy=lbot->val) != ATOM)
		errorh(Vermisc,"non atom to intern ",nil,FALSE,0,handy);
	/* compute hash of pname of arg */
	hash = hashfcn(handy->a.pname);

	/* search for atom with same pname on hash list */

	atpr = (lispval) hasht[hash];
	for(atpr = (lispval) hasht[hash] 
		 ; atpr != CNIL 
		 ; atpr = (lispval)atpr->a.hshlnk)
	{
		if(strcmp(atpr->a.pname,handy->a.pname) == 0) return(atpr);
	}
	
	/* not there yet, put the given one on */

	handy->a.hshlnk = hasht[hash];
	hasht[hash] = (struct atom *)handy;
	return(handy);
}
