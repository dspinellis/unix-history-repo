#include "global.h"

lispval
Lfork() {
	register lispval temp;
	int pid;

	chkarg(0);
	if ((pid=fork())) {
		temp = newint();
		temp->i = pid;
		return(temp);
	} else
		return(nil);
}

lispval
Lwait()
{
	register lispval ret, temp;
	int status = -1, pid;
	snpand(2);


	chkarg(0);
	pid = wait(&status);
	ret = newdot();
	protect(ret);
	temp = newint();
	temp->i = pid;
	ret->car = temp;
	temp = newint();
	temp->i = status;
	ret->cdr = temp;
	return(ret);
}

lispval
Lpipe()
{
	register lispval ret, temp;
	int pipes[2];

	chkarg(0);
	pipes[0] = -1;
	pipes[1] = -1;
	pipe(pipes);
	ret = newdot();
	protect(ret);
	temp = newint();
	temp->i = pipes[0];
	ret->car = temp;
	temp = newint();
	temp->i = pipes[1];
	ret->cdr = temp;
	return(ret);
}

lispval
Lfdopen()
{
	register lispval fd, type;
	FILE *ptr;

	chkarg(2);
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
	snpand(0);

	chkarg(3);
	envlist = (--np)->val;
	arglist = (--np)->val;
	fname = (--np)->val;
	if (TYPE(fname)!=ATOM)
		return(nil);
	if (TYPE(arglist)!=DTPR && arglist!=nil)
		return(nil);	
	for (sp=args; arglist!=nil; arglist=arglist->d.cdr) {
		temp = arglist->d.car;
		if (TYPE(temp)!=ATOM)
			return(nil);
		*sp++ = temp->a.pname;
	}
	*sp = 0;
	if (TYPE(envlist)!=DTPR && envlist!=nil)
		return(nil);
	for (sp=envs,cp=estrs; envlist!=nil; envlist=envlist->d.cdr) {
		temp = envlist->d.car;
		if (TYPE(temp)!=DTPR || TYPE(temp->d.car)!=ATOM
		  || TYPE(temp->d.cdr)!=ATOM)
			return(nil);
		*sp++ = cp;
		for (p=temp->d.car->a.pname; (*cp++ = *p++);) ;
		*(cp-1) = '=';
		for (p=temp->d.cdr->a.pname; (*cp++ = *p++);) ;
	}
	*sp = 0;
	execve(fname->a.pname, args, envs);
	return(nil);
}
	
lispval
Lgensym()
{
	lispval arg;
	char leader;
	static int counter = 0;

	chkarg(1);
	arg = lbot->val;
	leader = 'g';
	if (arg != nil && TYPE(arg)==ATOM)
		leader = arg->a.pname[0];
	sprintf(strbuf, "%c%05d", leader, counter++);
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
	register struct argent *lbot, *np;
	lispval atm;
	int disemp = FALSE;

	chkarg(2);
	argp = lbot;
	ind = argp[1].val;
	atm = argp->val;
	switch (TYPE(atm)) {
	case DTPR:
		pptr = atm->cdr;
		disemp = TRUE;
		break;
	case ATOM:
		if((lispval)atm==nil)
			pptr = nilplist;
		else
			pptr = atm->plist;
		break;
	default:
		errorh(Vermisc, "remprop: Illegal first argument :",
		       nil, FALSE, 0, atm);
	}
	opptr = nil;
	if (pptr==nil) 
		return(nil);
	while(TRUE) {
		if (TYPE(pptr->cdr)!=DTPR)
			errorh(Vermisc, "remprop: Bad property list",
			       nil, FALSE, 0,atm);
		if (pptr->car == ind) {
			if( opptr != nil)
				opptr->cdr = pptr->cdr->cdr;
			else if(disemp)
				atm->cdr = pptr->cdr->cdr;
			else if(atm==nil)
				nilplist = pptr->cdr->cdr;
			else
				atm->plist = pptr->cdr->cdr;
			return(pptr->cdr);
		}
		if ((pptr->cdr)->cdr == nil) return(nil);
		opptr = pptr->cdr;
		pptr = (pptr->cdr)->cdr;
	}
}

lispval
Lbcdad()
{
	lispval ret, temp;

	chkarg(1);
	temp = lbot->val;
	if (TYPE(temp)!=ATOM)
		error("ONLY ATOMS HAVE FUNCTION BINDINGS", FALSE);
	temp = temp->fnbnd;
	if (TYPE(temp)!=BCD)
		return(nil);
	ret = newint();
	ret->i = (int)temp;
	return(ret);
}

lispval
Lstringp()
{
	chkarg(1);
	if (TYPE(lbot->val)==STRNG)
		return(tatom);
	return(nil);
}

lispval
Lsymbolp()
{
	chkarg(1);
	if (TYPE(lbot->val)==ATOM)
		return(tatom);
	return(nil);
}

lispval
Lrematom()
{
	register lispval temp;

	chkarg(1);
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
	extern char *ctable;
	snpand(2);

	chkarg(1);
	a = lbot->val;
	switch (TYPE(a)) {
		case INT:
			sprintf(ctemp,"%d",a->i);
			break;

		case DOUB:
			sprintf(ctemp,"%f",a->r);
			break;
	
		case ATOM:
			temp = front = a->pname;
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
		prev->cdr = work = newdot();
		strbuf[0] = *temp++;
		strbuf[1] = 0;
		work->car = getatom();
		work->cdr = nil;
		prev = work;
	}
	return(ret->cdr);
}
Lexit()
{
	register lispval handy;
	if(np-lbot==0) exit(0);
	handy = lbot->val;
	if(TYPE(handy)==INT)
		exit(handy->i);
	exit(-1);
}
lispval
Iimplode(unintern)
{
	register lispval handy, work;
	register char *cp = strbuf;
	extern int atmlen;	/* used by newatom and getatom */

	chkarg(1);
	for(handy = lbot->val; handy!=nil; handy = handy->cdr)
	{
		work = handy->car;
		if(cp >= endstrb)
			errorh(Vermisc,"maknam/impode argument exceeds buffer",nil,FALSE,43,lbot->val);
	again:
		switch(TYPE(work))
		{
		case ATOM:
			*cp++ = work->pname[0];
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
