#ifndef lint
static char *rcsid =
   "$Header: lam2.c,v 1.6 87/12/14 18:48:13 sklower Exp $";
#endif

/*					-[Fri Aug  5 12:46:16 1983 by jkf]-
 * 	lam2.c				$Locker:  $
 * lambda functions
 *
 * (c) copyright 1982, Regents of the University of California
 */

# include "global.h"
# include <signal.h>
# include "structs.h"
# include "chars.h"
# include "chkrtab.h"
/*
 * (flatc 'thing ['max]) returns the smaller of max and the number of chars
 * required to print thing linearly.
 * if max argument is not given, we assume the second arg is infinity
 */
static flen; /*Internal to this module, used as a running counter of flatsize*/
static fmax; /*used for maximum for quick reference */
char *strcpy();

lispval
Lflatsi()
{
	register lispval current;
	Savestack(1); 			/* fixup entry mask */

	fmax = 0x7fffffff;	/* biggest integer by default */
	switch(np-lbot) 
	{
	    case 2: current = lbot[1].val;
		    while(TYPE(current) != INT)
			current = errorh1(Vermisc,
					"flatsize: second arg not integer",
					nil,TRUE,0,current);
		    fmax = current->i;
	    case 1: break;
	    default: argerr("flatsize");
	}

	flen = 0; 
	current = lbot->val;
	protect(nil); 			/*create space for argument to pntlen*/
	Iflatsi(current);
	Restorestack();
	return(inewint(flen));
}
/*
 * Iflatsi does the real work of the calculation for flatc
 */
Iflatsi(current)
register lispval current;
{

	if(flen > fmax) return;
	switch(TYPE(current)) {

	patom:
	case INT: case ATOM: case DOUB: case STRNG:
		np[-1].val = current;
		flen += Ipntlen();
		return;
	
	pthing:
	case DTPR:
		flen++;
		Iflatsi(current->d.car);
		current = current->d.cdr;
		if(current == nil) {
			flen++;
			return;
		}
		if(flen > fmax) return;
		switch(TYPE(current)) {
		case INT: case ATOM: case DOUB:
			flen += 4;
			goto patom;
		case DTPR:
			goto pthing;
		}
	}
}


#define EADC -1
#define EAD  -2
lispval
Lread()
{ return (r(EAD)); }

lispval
Lratom()
{ return (r(ATOM)); }

lispval
Lreadc()
{ return (r(EADC)); }


extern unsigned char *ctable;
/* r *********************************************************************/
/* this function maps the desired read 	function into the system-defined */
/* reading functions after testing for a legal port.			 */
lispval
r(op)
int op;
{
	unsigned char c; register lispval result;
	register cc;
	int orlevel; extern int rlevel;
	FILE *ttemp;
	struct nament *oldbnp = bnp;
	Savestack(2);

	switch(np-lbot) {
	case 0:
		protect(nil);
	case 1:
		protect(nil);
	case 2: break;
	default:
		argerr("read or ratom or readc");
	}
	result = Vreadtable->a.clb;
	chkrtab(result);
	orlevel = rlevel;
	rlevel = 0;
	ttemp = okport(Vpiport->a.clb,stdin);
	ttemp = okport(lbot->val,ttemp);
/*printf("entering switch\n");*/
	if(ttemp == stdin) fflush(stdout);	/* flush any pending 
						 * characters if reading stdin 
						 * there should be tests to see
						 * if this is a tty or pipe
						 */

	switch (op)
	{
	case EADC:	rlevel = orlevel;
			cc = getc(ttemp);
			c = cc;
			if(cc == EOF)
			{
				Restorestack();
				return(lbot[1].val);
			} else {
				strbuf[0] = hash = (c & 0177);
				strbuf[1] = 0;
				atmlen = 2;
				Restorestack();
				return((lispval)getatom(TRUE));
			}

	case ATOM:	rlevel = orlevel;
			result = (ratomr(ttemp));
			goto out;

	case EAD:	PUSHDOWN(Vpiport,P(ttemp)); /* rebind Vpiport */
			result = readr(ttemp);
	out:		if(result==eofa)
			{    
			     if(sigintcnt > 0) sigcall(SIGINT);
			     result = lbot[1].val;
			}
			rlevel = orlevel;
			popnames(oldbnp);	/* unwind bindings */
			Restorestack();
			return(result);
	}
	/* NOTREACHED */
}

/* Lload *****************************************************************/
/* Reads in and executes forms from the specified file. This should      */
/* really be an nlambda taking multiple arguments, but the error 	 */
/* handling gets funny in that case (one file out of several not 	 */
/* openable, for instance).						 */
lispval
Lload()
{
	register FILE *port;
	register char *p, *ttemp; register lispval vtemp;
	struct nament *oldbnp = bnp;
	int orlevel,typ;
	char longname[100];
	char *shortname, *end2, *Ilibdir();
	/*Savestack(4); not necessary because np not altered */

	chkarg(1,"load");
	if((typ = TYPE(lbot->val)) == ATOM)
	    ttemp =  lbot->val->a.pname ;  /* ttemp will point to name */
	else if(typ == STRNG)
	    ttemp = (char *) lbot->val;
	else 
	     return(error("FILENAME MUST BE ATOMIC",FALSE));
	strcpy(longname, Ilibdir());
	for(p = longname; *p; p++);
	*p++ = '/'; *p = 0;
	shortname = p;
	strcpy(p,ttemp);
	for(; *p; p++);
		end2 = p;
	strcpy(p,".l");
	if ((port = fopen(shortname,"r")) == NULL &&
		(port = fopen(longname, "r")) == NULL) {
			*end2 = 0;
			if ((port = fopen(shortname,"r")) == NULL &&
				(port = fopen(longname, "r")) == NULL)
					errorh1(Vermisc,"Can't open file: ", 
						     nil,FALSE,0,lbot->val);
	}
	orlevel = rlevel;
	rlevel = 0;

	if(ISNIL(copval(gcload,CNIL)) &&
		loading->a.clb != tatom &&
		ISNIL(copval(gcdis,CNIL)))
		gc((struct types *)CNIL);    /*  do a gc if gc will be off  */

	/* shallow bind the value of lisp atom piport 	*/
	/* so readmacros will work			*/
	PUSHDOWN(Vpiport,P(port));
	PUSHDOWN(loading,tatom);	/* set indication of loading status */

	while ((vtemp = readr(port)) != eofa) {
	    eval(vtemp);
	}
	popnames(oldbnp);		/* unbind piport, loading */

	rlevel = orlevel;
	fclose(port);
	return(nil);
}

/* concat **************************************************
-
-  use: (concat arg1 arg2 ... )
-
-  concatenates the print names of all of its arguments.
- the arguments may be atoms, integers or real numbers.
-
- *********************************************************/
lispval
Iconcat(unintern)
{
	register struct argent *temnp;
	register char *cp = strbuf;
	register lispval cur;
	int n;
	char *atomtoolong();
	lispval Lhau();

	*cp = NULL_CHAR ;

	/* loop for each argument */
	for(temnp = lbot + AD ; temnp < np ; temnp++)
	{
	    cur = temnp->val;
	    switch(TYPE(cur))
	    {
	    case ATOM:
		 n = strlen(cur->a.pname);
		 while(n + cp >= endstrb) cp = atomtoolong(cp);
		 strcpy(cp, cur->a.pname);
		 cp += n;
		 break;

	    case STRNG:
		 n = strlen( (char *) cur);
		 while(n + cp >= endstrb) cp = atomtoolong(cp);
		 strcpy(cp, (char *) cur);
		 cp += n;
		 break;

  	    case INT:
		 if(15 + cp >= endstrb) cp = atomtoolong(cp);
		 sprintf(cp,"%d",cur->i);
		 while(*cp) cp++;
		 break;

	    case DOUB:
		 if(15 + cp >= endstrb) cp = atomtoolong(cp);
		 sprintf(cp,"%f",cur->f);
		 while(*cp) cp++;
		 break;

	    case SDOT: {
		struct _iobuf _myiob;
		register lispval handy = cur;

		for(n = 12; handy->s.CDR!=(lispval) 0; handy = handy->s.CDR)
			n += 12;

		while(n + cp >= endstrb) cp = atomtoolong(cp);

		_myiob._flag = _IOWRT+_IOSTRG;
		_myiob._ptr = cp;
		_myiob._cnt = endstrb - cp - 1;

		pbignum(cur,&_myiob);
		cp = _myiob._ptr;
		*cp = 0;
		break; }
		    
	    default:
		 cur = error("Non atom or number to concat",TRUE);
		 continue;    /* if returns value, try it */
	   }

	}

	if(unintern)
		return( (lispval) newatom(FALSE)); /* uninterned atoms may
							have printname gc'd*/
	else
		return( (lispval) getatom(FALSE)) ;
}
lispval
Lconcat(){
	return(Iconcat(FALSE));
}
lispval
Luconcat(){
	return(Iconcat(TRUE));
}

lispval
Lputprop()
{
	lispval Iputprop();
	chkarg(3,"putprop");
	return(Iputprop(lbot->val,lbot[1].val,lbot[2].val));
}

/*
 * Iputprop :internal version of putprop used by some C functions
 *  note: prop and ind are lisp values but are not protected (by this
 * function) from gc.  The caller should protect them!!
 */
lispval
Iputprop(atm,prop,ind)
register lispval prop, ind, atm;
{
	register lispval pptr;
	lispval *tack;		/* place to begin property list */
	lispval pptr2;
	lispval errorh();
	Savestack(4);
	
 top:
	switch (TYPE(atm)) {
	case ATOM:
		if(atm == nil) tack = &nilplist;
		else tack =  &(atm->a.plist);
		break;
	case DTPR:
		for (pptr = atm->d.cdr ; pptr != nil ; pptr = pptr->d.cdr->d.cdr)
		    if(TYPE(pptr) != DTPR || TYPE(pptr->d.cdr) != DTPR) break;
		if(pptr != nil) 
		{   atm = errorh1(Vermisc,
				 "putprop: bad disembodied property list",
				 nil,TRUE,0,atm);
  		    goto top;
		}
		tack = (lispval *) &(atm->d.cdr);
		break;
	default:
		errorh1(Vermisc,"putprop: Bad first argument: ",nil,FALSE,0,atm);
	}
	pptr = *tack;	/* start of property list */
/*findit:*/
	for (pptr = *tack ; pptr != nil ; pptr = pptr->d.cdr->d.cdr)
		if (pptr->d.car == ind) {
			(pptr->d.cdr)->d.car = prop;
			Restorestack();
			return(prop);
		}
	/* not found, add to front
	   be careful, a gc could occur before the second newdot() */
	   
	pptr = newdot();
	pptr->d.car = prop;
	pptr->d.cdr = *tack;
	protect(pptr);
	pptr2 = newdot();
	pptr2->d.car = ind;
	pptr2->d.cdr = pptr;
	*tack = pptr2;
	Restorestack();
	return(prop);
}

/* get from property list 
 *   there are three routines to accomplish this
 *     Lget - lisp callable, the first arg can be a symbol or a disembodied
 *  	      property list.  In the latter case we check to make sure it
 *	      is a real one (as best we can).
 *     Iget - internal routine, the first arg must be a symbol, no disembodied
 *	      plists allowed
 *     Igetplist - internal routine, the first arg is the plist to search.
 */
lispval
Lget()
{
	register lispval ind, atm;
	register lispval dum1;
	lispval Igetplist();

	chkarg(2,"get");
	ind = lbot[1].val;
	atm = lbot[0].val;
top:
	switch(TYPE(atm)) {
	case ATOM:
		if(atm==nil) atm = nilplist;
		else atm = atm->a.plist;
		break;		

	case DTPR:
		for (dum1 = atm->d.cdr; dum1 != nil; dum1 = dum1->d.cdr->d.cdr)
		    if((TYPE(dum1) != DTPR) || 
		       (TYPE(dum1->d.cdr) != DTPR)) break; /* bad prop list */
		if(dum1 != nil) 
		{   atm = errorh1(Vermisc,
				 "get: bad disembodied property list",
				 nil,TRUE,0,atm);
  		    goto top;
		}
		atm = atm->d.cdr;
		break;
	default:
		/* remove since maclisp doesnt treat
		   this as an error, ugh
		   return(errorh1(Vermisc,"get: bad first argument: ",
			       nil,FALSE,0,atm));
		 */
		 return(nil);
	}

	while (atm != nil)
		{
			if (atm->d.car == ind)
				return ((atm->d.cdr)->d.car);
			atm = (atm->d.cdr)->d.cdr;
		}
	return(nil);
}
/*
 * Iget - the first arg must be a symbol.
 */
	
lispval
Iget(atm,ind)
register lispval atm, ind;
{
	lispval Igetplist();

	if(atm==nil)
		atm = nilplist;
	else
		atm = atm->a.plist;
	return(Igetplist(atm,ind));
}

/*
 *  Igetplist
 * pptr is a plist
 * ind is the indicator
 */

lispval
Igetplist(pptr,ind)
register lispval pptr,ind;
{
	while (pptr != nil)
		{
			if (pptr->d.car == ind)
				return ((pptr->d.cdr)->d.car);
			pptr = (pptr->d.cdr)->d.cdr;
		}
	return(nil);
}
lispval
Lgetd()
{
	register lispval typ;
	
	chkarg(1,"getd");
	typ = lbot->val;
	if (TYPE(typ) != ATOM) 
	   errorh1(Vermisc,
		  "getd: Only symbols have function definitions",
		  nil,
		  FALSE,
		  0,
		  typ);
	return(typ->a.fnbnd);
}
lispval
Lputd()
{
	register lispval atom, list;
	
	chkarg(2,"putd");
	list = lbot[1].val;
	atom = lbot->val;
	if (TYPE(atom) != ATOM) error("only symbols have function definitions",
					FALSE);
	atom->a.fnbnd = list;
	return(list);
}

/* ===========================================================
- mapping functions which return a list of the answers
- mapcar applies the given function to successive elements
- maplist applies the given function to successive sublists
- ===========================================================*/

lispval
Lmapcrx(maptyp,join)
int maptyp;		/* 0 = mapcar,  1 = maplist  */
int join;		/* 0 = the above, 1 = s/car/can/ */
{
	register struct argent *namptr;
	register index;
	register lispval temp;
	register lispval current;

	struct argent *first, *last;
	int count;
	lispval lists[25], result;
	Savestack(4);
	
	namptr = lbot + 1;
	count = np - namptr;
	if (count <= 0) return (nil);
	result = current =  (lispval) np;
	protect(nil);			/* set up space for returned list */
	protect(lbot->val);	/*copy funarg for call to funcall */
	lbot = np -1;
	first = np;
	last = np += count;
	for(index = 0; index < count; index++) {
		temp =(namptr++)->val; 
		if (TYPE (temp ) != DTPR && temp!=nil) 
			error ( "bad list argument to map",FALSE);
		lists[index] = temp;
	}
	for(;;) {
		for(namptr=first,index=0; index<count; index++) {
			temp = lists[index];
			if(temp==nil) goto done;

			if(maptyp==0) (namptr++)->val = temp->d.car;
			else (namptr++)->val = temp;

			lists[index] = temp->d.cdr;
		}
		if (join == 0) {
			current->l = newdot();
			current->l->d.car = Lfuncal();
			current = (lispval) &current->l->d.cdr;
		} else {
			current->l = Lfuncal();
			if ( TYPE ( current -> l) != DTPR && current->l != nil)
				error("bad type returned from funcall inside map",FALSE);
			else  while ( current -> l  != nil )
					current = (lispval) & (current ->l ->d.cdr);
		}
		np = last;
	}
done:	if (join == 0)current->l = nil;
	Restorestack();
	return(result->l);
}

/* ============================
-
- Lmapcar
- =============================*/

lispval
Lmpcar()
{
	return(Lmapcrx(0,0)); 	/* call general routine */
}


/* ============================
-
-
-  Lmaplist
- ==============================*/

lispval
Lmaplist()
{
	return(Lmapcrx(1,0)); 	/* call general routine */
}


/* ================================================
- mapping functions which return the value of the last function application.
- mapc and map
- ===================================================*/

lispval
Lmapcx(maptyp)
int maptyp;		/* 0= mapc   , 1= map  */
{
	register struct argent *namptr;
	register index;
	register lispval temp;
	register lispval result;

	int count;
	struct argent *first;
	lispval lists[25], errorh();
	Savestack(4);
	
	namptr = lbot + 1;
	count = np - namptr;
	if(count <= 0) return(nil);
	result = lbot[1].val;		/*This is what macsyma wants so ... */
					/*copy funarg for call to funcall */
	lbot = np; protect((namptr - 1)->val);
	first = np; np += count;

	for(index = 0; index < count; index++) {
		temp = (namptr++)->val;
		while(temp!=nil && TYPE(temp)!=DTPR)
			temp = errorh1(Vermisc,"Inappropriate list argument to mapc",nil,TRUE,0,temp);
		lists[index] = temp;
	}
	for(;;) {
		for(namptr=first,index=0; index<count; index++) {
			temp = lists[index];
			if(temp==nil)
				goto done;
			if(maptyp==0)
				(namptr++)->val = temp->d.car;
			else
				(namptr++)->val = temp;
			lists[index] = temp->d.cdr;
		}
		Lfuncal();
	}
done:	
	Restorestack();
	return(result);
}


/* ==================================
-
-	mapc   map the car of the lists
-
- ==================================*/

lispval
Lmapc()
{	return( Lmapcx(0) );  }


/* =================================
-
-	map    map the cdr of the lists
-
- ===================================*/

lispval
Lmap()
{	return( Lmapcx(1) );   }


lispval
Lmapcan()
{ 
	lispval Lmapcrx();

	return ( Lmapcrx ( 0,1 ) ); 
} 

lispval
Lmapcon()
{ 
	lispval Lmapcrx();

	return ( Lmapcrx ( 1,1 ) ); 
}
