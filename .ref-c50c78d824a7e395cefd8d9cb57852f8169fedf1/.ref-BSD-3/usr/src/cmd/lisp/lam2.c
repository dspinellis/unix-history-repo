# include "global.h"
/*
 * (flatsize thing max) returns the smaller of max and the number of chars
 * required to print thing linearly.
 */
static flen; /*Internal to this module, used as a running counter of flatsize*/
static fmax; /*used for maximum for quick reference */

lispval
Lflatsi()
{
	register lispval current, temp;
	register struct argent *mylbot = lbot;
	snpand(3); /* fixup entry mask */

	chkarg(2);
	flen = 0; fmax = mylbot[1].val->i;
	current = mylbot->val;
	protect(nil); 			/*create space for argument to pntlen*/
	Iflatsi(current);
	return(inewint(flen));
}
/*
 * Iflatsi does the real work of the calculation for flatsize
 */
Iflatsi(current)
register lispval current;
{
	register lispval handy;
	register int temp;

	if(flen > fmax) return(fmax);
	switch(TYPE(current)) {

	patom:
	case INT: case ATOM: case DOUB:
		np[-1].val = current;
		flen += Ipntlen();
		return;
	
	pthing:
	case DTPR:
		flen++;
		Iflatsi(current->car);
		current = current->cdr;
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

#include "chars.h"

extern char *ctable;
/* r *********************************************************************/
/* this function maps the desired read 	function into the system-defined */
/* reading functions after testing for a legal port.			 */
lispval
r(op)
int op;
{
	register char c; register lispval result;
	int orlevel; extern int rlevel;
	FILE *ttemp;
	struct nament *oldbnp = bnp;
	snpand(2);

 	chkarg(2);
	result = Vreadtable->clb;
	orlevel = rlevel;
	rlevel = 0;
	ttemp = okport(Vpiport->clb,stdin);
	ttemp = okport(lbot->val,ttemp);
/*printf("entering switch\n");*/
	fflush(stdout);		/* flush any pending characters */

	switch (op)
	{
	case EADC:	rlevel = orlevel;
			switch (ctable[c = getc(ttemp)] & 0377)
			{
			case VEOF:
				return(lbot[1].val);
			default:
				strbuf[0] = hash = c;
				strbuf[1] = 0;
				atmlen = 2;
				return((lispval)getatom());
			}
	case ATOM:	rlevel = orlevel;
			result = (ratomr(ttemp));
			goto out;

	case EAD:	PUSHDOWN(Vpiport,P(ttemp)); /* rebind Vpiport */
			result = readr(ttemp);
	out:		if(result==eofa)
				result = lbot[1].val;
			rlevel = orlevel;
			popnames(oldbnp);	/* unwind bindings */
			return(result);
	}
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
	register char *p; register lispval ttemp, vtemp;
	register struct argent *lbot, *np;
	struct nament *oldbnp = bnp;
	int orlevel;
	char longname[100];
	char *shortname, *end2;

	chkarg(1);
	ttemp = lbot->val;
	if(TYPE(ttemp)!=ATOM) return(error("FILENAME MUST BE ATOMIC",FALSE));
	strcpy(longname,"/usr/lib/lisp/" );
	for(p = longname; *p; p++);
		shortname = p;
	strcpy(p,ttemp->pname);
	for(; *p; p++);
		end2 = p;
	strcpy(p,".l");
	if ((port = fopen(shortname,"r")) == NULL &&
		(port = fopen(longname, "r")) == NULL) {
			*end2 = 0;
			if ((port = fopen(shortname,"r")) == NULL &&
				(port = fopen(longname, "r")) == NULL)
					error("CAN'T OPEN FILE", FALSE);
	}
	orlevel = rlevel;
	rlevel = 0;

	if(ISNIL(copval(gcload,CNIL)) &&
		loading->clb != tatom &&
		ISNIL(copval(gcdis,CNIL)))
		gc(CNIL);	/*  do a gc if gc will be off  */

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
	register int atmlen; /* Passt auf!  atmlen in the external
				sense calculated by newstr	    */
	int i;
	lispval cur;
	snpand(2);

	atmlen = 0 ;	
	strbuf[0] = NULL_CHAR ;

	/* loop for each argument */
	for(temnp = lbot + AD ; temnp < np ; temnp++)
	{
	    cur = temnp->val;
      loop: switch(TYPE(cur))
	    {
	    case ATOM:
		 strcpy(&strbuf[atmlen], ((struct atom *) cur) -> pname) ;
		 break;

  	    case INT:
		 sprintf(&strbuf[atmlen],"%d",cur->i);
		 break;

	    case DOUB:
		 sprintf(&strbuf[atmlen],"%f",cur->f);
		 break;

	    default:
		 cur = error("Non atom or number to concat",TRUE);
		 goto loop;    /* if returns value, try it */
	   }
	   atmlen = strlen(strbuf);

	}

	if(unintern)
		return( (lispval) newatom());
	else
		return( (lispval) getatom()) ;
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
	register struct argent *argp = lbot;
	lispval Iputprop();
	snpand(1);
	chkarg(3);
	return(Iputprop(argp->val,argp[1].val,argp[2].val));
}

lispval
Iputprop(atm,prop,ind)
register lispval prop, ind, atm;
{
	register lispval pptr;
	lispval *tack;		/* place to begin property list */
	lispval errorh();
 top:
	switch (TYPE(atm)) {
	case ATOM:
		if(atm == nil) tack = &nilplist;
		else tack =  &(atm->plist);
		break;
	case DTPR:
		for (pptr = atm->cdr ; pptr != nil ; pptr = pptr->cdr->cdr)
		    if(TYPE(pptr) != DTPR || TYPE(pptr->cdr) != DTPR) break;
		if(pptr != nil) 
		{   atm = errorh(Vermisc,
				 "putprop: bad disembodied property list",
				 nil,TRUE,0,atm);
  		    goto top;
		}
		tack = (lispval *) &(atm->cdr);
		break;
	default:
		errorh(Vermisc,"putprop: Bad first argument: ",nil,FALSE,0,atm);
	}
	pptr = *tack;	/* start of property list */
	findit:
	for (pptr = *tack ; pptr != nil ; pptr = pptr->cdr->cdr)
		if (pptr->car == ind) {
			(pptr->cdr)->car = prop;
			return(prop);
		}
		else tack = &(pptr->cdr->cdr) ;
	*tack = pptr = newdot();
	pptr->car = ind;
	pptr = pptr->cdr = (lispval) newdot();
	pptr->car = prop;
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
	register lispval dum1, dum2;
	lispval Igetplist();
	snpand(2);

	chkarg(2);
	ind = lbot[1].val;
	atm = lbot[0].val;
top:
	switch(TYPE(atm)) {
	case ATOM:
		if(atm==nil) atm = nilplist;
		else atm = atm->plist;
		break;		

	case DTPR:
		for (dum1 = atm->cdr; dum1 != nil; dum1 = dum1->cdr->cdr)
		    if((TYPE(dum1) != DTPR) || 
		       (TYPE(dum1->cdr) != DTPR)) break; /* bad prop list */
		if(dum1 != nil) 
		{   atm = errorh(Vermisc,
				 "putprop: bad disembodied property list",
				 nil,TRUE,0,atm);
  		    goto top;
		}
		atm = atm -> cdr;
		break;
	default:
		/* remove since maclisp doesnt treat
		   this as an error, ugh
		   return(errorh(Vermisc,"get: bad first argument: ",
			       nil,FALSE,0,atm));
		 */
		 return(nil);
	}
	return(Igetplist(atm,ind));
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
		atm = atm->plist;
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
			if (pptr->car == ind)
				return ((pptr->cdr)->car);
			pptr = (pptr->cdr)->cdr;
		}
	return(nil);
}
lispval
Lgetd()
{
	register lispval typ;
	snpand(1);
	
	chkarg(1);
	typ = lbot->val;
	if (TYPE(typ) != ATOM) 
	   errorh(Vermisc,
		  "getd: ONLY ATOMS HAVE FUNCTION DEFINITIONS",
		  nil,
		  FALSE,
		  0,
		  typ);
	return(typ->fnbnd);
}
lispval
Lputd()
{
	register lispval atom, list;
	register lispval dum1, dum2;
	register struct argent *lbot, *np;
	snpand(2);
	
	chkarg(2);
	list = lbot[1].val;
	atom = lbot->val;
	if (TYPE(atom) != ATOM) error("ONLY ATOMS HAVE FUNCTION DEFINITIONS",FALSE);
	atom->fnbnd = list;
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
	register struct argent *lbot;
	register struct argent *np;

	struct argent *first, *last;
	int count;
	lispval lists[25], result;
	
	namptr = lbot + 1;
	count = np - namptr;
	if (count <= 0) return (nil);
	/*oldlbot = lbot;  		/* lbot saved by virtue of entry mask */
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

			if(maptyp==0) (namptr++)->val = temp->car;
			else (namptr++)->val = temp;

			lists[index] = temp->cdr;
		}
		if (join == 0) {
			current->l = newdot();
			current->l->car = Lfuncal();
			current = (lispval) &current->l->cdr;
		} else {
			current->l = Lfuncal();
			if ( TYPE ( current -> l) != DTPR && current->l != nil)
				error("bad type returned from funcall inside map",FALSE);
			else  while ( current -> l  != nil )
					current = (lispval) & (current ->l ->cdr);
		}
		np = last;
	}
done:	if (join == 0)current->l = nil;
	/*lbot = oldlbot;*/
	return(result->l);
}

/* ============================
-
- Lmapcar
- =============================*/

lispval
Lmapcar()
{
	snpand(0);
	return(Lmapcrx(0,0)); }	/* call general routine */


/* ============================
-
-
-  Lmaplist
- ==============================*/

lispval
Lmaplist()
{
	snpand(0);
	return(Lmapcrx(1,0)); }	/* call general routine */


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
	register struct argent *lbot;
	register struct argent *np;

	int count;
	struct argent *first;
	lispval lists[25], errorh();
	
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
			temp = errorh(Vermisc,"Inappropriate list argument to mapc",nil,TRUE,0,temp);
		lists[index] = temp;
	}
	for(;;) {
		for(namptr=first,index=0; index<count; index++) {
			temp = lists[index];
			if(temp==nil)
				goto done;
			if(maptyp==0)
				(namptr++)->val = temp->car;
			else
				(namptr++)->val = temp;
			lists[index] = temp->cdr;
		}
		Lfuncal();
	}
done:	
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
