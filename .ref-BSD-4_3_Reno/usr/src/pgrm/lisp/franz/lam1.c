#ifndef lint
static char *rcsid =
   "$Header: lam1.c,v 1.8 87/12/14 18:39:12 sklower Exp $";
#endif

/*					-[Fri Feb 17 16:44:24 1984 by layer]-
 * 	lam1.c				$Locker:  $
 * lambda functions
 *
 * (c) copyright 1982, Regents of the University of California
 */

# include "global.h"
# include <sgtty.h>
# include "chkrtab.h"
# include "frame.h"

lispval
Leval()
{
	register lispval temp;

	chkarg(1,"eval");
	temp = lbot->val;
	    return(eval(temp));
}

lispval
Lxcar()
{	register int typ;
	register lispval temp, result;

	chkarg(1,"xcar");
	temp = lbot->val;
	if (((typ = TYPE(temp)) == DTPR) || (typ == ATOM) || HUNKP(temp))
	    return(temp->d.car);
	else if(typ == SDOT) {
		result = inewint(temp->i);
		return(result);
	} else if(Schainp!=nil && typ==ATOM)
		return(nil);
	else
		return(error("Bad arg to car",FALSE));

}

lispval
Lxcdr()
{	register int typ;
	register lispval temp;

	chkarg(1,"xcdr");
	temp = lbot->val;
	if(temp==nil) return (nil);

	if (((typ = TYPE(temp)) == DTPR) || HUNKP(temp)) 
	    return(temp->d.cdr);
	else if(typ==SDOT) {
		if(temp->s.CDR==0) return(nil);
		temp = temp->s.CDR;
		if(TYPE(temp)==DTPR)
		    errorh1(Vermisc,"Fell off the end of a bignum",nil,FALSE,5,lbot->val);
		return(temp);
	} else if(Schainp!=nil && typ==ATOM)
		return(nil);
	else
		return(error("Bad arg to cdr", FALSE));
}

lispval
cxxr(as,ds)
register int as,ds;
{

	register lispval temp, temp2;
	int i, typ;
	lispval errorh();

	chkarg(1,"c{ad}+r");
	temp = lbot->val;

	for( i=0 ; i<ds ; i++)
	{
	    if( temp != nil)
	    {
		typ = TYPE(temp);
		if ((typ == DTPR) || HUNKP(temp))
		    temp = temp->d.cdr;
		else
		    if(typ==SDOT)
		    {
			if(temp->s.CDR==0)
			    temp = nil;
			else
			    temp = temp->s.CDR;
			if(TYPE(temp)==DTPR)
		    	    errorh1(Vermisc,"Fell off the end of a bignum",nil,FALSE,5,lbot->val);
		    }
		else
		    if(Schainp!=nil && typ==ATOM)
			return(nil);
		else
		    return(errorh1(Vermisc,"Bad arg to cdr",nil,FALSE,5,temp));
	    }
	}

	for( i=0 ; i<as ; i++)
	{
	    if( temp != nil )
	    {
		typ = TYPE(temp);
		if ((typ == DTPR) || HUNKP(temp))
		    temp = temp->d.car;
		else if(typ == SDOT)
			temp2 = inewint(temp->i), temp = temp2;
		else if(Schainp!=nil && typ==ATOM)
		    return(nil);
		else
		    return(errorh1(Vermisc,"Bad arg to car",nil,FALSE,5,temp));
	    }
	}

	return(temp);
}

lispval
Lcar()
{	return(cxxr(1,0)); }

lispval
Lcdr()
{	return(cxxr(0,1)); }

lispval
Lcadr()
{	return(cxxr(1,1)); }

lispval
Lcaar()
{	return(cxxr(2,0)); }

lispval
Lc02r()
{	return(cxxr(0,2)); }	/* cddr */

lispval
Lc12r()
{	return(cxxr(1,2)); }	/* caddr */

lispval
Lc03r()
{	return(cxxr(0,3)); }	/* cdddr */

lispval
Lc13r()
{	return(cxxr(1,3)); }	/* cadddr */

lispval
Lc04r()
{	return(cxxr(0,4)); }	/* cddddr */

lispval
Lc14r()
{	return(cxxr(1,4)); }	/* caddddr */

/*
 *  
 *	(nthelem num list)
 *
 * Returns the num'th element of the list, by doing a caddddd...ddr
 * where there are num-1 d's. If num<=0 or greater than the length of
 * the list, we return nil.
 *
 */

lispval
Lnthelem()
{
	register lispval temp;
	register int i;

	chkarg(2,"nthelem");

	if( TYPE(temp = lbot->val) != INT)
	return (error ("First arg to nthelem must be a fixnum",FALSE));

	i = temp->i;	/* pick up the first arg */

	if( i <= 0) return(nil);

	++lbot;			/* fix lbot for call to cxxr() 'cadddd..r' */
	temp = cxxr(1,i-1);
	--lbot;

	return(temp);
}

lispval
Lscons()
{
	register struct argent *argp = lbot;
	register lispval retp, handy;

	chkarg(2,"scons");
	retp = newsdot();
	handy = (argp) -> val;
	if(TYPE(handy)!=INT)
		error("First arg to scons must be an int.",FALSE);
	retp->s.I = handy->i;
	handy = (argp+1)->val;
	if(handy==nil)
		retp->s.CDR = (lispval) 0;
	else {
		if(TYPE(handy)!=SDOT)
		    error("Currently you may only link sdots to sdots.",FALSE);
		retp->s.CDR = handy;
	}
	return(retp);
}

lispval
Lbigtol(){
	register lispval handy,newp;

	chkarg(1,"Bignum-to-list");
	handy = lbot->val;
	while(TYPE(handy)!=SDOT)
		handy = errorh1(Vermisc,
				"Non bignum argument to Bignum-to-list",
				nil,TRUE,5755,handy);
	protect(newp = newdot());
	while(handy) {
		newp->d.car = inewint((long)handy->s.I);
		if(handy->s.CDR==(lispval) 0) break;
		newp->d.cdr = newdot();
		newp = newp->d.cdr;
		handy = handy->s.CDR;
	}
	handy = (--np)->val;
	return(handy);
}

lispval
Lcons()
{
	register lispval retp;
	register struct argent *argp;

	chkarg(2,"cons");
	retp = newdot();
	retp->d.car = ((argp = lbot) -> val);
	retp->d.cdr = argp[1].val;
	return(retp);
}
#define CA 0
#define CD 1

lispval
rpla(what)
int what;
{	register struct argent *argp;
	register int typ; register lispval first, second;

	chkarg(2,"rplac[ad]");
	argp = np-1;
	first = (argp-1)->val;
	while(first==nil)
		first = error("Attempt to rplac[ad] nil.",TRUE);
	second = argp->val;
	if (((typ = TYPE(first)) == DTPR) || (typ == ATOM) || HUNKP(first)) {
		if (what == CA)
			first->d.car = second;
		else 
			first->d.cdr = second;
		return(first);
	}
	if (typ==SDOT) {
		if(what == CA) {
			typ = TYPE(second);
			if(typ!=INT) error("Rplacca of a bignum will only replace INTS",FALSE);
			first->s.I = second->i;
		} else {
			if(second==nil)
				first->s.CDR = (lispval) 0;
			else
				first->s.CDR = second;
		}
		return(first);
	}
	return(error("Bad arg to rpla",FALSE));
}
lispval
Lrplca()
{	return(rpla(CA));	}

lispval
Lrplcd()
{	return(rpla(CD));	}


lispval
Leq()
{
	register struct argent *mynp = lbot + AD;

	chkarg(2,"eq");
	if(mynp->val==(mynp+1)->val) return(tatom);
	return(nil);
}



lispval
Lnull()
{	chkarg(1,"null");
	return ((lbot->val == nil) ? tatom : nil);
}



/* Lreturn **************************************************************/
/* Returns the first argument - which is nill if not specified.		*/

lispval
Lreturn()
{
	if(lbot==np) protect (nil);
	Inonlocalgo(C_RET,lbot->val,nil);
	/* NOT REACHED */
}


lispval
Linfile()
{
	FILE *port;
	register lispval name;

	chkarg(1,"infile");
	name = lbot->val;
loop:
	name = verify(name,"infile: file name must be atom or string");
	/* return nil if file couldnt be opened
	if ((port = fopen((char *)name,"r")) == NULL) return(nil); */	

	if ((port = fopen((char *)name,"r")) == NULL) {
		name = errorh1(Vermisc,"Unable to open file for reading.",nil,TRUE,31,name);
		goto loop;
	}
	ioname[PN(port)] = (lispval) inewstr((char *)name); /* remember name */
	return(P(port));
}

/* outfile - open a file for writing.  
 * 27feb81 [jkf] - modifed to accept two arguments, the second one being a
 *   string or atom, which if it begins with an `a' tells outfile to open the
 *   file in append mode
 */
lispval
Loutfile()
{
	FILE *port; register lispval name;
	char *mode ="w";    /* mode is w for create new file, a for append */
	char *given;

	if(lbot+1== np) protect(nil);
	chkarg(2,"outfile");
	name = lbot->val;
	given = (char *)verify((lbot+1)->val,"Illegal file open mode.");
	if(*given == 'a') mode = "a";
loop:
	name = verify(name,"Please supply atom or string name for port.");
#ifdef	os_vms
	/*
	 *	If "w" mode, open it as a "txt" file for convenience in VMS
	 */
	if (strcmp(mode,"w") == 0) {
		int fd;

		if ((fd = creat(name,0777,"txt")) < 0) {
			name = errorh1(Vermisc,"Unable to open file for writing.",nil,TRUE,31,name);
			goto loop;
		}
		port = fdopen(fd,mode);
	} else
#endif
	if ((port = fopen((char *)name,mode)) == NULL) {
		name = errorh1(Vermisc,"Unable to open file for writing.",nil,TRUE,31,name);
		goto loop;
	}
	ioname[PN(port)] = (lispval) inewstr((char *)name);
	return(P(port));
}

lispval
Lterpr()
{
	register lispval handy;
	FILE *port;

	if(lbot==np) handy = nil;
	else 
	{ 
	    chkarg(1,"terpr");
	    handy = lbot->val;
	}

	port = okport(handy,okport(Vpoport->a.clb,stdout));
	putc('\n',port);
	fflush(port);
	return(nil);
}

lispval
Lclose()
{
	lispval port;

	chkarg(1,"close");
	port = lbot->val;
	if((TYPE(port))==PORT) {
		fclose(port->p);
		ioname[PN(port->p)] = nil;
		return(tatom);
	}
	errorh1(Vermisc,"close:Non-port",nil,FALSE,987,port);
	/* not reached */
}

lispval
Ltruename()
{
    chkarg(1,"truename");
    if(TYPE(lbot->val) != PORT)
    	errorh1(Vermisc,"truename: non port argument",nil,FALSE,0,lbot->val);

    return(ioname[PN(lbot->val->p)]);
}

lispval
Lnwritn()
{
	register FILE *port;
	register value;
	register lispval handy;

	if(lbot==np) handy = nil;
	else 
	{
	    chkarg(1,"nwritn");
	    handy = lbot->val;
	}

	port = okport(handy,okport(Vpoport->a.clb,stdout));
	value = port->_ptr - port->_base;
	return(inewint(value));
}

lispval
Ldrain()
{
	register FILE *port;
	register int iodes;
	register lispval handy;
	struct sgttyb arg;

	if(lbot==np) handy = nil;
	else 
	{
	    chkarg(1,"nwritn");
	    handy = lbot->val;
	}
	port = okport(handy, okport(Vpoport->a.clb,stdout));
	if(port->_flag & _IOWRT) {
		fflush(port);
		return(nil);
	}
	if(! port->_flag & _IOREAD) return(nil);
	port->_cnt = 0;
	port->_ptr = port->_base;
	iodes = fileno(port);
	if(gtty(iodes,&arg) != -1) stty(iodes,&arg);
	return(P(port));
}

lispval
Llist()
{
	/* added for the benefit of mapping functions. */
	register struct argent *ulim, *namptr;
	register lispval temp, result;
	Savestack(4);

	ulim = np;
	namptr = lbot + AD;
	temp = result = (lispval) np;
	protect(nil);
	for(; namptr < ulim;) {
		temp = temp->l = newdot();
		temp->d.car = (namptr++)->val;
	}
	temp->l = nil;
	Restorestack();
	return(result->l);
}

lispval
Lnumberp()
{
	chkarg(1,"numberp");
	switch(TYPE(lbot->val)) {
	case INT: case DOUB: case SDOT:
		return(tatom);
	}
	return(nil);
}

lispval
Latom()
{
	register struct argent *lb = lbot;
	chkarg(1,"atom");
	if(TYPE(lb->val)==DTPR || (HUNKP(lb->val)))
		return(nil);
	else
		return(tatom);
}

lispval
Ltype()
{
	chkarg(1,"type");
	switch(TYPE(lbot->val)) {
	case INT:
		return(int_name);
	case ATOM:
		return(atom_name);
	case SDOT:
		return(sdot_name);
	case DOUB:
		return(doub_name);
	case DTPR:
		return(dtpr_name);
	case STRNG:
		return(str_name);
	case ARRAY:
		return(array_name);
	case BCD:
		return(funct_name);
	case OTHER:
		return(other_name);

	case HUNK2:
		return(hunk_name[0]);
	case HUNK4:
		return(hunk_name[1]);
	case HUNK8:
		return(hunk_name[2]);
	case HUNK16:
		return(hunk_name[3]);
	case HUNK32:
		return(hunk_name[4]);
	case HUNK64:
		return(hunk_name[5]);
	case HUNK128:
		return(hunk_name[6]);
		
	case VECTOR:
		return(vect_name);
	case VECTORI:
		return(vecti_name);

	case VALUE:
		return(val_name);
	case PORT:
		return(port_name);
	}
	return(nil);
}

lispval
Ldtpr()
{
	chkarg(1,"dtpr");
	return(typred(DTPR, lbot->val));
}

lispval
Lbcdp()
{
	chkarg(1,"bcdp");
	return(typred(BCD, lbot->val));
}

lispval
Lportp()
{
	chkarg(1,"portp");
	return(typred(PORT, lbot->val));
}

lispval
Larrayp()
{
	chkarg(1,"arrayp");
	return(typred(ARRAY, lbot->val));
}

/*
 *	(hunkp 'g_arg1)
 * Returns t if g_arg1 is a hunk, otherwise returns nil.
 */

lispval
Lhunkp()
{
	chkarg(1,"hunkp");
	if (HUNKP(lbot->val))
		return(tatom);		/* If a hunk, return t */
	else
		return(nil);		/* else nil */
}

lispval
Lset()
{
	lispval varble;

	chkarg(2,"set");
	varble = lbot->val;
	switch(TYPE(varble))
		{
	case ATOM:	return(varble->a.clb = lbot[1].val);

	case VALUE:	return(varble->l = lbot[1].val);
		}

	error("IMPROPER USE OF SET",FALSE);
	/* NOTREACHED */
}

lispval
Lequal()
{
	register lispval first, second;
	register type1, type2;
	lispval Lsub(),Lzerop();
	long *oldsp;
	Keepxs();
	chkarg(2,"equal");


	if(lbot->val==lbot[1].val) return(tatom);

	oldsp=sp(); stack((long)lbot->val);stack((long)lbot[1].val);
	for(;oldsp > sp();) {

	    first = (lispval) unstack(); second = (lispval) unstack();
    again:
	    if(first==second) continue;

	    type1=TYPE(first); type2=TYPE(second);
	    if(type1!=type2) {
		if((type1==SDOT&&type2==INT)||(type1==INT&&type2==SDOT))
		    goto dosub;
		{Freexs(); return(nil);}
	    }
	    switch(type1) {
	    case DTPR:
		stack((long)first->d.cdr); stack((long)second->d.cdr);
		first = first->d.car; second = second->d.car;
		goto again;
	    case DOUB:
		if(first->r!=second->r)
		    {Freexs(); return(nil);}
		continue;
	    case INT:
		if(first->i!=second->i)
		    {Freexs(); return(nil);}
		continue;
	    case VECTOR:
	        if(!vecequal(first,second)) {Freexs(); return(nil);}
		continue;
	    case VECTORI:
	    	if(!veciequal(first,second)) {Freexs(); return(nil);}
		continue;
    dosub:
	    case SDOT: {
		lispval temp;
		struct argent *OLDlbot = lbot;
		lbot = np;
		np++->val = first;
		np++->val = second;
		temp = Lsub();
		np = lbot;
		lbot = OLDlbot;
		if(TYPE(temp)!=INT || temp->i!=0)
		    {Freexs(); return(nil);}
		}
		continue;
	    case VALUE:
		if(first->l!=second->l)
		    {Freexs(); return(nil);}
		continue;
	    case STRNG:
		if(strcmp((char *)first,(char *)second)!=0)
		    {Freexs(); return(nil);}
		continue;

	    default:
		{Freexs(); return(nil);}
	    }
	}
	{Freexs(); return(tatom);}
}
lispval
oLequal()
{
	chkarg(2,"equal");

	if( lbot[1].val == lbot->val ) return(tatom);
	if(Iequal(lbot[1].val,lbot->val)) return(tatom); else return(nil);
}

Iequal(first,second) 
register lispval first, second;
{
	register type1, type2;
	lispval Lsub(),Lzerop();

	if(first==second)
		return(1);
	type1=TYPE(first);
	type2=TYPE(second);
	if(type1!=type2) {
		if((type1==SDOT&&type2==INT)||(type1==INT&&type2==SDOT))
			goto dosub;
		return(0);
	}
	switch(type1) {
	case DTPR:
		 return(
			Iequal(first->d.car,second->d.car) &&
			Iequal(first->d.cdr,second->d.cdr) );
	case DOUB:
		return(first->r==second->r);
	case INT:
		return( (first->i==second->i));
dosub:
	case SDOT:
	{
		lispval temp;
		struct argent *OLDlbot = lbot;
		lbot = np;
		np++->val = first;
		np++->val = second;
		temp = Lsub();
		np = lbot;
		lbot = OLDlbot;
		return(TYPE(temp)==INT&& temp->i==0);
	}
	case VALUE:
		return( first->l==second->l );
	case STRNG:
		return(strcmp((char *)first,(char *)second)==0);
	}
	return(0);
}
lispval
Zequal()
{
	register lispval first, second;
	register type1, type2;
	lispval Lsub(),Lzerop();
	long *oldsp;
	Keepxs();
	chkarg(2,"equal");


	if(lbot->val==lbot[1].val) return(tatom);

	oldsp=sp(); stack((long)lbot->val);stack((long)lbot[1].val);

	for(;oldsp > sp();) {

	    first = (lispval) unstack(); second = (lispval) unstack();
    again:
	    if(first==second) continue;

	    type1=TYPE(first); type2=TYPE(second);
	    if(type1!=type2) {
		if((type1==SDOT&&type2==INT)||(type1==INT&&type2==SDOT))
		    goto dosub;
		{Freexs(); return(nil);}
	    }
	    switch(type1) {
	    case DTPR:
		stack((long)first->d.cdr); stack((long)second->d.cdr);
		first = first->d.car; second = second->d.car;
		goto again;
	    case DOUB:
		if(first->r!=second->r)
		    {Freexs(); return(nil);}
		continue;
	    case INT:
		if(first->i!=second->i)
		    {Freexs(); return(nil);}
		continue;
    dosub:
	    case SDOT:
	    {
		lispval temp;
		struct argent *OLDlbot = lbot;
		lbot = np;
		np++->val = first;
		np++->val = second;
		temp = Lsub();
		np = lbot;
		lbot = OLDlbot;
		if(TYPE(temp)!=INT || temp->i!=0)
		    {Freexs(); return(nil);}
	    }
		continue;
	    case VALUE:
		if(first->l!=second->l)
		    {Freexs(); return(nil);}
		continue;
	    case STRNG:
		if(strcmp((char *)first,(char *)second)!=0)
		    {Freexs(); return(nil);}
		continue;
	    }
	}
	{Freexs(); return(tatom);}
}

/*
 * (print 'expression ['port]) prints the given expression to the given
 * port or poport if no port is given.  The amount of structure
 * printed is a function of global lisp variables plevel and
 * plength.
 */
lispval
Lprint()
{
	register lispval handy;
	extern int plevel,plength;


	handy = nil;			/* port is optional, default nil */
	switch(np-lbot) 
	{
	    case 2: handy = lbot[1].val;
	    case 1: break;
	    default: argerr("print");
	}

	chkrtab(Vreadtable->a.clb);
	if(TYPE(Vprinlevel->a.clb) == INT)
	{ 
	   plevel = Vprinlevel->a.clb->i;
	}
	else plevel = -1;
	if(TYPE(Vprinlength->a.clb) == INT)
	{
	    plength = Vprinlength->a.clb->i;
	}
	else plength = -1;
	printr(lbot->val,okport(handy,okport(Vpoport->a.clb,poport)));
	return(nil);
}

/* patom does not use plevel or plength 
 *
 * form is (patom 'value ['port])
 */
lispval
Lpatom()
{
	register lispval temp;
	register lispval handy;
	register int typ;
	FILE *port;

	handy = nil;			/* port is optional, default nil */
	switch(np-lbot) 
	{
	    case 2: handy = lbot[1].val;
	    case 1: break;
	    default: argerr("patom");
	}

	temp = Vreadtable->a.clb;
	chkrtab(temp);
	port = okport(handy, okport(Vpoport->a.clb,stdout));
	if ((typ= TYPE((temp = (lbot)->val))) == ATOM)
		fputs(temp->a.pname, port);
	else if(typ == STRNG)
		fputs((char *)temp,port);
	else
	{
	    	if(TYPE(Vprinlevel->a.clb) == INT)
		{
		    plevel = Vprinlevel->a.clb->i;
		}
		else plevel = -1;
		if(TYPE(Vprinlength->a.clb) == INT)
		{
		    plength = Vprinlength->a.clb->i;
		}
		else plength = -1;

		printr(temp, port);
	}
	return(temp);
}

/*
 * (pntlen thing) returns the length it takes to print out
 * an atom or number.
 */

lispval
Lpntlen()
{
	return(inewint((long)Ipntlen()));
}
Ipntlen()
{
	register lispval temp;
	register char *handy;

	temp = np[-1].val;
loop:	switch(TYPE(temp)) {

	case ATOM:
		handy = temp->a.pname;
		break;

	case STRNG:
		handy = (char *) temp;
		break;

	case INT:
		sprintf(strbuf,"%d",temp->i);
		handy =strbuf;
		break;

	case DOUB:
		sprintf(strbuf,"%g",temp->r);
		handy =strbuf;
		break;

	default:
		temp = error("Non atom or number to pntlen\n",TRUE);
		goto loop;
	}

	return( strlen(handy));
}
#undef okport
FILE *
okport(arg,proper) 
lispval arg;
FILE *proper;
{
	if(TYPE(arg)!=PORT)
		return(proper);
	else
		return(arg->p);
}
