static char *sccsid = "@(#)lam8.c	35.7 7/8/81";

#include "global.h"
#include <sys/types.h>
#include <pagsiz.h>
#include "naout.h"
#include "frame.h"

/* various functions from the c math library */
double sin(),cos(),asin(),acos(),atan2(),sqrt(), log(), exp();
extern int current;

lispval Imath(func)
double (*func)();
{
	register lispval handy;
	register double res;
	chkarg(1,"Math functions");

	switch(TYPE(handy=lbot->val)) {
	 case INT: res = func((double)handy->i); 
		   break;

	 case DOUB: res = func(handy->r);
		   break;

	 default:  error("Non fixnum or flonum to math function",FALSE);
	}
	handy = newdoub();
	handy->r = res;
	return(handy);
}
lispval Lsin()
{
	return(Imath(sin));
}

lispval Lcos()
{
	return(Imath(cos));
}

lispval Lasin()
{
	return(Imath(asin));
}

lispval Lacos()
{
	return(Imath(acos));
}

lispval Lsqrt()
{
	return(Imath(sqrt));
}
lispval Lexp()
{
	return(Imath(exp));
}

lispval Llog()
{
	return(Imath(log));
}

/* although we call this atan, it is really atan2 to the c-world,
   that is, it takes two args
 */
lispval Latan()
{
	register lispval arg;
	register double arg1v;
	register double res;
	chkarg(2,"arctan");

	switch(TYPE(arg=lbot->val)) {

	case INT:  arg1v = (double) arg->i;
		   break;

	case DOUB: arg1v = arg->r;
		   break;

	default:   error("Non fixnum or flonum arg to atan2",FALSE);
	}

	switch(TYPE(arg = (lbot+1)->val)) {

	case INT: res = atan2(arg1v,(double) arg->i);
		  break;

	case DOUB: res = atan2(arg1v, arg->r);
		  break;

	default:  error("Non fixnum or flonum to atan2",FALSE);
	}
	arg = newdoub();
	arg->r = res;
	return(arg);
}

/* (random) returns a fixnum in the range -2**30 to 2**30 -1
   (random fixnum) returns a fixnum in the range 0 to fixnum-1
 */
lispval
Lrandom()
{
	register int curval;
	float pow();

	curval = rand();	/* get numb from 0 to 2**31-1 */

	if(np==lbot) return(inewint(curval-(int)pow((double)2,(double)30)));

	if((TYPE(lbot->val) != INT)
	    || (lbot->val->i <= 0)) errorh(Vermisc,"random: non fixnum arg:",
						 nil, FALSE, 0, lbot->val);

	return(inewint(curval % lbot->val->i )); 

}
lispval
Lmakunb()
{
	register lispval work;

	chkarg(1,"makunbound");
	work = lbot->val;
	if(work==nil || (TYPE(work)!=ATOM))
		return(work);
	work->a.clb = CNIL;
	return(work);
}
lispval
Lpolyev()
{
	register int count; 
	register double *handy, *base;
	register struct argent *argp;
	lispval result; int type;

	count = 2 * (((int) np) - (int) lbot);
	if(count == 0) 
		return(inewint(0));
	if(count == 8)
		return(lbot->val);
	base = handy = (double *) alloca(count);
	for(argp = lbot; argp < np; argp++) {
		while((type = TYPE(argp->val))!=DOUB && type!=INT)
			argp->val = (lispval) errorh(Vermisc,"%%machine-polyev:non-real arg",nil,TRUE,73,lbot,argp->val);
		if(TYPE(argp->val)==INT) {
			*handy++ = argp->val->i;
		} else
			*handy++ = argp->val->r;
	}
	count = count/sizeof(double) - 2;
	asm("polyd	(r9),r11,8(r9)");
	asm("movd	r0,(r9)");
	result = newdoub();
	result->r = *base;
	return(result);
}
typedef struct doub {
	unsigned short f1:7,expt:8,sign:1;
	unsigned short f2,f3p1:14,f3p2:2,f4;
} *dp;

typedef struct quad2 {
	unsigned long g4:16,g3p1:14;
} *qp2;

typedef struct quad1 {
	unsigned long g3p2:2,g2:16,g1:7,hide:1;
} *qp1;

static long workbuf[2];
static int  exponent;
static Idebig()
{
	register lispval work;
	register dp rdp;
	register qp1 rqp1;
	register qp2 rqp2;
	register struct argent *lbot,np;
	workbuf[1]  = workbuf[0] = 0;

	work = lbot->val;		/* Unfold mantissa */
	rqp2 = (qp2) workbuf + 1;
	rqp1 = (qp1) workbuf;
	rdp = (dp) work;
	rqp2->g4 = rdp->f4;
	rqp2->g3p1 = rdp->f3p1;
	rqp1->g3p2 = rdp->f3p2;
	rqp1->g2 = rdp->f2;
	rqp1->g1 = rdp->f1;
	rqp1->hide = 1;
	if(rdp->sign) {
		workbuf[0] = (- workbuf[0]);
		if(workbuf[1] = (- workbuf[1]) & 0xC0000000)
			workbuf[0]--;
	}
	/* calcuate exponent and adjustment */
	exponent = -129 - 55 + (int) rdp->expt;
}
lispval
Lfdecom()
{
	register lispval result, handy;
	register dum1,dum2;

	chkarg(1,"Decompose-float");
	while(TYPE(lbot->val)!=DOUB)
		lbot->val = error("Decompose-float: Non-real argument",TRUE);
	Idebig();
	np++->val = result = handy = newdot();
	handy->d.car = inewint(exponent);
	handy = handy->d.cdr = newdot();
	handy = handy->d.car = newsdot();
	handy->s.I = workbuf[1];
	handy = handy->s.CDR = newsdot();
	handy->s.I = workbuf[0];
}

lispval
Lfseek()
{
	register lispval result, handy;
	register dum1,dum2;

	FILE *f;
	long disk_addr, offset, whence;
	lispval retp;

	chkarg(3,"fseek");			/* Make sure there are three arguments*/

	f = lbot->val->p;		/* Get first argument into f */
	if (TYPE(lbot->val)!=PORT)	/* Check type of first */
		error("fseek: First argument must be a port.",FALSE);

	offset = lbot[1].val->i;	/* Get second argument */
	if (TYPE(lbot[1].val)!=INT)
		error("fseek: Second argument must be an integer.",FALSE);

	whence = lbot[2].val->i;	/* Get last arg	*/
	if (TYPE(lbot[2].val)!=INT)
		error("fseek: Third argument must be an integer.",FALSE);

	if (fseek(f, offset, whence) == -1)
		error("fseek: Illegal parameters.",FALSE);

	retp = inewint(ftell(f));

	return((lispval) retp);
}

/* function hashtabstat  : return list of number of members in  each bucket */
lispval Lhashst()
{
	register lispval handy,cur;
	register struct atom *pnt;
	int i,cnt;
	extern int hashtop;
	Savestack(3);

	handy = newdot();
	protect(handy);
	cur = handy;
	for(i = 0; i < hashtop; i++)
	{
	    pnt = hasht[i];
	    for(cnt = 0; pnt != (struct atom *) CNIL ; pnt=pnt->hshlnk , cnt++);
	    cur->d.cdr = newdot();
	    cur = cur->d.cdr;
	    cur->d.car = inewint(cnt);
	}
	cur->d.cdr = nil;
	Restorestack();
	return(handy->d.cdr);
}


/* Lctcherr
  this routine should only be called by the unwind protect simulation
  lisp code
  It is called after an unwind-protect frame has been entered and
  evalated and we want to get on with the error or throw
  We only handle the case where there are 0 to 2 extra arguments to the
  error call.
*/
lispval
Lctcherr()
{
	register lispval handy;
	lispval type,messg,valret,contuab,uniqid,datum1,datum2;

	chkarg(1,"I-throw-err");

	if(handy->d.car == tatom)
	{	/* continuing a throw */
		Inonlocalgo(C_THROW,
			    handy->d.cdr->d.car, 
			    handy->d.cdr->d.cdr->d.car);
		/* NOT REACHED */
	}

	/* decode the arg list */
	handy = handy->d.cdr;
	type = handy->d.car;
	handy = handy->d.cdr;
	messg = handy->d.car;
	handy = handy->d.cdr;
	valret = handy->d.car;
	handy = handy->d.cdr;
	contuab = handy->d.car;
	handy = handy->d.cdr;
	uniqid = handy->d.car;
	handy = handy->d.cdr;

	/* if not extra args */
	if(handy == nil)
	{
	  errorh(type,messg->a.pname,valret,contuab->i,uniqid->i);
	}
	datum1 = handy->d.car;
	handy = handy->d.cdr;

	/* if one extra arg */
	if(handy == nil)
	{
	  errorh(type,messg->a.pname,valret,contuab->i,uniqid->i,datum1);
	}

	/* if two or more extra args, just use first 2 */
	datum2 = handy->d.car;
	errorh(type,messg->a.pname,valret,contuab->i,uniqid->i,datum1,datum2);
}

/*
 *	(*makhunk '<fixnum>)
 *			  <fixnum>
 * Create a hunk of size 2       . <fixnum> must be between 0 and 6.
 *
 */

lispval
LMakhunk()
{
	register int hsize, hcntr;
	register lispval result;

	chkarg(1,"Makehunk");
	if (TYPE(lbot->val)==INT)
	{
		hsize = lbot->val->i;		/* size of hunk (0-6) */
		if ((hsize >= 0) && (hsize <= 6))
		{
			result = newhunk(hsize);
			hsize = 2 << hsize;	/* size of hunk (2-128) */
			for (hcntr = 0; hcntr < hsize; hcntr++)
				result->h.hunk[hcntr] = hunkfree;
		}
		else
			error("*makhunk: Illegal hunk size", FALSE);
	return(result);
	}
	else
		error("*makhunk: First arg must be an fixnum",FALSE);
}

/*
 *	(cxr '<fixnum> '<hunk>)
 * Returns the <fixnum>'th element of <hunk>
 *
 */
lispval
Lcxr()
{
	register lispval temp;

	chkarg(2,"cxr");
	if (TYPE(lbot->val)!=INT)
		error("cxr: First arg must be a fixnum", FALSE);
	else
	{
		if (! HUNKP(lbot[1].val))
			error("cxr: Second arg must be a hunk", FALSE);
		else
			if ( (lbot->val->i >= 0) &&
			     (lbot->val->i < (2 << HUNKSIZE(lbot[1].val))) )
			{
				temp = lbot[1].val->h.hunk[lbot->val->i];
				if (temp != hunkfree)
					return(temp);
				else
					error("cxr: Arg outside of hunk range",
					      FALSE);
			}
			else
				error("cxr: Arg outside of hunk range", FALSE);
	}
}

/*
 *	(rplacx '<fixnum> '<hunk> '<expr>)
 * Replaces the <fixnum>'th element of <hunk> with <expr>.
 *
 */
lispval
Lrplacx()
{
	lispval *handy;
	chkarg(3,"rplacx");
	if (TYPE(lbot->val)!=INT)
		error("rplacx: First arg must be a fixnum", FALSE);
	else
	{
		if (! HUNKP(lbot[1].val))
			error("rplacx: Second arg must be a hunk", FALSE);
		else
		{
			if ( (lbot->val->i >= 0) &&
			     (lbot->val->i < (2 << HUNKSIZE(lbot[1].val))) )
			{
			   if (*(handy = &(lbot[1].val->h.hunk[lbot->val->i]))
					!= hunkfree)
				    *handy  = lbot[2].val;
				else
					error("rplacx: Arg outside hunk range", FALSE);
			}
			else
				error("rplacx: Arg outside hunk range", FALSE);
		}
	}
	return(lbot[1].val);
}

/*
 *	(*rplacx '<fixnum> '<hunk> '<expr>)
 * Replaces the <fixnum>'th element of <hunk> with <expr>. This is the
 * same as (rplacx ...) except with this function you can replace EMPTY's.
 *
 */
lispval
Lstarrpx()
{
	chkarg(3,"*rplacx");
	if (TYPE(lbot->val)!=INT)
		error("*rplacx: First arg must be a fixnum", FALSE);
	else
	{
		if (! HUNKP(lbot[1].val))
			error("*rplacx: Second arg must be a hunk", FALSE);
		else
		{
			if ( (lbot->val->i >= 0) &&
			     (lbot->val->i < (2 << HUNKSIZE(lbot[1].val))) )
				lbot[1].val->h.hunk[lbot->val->i] = lbot[2].val;
			else
				error("*rplacx: Arg outside hunk range", FALSE);
		}
	}
	return(lbot[1].val);
}

/*
 *	(hunksize '<hunk>)
 * Returns the size of <hunk>
 *
 */
lispval
Lhunksize()
{
	register int size,i;

	chkarg(1,"hunksize");
	if (HUNKP(lbot->val))
	{
		size = 2 << HUNKSIZE(lbot->val);
		for (i = size-1; i >= 0; i--)
		{
			if (lbot->val->h.hunk[i] != hunkfree)
			{
				size = i + 1;
				break;
			}
		}
		return( inewint(size) );
	}
	else
		error("hunksize: First argument must me a hunk", FALSE);
}

/*
 *	(fileopen  filename mode)
 * open a file for read, write, or append the arguments can be either
 * strings or atoms.
 */
lispval
Lfileopen()
{
	FILE *port;
	register lispval name;
	register lispval mode;
	register char *namech;
	register char *modech;
	int typ;

	chkarg(2,"fileopen");
	name = lbot->val;
	mode = lbot[1].val;

	namech = (char *) verify(name,"fileopen:args must be atoms or strings");
	modech = (char *) verify(mode,"fileopen:args must be atoms or strings");

	while (modech[0] != 'r' && modech[0] != 'w' && modech[0] != 'a')
	{
		mode = errorh(Vermisc,"Modes are only r, w, a.",nil,TRUE,31,(char *) 0);
		modech = (char *) verify(mode,"fileopen:args must be atoms or strings");
	}

	while ((port = fopen(namech, modech)) == NULL)
	{
	    name = errorh(Vermisc,"Unable to open file.",nil,TRUE,31,name);
	    namech = (char *) verify(name,"fileopen:args must be atoms or strings");
	}
		    /* xports is a FILE *, cc complains about adding pointers */

	return( (lispval) (xports + (port - _iob)));
}

/*
 *	(*mod '<number> '<modulus>)
 * This function returns <number> mod <modulus> (for balanced modulus).
 * It is used in vaxima as a speed enhancement.
 */
lispval
LstarMod()
{
	register int mod_div_2, number, modulus;

	chkarg(2,"*mod");
	if ((TYPE(lbot->val) == INT) && (TYPE(lbot[1].val) == INT))
	{
		modulus = lbot[1].val->i;
		number = lbot->val->i % modulus;
		mod_div_2 = modulus / 2;
		if (number < 0)
		{
			if (number < (-mod_div_2))
				number += modulus;
		}
		else
		{
			if (number > mod_div_2)
				number -= modulus;
		}
		return( inewint(number) );
	}
	else
		error("*mod: Arguments must be fixnums", FALSE);
}
lispval
Llsh()
{
	register struct argent *mylbot = lbot;
	int val,shift;

	chkarg(2,"lsh");
	if((TYPE(mylbot->val) != INT) || (TYPE(mylbot[1].val) != INT))
		errorh(Vermisc,
		       "Non ints to lsh",
		       nil,FALSE,0,mylbot->val,mylbot[1].val);
	val = mylbot[0].val->i;
	shift = mylbot[1].val->i;
	if(shift < -32 || shift > 32)
	  return(inewint(0));
	val = val << shift;	/* do the shift */
	if((val < 0) && (shift < 0))
	{  	/* special case: the vax doesn't have a logical shift
		   instruction, so we must zero out the ones which
		   will propogate from the sign position
		*/
		return(inewint ( val & ~(0x80000000 << (shift+1))));
	}
	else return( inewint(val));
}

lispval
Lrot()
{
	register rot,val;		/* these must be the first registers */
	register struct argent *mylbot = lbot;

	chkarg(2,"rot");
	if((TYPE(mylbot->val) != INT) || (TYPE(mylbot[1].val) != INT))
		errorh(Vermisc,
		       "Non ints to rot",
		       nil,FALSE,0,mylbot->val,mylbot[1].val);
	val = mylbot[0].val->i;
	rot = mylbot[1].val->i;
	rot = rot % 32 ;	/* bring it down below one byte in size */
	asm(" rotl r11,r10,r10 ");  /* rotate val by rot and put back in val */
	return( inewint(val));
}

/*----------------- vms routines to simulate dumplisp -------------------- */
#ifdef VMS

extern char firstalloc[];
extern int lsbrkpnt;
extern char zfreespace[];
extern int end;
extern char *stabf;			/* Name of symbol table */
static char newstabf[100];		/* Buffer to hold new name */

#define roundup(a,b) (((a-1)|(b-1))+1)
lispval
Lsavelsp()
{
	char *filnm;
	int fp,fp1,i,num,start;
	char stabname[100], buf[5000];
	char *gstab();
	int strcmpn();

	chkarg(1,"savelisp");

	filnm = (char *) verify(lbot->val, "savelisp: non atom arg");
	if((fp=creat(filnm,0666)) < 0)
		errorh(Vermisc,"savelisp: can't open file",nil,FALSE,0,
					  lbot->val);
	start = roundup((int)firstalloc,PAGSIZ);
	num = roundup(((int)lsbrkpnt)-start,PAGSIZ);
	if((num = write(fp,start,num)) <= 0)
		error("savelisp: write failed ",FALSE);
	printf(" %x bytes written from %x to %x \n",num,start,start+num-1);
	close(fp);

	sprintf(buf,"%s.stb",gstab());
	sprintf(stabname,"%s.stb",filnm);
	if (!strcmpn(gstab(),"tmp:",4))
	  if ( link(buf,stabname) >= 0)
	    if (unlink(buf) >= 0) {
		strcpy(newstabf,filnm);
		stabf = newstabf;
		return(tatom);
	}
	fp1 = creat(stabname,0666,"var");
	fp = open(buf,0);
	while ( (i=read(fp,buf,5000)) > 0) write(fp1,buf,i);
	close(fp); close(fp1);
	if (!strcmpn(gstab(),"tmp:",4)) unlink(sprintf(buf,"%s.stb",gstab()));
	strcpy(newstabf,filnm);
	stabf = newstabf;
	return(tatom);
}

lispval
Lrestlsp()
{
	char *filnm;
	int fp,i,num,start;
	extern int xcycle;

	chkarg(1,"restorelisp");

	filnm = (char *) verify(lbot->val,"restorelisp: non atom arg");
	if((fp=open(filnm,0)) < 0)
		errorh(Vermisc,"restorelisp: can't open file",nil,FALSE,0,
					     lbot->val);

	strcpy(newstabf,filnm);		/* Mark the new symbol table */
	start = roundup((int)firstalloc,PAGSIZ);
	if((num = vread(fp,start,((int)&end)-start)) <= 0)
		error("restorelisp: read failed " ,FALSE);
	printf(" %x bytes read into %x to %x\n",num,start,start+num-1);
	xcycle = 0;	/* indicate no saved pages to xsbrk */
	close(fp);
	stabf = newstabf;		/* Make this our new symbol table */
	bnp = orgbnp;
	lbot = np = orgnp;
	Inonlocalgo(C_RESET,nil,nil);
}
#endif

/*----------------------------------------------------------- */


/* getaddress --
 *
 * (getaddress '|_entry1| 'fncname1 '|_entry2| 'fncname2 ...)
 *
 * binds value of symbol |_entry1| to function defition of atom fncname1, etc.
 *
 * returns fnc-binding of fncname1.
 *
 */

lispval
Lgetaddress(){
	register struct argent *mlbot = lbot;
	register lispval work;
	register int numberofargs, i;
	char *gstab();
	char ostabf[128];
	struct nlist NTABLE[100];
	lispval dispget();

	Savestack(4);

	if(np-lbot == 2) protect(nil);	/* allow 2 args */
	numberofargs = (np - lbot)/3;
	if(numberofargs * 3 != np-lbot)
	   error("getaddress: arguments must come in triples ",FALSE);

	for ( i=0; i<numberofargs; i++,mlbot += 3) {
		NTABLE[i].n_value = 0;
	        mlbot[0].val = verify(mlbot[0].val,"Incorrect entry specification for binding");
		NTABLE[i].n_un.n_name = (char *) mlbot[0].val;
		while(TYPE(mlbot[1].val) != ATOM)
			mlbot[1].val = errorh(Vermisc,
					"Bad associated atom name for binding",
					  nil,TRUE,0,mlbot[1].val);
		mlbot[2].val = dispget(mlbot[2].val,"getaddress: Incorrect discipline specification ",Vsubrou->a.pname);
	}
	NTABLE[(numberofargs)].n_un.n_name = "";
	strcpyn(ostabf,gstab(),128);
	if ( nlist(ostabf,NTABLE) == -1 ) {
	    errorh(Vermisc,"Getaddress: Bad file",nil,FALSE,0,inewstr(ostabf));
	} else 
	    for (i=0,mlbot=lbot+1; i<numberofargs; i++,mlbot+=3) {
		if ( NTABLE[i].n_value == 0 )
		    fprintf(stderr,"Undefined symbol: %s\n",
			      NTABLE[i].n_un.n_name);
		else {
		    work= newfunct();
		    work->bcd.entry = (lispval (*) ())NTABLE[i].n_value;
		    work->bcd.discipline = mlbot[1].val;
		    mlbot->val->a.fnbnd = work;
		}
	    };
	Restorestack();
	return(lbot[1].val->a.fnbnd);
};

/* very temporary function to test the validity of the bind stack */

bndchk()
{  
	register struct nament *npt;
	register lispval in2;

	in2 = inewint(200);
	for(npt=orgbnp; npt < bnp; npt++)
	{  if((int) npt->atm < (int) in2) asm(" halt ");
	}
}

/*
 *	formatted printer for lisp data
 *    use: (cprintf formatstring datum [port])
 */
lispval
Lcprintf()
{
    FILE *p;
    char *fstrng;
    lispval v;
    if(np-lbot == 2) protect(nil);	/* write to standard output port */
    chkarg(3,"cprintf");

    fstrng = (char *)verify(lbot->val,"cprintf: first arg not string or symbol");

    p = okport(lbot[2].val,okport(Vpoport->a.clb,poport));

    switch(TYPE(v=lbot[1].val)) {

	case INT:  fprintf(p,fstrng,v->i);
		   break;

	case DOUB: fprintf(p,fstrng,v->r);
		   break;

	case ATOM: fprintf(p,fstrng,v->a.pname);
		   break;

	case STRNG:fprintf(p,fstrng,v);
		   break;

	default:   error("cprintf: Illegal second argument",FALSE);
   };

   return(lbot[1].val);
}

lispval
Lprobef()
{
	char *name;
	chkarg(1,"probef");

	name = (char *)verify(lbot->val,"probef: not symbol or string arg ");

	if(access(name,0) == 0) return(tatom);
	else return(nil);
}

lispval
Lsubstring()
{	register char *name;
	register lispval index,length;
	int restofstring = FALSE;
	int len,ind,reallen;
	extern char strbuf[];

	switch (np-lbot) 
	{
	  case 2: restofstring = TRUE;
		  break;

	  case 3: break;

	  default: chkarg(3,"substring");
	}

	name = (char *)verify(lbot[0].val,"substring: not symbol or string arg ");

	while (TYPE(index = lbot[1].val) != INT)
	{  lbot[1].val = errorh(Vermisc,"substring: non integer index ",nil,
						    TRUE,0,index);
	}

	len = strlen(name);
	ind = index->i;

	if(ind < 0) ind = len+1 + ind;

	if(ind < 1 || ind > len) return(nil);	/*index out of bounds*/
	if(restofstring) return((lispval)inewstr(name+ind-1));

	while (TYPE(length = lbot[2].val) != INT)
	{ lbot[2].val = errorh(Vermisc,"substring: not integer length ",nil,
						   TRUE,0,length);
	}

	if((reallen = length->i ) < 0 || (reallen + ind) > len)
	  return((lispval)inewstr(name+ind-1));

	strncpy(strbuf,name+ind-1,reallen);
	strbuf[reallen] = '\0';
	return((lispval)newstr());
}

lispval
Lsubstringn()
{
	register char *name;
	register int len,ind,reallen;
	lispval index,length;
	int restofstring = FALSE;
	Savestack(4);

	if((np-lbot) == 2) restofstring = TRUE;
	else { chkarg(3,"substringn");}

	name = (char *) verify(lbot[0].val,"substringn: non symbol or string arg ");

	while (TYPE(index = lbot[1].val) != INT)
	{  lbot[1].val = errorh(Vermisc,"substringn: non integer index ",nil,
						    TRUE,0,index);
	}

	if(!restofstring)
	{
	    while (TYPE(length = lbot[2].val) != INT)
	    { lbot[2].val = errorh(Vermisc,"substringn: not integer length ",
							nil, TRUE,0,length);
	    }
	    reallen = length->i;
	}
	else reallen = -1;

	len = strlen(name);
	ind = index->i;
	if(ind < 0) ind = len + 1 + ind;
	if( ind < 1 || ind > len) return(nil);

	if(reallen == 0) 
	    return((lispval)inewint(*(name + ind - 1)));
	else {
	    char *pnt = name + ind - 1;
	    char *last = name + len -1;
	    lispval cur,start;

	    protect(cur = start = newdot());
	    cur->d.car = inewint(*pnt);
	    while(++pnt <= last && --reallen != 0)
	    {
	       cur->d.cdr = newdot();
	       cur = cur->d.cdr;
	       cur->d.car = inewint(*pnt);
	    }
	    Restorestack();
	    return(start);
	}

}

lispval Ipurcopy();


lispval
Lpurcopy()
{
	chkarg(1,"purcopy");
	return(Ipurcopy(lbot[0].val));
}
	    
lispval
Ipurcopy(handy)
lispval handy;
{
    extern int *beginsweep;
    register lispval retv, curv, lv;
    int i,size;

    switch(TYPE(handy)) {

	case DTPR:
		   retv = curv = pnewdot();
		   lv = handy;
		   while(TRUE)
		   {
		      curv->d.car = Ipurcopy(lv->d.car);
		      if(TYPE(lv = lv->d.cdr) == DTPR)
		      {
			  curv->d.cdr = pnewdot();
			  curv = curv->d.cdr;
		      }
		      else {
			  curv->d.cdr = Ipurcopy(lv);
			  break;
		      }
		    }
		    return(retv);

	case SDOT:
		    retv = curv = pnewsdot();
		    lv = handy;
		    while(TRUE)
		    {
			curv->s.I = lv->s.I;
			if(lv->s.CDR == nil) break;
			lv = lv->s.CDR;
			curv->s.CDR = pnewdot();
			curv = curv->s.CDR;
		    }
		    return(retv);

	case INT:
		    if((int *)handy < beginsweep) return(handy);
		    retv = pnewint();
		    retv->i = handy->i;
		    return(retv);

	case DOUB:
		    retv = pnewdoub();
		    retv->r = handy->r;
		    return(retv);

	case HUNK2:
		i = 0;
		goto hunkit;

	case HUNK4:
		i = 1;
		goto hunkit;

	case HUNK8:
		i = 2;
		goto hunkit;

	case HUNK16:
		i = 3;
		goto hunkit;

	case HUNK32:
		i = 4;
		goto hunkit;

	case HUNK64:
		i = 5;
		goto hunkit;

	case HUNK128:
		i = 6; 

	    hunkit:
		retv = pnewhunk(i);
		size = 2 << i ; /* number of elements to copy over */
		for( i = 0; i < size ; i++)
		{
		    retv->h.hunk[i] = Ipurcopy(handy->h.hunk[i]);
		}
		return(retv);



	case ATOM: 
	case STRNG:
	case BCD:
	case PORT:
	    return(handy);	/* We don't want to purcopy these, yet
				 * it won't hurt if we don't mark them
				 * since they either aren't swept or 
				 * will be marked in a special way 
				 */
	case ARRAY:
		error("purcopy: can't purcopy array structures",FALSE);

	default:
		error(" bad type to purcopy ",FALSE);
    }
}

/*
 * Lpurep returns t if the given arg is in pure space
 */
lispval
Lpurep()
{
    lispval Ipurep();

    chkarg(1,"purep");
    return(Ipurep(lbot->val));
}
