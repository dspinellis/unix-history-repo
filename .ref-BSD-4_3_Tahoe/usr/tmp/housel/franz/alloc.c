#ifndef lint
static char *rcsid =
   "$Header: alloc.c,v 1.13 87/12/11 17:27:45 sklower Exp $";
#endif

/*
 * 	alloc.c				$Locker:  $
 * storage allocator and garbage collector
 *
 * (c) copyright 1982, Regents of the University of California
 */
 
# include "global.h"
# include "structs.h"

#include <sys/types.h>
#include <sys/times.h>
#ifdef METER
#include <sys/vtimes.h>
#endif
 
# define NUMWORDS TTSIZE * 128  /*  max number of words in P0 space  */
# define BITQUADS TTSIZE * 2	/*  length of bit map in quad words  */
# define BITLONGS TTSIZE * 4	/*  length of bit map in long words  */

# ifdef vax
# define ftstbit	asm("	ashl	$-2,r11,r3");\
			asm("	bbcs	r3,_bitmapi,1f");\
			asm("	ret"); \
			asm("1:");

/* setbit is a fast way of setting a bit, it is like ftstbit except it
 * always continues on to the next instruction
 */
# define setbit		asm("	ashl	$-2,r11,r0"); \
			asm("	bbcs	r0,_bitmapi,$0");
# endif

# if m_68k
# define ftstbit	{if(Itstbt()) return;}
# define setbit		Itstbt()
# endif

# ifdef tahoe
# define ftstbit	if( readbit(p) ) return; oksetbit;
# define setbit		{bitmapi[(int)p>>7] |= bitmsk[((int)p >> 2)&31];}
# define readbit(p)	((int)bitmapi[r=(int)p>>7] & (s=bitmsk[((int)p>>2)&31]))
# define oksetbit	{bitmapi[r] |= s;}
# endif

/*	Unused bit macros
# define lookbit(p)	(bbitmap[(int)p>>5] & bitmsk[((int)p>>2) & 7])
# define readchk(p)	((int)bitfre[(int)p>>5] & bitmsk[((int)p>>2)&7])
# define setchk(p)	{bitfre[(int)p>>5] |= bitmsk[((int)p >> 2) & 7];}
*/

# define roundup(x,l)	(((x - 1) | (l - 1)) + 1) 

# define MARKVAL(v)	if(((int)v) >= (int)beginsweep) markdp(v);
# define ATOLX(p)	((((int)p)-OFFSET)>>7)

/* the Vax hardware only allows 2^16-1 bytes to be accessed with one
 * movc5 instruction.  We use the movc5 instruction to clear the 
 * bitmaps.
 */
# define MAXCLEAR ((1<<16)-1)

/* METER denotes something added to help meter storage allocation. */

extern int *beginsweep;			/* first sweepable data		*/
extern char purepage[];
extern int fakettsize;
extern int gcstrings;
int debugin  = FALSE;			/* temp debug flag */

extern lispval datalim;			/*  end of data space */
int bitmapi[BITLONGS];			/*  the bit map--one bit per long  */
double zeroq;				/*  a quad word of zeros  */
char *bbitmap = (char *) bitmapi;	/*  byte version of bit map array */
double  *qbitmap = (double *) bitmapi;	/*  integer version of bit map array */
#ifdef METER
extern int gcstat;
extern struct vtimes
	premark,presweep,alldone;	/* actually struct tbuffer's */

extern int mrkdpcnt;
extern int conssame, consdiff,consnil;	/* count of cells whose cdr point
					 * to the same page and different
					 * pages respectively
					 */
#endif
int bitmsk[32]={1,2,4,8,16,32,64,128,	/*  used by bit-marking macros  */
		0x100, 0x200, 0x400, 0x800, 
		0x1000, 0x2000, 0x4000, 0x8000, 
		0x10000, 0x20000, 0x40000, 0x80000, 
		0x100000, 0x200000, 0x400000, 0x800000, 
		0x1000000, 0x2000000, 0x4000000, 0x8000000, 
		0x10000000, 0x20000000, 0x40000000, 0x80000000}; 
extern int  *bind_lists;		/*  lisp data for compiled code */

char *xsbrk();
char *gethspace();


extern struct types atom_str, strng_str, int_str, dtpr_str, doub_str,
	array_str, sdot_str, val_str, funct_str, hunk_str[], vect_str,
	vecti_str, other_str;

extern struct str_x str_current[];

lispval hunk_items[7], hunk_pages[7], hunk_name[7];

extern int initflag; /* starts off TRUE: initially gc not allowed */


/* this is a table of pointers to all struct types objects
 * the index is the type number.
 */
static struct types *spaces[NUMSPACES] = 
	{&strng_str, &atom_str, &int_str,
	 &dtpr_str, &doub_str, &funct_str, 
	 (struct types *) 0,  /* port objects not allocated in this way  */
	 &array_str,
	 &other_str,  /* other objects not allocated in this way  */
	 &sdot_str,&val_str, 
	 &hunk_str[0], &hunk_str[1], &hunk_str[2],
	 &hunk_str[3], &hunk_str[4], &hunk_str[5],
	 &hunk_str[6],
	 &vect_str, &vecti_str};


/* this is a table of pointers to collectable struct types objects
 * the index is the type number.
 */
struct types *gcableptr[] = {
#ifndef GCSTRINGS
     (struct types *) 0,  /* strings not collectable */
#else
     &strng_str,
#endif
     &atom_str,
     &int_str, &dtpr_str, &doub_str,
     (struct types *) 0,  /* binary objects not collectable */
     (struct types *) 0,  /* port objects not collectable */
     &array_str,
     (struct types *) 0,  /* gap in the type number sequence */
     &sdot_str,&val_str, 
     &hunk_str[0], &hunk_str[1], &hunk_str[2],
     &hunk_str[3], &hunk_str[4], &hunk_str[5],
     &hunk_str[6],
     &vect_str, &vecti_str};


/*
 *   get_more_space(type_struct,purep) 
 *									
 *  Allocates and structures a new page, returning 0.
 *  If no space is available, returns positive number.
 *  If purep is TRUE, then pure space is allocated.
 */
get_more_space(type_struct,purep)                                 
struct types *type_struct;
{
	int cntr;
	char *start;
	int *loop, *temp;
	lispval p;
	extern char holend[];

	if( (int) datalim >= TTSIZE*LBPG+OFFSET ) return(2);

	/*
	 * If the hole is defined, then we allocate binary objects
	 * and strings in the hole.  However we don't put strings in
	 * the hole if strings are gc'ed.
	 */
#ifdef HOLE
	if(   purep
#ifndef GCSTRINGS
	   || type_struct==&strng_str
#endif
	   || type_struct==&funct_str)
		start = gethspace(LBPG,type_struct->type);
	else
#endif
		start = xsbrk(1);		/* get new page */


	SETTYPE(start, type_struct->type,20);  /*  set type of page  */

	purepage[ATOX(start)] = (char)purep;  /* remember if page was pure*/

	/* bump the page counter for this space if not pure */

	if(!purep) ++((*(type_struct->pages))->i);

	type_struct->space_left = type_struct->space;
	temp = loop = (int *) start;
	for(cntr=1; cntr < type_struct->space; cntr++)
		loop = (int *) (*loop = (int) (loop + type_struct->type_len));

	/* attach new cells to either the pure space free list  or the 
	 * standard free list
	 */
	if(purep) {
	    *loop = (int) (type_struct->next_pure_free);
	    type_struct->next_pure_free = (char *) temp;
	}
	else  {
	    *loop = (int) (type_struct->next_free);
	    type_struct->next_free = (char *) temp;
	}

	/*  if type atom, set pnames to CNIL  */

	if( type_struct == &atom_str )
		for(cntr=0, p=(lispval) temp; cntr<atom_str.space; ++cntr)
			{
			p->a.pname = (char *) CNIL;
			p = (lispval) ((int *)p + atom_str.type_len);
			}
	return(0);  /*  space was available  */
}


/*
 * next_one(type_struct) 
 *
 *  Allocates one new item of each kind of space, except STRNG.	
 *  If there is no space, calls gc, the garbage collector.
 *  If there is still no space, allocates a new page using
 *  get_more_space
 */

lispval
next_one(type_struct)
struct types *type_struct;
{

	register char *temp;

	while(type_struct->next_free == (char *) CNIL)
		{
		int g;

		if(
		   (initflag == FALSE) && 	/* dont gc during init */
#ifndef GCSTRINGS
		   (type_struct->type != STRNG) && /* can't collect strings */
#else
		   gcstrings &&			/* user (sstatus gcstrings) */
#endif
		   (type_struct->type != BCD) &&   /* nor function headers  */
		   gcdis->a.clb == nil )		/* gc not disabled */
					/* not to collect during load */

			{
			gc(type_struct);  /*  collect  */
			}

		if( type_struct->next_free != (char *) CNIL ) break;

		if(! (g=get_more_space(type_struct,FALSE))) break;

		space_warn(g);
		}
	temp = type_struct->next_free;
	type_struct->next_free = * (char **)(type_struct->next_free);
	(*(type_struct->items))->i ++;
	return((lispval) temp);
}
/*
 * Warn about exhaustion of space,
 * shared with next_pure_free().
 */
space_warn(g)
{
	if( g==1 ) {
	    plimit->i += NUMSPACES; /*  allow a few more pages  */
	    copval(plima,plimit);	/*  restore to reserved reg  */

	    error("PAGE LIMIT EXCEEDED--EMERGENCY PAGES ALLOCATED", TRUE);
	} else error("SORRY, ABSOLUTE PAGE LIMIT HAS BEEN REACHED", TRUE);
}


/* allocate an element of a pure structure.  Pure structures will
 * be ignored by the garbage collector.
 */
lispval
next_pure_one(type_struct)
struct types *type_struct;
{

	register char *temp;

	while(type_struct->next_pure_free == (char *) CNIL)
		{
		int g;
		if(! (g=get_more_space(type_struct,TRUE))) break;
		space_warn(g);
		}

	temp = type_struct->next_pure_free;
	type_struct->next_pure_free = * (char **)(type_struct->next_pure_free);
	return((lispval) temp);
}

lispval
newint()
{
	return(next_one(&int_str));
}

lispval
pnewint()
{
	return(next_pure_one(&int_str));
}

lispval
newdot()
{
	lispval temp;

	temp = next_one(&dtpr_str);
	temp->d.car = temp->d.cdr = nil;
	return(temp);
}

lispval
pnewdot()
{
	lispval temp;

	temp = next_pure_one(&dtpr_str);
	temp->d.car = temp->d.cdr = nil;
	return(temp);
}

lispval
newdoub()
{
	return(next_one(&doub_str));
}

lispval
pnewdb()
{
	return(next_pure_one(&doub_str));
}

lispval
newsdot()
{
	register lispval temp;
	temp = next_one(&sdot_str);
	temp->d.car = temp->d.cdr = 0;
	return(temp);
}

lispval
pnewsdot()
{
	register lispval temp;
	temp = next_pure_one(&sdot_str);
	temp->d.car = temp->d.cdr = 0;
	return(temp);
}

struct atom *
newatom(pure) {
	struct atom *save; char *mypname;

	mypname = newstr(pure);
	pnameprot = ((lispval) mypname);
	save = (struct atom *) next_one(&atom_str) ;	
	save->plist = save->fnbnd = nil;
	save->hshlnk = (struct atom *)CNIL;
	save->clb = CNIL;
	save->pname = mypname;
	return (save);
}

char *
newstr(purep) {
	char *save, *strcpy();
	int atmlen;
	register struct str_x *p = str_current + purep;

	atmlen = strlen(strbuf)+1;
	if(atmlen > p->space_left) {
		if(atmlen >= STRBLEN) {
			save = (char *)csegment(OTHER, atmlen, purep);
			SETTYPE(save,STRNG,40);
			purepage[ATOX(save)] = (char)purep;
			strcpy(save,strbuf);
			return(save);
		}
		p->next_free =  (char *) (purep ?
			next_pure_one(&strng_str) : next_one(&strng_str)) ;
		p->space_left = LBPG;
	}
	strcpy((save = p->next_free), strbuf);
	/*while(atmlen & 3) ++atmlen;	/*  even up length of string  */
	p->next_free += atmlen;
	p->space_left -= atmlen;
	return(save);
}

static char * Iinewstr(s,purep) char *s;
{
	int len = strlen(s);
	while(len > (endstrb - strbuf - 1)) atomtoolong(strbuf);
	strcpy(strbuf,s);
	return(newstr(purep));
}


char *inewstr(s) char *s;
{
	Iinewstr(s,0);
}

char *pinewstr(s) char *s;
{
	Iinewstr(s,1);
}

lispval
newarray()
	{
	register lispval temp;

	temp = next_one(&array_str);
	temp->ar.data = (char *)nil;
	temp->ar.accfun = nil;
	temp->ar.aux = nil;
	temp->ar.length = SMALL(0);
	temp->ar.delta = SMALL(0);
	return(temp);
	}

lispval
newfunct()
	{
	register lispval temp;
	lispval Badcall();
	temp = next_one(&funct_str);
	temp->bcd.start = Badcall;
	temp->bcd.discipline = nil;
	return(temp);
	}

lispval
newval()
	{
	register lispval temp;
	temp = next_one(&val_str);
	temp->l = nil;
	return(temp);
	}

lispval
pnewval()
	{
	register lispval temp;
	temp = next_pure_one(&val_str);
	temp->l = nil;
	return(temp);
	}

lispval
newhunk(hunknum)
int hunknum;
	{
	register lispval temp;

	temp = next_one(&hunk_str[hunknum]);	/* Get a hunk */
	return(temp);
	}

lispval
pnewhunk(hunknum)
int hunknum;
	{
	register lispval temp;

	temp = next_pure_one(&hunk_str[hunknum]);	/* Get a hunk */
	return(temp);
	}

lispval
inewval(arg) lispval arg;
	{
	lispval temp;
	temp = next_one(&val_str);
	temp->l = arg;
	return(temp);
	}

/*
 * Vector allocators.
 * a vector looks like:
 *  longword: N = size in bytes
 *  longword: pointer to lisp object, this is the vector property field
 *  N consecutive bytes
 *
 */
lispval getvec();

lispval
newvec(size)
{
    return(getvec(size,&vect_str,FALSE));
}

lispval
pnewvec(size)
{
    return(getvec(size,&vect_str,TRUE));
}

lispval
nveci(size)
{
    return(getvec(size,&vecti_str,FALSE));
}

lispval
pnveci(size)
{
    return(getvec(size,&vecti_str,TRUE));
}

/*
 * getvec
 *  get a vector of size byte, from type structure typestr and
 * get it from pure space if purep is TRUE.
 *  vectors are stored linked through their property field.  Thus
 * when the code here refers to v.vector[0], it is the prop field
 * and vl.vectorl[-1] is the size field.   In other code,
 * v.vector[-1] is the prop field, and vl.vectorl[-2] is the size.
 */
lispval
getvec(size,typestr,purep)
register struct types *typestr;
{
    register lispval back, current;
    int sizewant, bytes, thissize, pages, pindex, triedgc = FALSE;

    /* we have to round up to a multiple of 4 bytes to determine the
     * size of vector we want.  The rounding up assures that the
     * property pointers are longword aligned
     */
    sizewant = VecTotSize(size);
    if(debugin) fprintf(stderr,"want vect %db\n",size);    
 again:
    if(purep)
        back = (lispval) &(typestr->next_pure_free);
    else
        back = (lispval) &(typestr->next_free);
    current = back->v.vector[0];
    while(current !=  CNIL)
    {
	if(debugin)
            fprintf(stderr,"next free size %db; ", current->vl.vectorl[-1]);
	if ((thissize = VecTotSize(current->vl.vectorl[-1])) == sizewant)
	{
	    if(debugin) fprintf(stderr,"exact match of size %d at 0x%x\n",
	    			4*thissize, &current->v.vector[1]);
	    back->v.vector[0]
	    	= current->v.vector[0];/* change free pointer*/
	    current->v.vector[0] = nil; /* put nil in property */
	    /* to the user, vector begins one after property*/
	    return((lispval)&current->v.vector[1]);
	}
	else if (thissize >= sizewant + 3)
	{
	    /* the reason that there is a `+ 3' instead of `+ 2'
	     * is that we don't want to leave a zero sized vector which
	     * isn't guaranteed to be followed by another vector
	     */
	    if(debugin)
	     fprintf(stderr,"breaking a %d vector into a ",
	     				current->vl.vectorl[-1]);

	    current->v.vector[1+sizewant+1]
	    		= current->v.vector[0];  /* free list pointer */
	    current->vl.vectorl[1+sizewant]
	    		= VecTotToByte(thissize - sizewant - 2);/*size info */
	    back->v.vector[0] = (lispval) &(current->v.vector[1+sizewant+1]);
	    current->vl.vectorl[-1] = size;

	    if(debugin)fprintf(stderr," %d one and a %d one\n",
	    	current->vl.vectorl[-1],current->vl.vectorl[1+sizewant]);
	    current->v.vector[0] = nil; /* put nil in property */
	    /* vector begins one after the property */
	    if(debugin) fprintf(stderr," and returning vector at 0x%x\n",
	    			&current->v.vector[1]);
	    return((lispval)(&current->v.vector[1]));
	}
	back =  current;
	current =  current->v.vector[0];
    }
    if(!triedgc
        && !purep
    	&& (gcdis->a.clb == nil)
	&& (initflag == FALSE))
    {
	gc(typestr);
	triedgc = TRUE;
	goto again;
    }
    
    /* set bytes to size needed for this vector */
    bytes = size + 2*sizeof(long);
    
    /* must make sure that if the vector we are allocating doesnt
       completely fill a page, there is room for another vector to record
       the size left over */
    if((bytes & (LBPG - 1)) > (LBPG - 2*sizeof(long))) bytes += LBPG;
    bytes = roundup(bytes,LBPG);

    current = csegment(typestr->type,bytes/sizeof(long),purep);
    current->vl.vectorl[0] = bytes - 2*sizeof(long);
    
    if(purep) {
        current->v.vector[1] = (lispval)(typestr->next_pure_free);
        typestr->next_pure_free = (char *) &(current->v.vector[1]);
	/* make them pure */
	pages = bytes/LBPG;
	for(pindex = ATOX(current); pages ; pages--)
	{
	    purepage[pindex++] = TRUE;
	}
    } else {
        current->v.vector[1] = (lispval)(typestr->next_free);
        typestr->next_free = (char *) &(current->v.vector[1]);
	if(debugin) fprintf(stderr,"grabbed %d vec pages\n",bytes/LBPG);
    }
    if(debugin)
      fprintf(stderr,"creating a new vec, size %d\n",current->v.vector[0]);
    goto again;
}

/*
 * Ipurep :: routine to check for pureness of a data item
 *
 */
lispval 
Ipurep(element)
lispval element;
{
    if(purepage[ATOX(element)]) return(tatom) ; else return(nil);
}

/* routines to return space to the free list.  These are used by the
 * arithmetic routines which tend to create large intermediate results
 * which are know to be garbage after the calculation is over.
 *
 * There are jsb callable versions of these routines in qfuncl.s
 */

/* pruneb   - prune bignum. A bignum is an sdot followed by a list of
 *  dtprs.    The dtpr list is linked by car instead of cdr so when we
 *  put it in the free list, we have to change the links.
 */
pruneb(bignum)
lispval bignum;
{
	register lispval temp = bignum;

	if(TYPE(temp) != SDOT) 
	    errorh(Vermisc,"value to pruneb not a sdot",nil,FALSE,0);

	--(sdot_items->i);
	temp->s.I = (int) sdot_str.next_free;
	sdot_str.next_free = (char *) temp;

	/* bignums are not terminated by nil on the dual,
	   they are terminated by (lispval) 0 */

	while(temp = temp->s.CDR)
	{
	    if(TYPE(temp) != DTPR) 
	      errorh(Vermisc,"value to pruneb not a list",
		      nil,FALSE,0);
	    --(dtpr_items->i);
	    temp->s.I = (int) dtpr_str.next_free;
	    dtpr_str.next_free = (char *) temp;
	}
}
lispval
Badcall()
	{ error("BAD FUNCTION DESCRIPTOR USED IN CALL",FALSE); }



/*
 * Ngc 
 *  this is the lisp function gc
 *
 */

lispval
Ngc()
{
    return(gc((struct types *)CNIL));
}

/*
 * gc(type_struct) 
 *
 *  garbage collector:  Collects garbage by mark and sweep algorithm.
 *  After this is done, calls the Nlambda, gcafter.
 *  gc may also be called from LISP, as an  nlambda of no arguments.
 * type_struct is the type of lisp data that ran out causing this
 * garbage collection
 */
int printall = 0;
lispval
gc(type_struct)
	struct types *type_struct;
	{
	lispval save;
	struct tms begin, finish;
	extern int gctime;

	/* if this was called automatically when space ran out
	 * print out a message
	 */
	if((Vgcprint->a.clb != nil)
	   && (type_struct != (struct types *) CNIL ))
	{
	    FILE *port = okport(Vpoport->a.clb,poport);
	    fprintf(port,"gc:");
	    fflush(port);
	}
	
	if(gctime) times(&begin);

	gc1(); /* mark&sweep */

	/* Now we call gcafter--special c ase if gc called from LISP */

	if( type_struct == (struct types *) CNIL )
		gccall1->d.cdr = nil;  /* make the call "(gcafter)" */
	else
		{
		gccall1->d.cdr = gccall2;
		gccall2->d.car = *(type_struct->type_name);
		}
	PUSHDOWN(gcdis,gcdis);	/*  flag to indicate in garbage collector  */
	save = eval(gccall1);	/*  call gcafter  */
	POP;			/*  turn off flag  */

	if(gctime) {
		times(&finish);
		gctime += (finish.tms_utime - begin.tms_utime);
	}
	return(save);	/*  return result of gcafter  */
	}

	

/*  gc1()  **************************************************************/
/*									*/
/*  Mark-and-sweep phase						*/

gc1()
{
	int j, k;
	register int *start,bvalue,type_len; 
	register struct types *s;
	int *point,i,freecnt,itemstogo,bits,bindex,type,bytestoclear;
	int usedcnt;
	char *pindex;
	struct argent *loop2;
	struct nament *loop3;
	struct atom *symb;
	int markdp();
	extern int hashtop;

	pagerand(); 
	/*  decide whether to check LISP structure or not  */


#ifdef METER
	vtimes(&premark,0);
	mrkdpcnt = 0;
	conssame = consdiff = consnil = 0;
#endif

	/*  first set all bit maps to zero  */


#ifdef SLOCLEAR
	{
	    int enddat;
	    enddat = (int)(datalim-OFFSET) >> 8;
	    for(bvalue=0; bvalue < (int)enddat ; ++bvalue)
	    {
		 qbitmap[bvalue] = zeroq; 
	    }
	}
#endif

	/* try the movc5 to clear the bit maps */
	/* the maximum number of bytes we can clear in one sweep is
	 * 2^16 (or 1<<16 in the C lingo)
	 */
	bytestoclear = ((((int)datalim)-((int)beginsweep)) >> 9) * 16; 
	for(start = bitmapi + ATOLX(beginsweep);
	    bytestoclear > 0;)
	    {
		if(bytestoclear > MAXCLEAR)
			blzero((int)start,MAXCLEAR);
		else
			blzero((int)start,bytestoclear);
		start = (int *) (MAXCLEAR + (int) start);
		bytestoclear -= MAXCLEAR;
	    }
			
	/* mark all atoms in the oblist */
        for( bvalue=0 ; bvalue <= hashtop-1 ; bvalue++ ) /* though oblist */
        {
	    for( symb = hasht[bvalue] ; symb != (struct atom *) CNIL ;
		      symb = symb-> hshlnk) {
		  markdp((lispval)symb); 
	    }
	}


	/* Mark all the atoms and ints associated with the hunk
	   data types */
	   
	for(i=0; i<7; i++) {
		markdp(hunk_items[i]);
		markdp(hunk_name[i]);
		markdp(hunk_pages[i]);
	}
	/* next run up the name stack */
	for(loop2 = np - 1; loop2 >=  orgnp; --loop2) MARKVAL(loop2->val);	

	/* now the bindstack (vals only, atoms are marked elsewhere ) */
	for(loop3 = bnp - 1; loop3 >= orgbnp; --loop3)MARKVAL(loop3->val);

	
	/* next mark all compiler linked data */
	/* if the Vpurcopylits switch is non nil (lisp variable $purcopylits)
	 * then when compiled code is read in, it tables will not be linked
	 * into this table and thus will not be marked here.  That is ok
	 * though, since that data is assumed to be pure.
	 */
	 point = bind_lists;
	 while((start = point) != (int *)CNIL) {
	 	while( *start != -1 )
	 	{
	 		markdp((lispval)*start);
	 		start++;
	 	}
	 	point = (int *)*(point-1);
	 }

	/* next mark all system-significant lisp data */

	
	for(i=0; i<SIGNIF; ++i) markdp((lispsys[i]));

#ifdef METER
	vtimes(&presweep,0);
#endif
	/* all accessible data has now been marked. */
	/* all collectable spaces must be swept,    */
	/* and freelists constructed.		    */

	/* first clear the structure elements for types
	 * we will sweep
	 */
	
	for(k=0 ; k <= VECTORI ; k++)
	{
		if( s=gcableptr[k]) {
		    if(k==STRNG && !gcstrings) { /* don't do anything*/ }
		    else
			{
			  (*(s->items))->i = 0;
			  s->space_left = 0;
			  s->next_free = (char *) CNIL;
			}
		}
	}
#if m_68k
	fixbits(bitmapi+ATOLX(beginsweep),bitmapi+ATOLX(datalim));
#endif


	/* sweep up in memory looking at gcable pages */

	for(start = beginsweep,  bindex = ATOLX(start), 
		  pindex = &purepage[ATOX(start)]; 
	    start < (int *)datalim;
	    start += 128, pindex++)
	{
	    if(!(s=gcableptr[type = TYPE(start)]) || *pindex
#ifdef GCSTRINGS
	     || (type==STRNG && !gcstrings) 
#endif
		)
	    {
		/* ignore this page but advance pointer 	*/
		bindex += 4;   /* and 4 words of 32 bit bitmap words */
		continue;
	    }

	    freecnt = 0;		/* number of free items found */
	    usedcnt = 0;		/* number of used items found */
	    
	    point = start;
	    /* sweep dtprs as a special case, since
	     * 1) there will (usually) be more dtpr pages than any other type
	     * 2) most dtpr pages will be empty so we can really win by special
	     *    caseing the sweeping of massive numbers of free cells
	     */
	    /* since sdot's have the same structure as dtprs, this code will
	       work for them too
	     */
	    if((type == DTPR) || (type == SDOT))
	    {
		int *head,*lim;
		head = (int *) s->next_free;	/* first value on free list*/

		for(i=0; i < 4; i++)	/* 4 bit map words per page  */
		{
		    bvalue = bitmapi[bindex++];	/* 32 bits = 16 dtprs */
		    if(bvalue == 0)	/* if all are free	*/
		    {
			*point = (int)head;
			lim = point + 32;   /* 16 dtprs = 32 ints */
			for(point += 2; point < lim ; point += 2)
			{
			    *point = (int)(point - 2);
			}
			head = point - 2;
			freecnt += 16;
		    }
		    else for(j = 0; j < 16 ; j++)
		    {
			if(!(bvalue & 1))
			{
			    freecnt++;
			    *point = (int)head;
			    head = point;
			}
#ifdef METER
			/* check if the page address of this cell is the
			 * same as the address of its cdr
			 */
			else if(FALSE && gcstat && (type == DTPR))
			{  
			   if(((int)point & ~511) 
			      == ((int)(*point) & ~511)) conssame++;
			   else consdiff++;
			   usedcnt++;
			}
#endif
			else usedcnt++;		/* keep track of used */
			
			point += 2;
			bvalue = bvalue >> 2;
		    }
		}
		s->next_free = (char *) head;
	    }
	    else if((type == VECTOR) || (type == VECTORI))
	    {
		int canjoin = FALSE;
		int *tempp;

		/* check if first item on freelist ends exactly at
		   this page
		 */
		if(((tempp = (int *)s->next_free) != (int *)CNIL)
		   && ((VecTotSize(((lispval)tempp)->vl.vectorl[-1])
		   					    + 1 + tempp)
		   			== point))
		   canjoin = TRUE;
		   
		/* arbitrary sized vector sweeper */
		/*
		 * jump past first word since that is a size fixnum
		 * and second word since that is property word
		 */
		if(debugin)
		  fprintf(stderr,"vector sweeping, start at 0x%x\n",
				point);
		bits = 30;
		bvalue = bitmapi[bindex++] >> 2;
		point += 2;
		while (TRUE) {
		    type_len = point[VSizeOff];
		    if(debugin) {
		      fprintf(stderr,"point: 0x%x, type_len %d\n",
		    		point, type_len);
		      fprintf(stderr,"bvalue: 0x%x, bits: %d, bindex: 0x%x\n",
				bvalue, bits, bindex);
		    }
		    			/* get size of vector */
		    if(!(bvalue & 1))	/* if free */
		    {
			if(debugin) fprintf(stderr,"free\n");
			freecnt += type_len + 2*sizeof(long);
			if(canjoin)
			{
			    /* join by adjusting size of first vector */
			    ((lispval)(s->next_free))->vl.vectorl[-1]
			      +=  type_len + 2*sizeof(long); 
			    if(debugin)
			      fprintf(stderr,"joined size: %d\n",
			          ((lispval)(s->next_free))->vl.vectorl[-1]);
			}
			else {
			    /* vectors are linked at the property word */
			    *(point - 1) = (int)(s->next_free);
			    s->next_free = (char *) (point - 1);
			}
			canjoin = TRUE;
		    }
		    else {
		    	canjoin = FALSE;
			usedcnt += type_len + 2*sizeof(long);
		    }
		    
		    point += VecTotSize(type_len);
		    /* we stop sweeping only when we reach a page
		       boundary since vectors can span pages
		     */
		    if(((int)point & 511) == 0)
		    {
			/* reset the counters, we cannot predict how
			 * many pages we have crossed over
			 */
			bindex = ATOLX(point);
			/* these will be inced, so we must dec */
			pindex = &purepage[ATOX(point)] - 1;
			start = point - 128;
			if(debugin)
			fprintf(stderr,
				"out of vector sweep when point = 0x%x\n",
				point);
			break;
		    }
		    /* must advance to next point and next value in bitmap.
		     * we add VecTotSize(type_len) + 2 to get us to the 0th
		     * entry in the next vector (beyond the size fixnum)
		     */
		    point += 2; 	/* point to next 0th entry */
		    if ( (bits -= (VecTotSize(type_len) + 2)) > 0)  
		        bvalue = bvalue >> (VecTotSize(type_len) + 2);
		    else {
			bits = -bits;	/* must advance to next word in map */
			bindex += bits / 32; /* this is tricky stuff... */
			bits = bits % 32;
			bvalue = bitmapi[bindex++] >> bits;
			bits = 32 - bits;
		    }
		}
	    }
	    else { 
		/* general sweeper, will work for all types */
		itemstogo = s->space;	/* number of items per page  */
		bits = 32;			/* number of bits per word */
		type_len = s->type_len;

		/* printf(" s %d, itemstogo %d, len %d\n",s,itemstogo,type_len);*/
		bvalue = bitmapi[bindex++];

		while(TRUE)
		{
		    if(!(bvalue & 1))	/* if data element is not marked */
		    {
			freecnt++;
			*point = (int) (s->next_free) ;
			s->next_free = (char *) point;
		    }
		    else usedcnt++;

		    if( --itemstogo <= 0 ) 
		    {    if(type_len >= 64) 
			 {
			    bindex++;
			    if(type_len >=128) bindex += 2;
			 }
			 break;
		    }

		    point += type_len;
		    /* shift over mask by number of words in data type */

		    if( (bits -= type_len) > 0)
		    {  bvalue = bvalue >> type_len;
		    } 
		    else if( bits == 0 ) 
		    {  bvalue = bitmapi[bindex++];
		       bits = 32;
		    }
		    else
		    {  bits = -bits;
		       while( bits >= 32) { bindex++;
					    bits -= 32;
					  }
		       bvalue = bitmapi[bindex++];
		       bvalue = bvalue >> bits;
		       bits = 32 - bits;;
		    }
	    }
	}

     s->space_left += freecnt;
     (*(s->items))->i += usedcnt;
     }

#ifdef METER
        vtimes(&alldone,0);
	if(gcstat) gcdump();
#endif
	pagenorm(); 
}

/*
 * alloc
 *
 *  This routine tries to allocate one or more pages of the space named
 *  by the first argument.   Returns the number of pages actually allocated.
 *  
 */

lispval
alloc(tname,npages)
lispval tname; long npages;
{
	long ii, jj;
	struct types *typeptr;

	ii = typenum(tname);
        typeptr = spaces[ii];
	if(npages <= 0) return(inewint(npages));
	
	if((ATOX(datalim)) + npages > TTSIZE)
	   error("Space request would exceed maximum memory allocation",FALSE);
	if((ii == VECTOR) || (ii == VECTORI))
	{
	    /* allocate in one big chunk */
	    tname = csegment((int) ii,(int) npages*128,0);
	    tname->vl.vectorl[0] = (npages*512 - 2*sizeof(long));
	    tname->v.vector[1] = (lispval) typeptr->next_free;
	    typeptr->next_free = (char *) &(tname->v.vector[1]);
	    if(debugin) fprintf(stderr,"alloced %d vec pages\n",npages);
	    return(inewint(npages));
	}
	   
	for( jj=0; jj<npages; ++jj)
		if(get_more_space(spaces[ii],FALSE)) break;
	return(inewint(jj));
}

/*
 * csegment(typecode,nitems,useholeflag)
 *  allocate nitems of type typecode.  If useholeflag is true, then
 * allocate in the hole if there is room.  This routine doesn't look
 * in the free lists, it always allocates space.
 */	
lispval
csegment(typecode,nitems,useholeflag)
{
	register int ii, jj;
	register char *charadd;

	ii = typecode;

	if(ii!=OTHER) nitems *= 4*spaces[ii]->type_len;
	nitems = roundup(nitems,512);		/*  round up to right length  */
#ifdef HOLE
	if(useholeflag)
		charadd = gethspace(nitems,ii);
	else
#endif
	{
		charadd = sbrk(nitems);
		datalim = (lispval)(charadd+nitems);
	}
	if( (int) charadd <= 0 )
		error("NOT ENOUGH SPACE FOR ARRAY",FALSE);
	/*if(ii!=OTHER)*/ (*spaces[ii]->pages)->i +=  nitems/512;
	if(ATOX(datalim) > fakettsize) {
		datalim = (lispval) (OFFSET + (fakettsize << 9));
		if(fakettsize >= TTSIZE)
		{
		    printf("There isn't room enough to continue, goodbye\n");
		    franzexit(1);
		}
		fakettsize++;
		badmem(53);
	}
	for(jj=0; jj<nitems; jj=jj+512) {
		SETTYPE(charadd+jj, ii,30);
	}
	ii = (int) charadd;
	while(nitems > MAXCLEAR)
	{
	    blzero(ii,MAXCLEAR);
	    nitems -= MAXCLEAR;
	    ii += MAXCLEAR;
	}
	blzero(ii,nitems);
	return((lispval)charadd);
}

int csizeof(tname) lispval tname;
	{
	return( spaces[typenum(tname)]->type_len * 4 );
	}

int typenum(tname) lispval tname;
	{
	int ii;

chek:	for(ii=0; ii<NUMSPACES; ++ii)
		if(spaces[ii] && tname == *(spaces[ii]->type_name)) break;
	if(ii == NUMSPACES)
		{
		tname = error("BAD TYPE NAME",TRUE);
		goto chek;
		}

	return(ii);
	
	}
char *
gethspace(segsiz,type)
{
	extern usehole; extern char holend[]; extern char *curhbeg;
	register char *value;

	if(usehole) {	
		curhbeg = (char *) roundup(((int)curhbeg),LBPG);
		if((holend - curhbeg) < segsiz)
		{	
			usehole = FALSE;
			curhbeg = holend;
		} else {
			value = curhbeg;
			curhbeg = curhbeg + segsiz;
			/*printf("start %d, finish %d, size %d\n",value, curhbeg,segsiz);*/
			return(value);
		}
	}
	value = (ysbrk(segsiz/LBPG,type));
	datalim = (lispval)(value + segsiz);
	return(value);
}
gcrebear()
{
#ifdef HOLE
        register int i; register struct types *p;

	/* this gets done upon rebirth */
	str_current[1].space_left = 0;
#ifndef GCSTRINGS
	str_current[0].space_left = 0;  /* both kinds of strings go in hole*/
#endif
	funct_str.space_left = 0;
	funct_str.next_free = (char *) CNIL;
	/* clear pure space pointers */
	for(i = 0; i < NUMSPACES; i++)
	{
	    if(p=spaces[i])
	    p->next_pure_free = (char *) CNIL;
	}
#endif
}

/** markit(p) ***********************************************************/
/*  just calls markdp							*/

markit(p) lispval *p; { markdp(*p); }

/*
 * markdp(p)
 *
 *  markdp is the routine which marks each data item.  If it is a
 *  dotted pair, the car and cdr are marked also.
 *  An iterative method is used to mark list structure, to avoid
 *  excessive recursion.
 */
markdp(p) register lispval p;
	{
#ifdef tahoe
	register int r, s;	/* (goes with non-asm readbit, oksetbit) */
#endif
/*	register hsize, hcntr;						 */
	int hsize, hcntr;

#ifdef METER
	mrkdpcnt++;
#endif
ptr_loop:
	if(((int)p) <= ((int)nil)) return;	/*  do not mark special data types or nil=0  */

        	
	switch( TYPE(p) )
		{
		case ATOM:
			ftstbit;
			MARKVAL(p->a.clb);
			MARKVAL(p->a.plist);
			MARKVAL(p->a.fnbnd);
#ifdef GCSTRINGS
			if(gcstrings) MARKVAL(((lispval)p->a.pname));
			return;

		case STRNG:
			p = (lispval) (((int) p) & ~ (LBPG-1));
			ftstbit;
#endif
			return;
			
		case INT:
		case DOUB:
			ftstbit;
			return;
		case VALUE:
			ftstbit;
			p = p->l;
			goto ptr_loop;
		case DTPR:
			ftstbit;
			MARKVAL(p->d.car);
#ifdef METER
			/* if we are metering , then check if the cdr is
			 * nil, or if the cdr is on the same page, and if
			 * it isn't one of those, then it is on a different
			 * page
			 */
			 if(gcstat)
			 {
			     if(p->d.cdr == nil) consnil++;
			     else if(((int)p & ~511) 
				     == (((int)(p->d.cdr)) & ~511))
				conssame++;
			     else consdiff++;
			  }
#endif
			p = p->d.cdr;
			goto ptr_loop;

		case ARRAY:
			ftstbit;	/* mark array itself */

			MARKVAL(p->ar.accfun);	/* mark access function */
			MARKVAL(p->ar.aux);		/* mark aux data */
			MARKVAL(p->ar.length);	/* mark length */
			MARKVAL(p->ar.delta);	/* mark delta */
			if(TYPE(p->ar.aux)==DTPR && p->ar.aux->d.car==Vnogbar)
			{
			    /* a non garbage collected array must have its
			     * array space marked but the value of the array
			     * space is not marked
			     */
			     int l; 
			     int cnt,d;
			     if(debugin) {
			       printf("mark array holders len %d, del %d, start 0x%x\n",
			         p->ar.length->i,p->ar.delta->i,p->ar.data);
				 fflush(stdout);
			    }
			     l = p->ar.length->i; /* number of elements */
			     d = p->ar.delta->i;  /* bytes per element  */
			     p = (lispval) p->ar.data;/* address of first one*/
			     if(purepage[ATOX(p)]) return;

			     for((cnt = 0); cnt<l ; 
				      p = (lispval)(((char *) p) + d), cnt++)
			     {
				setbit;
			     }
			} else {
/*			register int i, l; int d;		*/
/*			register char *dataptr = p->ar.data;	*/
			int i,l,d;
			char *dataptr = p->ar.data;

			for(i=0, l=p->ar.length->i, d=p->ar.delta->i; i<l; ++i)
				{
				markdp((lispval)dataptr);
				dataptr += d;
				}
			}
			return;
		case SDOT:
			do {
				ftstbit;
				p = p->s.CDR;
			} while (p!=0);
			return;

		case BCD:
			ftstbit;
			markdp(p->bcd.discipline);
			return;

		case HUNK2:
		case HUNK4:
		case HUNK8:
		case HUNK16:
		case HUNK32:
		case HUNK64:
		case HUNK128:
			{
				hsize = 2 << HUNKSIZE(p);
				ftstbit;
				for (hcntr = 0; hcntr < hsize; hcntr++)
					MARKVAL(p->h.hunk[hcntr]);
				return;
			}
			
		case VECTORI:
			ftstbit;
			MARKVAL(p->v.vector[-1]);	/* mark property */
			return;
			
		case VECTOR:
			{
			    register int vsize;
			    ftstbit;
			    vsize = VecSize(p->vl.vectorl[VSizeOff]);
			    if(debugin)
			       fprintf(stderr,"mark vect at %x  size %d\n",
			       		p,vsize);
			    while(--vsize >= -1)
			    {
				MARKVAL(p->v.vector[vsize]);
			    };
			    return;
			}
		}
	return;
	}


/* xsbrk allocates space in large chunks (currently 16 pages)
 * xsbrk(1)  returns a pointer to a page
 * xsbrk(0)  returns a pointer to the next page we will allocate (like sbrk(0))
 */

char *
xsbrk(n)
	{
	static char *xx;	/*  pointer to next available blank page  */
	extern int xcycle;	/*  number of blank pages available  */
	lispval u;			/*  used to compute limits of bit table  */

	if( (xcycle--) <= 0 )
		{
		xcycle = 15;
		xx = sbrk(16*LBPG);	/*  get pages 16 at a time  */
		if( (int)xx== -1 )
			lispend("For sbrk from lisp: no space... Goodbye!");
		}
	else xx += LBPG;

	if(n == 0)
	{
	    xcycle++;	/* don't allocate the page */
	    xx -= LBPG;
	    return(xx);	/* just return its address */
	}

	if( (u = (lispval)(xx+LBPG))  > datalim ) datalim = u;
	return(xx);
	}

char *ysbrk(pages,type) int pages, type;
	{
	char *xx;	/*  will point to block of storage  */
	int i;

	xx = sbrk(pages*LBPG);
	if((int)xx == -1)
		error("OUT OF SPACE FOR ARRAY REQUEST",FALSE);

	datalim = (lispval)(xx+pages*LBPG);	/*  compute bit table limit  */

	/*  set type for pages  */

	for(i = 0; i < pages; ++i) {
		SETTYPE((xx + i*LBPG),type,10);
	}

	return(xx);	/*  return pointer to block of storage  */
	}
	
/*
 * getatom 
 * returns either an existing atom with the name specified in strbuf, or
 * if the atom does not already exist, regurgitates a new one and 
 * returns it.
 */
lispval
getatom(purep)
{   register lispval aptr;
    register char *name, *endname;
    register int hash;
    lispval	b;
    char	c;

	name = strbuf;
	if (*name == (char)0377) return (eofa);
	hash = hashfcn(name);
	atmlen = strlen(name) + 1;
	aptr = (lispval) hasht[hash];
	while (aptr != CNIL)
	    /* if (strcmp(name,aptr->a.pname)==0) */
	    if (*name==*aptr->a.pname && strcmp(name,aptr->a.pname)==0)
		return (aptr);
	    else
		aptr = (lispval) aptr->a.hshlnk;
	aptr = (lispval) newatom(purep);  /*share pname of atoms on oblist*/
	aptr->a.hshlnk = hasht[hash];
	hasht[hash] = (struct atom *) aptr;
	endname = name + atmlen - 2;
	if ((atmlen != 4) && (*name == 'c') && (*endname == 'r'))
		{
		b = newdot();
		protect(b);
		b->d.car = lambda;
		b->d.cdr = newdot();
		b = b->d.cdr;
		b->d.car = newdot();
		(b->d.car)->d.car = xatom;
		while(TRUE)
			{
			b->d.cdr = newdot();
			b= b->d.cdr;
			if(++name == endname)
				{
				b->d.car= (lispval) xatom;
				aptr->a.fnbnd = (--np)->val;
				break;
				}
			b->d.car= newdot();
			b= b->d.car;
			if((c = *name) == 'a') b->d.car = cara;
			else if (c == 'd') b->d.car = cdra;
			else{ --np;
			   break;
			 }
			}
		}

	return(aptr);
	}

/*
 * inewatom is like getatom, except that you provide it a string
 * to be used as the print name.  It doesn't do the automagic
 * creation of things of the form c[ad]*r.
 */
lispval
inewatom(name)
register char *name;
{   register struct atom *aptr;
    register int hash;
    extern struct types atom_str;
    char	c;

	if (*name == (char)0377) return (eofa);
	hash = hashfcn(name);
	aptr = hasht[hash];
	while (aptr != (struct atom *)CNIL)
	    if (strcmp(name,aptr->pname)==0)
		return ((lispval) aptr);
	    else
		aptr = aptr->hshlnk;
	aptr = (struct atom *) next_one(&atom_str) ;	
	aptr->plist = aptr->fnbnd = nil;
	aptr->clb = CNIL;
	aptr->pname = name;
	aptr->hshlnk = hasht[hash];
	hasht[hash] = aptr;
	return((lispval)aptr);
}


/* our hash function */

hashfcn(symb)
register char *symb;
{
	register int i;
/*	for (i=0 ; *symb ; i += i + *symb++); return(i & (HASHTOP-1)); */
	for (i=0 ; *symb ; i += i*2 + *symb++);
	return(i&077777 % HASHTOP);
}

lispval
LImemory()
{
    int nextadr, pagesinuse;
    
    printf("Memory report. max pages = %d (0x%x) = %d Bytes\n",
    		TTSIZE,TTSIZE,TTSIZE*LBPG);
#ifdef HOLE
        printf("This lisp has a hole:\n");
	printf("  current hole start: %d (0x%x), end %d (0x%x)\n",
		curhbeg, curhbeg, holend, holend);
	printf("  hole free: %d bytes = %d pages\n\n",
	       holend-curhbeg, (holend-curhbeg)/LBPG);
#endif 
    nextadr = (int) xsbrk(0);	/* next space to be allocated */
    pagesinuse = nextadr/LBPG;
    printf("Next allocation at addr %d (0x%x) = page %d\n",
			nextadr, nextadr, pagesinuse);
    printf("Free data pages: %d\n", TTSIZE-pagesinuse);
    return(nil);
}

extern struct atom *hasht[HASHTOP];
myhook(){}
