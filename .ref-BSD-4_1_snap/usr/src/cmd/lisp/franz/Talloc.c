

static char *sccsid = "@(#)Talloc.c	35.5 6/28/81";

# include "global.h"
# include "structs.h"
# ifndef   UNIXTS
# include <vadvise.h>
# endif

# define NUMWORDS TTSIZE * 128  /*  max number of words in P0 space  */
# define BITQUADS TTSIZE * 2	/*  length of bit map in quad words  */
# define BITLONGS TTSIZE * 4	/*  length of bit map in long words  */

# define ftstbit	asm("	ashl	$-2,r11,r3");\
			asm("	bbcs	r3,_bitmapi,$1");\
			asm("	.byte	4");
/* setbit is a fast way of setting a bit, it is like ftstbit except it
 * always continues on to the next instruction
 */
# define setbit		asm("	ashl	$-2,r11,r0"); \
			asm("	bbcs	r0,_bitmapi,$0");

/*  define ftstbit	if( readbit(p) ) return; oksetbit;  */
# define readbit(p)	((int)bitmap[r=(int)p>>5] & (s=bitmsk[((int)p>>2)&7]))
# define lookbit(p)	(bitmap[(int)p>>5] & bitmsk[((int)p>>2) & 7])
/* # define setbit(p)	{bitmap[(int)p>>5] |= bitmsk[((int)p >> 2) & 7];} */
# define oksetbit	{bitmap[r] |= s;}

# define readchk(p)	((int)bitfre[(int)p>>5] & bitmsk[((int)p>>2)&7])
# define setchk(p)	{bitfre[(int)p>>5] |= bitmsk[((int)p >> 2) & 7];}
# define roundup(x,l)	(((x - 1) | (l - 1)) + 1) 

# define MARKVAL(v)	if((v) >= (lispval)beginsweep) markdp(v);

/* the Vax hardware only allows 2^16-1 bytes to be accessed with one
 * movc5 instruction.  We use the movc5 instruction to clear the 
 * bitmaps.
 */
# define MAXCLEAR ((1<<16)-1)

/* METER denotes something added to help meter storage allocation. */

extern int *beginsweep;			/* first sweepable data		*/
extern char purepage[];
extern int fakettsize;

FILE * chkport;				/* garbage collection dump file */
extern lispval datalim;			/*  end of data space */
int bitmapi[BITLONGS];			/*  the bit map--one bit per long  */
double zeroq;				/*  a quad word of zeros  */
char *bitmap = (char *) bitmapi;	/*  byte version of bit map array */
double  *bitmapq = (double *) bitmapi;	/*  integer version of bit map array */
#ifdef METER
extern int gcstat;
extern int premark,presweep,alldone; /* actually struct tbuffer's */
extern int markdpcount;
extern int conssame, consdiff,consnil;	/* count of cells whose cdr point
					 * to the same page and different
					 * pages respectively
					 */
#endif
char bitmsk[8]={1,2,4,8,16,32,64,128};  /*  used by bit-marking macros  */
extern int  *bind_lists ;		/*  lisp data for compiled code */

char *xsbrk();
char *gethspace();


extern struct types atom_str, strng_str, int_str, dtpr_str, doub_str,
	array_str, sdot_str, val_str, funct_str, hunk_str[];

lispval hunk_items[7], hunk_pages[7], hunk_name[7];

extern int initflag; /* starts off TRUE: initially gc not allowed */

int gcflag = FALSE;	/*  TRUE during garbage collection  */

int current = 0;	/* number of pages currently allocated */

static struct types *(spaces[NUMSPACES]) = 
	{&atom_str, &strng_str, &int_str,
	 &dtpr_str, &doub_str, &array_str,
	 &sdot_str, &val_str, &funct_str,
	 &hunk_str[0], &hunk_str[1], &hunk_str[2],
	 &hunk_str[3], &hunk_str[4], &hunk_str[5],
	 &hunk_str[6]};

/* this is a table of pointers to collectable struct types objects
 * the index is the type number.
 */
struct types *gcableptr[] =
   { (struct types *) 0,  /* strings not collectable */
     &atom_str,
     &int_str, &dtpr_str, &doub_str,
     (struct types *) 0,  /* binary objects not collectable */
     (struct types *) 0,  /* port objects not collectable */
     &array_str,
     (struct types *) 0,  /* gap in the type number sequence */
     &sdot_str,&val_str, 
     &hunk_str[0], &hunk_str[1], &hunk_str[2],
     &hunk_str[3], &hunk_str[4], &hunk_str[5],
     &hunk_str[6]};


/** get_more_space(type_struct,purep) ***********************************/
/*									*/
/*  Allocates and structures a new page, returning 0.			*/
/*  If no space is available, returns 1.				*/
/*  If purep is TRUE, then pure space is allocated.			*/

get_more_space(type_struct,purep)                                 
struct types *type_struct;
{
	int cntr;
	char *start;
	int *loop, *temp;
	lispval p, plim;
	struct heads *next; extern char holend[];

	if(initflag == FALSE) 
		/*  mustn't look at plist of plima too soon  */
		{
		while( plim=copval(plima,(lispval)CNIL), TYPE(plim)!=INT )
			copval(plima,error("BAD PAGE LIMIT",TRUE));
		if( plim->i <= current ) return(1);	/*  Can't allocate  */
		}

	if( current >= TTSIZE ) return(2);

#ifdef HOLE
	if(purep || type_struct==&strng_str || (type_struct==&funct_str))
		start = gethspace(NBPG,type_struct->type);
	else
#endif
		start = xsbrk(1);		/* get new page */


	SETTYPE(start, type_struct->type);  /*  set type of page  */

	purepage[(int)start>>9] = (char)purep;  /* remember if page was pure */

	/* bump the page counter for this space if not pure */

	++((*(type_struct->pages))->i);

	type_struct->space_left = type_struct->space;
	if(type_struct==&strng_str) {
		type_struct->next_free = start;
		return(0);  /*  space was available  */
	}
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


/** next_one(type_struct) ************************************************/
/*									*/
/*  Allocates one new item of each kind of space, except STRNG.		*/
/*  If there is no space, calls gc, the garbage collector.		*/
/*  If there is still no space, allocates a new page using		*/
/*  get_more_space(type_struct,FALSE)						*/

lispval
next_one(type_struct)
struct types *type_struct;
{

	register char *temp;
	snpand(1);

	while(type_struct->next_free == (char *) CNIL)
		{
		int g;

		if((type_struct->type != ATOM) &&   /* can't collect atoms */
		   (type_struct->type != STRNG) &&  /* can't collect strings */
		   (type_struct->type != BCD) &&    /* nor function headers  */
		   (gcthresh->i <= current) &&		/* threshhold for gc */
		   gcdis->a.clb == nil &&		/* gc not disabled */
		   (NOTNIL(copval(gcload,CNIL)) || (loading->a.clb != tatom)) &&
					/* not to collect during load */
		   (initflag == FALSE) &&	/* dont gc during init */
		   (gcflag == FALSE))		  /* don't recurse gc */

			{
			/* fputs("Collecting",poport);
			dmpport(poport);*/
			gc(type_struct);  /*  collect  */
			}

		if( type_struct->next_free != (char *) CNIL ) break;

		if(! (g=get_more_space(type_struct,FALSE))) break;

		if( g==1 )
			{
			plimit->i = current+NUMSPACES;
				/*  allow a few more pages  */
			copval(plima,plimit);	/*  restore to reserved reg  */

			error("PAGE LIMIT EXCEEDED--EMERGENCY PAGES ALLOCATED",
				TRUE);
			}
		else error("SORRY, ABSOLUTE PAGE LIMIT HAS BEEN REACHED",
				TRUE);
		}

	temp = type_struct->next_free;
	type_struct->next_free = * (char **)(type_struct->next_free);
	return((lispval) temp);
}


/* allocate an element of a pure structure.  Pure structures will
 * be ignored by the garbage collector.
 */
lispval
next_pure_one(type_struct)
struct types *type_struct;
{

	register char *temp;
	snpand(1);

	while(type_struct->next_pure_free == (char *) CNIL)
		{
		int g;
		if(! (g=get_more_space(type_struct,TRUE))) break;

		if( g==1 )
			{
			plimit->i = current+NUMSPACES;
				/*  allow a few more pages  */
			copval(plima,plimit);	/*  restore to reserved reg  */

			error("PAGE LIMIT EXCEEDED--EMERGENCY PAGES ALLOCATED",
				TRUE);
			}
		else error("SORRY, ABSOLUTE PAGE LIMIT HAS BEEN REACHED",
				TRUE);
		}

	temp = type_struct->next_pure_free;
	type_struct->next_pure_free = * (char **)(type_struct->next_pure_free);
	return((lispval) temp);
}

lispval
newint()
{
	++(int_items->i);
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

	++(dtpr_items->i);
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
	++(doub_items->i);
	return(next_one(&doub_str));
}

lispval
pnewdoub()
{
	return(next_pure_one(&doub_str));
}

lispval
newsdot()
{
	register lispval temp;
	++(sdot_items->i);
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
newatom() {
	struct atom *save;

	++(atom_items->i);
	save = (struct atom *) next_one(&atom_str) ;	
	save->plist = save->fnbnd = nil;
	save->hshlnk = (struct atom *)CNIL;
	save->clb = CNIL;
	save->pname = newstr();
	return (save);
}

char *newstr() {
	char *save;
	int atmlen2,atmlen;

	++(str_items->i);
	atmlen = strlen(strbuf)+1;
	if(atmlen > strng_str.space_left)
		while(get_more_space(&strng_str,FALSE))
			error("YOU HAVE RUN OUT OF SPACE",TRUE);
	strcpy((save = strng_str.next_free), strbuf);
	atmlen2 = atmlen;
	while(atmlen2 & 3) ++atmlen2;	/*  even up length of string  */
	strng_str.next_free += atmlen2;
	strng_str.space_left -= atmlen2;
	return(save);
}

char *inewstr(s) char *s;
{
	strbuf[STRBLEN-1] = '\0';
	strcpyn(strbuf,s,STRBLEN-1);
	return(newstr());
}

lispval
newarray()
	{
	register lispval temp;

	++(array_items->i);
	temp = next_one(&array_str);
	temp->ar.data = (char *)nil;
	temp->ar.accfun = nil;
	temp->ar.aux = nil;
	temp->ar.length = SMALL(0);
	temp->ar.delta = SMALL(0);
	return(temp);
	}


/*
 * Ipurep :: routine to check for pureness of a data item
 *
 */
lispval 
Ipurep(element)
lispval element;
{
    if(purepage[(int)element >> 9]) return(tatom) ; else return(nil);
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
	/* the error  messages in this routine shouldn't print out the
	 * value since it is probably junk
	 */
	register lispval temp, save;
	--(sdot_items->i);
	if(TYPE(bignum) != SDOT) 
	    errorh(Vermisc,"value to pruneb not a sdot",nil,FALSE,0,bignum);
	bignum->s.I = (int) sdot_str.next_free;
	sdot_str.next_free = (char *) bignum;
	if((save = bignum->s.CDR) != nil)
	{
	    for(temp = save ; temp != nil ; temp = temp->d.car)
	    {
		if(TYPE(temp) != DTPR) 
		  errorh(Vermisc,"value to pruneb not a list", nil,FALSE,0,temp);
		--(dtpr_items->i);
		temp->d.cdr = temp->d.car;
		if(temp->d.cdr == nil)
		{
		    temp->d.cdr = (lispval) dtpr_str.next_free;
		    dtpr_str.next_free = (char *)save;
		    break;
		}
	    }
	}
}
lispval
badcall()
	{ error("BAD FUNCTION DESCRIPTOR USED IN CALL",FALSE); }

lispval
newfunct()
	{
	register lispval temp;
	++(funct_items->i);
	temp = next_one(&funct_str);
	temp->bcd.entry = badcall;
	temp->bcd.discipline = nil;
	return(temp);
	}

lispval
newval()
	{
	register lispval temp;
	++(val_items->i);
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

	++(hunk_items[hunknum]->i);		/* Update used hunks count */
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
	++(val_items->i);
	temp = next_one(&val_str);
	temp->l = arg;
	return(temp);
	}


/** Ngc *****************************************************************/
/*									*/
/*  LISP interface to gc.						*/

lispval Ngc()
	{
	lispval temp;

	if( ISNIL(lbot->val) ) return(gc(CNIL));

	if( TYPE(lbot->val) != DTPR ) error("BAD CALL TO GC",FALSE);

	chkport = poport;

	if( NOTNIL(lbot->val->d.car) )
		{
		temp = eval(lbot->val->d.car);
		if( TYPE(temp) == PORT ) chkport = temp->p;
		}

	gc1(TRUE);

	return(nil);
	}

/** gc(type_struct) *****************************************************/
/*									*/
/*  garbage collector:  Collects garbage by mark and sweep algorithm.	*/
/*  After this is done, calls the Nlambda, gcafter.			*/
/*  gc may also be called from LISP, as a lambda of no arguments.	*/

lispval
gc(type_struct)
	struct types *type_struct;
	{
	lispval save;
	struct {
		long mytime;
		long allelse[3];
	} begin, finish;
	extern int GCtime;

	save = copval(gcport,CNIL);
	if(GCtime)
		times(&begin);

	while( (TYPE(save) != PORT) && NOTNIL(save))
		save = error("NEED PORT FOR GC",TRUE);

	chkport = (ISNIL(save) ? poport : save->p);
	
	gc1(NOTNIL(copval(gccheck,CNIL)) || (chkport!=poport)); /* mark&sweep */

	/* Now we call gcafter--special case if gc called from LISP */

	if( type_struct == (struct types *) CNIL )
		gccall1->d.cdr = nil;  /* make the call "(gcafter)" */
	else
		{
		gccall1->d.cdr = gccall2;
		gccall2->d.car = *(type_struct->type_name);
		}
	{lispval temp;temp = rdrsdot, rdrsdot = rdrsdot2, rdrsdot2 = temp; /*KLUDGE*/}
	gcflag = TRUE;		/*  flag to indicate in garbage collector  */
	save = eval(gccall1);	/*  call gcafter  */
	gcflag = FALSE;		/*  turn off flag  */
	{lispval temp;temp = rdrsdot, rdrsdot = rdrsdot2, rdrsdot2 = temp; /*KLUDGE*/}

	if(GCtime) {
		times(&finish);
		GCtime += (finish.mytime - begin.mytime);
	}
	return(save);	/*  return result of gcafter  */
	}

	

/*  gc1()  **************************************************************/
/*									*/
/*  Mark-and-sweep phase						*/

gc1(chkflag) int chkflag;
	{
	int j, typep,k;
	register int *start,bvalue,type_len; 
	register struct types *s;
	int *point,i,freecnt,itemstogo,bits,bindex,type,enddat;
	int bytestoclear;
	char *pindex;
	struct heads *loop;
	struct argent *loop2;
	struct nament *loop3;
	struct atom *symb;
	int markdp();
	extern int hashtop;
	int debugin  = FALSE;	/* temp debug flag */
#define ERDB(s) { printf(s); fflush(stdout); }

#ifndef UNIXTS
	vadvise(VA_ANOM); 
	/*  decide whether to check LISP structure or not  */
#endif


#ifdef METER
	vtimes(&premark,0);
	markdpcount = 0;
	conssame = consdiff = consnil = 0;
#endif

	/*  first set all bit maps to zero  */


	if(debugin) ERDB("Begin gc\n");
	/*
	enddat = (int)datalim >> 8;
	for(bvalue=0; bvalue < (int)enddat ; ++bvalue)
	{
	     bitmapq[bvalue] = zeroq; 
	}
	*/

	/* try the movc5 to clear the bit maps */
	/* the maximum number of bytes we can clear in one sweep is
	 * 2^16 (or 1<<16 in the C lingo)
	 */
	bytestoclear = ((((int)datalim)-((int)beginsweep)) >> 9) * 16; 
	if(bytestoclear > MAXCLEAR)
	{ 
	   blzero(((int)&bitmapi[((int)beginsweep>>7)]) + MAXCLEAR, 
			    bytestoclear - MAXCLEAR);
	   bytestoclear = MAXCLEAR;
	}
	blzero(&bitmapi[((int)beginsweep)>>7],bytestoclear);
			
	/* mark all atoms in the oblist */
        for( bvalue=0 ; bvalue <= hashtop-1 ; bvalue++ ) /* though oblist */
        {
	    for( symb = hasht[bvalue] ; symb != (struct atom *) CNIL ;
		      symb = symb-> hshlnk)
	       markdp(symb);
	}


	/* Mark all the atoms and ints associated with the hunk
	   data types */
	   
	for(i=0; i<8; i++) {
		markdp(hunk_items[i]);
		markdp(hunk_name[i]);
		markdp(hunk_pages[i]);
	}
	/* next run up the name stack */
	if(debugin) ERDB("name stack\n");
	for(loop2 = np - 1; loop2 >=  orgnp; --loop2) MARKVAL(loop2->val);	

	/* now the bindstack (vals only, atoms are marked elsewhere ) */
	for(loop3 = bnp - 1; loop3 >= orgbnp; --loop3)MARKVAL(loop3->val);

	if(debugin) ERDB("compiler stuff\n");	
	/* from TBL 29july79  */
	/* next mark all compiler linked data */
	/* dont mark these, they are pure
	 *point = bind_lists;
	 *while((start = point) != (int *)CNIL) {
	 *	if(debugin) ERDB("once ");
	 *	while( *start != -1 )
	 *	{
	 *		markdp(*start);
	 *		start++;
	 *	}
	 *	point = (int *)*(point-1);
	 *}
	 */
	/* end from TBL */

	if(debugin) ERDB("signif stuff\n");
	/* next mark all system-significant lisp data */

	for(i=0; i<SIGNIF; ++i) markdp((lispsys[i]));

#ifdef METER
	vtimes(&presweep,0);
#endif
	if(debugin) printf("time to sweep up\n");	
	/* all accessible data has now been marked. */
	/* all collectable spaces must be swept,    */
	/* and freelists constructed.		    */

	/* first clear the structure elements for types
	 * we will sweep
	 */
	
	for(k=0 ; k <= HUNK128 ; k++)
	{
		if( s=gcableptr[k] )
		{
		  (*(s->items))->i = 0;
		  s->space_left = 0;
		  s->next_free = (char *) CNIL;
		}
	}


	/* sweep up in memory looking at gcable pages */

	for(start = beginsweep,  bindex = (int)start >> 7, 
		  pindex = &purepage[(int)start>>9]; 
	    start < (int *)datalim;
	    start += 128, pindex++)
	{
	    /* printf(" start %x, bindex %x\n",start,bindex); */
	    if(!(s=gcableptr[type = TYPE(start)]) || *pindex) 
	    {   
		bindex += 4;   /* and 4 words of 32 bit bitmap words */
		continue;
	    }

	    freecnt = 0;		/* number of free items found */
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
			}
#endif
			point += 2;
			bvalue = bvalue >> 2;
		    }
		}
		s->next_free = (char *) head;
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
		    /*printf(" bv: %08x, ",bvalue);*/
		    if(!(bvalue & 1))	/* if data element is not marked */
		    {
			freecnt++;
			*point = (int) (s->next_free) ;
			s->next_free = (char *) point;
		    }

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

     /* printf(" t %d,fr %d ",type,freecnt); */
     s->space_left += freecnt;
     (*(s->items))->i += s->space - freecnt;
     }

#ifdef METER
        vtimes(&alldone,0);
	if(gcstat) gcdump();
#endif
#ifndef UNIXTS
	vadvise(VA_NORM); 
#endif
}

/** alloc() *************************************************************/
/*									*/
/*  This routine tries to allocate one more page of the space named	*/
/*  by the argument.  If no more space is available returns 1, else 0.	*/

lispval
alloc(tname,npages)
	lispval tname; int npages;
	{
	int ii, jj;

	ii = typenum(tname);

	if(((int)datalim >> 9) + npages > TTSIZE)
	   error("Space request would exceed maximum memory allocation",FALSE);

	for( jj=0; jj<npages; ++jj)
		if(get_more_space(spaces[ii],FALSE)) break;
	return(inewint(jj));
	}

lispval
csegment(tname,nitems,useholeflag)
lispval tname; int nitems;
{
	int ii, jj;
	char *charadd;

	ii = typenum(tname);

	nitems = nitems*4*spaces[ii]->type_len;	/*  find c-length of space  */
	nitems = roundup(nitems,512);		/*  round up to right length  */
#ifdef HOLE
	if((tname==str_name) && useholeflag)
		charadd = gethspace(nitems,ii);
	else
#endif
	{
		current += nitems/512;
		charadd = sbrk(nitems);
		datalim = (lispval)(charadd+nitems);
	}
	if( (int) charadd == 0 )
		error("NOT ENOUGH SPACE FOR ARRAY",FALSE);
	if((((int)datalim) >> 9) > fakettsize) {
		datalim = (lispval) (fakettsize << 9);
		if(fakettsize >= TTSIZE)
		{
		    printf("There isn't room enough to continue, goodbye\n");
		    franzexit(1);
		}
		fakettsize++;
		badmem(53);
	}
	for(jj=0; jj<nitems; jj=jj+512) {
		SETTYPE(charadd+jj, spaces[ii]->type);
	}
	blzero(charadd,nitems);
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
		if(tname == *(spaces[ii]->type_name)) break;
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
		curhbeg = (char *) roundup(((int)curhbeg),NBPG);
		if((holend - curhbeg) < segsiz)
		{	printf("[fasl hole filled up]\n");
			usehole = FALSE;
		} else {
			value = curhbeg;
			curhbeg = curhbeg + segsiz;
			/*printf("start %d, finish %d, size %d\n",value, curhbeg,segsiz);*/
			return(value);
		}
	}
	value = (ysbrk(segsiz/NBPG,type));
	datalim = (lispval)(value + segsiz);
	return(value);
}
gcrebear()
{
        int i;
#ifdef HOLE
	/* this gets done upon rebirth */
	strng_str.space_left = 0;
	funct_str.space_left = 0;
	funct_str.next_free = (char *) CNIL;
	/* clear pure space pointers */
	for(i = 0; i < NUMSPACES; i++)
	{
	    spaces[i]->next_pure_free = (char *) CNIL;
	}
#endif
}

/** markit(p) ***********************************************************/
/*  just calls markdp							*/

markit(p) lispval *p; { markdp(*p); }

/** markdp(p) ***********************************************************/
/*									*/
/*  markdp is the routine which marks each data item.  If it is a	*/
/*  dotted pair, the car and cdr are marked also.			*/
/*  An iterative method is used to mark list structure, to avoid	*/
/*  excessive recursion.						*/


markdp(p) register lispval p;
	{
/*	register int r, s;	(goes with non-asm readbit, oksetbit)	*/
/*	register hsize, hcntr;						*/
	int hsize, hcntr;

#ifdef METER
	markdpcount++;
#endif
ptr_loop:
	if(p <= (lispval) 0) return;	/*  do not mark special data types or nil=0  */

	switch( TYPE(p) )
		{
		case ATOM:
			ftstbit;
			MARKVAL(p->a.clb);
			MARKVAL(p->a.plist);
			MARKVAL(p->a.fnbnd);
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
			     l = p->ar.length->i; /* number of elements */
			     d = p->ar.delta->i;  /* bytes per element  */
			     for(cnt = 0, p=(lispval)(p->ar.data) ; cnt<l ; 
				      p = (lispval)(((char *) p) + d), cnt++)
			     {
				setbit;
			     }
			}
			else {
/*			register int i, l; int d;		*/
/*			register char *dataptr = p->ar.data;	*/
			int i,l,d;
			char *dataptr = p->ar.data;

			for(i=0, l=p->ar.length->i, d=p->ar.delta->i; i<l; ++i)
				{
				markdp(dataptr);
				dataptr += d;
				}
			return;
			}
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
		xx = sbrk(16*NBPG);	/*  get pages 16 at a time  */
		if( (int)xx== -1 )
			lispend("For sbrk from lisp: no space... Goodbye!");
		}
	else xx += NBPG;

	if(n == 0)
	{
	    xcycle++;	/* don't allocate the page */
	    xx -= NBPG;
	    return(xx);	/* just return its address */
	}

	if( (u = (lispval)(xx+NBPG))  > datalim ) datalim = u;
	return(xx);
	}

char *ysbrk(pages,type) int pages, type;
	{
	char *xx;	/*  will point to block of storage  */
	int i;

	xx = sbrk(pages*NBPG);
	if((int)xx == -1)
		error("OUT OF SPACE FOR ARRAY REQUEST",FALSE);

	datalim = (lispval)(xx+pages*NBPG);	/*  compute bit table limit  */

	/*  set type for pages  */

	for(i = 0; i < pages; ++i) {
		SETTYPE((xx + i*NBPG),type);
	}

	return(xx);	/*  return pointer to block of storage  */
	}

#ifdef VMS
/* sbrk - 
 *   this function is used by the VMS franz to allocate space.  
 * It allocates space in the zfreespace array.
 * The single argument passed to sbrk is the number of bytes to allocate
 *
 */

extern char zfreespace[];
extern char *lsbrkpnt;

char *
sbrk(n)
{
	char *result;
	if(lsbrkpnt == (char *)0)
	{  
	   lsbrkpnt = (char *) roundup((int)zfreespace,NBPG);
	}
	result = lsbrkpnt;
/*	printf("lispbrk: %x \n",lsbrkpnt);
	fflush(stdout);  */
	lsbrkpnt += n;
	if(lsbrkpnt > &zfreespace[FREESIZE])
	  error("sbrk: out of space ",FALSE);
	return(result);
}
#endif
/* getatom **************************************************************/
/* returns either an existing atom with the name specified in strbuf, or*/
/* if the atom does not already exist, regurgitates a new one and       */
/* returns it.                                                          */
lispval
getatom()
{   register lispval aptr;
    register char *name, *endname;
    register int hash;
    register struct argent *lbot, *np;
    lispval	b;
    char	c;

	name = strbuf;
	if (*name == (char)0377) return (eofa);
	hash = hashfcn(name);
	atmlen = strlen(name) + 1;
	aptr = (lispval) hasht[hash];
	while (aptr != CNIL)
	    if (strcmp(name,aptr->a.pname)==0)
		return (aptr);
	    else
		aptr = (lispval) aptr->a.hshlnk;
	aptr = (lispval) newatom();
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
				aptr->a.fnbnd = unprot();
				break;
				}
			b->d.car= newdot();
			b= b->d.car;
			if((c = *name) == 'a') b->d.car = cara;
			else if (c == 'd') b->d.car = cdra;
			else{ unprot();
			   break;
			 }
			}
		}

	return(aptr);
	}

/* our hash function */

hashfcn(symb)
char *symb;
{
	register int i;
	for (i=0 ; *symb ; i += i + *symb++);
	return(i & (HASHTOP-1));
}

extern struct atom *hasht[HASHTOP];
#ifdef VMS
vadvise(){}
#endif
