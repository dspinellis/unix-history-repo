# include "global.h"

# define NUMWORDS TTSIZE * 128  /*  max number of words in P0 space  */
# define BITQUADS TTSIZE * 2	/*  length of bit map in quad words  */

# define ftstbit	asm("	ashl	$-2,r11,r3");\
			asm("	bbcs	r3,_bitmapq,$1");\
			asm("	.byte	4");
/*  define ftstbit	if( readbit(p) ) return; oksetbit;  */
# define readbit(p)	((int)bitmap[r=(int)p>>5] & (s=bitmsk[((int)p>>2)&7]))
# define lookbit(p)	(bitmap[(int)p>>5] & bitmsk[((int)p>>2) & 7])
# define setbit(p)	{bitmap[(int)p>>5] |= bitmsk[((int)p >> 2) & 7];}
# define oksetbit	{bitmap[r] |= s;}

# define readchk(p)	((int)bitfre[(int)p>>5] & bitmsk[((int)p>>2)&7])
# define setchk(p)	{bitfre[(int)p>>5] |= bitmsk[((int)p >> 2) & 7];}

struct heads {
	struct heads *link;
	char *pntr;
}  header[TTSIZE];

FILE * chkport;				/* garbage collection dump file */
lispval datalim;			/*  end of data space */
double bitmapq[BITQUADS];		/*  the bit map--one bit per long  */
double zeroq;				/*  a quad word of zeros  */
char *bitmap = (char *) bitmapq;	/*  byte version of bit map array */
char bitmsk[8]={1,2,4,8,16,32,64,128};  /*  used by bit-marking macros  */
int  *bind_lists = (int *) CNIL;	/*  lisp data for compiled code */

char *xsbrk();


int atmlen;

struct types {
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
} atom_str = {(char *)CNIL,0,ATOMSPP,ATOM,5,&atom_items,&atom_pages,&atom_name,(struct heads *)CNIL},
  strng_str    = {(char *)CNIL,0,STRSPP,STRNG,1,&str_items,&str_pages,&str_name,(struct heads *)CNIL},
  int_str  = {(char *)CNIL,0,INTSPP,INT,1,&int_items,&int_pages,&int_name,(struct heads *)CNIL},
  dtpr_str = {(char *)CNIL,0,DTPRSPP,DTPR,2,&dtpr_items,&dtpr_pages,&dtpr_name,(struct heads *)CNIL},
  doub_str     = {(char *)CNIL,0,DOUBSPP,DOUB,2,&doub_items,&doub_pages,&doub_name,(struct heads *)CNIL},
  array_str   = {(char *)CNIL,0,ARRAYSPP,ARRAY,5,&array_items,&array_pages,&array_name,(struct heads *)CNIL},
  sdot_str = {(char *)CNIL,0,SDOTSPP,SDOT,2,&sdot_items,&sdot_pages,&sdot_name,(struct heads *)CNIL},
  val_str  = {(char *)CNIL,0,VALSPP,VALUE,1,&val_items,&val_pages,&val_name,(struct heads *)CNIL},
  funct_str = {(char *)CNIL,0,BCDSPP,BCD,2,&funct_items,&funct_pages,&funct_name,(struct heads *)CNIL};

extern int initflag; /* starts off TRUE: initially gc not allowed */

int gcflag = FALSE;	/*  TRUE during garbage collection  */

int current = 0;	/* number of pages currently allocated */

#define NUMSPACES 9

static struct types *(spaces[NUMSPACES]) = 
	{&atom_str, &strng_str, &int_str,
	 &dtpr_str, &doub_str, &array_str,
	 &sdot_str, &val_str, &funct_str};


/** get_more_space(type_struct) *****************************************/
/*									*/
/*  Allocates and structures a new page, returning 0.			*/
/*  If no space is available, returns 1.				*/

get_more_space(type_struct)                                 
struct types *type_struct;
{
	int cntr;
	char *start;
	int *loop, *temp;
	lispval p, plim;
	struct heads *next;

	if(initflag == FALSE) 
		/*  mustn't look at plist of plima too soon  */
		{
		while( plim=copval(plima,(lispval)CNIL), TYPE(plim)!=INT )
			copval(plima,error("BAD PAGE LIMIT",TRUE));
		if( plim->i <= current ) return(1);	/*  Can't allocate  */
		}

	if( current >= TTSIZE ) return(2);

	start = xsbrk( NBPG );

	/* bump the page counter for this space */

	++((*(type_struct->pages))->i);

	SETTYPE(start, type_struct->type);  /*  set type of page  */

	type_struct->space_left = type_struct->space;
	next = &header[ current++ ];
	if ((type_struct->type)==STRNG)
		{
		type_struct->next_free = start;
		return(0);  /*  space was available  */
		}
	next->pntr = start;
	next->link = type_struct->first;
	type_struct->first = next;
	temp = loop = (int *) start;
	for(cntr=1; cntr < type_struct->space; cntr++)
		loop = (int *) (*loop = (int) (loop + type_struct->type_len));
	*loop = (int) (type_struct->next_free);
	type_struct->next_free = (char *) temp;

	/*  if type atom, set pnames to CNIL  */

	if( type_struct == &atom_str )
		for(cntr=0, p=(lispval) temp; cntr<atom_str.space; ++cntr)
			{
			p->pname = (char *) CNIL;
			p = (lispval) ((int *)p + atom_str.type_len);
			}
	return(0);  /*  space was available  */
}


/** next_one(type_struct) ************************************************/
/*									*/
/*  Allocates one new item of each kind of space, except STRNG.		*/
/*  If there is no space, calls gc, the garbage collector.		*/
/*  If there is still no space, allocates a new page using		*/
/*  get_more_space(type_struct)						*/

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
		   (gcthresh->i <= current) &&       /* threshhold for gc */
		   ISNIL(copval(gcdis,CNIL)) &&	  /* gc not disabled */
		   (NOTNIL(copval(gcload,CNIL)) || (loading->clb != tatom)) &&
					/* not to collect during load */
		   (initflag == FALSE) &&	/* dont gc during init */
		   (gcflag == FALSE))		  /* don't recurse gc */

			{
			/* fputs("Collecting",poport);
			dmpport(poport);*/
			gc(type_struct);  /*  collect  */
			}

		if( type_struct->next_free != (char *) CNIL ) break;

		if(! (g=get_more_space(type_struct))) break;

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

lispval
newint()
{
	++(int_items->i);
	return(next_one(&int_str));
}

lispval
newdot()
{
	lispval temp;

	++(dtpr_items->i);
	temp = next_one(&dtpr_str);
	temp->car = temp->cdr = nil;
	return(temp);
}

lispval
newdoub()
{
	++(doub_items->i);
	return(next_one(&doub_str));
}

lispval
newsdot()
{
	register lispval temp;
	++(dtpr_items->i);
	temp = next_one(&sdot_str);
	temp->car = temp->cdr = 0;
	return(temp);
}

struct atom *newatom() {
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
	int atmlen2;

	++(str_items->i);
	atmlen = strlen(strbuf)+1;
	if(atmlen > strng_str.space_left)
		while(get_more_space(&strng_str))
			error("YOU HAVE RUN OUT OF SPACE",TRUE);
	strcpy((save = strng_str.next_free), strbuf);
	atmlen2 = atmlen;
	while(atmlen2 % 4) ++atmlen2;	/*  even up length of string  */
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
	temp->data = (char *)nil;
	temp->accfun = nil;
	temp->aux = nil;
	temp->length = SMALL(0);
	temp->delta = SMALL(0);
	return(temp);
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
	temp->entry = badcall;
	temp->discipline = nil;
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

	if( NOTNIL(lbot->val->car) )
		{
		temp = eval(lbot->val->car);
		if( TYPE(temp) == PORT ) chkport = (FILE *)*temp;
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

	chkport = ISNIL(save) ? poport : (FILE *)*save;
	
	gc1(NOTNIL(copval(gccheck,CNIL)) || (chkport!=poport)); /* mark&sweep */

	/* Now we call gcafter--special case if gc called from LISP */

	if( type_struct == (struct types *) CNIL )
		gccall1->cdr = nil;  /* make the call "(gcafter)" */
	else
		{
		gccall1->cdr = gccall2;
		gccall2->car = *(type_struct->type_name);
		}
	gcflag = TRUE;		/*  flag to indicate in garbage collector  */
	save = eval(gccall1);	/*  call gcafter  */
	gcflag = FALSE;		/*  turn off flag  */

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
	int i, j, typep;
	register int *start, *point;
	struct types *s;
	struct heads *loop;
	struct argent *loop2;
	int markdp();

	
	/*  decide whether to check LISP structure or not  */




	/*  first set all bit maps to zero  */

	for(i=0; i<((int)datalim >> 8); ++i) bitmapq[i] = zeroq;


	/* then mark all atoms' plists, clbs, and function bindings */

	for(loop=atom_str.first; loop!=(struct heads *)CNIL; loop=loop->link)
		for(start=(int *)(loop->pntr), i=1;
			i <= atom_str.space;
			start = start + atom_str.type_len, ++i)
			{

			/* unused atoms are marked with pname == CNIL */
			/* this is done by get_more_space, as well as */
			/* by gc (in the future)		      */

			if(((lispval)start)->pname == (char *)CNIL) continue;
#define MARKSUB(p)	if(nil!=((lispval)start)->p)markdp(((lispval)start)->p);
			MARKSUB(clb);
			MARKSUB(fnbnd);
			MARKSUB(plist);
			}

	/* next run up the name stack */

	for(loop2 = np - 1; loop2 >=  orgnp; --loop2) markdp((loop2->val));	
	/* from TBL 29july79  */
	/* next mark all compiler linked data */
	point = bind_lists;
	while((start = point) != (int *)CNIL) {
		while( *start != -1 )
			markdp(*start++);
		point = (int *)*(point-1);
	}
	/* end from TBL */

	/* next mark all system-significant lisp data */

	for(i=0; i<SIGNIF; ++i) markdp((lispsys[i]));

	/* all accessible data has now been marked. */
	/* all collectable spaces must be swept,    */
	/* and freelists constructed.		    */

	for(i=0; i<NUMSPACES; ++i)
		{
		/* STRINGS do not participate. */
		/* ATOMS dont either (currently) */

		s = spaces[i];
		typep = s->type;
		if((typep==STRNG) || (typep==ATOM)) continue;

		s->space_left = 0;  /* we will count free cells */
		(*(s->items))->i = 0;    /* and compute cells used    */

		/* for each space, traverse list of pages. */

		s->next_free = (char *) CNIL;	/*  reinitialize free list  */

		for(loop = s->first; loop != (struct heads *) CNIL; loop=loop->link)
			{
			/* add another page's worth to use count */

			(*(s->items))->i  += s->space;

			/* for each page, make a list of unmarked data */

			for(j=0, point=(int *)(loop->pntr);
				j<s->space; ++j, point += s->type_len)
				if( ! lookbit(point) )
					{
						/* add to free list */
						/* update pointer to free list*/
						/* update count of free list */

					*point = (int)(s->next_free);
					s->next_free = (char *) point;
					++(s->space_left);
					}
			}
		(*(s->items))->i -= s->space_left;	/* compute cells used */
		}
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

	for( jj=0; jj<npages; ++jj)
		if(get_more_space(spaces[ii])) break;
	return(inewint(jj));
	}

lispval
csegment(tname,nitems)
lispval tname; int nitems;
	{
	int ii, jj;
	char *charadd;

	ii = typenum(tname);

	nitems = nitems*4*spaces[ii]->type_len;	/*  find c-length of space  */
	while( nitems%512 ) ++nitems;		/*  round up to right length  */
	current += nitems/512;
	charadd = sbrk(nitems);
	if( (int) charadd == 0 )
		error("NOT ENOUGH SPACE FOR ARRAY",FALSE);
	(datalim = (lispval)(charadd+nitems));
	if((((int)datalim) >> 9) > TTSIZE) {
		datalim = (lispval) (TTSIZE << 9);
		badmem(53);
	}
	for(jj=0; jj<nitems; jj=jj+512) {
		SETTYPE(charadd+jj, spaces[ii]->type);
	}
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

ptr_loop:
	if((int)p <= 0) return;	/*  do not mark special data types or nil=0  */

	switch( TYPE(p) )
		{
		case INT:
		case DOUB:
/*			setbit(p);*/
			ftstbit;
			return;
		case VALUE:
			ftstbit;
			p = p->l;
			goto ptr_loop;
		case DTPR:
			ftstbit;
			markdp(p->car);
			p = p->cdr;
			goto ptr_loop;

		case ARRAY:
			ftstbit;	/* mark array itself */

			markdp(p->accfun);	/* mark access function */
			markdp(p->aux);		/* mark aux data */
			markdp(p->length);	/* mark length */
			markdp(p->delta);	/* mark delta */

			{
			register int i, l; int d;
			register char *dataptr = p->data;

			for(i=0, l=p->length->i, d=p->delta->i; i<l; ++i)
				{
				markdp(dataptr);
				dataptr += d;
				}
			return;
			}
		case SDOT:
			do {
				ftstbit;
				p = p->CDR;
			} while (p!=0);
			return;

		case BCD:
			ftstbit;
			markdp(p->discipline);
			return;
		}
	return;
	}



char *
xsbrk()
	{
	static char *xx;	/*  pointer to next available blank page  */
	static int cycle = 0;	/*  number of blank pages available  */
	lispval u;			/*  used to compute limits of bit table  */

	if( (cycle--) <= 0 )
		{
		cycle = 15;
		xx = sbrk(16*NBPG);	/*  get pages 16 at a time  */
		if( (int)xx== -1 )
			lispend("For sbrk from lisp: no space... Goodbye!");
		goto done;
		}
	xx += NBPG;
done:	if( (u = (lispval)(xx+NBPG))  > datalim ) datalim = u;
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

/* getatom **************************************************************/
/* returns either an existing atom with the name specified in strbuf, or*/
/* if the atom does not already exist, regurgitates a new one and       */
/* returns it.                                                          */
lispval
getatom()
{   register lispval aptr;
    register char *name, *endname;
    lispval	b;
    char	c;
    register int hash;
    snpand(4);

	name = strbuf;
	if (*name == (char)0377) return (eofa);
	hash = 0;
	for(name=strbuf; *name;) {
		hash ^= *name++;
	}
	hash &= 0177;	/*  make sure no high-order bits have crept in  */
	atmlen = name - strbuf + 1;
	aptr = (lispval) hasht[hash];
	while (aptr != CNIL)
	    if (strcmp(strbuf,aptr->pname)==0)
		return (aptr);
	    else
		aptr = (lispval) aptr->hshlnk;
	aptr = (lispval) newatom();
	aptr->hshlnk = hasht[hash];
	hasht[hash] = (struct atom *) aptr;
	endname = name - 1;
	name = strbuf;
	if ((atmlen != 4) && (*name == 'c') && (*endname == 'r'))
		{
		b = newdot();
		protect(b);
		b->car = lambda;
		b->cdr = newdot();
		b = b->cdr;
		b->car = newdot();
		(b->car)->car = xatom;
		while(TRUE)
			{
			b->cdr = newdot();
			b= b->cdr;
			if(++name == endname)
				{
				b->car= (lispval) xatom;
				aptr->fnbnd = unprot();
				break;
				}
			b->car= newdot();
			b= b->car;
			if((c = *name) == 'a') b->car = cara;
			else if (c == 'd') b->car = cdra;
			else{ unprot();
			   break;
			 }
			}
		}

	return(aptr);
	}

