#ifndef lint
static char *rcsid =
   "$Header: fasl.c,v 1.11 87/12/14 16:49:06 sklower Exp $";
#endif

/*					-[Thu Jun  2 21:44:26 1983 by jkf]-
 * 	fasl.c				$Locker:  $
 * compiled lisp loader
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"
#include <sys/types.h>
#include "lispo.h"
#include "chkrtab.h"
#include "structs.h"
#include "frame.h"

/* fasl  -  fast loader				j.k.foderaro
 * this loader is tuned for the lisp fast loading application
 * any changes in the system loading procedure will require changes
 * to this file
 *
 *  The format of the object file we read as input:
 *  text segment:
 *    1) program text - this comes first.
 *    2) binder table - one word entries, see struct bindage
 *			begins with symbol:  bind_org
 *    3) litterals - exploded lisp objects. 
 *			begins with symbol:  lit_org
 *		        ends with symbol:    lit_end
 * data segment:
 *	not used
 *
 *
 *  these segments are created permanently in memory:
 *	code segment - contains machine codes to evaluate lisp functions.
 *	linker segment - a list of pointers to lispvals.  This allows the
 *			compiled code to reference constant lisp objects.
 *		  	The first word of the linker segment is a gc link
 *			pointer and does not point to a literal.  The
 *			symbol binder is assumed to point to the second
 *			longword in this segment.  The last word in the
 *			table is -1 as a sentinal to the gc marker.
 *			The number of real entries in the linker segment 
 *			is given as the value of the linker_size symbol.  
 *			Taking into account the 2 words required for the
 *			gc, there are 4*linker_size + 8 bytes in this segment.
 *	transfer segment - this is a transfer table block.  It is used to
 *			allow compiled code to call other functions 
 *			quickly.  The number of entries in the transfer table is
 *			given as the value of the trans_size symbol.
 *
 *  the following segments are set up in memory temporarily then flushed
 *	binder segment -  a list of struct bindage entries.  They describe
 *			what to do with the literals read from the literal
 *			table.  The binder segment begins in the file
 *			following the bindorg symbol.
 *	literal segment - a list of characters which _Lread will read to 
 *			create the lisp objects.  The order of the literals
 *			is:
 *		         linker literals - used to fill the linker segment.
 *			 transfer table literals - used to fill the 
 *			   transfer segment
 *			 binder literals - these include names of functions
 *			   to bind interspersed with forms to evaluate.
 *			   The meanings of the binder literals is given by
 *			   the values in the binder segment.
 * 	string segment - this is the string table from the file.  We have
 *			 to allocate space for it in core to speed up
 *			 symbol referencing.
 *
 */


/* external functions called or referenced */

lispval qcons(),qlinker(),qget();
int _qf0(), _qf1(), _qf2(), _qf3(), _qf4(), _qfuncl(), svkludg(),qnewint();
int qnewdoub(),qoneplus(),qoneminus(), wnaerr();
lispval Lread(), Lcons(), Lminus(), Ladd1(), Lsub1(), Lplist(), Lputprop();
lispval Lprint(), Lpatom(), Lconcat(), Lget(), Lmapc(), Lmapcan();
lispval Llist(), Ladd(), Lgreaterp(), Lequal(), Ltimes(), Lsub(), Ipurcopy();
lispval Lncons(), Ibindvars(), Iunbindvars(),error();
int Inonlocalgo();
lispval Istsrch();
int mcount(), qpushframe();
extern int mcnts[],mcntp,doprof;

extern lispval *tynames[];
extern struct frame *errp;
extern char _erthrow[];

extern int initflag;		/* when TRUE, inhibits gc */

char *alloca();			/* stack space allocator */

/* mini symbol table, contains the only external symbols compiled code
   is allowed to reference
 */


struct ssym { char *fnam;	/* pointer to string containing name */
	      int  floc;	/* address of symbol */
	      int  ord;		/* ordinal number within cur sym tab */

	      } Symbtb[] 
		          = {
			     "trantb",	0,	-1,   /* must be first */
			     "linker",  0,	-1,   /* must be second */
			     "mcount",	  (int) mcount,   -1,
			     "mcnts",	  (int) mcnts,  -1,
			     "_wnaerr",   (int) wnaerr, -1,
			     "_qnewint",   (int) qnewint,  -1,
			     "_qnewdoub",   (int) qnewdoub,  -1,
			     "_qcons",	  (int) qcons,    -1,
			     "_qoneplus", (int) qoneplus, -1,
			     "_qoneminus", (int) qoneminus, -1,
			     "_typetable",  (int) typetable,  -1,
			     "_tynames",  (int) tynames,  -1,
			     "_qget",	  (int) qget,	  -1,
			     "_errp",     (int) &errp,     	-1,
			     "_Inonlocalgo",  (int) Inonlocalgo, -1,
			     "__erthrow",  (int) _erthrow,  	-1,
			     "_error",    (int) error,    	-1,
			     "_qpushframe",  (int) qpushframe,  -1,
			     "_retval",		(int)&retval,	-1,
			     "_lispretval",	(int)&lispretval,-1,
#ifndef NPINREG
			     "_np",	  (int) &np,	  -1,
			     "_lbot",	  (int) &lbot,	  -1,
#endif
#ifndef NILIS0
			     "_nilatom",  (int) &nilatom, -1,
#endif
			     "_bnp",	  (int) &bnp,	  -1,
			     "_Ibindvars", (int) Ibindvars, -1,
			     "_Iunbindvars", (int) Iunbindvars, -1
			     };

#define SYMMAX ((sizeof Symbtb) / (sizeof (struct ssym)))

#define roundup(x) (char *)(((int)x + 3) & ~3) /* round to longword boundary */

struct nlist syml;		/* to read a.out symb tab */
extern int *bind_lists;		/* gc binding lists 	  */

/* bindage structure:
 *  the bindage structure describes the linkages of functions and name,
 *  and tells which functions should be evaluated.  It is mainly used 
 *  for the non-fasl'ing of files, we only use one of the fields in fasl
 */
struct bindage
{
     int     b_type;			/* type code, as described below */
};

/* the possible values of b_type
 * -1 - this is the end of the bindage entries
 * 0  - this is a lambda function
 * 1  - this is a nlambda function
 * 2  - this is a macro function
 * 99 - evaluate the string
 *
 */


extern struct trtab *trhead;	/* head of list of transfer tables	    */
extern struct trent *trcur;	/* next entry to allocate		    */
extern int trleft;		/* # of entries left in this transfer table */

struct trent *gettran();	/* function to allocate entries */

/* maximum number of functions */
#define MAXFNS 2000

lispval Lfasl()
{
	extern int holend,usehole;
	extern int uctolc;
	extern char *curhbeg;
	struct argent *svnp;
	struct exec exblk;	/* stores a.out header */
	FILE *filp, *p, *map, *fstopen(); 	/* file pointer */
	int domap,note_redef;
	lispval handy,debugmode;
	struct relocation_info reloc;
	struct trent *tranloc;
	int trsize;
	int i,j,times, *iptr;
	int  funloc[MAXFNS];	/* addresses of functions rel to txt org */
	int funcnt = 0;

	/* symbols whose values are taken from symbol table of .o file */
	int bind_org = 0;		/* beginning of bind table */
	int lit_org = 0;	/* beginning of literal table */
	int lit_end;		/* end of literal table  */
	int trans_size = 0;	/* size in entries of transfer table */
	int linker_size;	/* size in bytes   of linker table 
					(not counting gc ptr) */

       /* symbols which hold the locations of the segments in core and 
	* in the file
	*/
	char *code_core_org,	/* beginning of code segment */
	     *lc_org,  /* beginning of linker segment */
	     *lc_end,  /* last word in linker segment */
	     *literal_core_org, /* beginning of literal table   */
	     *binder_core_org,  /* beginning of binder table   */
	     *string_core_org;

	int /*string_file_org,	/* location of string table in file */
	    string_size,	/* number of chars in string table */
	    segsiz;		/* size of permanent incore segment */

	char *symbol_name;
	struct bindage *curbind;
	lispval rdform, *linktab;
	int ouctolc;
	int debug = 0;
	lispval currtab,curibase;
	char ch,*filnm,*nfilnm;
	char tempfilbf[100];
	char *strcat();
	long lseek();
	Keepxs();
	

	switch(np-lbot) {
	case 0:
		protect(nil);
	case 1:
		protect(nil);
	case 2:
		protect(nil);
	case 3:
		break;
	default:
		argerr("fasl");
	}
	filnm = (char *) verify(lbot->val,"fasl: non atom arg");


	domap = FALSE;
	/* debugging */
	debugmode = Istsrch(matom("debugging"))->d.cdr->d.cdr->d.cdr;
	if (debugmode != nil) debug = 1;
        /* end debugging */


	/* insure that the given file name ends in .o
	   if it doesnt, copy to a new buffer and add a .o
	   but Allow non .o file names (5mar80 jkf)
	*/
	tempfilbf[0] = '\0';
	nfilnm = filnm;		/* same file name for now */
	if( (i = strlen(filnm)) < 2 ||
	     strcmp(filnm+i-2,".o") != 0)
	{
		strncat(tempfilbf,filnm,96);
		strcat(tempfilbf,".o");
		nfilnm = tempfilbf;
	}

	if ( (filp = fopen(nfilnm,"r")) == NULL)
	   if ((filnm == nfilnm) || ((filp = fopen(filnm,"r")) == NULL))
	       errorh1(Vermisc,"Can't open file",nil,FALSE,9797,lbot->val);

	if ((handy = (lbot+1)->val) != nil )
	{
	    if((TYPE(handy) != ATOM )   ||
	       (map = fopen(handy->a.pname,
			    (Istsrch(matom("appendmap"))->d.cdr->d.cdr->d.cdr == nil 
				    ? "w" : "a")))  == NULL)
		error("fasl: can't open map file",FALSE);
	    else 
	    {	domap = TRUE;
		/* fprintf(map,"Map of file %s\n",lbot->val->a.pname); */
	    }
	}

	/* set the note redefinition flag */
	if((lbot+2)->val != nil) note_redef = TRUE;
	else    note_redef = FALSE;

	/* if nil don't print fasl message */
	if ( Vldprt->a.clb != nil ) {
		printf("[fasl %s]",filnm);
		fflush(stdout);
	}
	svnp = np;



	/* clear the ords in the symbol table */
	for(i=0 ; i < SYMMAX ; i++) Symbtb[i].ord = -1;

	if( read(fileno(filp),(char *)&exblk,sizeof(struct exec)) 
		!= sizeof(struct exec))
	  error("fasl: header read failed",FALSE);
	  
	/* check that the magic number is valid	*/

	if(exblk.a_magic != 0407)
	   errorh1(Vermisc,"fasl: file is not a lisp object file (bad magic number): ",
	   	nil,FALSE,0,lbot->val);

	/* read in string table */
	lseek(fileno(filp),(long)(/*string_file_org =*/N_STROFF(exblk)),0);
	if( read(fileno(filp), (char *)&string_size , 4) != 4)
	  error("fasl: string table read error, probably old fasl format", FALSE);
	
	lbot = np; 		/* set up base for later calls */
        /* allocate space for string table on the stack */
	string_core_org = alloca(string_size - 4);

	if( read(fileno(filp), string_core_org , string_size - 4)
		!= string_size -4) error("fasl: string table read error ",FALSE);
	/* read in symbol table and set the ordinal values */

	fseek(filp,(long) (N_SYMOFF(exblk)),0);

	times = exblk.a_syms/sizeof(struct nlist);
	if(debug) printf(" %d symbols in symbol table\n",times);

	for(i=0; i < times ; i++)
	{
	   if( fread((char *)&syml,sizeof(struct nlist),1,filp) != 1)
	       error("fasl: Symb tab read error",FALSE);
	
	   symbol_name = syml.n_un.n_strx - 4 + string_core_org;
	   if(debug) printf("symbol %s\n read\n",symbol_name);
	   if (syml.n_type == N_EXT) 
	   { 
	      for(j=0; j< SYMMAX; j++)
	      {
	         if((Symbtb[j].ord < 0) 
			  && strcmp(Symbtb[j].fnam,symbol_name)==0)
	         {    Symbtb[j].ord = i;
		      if(debug)printf("symbol %s ord is %d\n",symbol_name,i);
		      break;
	         };

	      };

	      if( j>=SYMMAX )  printf("Unknown symbol %s\n",symbol_name);
	   }
	   else if (((ch = symbol_name[0]) == 's')
		     || (ch == 'L')
		     || (ch == '.') )  ;		/* skip this */
	   else if (symbol_name[0] == 'F')
	   {
	       if(funcnt >= MAXFNS)
	       		error("fasl: too many function in file",FALSE);
	       funloc[funcnt++] = syml.n_value;		/* seeing function */
	   }
	   else if (!bind_org && (strcmp(symbol_name, "bind_org") == 0))
	     bind_org = syml.n_value;
	   else if (strcmp(symbol_name, "lit_org") == 0)
	     lit_org = syml.n_value;
	   else if (strcmp(symbol_name, "lit_end") == 0)
	     lit_end = syml.n_value;
	   else if (strcmp(symbol_name, "trans_size") == 0)
	     trans_size = syml.n_value;
	   else if (strcmp(symbol_name, "linker_size") == 0)
	     linker_size = syml.n_value;
	}

#if m_68k
	/* 68k only, on the vax the symbols appear in the correct order */
	{ int compar();
	  qsort(funloc,funcnt,sizeof(int),compar);
	}
#endif

	if (debug)
	  printf("lit_org %x,  lit_end %x, bind_org %x, linker_size %x\n",
		lit_org, lit_end, bind_org, linker_size);
	/* check to make sure we are working with the right format */
	if((lit_org == 0) || (lit_end == 0))
	   errorh1(Vermisc,"File not in new fasl format",nil,FALSE,0,lbot->val);

        /*----------------*/

	/* read in text segment  up to beginning of binder table */

	segsiz = bind_org + 4*linker_size + 8 + 3; /* size is core segment size
						 * plus linker table size
						 * plus 2 for gc list
						 * plus 3 to round up to word
						 */

	lseek(fileno(filp),(long)sizeof(struct exec),0);
	code_core_org = (char *) csegment(OTHER,segsiz,TRUE);
	if(read(fileno(filp),code_core_org,bind_org) != bind_org)
	    error("Read error in text ",FALSE);

  if(debug) {
	printf("Read %d bytes of text into 0x%x\n",bind_org,code_core_org);
	 printf(" incore segment size: %d (0x%x)\n",segsiz,segsiz);
	 }
	 
	/* linker table is 2 entries (8 bytes) larger than the number of
	 * entries given by linker_size .  There must be a gc word at
	 * the beginning and a -1 at the end
	 */
	lc_org = roundup(code_core_org + bind_org);
	lc_end = lc_org + 4*linker_size + 4; 
					/* address of gc sentinal last */

	if(debug)printf("lin_cor_org: %x, link_cor_end %x\n",
				      lc_org,
				      lc_end);
	Symbtb[1].floc = (int) (lc_org + 4);

	/* set the linker table to all -1's so we can put in the gc table */
	for( iptr = (int *)(lc_org + 4 ); 
	     iptr <= (int *)(lc_end); 
	     iptr++)
	  *iptr = -1;


	/* link our table into the gc tables */
	/* only do so if we will not purcopy these tables */
	if(Vpurcopylits->a.clb == nil)
	{
	    *(int *)lc_org = (int)bind_lists;	/* point to current */
	    bind_lists = (int *) (lc_org + 4); /* point to first
	    							item */
	}

	/* read the binder table and literals onto the stack */

	binder_core_org =  alloca(lit_end - bind_org);
	read(fileno(filp),binder_core_org,lit_end-bind_org);

	literal_core_org = binder_core_org + lit_org - bind_org;

	/* check if there is a transfer table required for this
	 * file, and if so allocate one of the necessary size
	 */

	if(trans_size > 0)
	{
	    tranloc = gettran(trans_size);
	    Symbtb[0].floc = (int) tranloc;
	}

	/* now relocate the necessary symbols in the text segment */

	fseek(filp,(long)(sizeof(struct exec) + exblk.a_text + exblk.a_data),0);
	times = (exblk.a_trsize)/sizeof(struct relocation_info);
		
	/* the only symbols we will relocate are references to  
		external symbols.  They are recognized by 
		extern and pcrel set.
	 */

        for( i=1; i<=times ; i++)
	    {
		if( fread((char *)&reloc,sizeof(struct relocation_info),1,filp) != 1)
		   error("Bad text reloc read",FALSE);
	     if(reloc.r_extern)
	     {
	        for(j=0; j < SYMMAX; j++)
		{

		   if(Symbtb[j].ord == reloc.r_symbolnum)  /* look for this sym */
		    {
#define offset(p) (((p).r_pcrel) ? ((int) code_core_org): 0)
		      if(debug) printf("Relocating %d (ord %d) at %x\n",
					 j, Symbtb[j].ord, reloc.r_address);
			if (Symbtb[j].floc == (int)  mcnts) {
			    add_offset((int *)(code_core_org + reloc.r_address),
		               		mcntp - offset(reloc)); 
			    if(doprof){
			     if (mcntp == (int) &mcnts[NMCOUNT-2])
				printf("Ran out of counters; increas NMCOUNT in fasl.c\n");
			     if (mcntp < (int) &mcnts[NMCOUNT-1])
			        mcntp += 4;
			    }
			} else
		            add_offset((int *)(code_core_org + reloc.r_address),
		               		Symbtb[j].floc - offset(reloc)); 
			  
		        break;
		      
		      }
		 };
		 if( j >= SYMMAX) if(debug) printf("Couldnt find ord # %d\n",
						   reloc.r_symbolnum);
	     }

	    }
	
	if ( Vldprt->a.clb != nil ) {
		putchar('\n');
		fflush(stdout);
	}

	/* set up a fake port so we can read from core */
	/* first find a free port 	 	       */

	p = fstopen((char *) literal_core_org, lit_end - lit_org, "r");

	if(debug)printf("lit_org %d, charstrt  %d\n",lit_org, p->_base);
 	/* the first forms we wish to read are those literals in the 
	 * literal table, that is those forms referenced by an offset
	 * from r8 in  compiled code
	 */

	/* to read in the forms correctly, we must set up the read table
	 */
	currtab = Vreadtable->a.clb;
	Vreadtable->a.clb = strtab;		/* standard read table */
	curibase = ibase->a.clb;
	ibase->a.clb = inewint(10);		/* read in decimal */
	ouctolc = uctolc; 	/* remember value of uctolc flag */

	PUSHDOWN(gcdis,tatom);			/* turn off gc */

	i = 1;	
	linktab = (lispval *)(lc_org +4);
	while (linktab < (lispval *)lc_end)
	{
	   np = svnp;
	   protect(P(p));
	   uctolc = FALSE;
	   handy = (lispval)Lread();
	   if (Vpurcopylits->a.clb != nil) {
		handy = Ipurcopy(handy);
	   }
	   uctolc = ouctolc;
	   getc(p);			/* eat trailing blank */
	   if(debugmode != nil)
	   {   printf("form %d read: ",i++);
	       printr(handy,stdout); 
	       putchar('\n');
	       fflush(stdout);
	   }
	   *linktab++ = handy;
	}

	/* process the transfer table if one is used		*/
	trsize = trans_size;
	while(trsize--)
	{
	    np = svnp;
	    protect(P(p));
	    uctolc = FALSE;
	    handy = Lread();	    /* get function name */
	    uctolc = ouctolc;
	    getc(p);
	    tranloc->name = handy;
	    tranloc->fcn = qlinker;	/* initially go to qlinker */
	    tranloc++;
	}



	/* now process the binder table, which contains pointers to 
	   functions to link in and forms to evaluate.
	*/
	funcnt = 0;

	curbind = (struct bindage *) binder_core_org;
	for( ; curbind->b_type != -1 ; curbind++) 
	{
	    np = svnp;
	    protect(P(p));
	    uctolc = FALSE;		/* inhibit uctolc conversion */
	    rdform = Lread();
	    /* debugging */
	    if(debugmode != nil) { printf("link form read: ");
			printr(rdform,stdout);
			printf("  ,type: %d\n",
				 curbind->b_type);
			fflush(stdout);
		      }
	    /* end debugging */
	    uctolc = ouctolc;		/* restore previous state */
	    getc(p);			/* eat trailing null */
	    protect(rdform);
	    if(curbind->b_type <= 2)	/* if function type */
	    { 
	       handy = newfunct();
	       if (note_redef && (rdform->a.fnbnd != nil))
	       {
		   printr(rdform,stdout);
		   printf(" redefined\n");
	       }
	       rdform->a.fnbnd = handy;
	       handy->bcd.start = (lispval (*)())(code_core_org + funloc[funcnt++]);
	       handy->bcd.discipline =
		  (curbind->b_type == 0 ? lambda :
		       curbind->b_type == 1 ? nlambda :
			  macro);
	       if(domap) {
	           fprintf(map,"%s\n%x\n",rdform->a.pname,handy->bcd.start);
	       }
	    }
	    else {
		Vreadtable->a.clb = currtab;
		ibase->a.clb = curibase;

		/* debugging */
		if(debugmode != nil) {
			printf("Eval: ");
			printr(rdform,stdout);
			printf("\n");
			fflush(stdout);
		};
		/* end debugging */

		eval(rdform);		/* otherwise eval it */

		if(uctolc) ouctolc = TRUE; /* if changed by eval, remember */
		curibase = ibase->a.clb;
		ibase->a.clb = inewint(10);
		Vreadtable->a.clb = strtab;
	   }
	};
	      
	fclose(p);	/* give up file descriptor */

	POP;			/* restore state of gcdisable variable */

	Vreadtable->a.clb = currtab;
	chkrtab(currtab);
	ibase->a.clb = curibase;

	fclose(filp);
	if(domap) fclose(map);
	Freexs();
	return(tatom);
}

#if m_68k
/* function used in qsort for 68k version only */
compar(arg1,arg2)
int *arg1,*arg2;
{
	if(*arg1 < *arg2) return (-1);
        else if (*arg1 == *arg2) return (0);
	else return(1);
}
#endif

/* gettran :: allocate a segment of transfer table of the given size	*/

struct trent *
gettran(size)
{
	struct trtab *trp;
	struct trent *retv;
	int ousehole;
	extern int usehole;

	if(size > TRENTS)
	  error("transfer table too large",FALSE);
	
	if(size > trleft)
	{
	    /* allocate a new transfer table */
	    /* must not allocate in the hole or we cant modify it */
	    ousehole = usehole; /* remember old value */
	    usehole = FALSE;
	    trp = (struct trtab *)csegment(OTHER,sizeof(struct trtab),FALSE);
	    usehole = ousehole;

	    trp->sentinal = 0;		/* make sure the sentinal is 0 */
	    trp->nxtt = trhead;	/* link at beginning of table  */
	    trhead = trp;
	    trcur = &(trp->trentrs[0]);	/* begin allocating here	*/
	    trleft = TRENTS;
	}

	trleft = trleft - size;
	retv = trcur;
	trcur = trcur + size;
	return(retv);
}

/* clrtt :: clear transfer tables, or link them all up;
 * this has two totally opposite functions:
 * 1) all transfer tables are reset so that all function calls will go
 * through qlinker
 * 2) as many transfer tables are set up to point to bcd functions
 *    as possible
 */
clrtt(flag)
{
	/*  flag = 0 :: set to qlinker
	 *  flag = 1 :: set to function bcd binding if possible
	 */
	register struct trtab *temptt;
	register struct trent *tement;
	register lispval fnb;

	for (temptt = trhead; temptt != 0 ; temptt = temptt->nxtt)
	{ 
	    for(tement = &temptt->trentrs[0] ; tement->fcn != 0 ; tement++)
	    {   if(flag == 0 || TYPE(fnb=tement->name->a.fnbnd) != BCD
			     || TYPE(fnb->bcd.discipline) == STRNG)
		tement->fcn =  qlinker;
		else tement->fcn = fnb->bcd.start;
	    }
	}
}

/* chktt - builds a list of transfer table entries which don't yet have
  a function associated with them, i.e if this transfer table entry
  were used, an undefined function error would result
 */
lispval 
chktt()
{
	register struct trtab *temptt;
	register struct trent *tement;
	register lispval retlst,curv;
	Savestack(4);

	retlst = newdot();		/* build list of undef functions */
	protect(retlst);
	for (temptt = trhead; temptt != 0 ; temptt = temptt->nxtt)
	{ 
            for(tement = &temptt->trentrs[0] ; tement->fcn != 0 ; tement++)
	    {
	       if(tement->name->a.fnbnd == nil)
	       {
		  curv= newdot();
		  curv->d.car = tement->name;
		  curv->d.cdr = retlst->d.cdr;
		  retlst->d.cdr = curv;
		}
	     }
	 }
	 Restorestack();
	 return(retlst->d.cdr);
}

/* since the tahoe machine is picky about word/longword alignment
** when it is doing data access but not when doing instruction fetches,
** we have to add the relocation offset in a slightly different manner.
*/
#ifdef tahoe
add_offset(addr, relocoffset)
register int *addr;
{register int r11, r10, r9, r8;
 asm("	cvtbl	(r12), r0");
 asm("	cvtbl	8(fp), r1");
 asm("	cvtbl	1(r12), r8");
 asm("	cvtbl	9(fp), r9");
 asm("	cvtbl	2(r12), r10");
 asm("	cvtbl	10(fp), r11");
 asm("	addb2	11(fp), 3(r12)");	/* add least sig. bytes */
 asm("	adwc	r11, r10");
 asm("	adwc	r9, r8");
 asm("	adwc	r1, r0");
 asm("	cvtlb	r10, 2(r12)");
 asm("	cvtlb	r8, 1(r12)");
 asm("	cvtlb	r0,(r12)");
}
#else
add_offset(addr, relocoffset)
register int *addr;
{
 *addr += relocoffset;
}
#endif
