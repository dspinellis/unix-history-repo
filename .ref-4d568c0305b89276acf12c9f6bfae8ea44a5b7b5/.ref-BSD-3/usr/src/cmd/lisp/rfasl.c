#include "global.h"
#include <stdio.h>
#include <a.out.h>
#include "chkrtab.h"

/* rfasl  - really fast loader		j.k.foderaro
 * this loader is tuned for the lisp fast loading application
 * any changes in the system loading procedure will require changes
 * to this file
 * Nov 4, 1979 - this now becomes fasl to the lisp world
 */



/* global variables to keep track of allocation */

int curps ;

/* external functions called or referenced */

int _qf0(), _qf1(), _qf2(), _qf3(), _qf4(), _qfuncl(), svkludg();
lispval Lread(), Lcons(), Lminus(), Ladd1(), Lsub1(), Lplist(), Lputprop();
lispval Lprint(), Lpatom(), Lconcat(), Lget(), Lmapc(), Lmapcan();
lispval Llist(), Ladd(), Lgreaterp(), Lequal(), Ltimes(), Lsub();
lispval Lncons();
lispval Idothrow(),error();
extern lispval *tynames[];
extern int errp;
extern char _erthrow[];
extern char setsav[];

extern int initflag;		/* when TRUE, inhibits gc */
/* prelud to linker table in data segment  
 * these locations always begin the data segment, if there is any change
 * to the compiler, this must be fixed up.
 *
 */


#define PRESIZ (8*4)

struct prelud
{
	int dummy[PRESIZ/4];
} prel = {
       (int) &bnp,
       (int) _qfuncl,
       (int) _qf4,
       (int) _qf3,
       (int) _qf2,
       (int) _qf1,
       (int) _qf0,
       (int) 0 };
/* mini symbol table, contains the only external symbols compiled code
   is allowed to reference
 */

#define SYMMAX 35
struct ssym { char *fnam;	/* pointer to string containing name */
	      int  floc;	/* address of symbol */
	      int  ord;		/* ordinal number within cur sym tab */

	      } symbtb[SYMMAX] 
		          = {
			     "_Lminus",   (int) Lminus,   -1,
			     "_Ladd1",    (int) Ladd1,    -1,
			     "_Lsub1",    (int) Lsub1,    -1,
			     "_Lplist",   (int) Lplist,   -1,
			     "_Lcons",    (int) Lcons,    -1,
			     "_Lputpro", (int) Lputprop, -1,
			     "_Lprint",   (int) Lprint,   -1,
			     "_Lpatom",   (int) Lpatom,   -1,
			     "_Lread",    (int) Lread,    -1,
			     "_Lconcat",  (int) Lconcat,  -1,
			     "_Lget",     (int) Lget,     -1,
			     "_Lmapc",    (int) Lmapc,    -1,
			     "_Lmapcan",  (int) Lmapcan,  -1,
			     "_Llist",    (int) Llist,    -1,
			     "_Ladd",     (int) Ladd,     -1,
			     "_Lgreate",(int) Lgreaterp,-1,
			     "_Lequal",   (int) Lequal,   -1,
			     "_Ltimes",   (int) Ltimes,   -1,
			     "_Lsub",     (int) Lsub,     -1,
			     "_Lncons",   (int) Lncons,   -1,
			     "_typetab",  (int) typetab,  -1,
			     "_tynames",  (int) tynames,  -1,
			     "_errp",     (int) &errp,     -1,
			     "_Idothro",  (int) Idothrow, -1,
			     "__erthro",  (int) _erthrow,  -1,
			     "_error",    (int) error,    -1,
			     "_bnp",	  (int) &bnp,	  -1,
			     "__qfuncl",  (int) _qfuncl,  -1,
			     "__qf4",	  (int) _qf4,     -1,
			     "__qf3",     (int) _qf3,     -1,
			     "__qf2",     (int) _qf2,     -1,
			     "__qf1",     (int) _qf1,     -1,
			     "__qf0",     (int) _qf0,     -1,
			     "_setsav",	  (int) setsav,   -1,
			     "_svkludg",  (int) svkludg,  -1
			     };

struct nlist syml;		/* to read a.out symb tab */
extern lispval *bind_lists;	/* gc binding lists 	  */

/* bindage structure:
 *  the bindage structure describes the linkages of functions and name,
 *  and tells which functions should be evaluated.  It is mainly used 
 *  for the non-fasl'ing of files, we only use one of the fields in fasl
 */
struct bindage
{
     lispval (*b_entry)();		/* function entry point */
     int     b_atmlnk;			/* pointer to string    */
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

/* maximum number of functions */
#define MAXFNS 500		

lispval Lfasl()
{
	register int orgtx,orgdt,orgps;
	register struct argent *svnp, *lbot, *np;
	struct exec exblk;	/* stores a.out header */
	FILE *filp, *p, *map; 	/* file pointer */
	int domap;
	lispval handy;
	struct relocation_info reloc;
	struct prelud *ppre;
 	lispval disp;
	int i,j,times, *iptr, oldinitflag;
	int  funloc[MAXFNS];	/* addresses of functions rel to txt org */
	int funcnt = 0;

			/* unrelocated start and end of litteral table */
	int litstrt = 0 , litend = 0;

	int segdif;
	struct bindage *bindorg, *curbind;
	int linkerloc, bindloc = 0 , typer,linkstrt,linkend;
	lispval rdform, *linktab;
	int segsiz;
	int debug = 0;
	lispval currtab,curibase;
	char ch;


	chkarg(2);
	if (TYPE(lbot->val) != ATOM) error("non atom arg",FALSE);

	if ( (filp = fopen((lbot->val)->pname,"r")) == NULL)
	    errorh(Vermisc,"Can't open file",nil,FALSE,9797,lbot->val);

	domap = FALSE;
	if ((handy = (lbot+1)->val) != nil )
	{
	    if((TYPE(handy) != ATOM )   ||
	       (map = fopen(handy->pname,"w"))  == NULL)
		error("rfasl: can't open map file",FALSE);
	    else 
	    {	domap = TRUE;
		fprintf(map,"Map of file %s\n",lbot->val->pname);
	    }
	}

	printf("[fasl %s]",lbot->val->pname);
	fflush(stdout);
	svnp = np;

	lbot = np; 		/* set up base for later calls */


	/* clear the ords in the symbol table */
	for(i=0 ; i < SYMMAX ; i++) symbtb[i].ord = -1;

	if( fread(&exblk,sizeof(struct exec),1,filp) != 1)
	  error("Read failed",FALSE);
	  

	/* read in symbol table and set the ordinal values */

	fseek(filp,
	      (long)(32+exblk.a_text+exblk.a_data+exblk.a_trsize+exblk.a_drsize)
	      ,0);

	times = exblk.a_syms/sizeof(struct nlist);
	if(debug) printf(" %d symbols in symbol table\n",times);

	for(i=0; i < times ; i++)
	{
	   if( fread(&syml,sizeof(struct nlist),1,filp) != 1)


	       error("Symb tab read error",FALSE);
	
	   if (syml.n_type == N_EXT) 
	   { 
	      for(j=0; j< SYMMAX; j++)
	      {
	         if((symbtb[j].ord < 0) 
			  && strcmpn(symbtb[j].fnam,syml.n_name,8)==0)
	         {    symbtb[j].ord = i;
		      if(debug)printf("symbol %s ord is %d\n",syml.n_name,i);
		      break;
	         };

	      };

	      if( j>=SYMMAX )  printf("Unknown symbol %s\n",syml.n_name);
	   }
	   else if (((ch = syml.n_name[0]) == 's')
		     || (ch == 'L')
		     || (ch == '.') )  ;		/* skip this */
	   else if (syml.n_name[0] == 'F')
	       funloc[funcnt++] = syml.n_value;		/* seeing function */
	   else if (!bindloc && (strcmp(syml.n_name, "BINDER") == 0))
	     bindloc = syml.n_value;
	   else if (strcmp(syml.n_name, "litstrt") == 0)
	     litstrt = syml.n_value;
	   else if (strcmp(syml.n_name, "litend") == 0)
	     litend = syml.n_value;
	}

	/* check to make sure we are working with the right format */
	if((litstrt == 0) || (litend == 0))
	   errorh(Vermisc,"File not in new fasl format",nil,FALSE,0,lbot->val);

        /*----------------*/

	/* read in text segment */


	fseek(filp,(long)32,0);
	segsiz = exblk.a_text + exblk.a_data;
	if(fread(curps = (int) csegment(int_name,segsiz/sizeof(int))
		 ,1,exblk.a_text,filp) != exblk.a_text)
	    error("Read error in text and data read",FALSE);

	orgtx = curps;
	orgdt = curps + exblk.a_text;

	linkstrt = orgdt + PRESIZ;		/* start of linker table */
	linkend  = orgdt + exblk.a_data - 4;	/* end of linker table */

	/* the object file is a 410 file and thus has seperate text and
	   data segments.  The data is assumed to be loaded at the start
	   of the next PAGSIZ byte boundary, we must calculate the difference
	   between where the data segment begins and where the loader
	   thinks it begins.  Caclulate by rounding up the text size and
	   seeing how much is skipped
	*/
	segdif = ((exblk.a_text + PAGRND) & ~PAGRND) - exblk.a_text;
	if(debug) printf("funcs %d, orgtx %x, orgdt %x, linkstrt %x, linkend %x segdif %x",
		      funcnt,orgtx,orgdt,linkstrt,linkend,segdif);

	/* set the linker table to all -1's so we can put in the gc table */
	for( iptr = (int *)linkstrt ; iptr <= (int *)linkend ; iptr++)
	  *iptr = -1;

	/* copy in the prelud */
	ppre = (struct prelud *) orgdt;		/* use structure to copy */
	*ppre = prel;				/* copy over prelud */

	/* link our table into the gc tables */
	*( ((int *)linkstrt) -1) = (int)bind_lists;	/* point to current */
	bind_lists = (lispval *) linkstrt;

	/* new relocate the necessary symbols in the text segment */

	orgps = orgtx;
	fseek(filp,(long)(32+exblk.a_text+exblk.a_data),0);
	times = (exblk.a_trsize)/sizeof(struct relocation_info);
		
	/* the only symbols we will relocate are references to  lisp
	   1) functions like _Lcons 
	   2) the symbol linker in the data segment

	  type (1) can be recognized by extern and pcrel, while
	  type (2) can be recognized by !extern and pcrel and data segment
	 */

        for( i=1; i<=times ; i++)
	    {
		if( fread(&reloc,sizeof(struct relocation_info),1,filp) != 1)
		   error("Bad text reloc read",FALSE);
	     if(reloc.r_extern && reloc.r_pcrel)
	     {
	        for(j=0; j < SYMMAX; j++)
		{

		   if(symbtb[j].ord == reloc.r_symbolnum)  /* look for this sym */
		    {
		      if(debug) printf("Relocating %d (ord %d) at %x\n",
					 j, symbtb[j].ord, reloc.r_address);
		            *(int *)(orgps+reloc.r_address) 
		               += symbtb[j].floc - orgtx; 
			  
		            break;
		      
		          }
		 };
		 if( j >= SYMMAX) if(debug) printf("Couldnt find ord # %d\n",
						   reloc.r_symbolnum);
	     }
	     else if(!reloc.r_extern && reloc.r_pcrel && 
				     (reloc.r_symbolnum == N_DATA))
	     {  if(debug) printf("relocing at addr %x \n",reloc.r_address);
		*(int *)(orgps + reloc.r_address) -= segdif;
	     }

	    }
	
        putchar('\n');
	fflush(stdout);

	/* set up a fake port so we can read from core */
	/* first find a free port 	 	       */

	p = stdin;
	for( ; p->_flag & (_IOREAD|_IOWRT) ; p++)
	   if( p >= _iob + _NFILE)
	       error(" No free file descriptor for fasl ",FALSE);
	       
	p->_flag = _IOREAD | _IOSTRG;
	p->_base = p->_ptr = (char *) (orgtx + litstrt);   /* start at beginning of lit */
	p->_cnt = litend - litstrt;

	if(debug)printf("litstrt %d, charstrt  %d\n",litstrt, p->_base);
 	/* the first forms we wish to read are those literals in the 
	 * literal table, that is those forms referenced by an offset
	 * from r8 in  compiled code
	 */

	/* to read in the forms correctly, we must set up the read table
	 */
	currtab = Vreadtable->clb;
	Vreadtable->clb = strtab;		/* standard read table */
	curibase = ibase->clb;
	ibase->clb = inewint(10);		/* read in decimal */

	linktab = (lispval *)linkstrt;

	oldinitflag = initflag;			/* remember current val */
	initflag = TRUE;			/* turn OFF gc */
	
	while (linktab < (lispval *)linkend)
	{
	   np = svnp;
	   protect(P(p));
	   handy = Lread();
	   getc(p);			/* eat trailing blank */
	   if(debug)
	   {   printf("one form read: ");
	       printr(handy,stdout); fflush(stdout);
	   }
	   *linktab++ = handy;
	}

	/* now process the binder table, which contains pointers to 
	   functions to link in and forms to evaluate.
	*/
	bindorg = (struct bindage *) (orgtx + bindloc);
	funcnt = 0;
	if(debug) printf("binding loc %d, orgin : %d\n",bindloc,bindorg);

	for( curbind = bindorg; curbind->b_type != -1 ; curbind++) 
	{
	    np = svnp;
	    protect(P(p));
	    rdform = Lread();
	    getc(p);			/* eat trailing null */
	    protect(rdform);
	    if(curbind->b_type <= 2)	/* if function type */
	    { 
	       handy = newfunct();
	       rdform->fnbnd = handy;
	       handy->entry = (lispval (*)())(orgtx + funloc[funcnt++]);
	       handy->discipline =
		  (curbind->b_type == 0 ? lambda :
		       curbind->b_type == 1 ? nlambda :
			  macro);
	       if(domap) fprintf(map,"%s\n%x\n",rdform->pname,handy->entry);
	    }
	    else {
		Vreadtable->clb = currtab;
		ibase->clb = curibase;

		eval(rdform);		/* otherwise eval it */

		curibase = ibase->clb;
		ibase->clb = inewint(10);
		Vreadtable->clb = strtab;
	   }
	};
	      
	p->_flag = 0;			/* give up file descriptor */
	initflag = oldinitflag;		/* restore state of gc */
	Vreadtable->clb = currtab;
	chkrtab(currtab);
	ibase->clb = curibase;

	fclose(filp);
	if(domap) fclose(map);
	return(tatom);
}

