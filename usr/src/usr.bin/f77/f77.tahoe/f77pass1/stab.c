/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)stab.c	5.1 (Berkeley) 6/7/85";
#endif not lint

/*
 * stab.c
 *
 * Symbolic debugging info interface for the f77 compiler.
 *
 * Here we generate pseudo-ops that cause the assembler to put
 * symbolic debugging information into the object file.
 *
 * University of Utah CS Dept modification history:
 *
 * $Log:	stab.c,v $
 * Revision 1.2  85/02/02  01:30:09  donn
 * Don't put the 'program' name into the file; it only confuses dbx, sigh.
 * 
 */

#include "defs.h"

#include <sys/types.h>
#include <a.out.h>
#include <stab.h>

#define public
#define private static
#define and &&
#define or ||
#define not !
#define div /
#define mod %
#define nil 0

typedef enum { false, true } Boolean;

static char asmline[128];
int len;
extern char *malloc();

prstab(s, code, type, loc)
char *s, *loc;
int code, type;
{
    char *locout;

    if (sdbflag) {
	locout = (loc == nil) ? "0" : loc;
	if (s == nil) {
	    sprintf(asmline, "\t.stabn\t0x%x,0,0x%x,%s\n", code, type, locout);
	} else {
	    sprintf(asmline, "\t.stabs\t\"%s\",0x%x,0,0x%x,%s\n", s, code, type,
		locout);
	}
        p2pass( asmline );
    }
}

filenamestab(s)
char *s;
{
   sprintf(asmline,"\t.stabs\t\"%s\",0x%x,0,0,0\n", s, N_SO);
   p2pass( asmline );
}

linenostab(lineno)
int lineno;
{
   sprintf(asmline,"\t.stabd\t0x%x,0,%d\n", N_SLINE, lineno);
   p2pass( asmline );
}

/*
 * Generate information for an entry point
 */

public entrystab(p,class)
register struct Entrypoint *p;
int class;
{
int et;
Namep q;

  switch(class) {
    case CLMAIN: 
        et=writestabtype(TYSUBR);
	sprintf(asmline, "\t.stabs\t\"MAIN:F%2d\",0x%x,0,0,L%d\n",
				et,N_FUN,p->entrylabel);
	p2pass(asmline);
	break;
	
     case CLBLOCK:     /* May need to something with block data LATER */
	break;

     default :
 	if( (q=p->enamep) == nil) fatal("entrystab has no nameblock");
	sprintf(asmline, "\t.stabs\t\"%s:F", varstr(VL,q->varname));
	len = strlen(asmline);
	/* when insufficient information is around assume TYSUBR; enddcl
	   will fill this in*/
	if(q->vtype == TYUNKNOWN || (q->vtype == TYCHAR && q->vleng == nil) ){
           sprintf(asmline+len, "%2d", writestabtype(TYSUBR));
 	}
        else addtypeinfo(q);
	len += strlen(asmline+len);
	sprintf(asmline+len, "\",0x%x,0,0,L%d\n",N_FUN,p->entrylabel);
	p2pass(asmline);
        break;
   }
}

/*
 * Generate information for a symbol table (name block ) entry.
 */

public namestab(sym)
Namep sym;
{
    register Namep p;
    char *varname, *classname;
    Boolean ignore;
    int vartype;

	ignore = false;
	p = sym;
	if(!p->vdcldone) return;
	vartype = p->vtype;
	varname = varstr(VL, p->varname);
	switch (p->vclass) {
	    case CLPARAM:	/* parameter (constant) */
		classname = "c";
		break;

	    case CLVAR:		/* variable */
	    case CLUNKNOWN:   
 		if(p->vstg == STGARG) classname = "v";
    		else classname = "V";
		break;

	    case CLMAIN:	/* main program */
	    case CLENTRY:	/* secondary entry point */
	    case CLBLOCK:       /* block data name*/
	    case CLPROC:	/* external or function or subroutine */
		ignore = true;  /* these are put out by entrystab */
		break;


	}
	if (not ignore) {
	    sprintf(asmline, "\t.stabs\t\"%s:%s", varname, classname);
	    len = strlen(asmline);
            addtypeinfo(p);
	    len += strlen(asmline+len);
	    switch(p->vstg) {

	      case STGUNKNOWN :
	      case STGCONST :
	      case STGEXT :
	      case STGINTR :
	      case STGSTFUNCT :
	      case STGLENG :
	      case STGNULL :
	      case STGREG :
	      case STGINIT :
	          sprintf(asmline+len,
		  "\",0x%x,0,0,0 /* don't know how to calc loc for stg %d*/ \n",
			       N_LSYM,p->vstg);
		  break;

	      case STGARG :
		  sprintf(asmline+len,"\",0x%x,0,0,%d \n",
			      N_PSYM,p->vardesc.varno + ARGOFFSET );
		  break;

	      case STGCOMMON :
		  sprintf(asmline+len, "\",0x%x,0,0,%d\n", 
		       N_GSYM, p->voffset);
		  break;

	      case STGBSS :
		  sprintf(asmline+len, "\",0x%x,0,0,v.%d\n",
		     	 (p->inlcomm ? N_LCSYM : N_STSYM), 
                         p->vardesc.varno);
		  break;

	      case STGEQUIV :
		  sprintf(asmline+len, "\",0x%x,0,0,%s + %d \n",
		     	 (p->inlcomm ? N_LCSYM : N_STSYM) , 
                         memname(STGEQUIV,p->vardesc.varno),(p->voffset)) ;
		  break;

	      case STGAUTO :
		  sprintf(asmline+len, "\",0x%x,0,0,-%d \n",
		     	N_LSYM, p->voffset);

	    }
	    p2pass(asmline);       
	}
}

static typenum[NTYPES]; /* has the given type already been defined ?*/

private writestabtype(type)
int type;
{
 char asmline[130];
 static char *typename[NTYPES] =
 { "unknown", "addr","integer*2", "integer", "real", "double precision",
   "complex", "double complex", "logical", "char", "void", "error" };

 static int typerange[NTYPES] = { 0, 3, 2, 3, 4, 5, 6, 7, 3, 9, 10, 11 };

 /* compare with typesize[] in init.c */
 static int typebounds[2] [NTYPES] ={
 /* "unknown", "addr","integer*2", "integer",    "real", "double precision", */
    { 0      ,   0   ,   -32768,    -2147483648,   4,       8,
 /* "complex", "double complex", "logical", "char", "void", "error" }; */
      8,         16,               0,        0,       0,          0 },
 /* "unknown", "addr","integer*2", "integer",    "real", "double precision", */
    { 0  ,       -1,      32767,    2147483647,   0,         0,
 /* "complex", "double complex", "logical", "char", "void", "error" }; */
      0,         0,               1,        127,       0,          0 }
 };
                    

 if( type < 0 || type > NTYPES) badtype("writestabtype",type);

    if (typenum[type]) return(typenum[type]);
    typenum[type] = type;
    sprintf(asmline, "\t.stabs\t\"%s:t%d=r%d;%ld;%ld;\",0x%x,0,0,0 \n", 
	typename[type], type, typerange[type], typebounds[0][type], 
        typebounds[1][type], N_GSYM) ;
    p2pass(asmline);
    return(typenum[type]);
}


private getbasenum(p)
Namep p;
{

  int t;
  t = p->vtype;
  if( t < TYSHORT || t > TYSUBR)
  dclerr("can't get dbx basetype information",p);

  if (p->vtype == TYCHAR || p->vdim != nil ) writestabtype(TYINT);
  return(writestabtype(t));
}

/*
 * Generate debugging information for the given type of the given symbol.
 */

private addtypeinfo(sym)
Namep sym;
{
    Namep p;
    int i,tnum;
    char lb[20],ub[20];

    p = sym;
    if (p->tag != TNAME) badtag("addtypeinfo",p->tag);

    tnum = getbasenum(p);
    if(p->vdim != (struct Dimblock *) ENULL) {
    
      for (i = p->vdim->ndim-1; i >=0 ; --i) { 
         if(p->vdim->dims[i].lbaddr == ENULL) {
	      sprintf(lb,"%d", p->vdim->dims[i].lb->constblock.const.ci);
	 }
	 else  { 
	      sprintf(lb,"T%d", p->vdim->dims[i].lbaddr->addrblock.memoffset->constblock.const.ci);
         }
         if(p->vdim->dims[i].ubaddr == ENULL) {
	      sprintf(ub,"%d",p->vdim->dims[i].ub->constblock.const.ci);
	 }
	 else  {
	      sprintf(ub,"T%d",p->vdim->dims[i].ubaddr->addrblock.memoffset->constblock.const.ci);
         }
       	 sprintf(asmline+len, "ar%d;%s;%s;", TYINT, lb, ub);
	 len += strlen(asmline+len);
     }
   }
    if (p->vtype == TYCHAR) {
    /* character type always an array(1:?) */
        if( ! (p->vleng ) )
           fatalstr("missing length in addtypeinfo for character variable %s", varstr(p->varname));

        if (ISCONST(p->vleng)) sprintf(ub,"%d",p->vleng->constblock.const.ci);
         else sprintf(ub,"A%d",p->vleng->addrblock.memno + ARGOFFSET);

	sprintf(asmline+len,"ar%d;1;%s;", TYINT, ub);
	len += strlen(asmline+len);
    }
    sprintf(asmline+len, "%d",tnum);
}
