spec:	  dcl
	| common
	| external
	| intrinsic
	| equivalence
	| implicit
	| data
	| namelist
	| SSAVE
		{ NO66("SAVE statement");
		  saveall = YES; }
	| SSAVE savelist
		{ NO66("SAVE statement"); }
	| SFORMAT
		{
		if (parstate < INDCL)
			parstate = INDCL;
		fmtstmt(thislabel);
		setfmt(thislabel);
		}
	| SPARAM in_dcl SLPAR paramlist SRPAR
		{ NO66("PARAMETER statement"); }
	;

dcl:	  type opt_comma name in_dcl dims lengspec
		{ settype($3, $1, $6);
		  if(ndim>0) setbound($3,ndim,dims);
		}
	| dcl SCOMMA name dims lengspec
		{ settype($3, $1, $5);
		  if(ndim>0) setbound($3,ndim,dims);
		}
	;

type:	  typespec lengspec
		{ varleng = $2; }
	;

typespec:  typename
		{ varleng = ($1<0 || $1==TYLONG ? 0 : typesize[$1]); }
	;

typename:    SINTEGER	{ $$ = TYLONG; }
	| SREAL		{ $$ = TYREAL; }
	| SCOMPLEX	{ $$ = TYCOMPLEX; }
	| SDOUBLE	{ $$ = TYDREAL; }
	| SDCOMPLEX	{ NOEXT("DOUBLE COMPLEX statement"); $$ = TYDCOMPLEX; }
	| SLOGICAL	{ $$ = TYLOGICAL; }
	| SCHARACTER	{ NO66("CHARACTER statement"); $$ = TYCHAR; }
	| SUNDEFINED	{ $$ = TYUNKNOWN; }
	| SDIMENSION	{ $$ = TYUNKNOWN; }
	| SAUTOMATIC	{ NOEXT("AUTOMATIC statement"); $$ = - STGAUTO; }
	| SSTATIC	{ NOEXT("STATIC statement"); $$ = - STGBSS; }
	;

lengspec:
		{ $$ = varleng; }
	| SSTAR intonlyon expr intonlyoff
		{
		expptr p;
		p = $3;
		NO66("length specification *n");
		if( ! ISICON(p) || p->constblock.const.ci<0 )
			{
			$$ = 0;
			dclerr("length must be a positive integer constant",
				PNULL);
			}
		else $$ = p->constblock.const.ci;
		}
	| SSTAR intonlyon SLPAR SSTAR SRPAR intonlyoff
		{ NO66("length specification *(*)"); $$ = -1; }
	;

common:	  SCOMMON in_dcl var
		{ incomm( $$ = comblock(0, CNULL) , $3 ); }
	| SCOMMON in_dcl comblock var
		{ $$ = $3;  incomm($3, $4); }
	| common opt_comma comblock opt_comma var
		{ $$ = $3;  incomm($3, $5); }
	| common SCOMMA var
		{ incomm($1, $3); }
	;

comblock:  SCONCAT
		{ $$ = comblock(0, CNULL); }
	| SSLASH SNAME SSLASH
		{ $$ = comblock(toklen, token); }
	;

external: SEXTERNAL in_dcl name
		{ setext($3); }
	| external SCOMMA name
		{ setext($3); }
	;

intrinsic:  SINTRINSIC in_dcl name
		{ NO66("INTRINSIC statement"); setintr($3); }
	| intrinsic SCOMMA name
		{ setintr($3); }
	;

equivalence:  SEQUIV in_dcl equivset
	| equivalence SCOMMA equivset
	;

equivset:  SLPAR equivlist SRPAR
		{
		struct Equivblock *p;
		if(nequiv >= maxequiv)
			many("equivalences", 'q');
		p  =  & eqvclass[nequiv++];
		p->eqvinit = NO;
		p->eqvbottom = 0;
		p->eqvtop = 0;
		p->equivs = $2;
		p->init = NO;
		p->initoffset = 0;
		}
	;

equivlist:  lhs
		{ $$=ALLOC(Eqvchain);
		  $$->eqvitem.eqvlhs = (struct Primblock *)$1;
		}
	| equivlist SCOMMA lhs
		{ $$=ALLOC(Eqvchain);
		  $$->eqvitem.eqvlhs = (struct Primblock *) $3;
		  $$->eqvnextp = $1;
		}
	;


savelist: saveitem
	| savelist SCOMMA saveitem
	;

saveitem: name
		{ int k;
		  $1->vsave = YES;
		  k = $1->vstg;
		if( ! ONEOF(k, M(STGUNKNOWN)|M(STGBSS)|M(STGINIT)) )
			dclerr("can only save static variables", $1);
		}
	| comblock
		{ $1->extsave = 1; }
	;

paramlist:  paramitem
	| paramlist SCOMMA paramitem
	;

paramitem:  name SEQUALS expr
		{
		  if ($1->vclass == CLUNKNOWN)
		    $1->vclass = CLPARAM;
		  else
		    dclerr("%s redefined", $1);

		  if ($1->vclass == CLPARAM)
		    {
		      if (!ISCONST($3))
			$3 = fixtype($3);

		      if ($1->vtype == TYUNKNOWN)
			{
			  char c;

			  c = $1->varname[0];
			  if (c >= 'A' && c <= 'Z')
			    c = c - 'A';
			  else
			    c = c - 'a';
			  $1->vtype = impltype[c];
			  $1->vleng = ICON(implleng[c]);
			}
		      if ($1->vtype == TYUNKNOWN)
			{ 
			  warn1("type undefined for %s",
				varstr(VL, $1->varname));
			  ((struct Paramblock *) ($1))->paramval = $3;
			}
		      else
			{
			  extern int badvalue;
			  extern expptr constconv();
			  int type;
			  ftnint len;

			  type = $1->vtype;
			  if (type == TYCHAR)
			    {
			      if ($1->vleng != NULL)
				len = $1->vleng->constblock.const.ci;
			      else if (ISCONST($3) &&
					$3->constblock.vtype == TYCHAR)
				len = $3->constblock.vleng->
					constblock.const.ci;
			      else
				len = 1;
			    }
			  badvalue = 0;
			  if (ISCONST($3))
			    {
			      ((struct Paramblock *) ($1))->paramval =
			        convconst($1->vtype, len, $3);
			      if (type == TYLOGICAL)
				((struct Paramblock *) ($1))->paramval->
				  headblock.vtype = TYLOGICAL;
			      frexpr((tagptr) $3);
			    }
			  else
			    {
			      warn1("%s set to a nonconstant",
				    varstr(VL, $1->varname));
			      ((struct Paramblock *) ($1))->paramval = $3;
			    }
			}
		    }
		}
	;

var:	  name dims
		{ if(ndim>0) setbound($1, ndim, dims); }
	;


dims:
		{ ndim = 0; }
	| SLPAR dimlist SRPAR
	;

dimlist:   { ndim = 0; }   dim
	| dimlist SCOMMA dim
	;

dim:	  ubound
		{ if(ndim == maxdim)
			err("too many dimensions");
		  else if(ndim < maxdim)
			{ dims[ndim].lb = 0;
			  dims[ndim].ub = $1;
			}
		  ++ndim;
		}
	| expr SCOLON ubound
		{ if(ndim == maxdim)
			err("too many dimensions");
		  else if(ndim < maxdim)
			{ dims[ndim].lb = $1;
			  dims[ndim].ub = $3;
			}
		  ++ndim;
		}
	;

ubound:	  SSTAR
		{ $$ = 0; }
	| expr
	;

labellist: label
		{ nstars = 1; labarray[0] = $1; }
	| labellist SCOMMA label
		{ if(nstars < MAXLABLIST)  labarray[nstars++] = $3; }
	;

label:	  SICON
		{ $$ = execlab( convci(toklen, token) ); }
	;

implicit:  SIMPLICIT in_dcl implist
		{ NO66("IMPLICIT statement"); }
	| implicit SCOMMA implist
	;

implist:  imptype SLPAR letgroups SRPAR
	;

imptype:   { needkwd = 1; } type
		{ vartype = $2; }
	;

letgroups: letgroup
	| letgroups SCOMMA letgroup
	;

letgroup:  letter
		{ setimpl(vartype, varleng, $1, $1); }
	| letter SMINUS letter
		{ setimpl(vartype, varleng, $1, $3); }
	;

letter:  SNAME
		{ if(toklen!=1 || token[0]<'a' || token[0]>'z')
			{
			dclerr("implicit item must be single letter", PNULL);
			$$ = 0;
			}
		  else $$ = token[0];
		}
	;

namelist:	SNAMELIST
	| namelist namelistentry
	;

namelistentry:  SSLASH name SSLASH namelistlist
		{
		if($2->vclass == CLUNKNOWN)
			{
			$2->vclass = CLNAMELIST;
			$2->vtype = TYINT;
			$2->vstg = STGINIT;
			$2->varxptr.namelist = $4;
			$2->vardesc.varno = ++lastvarno;
			}
		else dclerr("cannot be a namelist name", $2);
		}
	;

namelistlist:  name
		{ $$ = mkchain($1, CHNULL); }
	| namelistlist SCOMMA name
		{ $$ = hookup($1, mkchain($3, CHNULL)); }
	;

in_dcl:
		{ switch(parstate)	
			{
			case OUTSIDE:	newproc();
					startproc(PNULL, CLMAIN);
			case INSIDE:	parstate = INDCL;
			case INDCL:	break;

			default:
				dclerr("declaration among executables", PNULL);
			}
		}
	;

data:	data1
	{
	  if (overlapflag == YES)
	    warn("overlapping initializations");
	}

data1:	SDATA in_data datapair
    |	data1 opt_comma datapair
    ;

in_data:
		{ if(parstate == OUTSIDE)
			{
			newproc();
			startproc(PNULL, CLMAIN);
			}
		  if(parstate < INDATA)
			{
			enddcl();
			parstate = INDATA;
			}
		  overlapflag = NO;
		}
	;

datapair:	datalvals SSLASH datarvals SSLASH
			{ savedata($1, $3); }
	;

datalvals:	datalval
		{ $$ = preplval(NULL, $1); }
	 |	datalvals SCOMMA datalval
		{ $$ = preplval($1, $3); }
	 ;

datarvals:	datarval
	 |	datarvals SCOMMA datarval
			{
			  $3->next = $1;
			  $$ = $3;
			}
	 ;

datalval:	dataname
			{ $$ = mkdlval($1, NULL, NULL); }
	|	dataname datasubs
			{ $$ = mkdlval($1, $2, NULL); }
	|	dataname datarange
			{ $$ = mkdlval($1, NULL, $2); }
	|	dataname datasubs datarange
			{ $$ = mkdlval($1, $2, $3); }
	|	dataimplieddo
	;

dataname:	SNAME { $$ = mkdname(toklen, token); }
	;

datasubs:	SLPAR iconexprlist SRPAR
			{ $$ = revvlist($2); }
	;

datarange:	SLPAR opticonexpr SCOLON opticonexpr SRPAR
			{ $$ = mkdrange($2, $4); }
	 ;

iconexprlist:	iconexpr
			{
			  $$ = prepvexpr(NULL, $1);
			}
	    |	iconexprlist SCOMMA iconexpr
			{
			  $$ = prepvexpr($1, $3);
			}
	    ;

opticonexpr:			{ $$ = NULL; }
	   |	iconexpr	{ $$ = $1; }
	   ;

dataimplieddo:	SLPAR dlist SCOMMA dataname SEQUALS iconexprlist SRPAR
		{ $$ = mkdatado($2, $4, $6); }
	     ;

dlist:	dataelt
	{ $$ = preplval(NULL, $1); }
     |	dlist SCOMMA dataelt
	{ $$ = preplval($1, $3); }
     ;

dataelt:	dataname datasubs
		{ $$ = mkdlval($1, $2, NULL); }
       |	dataname datarange
		{ $$ = mkdlval($1, NULL, $2); }
       |	dataname datasubs datarange
		{ $$ = mkdlval($1, $2, $3); }
       |	dataimplieddo
       ;

datarval:	datavalue
			{
			  static dvalue one = { DVALUE, NORMAL, 1 };

			  $$ = mkdrval(&one, $1);
			}
	|	dataname SSTAR datavalue
			{
			  $$ = mkdrval($1, $3);
			  frvexpr($1);
			}
	|	unsignedint SSTAR datavalue
			{
			  $$ = mkdrval($1, $3);
			  frvexpr($1);
			}
	;

datavalue:	dataname
			{
			  $$ = evparam($1);
			  free((char *) $1);
			}
	 |	int_const
			{
			  $$ = ivaltoicon($1);
			  frvexpr($1);
			}

	 |	real_const
	 |	complex_const
	 |	STRUE		{ $$ = mklogcon(1); }
	 |	SFALSE		{ $$ = mklogcon(0); }
	 |	SHOLLERITH	{ $$ = mkstrcon(toklen, token); }
	 |	SSTRING		{ $$ = mkstrcon(toklen, token); }
	 |	bit_const
	 ;

int_const:	unsignedint
	 |	SPLUS unsignedint
			{ $$ = $2; }
	 |	SMINUS unsignedint
			{
			  $$ = negival($2);
			  frvexpr($2);
			}
				
	 ;

unsignedint:	SICON { $$ = evicon(toklen, token); }
	   ;

real_const:	unsignedreal
	  |	SPLUS unsignedreal
			{ $$ = $2; }
	  |	SMINUS unsignedreal
			{
			  consnegop($2);
			  $$ = $2;
			}
	  ;

unsignedreal:	SRCON { $$ = mkrealcon(TYREAL, convcd(toklen, token)); }
	    |	SDCON { $$ = mkrealcon(TYDREAL, convcd(toklen, token)); }
	    ;

bit_const:	SHEXCON { $$ = mkbitcon(4, toklen, token); }
	 |	SOCTCON { $$ = mkbitcon(3, toklen, token); }
	 |	SBITCON { $$ = mkbitcon(1, toklen, token); }
	 ;

iconexpr:	iconterm
	|	SPLUS iconterm
			{ $$ = $2; }
	|	SMINUS iconterm
			{ $$ = mkdexpr(OPNEG, NULL, $2); }
	|	iconexpr SPLUS iconterm
			{ $$ = mkdexpr(OPPLUS, $1, $3); }
	|	iconexpr SMINUS iconterm
			{ $$ = mkdexpr(OPMINUS, $1, $3); }
	;

iconterm:	iconfactor
	|	iconterm SSTAR iconfactor
			{ $$ = mkdexpr(OPSTAR, $1, $3); }
	|	iconterm SSLASH iconfactor
			{ $$ = mkdexpr(OPSLASH, $1, $3); }
	;

iconfactor:	iconprimary
	  |	iconprimary SPOWER iconfactor
			{ $$ = mkdexpr(OPPOWER, $1, $3); }
	  ;

iconprimary:	SICON
			{ $$ = evicon(toklen, token); }
	   |	dataname
	   |	SLPAR iconexpr SRPAR
			{ $$ = $2; }
	   ;
