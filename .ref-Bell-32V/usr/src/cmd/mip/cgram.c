# define NAME 2
# define STRING 3
# define ICON 4
# define FCON 5
# define PLUS 6
# define MINUS 8
# define MUL 11
# define AND 14
# define OR 17
# define ER 19
# define QUEST 21
# define COLON 22
# define ANDAND 23
# define OROR 24
# define ASOP 25
# define RELOP 26
# define EQUOP 27
# define DIVOP 28
# define SHIFTOP 29
# define INCOP 30
# define UNOP 31
# define STROP 32
# define TYPE 33
# define CLASS 34
# define STRUCT 35
# define RETURN 36
# define GOTO 37
# define IF 38
# define ELSE 39
# define SWITCH 40
# define BREAK 41
# define CONTINUE 42
# define WHILE 43
# define DO 44
# define FOR 45
# define DEFAULT 46
# define CASE 47
# define SIZEOF 48
# define ENUM 49
# define LP 50
# define RP 51
# define LC 52
# define RC 53
# define LB 54
# define RB 55
# define CM 56
# define SM 57
# define ASSIGN 58

# line 108 "/usr/src/cmd/mip/cgram.y"
# include "mfile1"
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;

# line 127 "/usr/src/cmd/mip/cgram.y"
	static int fake = 0;
	static char fakename[NCHNAM+1];
# define YYERRCODE 256

# line 814 "/usr/src/cmd/mip/cgram.y"


NODE *
mkty( t, d, s ) unsigned t; {
	return( block( TYPE, NIL, NIL, t, d, s ) );
	}

NODE *
bdty( op, p, v ) NODE *p; {
	register NODE *q;

	q = block( op, p, NIL, INT, 0, INT );

	switch( op ){

	case UNARY MUL:
	case UNARY CALL:
		break;

	case LB:
		q->right = bcon(v);
		break;

	case NAME:
		q->rval = v;
		break;

	default:
		cerror( "bad bdty" );
		}

	return( q );
	}

dstash( n ){ /* put n into the dimension table */
	if( curdim >= DIMTABSZ-1 ){
		cerror( "dimension table overflow");
		}
	dimtab[ curdim++ ] = n;
	}

savebc() {
	if( psavbc > & asavbc[BCSZ-4 ] ){
		cerror( "whiles, fors, etc. too deeply nested");
		}
	*psavbc++ = brklab;
	*psavbc++ = contlab;
	*psavbc++ = flostat;
	*psavbc++ = swx;
	flostat = 0;
	}

resetbc(mask){

	swx = *--psavbc;
	flostat = *--psavbc | (flostat&mask);
	contlab = *--psavbc;
	brklab = *--psavbc;

	}

addcase(p) NODE *p; { /* add case to switch */

	p = optim( p );  /* change enum to ints */
	if( p->op != ICON ){
		uerror( "non-constant case expression");
		return;
		}
	if( swp == swtab ){
		uerror( "case not in switch");
		return;
		}
	if( swp >= &swtab[SWITSZ] ){
		cerror( "switch table overflow");
		}
	swp->sval = p->lval;
	deflab( swp->slab = getlab() );
	++swp;
	tfree(p);
	}

adddef(){ /* add default case to switch */
	if( swtab[swx].slab >= 0 ){
		uerror( "duplicate default in switch");
		return;
		}
	if( swp == swtab ){
		uerror( "default not inside switch");
		return;
		}
	deflab( swtab[swx].slab = getlab() );
	}

swstart(){
	/* begin a switch block */
	if( swp >= &swtab[SWITSZ] ){
		cerror( "switch table overflow");
		}
	swx = swp - swtab;
	swp->slab = -1;
	++swp;
	}

swend(){ /* end a switch block */

	register struct sw *swbeg, *p, *q, *r, *r1;
	CONSZ temp;
	int tempi;

	swbeg = &swtab[swx+1];

	/* sort */

	r1 = swbeg;
	r = swp-1;

	while( swbeg < r ){
		/* bubble largest to end */
		for( q=swbeg; q<r; ++q ){
			if( q->sval > (q+1)->sval ){
				/* swap */
				r1 = q+1;
				temp = q->sval;
				q->sval = r1->sval;
				r1->sval = temp;
				tempi = q->slab;
				q->slab = r1->slab;
				r1->slab = tempi;
				}
			}
		r = r1;
		r1 = swbeg;
		}

	/* it is now sorted */

	for( p = swbeg+1; p<swp; ++p ){
		if( p->sval == (p-1)->sval ){
			uerror( "duplicate case in switch, %d", tempi=p->sval );
			return;
			}
		}

	genswitch( swbeg-1, swp-swbeg );
	swp = swbeg-1;
	}
extern short  yyexca[];
# define YYNPROD 184
# define YYLAST 1232
extern short  yyact[];
extern short  yypact[];
extern short  yypgo[];
extern short  yyr1[];
extern short  yyr2[];
extern short  yychk[];
extern short  yydef[];
#
# define YYFLAG -1000
# define YYERROR goto yyerrlab
# define YYACCEPT return(0)
# define YYABORT return(1)

/*	parser for yacc output	*/

#ifdef YYDEBUG
int yydebug = 0; /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar = -1; /* current input token number */
int yynerrs = 0;  /* number of errors */
short yyerrflag = 0;  /* error recovery flag */

yyparse() {

	short yys[YYMAXDEPTH];
	short yyj, yym;
	register YYSTYPE *yypvt;
	register short yystate, *yyps, yyn;
	register YYSTYPE *yypv;
	register short *yyxi;

	yystate = 0;
	yychar = -1;
	yynerrs = 0;
	yyerrflag = 0;
	yyps= &yys[-1];
	yypv= &yyv[-1];

 yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
	if( yydebug  ) printf( "state %d, char 0%o\n", yystate, yychar );
#endif
		if( ++yyps> &yys[YYMAXDEPTH] ) { yyerror( "yacc stack overflow" ); return(1); }
		*yyps = yystate;
		++yypv;
		*yypv = yyval;

 yynewstate:

	yyn = yypact[yystate];

	if( yyn<= YYFLAG ) goto yydefault; /* simple state */

	if( yychar<0 ) if( (yychar=yylex())<0 ) yychar=0;
	if( (yyn += yychar)<0 || yyn >= YYLAST ) goto yydefault;

	if( yychk[ yyn=yyact[ yyn ] ] == yychar ){ /* valid shift */
		yychar = -1;
		yyval = yylval;
		yystate = yyn;
		if( yyerrflag > 0 ) --yyerrflag;
		goto yystack;
		}

 yydefault:
	/* default state action */

	if( (yyn=yydef[yystate]) == -2 ) {
		if( yychar<0 ) if( (yychar=yylex())<0 ) yychar = 0;
		/* look through exception table */

		for( yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2 ) ; /* VOID */

		while( *(yyxi+=2) >= 0 ){
			if( *yyxi == yychar ) break;
			}
		if( (yyn = yyxi[1]) < 0 ) return(0);   /* accept */
		}

	if( yyn == 0 ){ /* error */
		/* error ... attempt to resume parsing */

		switch( yyerrflag ){

		case 0:   /* brand new error */

			yyerror( "syntax error" );
		yyerrlab:
			++yynerrs;

		case 1:
		case 2: /* incompletely recovered error ... try again */

			yyerrflag = 3;

			/* find a state where "error" is a legal shift action */

			while ( yyps >= yys ) {
			   yyn = yypact[*yyps] + YYERRCODE;
			   if( yyn>= 0 && yyn < YYLAST && yychk[yyact[yyn]] == YYERRCODE ){
			      yystate = yyact[yyn];  /* simulate a shift of "error" */
			      goto yystack;
			      }
			   yyn = yypact[*yyps];

			   /* the current yyps has no shift onn "error", pop stack */

#ifdef YYDEBUG
			   if( yydebug ) printf( "error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1] );
#endif
			   --yyps;
			   --yypv;
			   }

			/* there is no state on the stack with an error shift ... abort */

	yyabort:
			return(1);


		case 3:  /* no shift yet; clobber input char */

#ifdef YYDEBUG
			if( yydebug ) printf( "error recovery discards char %d\n", yychar );
#endif

			if( yychar == 0 ) goto yyabort; /* don't discard EOF, quit */
			yychar = -1;
			goto yynewstate;   /* try again in the same state */

			}

		}

	/* reduction by production yyn */

#ifdef YYDEBUG
		if( yydebug ) printf("reduce %d\n",yyn);
#endif
		yyps -= yyr2[yyn];
		yypvt = yypv;
		yypv -= yyr2[yyn];
		yyval = yypv[1];
		yym=yyn;
			/* consult goto table to find next state */
		yyn = yyr1[yyn];
		yyj = yypgo[yyn] + *yyps + 1;
		if( yyj>=YYLAST || yychk[ yystate = yyact[yyj] ] != -yyn ) yystate = yyact[yypgo[yyn]];
		switch(yym){
			
case 2:
# line 133 "/usr/src/cmd/mip/cgram.y"
ftnend(); break;
case 3:
# line 136 "/usr/src/cmd/mip/cgram.y"
{ curclass = SNULL;  blevel = 0; } break;
case 4:
# line 138 "/usr/src/cmd/mip/cgram.y"
{ curclass = SNULL;  blevel = 0; } break;
case 5:
# line 142 "/usr/src/cmd/mip/cgram.y"
{  yypvt[-1].nodep->op = FREE; } break;
case 6:
# line 144 "/usr/src/cmd/mip/cgram.y"
{  yypvt[-2].nodep->op = FREE; } break;
case 7:
# line 145 "/usr/src/cmd/mip/cgram.y"
{
				defid( tymerge(yypvt[-1].nodep,yypvt[-0].nodep), curclass==STATIC?STATIC:EXTDEF );
#ifndef LINT
				pfstab(stab[yypvt[-0].nodep->rval].sname);
#endif
				} break;
case 8:
# line 151 "/usr/src/cmd/mip/cgram.y"
{  
			    if( blevel ) cerror( "function level error" );
			    if( reached ) retstat |= NRETVAL; 
			    yypvt[-3].nodep->op = FREE;
			    ftnend();
			    } break;
case 11:
# line 162 "/usr/src/cmd/mip/cgram.y"
{  blevel = 1; } break;
case 13:
# line 167 "/usr/src/cmd/mip/cgram.y"
{  bccode();
			    locctr(PROG);
			    } break;
case 14:
# line 173 "/usr/src/cmd/mip/cgram.y"
{  yypvt[-1].nodep->op = FREE; 
#ifndef LINT
			    plcstab(blevel);
#endif
			    } break;
case 15:
# line 179 "/usr/src/cmd/mip/cgram.y"
{  yypvt[-2].nodep->op = FREE; 
#ifndef LINT
			    plcstab(blevel);
#endif
			    } break;
case 16:
# line 187 "/usr/src/cmd/mip/cgram.y"
{  yypvt[-1].nodep->op = FREE; } break;
case 17:
# line 189 "/usr/src/cmd/mip/cgram.y"
{  yypvt[-2].nodep->op = FREE; } break;
case 19:
# line 193 "/usr/src/cmd/mip/cgram.y"
{ curclass = SNULL;  yypvt[-2].nodep->op = FREE; } break;
case 20:
# line 195 "/usr/src/cmd/mip/cgram.y"
{ curclass = SNULL;  yypvt[-1].nodep->op = FREE; } break;
case 21:
# line 197 "/usr/src/cmd/mip/cgram.y"
{  curclass = SNULL; } break;
case 23:
# line 201 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = mkty(INT,0,INT);  curclass = SNULL; } break;
case 24:
# line 204 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = yypvt[-0].nodep; } break;
case 26:
# line 207 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = mkty(INT,0,INT); } break;
case 27:
# line 209 "/usr/src/cmd/mip/cgram.y"
{ curclass = SNULL ; } break;
case 28:
# line 214 "/usr/src/cmd/mip/cgram.y"
{  curclass = yypvt[-0].intval; } break;
case 30:
# line 219 "/usr/src/cmd/mip/cgram.y"
{  yypvt[-1].nodep->type = types( yypvt[-1].nodep->type, yypvt[-0].nodep->type, UNDEF );
			    yypvt[-0].nodep->op = FREE;
			    } break;
case 31:
# line 223 "/usr/src/cmd/mip/cgram.y"
{  yypvt[-2].nodep->type = types( yypvt[-2].nodep->type, yypvt[-1].nodep->type, yypvt[-0].nodep->type );
			    yypvt[-1].nodep->op = yypvt[-0].nodep->op = FREE;
			    } break;
case 34:
# line 231 "/usr/src/cmd/mip/cgram.y"
{ yyval.nodep = dclstruct(yypvt[-4].intval); } break;
case 35:
# line 233 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = rstruct(yypvt[-0].intval,0);  stwart = instruct; } break;
case 36:
# line 237 "/usr/src/cmd/mip/cgram.y"
{  yyval.intval = bstruct(-1,0); } break;
case 37:
# line 239 "/usr/src/cmd/mip/cgram.y"
{  yyval.intval = bstruct(yypvt[-0].intval,0); } break;
case 40:
# line 247 "/usr/src/cmd/mip/cgram.y"
{  moedef( yypvt[-0].intval ); } break;
case 41:
# line 249 "/usr/src/cmd/mip/cgram.y"
{  strucoff = yypvt[-0].intval;  moedef( yypvt[-2].intval ); } break;
case 42:
# line 253 "/usr/src/cmd/mip/cgram.y"
{ yyval.nodep = dclstruct(yypvt[-4].intval);  } break;
case 43:
# line 255 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = rstruct(yypvt[-0].intval,yypvt[-1].intval); } break;
case 44:
# line 259 "/usr/src/cmd/mip/cgram.y"
{  yyval.intval = bstruct(-1,yypvt[-0].intval);  stwart=0; } break;
case 45:
# line 261 "/usr/src/cmd/mip/cgram.y"
{  yyval.intval = bstruct(yypvt[-0].intval,yypvt[-1].intval);  stwart=0;  } break;
case 48:
# line 269 "/usr/src/cmd/mip/cgram.y"
{ curclass = SNULL;  stwart=0; yypvt[-1].nodep->op = FREE; } break;
case 49:
# line 271 "/usr/src/cmd/mip/cgram.y"
{  if( curclass != MOU ){
				curclass = SNULL;
				}
			    else {
				sprintf( fakename, "$%dFAKE", fake++ );
				defid( tymerge(yypvt[-0].nodep, bdty(NAME,NIL,lookup( fakename, SMOS ))), curclass );
				}
			    stwart = 0;
			    yypvt[-0].nodep->op = FREE;
			    } break;
case 50:
# line 285 "/usr/src/cmd/mip/cgram.y"
{ defid( tymerge(yypvt[-1].nodep,yypvt[-0].nodep), curclass);  stwart = instruct; } break;
case 51:
# line 286 "/usr/src/cmd/mip/cgram.y"
{yyval.nodep=yypvt[-2].nodep;} break;
case 52:
# line 287 "/usr/src/cmd/mip/cgram.y"
{ defid( tymerge(yypvt[-4].nodep,yypvt[-0].nodep), curclass);  stwart = instruct; } break;
case 55:
# line 293 "/usr/src/cmd/mip/cgram.y"
{  if( !(instruct&INSTRUCT) ) uerror( "field outside of structure" );
			    if( yypvt[-0].intval<0 || yypvt[-0].intval >= FIELD ){
				uerror( "illegal field size" );
				yypvt[-0].intval = 1;
				}
			    defid( tymerge(yypvt[-3].nodep,yypvt[-2].nodep), FIELD|yypvt[-0].intval );
			    yyval.nodep = NIL;
			    } break;
case 56:
# line 303 "/usr/src/cmd/mip/cgram.y"
{  if( !(instruct&INSTRUCT) ) uerror( "field outside of structure" );
			    falloc( stab, yypvt[-0].intval, -1, yypvt[-2].nodep );  /* alignment or hole */
			    yyval.nodep = NIL;
			    } break;
case 57:
# line 308 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = NIL; } break;
case 58:
# line 313 "/usr/src/cmd/mip/cgram.y"
{  umul:
				yyval.nodep = bdty( UNARY MUL, yypvt[-0].nodep, 0 ); } break;
case 59:
# line 316 "/usr/src/cmd/mip/cgram.y"
{  uftn:
				yyval.nodep = bdty( UNARY CALL, yypvt[-2].nodep, 0 );  } break;
case 60:
# line 319 "/usr/src/cmd/mip/cgram.y"
{  uary:
				yyval.nodep = bdty( LB, yypvt[-2].nodep, 0 );  } break;
case 61:
# line 322 "/usr/src/cmd/mip/cgram.y"
{  bary:
				if( (int)yypvt[-1].intval <= 0 ) werror( "zero or negative subscript" );
				yyval.nodep = bdty( LB, yypvt[-3].nodep, yypvt[-1].intval );  } break;
case 62:
# line 326 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = bdty( NAME, NIL, yypvt[-0].intval );  } break;
case 63:
# line 328 "/usr/src/cmd/mip/cgram.y"
{ yyval.nodep=yypvt[-1].nodep; } break;
case 64:
# line 331 "/usr/src/cmd/mip/cgram.y"
{  goto umul; } break;
case 65:
# line 333 "/usr/src/cmd/mip/cgram.y"
{  goto uftn; } break;
case 66:
# line 335 "/usr/src/cmd/mip/cgram.y"
{  goto uary; } break;
case 67:
# line 337 "/usr/src/cmd/mip/cgram.y"
{  goto bary; } break;
case 68:
# line 339 "/usr/src/cmd/mip/cgram.y"
{ yyval.nodep = yypvt[-1].nodep; } break;
case 69:
# line 341 "/usr/src/cmd/mip/cgram.y"
{
				if( blevel!=0 ) uerror("function declaration in bad context");
				yyval.nodep = bdty( UNARY CALL, bdty(NAME,NIL,yypvt[-2].intval), 0 );
				stwart = 0;
				} break;
case 70:
# line 347 "/usr/src/cmd/mip/cgram.y"
{
				yyval.nodep = bdty( UNARY CALL, bdty(NAME,NIL,yypvt[-1].intval), 0 );
				stwart = 0;
				} break;
case 71:
# line 354 "/usr/src/cmd/mip/cgram.y"
{
				/* turn off typedefs for argument names */
				stwart = SEENAME;
				} break;
case 72:
# line 361 "/usr/src/cmd/mip/cgram.y"
{ ftnarg( yypvt[-0].intval );  stwart = SEENAME; } break;
case 73:
# line 363 "/usr/src/cmd/mip/cgram.y"
{ ftnarg( yypvt[-0].intval );  stwart = SEENAME; } break;
case 75:
# line 368 "/usr/src/cmd/mip/cgram.y"
{yyval.nodep=yypvt[-2].nodep;} break;
case 77:
# line 372 "/usr/src/cmd/mip/cgram.y"
{  defid( yypvt[-0].nodep = tymerge(yypvt[-1].nodep,yypvt[-0].nodep), curclass);
			    beginit(yypvt[-0].nodep->rval);
			    } break;
case 79:
# line 379 "/usr/src/cmd/mip/cgram.y"
{  nidcl( tymerge(yypvt[-1].nodep,yypvt[-0].nodep) ); } break;
case 80:
# line 381 "/usr/src/cmd/mip/cgram.y"
{  defid( tymerge(yypvt[-1].nodep,yypvt[-0].nodep), uclass(curclass) );
			} break;
case 81:
# line 385 "/usr/src/cmd/mip/cgram.y"
{  doinit( yypvt[-0].nodep );
			    endinit(); } break;
case 82:
# line 388 "/usr/src/cmd/mip/cgram.y"
{  endinit(); } break;
case 85:
# line 397 "/usr/src/cmd/mip/cgram.y"
{  doinit( yypvt[-0].nodep ); } break;
case 86:
# line 399 "/usr/src/cmd/mip/cgram.y"
{  irbrace(); } break;
case 91:
# line 411 "/usr/src/cmd/mip/cgram.y"
{  werror( "old-fashioned initialization: use =" ); } break;
case 93:
# line 416 "/usr/src/cmd/mip/cgram.y"
{  ilbrace(); } break;
case 96:
# line 426 "/usr/src/cmd/mip/cgram.y"
{  
#ifndef LINT
			    prcstab(blevel);
#endif
			    --blevel;
			    if( blevel == 1 ) blevel = 0;
			    clearst( blevel );
			    checkst( blevel );
			    autooff = *--psavbc;
			    regvar = *--psavbc;
			    } break;
case 97:
# line 440 "/usr/src/cmd/mip/cgram.y"
{  --blevel;
			    if( blevel == 1 ) blevel = 0;
			    clearst( blevel );
			    checkst( blevel );
			    autooff = *--psavbc;
			    regvar = *--psavbc;
			    } break;
case 98:
# line 450 "/usr/src/cmd/mip/cgram.y"
{  if( blevel == 1 ) dclargs();
			    ++blevel;
			    if( psavbc > &asavbc[BCSZ-2] ) cerror( "nesting too deep" );
			    *psavbc++ = regvar;
			    *psavbc++ = autooff;
			    } break;
case 99:
# line 459 "/usr/src/cmd/mip/cgram.y"
{ ecomp( yypvt[-1].nodep ); } break;
case 101:
# line 462 "/usr/src/cmd/mip/cgram.y"
{ deflab(yypvt[-1].intval);
			   reached = 1;
			   } break;
case 102:
# line 466 "/usr/src/cmd/mip/cgram.y"
{  if( yypvt[-1].intval != NOLAB ){
				deflab( yypvt[-1].intval );
				reached = 1;
				}
			    } break;
case 103:
# line 472 "/usr/src/cmd/mip/cgram.y"
{  branch(  contlab );
			    deflab( brklab );
			    if( (flostat&FBRK) || !(flostat&FLOOP)) reached = 1;
			    else reached = 0;
			    resetbc(0);
			    } break;
case 104:
# line 479 "/usr/src/cmd/mip/cgram.y"
{  deflab( contlab );
			    if( flostat & FCONT ) reached = 1;
			    ecomp( buildtree( CBRANCH, buildtree( NOT, yypvt[-2].nodep, NIL ), bcon( yypvt[-6].intval ) ) );
			    deflab( brklab );
			    reached = 1;
			    resetbc(0);
			    } break;
case 105:
# line 487 "/usr/src/cmd/mip/cgram.y"
{  deflab( contlab );
			    if( flostat&FCONT ) reached = 1;
			    if( yypvt[-2].nodep ) ecomp( yypvt[-2].nodep );
			    branch( yypvt[-3].intval );
			    deflab( brklab );
			    if( (flostat&FBRK) || !(flostat&FLOOP) ) reached = 1;
			    else reached = 0;
			    resetbc(0);
			    } break;
case 106:
# line 497 "/usr/src/cmd/mip/cgram.y"
{  if( reached ) branch( brklab );
			    deflab( yypvt[-1].intval );
			   swend();
			    deflab(brklab);
			    if( (flostat&FBRK) || !(flostat&FDEF) ) reached = 1;
			    resetbc(FCONT);
			    } break;
case 107:
# line 505 "/usr/src/cmd/mip/cgram.y"
{  if( brklab == NOLAB ) uerror( "illegal break");
			    else if(reached) branch( brklab );
			    flostat |= FBRK;
			    if( brkflag ) goto rch;
			    reached = 0;
			    } break;
case 108:
# line 512 "/usr/src/cmd/mip/cgram.y"
{  if( contlab == NOLAB ) uerror( "illegal continue");
			    else branch( contlab );
			    flostat |= FCONT;
			    goto rch;
			    } break;
case 109:
# line 518 "/usr/src/cmd/mip/cgram.y"
{  retstat |= NRETVAL;
			    branch( retlab );
			rch:
			    if( !reached ) werror( "statement not reached");
			    reached = 0;
			    } break;
case 110:
# line 525 "/usr/src/cmd/mip/cgram.y"
{  register NODE *temp;
			    idname = curftn;
			    temp = buildtree( NAME, NIL, NIL );
			    temp->type = DECREF( temp->type );
			    temp = buildtree( RETURN, temp, yypvt[-1].nodep );
			    /* now, we have the type of the RHS correct */
			    temp->left->op = FREE;
			    temp->op = FREE;
			    ecomp( buildtree( FORCE, temp->right, NIL ) );
			    retstat |= RETVAL;
			    branch( retlab );
			    reached = 0;
			    } break;
case 111:
# line 539 "/usr/src/cmd/mip/cgram.y"
{  register NODE *q;
			    q = block( FREE, NIL, NIL, INT|ARY, 0, INT );
			    q->rval = idname = yypvt[-1].intval;
			    defid( q, ULABEL );
			    stab[idname].suse = -lineno;
			    branch( stab[idname].offset );
			    goto rch;
			    } break;
case 116:
# line 553 "/usr/src/cmd/mip/cgram.y"
{  register NODE *q;
			    q = block( FREE, NIL, NIL, INT|ARY, 0, LABEL );
			    q->rval = yypvt[-1].intval;
			    defid( q, LABEL );
			    reached = 1;
			    } break;
case 117:
# line 560 "/usr/src/cmd/mip/cgram.y"
{  addcase(yypvt[-1].nodep);
			    reached = 1;
			    } break;
case 118:
# line 564 "/usr/src/cmd/mip/cgram.y"
{  reached = 1;
			    adddef();
			    flostat |= FDEF;
			    } break;
case 119:
# line 570 "/usr/src/cmd/mip/cgram.y"
{  savebc();
			    if( !reached ) werror( "loop not entered at top");
			    brklab = getlab();
			    contlab = getlab();
			    deflab( yyval.intval = getlab() );
			    reached = 1;
			    } break;
case 120:
# line 579 "/usr/src/cmd/mip/cgram.y"
{  ecomp( buildtree( CBRANCH, yypvt[-1].nodep, bcon( yyval.intval=getlab()) ) ) ;
			    reached = 1;
			    } break;
case 121:
# line 584 "/usr/src/cmd/mip/cgram.y"
{  if( reached ) branch( yyval.intval = getlab() );
			    else yyval.intval = NOLAB;
			    deflab( yypvt[-2].intval );
			    reached = 1;
			    } break;
case 122:
# line 592 "/usr/src/cmd/mip/cgram.y"
{  savebc();
			    if( !reached ) werror( "loop not entered at top");
			    if( yypvt[-1].nodep->op == ICON && yypvt[-1].nodep->lval != 0 ) flostat = FLOOP;
			    deflab( contlab = getlab() );
			    reached = 1;
			    brklab = getlab();
			    if( flostat == FLOOP ) tfree( yypvt[-1].nodep );
			    else ecomp( buildtree( CBRANCH, yypvt[-1].nodep, bcon( brklab) ) );
			    } break;
case 123:
# line 603 "/usr/src/cmd/mip/cgram.y"
{  if( yypvt[-3].nodep ) ecomp( yypvt[-3].nodep );
			    else if( !reached ) werror( "loop not entered at top");
			    savebc();
			    contlab = getlab();
			    brklab = getlab();
			    deflab( yyval.intval = getlab() );
			    reached = 1;
			    if( yypvt[-1].nodep ) ecomp( buildtree( CBRANCH, yypvt[-1].nodep, bcon( brklab) ) );
			    else flostat |= FLOOP;
			    } break;
case 124:
# line 615 "/usr/src/cmd/mip/cgram.y"
{  savebc();
			    brklab = getlab();
			    ecomp( buildtree( FORCE, yypvt[-1].nodep, NIL ) );
			    branch( yyval.intval = getlab() );
			    swstart();
			    reached = 0;
			    } break;
case 125:
# line 624 "/usr/src/cmd/mip/cgram.y"
{ yyval.intval=instruct; stwart=instruct=0; } break;
case 126:
# line 626 "/usr/src/cmd/mip/cgram.y"
{  yyval.intval = icons( yypvt[-0].nodep );  instruct=yypvt[-1].intval; } break;
case 128:
# line 630 "/usr/src/cmd/mip/cgram.y"
{ yyval.nodep=0; } break;
case 130:
# line 635 "/usr/src/cmd/mip/cgram.y"
{  goto bop; } break;
case 131:
# line 639 "/usr/src/cmd/mip/cgram.y"
{
			preconf:
			    if( yychar==RELOP||yychar==EQUOP||yychar==AND||yychar==OR||yychar==ER ){
			    precplaint:
				if( hflag ) werror( "precedence confusion possible: parenthesize!" );
				}
			bop:
			    yyval.nodep = buildtree( yypvt[-1].intval, yypvt[-2].nodep, yypvt[-0].nodep );
			    } break;
case 132:
# line 649 "/usr/src/cmd/mip/cgram.y"
{  yypvt[-1].intval = COMOP;
			    goto bop;
			    } break;
case 133:
# line 653 "/usr/src/cmd/mip/cgram.y"
{  goto bop; } break;
case 134:
# line 655 "/usr/src/cmd/mip/cgram.y"
{  if(yychar==SHIFTOP) goto precplaint; else goto bop; } break;
case 135:
# line 657 "/usr/src/cmd/mip/cgram.y"
{  if(yychar==SHIFTOP ) goto precplaint; else goto bop; } break;
case 136:
# line 659 "/usr/src/cmd/mip/cgram.y"
{  if(yychar==PLUS||yychar==MINUS) goto precplaint; else goto bop; } break;
case 137:
# line 661 "/usr/src/cmd/mip/cgram.y"
{  goto bop; } break;
case 138:
# line 663 "/usr/src/cmd/mip/cgram.y"
{  goto preconf; } break;
case 139:
# line 665 "/usr/src/cmd/mip/cgram.y"
{  if( yychar==RELOP||yychar==EQUOP ) goto preconf;  else goto bop; } break;
case 140:
# line 667 "/usr/src/cmd/mip/cgram.y"
{  if(yychar==RELOP||yychar==EQUOP) goto preconf; else goto bop; } break;
case 141:
# line 669 "/usr/src/cmd/mip/cgram.y"
{  if(yychar==RELOP||yychar==EQUOP) goto preconf; else goto bop; } break;
case 142:
# line 671 "/usr/src/cmd/mip/cgram.y"
{  goto bop; } break;
case 143:
# line 673 "/usr/src/cmd/mip/cgram.y"
{  goto bop; } break;
case 144:
# line 675 "/usr/src/cmd/mip/cgram.y"
{  abop:
				yyval.nodep = buildtree( ASG yypvt[-2].intval, yypvt[-3].nodep, yypvt[-0].nodep );
				} break;
case 145:
# line 679 "/usr/src/cmd/mip/cgram.y"
{  goto abop; } break;
case 146:
# line 681 "/usr/src/cmd/mip/cgram.y"
{  goto abop; } break;
case 147:
# line 683 "/usr/src/cmd/mip/cgram.y"
{  goto abop; } break;
case 148:
# line 685 "/usr/src/cmd/mip/cgram.y"
{  goto abop; } break;
case 149:
# line 687 "/usr/src/cmd/mip/cgram.y"
{  goto abop; } break;
case 150:
# line 689 "/usr/src/cmd/mip/cgram.y"
{  goto abop; } break;
case 151:
# line 691 "/usr/src/cmd/mip/cgram.y"
{  goto abop; } break;
case 152:
# line 693 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep=buildtree(QUEST, yypvt[-4].nodep, buildtree( COLON, yypvt[-2].nodep, yypvt[-0].nodep ) );
			    } break;
case 153:
# line 696 "/usr/src/cmd/mip/cgram.y"
{  werror( "old-fashioned assignment operator" );  goto bop; } break;
case 154:
# line 698 "/usr/src/cmd/mip/cgram.y"
{  goto bop; } break;
case 156:
# line 702 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = buildtree( yypvt[-0].intval, yypvt[-1].nodep, bcon(1) ); } break;
case 157:
# line 704 "/usr/src/cmd/mip/cgram.y"
{ ubop:
			    yyval.nodep = buildtree( UNARY yypvt[-1].intval, yypvt[-0].nodep, NIL );
			    } break;
case 158:
# line 708 "/usr/src/cmd/mip/cgram.y"
{  if( ISFTN(yypvt[-0].nodep->type) || ISARY(yypvt[-0].nodep->type) ){
				werror( "& before array or function: ignored" );
				yyval.nodep = yypvt[-0].nodep;
				}
			    else goto ubop;
			    } break;
case 159:
# line 715 "/usr/src/cmd/mip/cgram.y"
{  goto ubop; } break;
case 160:
# line 717 "/usr/src/cmd/mip/cgram.y"
{
			    yyval.nodep = buildtree( yypvt[-1].intval, yypvt[-0].nodep, NIL );
			    } break;
case 161:
# line 721 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = buildtree( yypvt[-1].intval==INCR ? ASG PLUS : ASG MINUS,
						yypvt[-0].nodep,
						bcon(1)  );
			    } break;
case 162:
# line 726 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = doszof( yypvt[-0].nodep ); } break;
case 163:
# line 728 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = buildtree( CAST, yypvt[-2].nodep, yypvt[-0].nodep );
			    yyval.nodep->left->op = FREE;
			    yyval.nodep->op = FREE;
			    yyval.nodep = yyval.nodep->right;
			    } break;
case 164:
# line 734 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = doszof( yypvt[-1].nodep ); } break;
case 165:
# line 736 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = buildtree( UNARY MUL, buildtree( PLUS, yypvt[-3].nodep, yypvt[-1].nodep ), NIL ); } break;
case 166:
# line 738 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep=buildtree(UNARY CALL,yypvt[-1].nodep,NIL); } break;
case 167:
# line 740 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep=buildtree(CALL,yypvt[-2].nodep,yypvt[-1].nodep); } break;
case 168:
# line 742 "/usr/src/cmd/mip/cgram.y"
{  if( yypvt[-1].intval == DOT ){
				yypvt[-2].nodep = buildtree( UNARY AND, yypvt[-2].nodep, NIL );
				}
			    idname = yypvt[-0].intval;
			    yyval.nodep = buildtree( STREF, yypvt[-2].nodep, buildtree( NAME, NIL, NIL ) );
			    } break;
case 169:
# line 749 "/usr/src/cmd/mip/cgram.y"
{  idname = yypvt[-0].intval;
			    /* recognize identifiers in initializations */
			    if( blevel==0 && stab[idname].stype == UNDEF ) {
				register NODE *q;
				werror( "undeclared initializer name %.8s", stab[idname].sname );
				q = block( FREE, NIL, NIL, INT, 0, INT );
				q->rval = idname;
				defid( q, EXTERN );
				}
			    yyval.nodep=buildtree(NAME,NIL,NIL);
			    stab[yypvt[-0].intval].suse = -lineno;
			} break;
case 170:
# line 762 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep=bcon(0);
			    yyval.nodep->lval = lastcon;
			    yyval.nodep->rval = NONAME;
			    if( yypvt[-0].intval ) yyval.nodep->csiz = yyval.nodep->type = ctype(LONG);
			    } break;
case 171:
# line 768 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep=buildtree(FCON,NIL,NIL);
			    yyval.nodep->dval = dcon;
			    } break;
case 172:
# line 772 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = getstr(); /* get string contents */ } break;
case 173:
# line 774 "/usr/src/cmd/mip/cgram.y"
{ yyval.nodep=yypvt[-1].nodep; } break;
case 174:
# line 778 "/usr/src/cmd/mip/cgram.y"
{
			yyval.nodep = tymerge( yypvt[-1].nodep, yypvt[-0].nodep );
			yyval.nodep->op = NAME;
			yypvt[-1].nodep->op = FREE;
			} break;
case 175:
# line 786 "/usr/src/cmd/mip/cgram.y"
{ yyval.nodep = bdty( NAME, NIL, -1 ); } break;
case 176:
# line 788 "/usr/src/cmd/mip/cgram.y"
{ yyval.nodep = bdty( UNARY CALL, bdty(NAME,NIL,-1),0); } break;
case 177:
# line 790 "/usr/src/cmd/mip/cgram.y"
{  yyval.nodep = bdty( UNARY CALL, yypvt[-3].nodep, 0 ); } break;
case 178:
# line 792 "/usr/src/cmd/mip/cgram.y"
{  goto umul; } break;
case 179:
# line 794 "/usr/src/cmd/mip/cgram.y"
{  goto uary; } break;
case 180:
# line 796 "/usr/src/cmd/mip/cgram.y"
{  goto bary;  } break;
case 181:
# line 798 "/usr/src/cmd/mip/cgram.y"
{ yyval.nodep = yypvt[-1].nodep; } break;
case 182:
# line 802 "/usr/src/cmd/mip/cgram.y"
{  if( stab[yypvt[-1].intval].stype == UNDEF ){
				register NODE *q;
				q = block( FREE, NIL, NIL, FTN|INT, 0, INT );
				q->rval = yypvt[-1].intval;
				defid( q, EXTERN );
				}
			    idname = yypvt[-1].intval;
			    yyval.nodep=buildtree(NAME,NIL,NIL);
			    stab[idname].suse = -lineno;
			} break;
		}
		goto yystack;  /* stack new state and value */

	}
