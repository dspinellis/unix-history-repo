/*
 * Copyright (c) 1979 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)ey2.c	5.1 (Berkeley) 4/29/85";
#endif not lint

# include "ey.h"
# define IDENTIFIER 257
# define MARK 258
# define TERM 259
# define LEFT 260
# define BINARY 261
# define RIGHT 262
# define PREC 263
# define LCURLY 264
# define C_IDENTIFIER 265  /* name followed by colon */
# define NUMBER 266

FILE	*copen();

setup(argc,argv) int argc; char *argv[];
{	int i,j,lev,t;
	int c;

	foutput = stdout;
	i = 1;
	while( argc >= 2  && argv[1][0] == '-' ) {
		while( *++(argv[1]) ){
			switch( *argv[1] ){
			case 'v':
			case 'V':
				foutput = copen("y.output", 'w' );
				if( foutput == 0 ) error( "cannot open y.output");
				continue;
			case 'o':
			case 'O':
				oflag = 1;
				continue;
			case 'r':
			case 'R':
				oflag = 1;
				rflag = 1;
				continue;
			default:  error( "illegal option: %c", *argv[1]);
				}
			}
		argv++;
		argc--;
		}

	ftable = copen( oflag ? "yacc.tmp" : "y.tab.c" , 'w' );
	if( ftable==0 ) error( "cannot open table file" );
	if( argc > 1 ) { cin = copen( argv[1], 'r' );
	if( cin == 0 ) error( "cannot open input" );
	}
	settab();
	fprintf( cout , "#\n");
	ctokn = "$end";
	defin(0);  /* eof */
	extval = 0400;  /* beginning of assigned values */
	ctokn = "error";
	defin(0);
	ctokn = "$accept";
	defin(1);
	mem=mem0;
	cnamp = cnames;
	lev=0;
	i=0;

	while( ( t = gettok() ) != EOF ) {
		switch( t ){
			case IDENTIFIER:	j = chfind(0);
					trmlev[j] = lev;
					continue;
			case ',':
			case ';':		continue;
			case TERM:		lev=0; continue;
			case LEFT:		lev=(++i<<3)|01; continue;
			case BINARY:	lev=(++i<<3)|02; continue;
			case RIGHT:	lev=(++i<<3)|03; continue;
			case MARK:
					defout();
					if( rflag ){ /* RATFOR */
						fprintf( cout ,  "define yyerrok yyerrf = 0\n" );
						fprintf( cout ,  "define yyclearin yychar = -1\n" );
						fprintf( cout ,  "subroutine yyactr(yyprdn)\n");
						fprintf( cout ,  "common/yycomn/yylval,yyval,yypv,yyvalv(150)\n" );
						fprintf( cout ,  "common/yylcom/yychar,yyerrf,yydebu\n" );
						fprintf( cout ,  "integer yychar, yyerrf, yydebu\n" );
						fprintf( cout ,  "integer yyprdn,yyval,yylval,yypv,yyvalv\n" );
						}
					else {
						fprintf( cout ,  "#define yyclearin yychar = -1\n" );
						fprintf( cout ,  "#define yyerrok yyerrflag = 0\n" );
						fprintf( cout ,  "extern int yychar, yyerrflag;\n" );
						fprintf( cout , "\nint yyval 0;\nint *yypv;\nint yylval 0;");
						fprintf( cout , "\nyyactr(__np__){\n");
						}
					break;
			case LCURLY:	defout();
					cpycode();
					continue;
			case NUMBER:
				trmset[j].value = numbval;
				if( j < ndefout && j>2 ) 
					error("please define type # of %s earlier", trmset[j].name );
				continue;
			default:	error("bad precedence syntax, input %d", t );
			}
		break;
		}
	prdptr[0]=mem;
	/* added production */
	*mem++ = NTBASE;
	*mem++ = NTBASE+1;
	*mem++ = 1;
	*mem++ = 0;
	prdptr[1]=mem;
	i=0;

	/* i is 0 when a rule can begin, 1 otherwise */

	for(;;) switch( t=gettok() ) {
	case C_IDENTIFIER:		if( mem == prdptr[1] ) {  /* first time */
						if( rflag ){
							fprintf( cout ,  "goto 1000\n" );
							}
						else fprintf( cout , "\nswitch(__np__){\n");
						}
				if( i != 0 ) error( "previous rule not terminated" );
				*mem = chfind(1);
				if( *mem < NTBASE )error( "token illegal on lhs of grammar rule" );
				i=1;
				++mem;
				continue;
	case IDENTIFIER:
			*mem=chfind(1);
			if(*mem < NTBASE)levprd[nprod]=trmlev[*mem];
			mem++;
			if(i==0) error("missing :");
			continue;
	case '=':		levprd[nprod] |= 04;
				if( i==0 ) error("semicolon preceeds action");
			fprintf( cout ,  rflag?"\n%d ":"\ncase %d:", nprod );
			cpyact();
			fprintf( cout ,  rflag ? " return" : " break;" );
	case '|':
	case ';':		if(i){
				*mem++ = -nprod;
				prdptr[++nprod] = mem;
				levprd[nprod]=0;
				i=0;}
			if (t=='|'){i=1;*mem++ = *prdptr[nprod-1];}
			continue;
	case 0:		/* End Of File */
	case EOF:
	case MARK:	if( i != 0 ) error( "rule not terminated before %%%% or EOF" );
			settab();
			finact();
			/* copy the programs which follow the rules */
			if( t == MARK ){
				while (( c=fgetc( cin)) != EOF ) fputc(c,cout);
				}
			return;
	case PREC:	
		if( i==0 ) error( "%%prec must appear inside rule" );
		if( gettok()!=IDENTIFIER)error("illegal %%prec syntax" );
		j=chfind(2);
		if(j>=NTBASE)error("nonterminal %s illegal after %%prec", nontrst[j-NTBASE].name);
		levprd[nprod]=trmlev[j];
		continue;
	case LCURLY:	
		if( i!=0 ) error( "%%{ appears within a rule" );
		cpycode();
		continue;
	default: error( "syntax error, input %d", t  );
	}
}

finact(){
	/* finish action routine */
	register i;

	if( rflag ){

		fprintf( cout ,  "\n1000 goto(" );
		for( i=1; i<nprod; ++i ){
			fprintf( cout ,  "%d,", (levprd[i]&04)==0?999:i );
			}
		fprintf( cout ,  "999),yyprdn\n" );
		fprintf( cout ,  "999 return\nend\n" );
		fprintf( cout ,  "define YYERRCODE %d\n", trmset[2].value );
		}
	else {
		fprintf( cout ,  "\n}\n}\n" );
		fprintf( cout ,  "int yyerrval %d;\n", trmset[2].value );
		}
	}
defin(t) {
/*	define ctokn to be a terminal if t=0
	or a nonterminal if t=1		*/
	char *cp,*p;
	int c;


        if (t) {
          if( ++nnonter >= ntlim ) error("too many nonterminals, limit %d",ntlim);
	  nontrst[nnonter].name = ctokn;
	  return( NTBASE + nnonter );
          }
        else {
          if( ++nterms >= tlim ) error("too many terminals, limit %d",tlim );
          trmset[nterms].name = ctokn;
	if( ctokn[0]==' ' && ctokn[2]=='\0' ) /* single character literal */
		trmset[nterms].value = ctokn[1];
	else if ( ctokn[0]==' ' && ctokn[1]=='\\' ) { /* escape sequence */
		if( ctokn[3] == '\0' ){ /* single character escape sequence */
			switch ( ctokn[2] ){
				 /* character which is escaped */
			case 'n': trmset[nterms].value = '\n'; break;
			case 'r': trmset[nterms].value = '\r'; break;
			case 'b': trmset[nterms].value = '\b'; break;
			case 't': trmset[nterms].value = '\t'; break;
			case '\'': trmset[nterms].value = '\''; break;
			case '"': trmset[nterms].value = '"'; break;
			case '\\': trmset[nterms].value = '\\'; break;
			default: error( "invalid escape" );
				}
			}
		else if( ctokn[2] <= '7' && ctokn[2]>='0' ){ /* \nnn sequence */
			if( ctokn[3]<'0' || ctokn[3] > '7' || ctokn[4]<'0' ||
				ctokn[4]>'7' || ctokn[5] != '\0' ) error("illegal \\nnn construction" );
			trmset[nterms].value = 64*(ctokn[2]-'0')+8*(ctokn[3]-'0')+ctokn[4]-'0';
			if( trmset[nterms].value == 0 ) error( "'\\000' is illegal" );
			}
		}
	else {
		trmset[nterms].value = extval++;

		}
	trmlev[nterms] = 0;
	return( nterms );
          }
}

defout(){ /* write out the defines (at the end of the declaration section) */

	_REGISTER int i, c;
	_REGISTER char *cp;

	for( i=ndefout; i<=nterms; ++i ){

		cp = trmset[i].name;
		if( *cp == ' ' ) ++cp;  /* literals */

		for( ; (c= *cp)!='\0'; ++cp ){

			if( c>='a' && c<='z' ||
			    c>='A' && c<='Z' ||
			    c>='0' && c<='9' ||
			    c=='_' )  ; /* VOID */
			else goto nodef;
			}

		/* define it */

		fprintf( cout ,  "%c define %s %d\n", rflag?' ':'#', trmset[i].name, trmset[i].value );

	nodef:	;
		}

	ndefout = nterms+1;

	}

chstash( c ){
  /* put character away into cnames */
  if( cnamp >= &cnames[cnamsz] ) error("too many characters in id's and literals" );
  else *cnamp++ = c;
  }

int gettok() {
	int j, base;
	static int peekline; /* number of '\n' seen in lookahead */
	auto int c, match, reserve;

begin:
	reserve = 0;
        if( peekc>=0 ) {
		c = peekc;
		lineno += peekline;
		peekc = -1;
		peekline = 0;
		}
        else c = fgetc( cin);
        while( c==' ' || c=='\n' || c=='\t' || c == '\014'){
          if( c == '\n' ) ++lineno;
          c=fgetc( cin);
          }
	if (c=='/')
		{if (fgetc( cin)!='*')error("illegal /");
		c=fgetc( cin);
		while(c != EOF) {
			if( c == '\n' ) ++lineno;
			if (c=='*')
				{if((c=fgetc( cin))=='/')break;}
			else c=fgetc( cin);}
		if (!c) return(0);
		goto begin;}
	j=0;
	switch(c){
	case '"':	
	case '\'':	match = c;
			ctokn = cnamp;
			chstash( ' ' );
			while(1){
				c = fgetc( cin);
				if( c == '\n' || c == '\0' )
					error("illegal or missing ' or \"");
				if( c == '\\' ){
					c = fgetc( cin);
					chstash( '\\' );
					}
				else if( c == match ) break;
				chstash( c );
				}
			break;
	case '%':
	case '\\':	switch(c=fgetc( cin))
		{case '0':	return(TERM);
		case '<':	return(LEFT);
		case '2':	return(BINARY);
		case '>':	return(RIGHT);
		case '%':
		case '\\':	return(MARK);
		case '=':	return(PREC);
		case '{':	return(LCURLY);
		default:	reserve = 1;
		}
	default:	if( c >= '0' && c <= '9' ){ /* number */
				numbval = c-'0' ;
				base = (c=='0') ? 8 : 10 ;
				for( c=fgetc( cin); c>='0' && c<='9'; c=fgetc( cin) ){
					numbval = numbval*base + c - '0';
					}
				peekc = c;
				return(NUMBER);
				}
			else if( (c>='a'&&c<='z')||(c>='A'&&c<='Z')||c=='_'||c=='.'||c=='$'){
				ctokn = cnamp;
				while(	(c>='a'&&c<='z') ||
					(c>='A'&&c<='Z') ||
					(c>='0'&&c<='9') ||
					c=='_' || c=='.' || c=='$' ) {
					chstash( c );
					if( peekc>=0 ) { c = peekc; peekc = -1; }
					else c = fgetc( cin);
					}
				}
			else return(c);

			peekc=c;
			}
	chstash( '\0' );

	if( reserve ){ /* find a reserved word */
		if( compare("term")) return( TERM );
		if( compare("TERM")) return( TERM );
		if( compare("token")) return( TERM );
		if( compare("TOKEN")) return( TERM );
		if( compare("left")) return( LEFT );
		if( compare("LEFT")) return( LEFT );
		if( compare("nonassoc")) return( BINARY );
		if( compare("NONASSOC")) return( BINARY );
		if( compare("binary")) return( BINARY );
		if( compare("BINARY")) return( BINARY );
		if( compare("right")) return( RIGHT );
		if( compare("RIGHT")) return( RIGHT );
		if( compare("prec")) return( PREC );
		if( compare("PREC")) return( PREC );
		error("invalid escape, or illegal reserved word: %s", ctokn );
		}

	/* look ahead to distinguish IDENTIFIER from C_IDENTIFIER */

  look:
	while( peekc==' ' || peekc=='\t' || peekc == '\n' || peekc == '\014' )
	{
		if( peekc == '\n' ) ++peekline;
		peekc = fgetc( cin);
	}

	if( peekc != ':' ) return( IDENTIFIER );
	peekc = -1;
	lineno += peekline;
	peekline = 0;
	return( C_IDENTIFIER );
}
chfind(t)

{	int i,j;

	if (ctokn[0]==' ')t=0;
	for(i=1;i<=nterms;i++)
		if(compare(trmset[i].name)){
			cnamp = ctokn;
			return( i );
			}
	for(i=1;i<=nnonter;i++)
		if(compare(nontrst[i].name)) {
			cnamp = ctokn;
			return( i+NTBASE );
			}
	/* cannot find name */
	if( t>1 && ctokn[0] != ' ' )
		error( "%s should have been defined earlier", ctokn );
	return( defin( t ) );
	}

cpycode(){ /* copies code between \{ and \} */

	int c;
	c = fgetc( cin);
	if( c == '\n' ) {
		c = fgetc( cin);
		lineno++;
		}
	while( c != EOF ){
		if( c=='\\' )
			if( (c=fgetc( cin)) == '}' ) return;
			else fputc('\\',cout);
		if( c=='%' )
			if( (c=fgetc( cin)) == '}' ) return;
			else fputc('%',cout);
		fputc( c, cout );
		if( c == '\n' ) ++lineno;
		c = fgetc( cin);
		}
	error("eof before %%}");
	}

cpyact(){ /* copy C action to the next ; or closing } */
	int brac, c, match, *i, j, s;

	brac = 0;

loop:
	c = fgetc( cin);
swt:
	switch( c ){

case ';':
		if( brac == 0 ){
			fputc( c, cout );
			return;
			}
		goto lcopy;

case '{':
		brac++;
		goto lcopy;

case '$':
		s = 1;
		c = fgetc( cin);
		if( c == '$' ){
			fprintf( cout , "yyval");
			goto loop;
			}
		if( c == '-' ){
			s = -s;
			c = fgetc( cin);
			}
		if( c>='0' && c <= '9' ){
			j=0;
			while( c>='0' && c<= '9' ){
				j= j*10+c-'0';
				c = fgetc( cin);
				}
			if( rflag ) fprintf( cout ,  "yyvalv(yypv%c%d)", s==1?'+':'-', j );
			else fprintf( cout , "yypv[%d]", s*j );
			goto swt;
			}
		fputc( '$' , cout);
		if( s<0 ) fputc('-', cout);
		goto swt;

case '}':
		brac--;
		if( brac == 0 ){
			fputc( c , cout);
			return;
			}
		goto lcopy;

case '/':	/* look for comments */
		fputc( c ,cout);
		c = fgetc( cin);
		if( c != '*' ) goto swt;

		/* it really is a comment */

		fputc( c , cout);
		while( (c=fgetc( cin)) != EOF ){
			if( c=='*' ){
				fputc( c , cout);
				if( (c=fgetc( cin)) == '/' ) goto lcopy;
				}
			fputc( c , cout);
			}
		error( "EOF inside comment" );

case '\'':	/* character constant */
		match = '\'';
		goto string;

case '"':	/* character string */
		match = '"';

	string:

		fputc( c , cout);
		while( (c=fgetc( cin)) != EOF ){

			if( c=='\\' ){
				fputc( c , cout);
				c=fgetc( cin);
				}
			else if( c==match ) goto lcopy;
			fputc( c , cout);
			}
		error( "EOF in string or character constant" );

case '\0':
		error("action does not terminate");
case '\n':	++lineno;
		goto lcopy;

		}

lcopy:
	fputc( c , cout);
	goto loop;
	}
