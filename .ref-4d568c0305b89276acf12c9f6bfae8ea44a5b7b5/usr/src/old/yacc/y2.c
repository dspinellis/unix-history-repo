#ifndef lint
static char sccsid[] = "@(#)y2.c	4.1	(Berkeley)	%G%";
#endif not lint

# include "dextern"
# define IDENTIFIER 257
# define MARK 258
# define TERM 259
# define LEFT 260
# define RIGHT 261
# define BINARY 262
# define PREC 263
# define LCURLY 264
# define C_IDENTIFIER 265  /* name followed by colon */
# define NUMBER 266
# define START 267
# define TYPEDEF 268
# define TYPENAME 269
# define UNION 270
# define ENDFILE 0

	/* communication variables between various I/O routines */

char *infile;	/* input file name */
int numbval;	/* value of an input number */
char tokname[NAMESIZE];	/* input token name */

	/* storage of names */

char cnames[CNAMSZ];	/* place where token and nonterminal names are stored */
int cnamsz = CNAMSZ;	/* size of cnames */
char * cnamp = cnames;	/* place where next name is to be put in */
int ndefout = 3;  /* number of defined symbols output */

	/* storage of types */
int ntypes;	/* number of types defined */
char * typeset[NTYPES];	/* pointers to type tags */

	/* symbol tables for tokens and nonterminals */

int ntokens = 0;
struct toksymb tokset[NTERMS];
int toklev[NTERMS];
int nnonter = -1;
struct ntsymb nontrst[NNONTERM];
int start;	/* start symbol */

	/* assigned token type values */
int extval = 0;

	/* input and output file descriptors */

FILE * finput;		/* yacc input file */
FILE * faction;		/* file for saving actions */
FILE * fdefine;		/* file for # defines */
FILE * ftable;		/* y.tab.c file */
FILE * ftemp;		/* tempfile to pass 2 */
FILE * foutput;		/* y.output file */

	/* storage for grammar rules */

int mem0[MEMSIZE] ; /* production storage */
int *mem = mem0;
int nprod= 1;	/* number of productions */
int *prdptr[NPROD];	/* pointers to descriptions of productions */
int levprd[NPROD] ;	/* precedence levels for the productions */


setup(argc,argv) int argc; char *argv[];
{	int i,j,lev,t, ty;
	int c;
	int *p;
	char actname[8];

	foutput = NULL;
	fdefine = NULL;
	i = 1;
	while( argc >= 2  && argv[1][0] == '-' ) {
		while( *++(argv[1]) ){
			switch( *argv[1] ){
			case 'v':
			case 'V':
				foutput = fopen(FILEU, "w" );
				if( foutput == NULL ) error( "cannot open y.output" );
				continue;
			case 'D':
			case 'd':
				fdefine = fopen( FILED, "w" );
				continue;
			case 'o':
			case 'O':
				fprintf( stderr, "`o' flag now default in yacc\n" );
				continue;

			case 'r':
			case 'R':
				error( "Ratfor Yacc is dead: sorry...\n" );

			default:
				error( "illegal option: %c", *argv[1]);
				}
			}
		argv++;
		argc--;
		}

	ftable = fopen( OFILE, "w" );
	if( ftable == NULL ) error( "cannot open table file" );

	ftemp = fopen( TEMPNAME, "w" );
	faction = fopen( ACTNAME, "w" );
	if( ftemp==NULL || faction==NULL ) error( "cannot open temp file" );

	if( argc < 2 || ((finput=fopen( infile=argv[1], "r" )) == NULL ) ){
		error( "cannot open input file" );
		}

	cnamp = cnames;
	defin(0,"$end");
	extval = 0400;
	defin(0,"error");
	defin(1,"$accept");
	mem=mem0;
	lev = 0;
	ty = 0;
	i=0;

	/* sorry -- no yacc parser here.....
		we must bootstrap somehow... */

	for( t=gettok();  t!=MARK && t!= ENDFILE; ){
		switch( t ){

		case ';':
			t = gettok();
			break;

		case START:
			if( (t=gettok()) != IDENTIFIER ){
				error( "bad %%start construction" );
				}
			start = chfind(1,tokname);
			t = gettok();
			continue;

		case TYPEDEF:
			if( (t=gettok()) != TYPENAME ) error( "bad syntax in %%type" );
			ty = numbval;
			for(;;){
				t = gettok();
				switch( t ){

				case IDENTIFIER:
					if( (t=chfind( 1, tokname ) ) < NTBASE ) {
						j = TYPE( toklev[t] );
						if( j!= 0 && j != ty ){
							error( "type redeclaration of token %s",
								tokset[t].name );
							}
						else SETTYPE( toklev[t],ty);
						}
					else {
						j = nontrst[t-NTBASE].tvalue;
						if( j != 0 && j != ty ){
							error( "type redeclaration of nonterminal %s",
								nontrst[t-NTBASE].name );
							}
						else nontrst[t-NTBASE].tvalue = ty;
						}
				case ',':
					continue;

				case ';':
					t = gettok();
					break;
				default:
					break;
					}
				break;
				}
			continue;

		case UNION:
			/* copy the union declaration to the output */
			cpyunion();
			t = gettok();
			continue;

		case LEFT:
		case BINARY:
		case RIGHT:
			++i;
		case TERM:
			lev = t-TERM;  /* nonzero means new prec. and assoc. */
			ty = 0;

			/* get identifiers so defined */

			t = gettok();
			if( t == TYPENAME ){ /* there is a type defined */
				ty = numbval;
				t = gettok();
				}

			for(;;) {
				switch( t ){

				case ',':
					t = gettok();
					continue;

				case ';':
					break;

				case IDENTIFIER:
					j = chfind(0,tokname);
					if( lev ){
						if( ASSOC(toklev[j]) ) error( "redeclaration of precedence of %s", tokname );
						SETASC(toklev[j],lev);
						SETPLEV(toklev[j],i);
						}
					if( ty ){
						if( TYPE(toklev[j]) ) error( "redeclaration of type of %s", tokname );
						SETTYPE(toklev[j],ty);
						}
					if( (t=gettok()) == NUMBER ){
						tokset[j].value = numbval;
						if( j < ndefout && j>2 ){
							error( "please define type number of %s earlier",
								tokset[j].name );
							}
						t=gettok();
						}
					continue;

					}

				break;
				}

			continue;

		case LCURLY:
			defout();
			cpycode();
			t = gettok();
			continue;

		default:
			error( "syntax error" );

			}

		}

	if( t == ENDFILE ){
		error( "unexpected EOF before %%" );
		}

	/* t is MARK */

	defout();

		fprintf( ftable,  "#define yyclearin yychar = -1\n" );
		fprintf( ftable,  "#define yyerrok yyerrflag = 0\n" );
		fprintf( ftable,  "extern int yychar;\nextern short yyerrflag;\n" );
		fprintf( ftable,  "#ifndef YYMAXDEPTH\n#define YYMAXDEPTH 150\n#endif\n" );
		if( !ntypes ) fprintf( ftable,  "#ifndef YYSTYPE\n#define YYSTYPE int\n#endif\n" );
		fprintf( ftable,  "YYSTYPE yylval, yyval;\n" );

	prdptr[0]=mem;
	/* added production */
	*mem++ = NTBASE;
	*mem++ = start;  /* if start is 0, we will overwrite with the lhs of the first rule */
	*mem++ = 1;
	*mem++ = 0;
	prdptr[1]=mem;

	while( (t=gettok()) == LCURLY ) cpycode();

	if( t != C_IDENTIFIER ) error( "bad syntax on first rule" );

	if( !start ) prdptr[0][1] = chfind(1,tokname);

	/* read rules */

	while( t!=MARK && t!=ENDFILE ){

		/* process a rule */

		if( t == '|' ){
			*mem++ = *prdptr[nprod-1];
			}
		else if( t == C_IDENTIFIER ){
			*mem = chfind(1,tokname);
			if( *mem < NTBASE ) error( "token illegal on LHS of grammar rule" );
			++mem;
			}
		else error( "illegal rule: missing semicolon or | ?" );

		/* read rule body */


		t = gettok();
	more_rule:
		while( t == IDENTIFIER ) {
			*mem = chfind(1,tokname);
			if( *mem<NTBASE ) levprd[nprod] = toklev[*mem];
			++mem;
			t = gettok();
			}


		if( t == PREC ){
			if( gettok()!=IDENTIFIER) error( "illegal %%prec syntax" );
			j = chfind(2,tokname);
			if( j>=NTBASE)error("nonterminal %s illegal after %%prec", nontrst[j-NTBASE].name);
			levprd[nprod]=toklev[j];
			t = gettok();
			}

		if( t == '=' ){
			levprd[nprod] |= ACTFLAG;
			fprintf( faction, "\ncase %d:", nprod );
			cpyact( mem-prdptr[nprod]-1 );
			fprintf( faction, " break;" );
			if( (t=gettok()) == IDENTIFIER ){
				/* action within rule... */

				sprintf( actname, "$$%d", nprod );
				j = chfind(1,actname);  /* make it a nonterminal */

				/* the current rule will become rule number nprod+1 */
				/* move the contents down, and make room for the null */

				for( p=mem; p>=prdptr[nprod]; --p ) p[2] = *p;
				mem += 2;

				/* enter null production for action */

				p = prdptr[nprod];

				*p++ = j;
				*p++ = -nprod;

				/* update the production information */

				levprd[nprod+1] = levprd[nprod] & ~ACTFLAG;
				levprd[nprod] = ACTFLAG;

				if( ++nprod >= NPROD ) error( "more than %d rules", NPROD );
				prdptr[nprod] = p;

				/* make the action appear in the original rule */
				*mem++ = j;

				/* get some more of the rule */

				goto more_rule;
				}

			}

		while( t == ';' ) t = gettok();

		*mem++ = -nprod;

		/* check that default action is reasonable */

		if( ntypes && !(levprd[nprod]&ACTFLAG) && nontrst[*prdptr[nprod]-NTBASE].tvalue ){
			/* no explicit action, LHS has value */
			register tempty;
			tempty = prdptr[nprod][1];
			if( tempty < 0 ) error( "must return a value, since LHS has a type" );
			else if( tempty >= NTBASE ) tempty = nontrst[tempty-NTBASE].tvalue;
			else tempty = TYPE( toklev[tempty] );
			if( tempty != nontrst[*prdptr[nprod]-NTBASE].tvalue ){
				error( "default action causes potential type clash" );
				}
			}

		if( ++nprod >= NPROD ) error( "more than %d rules", NPROD );
		prdptr[nprod] = mem;
		levprd[nprod]=0;

		}

	/* end of all rules */

	finact();
	if( t == MARK ){
		fprintf( ftable, "\n# line %d \"%s\"\n", lineno, infile );
		while( (c=getc(finput)) != EOF ) putc( c, ftable );
		}
	fclose( finput );
	}

finact(){
	/* finish action routine */

	fclose(faction);

	fprintf( ftable, "# define YYERRCODE %d\n", tokset[2].value );

	}

defin( t, s ) register char  *s; {
/*	define s to be a terminal if t=0
	or a nonterminal if t=1		*/

	register val;

	if (t) {
		if( ++nnonter >= NNONTERM ) error("too many nonterminals, limit %d",NNONTERM);
		nontrst[nnonter].name = cstash(s);
		return( NTBASE + nnonter );
		}
	/* must be a token */
	if( ++ntokens >= NTERMS ) error("too many terminals, limit %d",NTERMS );
	tokset[ntokens].name = cstash(s);

	/* establish value for token */

	if( s[0]==' ' && s[2]=='\0' ) /* single character literal */
		val = s[1];
	else if ( s[0]==' ' && s[1]=='\\' ) { /* escape sequence */
		if( s[3] == '\0' ){ /* single character escape sequence */
			switch ( s[2] ){
					 /* character which is escaped */
			case 'n': val = '\n'; break;
			case 'r': val = '\r'; break;
			case 'b': val = '\b'; break;
			case 't': val = '\t'; break;
			case 'f': val = '\f'; break;
			case '\'': val = '\''; break;
			case '"': val = '"'; break;
			case '\\': val = '\\'; break;
			default: error( "invalid escape" );
				}
			}
		else if( s[2] <= '7' && s[2]>='0' ){ /* \nnn sequence */
			if( s[3]<'0' || s[3] > '7' || s[4]<'0' ||
				s[4]>'7' || s[5] != '\0' ) error("illegal \\nnn construction" );
			val = 64*s[2] + 8*s[3] + s[4] - 73*'0';
			if( val == 0 ) error( "'\\000' is illegal" );
			}
		}
	else {
		val = extval++;
		}
	tokset[ntokens].value = val;
	toklev[ntokens] = 0;
	return( ntokens );
	}

defout(){ /* write out the defines (at the end of the declaration section) */

	register int i, c;
	register char *cp;

	for( i=ndefout; i<=ntokens; ++i ){

		cp = tokset[i].name;
		if( *cp == ' ' ) ++cp;  /* literals */

		for( ; (c= *cp)!='\0'; ++cp ){

			if( islower(c) || isupper(c) || isdigit(c) || c=='_' );  /* VOID */
			else goto nodef;
			}

		fprintf( ftable, "# define %s %d\n", tokset[i].name, tokset[i].value );
		if( fdefine != NULL ) fprintf( fdefine, "# define %s %d\n", tokset[i].name, tokset[i].value );

	nodef:	;
		}

	ndefout = ntokens+1;

	}

char *
cstash( s ) register char *s; {
	char *temp;

	temp = cnamp;
	do {
		if( cnamp >= &cnames[cnamsz] ) error("too many characters in id's and literals" );
		else *cnamp++ = *s;
		}  while ( *s++ );
	return( temp );
	}

gettok() {
	register i, base;
	static int peekline; /* number of '\n' seen in lookahead */
	register c, match, reserve;

begin:
	reserve = 0;
	lineno += peekline;
	peekline = 0;
	c = getc(finput);
	while( c==' ' || c=='\n' || c=='\t' || c=='\f' ){
		if( c == '\n' ) ++lineno;
		c=getc(finput);
		}
	if( c == '/' ){ /* skip comment */
		lineno += skipcom();
		goto begin;
		}

	switch(c){

	case EOF:
		return(ENDFILE);
	case '{':
		ungetc( c, finput );
		return( '=' );  /* action ... */
	case '<':  /* get, and look up, a type name (union member name) */
		i = 0;
		while( (c=getc(finput)) != '>' && c>=0 && c!= '\n' ){
			tokname[i] = c;
			if( ++i >= NAMESIZE ) --i;
			}
		if( c != '>' ) error( "unterminated < ... > clause" );
		tokname[i] = '\0';
		for( i=1; i<=ntypes; ++i ){
			if( !strcmp( typeset[i], tokname ) ){
				numbval = i;
				return( TYPENAME );
				}
			}
		typeset[numbval = ++ntypes] = cstash( tokname );
		return( TYPENAME );

	case '"':	
	case '\'':
		match = c;
		tokname[0] = ' ';
		i = 1;
		for(;;){
			c = getc(finput);
			if( c == '\n' || c == EOF )
				error("illegal or missing ' or \"" );
			if( c == '\\' ){
				c = getc(finput);
				tokname[i] = '\\';
				if( ++i >= NAMESIZE ) --i;
				}
			else if( c == match ) break;
			tokname[i] = c;
			if( ++i >= NAMESIZE ) --i;
			}
		break;

	case '%':
	case '\\':

		switch(c=getc(finput)) {

		case '0':	return(TERM);
		case '<':	return(LEFT);
		case '2':	return(BINARY);
		case '>':	return(RIGHT);
		case '%':
		case '\\':	return(MARK);
		case '=':	return(PREC);
		case '{':	return(LCURLY);
		default:	reserve = 1;
			}

	default:

		if( isdigit(c) ){ /* number */
			numbval = c-'0' ;
			base = (c=='0') ? 8 : 10 ;
			for( c=getc(finput); isdigit(c) ; c=getc(finput) ){
				numbval = numbval*base + c - '0';
				}
			ungetc( c, finput );
			return(NUMBER);
			}
		else if( islower(c) || isupper(c) || c=='_' || c=='.' || c=='$' ){
			i = 0;
			while( islower(c) || isupper(c) || isdigit(c) || c=='_' || c=='.' || c=='$' ){
				tokname[i] = c;
				if( reserve && isupper(c) ) tokname[i] += 'a'-'A';
				if( ++i >= NAMESIZE ) --i;
				c = getc(finput);
				}
			}
		else return(c);

		ungetc( c, finput );
		}

	tokname[i] = '\0';

	if( reserve ){ /* find a reserved word */
		if( !strcmp(tokname,"term")) return( TERM );
		if( !strcmp(tokname,"token")) return( TERM );
		if( !strcmp(tokname,"left")) return( LEFT );
		if( !strcmp(tokname,"nonassoc")) return( BINARY );
		if( !strcmp(tokname,"binary")) return( BINARY );
		if( !strcmp(tokname,"right")) return( RIGHT );
		if( !strcmp(tokname,"prec")) return( PREC );
		if( !strcmp(tokname,"start")) return( START );
		if( !strcmp(tokname,"type")) return( TYPEDEF );
		if( !strcmp(tokname,"union")) return( UNION );
		error("invalid escape, or illegal reserved word: %s", tokname );
		}

	/* look ahead to distinguish IDENTIFIER from C_IDENTIFIER */

	c = getc(finput);
	while( c==' ' || c=='\t'|| c=='\n' || c=='\f' || c== '/' ) {
		if( c == '\n' ) ++peekline;
		else if( c == '/' ){ /* look for comments */
			peekline += skipcom();
			}
		c = getc(finput);
		}
	if( c == ':' ) return( C_IDENTIFIER );
	ungetc( c, finput );
	return( IDENTIFIER );
}

fdtype( t ){ /* determine the type of a symbol */
	register v;
	if( t >= NTBASE ) v = nontrst[t-NTBASE].tvalue;
	else v = TYPE( toklev[t] );
	if( v <= 0 ) error( "must specify type for %s", (t>=NTBASE)?nontrst[t-NTBASE].name:
			tokset[t].name );
	return( v );
	}

chfind( t, s ) register char *s; {
	int i;

	if (s[0]==' ')t=0;
	TLOOP(i){
		if(!strcmp(s,tokset[i].name)){
			return( i );
			}
		}
	NTLOOP(i){
		if(!strcmp(s,nontrst[i].name)) {
			return( i+NTBASE );
			}
		}
	/* cannot find name */
	if( t>1 )
		error( "%s should have been defined earlier", s );
	return( defin( t, s ) );
	}

cpyunion(){
	/* copy the union declaration to the output, and the define file if present */

	int level, c;
	fprintf( ftable, "\n# line %d \"%s\"\n", lineno, infile );
	fprintf( ftable, "typedef union " );
	if( fdefine ) fprintf( fdefine, "\ntypedef union " );

	level = 0;
	for(;;){
		if( (c=getc(finput)) < 0 ) error( "EOF encountered while processing %%union" );
		putc( c, ftable );
		if( fdefine ) putc( c, fdefine );

		switch( c ){

		case '\n':
			++lineno;
			break;

		case '{':
			++level;
			break;

		case '}':
			--level;
			if( level == 0 ) { /* we are finished copying */
				fprintf( ftable, " YYSTYPE;\n" );
				if( fdefine ) fprintf( fdefine, " YYSTYPE;\nextern YYSTYPE yylval;\n" );
				return;
				}
			}
		}
	}

cpycode(){ /* copies code between \{ and \} */

	int c;
	c = getc(finput);
	if( c == '\n' ) {
		c = getc(finput);
		lineno++;
		}
	fprintf( ftable, "\n# line %d \"%s\"\n", lineno, infile );
	while( c>=0 ){
		if( c=='\\' )
			if( (c=getc(finput)) == '}' ) return;
			else putc('\\', ftable );
		if( c=='%' )
			if( (c=getc(finput)) == '}' ) return;
			else putc('%', ftable );
		putc( c , ftable );
		if( c == '\n' ) ++lineno;
		c = getc(finput);
		}
	error("eof before %%}" );
	}

skipcom(){ /* skip over comments */
	register c, i=0;  /* i is the number of lines skipped */

	/* skipcom is called after reading a / */

	if( getc(finput) != '*' ) error( "illegal comment" );
	c = getc(finput);
	while( c != EOF ){
		while( c == '*' ){
			if( (c=getc(finput)) == '/' ) return( i );
			}
		if( c == '\n' ) ++i;
		c = getc(finput);
		}
	error( "EOF inside comment" );
	/* NOTREACHED */
	}

cpyact(offset){ /* copy C action to the next ; or closing } */
	int brac, c, match, j, s, tok;

	fprintf( faction, "\n# line %d \"%s\"\n", lineno, infile );

	brac = 0;

loop:
	c = getc(finput);
swt:
	switch( c ){

case ';':
		if( brac == 0 ){
			putc( c , faction );
			return;
			}
		goto lcopy;

case '{':
		brac++;
		goto lcopy;

case '$':
		s = 1;
		tok = -1;
		c = getc(finput);
		if( c == '<' ){ /* type description */
			ungetc( c, finput );
			if( gettok() != TYPENAME ) error( "bad syntax on $<ident> clause" );
			tok = numbval;
			c = getc(finput);
			}
		if( c == '$' ){
			fprintf( faction, "yyval");
			if( ntypes ){ /* put out the proper tag... */
				if( tok < 0 ) tok = fdtype( *prdptr[nprod] );
				fprintf( faction, ".%s", typeset[tok] );
				}
			goto loop;
			}
		if( c == '-' ){
			s = -s;
			c = getc(finput);
			}
		if( isdigit(c) ){
			j=0;
			while( isdigit(c) ){
				j= j*10+c-'0';
				c = getc(finput);
				}

			j = j*s - offset;
			if( j > 0 ){
				error( "Illegal use of $%d", j+offset );
				}

			fprintf( faction, "yypvt[-%d]", -j );
			if( ntypes ){ /* put out the proper tag */
				if( j+offset <= 0 && tok < 0 ) error( "must specify type of $%d", j+offset );
				if( tok < 0 ) tok = fdtype( prdptr[nprod][j+offset] );
				fprintf( faction, ".%s", typeset[tok] );
				}
			goto swt;
			}
		putc( '$' , faction );
		if( s<0 ) putc('-', faction );
		goto swt;

case '}':
		if( --brac ) goto lcopy;
		putc( c, faction );
		return;


case '/':	/* look for comments */
		putc( c , faction );
		c = getc(finput);
		if( c != '*' ) goto swt;

		/* it really is a comment */

		putc( c , faction );
		c = getc(finput);
		while( c != EOF ){
			while( c=='*' ){
				putc( c , faction );
				if( (c=getc(finput)) == '/' ) goto lcopy;
				}
			putc( c , faction );
			if( c == '\n' )++lineno;
			c = getc(finput);
			}
		error( "EOF inside comment" );

case '\'':	/* character constant */
		match = '\'';
		goto string;

case '"':	/* character string */
		match = '"';

	string:

		putc( c , faction );
		while( c=getc(finput) ){

			if( c=='\\' ){
				putc( c , faction );
				c=getc(finput);
				if( c == '\n' ) ++lineno;
				}
			else if( c==match ) goto lcopy;
			else if( c=='\n' ) error( "newline in string or char. const." );
			putc( c , faction );
			}
		error( "EOF in string or character constant" );

case EOF:
		error("action does not terminate" );

case '\n':	++lineno;
		goto lcopy;

		}

lcopy:
	putc( c , faction );
	goto loop;
	}
