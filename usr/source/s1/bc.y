%right '='
%left '+' '-'
%left '*' '/' '%'
%right '^'
%left UMINUS

%term LETTER DIGIT SQRT _IF  FFF EQ
%term _WHILE _FOR NE LE GE INCR DECR
%term _RETURN _BREAK _DEFINE BASE OBASE SCALE
%term EQPL EQMI EQMUL EQDIV EQREM EQEXP
%term _AUTO DOT
%term QSTR

%{
char cary[1000], *cp { cary };
char string[1000], *str {string};
int crs '0';
int rcrs '0';  /* reset crs */
int bindx 0;
int lev 0;
int bstack[10] { 0 };
char *numb[15] {
  " 0", " 1", " 2", " 3", " 4", " 5",
  " 6", " 7", " 8", " 9", " 10", " 11",
  " 12", " 13", " 14" };
int *pre, *post;
%}
%%
start	: 
	|  start stat tail
		= output( $2 );
	|  start def dargs ')' '{' dlist slist '}'
		={	bundle( pre, $7, post );
			conout( $$, $2 );
			rcrs = crs;
			output( "" );
			lev = bindx = 0;
			}
	;

dlist	:  tail
	| dlist _AUTO dlets tail
	;

stat	:  e 
		={ bundle( $1, "ps." ); }
	| 
		={ bundle( "" ); }
	|  QSTR
		={ bundle("[",$1,"]P");}
	|  LETTER '=' e
		={ bundle( $3, "s", $1 ); }
	|  LETTER '[' e ']' '=' e
		={ bundle( $6, $3, ":", geta($1)); }
	|  LETTER EQOP e
		={ bundle( "l", $1, $3, $2, "s", $1 ); }
	|  LETTER '[' e ']' EQOP e
		={ bundle($3, ";", geta($1), $6, $5, $3, ":", geta($1));}
	|  _BREAK
		={ bundle( numb[lev-bstack[bindx-1]], "Q" ); }
	|  _RETURN '(' e ')'
		= bundle( $3, post, numb[lev], "Q" );
	|  _RETURN '(' ')'
		= bundle( "0", post, numb[lev], "Q" );
	| SCALE e
		= bundle( $2, "k" );
	| SCALE '=' e
		= bundle( $3, "k");
	| SCALE EQOP e
		= bundle("K",$3,$2,"k");
	| BASE e
		= bundle( $2, "i" );
	| BASE '=' e
		= bundle($3, "i");
	| BASE EQOP e
		= bundle("I",$3,$2,"i");
	| OBASE e
		= bundle( $2, "o" );
	| OBASE '=' e
		= bundle($3,"o");
	| OBASE EQOP e
		= bundle("O",$3,$2,"o");
	|  '{' slist '}'
		={ $$ = $2; }
	|  FFF
		={ bundle("f"); }
	|  error
		={ bundle("c"); }
	|  _IF CRS BLEV '(' re ')' stat
		={	conout( $7, $2 );
			bundle( $5, $2, " " );
			}
	|  _WHILE CRS '(' re ')' stat BLEV
		={	bundle( $6, $4, $2 );
			conout( $$, $2 );
			bundle( $4, $2, " " );
			}
	|  fprefix CRS re ';' e ')' stat BLEV
		={	bundle( $7, $5, "s.", $3, $2 );
			conout( $$, $2 );
			bundle( $1, "s.", $3, $2, " " );
			}
	|  '~' LETTER '=' e
		={	bundle($4,"S",$2); }
	;

EQOP	:  EQPL
		={ $$ = "+"; }
	|  EQMI
		={ $$ = "-"; }
	|  EQMUL
		={ $$ = "*"; }
	|  EQDIV
		={ $$ = "/"; }
	|  EQREM
		={ $$ = "%%"; }
	|  EQEXP
		={ $$ = "^"; }
	;

fprefix	:  _FOR '(' e ';'
		={ $$ = $3; }
	;

BLEV	:
		={ --bindx; }
	;

slist	:  stat
	|  slist tail stat
		={ bundle( $1, $3 ); }
	;

tail	:  '\n'
	|  ';'
	;

re	:  e EQ e
		= bundle( $1, $3, "=" );
	|  e '<' e
		= bundle( $1, $3, ">" );
	|  e '>' e
		= bundle( $1, $3, "<" );
	|  e NE e
		= bundle( $1, $3, "!=" );
	|  e GE e
		= bundle( $1, $3, "!>" );
	|  e LE e
		= bundle( $1, $3, "!<" );
	|  e
		= bundle( $1, " 0!=" );
	;

e	:  e '+' e
		= bundle( $1, $3, "+" );
	|  e '-' e
		= bundle( $1, $3, "-" );
	| '-' e		%prec UMINUS
		= bundle( " 0", $2, "-" );
	|  e '*' e
		= bundle( $1, $3, "*" );
	|  e '/' e
		= bundle( $1, $3, "/" );
	|  e '%' e
		= bundle( $1, $3, "%%" );
	|  e '^' e
		= bundle( $1, $3, "^" );
	|  LETTER '[' e ']'
		={ bundle($3, ";", geta($1)); }
	|  LETTER INCR
		= bundle( "l", $1, "d1+s", $1 );
	|  INCR LETTER
		= bundle( "l", $2, "1+ds", $2 );
	|  DECR LETTER
		= bundle( "l", $2, "1-ds", $2 );
	|  LETTER DECR
		= bundle( "l", $1, "d1-s", $1 );
	| LETTER '[' e ']' INCR
		= bundle($3,";",geta($1),"d1+",$3,":",geta($1));
	| INCR LETTER '[' e ']'
		= bundle($4,";",geta($2),"1+d",$4,":",geta($2));
	| LETTER '[' e ']' DECR
		= bundle($3,";",geta($1),"d1-",$3,":",geta($1));
	| DECR LETTER '[' e ']'
		= bundle($4,";",geta($2),"1-d",$4,":",geta($2));
	| SCALE INCR
		= bundle("Kd1+k");
	| INCR SCALE
		= bundle("K1+dk");
	| SCALE DECR
		= bundle("Kd1-k");
	| DECR SCALE
		= bundle("K1-dk");
	| BASE INCR
		= bundle("Id1+i");
	| INCR BASE
		= bundle("I1+di");
	| BASE DECR
		= bundle("Id1-i");
	| DECR BASE
		= bundle("I1-di");
	| OBASE INCR
		= bundle("Od1+o");
	| INCR OBASE
		= bundle("O1+do");
	| OBASE DECR
		= bundle("Od1-o");
	| DECR OBASE
		= bundle("O1-do");
	|  LETTER '(' cargs ')'
		= bundle( $3, "l", getf($1), "x" );
	|  LETTER '(' ')'
		= bundle( "l", getf($1), "x" );
	|  cons
		={ bundle( " ", $1 ); }
	|  DOT cons
		={ bundle( " .", $2 ); }
	|  cons DOT cons
		={ bundle( " ", $1, ".", $3 ); }
	|  cons DOT
		={ bundle( " ", $1, "." ); }
	|  DOT
		={ $$ = "l."; }
	|  LETTER
		= { bundle( "l", $1 ); }
	|  LETTER '=' e
		={ bundle( $3, "ds", $1 ); }
	|  LETTER EQOP e	%prec '='
		={ bundle( "l", $1, $3, $2, "ds", $1 ); }
	|  '(' e ')'
		= { $$ = $2; }
	|  '?'
		={ bundle( "?" ); }
	|  SQRT '(' e ')'
		={ bundle( $3, "v" ); }
		| '~' LETTER
		={ bundle("L",$2); }
	| SCALE e
		= bundle($2,"dk");
	| SCALE '=' e
		= bundle($3,"dk");
	| SCALE EQOP e		%prec '='
		= bundle("K",$3,$2,"dk");
	| BASE e
		= bundle($2,"di");
	| BASE '=' e
		= bundle($3,"di");
	| BASE EQOP e		%prec '='
		= bundle("I",$3,$2,"di");
	| OBASE e
		= bundle($2,"do");
	| OBASE '=' e
		= bundle($3,"do");
	| OBASE EQOP e		%prec '='
		= bundle("O",$3,$2,"do");
	| SCALE
		= bundle("K");
	| BASE
		= bundle("I");
	| OBASE
		= bundle("O");
	;

cargs	:  eora
	|  cargs ',' eora
		= bundle( $1, $3 );
	;
eora:	  e
	| LETTER '[' ']'
		=bundle("l",geta($1));
	;

cons	:  constant
		={ *cp++ = '\0'; }

constant:
	  '_'
		={ $$ = cp; *cp++ = '_'; }
	|  DIGIT
		={ $$ = cp; *cp++ = $1; }
	|  constant DIGIT
		={ *cp++ = $2; }
	;

CRS	:
		={ $$ = cp; *cp++ = crs++; *cp++ = '\0'; bstack[bindx++] = lev++; }
	;

def	:  _DEFINE LETTER '('
		={	$$ = getf($2);
			pre = "";
			post = "";
			lev = 1;
			bstack[bindx=0] = 0;
			}
	;

dargs	:
	|  lora
		={ pp( $1 ); }
	|  dargs ',' lora
		={ pp( $3 ); }
	;

dlets	:  lora
		={ tp($1); }
	|  dlets ',' lora
		={ tp($3); }
	;
lora	:  LETTER
	|  LETTER '[' ']'
		={ $$ = geta($1); }
	;

%%
# define error 256

int peekc -1;
int sargc;
int ifile;
char **sargv;
extern int fin;

char *funtab[26]{
	01,02,03,04,05,06,07,010,011,012,013,014,015,016,017,
	020,021,022,023,024,025,026,027,030,031,032 };
char *atab[26]{
	0241,0242,0243,0244,0245,0246,0247,0250,0251,0252,0253,
	0254,0255,0256,0257,0260,0261,0262,0263,0264,0265,0266,
	0267,0270,0271,0272};
char *letr[26] {
  "a","b","c","d","e","f","g","h","i","j",
  "k","l","m","n","o","p","q","r","s","t",
  "u","v","w","x","y","z" } ;
char *dot { "." };
yylex(){
  int c,ch;
restart:
  c = getc();
  peekc = -1;
  while( c == ' ' || c == '\t' ) c = getc();
  if( c<= 'z' && c >= 'a' ) {
    /* look ahead to look for reserved words */
    peekc = getc();
    if( peekc >= 'a' && peekc <= 'z' ){ /* must be reserved word */
      if( c=='i' && peekc=='f' ){ c=_IF; goto skip; }
      if( c=='w' && peekc=='h' ){ c=_WHILE; goto skip; }
      if( c=='f' && peekc=='o' ){ c=_FOR; goto skip; }
      if( c=='s' && peekc=='q' ){ c=SQRT; goto skip; }
      if( c=='r' && peekc=='e' ){ c=_RETURN; goto skip; }
      if( c=='b' && peekc=='r' ){ c=_BREAK; goto skip; }
      if( c=='d' && peekc=='e' ){ c=_DEFINE; goto skip; }
      if( c=='s' && peekc=='c' ){ c= SCALE; goto skip; }
      if( c=='b' && peekc=='a' ){ c=BASE; goto skip; }
      if( c=='o' && peekc=='b' ){ c=OBASE; goto skip; }
      if( c=='d' && peekc=='i' ){ c=FFF; goto skip; }
      if( c=='a' && peekc=='u' ){ c=_AUTO; goto skip; }
      if( c == 'q' && peekc == 'u')getout();
      /* could not be found */
      return( error );
    skip:  /* skip over rest of word */
	peekc = -1;
      while( (ch = getc()) >= 'a' && ch <= 'z' );
	peekc = ch;
      return( c );
      }

    /* usual case; just one single letter */

    yylval = letr[c-'a'];
    return( LETTER );
    }
  if( c>= '0' && c <= '9' || c>= 'A' && c<= 'F' ){
    yylval = c;
    return( DIGIT );
    }
  switch( c ){
    case '.':  return( DOT );
    case '=':
      switch( peekc = getc() ){
        case '=': c=EQ; goto gotit;
        case '+': c=EQPL; goto gotit;
        case '-': c=EQMI; goto gotit;
        case '*': c=EQMUL; goto gotit;
        case '/': c=EQDIV; goto gotit;
        case '%': c=EQREM; goto gotit;
        case '^': c=EQEXP; goto gotit;
        default:   return( '=' );
        gotit:     peekc = -1; return(c);
        }
    case '+':  return( cpeek( '+', INCR, '+' ) );
    case '-':  return( cpeek( '-', DECR, '-' ) );
    case '<':  return( cpeek( '=', LE, '<' ) );
    case '>':  return( cpeek( '=', GE, '>' ) );
    case '!':  return( cpeek( '=', NE, '!' ) );
    case '/':
	if((peekc = getc()) == '*'){
		peekc = -1;
		while((getc() != '*') || ((peekc = getc()) != '/'));
		peekc = -1;
		goto restart;
	}
	else return(c);
    case '"':  
	       yylval = str;
	       while((c=getc()) != '"')*str++ = c;
	       *str++ = '\0';
	       return(QSTR);
    default:   return( c );
    }
  }

cpeek( c, yes, no ){
  if( (peekc=getc()) != c ) return( no );
  else {
    peekc = -1;
    return( yes );
    }
  }

getc(){
  int ch;
loop:
  ch = (peekc < 0) ? getchar() : peekc;
  peekc = -1;
  if(ch != '\0')return(ch);
  if(++ifile > sargc){
	if(ifile >= sargc+2)getout();
	fin = dup(0);
	goto loop;
  }
close(fin);
  if((fin = open(sargv[ifile],0)) >= 0)goto loop;
  yyerror("cannot open input file");
}
# define b_sp_max 1500
int b_space [ b_sp_max ];
int * b_sp_nxt { b_space };

bdebug 0;
bundle(a){
  int i, *p, *q;

  i = nargs();
  q = b_sp_nxt;

  if( bdebug ) printf("bundle %d elements at %o\n", i, q );

  for( p = &a; i-->0; ++p ){

    if( b_sp_nxt >= & b_space[b_sp_max] ) yyerror( "bundling space exceeded" );

    * b_sp_nxt++ = *p;
    }
  * b_sp_nxt++ = 0;
  yyval = q;
  return( q );
  }

routput(p) int *p; {
  if( bdebug ) printf("routput(%o)\n", p );
  if( p >= &b_space[0] && p < &b_space[b_sp_max]){
    /* part of a bundle */
    while( *p != 0 ) routput( *p++ );
    }
  else printf( p );   /* character string */
  }

output( p ) int *p; {
  routput( p );
  b_sp_nxt = & b_space[0];
  printf( "\n" );
  cp = cary;
  str = string;
  crs = rcrs;
  }

conout( p, s ) int *p; char *s; {
  printf("[");
  routput( p );
  printf("]s%s\n", s );
  lev--;
  str = string;
  }

yyerror( s ) char *s; {
  printf("c[%s]pc\n", s );
  cp = cary;
  crs = rcrs;
  bindx = 0;
  lev = 0;
  b_sp_nxt = &b_space[0];
 str = string;
  }

pp( s ) char *s; {
  /* puts the relevant stuff on pre and post for the letter s */

  bundle( "S", s, pre );
  pre = yyval;
  bundle( post, "L", s, "s." );
  post = yyval;
  }

tp( s ) char *s; { /* same as pp, but for temps */
  bundle( "0S", s, pre );
  pre = yyval;
  bundle( post, "L", s, "s." );
  post = yyval;
  }

yyinit(argc,argv) int argc; char *argv[];{
  int (*getout)();
  signal( 2, getout );  /* ignore all interrupts */
  sargv=argv;
  sargc= -- argc;
  if(sargc == 0)fin=dup(0);
   else if((fin = open(sargv[1],0)) < 0)
	yyerror("cannot open input file");
  ifile = 1;
  }
getout(){
printf("q");
exit();
}

getf(p) char *p;{
return(&funtab[*p -0141]);
}
geta(p) char *p;{
	return(&atab[*p - 0141]);
}

main(argc, argv)
char **argv;
{
	int p[2];


	if (argc > 1 && *argv[1] == '-') {
		if(argv[1][1] == 'd'){
			yyinit(--argc, ++argv);
			yyparse();
			exit();
		}
		if(argv[1][1] != 'l'){
			printf("unrecognizable argument\n");
			exit();
		}
		argv[1] = "/usr/lib/lib.b";
	}
	pipe(p);
	if (fork()==0) {
		close(1);
		dup(p[1]);
		close(p[0]);
		close(p[1]);
		yyinit(argc, argv);
		yyparse();
		exit();
	}
	close(0);
	dup(p[0]);
	close(p[0]);
	close(p[1]);
	execl("/bin/dc", "dc", "-", 0);
}
