# include "ey.h"
  /*     * * * *    e y a c c     * * * *     */

  /**** NB -----
   *
   * This version of yacc, known as "eyacc" has been slightly but
   * importantly modified to allow error recovery in the UNIX Pascal
   * translator "pi" and also in "pix".
   *
   * Changes here include:
   *
   * 1) Enumeration of test actions when "error" is an input token.
   *
   * 2) Change to the encoding of the action entries.  Test entries
   *    are encoded as the arithmetic inverse of the symbol being tested
   *	for.  This is an optimization that makes the parser run at the
   *	same speed even though, with error productions and enumerated
   *	lookaheads, it would normally be much slower.  Of course the
   *	same thing could be done to the regular yacc...
   *
   * 3) Different table sizes
   *
   * 4) Recognizes form feeds
   *
   * 5) Also most of the numbers for the sizes of the tables have been
   *	increased, to an extent to allow for "eyacc"ing of the Pascal grammar
   *	and of a grammar which I have for "EUCLID".
   *
   *	There seem to be subtle dependencies between the various magic
   *	numbers... I found some of them but to be safe most of the limits
   *	are very generous... for this reason "eyacc" will most likely
   *	have to run separate i/d... no matter.
   *
   *					Bill Joy
   *					Computer Science Division
   *					EECS Department
   *					University of California, Berkeley
   *					Berkeley, California  94704
   *	
   *					Office:	(415) 642-4948
   *					Home:	(415) 524-4510
   ****/

  /*      features to be fixed up ...

  ***  Print estimate of total space needed for parser
  ***  Either list inputs on y.output, or list empty prdn's in states
  ***  Mention nonterms not used (or, rules. not reduced) as nonfatal error
  ***  Output states where conflicts were found by default on y.output
  ***  Engage in newspeak: production=>grammar rules, term=>token, etc.
  ***  handle # define, #ifdef, etc., in yacc actions, %{ %}
  */

  /*      new features to be added

  ***  reductions by single productions ( by request )
  ***  follow sets for start symbol
  ***  option to only do slr(1)
  ***  easily changed array names on output
  ***  allocate core, rather than predefined
  ***  input controlled by a grammar
  ***  support multiple choices for  conflicts
  ***  better conflict diagnostics
  */



main(argc,argv) int argc; char *argv[]; {
  auto int n;

  whereami();
  setup(argc,argv); /* initialize and read productions */
  tbitset = (nterms+16)/16;
  cpres(); /* make table of which productions yield a given nonterminal */
  cempty(); /* make a table of which nonterminals can match the empty string */
  cpfir(); /* make a table of e free first lists */
  stagen(); /* generate the states */
  output();  /* write the states and the tables */
  go2out();
  summary();
  windup();
  }

whereami(){ /* sets the variable machine to UNIX, GCOS, or IBM */

  int i;

  i = 1;
  i = i << 30;
  if( i == 0 ) {
    machine = UNIX;
    return;
    }
  i = i << 4;
  if( i == 0 ){
    machine = IBM;
    return;
    }
  machine = GCOS;
  }

windup(){
  /* no errors, do the optimization if appropriate */
  char *cp;
  int i;

  cflush(1);
  if( !oflag ) cexit(0);

  for( i=3; i<10; ++i ) cclose(i);
  switch( machine ){

  case GCOS:
    if( rflag ){
      if( foutput<0 ) system( "./yopt -r" );
      else system( "./yopt -rv" );
      }
    else {
      if( foutput<0 ) system( "./yopt" );
      else system( "./yopt -v" );
      }
    cexit(0);  /* terminate */

  case UNIX:
    cp = "/usr/nlib/yaccopt";
    if( rflag ) execl( cp, cp, (foutput<0)?"-r":"-rv", 0 );
    else if( foutput<0 ) execl( cp, cp, 0 );
    else execl( cp, cp, "-v", 0 );
    error( "optimization execl call fails" );

  case IBM:
    if( rflag ){
      if( foutput<0 ) system( "MH2019.yaccopt -r" );
      else system( "MH2019.yaccopt -rv" );
      }
    else {
      if( foutput<0 ) system( "MH2019.yaccopt" );
      else system( "MH2019.yaccopt -v" );
      }
    cexit(0);

    }

  }

settty()
/*	sets the output file to y.output */
{	
	cflush( foutput );  /* a bit of a cheat */
	cout = foutput;
	}

settab(){ /* sets the output file to y.tab.c */
	
	cflush( ftable );
	cout = ftable;
	}

char *chcopy( p, q )  char *p, *q; {
	/* copies string q into p, returning next free char ptr */
	while( *p = *q++ ) ++p;
	return( p );
	}

char *writem(pp) struct item *pp; { /* creates output string for item pointed to by pp */
	int i,*p;
	static char sarr[100];
	char *q;

	for( p=pp->pitem; *p>0 ; ++p ) ;
	p = prdptr[-*p];
	q = chcopy( sarr, nontrst[*p-NTBASE].name );
	q = chcopy( q, " : " );

	for(;;){
		*q++ = ++p==(pp->pitem) ? '_' : ' ';
		if((i = *p) <= 0) break;
		q = chcopy( q, symnam(i) );
		}

	*q = '\0' ;
	return( sarr );
	}

char *symnam(i){ /* return a pointer to the name of symbol i */
	char *cp;

	cp = (i>=NTBASE) ? nontrst[i-NTBASE].name : trmset[i].name ;
	if( *cp == ' ' ) ++cp;
	return( cp );
	}

summary(){ /* output the summary on the tty */

  int i, s, *pn;
  

	if( !rflag ){
		settab();
		printf("\nint nterms %d;",nterms);
		printf("\nint nnonter %d;", nnonter);
		printf("\nint nstate %d;", nstate);
		printf("\nchar *yysterm[] {");
		for (i=1;i<=nterms;i++) if( trmset[i].value >= 0400 ) printf("\n\"%s\",",symnam(i));
		printf( "\n0 };\n" );
		printf("\nchar *yysnter[] {");
		for (i=0;i<nnonter;i++) printf("\n\"%s\",",nontrst[i].name);
		printf("\n\"%s\" };\n",nontrst[nnonter].name);
		}

  settty();
  printf("\n%d/%d terminals, %d/%d nonterminals\n", nterms, tlim,
      nnonter, ntlim );
  printf("%d/%d grammar rules, %d/%d states\n", nprod, prdlim, nstate, stsize );
  printf("%d shift/reduce, %d reduce/reduce conflicts reported\n", zzsrconf, zzrrconf );
  pn = pstate[nstate+1];
  printf("%d/%d working sets used\n", zzcwset,  wssize );
  printf("memory: states,etc. %d/%d, parser %d/%d\n", pn-mem0, memsiz,
      memact, actsiz );
  printf("%d/%d distinct lookahead sets\n", nlset, lsetsz );
  printf("%d extra closures\n", zzclose - 2*nstate );
  printf("%d action entries\n", zzacent );
  printf("%d action entries saved through merging %d states\n",zzacsave,zznsave);
  printf("%d goto entries\n", zzgoent );
  printf("%d entries saved by goto default\n", zzgobest );
  if( zzsrconf!=0 || zzrrconf!=0 ){
    cflush( errfileno );
    cout = errfileno;
    printf("\nconflicts: ");
    if( zzsrconf )printf( "%d shift/reduce" , zzsrconf );
    if( zzsrconf && zzrrconf )printf( ", " );
    if( zzrrconf )printf( "%d reduce/reduce" , zzrrconf );
    printf( "\n" );
    }
  }

error(s,a1){ /* write out error comment */
	
	int c;
	++nerrors;
	cflush( errfileno );
	cout = errfileno;   /* set output to tty */
	printf("\n fatal error: ");
	printf(s,a1);
        printf(", line %d\n", lineno );
	if( !fatfl ) return;
	summary();
	cexit(1);
	}

arrset(s) char s[]; {
	printf("\nint %s[] {0", s );
	arrndx = 1;
	}

arrval(n){
	printf(",%d",n);
	if( (++arrndx%10) == 0 ) printf("\n");
	}

arrdone(){
	printf(",-1};\n");
	}

copy(v) char *v; {	/* copy ctokn to v */
	char *p;

	p=ctokn;
	while( *v++ = *p++ );
	}

compare(v) char *v; {	/* compare ctokn with v */
	char *p;

	for( p=ctokn; ; ++p ){
		if( *p != *v++ ) return( 0 );
		if( *p == 0 ) return(1);
		}
	}

int *yalloc(n){ /* allocate n+1 words from vector mem */
	int *omem;
	omem = mem;
	mem =+ n+1;
	if(mem-mem0 >= memsiz) error("memory overflow");
	return(omem);
	}

aryfil( v, n, c ) int *v,n,c; { /* set elements 0 through n-1 to c */
  int i;
  for( i=0; i<n; ++i ) v[i] = c;
  }

union( a, b, c ) int *a, *b, *c; {
  /* set a to the union of b and c */
  /* a may equal b */
  /* return 1 if c is not a subset of b, 0 otherwise */

  _REGISTER int i, x, sub;

  sub = 0;
  for( i=0; i<tbitset; ++i ){
    x = b[i] | c[i];
    if( x != b[i] ) sub=1;
    a[i] = x;
    }
  return( sub );
  }

prlook( pp ) int *pp;{
	int j;
	pp = pp->lset;
	if( pp == 0 ) printf("\tNULL");
	else {
		printf(" { " );
		for( j=1; j<=nterms; ++j ){
			if( (pp[j>>4]>>(j&017) )&01 != 0 ) printf( "%s ", symnam(j) );
			}
		printf( "}" );
		}
	}
