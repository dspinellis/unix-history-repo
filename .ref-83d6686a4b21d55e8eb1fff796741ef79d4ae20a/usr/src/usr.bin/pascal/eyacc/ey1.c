/*
 * Copyright (c) 1979 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)ey1.c	5.2 (Berkeley) %G%";
#endif not lint

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

extern char *symnam();

main(argc,argv) int argc; char *argv[]; {
  auto int n;

  setup(argc,argv); /* initialize and read productions */
  tbitset = (nterms+16)/16;
  cpres(); /* make table of which productions yield a given nonterminal */
  cempty(); /* make a table of which nonterminals can match the empty string */
  cpfir(); /* make a table of e free first lists */
  stagen(); /* generate the states */
  output();  /* write the states and the tables */
  go2out();
  summary();
  exit(0);
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
		fprintf( cout , "\nint nterms %d;",nterms);
		fprintf( cout , "\nint nnonter %d;", nnonter);
		fprintf( cout , "\nint nstate %d;", nstate);
		fprintf( cout , "\nchar *yysterm[] {");
		for (i=1;i<=nterms;i++) if( trmset[i].value >= 0400 ) fprintf( cout , "\n\"%s\",",symnam(i));
		fprintf( cout ,  "\n0 };\n" );
		fprintf( cout , "\nchar *yysnter[] {");
		for (i=0;i<nnonter;i++) fprintf( cout , "\n\"%s\",",nontrst[i].name);
		fprintf( cout , "\n\"%s\" };\n",nontrst[nnonter].name);
		}

  settty();
  fprintf( cout , "\n%d/%d terminals, %d/%d nonterminals\n", nterms, tlim,
      nnonter, ntlim );
  fprintf( cout , "%d/%d grammar rules, %d/%d states\n", nprod, prdlim, nstate, stsize );
  fprintf( cout , "%d shift/reduce, %d reduce/reduce conflicts reported\n", zzsrconf, zzrrconf );
  pn = (int *)pstate[nstate+1];
  fprintf( cout , "%d/%d working sets used\n", zzcwset,  wssize );
  fprintf( cout , "memory: states,etc. %d/%d, parser %d/%d\n", pn-mem0, memsiz,
      memact, actsiz );
  fprintf( cout , "%d/%d distinct lookahead sets\n", nlset, lsetsz );
  fprintf( cout , "%d extra closures\n", zzclose - 2*nstate );
  fprintf( cout , "%d action entries\n", zzacent );
  fprintf( cout , "%d action entries saved through merging %d states\n",zzacsave,zznsave);
  fprintf( cout , "%d goto entries\n", zzgoent );
  fprintf( cout , "%d entries saved by goto default\n", zzgobest );
  fprintf( cout , "%d lookahead sets saved\n", savedlook);
  if( zzsrconf!=0 || zzrrconf!=0 ){
    cflush( errfileno );
    cout = errfileno;
    fprintf( cout , "\nconflicts: ");
    if( zzsrconf )fprintf( cout ,  "%d shift/reduce" , zzsrconf );
    if( zzsrconf && zzrrconf )fprintf( cout ,  ", " );
    if( zzrrconf )fprintf( cout ,  "%d reduce/reduce" , zzrrconf );
    fprintf( cout ,  "\n" );
    }
  }

error(s,a1){ /* write out error comment */
	
	int c;
	++nerrors;
	cflush( errfileno );
	cout = errfileno;   /* set output to tty */
	fprintf( cout , "\n fatal error: ");
	fprintf( cout , s,a1);
        fprintf( cout , ", line %d\n", lineno );
	if( !fatfl ) return;
	summary();
	cexit(1);
	}

arrset(s) char s[]; {
	fprintf( cout , "\nint %s[] {0", s );
	arrndx = 1;
	}

arrval(n){
	fprintf( cout , ",%d",n);
	if( (++arrndx%10) == 0 ) fprintf( cout , "\n");
	}

arrdone(){
	fprintf( cout , ",-1};\n");
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
	mem += n+1;
	if(mem-mem0 >= memsiz) error("memory overflow");
	return(omem);
	}

aryfil( v, n, c ) int *v,n,c; { /* set elements 0 through n-1 to c */
  int i;
  for( i=0; i<n; ++i ) v[i] = c;
  }

UNION( a, b, c ) int *a, *b, *c; {
  /* set a to the UNION of b and c */
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

prlook( p ) struct looksets *p;{
	int j, *pp;
	pp = p->lset;
	if( pp == 0 ) fprintf( cout , "\tNULL");
	else {
		fprintf( cout , " { " );
		for( j=1; j<=nterms; ++j ){
			if( (pp[j>>4]>>(j&017) )&01 != 0 ) fprintf( cout ,  "%s ", symnam(j) );
			}
		fprintf( cout ,  "}" );
		}
	}
