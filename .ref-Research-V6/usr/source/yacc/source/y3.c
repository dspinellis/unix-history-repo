# include "dextern"

cpres(){ /* conpute an array with the beginnings of  productions yielding given nonterminals
	The array pres points to these lists */
	int i,j,c;
	pres = yalloc(nnonter+1);
	for(i=0;i<=nnonter;i++){
		c = i+NTBASE;
		pres[i] = mem;
		fatfl = 0;  /* make undefined  symbols  nonfatal */
		for(j=0;j<nprod;j++)
		if (*prdptr[j] == c) *mem++ =  prdptr[j]+1;
		if(pres[i] == mem){
			error("nonterminal %s not defined!", nontrst[i].name);
			}
		}
	pres[i] = mem;
	fatfl = 1;
	if( nerrors ){
		summary();
		cexit(1);
		}
	}

int indebug 0;
cpfir() {
  /* compute an array with the first of nonterminals */
  int i, ch, **s, **t, changes, *p;

  pfirst = yalloc(nnonter+1);
  for (i=0;i<=nnonter;i++) {
    aryfil( wsets[i].ws, tbitset, 0 );
    t = pres[i+1];
    for( s=pres[i]; s<t; ++s ){ /* initially fill the sets */
      for( p = *s; (ch = *p) > 0 ; ++p ) {
        if( ch < NTBASE ) {
          wsets[i].ws[ch>>4] =| (1 << (ch&017) );
          break;
          }
        else if( !pempty[ch-NTBASE] ) break;
        }
      }
    }

  /* now, reflect transitivity */

  changes = 1;
  while( changes ){
    changes = 0;
    for( i=0; i<=nnonter; ++i ){
      t = pres[i+1];
      for( s=pres[i]; s<t; ++s ){
        for( p = *s; ( ch = (*p-NTBASE) ) >= 0; ++p ) {
          changes =| union( wsets[i].ws, wsets[i].ws, wsets[ch].ws );
          if( !pempty[ch] ) break;
          }
        }
      }
    }

  for( i=0; i<=nnonter; i++ ) pfirst[i] = flset( wsets[i].ws );
  if( !indebug ) return;
  settty();
  for( i=0; i<=nnonter; i++ ){
    printf( "\n%s: ", nontrst[i].name );
    prlook( pfirst[i] );
    printf( " %d\n", pempty[i] );
    }
  }

state(c){ /* sorts last state,and sees if it equals earlier ones. returns state number */
	int s,size1,size2;
	_REGISTER i;
	struct item *p1, *p2, *k, *l, *q1, *q2;
	p1 = pstate[nstate];
	p2 = pstate[nstate+1];
	if(p1==p2) return(0); /* null state */
	/* sort the items */
	for(k=p2-1;k>p1;k--) {	/* make k the biggest */
		for(l=k-1;l>=p1;--l)if( l->pitem > k->pitem ){
			s = k->pitem;
			k->pitem = l->pitem;
			l->pitem = s;
			s = k->look;
			k->look = l->look;
			l->look = s;
			}
		}
	size1 = p2 - p1; /* size of state */

	for( i= (c>=NTBASE)?ntstates[c-NTBASE]:tstates[c]; i != 0; i = mstates[i] ) {
		/* get ith state */
		q1 = pstate[i];
		q2 = pstate[i+1];
		size2 = q2 - q1;
		if (size1 != size2) continue;
		k=p1;
		for(l=q1;l<q2;l++) {
			if( l->pitem != k->pitem ) break;
			++k;
			}
		if (l != q2) continue;
		/* found it */
		pstate[nstate+1] = pstate[nstate]; /* delete last state */
		/* fix up lookaheads */
		k=p1;
		for( l=q1; l<q2; ++l ){
			if( union( clset.lset, l->look->lset, k->look->lset ) ) {
				tystate[i] = 1;
				/* register the new set */
				l->look = flset( &clset );
				}
			++k;
			}
		return (i);
		}
/* state is new */
	pstate[nstate+2] = p2;
	if(nstate+1 >= stsize) error("too many states");
	if( c >= NTBASE ){
		mstates[ nstate ] = ntstates[ c-NTBASE ];
		ntstates[ c-NTBASE ] = nstate;
		}
	else {
		mstates[ nstate ] = tstates[ c ];
		tstates[ c ] = nstate;
		}
	tystate[nstate]=1;
	return(nstate++);
	}

int pidebug 0; /* debugging flag for putitem */
putitem ( ptr, lptr )		int *ptr; struct looksets *lptr;{
	int *jip, k;
	struct item *i, *j;

        if( pidebug ) {
          settty();
	  printf("putitem(%s), state %d\n", writem(&ptr), nstate );
          }
	/* see if it's there */
	j = pstate[nstate+1];
        for( i=pstate[nstate]; i<j; ++i )
		if(i->pitem == ptr) { 
			error("yacc error--duplicate item");
			}
	/* not there */
	j->pitem = ptr;
	j->look = flset( lptr );
	pstate[nstate+1] = ++j;
	jip = j;
	if(jip-mem0 >= memsiz) error("out of state space");
	}

cempty(){ /* mark nonterminals which derive the empty string */

  int i, *p;

  /* set pempty to 0 */
  pempty = yalloc( nnonter );
  aryfil( pempty, nnonter+1, 0 );
  for( i=1; i<nprod; ++i ) /* loop over productions */
    if( prdptr[i][1] <= 0 ) /* i is empty production */
      pempty[ *prdptr[i]-NTBASE ] = 1;

  /* now, all explicitly empty nonterminals marked with pempty = 1 */

  /* loop as long as we keep finding nontrivial empty nonterminals */

again:
  for( i=1; i<nprod; ++i ){
    if( pempty[ *prdptr[i]-NTBASE ]==0 ){ /* not known to be empty */
      for( p=prdptr[i]+1; *p>=NTBASE && pempty[*p-NTBASE]!=0 ; ++p ) ;
      if( *p < 0 ){ /* we have a nontrivially empty nonterminal */
        pempty[*prdptr[i]-NTBASE] = 1;
        goto again; /* got one ... try for another */
        }
      }
    }
  }

int gsdebug 0;
stagen(){ /* generate the states */

  int i, j, k, c;

  /* initialize */

  nstate = 0;
  pstate[0] = pstate[1] = mem;
  aryfil( clset.lset, tbitset, 0 );
  putitem( prdptr[0]+1, &clset );
  tystate[0] = 1;
  nstate = 1;
  pstate[2] = pstate[1];

  memact = 0;
  aryfil( amem, actsiz, 0 );

  /* now, the main state generation loop */

  more:
  for( i=0; i<nstate; ++i ){
    if( tystate[i] != 1 ) continue;
    tystate[i] = 0;
    aryfil( temp1, nterms+nnonter+1, 0 );
    /* take state i, close it, and do gotos */
    closure(i);
    for( j=0; j<cwset; ++j ){ /* generate gotos */
      if( wsets[j].flag ) continue;
      wsets[j].flag = 1;
      c = *(wsets[j].pitem);
      if( c <= 1 ) {
		if( pstate[i+1]-pstate[i] <= j ) tystate[i] = (tystate[i]==1)?1:2;
		continue;
		}
      /* do a goto on c */
      for( k=j; k<cwset; ++k ){
        if( c == *(wsets[k].pitem) ){ /* this item contributes to the goto */
          putitem( wsets[k].pitem + 1, wsets[k].ws );
          wsets[k].flag = 1;
          }
        }
      if( c < NTBASE ) temp1[c] = state(c);
      else temp1[c-NTBASE+nterms] = state(c);
      }
    if( gsdebug ){
      settty();
      printf( "%d: ", i );
      for( j=1; j<=nterms; ++j ){
        if( temp1[j] != 0 ) printf( "%s %d, ", symnam(j), temp1[j]);
        }
      for( j=1; j<=nnonter; ++j ){
        if( temp1[j+nterms] ) printf( "%s %d, ", nontrst[j].name, temp1[j+nterms] );
        }
      printf("\n");
      }
    apstate[i] = apack( &temp1[0], nterms );
    indgo[i] = apack( &temp1[nterms+1], nnonter-1 ) - 1;
    goto more; /* we have done one goto; do some more */
    }
  /* no more to do... stop */
  }

int cldebug 0; /* debugging flag for closure */
closure(i){ /* generate the closure of state i */

  int c, ch, work;
  _REGISTER j, k;
  int *pi;
  int **s, **t;
  struct item *q;
  _REGISTER struct item *p;

  ++zzclose;

  /* first, copy kernel of state i to wsets */

  cwset = 0;
  q = pstate[i+1];
  for( p=pstate[i]; p<q; ++p ){
    wsets[cwset].pitem = p->pitem;
    wsets[cwset].flag = 1;    /* this item must get closed */
    for( k=0; k<tbitset; ++k ) wsets[cwset].ws[k] = p->look->lset[k];
    ++cwset;
    }

  /* now, go through the loop, closing each item */

  work = 1;
  while( work ){
    work = 0;
    for( j=0; j<cwset; ++j ){
  
      if( wsets[j].flag == 0 ) continue;
      c = *(wsets[j].pitem);  /* dot is before c */
  
      if( c < NTBASE ){
        wsets[j].flag = 0;
        continue;  /* only interesting case is where . is before nonterminal */
        }
  
      /* compute the lookahead */
      aryfil( clset.lset, tbitset, 0 );

      /* find items involving c */

      for( k=j; k<cwset; ++k ){
        if( wsets[k].flag == 1 && *(pi=wsets[k].pitem) == c ){
          wsets[k].flag = 0;
          if( nolook ) continue;
          while( (ch= *++pi)>0 ){
            if( ch < NTBASE ){ /* terminal symbol */
              clset.lset[ch>>4] =| (1<<(ch&017));
              break;
              }
            /* nonterminal symbol */
            union( clset.lset, clset.lset, pfirst[ch-NTBASE] );
            if( !pempty[ch-NTBASE] ) break;
            }
          if( ch<=0 ) union( clset.lset, clset.lset, wsets[k].ws );
          }
        }
  
      /*  now loop over productions derived from c */
  
      c =- NTBASE; /* c is now nonterminal number */
  
      t = pres[c+1];
      for( s=pres[c]; s<t; ++s ){
        /* put these items into the closure */
        for( k=0; k<cwset; ++k ){ /* is the item there */
          if( wsets[k].pitem == *s ){ /* yes, it is there */
            if( nolook ) goto nexts;
            if( union( wsets[k].ws, wsets[k].ws, clset.lset ) ) wsets[k].flag = work = 1;
            goto nexts;
            }
          }
  
        /*  not there; make a new entry */
        if( cwset+1 >= wssize ) error( "working set overflow" );
        wsets[cwset].pitem = *s;
        wsets[cwset].flag = 1;
        if( nolook ){
          cwset++;
          goto nexts;
          }
        work = 1;
        for( k=0; k<tbitset; ++k ) wsets[cwset].ws[k] = clset.lset[k];
        cwset++;
  
      nexts: ;
        }
  
      }
    }

  /* have computed closure; flags are reset; return */

  if( cwset > zzcwset ) zzcwset = cwset;
  if( !cldebug ) return;
  settty();
  printf("\nState %d, nolook = %d\n", i, nolook );
  for( j=0; j<cwset; ++j ){
    if( wsets[j].flag ) printf("flag set!\n");
    wsets[j].flag = 0;
    printf("\t%s", writem(&wsets[j].pitem));
    prlook( wsets[j].ws );
    printf( "\n" );
    }

  }

struct looksets *flset( p ) 
	struct looksets *p;
	{
	/* decide if the lookahead set pointed to by p is known */
	/* return pointer to a perminent location for the set */

	int j, *w;
	_REGISTER *u, *v, i;

	for( i=0; i<nlset; ++i ){
		u = p->lset;
		v = lkst[i].lset;
		w = & v[tbitset];
		while( v<w) if( *u++ != *v++ ) goto more;
		/* we have matched */
		return( & lkst[i] );
		more: ;
		}
	/* add a new one */
	if( nlset+1 >= lsetsz )error("too many lookahead sets");
	for( j=0; j<tbitset; ++j ){
		lkst[nlset].lset[j] = p->lset[j];
		}
	return( & lkst[nlset++]);
	}
