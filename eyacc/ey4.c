# include "ey.h"

output(){ /* print the output for the states */

  int i, j, k, c;

  settab();
  arrset("yyact");

  for( i=0; i<nstate; ++i ){ /* output the stuff for state i */
    nolook = (tystate[i]==0);
    closure(i);
    /* output actions */
    aryfil( temp1, nterms+1, 0 );
    for( j=0; j<cwset; ++j ){ /* look at the items */
      c = *( wsets[j].pitem );
      if( c>0 && c<NTBASE && temp1[c]==0 ) temp1[c] = go2(i,c);
      }

    if( i == 1 ) temp1[1] = ACCEPTCODE;

    /* now, we have the shifts; look at the reductions */

    lastred = 0;
    for( j=0; j<cwset; ++j ){
      c = *( wsets[j].pitem );
      if( c<=0 ){ /* reduction */
        lastred = -c;
        for( k=1; k<=nterms; ++k ){
          if( ((wsets[j].ws[k>>4])&(1<<(k&017))) != 0 ) {
            if( temp1[k] == 0 ) temp1[k] = c;
            else if( temp1[k]<0 ){ /* reduce/reduce conflict */
              settty();
              printf("\n%d: reduce/reduce conflict (red'ns %d and %d ) on %s",
                i, -temp1[k], lastred, symnam(k) );
              if( -temp1[k] > lastred ) temp1[k] = -lastred;
              ++zzrrconf;
              settab();
              }
            else { /* potential shift/reduce conflict */
              switch( precftn( lastred, k ) ) {

            case 0: /* precedence does not apply */

                settty();
                printf("\n%d: shift/reduce conflict (shift %d, red'n %d) on %s", i,
			temp1[k], lastred, symnam(k) );
                ++zzsrconf;
                settab();
                break;

            case 1: /*  reduce */

                temp1[k] = -lastred;
                break;

            case 2: /* error, binary operator */

                temp1[k] = ERRCODE;
                break;

            case 3: /* shift ... leave the entry alone */

                break;
                }
              }
            }
          }
        }
      }
    wract(i);
    }

  settab();
  arrdone();

  /* now, output the pointers to the action array */
  /* also output the info about reductions */
  prred();
  }

prred(){ /* print the information about the actions and the reductions */
  int index, i;

  arrset("yypact");
  index = 1;    /* position in the output table */

  for( i=0; i<nstate; ++i ){
    if( tystate[i]>0 ){  /* the state is real */
      temp1[i] = index;
      arrval( index );
      index =+ tystate[i];
      }
    else {
      arrval( temp1[-tystate[i]] );
      }
    }

  arrdone();

  arrset("yyr1");
  for( i=1; i<nprod; ++i ) arrval( *prdptr[i] - NTBASE );
  arrdone();

  arrset("yyr2");
  for( i=1; i<nprod; ++i ) arrval( ( prdptr[i+1]-prdptr[i]-2 ) );
  arrdone();

  }

go2(i,c){ /* do a goto on the closure state, not worrying about lookaheads */
  if( c<NTBASE ) return( amem[ apstate[i]+c ] );
  else return( amem[ apstate[i] + c - NTBASE + nterms ] );
  }

int pkdebug 0;
apack(p, n ) int *p;{ /* pack state i from temp1 into amem */
  _REGISTER k, l, off;
  int j;

  /* find the spot */

  j = n;
  for( off = 0; off <= j && p[off] == 0; ++off ) ;
  if( off > j ){ /* no actions */
    return(0);
    }
  j =- off;
  for( k=0; k<actsiz; ++k ){
    for( l=0; l<=j; ++l ){
      if( p[off+l] != 0 ){
        if( p[off+l] != amem[k+l] && amem[k+l] != 0 ) goto nextk;
        }
      }
    if( pkdebug ){ settty(); printf("off = %d, k = %d\n", off, k ); }
    /* we have found an acceptable k */
    for( l=0; l<=j; ++l ){
      if( p[off+l] ){
        if( k+l >= actsiz ) error("action table overflow");
        if( k+l >= memact ) memact = k+l;
        amem[k+l] = p[off+l];
        }
      }
    if( pkdebug ){
      for( k=0; k<memact; k=+10){
        printf("\t");
        for( l=0; l<=9; ++l ) printf("%d ", amem[k+l] );
        printf("\n");
        }
      }
    return(k-off);

    nextk: ;
    }
  error("no space in action table");
  }

go2out(){ /* output the gotos for the nontermninals */
  int i, j, k, best, offset, count, cbest, times;

  settab();
  arrset("yygo");
  offset = 1;

  for( i=1; i<=nnonter; ++i ) {
    go2gen(i);

    /* find the best one to make default */

    temp2[i] = offset;

    best = -1;
    times = 0;

    for( j=0; j<=nstate; ++j ){ /* is j the most frequent */
      if( tystate[j] == 0 ) continue;
      if( tystate[j] == best ) continue;

      /* is tystate[j] the most frequent */

      count = 0;
      cbest = tystate[j];

      for( k=j; k<=nstate; ++k ) if( tystate[k]==cbest ) ++count;

      if( count > times ){
        best = cbest;
        times = count;
        }
      }

    /* best is now the default entry */

    zzgobest =+ (times-1)*2;
    settab();
    for( j=0; j<=nstate; ++j ){
      if( tystate[j] != 0 && tystate[j]!=best ){
        arrval( j );
        arrval( tystate[j] );
        offset =+ 2;
        zzgoent =+ 2;
        }
      }

    /* now, the default */

    zzgoent =+ 2;
    arrval( -1 );
    arrval( best );
    offset =+ 2;

    }

  arrdone();

  arrset("yypgo");
  for( i=1; i<=nnonter; ++i ) arrval( temp2[i] );
  arrdone();

  }

int g2debug 0;
go2gen(c){ /* output the gotos for nonterminal c */

  int i, work, cc;
  struct item *p, *q;

  /* first, find nonterminals with gotos on c */

  aryfil( temp1, nnonter+1, 0 );
  temp1[c] = 1;

  work = 1;
  while( work ){
    work = 0;
    for( i=0; i<nprod; ++i ){
      if( (cc=prdptr[i][1]-NTBASE) >= 0 ){ /* cc is a nonterminal */
        if( temp1[cc] != 0 ){ /* cc has a goto on c */
          cc = *prdptr[i]-NTBASE; /* thus, the left side of production i does too */
          if( temp1[cc] == 0 ){
            work = 1;
            temp1[cc] = 1;
            }
          }
        }
      }
    }

  /* now, we have temp1[c] = 1 if a goto on c in closure of cc */

  if( g2debug ){
    settty();
    printf("%s: gotos on ", nontrst[c].name );
    for( i=0; i<=nnonter; ++i ) if( temp1[i]) printf("%s ", nontrst[i].name);
    printf("\n");
    }

  /* now, go through and put gotos into tystate */

  aryfil( tystate, nstate, 0 );
  settty();
  printf("\nnonterminal %s\n", nontrst[c].name );
  for( i=0; i<nstate; ++i ){
    q = pstate[i+1];
    for( p=pstate[i]; p<q; ++p ){
      if( (cc= *p->pitem) >= NTBASE ){
        if( temp1[cc =- NTBASE] ){ /* goto on c is possible */
          tystate[i] = amem[indgo[i]+c];
          break;
          }
        }
      }
    if( tystate[i] ) printf("\t%d\t%d\n", i, tystate[i]);
    }
  }

precftn(r,t){ /* decide a shift/reduce conflict by precedence.
			Returns 0 if action is 'do nothing',1 if action is reduce,
			2 if the action is 'error,binary operator'
			and 3 if the action is 'reduce'. */

	int lp,lt;
	lp = levprd[r];
	lt = trmlev[t];
	if ((lt==0)||((lp&03)==0))return(0);
	if((lt>>3) == (lp>>3)){
		return(lt&03);
		}
	if((lt>>3) > (lp>>3)) return(3);
	return(1);
	}

int cdebug 0; /* debug for common states */
wract(i){ /* output state i */
  /* temp1 has the actions, lastred the default */
  int p, p0, p1, size;
  int ntimes, tred, count, j;
  struct item *q0, *q1;

  /* find the best choice for lastred */

  lastred = 0;
  ntimes = 0;
  /***** UCB MOD - full state spec if shift on error *****/
  if (temp1[2] <= 0)
  for( j=1; j<=nterms; ++j ){
    if( temp1[j] >= 0 ) continue;
    if( temp1[j]+lastred == 0 ) continue;
    /* count the number of appearances of temp1[j] */
    count = 0;
    tred = -temp1[j];
    for( p=1; p<=nterms; ++p ){
      if( temp1[p]+tred == 0 ) ++count;
      }
    if( count >ntimes ){
      lastred = tred;
      ntimes = count;
      }
    }

    /* clear out entries in temp1 which equal lastred */
    for( p=1; p<= nterms; ++p ) if( temp1[p]+lastred == 0 )temp1[p]=0;

    /* write out the state */

    /* first, check for equality with another state */
    /* see if there is a nonterminal with all dots before it. */

    p0 = 0;
    q1 = pstate[i+1];
    for( q0=pstate[i]; q0<q1; ++q0 ){
      if( (p1= *(q0->pitem) ) < NTBASE ) goto standard;
      if( p0 == 0 ) p0 = p1;
      else if( p0 != p1 ) goto standard;
      }

    /* now, all items have dots before p0 */
    if( cdebug ){
      settty();
      printf("state %d, pre-nonterminal %s\n",i,nontrst[p0-NTBASE].name);
      }

    for( j=0; j<i; ++j ){
      if( apstate[i] != apstate[j] ) continue;

      /* equal positions -- check items */

      if( cdebug )printf("states %d and %d have equal positions\n",i,j);
      q1 = pstate[j+1];
      for( q0=pstate[j]; q0<q1; ++q0 ){
        if( *(q0->pitem) != p0 ) goto nextj;
        }

      /* we have a match with state j ! */

      tystate[i] = -j;
      zzacsave =+ tystate[j];
      zznsave++;
      wrstate(i);
      return;

    nextj:  ;
      }


  standard:
    tystate[i] = 2;
    wrstate(i);

    size = 0;
    for( p0=1; p0<=nterms; ++p0 )
      if( (p1=temp1[p0])!=0 ) {
	/***** UCB MOD - test actions are negative of symbol to be tested
			 this speeds up the parser as it is easy to check for
	 *****/
        arrval( -trmset[p0].value );
        if( p1 < 0 ) arrval( REDUCACT - p1 );
        else if( p1 == ACCEPTCODE ) arrval( ACCEPTACT );
        else if( p1 == ERRCODE ) arrval( ERRACT );
        else arrval( SHIFTACT + p1 );
        size =+ 2;
        }
    if( lastred ) arrval( REDUCACT + lastred );
    else arrval( ERRACT );
    tystate[i] = size+1; /* store entry size in tystate */
    zzacent =+ (size+1);
    return;
  }

wrstate(i){ /* writes state i */
	int j0,j1,s;
        struct item *pp, *qq;
	settty();
	printf("\nstate %d\n",i);
	qq = pstate[i+1];
	for( pp=pstate[i]; pp<qq; ++pp) printf("\t%s\n", writem(pp));

        /* check for state equal to another */

        if( tystate[i] <= 0 ){
          printf("\n\tsame as %d\n\n", -tystate[i] );
          return;
          }

	for( j0=1; j0<=nterms; ++j0 ) if( (j1=temp1[j0]) != 0 ){
	printf("\n\t%s  ", symnam(j0) );
             if( j1>0 ){ /* shift, error, or accept */
               if( j1 == ACCEPTCODE ) printf( "accept" );
               else if( j1 == ERRCODE ) printf( "error" );
               else printf( "shift %d", j1 );
               }
		   else printf("reduce %d",-j1 );
	   }

	/* output the final production */

	if( lastred ) printf("\n\t.  reduce %d\n\n", lastred );
	else printf("\n\t.  error\n\n" );

ret:
	settab();
	}
