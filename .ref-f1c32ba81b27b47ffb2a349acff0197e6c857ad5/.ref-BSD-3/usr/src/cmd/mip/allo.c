# include "mfile2"

NODE resc[3];

int busy[REGSZ];

int maxa, mina, maxb, minb;

allo0(){ /* free everything */

	register i;

	maxa = maxb = -1;
	mina = minb = 0;

	REGLOOP(i){
		busy[i] = 0;
		if( rstatus[i] & STAREG ){
			if( maxa<0 ) mina = i;
			maxa = i;
			}
		if( rstatus[i] & STBREG ){
			if( maxb<0 ) minb = i;
			maxb = i;
			}
		}
	}

# define TBUSY 01000

allo( p, q ) NODE *p; struct optab *q; {

	register n, i, j;

	n = q->needs;
	i = 0;

	while( n & NACOUNT ){
		resc[i].op = REG;
		resc[i].rval = freereg( p, n&NAMASK );
		resc[i].lval = 0;
		resc[i].name[0] = '\0';
		n -= NAREG;
		++i;
		}

	while( n & NBCOUNT ){
		resc[i].op = REG;
		resc[i].rval = freereg( p, n&NBMASK );
		resc[i].lval = 0;
		resc[i].name[0] = '\0';
		n -= NBREG;
		++i;
		}

	if( n & NTMASK ){
		resc[i].op = OREG;
		resc[i].rval = TMPREG;
		if( p->op == STCALL || p->op == STARG || p->op == UNARY STCALL || p->op == STASG ){
			resc[i].lval = freetemp( (SZCHAR*p->stsize + (SZINT-1))/SZINT );
			}
		else {
			resc[i].lval = freetemp( (n&NTMASK)/NTEMP );
			}
		resc[i].name[0] = '\0';
		resc[i].lval = BITOOR(resc[i].lval);
		++i;
		}

	/* turn off "temporarily busy" bit */

	REGLOOP(j){
		busy[j] &= ~TBUSY;
		}

	for( j=0; j<i; ++j ) if( resc[j].rval < 0 ) return(0);
	return(1);

	}

freetemp( k ){ /* allocate k integers worth of temp space */
	/* we also make the convention that, if the number of words is more than 1,
	/* it must be aligned for storing doubles... */

# ifndef BACKTEMP
	int t;

	if( k>1 ){
		SETOFF( tmpoff, ALDOUBLE );
		}

	t = tmpoff;
	tmpoff += k*SZINT;
	if( tmpoff > maxoff ) maxoff = tmpoff;
	if( tmpoff-baseoff > maxtemp ) maxtemp = tmpoff-baseoff;
	return(t);

# else
	tmpoff += k*SZINT;
	if( k>1 ) {
		SETOFF( tmpoff, ALDOUBLE );
		}
	if( tmpoff > maxoff ) maxoff = tmpoff;
	if( tmpoff-baseoff > maxtemp ) maxtemp = tmpoff-baseoff;
	return( -tmpoff );
# endif
	}

freereg( p, n ) NODE *p; {
	/* allocate a register of type n */
	/* p gives the type, if floating */

	register j;

	/* not general; means that only one register (the result) OK for call */
	if( callop(p->op) ){
		j = callreg(p);
		if( usable( p, n, j ) ) return( j );
		/* have allocated callreg first */
		}
	j = p->rall & ~MUSTDO;
	if( j!=NOPREF && usable(p,n,j) ){ /* needed and not allocated */
		return( j );
		}
	if( n&NAMASK ){
		for( j=mina; j<=maxa; ++j ) if( rstatus[j]&STAREG ){
			if( usable(p,n,j) ){
				return( j );
				}
			}
		}
	else if( n &NBMASK ){
		for( j=minb; j<=maxb; ++j ) if( rstatus[j]&STBREG ){
			if( usable(p,n,j) ){
				return(j);
				}
			}
		}

	return( -1 );
	}

usable( p, n, r ) NODE *p; {
	/* decide if register r is usable in tree p to satisfy need n */

	/* checks, for the moment */
	if( !istreg(r) ) cerror( "usable asked about nontemp register" );

	if( busy[r] > 1 ) return(0);
	if( isbreg(r) ){
		if( n&NAMASK ) return(0);
		}
	else {
		if( n & NBMASK ) return(0);
		}
	if( (n&NAMASK) && (szty(p->type) == 2) ){ /* only do the pairing for real regs */
		if( !istreg(r+1) ) return( 0 );
		if( busy[r+1] > 1 ) return( 0 );
		if( busy[r] == 0 && busy[r+1] == 0  ||
		    busy[r+1] == 0 && shareit( p, r, n ) ||
		    busy[r] == 0 && shareit( p, r+1, n ) ||
		    shareit( p, r, n ) && shareit( p, r+1, n ) ){
			busy[r] |= TBUSY;
			busy[r+1] |= TBUSY;
			return(1);
			}
		else return(0);
		}
	if( busy[r] == 0 ) {
		busy[r] |= TBUSY;
		return(1);
		}

	/* busy[r] is 1: is there chance for sharing */
	return( shareit( p, r, n ) );

	}

shareit( p, r, n ) NODE *p; {
	/* can we make register r available by sharing from p
	   given that the need is n */
	if( (n&(NASL|NBSL)) && ushare( p, 'L', r ) ) return(1);
	if( (n&(NASR|NBSR)) && ushare( p, 'R', r ) ) return(1);
	return(0);
	}

ushare( p, f, r ) NODE *p; {
	/* can we find a register r to share on the left or right
		(as f=='L' or 'R', respectively) of p */
	p = getlr( p, f );
	if( p->op == UNARY MUL ) p = p->left;
	if( p->op == OREG ){
		if( R2TEST(p->rval) ){
			return( r==R2UPK1(p->rval) || r==R2UPK2(p->rval) );
			}
		else return( r == p->rval );
		}
	if( p->op == REG ){
		return( r == p->rval || ( szty(p->type) == 2 && r==p->rval+1 ) );
		}
	return(0);
	}

recl2( p ) register NODE *p; {
	register r = p->rval;
	if( p->op == REG ) rfree( r, p->type );
	else if( p->op == OREG ) {
		if( R2TEST( r ) ) {
			if( R2UPK1( r ) != 100 ) rfree( R2UPK1( r ), PTR+INT );
			rfree( R2UPK2( r ), INT );
			}
		else {
			rfree( r, PTR+INT );
			}
		}
	}

int rdebug = 0;

rfree( r, t ) TWORD t; {
	/* mark register r free, if it is legal to do so */
	/* t is the type */

	if( rdebug ){
		printf( "rfree( %s ), size %d\n", rnames[r], szty(t) );
		}

	if( istreg(r) ){
		if( --busy[r] < 0 ) cerror( "register overfreed");
		if( szty(t) == 2 ){
			if( (istreg(r)^istreg(r+1)) ) cerror( "illegal free" );
			if( --busy[r+1] < 0 ) cerror( "register overfreed" );
			}
		}
	}

rbusy(r,t) TWORD t; {
	/* mark register r busy */
	/* t is the type */

	if( rdebug ){
		printf( "rbusy( %s ), size %d\n", rnames[r], szty(t) );
		}

	if( istreg(r) ) ++busy[r];
	if( szty(t) == 2 ){
		if( istreg(r+1) ) ++busy[r+1];
		if( (istreg(r)^istreg(r+1)) ) cerror( "illegal register pair freed" );
		}
	}

rwprint( rw ){ /* print rewriting rule */
	register i, flag;
	static char * rwnames[] = {

		"RLEFT",
		"RRIGHT",
		"RESC1",
		"RESC2",
		"RESC3",
		0,
		};

	if( rw == RNULL ){
		printf( "RNULL" );
		return;
		}

	if( rw == RNOP ){
		printf( "RNOP" );
		return;
		}

	flag = 0;
	for( i=0; rwnames[i]; ++i ){
		if( rw & (1<<i) ){
			if( flag ) printf( "|" );
			++flag;
			printf( rwnames[i] );
			}
		}
	}

reclaim( p, rw, cookie ) NODE *p; {
	register NODE **qq;
	register NODE *q;
	register i;
	NODE *recres[5];
	struct respref *r;

	/* get back stuff */

	if( rdebug ){
		printf( "reclaim( %o, ", p );
		rwprint( rw );
		printf( ", " );
		prcook( cookie );
		printf( " )\n" );
		}

	if( rw == RNOP || ( p->op==FREE && rw==RNULL ) ) return;  /* do nothing */

	walkf( p, recl2 );

	if( callop(p->op) ){
		/* check that all scratch regs are free */
		callchk(p);  /* ordinarily, this is the same as allchk */
		}

	if( rw == RNULL || (cookie&FOREFF) ){ /* totally clobber, leaving nothing */
		tfree(p);
		return;
		}

	/* handle condition codes specially */

	if( (cookie & FORCC) && (rw&RESCC)) {
		/* result is CC register */
		tfree(p);
		p->op = CCODES;
		p->lval = 0;
		p->rval = 0;
		return;
		}

	/* locate results */

	qq = recres;

	if( rw&RLEFT) *qq++ = p->left;
	if( rw&RRIGHT ) *qq++ = p->right;
	if( rw&RESC1 ) *qq++ = &resc[0];
	if( rw&RESC2 ) *qq++ = &resc[1];
	if( rw&RESC3 ) *qq++ = &resc[2];

	if( qq == recres ){
		cerror( "illegal reclaim");
		}

	*qq = NIL;

	/* now, select the best result, based on the cookie */

	for( r=respref; r->cform; ++r ){
		if( cookie & r->cform ){
			for( qq=recres; (q= *qq) != NIL; ++qq ){
				if( tshape( q, r->mform ) ) goto gotit;
				}
			}
		}

	/* we can't do it; die */
	cerror( "cannot reclaim");

	gotit:

	if( p->op == STARG ) p = p->left;  /* STARGs are still STARGS */

/*	q->type = p->type;  /* to make multi-register allocations work */
	q->type = p->type==FLOAT && q->op==REG ? DOUBLE : p->type;
		/* maybe there is a better way! */
	q = tcopy(q);

	tfree(p);

	p->op = q->op;
	p->lval = q->lval;
	p->rval = q->rval;
	for( i=0; i<NCHNAM; ++i )
		p->name[i] = q->name[i];

	q->op = FREE;

	/* if the thing is in a register, adjust the type */

	switch( p->op ){

	case REG:
		if( p->type == CHAR || p->type == SHORT ) p->type = INT;
		else if( p->type == UCHAR || p->type == USHORT ) p->type = UNSIGNED;
		else if( p->type == FLOAT ) p->type = DOUBLE;
		if( ! (p->rall & MUSTDO ) ) return;  /* unless necessary, ignore it */
		i = p->rall & ~MUSTDO;
		if( i & NOPREF ) return;
		if( i != p->rval ){
			if( busy[i] || ( szty(p->type)==2 && busy[i+1] ) ){
				cerror( "faulty register move" );
				}
			rbusy( i, p->type );
			rfree( p->rval, p->type );
			rmove( i, p->rval, p->type );
			p->rval = i;
			}

	case OREG:
		if( p->op == REG || !R2TEST(p->rval) ) {
			if( busy[p->rval]>1 && istreg(p->rval) ) cerror( "potential register overwrite");
			}
		else
			if( (R2UPK1(p->rval) != 100 && busy[R2UPK1(p->rval)]>1 && istreg(R2UPK1(p->rval)) )
				|| (busy[R2UPK2(p->rval)]>1 && istreg(R2UPK2(p->rval)) ) )
			   cerror( "potential register overwrite");
		}

	}

ncopy( q, p ) NODE *p, *q; {
	/* copy the contents of p into q, without any feeling for
	   the contents */
	/* this code assume that copying rval and lval does the job;
	   in general, it might be necessary to special case the
	   operator types */
	register i;

	q->op = p->op;
	q->rall = p->rall;
	q->type = p->type;
	q->lval = p->lval;
	q->rval = p->rval;
	for( i=0; i<NCHNAM; ++i ) q->name[i]  = p->name[i];

	}

NODE *
tcopy( p ) register NODE *p; {
	/* make a fresh copy of p */

	register NODE *q;
	register r;

	ncopy( q=talloc(), p );

	r = p->rval;
	if( p->op == REG ) rbusy( r, p->type );
	else if( p->op == OREG ) {
		if( R2TEST(r) ){
			if( R2UPK1(r) != 100 ) rbusy( R2UPK1(r), PTR+INT );
			rbusy( R2UPK2(r), INT );
			}
		else {
			rbusy( r, PTR+INT );
			}
		}

	switch( optype(q->op) ){

	case BITYPE:
		q->right = tcopy(p->right);
	case UTYPE:
		q->left = tcopy(p->left);
		}

	return(q);
	}

allchk(){
	/* check to ensure that all register are free */

	register i;

	REGLOOP(i){
		if( istreg(i) && busy[i] ){
			cerror( "register allocation error");
			}
		}

	}
