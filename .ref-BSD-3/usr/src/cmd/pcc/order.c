# include "mfile2"

int maxargs = { -1 };

stoasg( p, o ) register NODE *p; {
	/* should the assignment op p be stored,
	   given that it lies as the right operand of o
	   (or the left, if o==UNARY MUL) */
/*
	if( p->op == INCR || p->op == DECR ) return;
	if( o==UNARY MUL && p->left->op == REG && !isbreg(p->left->rval) ) SETSTO(p,INAREG);
 */
	}

deltest( p ) register NODE *p; {
	/* should we delay the INCR or DECR operation p */
	p = p->left;
	return( p->op == REG || p->op == NAME || p->op == OREG );
	}

autoincr( p ) NODE *p; {
	register NODE *q = p->left, *r;

	if( q->op == INCR && (r=q->left)->op == REG &&
	    ISPTR(q->type) && p->type == DECREF(q->type) &&
	    tlen(p) == q->right->lval ) return(1);

	return(0);
	}

mkadrs(p) register NODE *p; {
	register o;

	o = p->op;

	if( asgop(o) ){
		if( p->left->su >= p->right->su ){
			if( p->left->op == UNARY MUL ){
				SETSTO( p->left->left, INTEMP );
				}
			else if( p->left->op == FLD && p->left->left->op == UNARY MUL ){
				SETSTO( p->left->left->left, INTEMP );
				}
			else { /* should be only structure assignment */
				SETSTO( p->left, INTEMP );
				}
			}
		else SETSTO( p->right, INTEMP );
		}
	else {
		if( p->left->su > p->right->su ){
			SETSTO( p->left, INTEMP );
			}
		else {
			SETSTO( p->right, INTEMP );
			}
		}
	}

notoff( t, r, off, cp) CONSZ off; char *cp; {
	/* is it legal to make an OREG or NAME entry which has an
	/* offset of off, (from a register of r), if the
	/* resulting thing had type t */

/*	if( r == R0 ) return( 1 );  /* NO */
	return(0);  /* YES */
	}

# define max(x,y) ((x)<(y)?(y):(x))

sucomp( p ) register NODE *p; {

	/* set the su field in the node to the sethi-ullman
	   number, or local equivalent */

	register o, ty, sul, sur, r;

	o = p->op;
	ty = optype( o );
	p->su = szty( p->type );   /* 2 for float or double, else 1 */;

	if( ty == LTYPE ){
		if( o == OREG ){
			r = p->rval;
			/* oreg cost is (worst case) 1 + number of temp registers used */
			if( R2TEST(r) ){
				if( R2UPK1(r)!=100 && istreg(R2UPK1(r)) ) ++p->su;
				if( istreg(R2UPK2(r)) ) ++p->su;
				}
			else {
				if( istreg( r ) ) ++p->su;
				}
			}
		if( p->su == szty(p->type) &&
		   (p->op!=REG || !istreg(p->rval)) &&
		   (p->type==INT || p->type==UNSIGNED || p->type==DOUBLE) )
			p->su = 0;
		return;
		}

	else if( ty == UTYPE ){
		switch( o ) {
		case UNARY CALL:
		case UNARY STCALL:
			p->su = fregs;  /* all regs needed */
			return;

		default:
			p->su =  p->left->su + (szty( p->type ) > 1 ? 2 : 0) ;
			return;
			}
		}


	/* If rhs needs n, lhs needs m, regular su computation */

	sul = p->left->su;
	sur = p->right->su;

	if( o == ASSIGN ){
		/* computed by doing right, then left (if not in mem), then doing it */
		p->su = max(sur,sul+1);
		return;
		}

	if( o == CALL || o == STCALL ){
		/* in effect, takes all free registers */
		p->su = fregs;
		return;
		}

	if( o == STASG ){
		/* right, then left */
		p->su = max( max( 1+sul, sur), fregs );
		return;
		}

	if( asgop(o) ){
		/* computed by doing right, doing left address, doing left, op, and store */
		p->su = max(sur,sul+2);
/*
		if( o==ASG MUL || o==ASG DIV || o==ASG MOD) p->su = max(p->su,fregs);
 */
		return;
		}

	switch( o ){
	case ANDAND:
	case OROR:
	case QUEST:
	case COLON:
	case COMOP:
		p->su = max( max(sul,sur), 1);
		return;

	case PLUS:
	case OR:
	case ER:
		/* commutative ops; put harder on left */
		if( p->right->su > p->left->su && !istnode(p->left) ){
			register NODE *temp;
			temp = p->left;
			p->left = p->right;
			p->right = temp;
			}
		break;
		}

	/* binary op, computed by left, then right, then do op */
	p->su = max(sul,szty(p->right->type)+sur);
/*
	if( o==MUL||o==DIV||o==MOD) p->su = max(p->su,fregs);
 */

	}

int radebug = 0;

rallo( p, down ) NODE *p; {
	/* do register allocation */
	register o, type, down1, down2, ty;

	if( radebug ) printf( "rallo( %o, %d )\n", p, down );

	down2 = NOPREF;
	p->rall = down;
	down1 = ( down &= ~MUSTDO );

	ty = optype( o = p->op );
	type = p->type;


	if( type == DOUBLE || type == FLOAT ){
		if( o == FORCE ) down1 = R0|MUSTDO;
		}
	else switch( o ) {
	case ASSIGN:	
		down1 = NOPREF;
		down2 = down;
		break;

/*
	case MUL:
	case DIV:
	case MOD:
		down1 = R3|MUSTDO;
		down2 = R5|MUSTDO;
		break;

	case ASG MUL:
	case ASG DIV:
	case ASG MOD:
		p->left->rall = down1 = R3|MUSTDO;
		if( p->left->op == UNARY MUL ){
			rallo( p->left->left, R4|MUSTDO );
			}
		else if( p->left->op == FLD  && p->left->left->op == UNARY MUL ){
			rallo( p->left->left->left, R4|MUSTDO );
			}
		else rallo( p->left, R3|MUSTDO );
		rallo( p->right, R5|MUSTDO );
		return;
 */

	case CALL:
	case STASG:
	case EQ:
	case NE:
	case GT:
	case GE:
	case LT:
	case LE:
	case NOT:
	case ANDAND:
	case OROR:
		down1 = NOPREF;
		break;

	case FORCE:	
		down1 = R0|MUSTDO;
		break;

		}

	if( ty != LTYPE ) rallo( p->left, down1 );
	if( ty == BITYPE ) rallo( p->right, down2 );

	}

offstar( p ) register NODE *p; {
	if( p->op == PLUS ) {
		if( p->left->su == fregs ) {
			order( p->left, INTAREG|INAREG );
			return;
		} else if( p->right->su == fregs ) {
			order( p->right, INTAREG|INAREG );
			return;
		}
		if( p->left->op==LS && 
		  (p->left->left->op!=REG || tlen(p->left->left)!=sizeof(int) ) ) {
			order( p->left->left, INTAREG|INAREG );
			return;
		}
		if( p->right->op==LS &&
		  (p->right->left->op!=REG || tlen(p->right->left)!=sizeof(int) ) ) {
			order( p->right->left, INTAREG|INAREG );
			return;
		}
		if( p->type == (PTR|CHAR) || p->type == (PTR|UCHAR) ) {
			if( p->left->op!=REG || tlen(p->left)!=sizeof(int) ) {
				order( p->left, INTAREG|INAREG );
				return;
			}
			else if( p->right->op!=REG || tlen(p->right)!=sizeof(int) ) {
				order(p->right, INTAREG|INAREG);
				return;
			}
		}
	}
	if( p->op == PLUS || p->op == MINUS ){
		if( p->right->op == ICON ){
			p = p->left;
			order( p , INTAREG|INAREG);
			return;
			}
		}

	if( p->op == UNARY MUL && !canaddr(p) ) {
		offstar( p->left );
		return;
	}

	order( p, INTAREG|INAREG );
	}

setincr( p ) NODE *p; {
	return( 0 );  /* for the moment, don't bother */
	}

setbin( p ) register NODE *p; {
	register ro, rt;

	rt = p->right->type;
	ro = p->right->op;

	if( canaddr( p->left ) && !canaddr( p->right ) ) { /* address rhs */
		if( ro == UNARY MUL ) {
			offstar( p->right->left );
			return(1);
		} else {
			order( p->right, INAREG|INTAREG|SOREG );
			return(1);
		}
	}
	if( !istnode( p->left) ) { /* try putting LHS into a reg */
/*		order( p->left, logop(p->op)?(INAREG|INBREG|INTAREG|INTBREG|SOREG):(INTAREG|INTBREG|SOREG) );*/
		order( p->left, INAREG|INTAREG|INBREG|INTBREG|SOREG );
		return(1);
		}
	else if( ro == UNARY MUL && rt != CHAR && rt != UCHAR ){
		offstar( p->right->left );
		return(1);
		}
	else if( rt == CHAR || rt == UCHAR || rt == SHORT || rt == USHORT || (ro != REG &&
			ro != NAME && ro != OREG && ro != ICON ) ){
		order( p->right, INAREG|INBREG );
		return(1);
		}
/*
	else if( logop(p->op) && rt==USHORT ){  /* must get rhs into register */
/*
		order( p->right, INAREG );
		return( 1 );
		}
 */
	return(0);
	}

setstr( p ) register NODE *p; { /* structure assignment */
	if( p->right->op != REG ){
		order( p->right, INTAREG );
		return(1);
		}
	p = p->left;
	if( p->op != NAME && p->op != OREG ){
		if( p->op != UNARY MUL ) cerror( "bad setstr" );
		order( p->left, INTAREG );
		return( 1 );
		}
	return( 0 );
	}

setasg( p ) register NODE *p; {
	/* setup for assignment operator */

	if( !canaddr(p->right) ) {
		if( p->right->op == UNARY MUL )
			offstar(p->right->left);
		else
			order( p->right, INAREG|INBREG|SOREG );
		return(1);
		}
	if( p->left->op == UNARY MUL ) {
		offstar( p->left->left );
		return(1);
		}
	if( p->left->op == FLD && p->left->left->op == UNARY MUL ){
		offstar( p->left->left->left );
		return(1);
		}
/* FLD patch */
	if( p->left->op == FLD && !(p->right->type==INT || p->right->type==UNSIGNED)) {
		order( p->right, INAREG);
		return(1);
		}
/* end of FLD patch */
	return(0);
	}

setasop( p ) register NODE *p; {
	/* setup for =ops */
	register rt, ro;

	rt = p->right->type;
	ro = p->right->op;

	if( ro == UNARY MUL && rt != CHAR ){
		offstar( p->right->left );
		return(1);
		}
	if( ( rt == CHAR || rt == SHORT || rt == UCHAR || rt == USHORT ||
			( ro != REG && ro != ICON && ro != NAME && ro != OREG ) ) ){
		order( p->right, INAREG|INBREG );
		return(1);
		}
/*
	if( (p->op == ASG LS || p->op == ASG RS) && ro != ICON && ro != REG ){
		order( p->right, INAREG );
		return(1);
		}
 */


	p = p->left;
	if( p->op == FLD ) p = p->left;

	switch( p->op ){

	case REG:
	case ICON:
	case NAME:
	case OREG:
		return(0);

	case UNARY MUL:
		if( p->left->op==OREG )
			return(0);
		else
			offstar( p->left );
		return(1);

		}
	cerror( "illegal setasop" );
	}

int crslab = 9999;  /* Honeywell */

getlab(){
	return( crslab-- );
	}

deflab( l ){
	printf( "L%d:\n", l );
	}

genargs( p, ptemp ) register NODE *p, *ptemp; {
	register NODE *pasg;
	register align;
	register size;
	register TWORD type;

	/* generate code for the arguments */

	/*  first, do the arguments on the right */
	while( p->op == CM ){
		genargs( p->right, ptemp );
		p->op = FREE;
		p = p->left;
		}

	if( p->op == STARG ){ /* structure valued argument */

		size = p->stsize;
		align = p->stalign;
		if( p->left->op == ICON ){
			p->op = FREE;
			p= p->left;
			}
		else {
			/* make it look beautiful... */
			p->op = UNARY MUL;
			canon( p );  /* turn it into an oreg */
			if( p->op != OREG ){
				offstar( p->left );
				canon( p );
				if( p->op != OREG ){
					offstar( p->left );
					canon( p );
					if( p->op != OREG ) cerror( "stuck starg" );
					}
				}
			}


 		ptemp->lval = 0;	/* all moves to (sp) */

		pasg = talloc();
		pasg->op = STASG;
		pasg->stsize = size;
		pasg->stalign = align;
		pasg->right = p;
		pasg->left = tcopy( ptemp );

		/* the following line is done only with the knowledge
		that it will be undone by the STASG node, with the
		offset (lval) field retained */

		if( p->op == OREG ) p->op = REG;  /* only for temporaries */

 		order( pasg, FORARG );
		ptemp->lval += size;
		return;
		}

	/* ordinary case */

	order( p, FORARG );
	}

argsize( p ) register NODE *p; {
	register t;
	t = 0;
	if( p->op == CM ){
		t = argsize( p->left );
		p = p->right;
		}
	if( p->type == DOUBLE || p->type == FLOAT ){
		SETOFF( t, 4 );
		return( t+8 );
		}
	else if( p->op == STARG ){
 		SETOFF( t, 4 );  /* alignment */
 		return( t + ((p->stsize+3)/4)*4 );  /* size */
		}
	else {
		SETOFF( t, 4 );
		return( t+4 );
		}
	}



