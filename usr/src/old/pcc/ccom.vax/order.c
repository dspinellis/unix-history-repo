#ifndef lint
static char *sccsid ="@(#)order.c	1.17 (Berkeley) %G%";
#endif lint

# include "pass2.h"

int maxargs = { -1 };

/*ARGSUSED*/
stoasg( p, o ) NODE *p; {
	/* should the assignment op p be stored,
	   given that it lies as the right operand of o
	   (or the left, if o==UNARY MUL) */
	}

deltest( p ) register NODE *p; {
	/* should we delay the INCR or DECR operation p */
	p = p->in.left;
	return( p->in.op == REG || p->in.op == NAME || p->in.op == OREG );
	}

autoincr( p ) NODE *p; {
	register NODE *q = p->in.left;
	register TWORD t;

	if( q->in.op != INCR ||
	    q->in.left->in.op != REG ||
	    !ISPTR(q->in.type) )
		return(0);
	t = q->in.type;
	q->in.type = DECREF(t);
	if( tlen(p) != tlen(q) ) { /* side effects okay? */
		q->in.type = t;
		return(0);
		}
	q->in.type = t;
	if( tlen(p) != q->in.right->tn.lval )
		return(0);

	return(1);
	}

mkadrs(p) register NODE *p; {
	register int o;

	o = p->in.op;

	if( asgop(o) ){
		if( p->in.left->in.su >= p->in.right->in.su ){
			if( p->in.left->in.op == UNARY MUL ){
				SETSTO( p->in.left->in.left, INTEMP );
				}
			else if( p->in.left->in.op == FLD && p->in.left->in.left->in.op == UNARY MUL ){
				SETSTO( p->in.left->in.left->in.left, INTEMP );
				}
			else { /* should be only structure assignment */
				SETSTO( p->in.left, INTEMP );
				}
			}
		else SETSTO( p->in.right, INTEMP );
		}
	else {
		if( p->in.left->in.su > p->in.right->in.su ){
			SETSTO( p->in.left, INTEMP );
			}
		else {
			SETSTO( p->in.right, INTEMP );
			}
		}
	}

/*ARGSUSED*/
notoff( t, r, off, cp) TWORD t; CONSZ off; char *cp; {
	/* is it legal to make an OREG or NAME entry which has an
	/* offset of off, (from a register of r), if the
	/* resulting thing had type t */

	return(0);  /* YES */
	}

# define max(x,y) ((x)<(y)?(y):(x))

sucomp( p ) register NODE *p; {

	/* set the su field in the node to the sethi-ullman
	   number, or local equivalent */

	register int o, ty, sul, sur, r;
	int szr;
	NODE *temp;

	o = p->in.op;
	ty = optype( o );
	p->in.su = szty( p->in.type );   /* 2 for double, else 1 */;

	if( ty == LTYPE ){
		if( o == OREG ){
			r = p->tn.rval;
			/* oreg cost is (worst case) 1 + number of temp registers used */
			if( R2TEST(r) ){
				if( R2UPK1(r)!=100 && istreg(R2UPK1(r)) ) ++p->in.su;
				if( istreg(R2UPK2(r)) ) ++p->in.su;
				}
			else {
				if( istreg( r ) ) ++p->in.su;
				}
			}
		if( p->in.su == szty(p->in.type) &&
		   (p->in.op!=REG || !istreg(p->tn.rval)) &&
		   (p->in.type==INT ||
		    p->in.type==UNSIGNED ||
#if defined(FORT) || defined(SPRECC)
		    p->in.type==FLOAT ||
#endif
		    p->in.type==DOUBLE ||
		    ISPTR(p->in.type) ||
		    ISARY(p->in.type)) )
			p->in.su = 0;
		return;
		}

	else if( ty == UTYPE ){
		switch( o ) {
		case UNARY CALL:
		case UNARY STCALL:
			p->in.su = fregs;  /* all regs needed */
			return;

		default:
			p->in.su =  p->in.left->in.su + (szty( p->in.type ) > 1 ? 2 : 0) ;
			return;
			}
		}


	/* If rhs needs n, lhs needs m, regular su computation */

	sul = p->in.left->in.su;
	sur = p->in.right->in.su;
	szr = szty( p->in.right->in.type );
	if( szty( p->in.type ) > szr && szr >= 1 ) {
		/* implicit conversion in rhs */
		szr = szty( p->in.type );
		sur = max( szr, sur );
	}

	if( o == ASSIGN ){
		/* computed by doing right, then left (if not in mem), then doing it */
		p->in.su = max(sur,sul+1);
		return;
		}

	if( o == CALL || o == STCALL ){
		/* in effect, takes all free registers */
		p->in.su = fregs;
		return;
		}

	if( o == STASG ){
		/* right, then left */
		p->in.su = max( max( 1+sul, sur), fregs );
		return;
		}

	switch( o ){
		case DIV:
		case ASG DIV:
		case MOD:
		case ASG MOD:
			/* EDIV instructions require reg pairs */
			if( p->in.left->in.type == UNSIGNED &&
			    p->in.right->in.op == ICON &&
			    p->in.right->tn.name[0] == '\0' &&
			    (unsigned) p->in.right->tn.lval < 0x80000000 ) {
				p->in.su = sul + 2;
				return;
				}
			break;
		}

	if( asgop(o) ){
		/* computed by doing right, doing left address, doing left, op, and store */
		p->in.su = max(sur,sul+2);
		return;
		}

	switch( o ){
	case ANDAND:
	case OROR:
	case QUEST:
	case COLON:
	case COMOP:
		p->in.su = max( max(sul,sur), 1);
		return;

	case PLUS:
	case MUL:
	case OR:
	case ER:
		/* commutative ops; put harder on left */
		if( p->in.right->in.su > p->in.left->in.su && !istnode(p->in.left) ){
			temp = p->in.left;
			p->in.left = p->in.right;
			p->in.right = temp;
			sul = p->in.left->in.su;
			sur = p->in.right->in.su;
			szr = szty( p->in.right->in.type );
			if( szty( p->in.type ) > szr && szr >= 1 ) {
				/* implicit conversion in rhs */
				szr = szty( p->in.type );
				sur = max( szr, sur );
				}
			}
		break;
		}
	/* binary op, computed by left, then right, then do op */
	p->in.su = max(sul,szr+sur);

	}

int radebug = 0;

rallo( p, down ) NODE *p; {
	/* do register allocation */
	register int o, down1, down2, ty;

	if( radebug ) printf( "rallo( %o, %d )\n", p, down );

	down2 = NOPREF;
	p->in.rall = down;
	down1 = ( down &= ~MUSTDO );

	ty = optype( o = p->in.op );
	switch( o ) {
	case ASSIGN:	
		down1 = NOPREF;
		down2 = down;
		break;

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

	if( ty != LTYPE ) rallo( p->in.left, down1 );
	if( ty == BITYPE ) rallo( p->in.right, down2 );

	}

offstar( p ) register NODE *p; {
	if( p->in.op == PLUS ) {
		if( p->in.left->in.su == fregs ) {
			order( p->in.left, INTAREG|INAREG );
			return;
		} else if( p->in.right->in.su == fregs ) {
			order( p->in.right, INTAREG|INAREG );
			return;
		}
		if( p->in.left->in.op==LS && 
		  (p->in.left->in.left->in.op!=REG || tlen(p->in.left->in.left)!=SZINT/SZCHAR ) ) {
			order( p->in.left->in.left, INTAREG|INAREG );
			return;
		}
		if( p->in.right->in.op==LS &&
		  (p->in.right->in.left->in.op!=REG || tlen(p->in.right->in.left)!=SZINT/SZCHAR ) ) {
			order( p->in.right->in.left, INTAREG|INAREG );
			return;
		}
		if( p->in.type == (PTR|CHAR) || p->in.type == (PTR|UCHAR) ) {
			if( p->in.left->in.op!=REG || tlen(p->in.left)!=SZINT/SZCHAR ) {
				order( p->in.left, INTAREG|INAREG );
				return;
			}
			else if( p->in.right->in.op!=REG || tlen(p->in.right)!=SZINT/SZCHAR ) {
				order(p->in.right, INTAREG|INAREG);
				return;
			}
		}
	}
	if( p->in.op == PLUS || p->in.op == MINUS ){
		if( p->in.right->in.op == ICON ){
			p = p->in.left;
			order( p , INTAREG|INAREG);
			return;
			}
		}

	if( p->in.op == UNARY MUL && !canaddr(p) ) {
		offstar( p->in.left );
		return;
	}

	order( p, INTAREG|INAREG );
	}

setincr( p ) register NODE *p; {
	p = p->in.left;
	if( p->in.op == UNARY MUL ){
		offstar( p->in.left );
		return( 1 );
		}
	return( 0 );
	}

setbin( p ) register NODE *p; {
	register int ro, rt;

	rt = p->in.right->in.type;
	ro = p->in.right->in.op;

	if( canaddr( p->in.left ) && !canaddr( p->in.right ) ) { /* address rhs */
		if( ro == UNARY MUL ) {
			offstar( p->in.right->in.left );
			return(1);
		} else {
			order( p->in.right, INAREG|INTAREG|SOREG );
			return(1);
		}
	}
	if( !istnode( p->in.left) ) { /* try putting LHS into a reg */
		order( p->in.left, INAREG|INTAREG|INBREG|INTBREG|SOREG );
		return(1);
		}
	else if( ro == UNARY MUL && rt != CHAR && rt != UCHAR ){
		offstar( p->in.right->in.left );
		return(1);
		}
	else if( rt == CHAR || rt == UCHAR || rt == SHORT || rt == USHORT ||
#ifndef SPRECC
	    rt == FLOAT ||
#endif
	    (ro != REG && ro != NAME && ro != OREG && ro != ICON ) ){
		order( p->in.right, INAREG|INBREG );
		return(1);
		}
	return(0);
	}

setstr( p ) register NODE *p; { /* structure assignment */
	if( p->in.right->in.op != REG ){
		order( p->in.right, INTAREG );
		return(1);
		}
	p = p->in.left;
	if( p->in.op != NAME && p->in.op != OREG ){
		if( p->in.op != UNARY MUL ) cerror( "bad setstr" );
		order( p->in.left, INTAREG );
		return( 1 );
		}
	return( 0 );
	}

setasg( p ) register NODE *p; {
	/* setup for assignment operator */

	if( !canaddr(p->in.right) ) {
		if( p->in.right->in.op == UNARY MUL )
			offstar(p->in.right->in.left);
		else
			order( p->in.right, INAREG|INBREG|SOREG );
		return(1);
		}
	if( p->in.left->in.op == UNARY MUL ) {
		offstar( p->in.left->in.left );
		return(1);
		}
	if( p->in.left->in.op == FLD && p->in.left->in.left->in.op == UNARY MUL ){
		offstar( p->in.left->in.left->in.left );
		return(1);
		}
/* FLD patch */
	if( p->in.left->in.op == FLD && !(p->in.right->in.type==INT || p->in.right->in.type==UNSIGNED)) {
		order( p->in.right, INAREG);
		return(1);
		}
/* end of FLD patch */
	return(0);
	}

setasop( p ) register NODE *p; {
	/* setup for =ops */
	register int rt, ro;

	rt = p->in.right->in.type;
	ro = p->in.right->in.op;

	if( ro == UNARY MUL && rt != CHAR ){
		offstar( p->in.right->in.left );
		return(1);
		}
	if( rt == CHAR || rt == SHORT || rt == UCHAR || rt == USHORT ||
#ifndef SPRECC
	    rt == FLOAT ||
#endif
	    ( ro != REG && ro != ICON && ro != NAME && ro != OREG ) ){
		order( p->in.right, INAREG|INBREG );
		return(1);
		}


	p = p->in.left;
	if( p->in.op == FLD ) p = p->in.left;

	switch( p->in.op ){

	case REG:
	case ICON:
	case NAME:
	case OREG:
		return(0);

	case UNARY MUL:
		if( p->in.left->in.op==OREG )
			return(0);
		else
			offstar( p->in.left );
		return(1);

		}
	cerror( "illegal setasop" );
	/*NOTREACHED*/
	}

int crslab = 99999;  /* VAX */

getlab(){
	return( crslab-- );
	}

#ifndef deflab
deflab( l ){
	printf( "L%d:\n", l );
	}
#endif

genargs( p ) register NODE *p; {
	register NODE *pasg;
	register int align;
	register int size;
	int count;

	/* generate code for the arguments */

	/*  first, do the arguments on the right */
	while( p->in.op == CM ){
		genargs( p->in.right );
		p->in.op = FREE;
		p = p->in.left;
		}

	if( p->in.op == STARG ){ /* structure valued argument */

		size = p->stn.stsize;
		align = p->stn.stalign;
		if( p->in.left->in.op == ICON ){
			p->in.op = FREE;
			p = p->in.left;
			}
		else {
			/* make it look beautiful... */
			p->in.op = UNARY MUL;
			canon( p );  /* turn it into an oreg */
			for( count = 0; p->in.op != OREG && count < 10; ++count ){
				offstar( p->in.left );
				canon( p );
				}
			if( p->in.op != OREG ) cerror( "stuck starg" );
			}

		pasg = talloc();
		pasg->in.op = STARG;
		pasg->in.rall = NOPREF;
		pasg->stn.stsize = size;
		pasg->stn.stalign = align;
		pasg->in.left = p;

 		order( pasg, FORARG );
		return;
		}

	/* ordinary case */

	order( p, FORARG );
	}

argsize( p ) register NODE *p; {
	register int t;
	t = 0;
	if( p->in.op == CM ){
		t = argsize( p->in.left );
		p = p->in.right;
		}
	if( p->in.type == DOUBLE || p->in.type == FLOAT ){
		SETOFF( t, 4 );
		return( t+8 );
		}
	else if( p->in.op == STARG ){
 		SETOFF( t, 4 );  /* alignment */
 		return( t + ((p->stn.stsize+3)/4)*4 );  /* size */
		}
	else {
		SETOFF( t, 4 );
		return( t+4 );
		}
	}
