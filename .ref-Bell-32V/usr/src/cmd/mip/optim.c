# include "mfile1"

# define SWAP(p,q) {sp=p; p=q; q=sp;}
# define RCON(p) (p->right->op==ICON)
# define RO(p) p->right->op
# define RV(p) p->right->lval
# define LCON(p) (p->left->op==ICON)
# define LO(p) p->left->op
# define LV(p) p->left->lval

int oflag = 0;

NODE *
fortarg( p ) NODE *p; {
	/* fortran function arguments */

	if( p->op == CM ){
		p->left = fortarg( p->left );
		p->right = fortarg( p->right );
		return(p);
		}

	while( ISPTR(p->type) ){
		p = buildtree( UNARY MUL, p, NIL );
		}
	return( optim(p) );
	}

	/* mapping relationals when the sides are reversed */
short revrel[] ={ EQ, NE, GE, GT, LE, LT, UGE, UGT, ULE, ULT };
NODE *
optim(p) register NODE *p; {
	/* local optimizations, most of which are probably machine independent */

	register o, ty;
	NODE *sp;
	int i;
	TWORD t;

	if( (t=BTYPE(p->type))==ENUMTY || t==MOETY ) econvert(p);
	if( oflag ) return(p);
	ty = optype( o=p->op);
	if( ty == LTYPE ) return(p);

	if( ty == BITYPE ) p->right = optim(p->right);
	p->left = optim(p->left);

	/* collect constants */

	switch(o){

	case SCONV:
	case PCONV:
		return( clocal(p) );

	case FORTCALL:
		p->right = fortarg( p->right );
		break;

	case UNARY AND:
		if( LO(p) != NAME ) cerror( "& error" );

		if( !andable(p->left) ) return(p);

		LO(p) = ICON;

		setuleft:
		/* paint over the type of the left hand side with the type of the top */
		p->left->type = p->type;
		p->left->cdim = p->cdim;
		p->left->csiz = p->csiz;
		p->op = FREE;
		return( p->left );

	case UNARY MUL:
		if( LO(p) != ICON ) break;
		LO(p) = NAME;
		goto setuleft;

	case MINUS:
		if( !nncon(p->right) ) break;
		RV(p) = -RV(p);
		o = p->op = PLUS;

	case MUL:
	case PLUS:
	case AND:
	case OR:
	case ER:
		/* commutative ops; for now, just collect constants */
		/* someday, do it right */
		if( nncon(p->left) || ( LCON(p) && !RCON(p) ) ) SWAP( p->left, p->right );
		/* make ops tower to the left, not the right */
		if( RO(p) == o ){
			NODE *t1, *t2, *t3;
			t1 = p->left;
			sp = p->right;
			t2 = sp->left;
			t3 = sp->right;
			/* now, put together again */
			p->left = sp;
			sp->left = t1;
			sp->right = t2;
			p->right = t3;
			}
		if(o == PLUS && LO(p) == MINUS && RCON(p) && RCON(p->left) &&
		  conval(p->right, MINUS, p->left->right)){
			zapleft:
			RO(p->left) = FREE;
			LO(p) = FREE;
			p->left = p->left->left;
		}
		if( RCON(p) && LO(p)==o && RCON(p->left) && conval( p->right, o, p->left->right ) ){
			goto zapleft;
			}
		else if( LCON(p) && RCON(p) && conval( p->left, o, p->right ) ){
			zapright:
			RO(p) = FREE;
			p->left = makety( p->left, p->type, p->cdim, p->csiz );
			p->op = FREE;
			return( clocal( p->left ) );
			}

		/* change muls to shifts */

		if( o==MUL && nncon(p->right) && (i=ispow2(RV(p)))>=0){
			if( i == 0 ){ /* multiplication by 1 */
				goto zapright;
				}
			o = p->op = LS;
			p->right->type = p->right->csiz = INT;
			RV(p) = i;
			}

		/* change +'s of negative consts back to - */
		if( o==PLUS && nncon(p->right) && RV(p)<0 ){
			RV(p) = -RV(p);
			o = p->op = MINUS;
			}
		break;

	case DIV:
		if( nncon( p->right ) && p->right->lval == 1 ) goto zapright;
		break;

	case EQ:
	case NE:
	case LT:
	case LE:
	case GT:
	case GE:
	case ULT:
	case ULE:
	case UGT:
	case UGE:
		if( !LCON(p) ) break;

		/* exchange operands */

		sp = p->left;
		p->left = p->right;
		p->right = sp;
		p->op = revrel[p->op - EQ ];
		break;

		}

	return(p);
	}

ispow2( c ) CONSZ c; {
	register i;
	if( c <= 0 || (c&(c-1)) ) return(-1);
	for( i=0; c>1; ++i) c >>= 1;
	return(i);
	}

nncon( p ) NODE *p; {
	/* is p a constant without a name */
	return( p->op == ICON && p->rval == NONAME );
	}
