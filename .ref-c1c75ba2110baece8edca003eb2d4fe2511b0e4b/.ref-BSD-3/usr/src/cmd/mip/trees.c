# include "mfile1"

/*	some special actions, used in finding the type of nodes */
# define NCVT 01
# define PUN 02
# define TYPL 04
# define TYPR 010
# define TYMATCH 040
# define LVAL 0100
# define CVTO 0200
# define CVTL 0400
# define CVTR 01000
# define PTMATCH 02000
# define OTHER 04000
# define NCVTR 010000

/* node conventions:

	NAME:	rval>0 is stab index for external
		rval<0 is -inlabel number
		lval is offset in bits
	ICON:	lval has the value
		rval has the STAB index, or - label number,
			if a name whose address is in the constant
		rval = NONAME means no name
	REG:	rval is reg. identification cookie

	*/

int bdebug = 0;

NODE *
buildtree( o, l, r ) register NODE *l, *r; {
	register NODE *p, *q;
	register actions;
	register opty;
	register struct symtab *sp;
	register NODE *lr, *ll;
	int i;
	extern int eprint();

	if( bdebug ) printf( "buildtree( %s, %o, %o )\n", opst[o], l, r );
	opty = optype(o);

	/* check for constants */

	if( opty == UTYPE && l->op == ICON ){

		switch( o ){

		case NOT:
			if( hflag ) werror( "constant argument to NOT" );
		case UNARY MINUS:
		case COMPL:
			if( conval( l, o, l ) ) return(l);
			break;

			}
		}

	else if( o==UNARY MINUS && l->op==FCON ){
		l->dval = -l->dval;
		return(l);
		}

	else if( o==QUEST && l->op==ICON ) {
		l->op = FREE;
		r->op = FREE;
		if( l->lval ){
			tfree( r->right );
			return( r->left );
			}
		else {
			tfree( r->left );
			return( r->right );
			}
		}

	else if( (o==ANDAND || o==OROR) && (l->op==ICON||r->op==ICON) ) goto ccwarn;

	else if( opty == BITYPE && l->op == ICON && r->op == ICON ){

		switch( o ){

		case ULT:
		case UGT:
		case ULE:
		case UGE:
		case LT:
		case GT:
		case LE:
		case GE:
		case EQ:
		case NE:
		case ANDAND:
		case OROR:
		case CBRANCH:

		ccwarn:
			if( hflag ) werror( "constant in conditional context" );

		case PLUS:
		case MINUS:
		case MUL:
		case DIV:
		case MOD:
		case AND:
		case OR:
		case ER:
		case LS:
		case RS:
			if( conval( l, o, r ) ) {
				r->op = FREE;
				return(l);
				}
			break;
			}
		}

	else if( opty == BITYPE && (l->op==FCON||l->op==ICON) &&
		(r->op==FCON||r->op==ICON) ){
		switch(o){
		case PLUS:
		case MINUS:
		case MUL:
		case DIV:
			if( l->op == ICON ){
				l->dval = l->lval;
				}
			if( r->op == ICON ){
				r->dval = r->lval;
				}
			l->op = FCON;
			l->type = l->csiz = DOUBLE;
			r->op = FREE;
			switch(o){
			case PLUS:
				l->dval += r->dval;
				return(l);
			case MINUS:
				l->dval -= r->dval;
				return(l);
			case MUL:
				l->dval *= r->dval;
				return(l);
			case DIV:
				if( r->dval == 0 ) uerror( "division by 0." );
				else l->dval /= r->dval;
				return(l);
				}
			}
		}

	/* its real; we must make a new node */

	p = block( o, l, r, INT, 0, INT );

	actions = opact(p);

	if( actions&LVAL ){ /* check left descendent */
		if( notlval(p->left) ) {
			uerror( "lvalue required" );
			}
		}

	if( actions & NCVTR ){
		p->left = pconvert( p->left );
		}
	else if( !(actions & NCVT ) ){
		switch( opty ){

		case BITYPE:
			p->right = pconvert( p->right );
		case UTYPE:
			p->left = pconvert( p->left );

			}
		}

	if( (actions&PUN) && (o!=CAST||cflag) ){
		chkpun(p);
		}

	if( actions & (TYPL|TYPR) ){

		q = (actions&TYPL) ? p->left : p->right;

		p->type = q->type;
		p->cdim = q->cdim;
		p->csiz = q->csiz;
		}

	if( actions & CVTL ) p = convert( p, CVTL );
	if( actions & CVTR ) p = convert( p, CVTR );
	if( actions & TYMATCH ) p = tymatch(p);
	if( actions & PTMATCH ) p = ptmatch(p);

	if( actions & OTHER ){
		l = p->left;
		r = p->right;

		switch(o){

		case NAME:
			sp = &stab[idname];
			if( sp->stype == UNDEF ){
				uerror( "%.8s undefined", sp->sname );
				/* make p look reasonable */
				p->type = p->cdim = p->csiz = INT;
				p->rval = idname;
				p->lval = 0;
				defid( p, SNULL );
				break;
				}
			p->type = sp->stype;
			p->cdim = sp->dimoff;
			p->csiz = sp->sizoff;
			p->lval = 0;
			p->rval = idname;
			/* special case: MOETY is really an ICON... */
			if( p->type == MOETY ){
				p->rval = NONAME;
				p->lval = sp->offset;
				p->cdim = 0;
				p->type = ENUMTY;
				p->op = ICON;
				}
			break;

		case ICON:
			p->type = INT;
			p->cdim = 0;
			p->csiz = INT;
			break;

		case STRING:
			p->op = NAME;
			p->type = CHAR+ARY;
			p->lval = 0;
			p->rval = NOLAB;
			p->cdim = curdim;
			p->csiz = CHAR;
			break;

		case FCON:
			p->lval = 0;
			p->rval = 0;
			p->type = DOUBLE;
			p->cdim = 0;
			p->csiz = DOUBLE;
			break;

		case STREF:
			/* p->x turned into *(p+offset) */
			/* rhs must be a name; check correctness */

			i = r->rval;
			if( i<0 || ((sp= &stab[i])->sclass != MOS && sp->sclass != MOU && !(sp->sclass&FIELD)) ){
				uerror( "member of structure or union required" );
				}
			else {
				register j;
				if( l->type != PTR+STRTY && l->type != PTR+UNIONTY ){
					werror( "struct/union or struct/union pointer required" );
					}
				else if( (j=l->csiz+1)<0 ) cerror( "undefined structure or union" );
				else if( !chkstr( i, dimtab[j], DECREF(l->type) ) ){
					werror( "illegal member use: %.8s", stab[i].sname );
					}
				}

			p = stref( p );
			break;

		case UNARY MUL:
			if( l->op == UNARY AND ){
				p->op = l->op = FREE;
				p = l->left;
				}
			if( !ISPTR(l->type))uerror("illegal indirection");
			p->type = DECREF(l->type);
			p->cdim = l->cdim;
			p->csiz = l->csiz;
			break;

		case UNARY AND:
			switch( l->op ){

			case UNARY MUL:
				p->op = l->op = FREE;
				p = l->left;
			case NAME:
				p->type = INCREF( l->type );
				p->cdim = l->cdim;
				p->csiz = l->csiz;
				break;

			case COMOP:
				lr = buildtree( UNARY AND, l->right, NIL );
				p->op = l->op = FREE;
				p = buildtree( COMOP, l->left, lr );
				break;

			case QUEST:
				lr = buildtree( UNARY AND, l->right->right, NIL );
				ll = buildtree( UNARY AND, l->right->left, NIL );
				p->op = l->op = l->right->op = FREE;
				p = buildtree( QUEST, l->left, buildtree( COLON, ll, lr ) );
				break;

			default:
				uerror( "unacceptable operand of &" );
				break;
				}
			break;

		case LS:
		case RS:
		case ASG LS:
		case ASG RS:
			if(tsize(p->right->type, p->right->cdim, p->right->csiz) > SZINT)
				p->right = makety(p->right, INT, 0, INT );
			break;

		case RETURN:
		case ASSIGN:
		case CAST:
			/* structure assignment */
			/* take the addresses of the two sides; then make an
			/* operator using STASG and
			/* the addresses of left and right */

			{
				register TWORD t;
				register d, s;

				if( l->csiz != r->csiz ) uerror( "assignment of different structures" );

				r = buildtree( UNARY AND, r, NIL );
				t = r->type;
				d = r->cdim;
				s = r->csiz;

				l = block( STASG, l, r, t, d, s );

				if( o == RETURN ){
					p->op = FREE;
					p = l;
					break;
					}

				p->op = UNARY MUL;
				p->left = l;
				p->right = NIL;
				break;
				}
		case COLON:
			/* structure colon */

			if( l->csiz != r->csiz ) uerror( "type clash in conditional" );
			break;

		case CALL:
			p->right = r = strargs( p->right );
		case UNARY CALL:
			if( !ISPTR(l->type)) uerror("illegal function");
			p->type = DECREF(l->type);
			if( !ISFTN(p->type)) uerror("illegal function");
			p->type = DECREF( p->type );
			p->cdim = l->cdim;
			p->csiz = l->csiz;
			if( l->op == UNARY AND && l->left->op == NAME &&
				l->left->rval >= 0 && l->left->rval != NONAME &&
				( (i=stab[l->left->rval].sclass) == FORTRAN || i==UFORTRAN ) ){
				p->op += (FORTCALL-CALL);
				}
			if( p->type == STRTY || p->type == UNIONTY ){
				/* function returning structure */
				/*  make function really return ptr to str., with * */

				p->op += STCALL-CALL;
				p->type = INCREF( p->type );
				p = buildtree( UNARY MUL, p, NIL );

				}
			break;

		default:
			cerror( "other code %d", o );
			}

		}

	if( actions & CVTO ) p = oconvert(p);
	p = clocal(p);

	if( bdebug ) fwalk( p, eprint, 0 );

	return(p);

	}

NODE *
strargs( p ) register NODE *p;  { /* rewrite structure flavored arguments */

	if( p->op == CM ){
		p->left = strargs( p->left );
		p->right = strargs( p->right );
		return( p );
		}

	if( p->type == STRTY || p->type == UNIONTY ){
		p = block( STARG, p, NIL, p->type, p->cdim, p->csiz );
		p->left = buildtree( UNARY AND, p->left, NIL );
		p = clocal(p);
		}
	return( p );
	}

chkstr( i, j, type ) TWORD type; {
	/* is the MOS or MOU at stab[i] OK for strict reference by a ptr */
	/* i has been checked to contain a MOS or MOU */
	/* j is the index in dimtab of the members... */
	int k, kk;

	extern int ddebug;

	if( ddebug > 1 ) printf( "chkstr( %.8s(%d), %d )\n", stab[i].sname, i, j );
	if( (k = j) < 0 ) uerror( "undefined structure or union" );
	else {
		for( ; (kk = dimtab[k] ) >= 0; ++k ){
			if( kk >= SYMTSZ ){
				cerror( "gummy structure" );
				return(1);
				}
			if( kk == i ) return( 1 );
			switch( stab[kk].stype ){

			case STRTY:
			case UNIONTY:
				if( type == STRTY ) continue;  /* no recursive looking for strs */
				if( chkstr( i, dimtab[stab[kk].sizoff+1], stab[kk].stype ) ) return(1);
				}
			}
		}
	return( 0 );
	}

conval( p, o, q ) register NODE *p, *q; {
	/* apply the op o to the lval part of p; if binary, rhs is val */
	int i, u;
	CONSZ val;

	val = q->lval;
	u = ISUNSIGNED(p->type) || ISUNSIGNED(q->type);
	if( u && (o==LE||o==LT||o==GE||o==GT)) o += (UGE-GE);

	if( p->rval != NONAME && q->rval != NONAME ) return(0);
	if( q->rval != NONAME && o!=PLUS ) return(0);
	if( p->rval != NONAME && o!=PLUS && o!=MINUS ) return(0);

	switch( o ){

	case PLUS:
		p->lval += val;
		if( p->rval == NONAME ){
			p->rval = q->rval;
			p->type = q->type;
			}
		break;
	case MINUS:
		p->lval -= val;
		break;
	case MUL:
		p->lval *= val;
		break;
	case DIV:
		if( val == 0 ) uerror( "division by 0" );
		else p->lval /= val;
		break;
	case MOD:
		if( val == 0 ) uerror( "division by 0" );
		else p->lval %= val;
		break;
	case AND:
		p->lval &= val;
		break;
	case OR:
		p->lval |= val;
		break;
	case ER:
		p->lval ^=  val;
		break;
	case LS:
		i = val;
		p->lval = p->lval << i;
		break;
	case RS:
		i = val;
		p->lval = p->lval >> i;
		break;

	case UNARY MINUS:
		p->lval = - p->lval;
		break;
	case COMPL:
		p->lval = ~p->lval;
		break;
	case NOT:
		p->lval = !p->lval;
		break;
	case LT:
		p->lval = p->lval < val;
		break;
	case LE:
		p->lval = p->lval <= val;
		break;
	case GT:
		p->lval = p->lval > val;
		break;
	case GE:
		p->lval = p->lval >= val;
		break;
	case ULT:
		p->lval = (p->lval-val)<0;
		break;
	case ULE:
		p->lval = (p->lval-val)<=0;
		break;
	case UGE:
		p->lval = (p->lval-val)>=0;
		break;
	case UGT:
		p->lval = (p->lval-val)>0;
		break;
	case EQ:
		p->lval = p->lval == val;
		break;
	case NE:
		p->lval = p->lval != val;
		break;
	default:
		return(0);
		}
	return(1);
	}

chkpun(p) register NODE *p; {

	/* checks p for the existance of a pun */

	/* this is called when the op of p is ASSIGN, RETURN, CAST, COLON, or relational */

	/* one case is when enumerations are used: this applies only to lint */
	/* in the other case, one operand is a pointer, the other integer type */
	/* we check that this integer is in fact a constant zero... */

	/* in the case of ASSIGN, any assignment of pointer to integer is illegal */
	/* this falls out, because the LHS is never 0 */

	register NODE *q;
	register t1, t2;
	register d1, d2;

	t1 = p->left->type;
	t2 = p->right->type;

	if( t1==ENUMTY || t2==ENUMTY ) { /* check for enumerations */
		if( logop( p->op ) && p->op != EQ && p->op != NE ) {
			uerror( "illegal comparison of enums" );
			return;
			}
		if( t1==ENUMTY && t2==ENUMTY && p->left->csiz==p->right->csiz ) return;
		werror( "enumeration type clash, operator %s", opst[p->op] );
		return;
		}

	if( ISPTR(t1) || ISARY(t1) ) q = p->right;
	else q = p->left;

	if( !ISPTR(q->type) && !ISARY(q->type) ){
		if( q->op != ICON || q->lval != 0 ){
			werror( "illegal combination of pointer and integer");
			}
		}
	else {
		d1 = p->left->cdim;
		d2 = p->right->cdim;
		for( ;; ){
			if( t1 == t2 ) {;
				if( p->left->csiz != p->right->csiz ) {
					werror( "illegal structure pointer combination" );
					}
				return;
				}
			if( ISARY(t1) || ISPTR(t1) ){
				if( !ISARY(t2) && !ISPTR(t2) ) break;
				if( ISARY(t1) && ISARY(t2) && dimtab[d1] != dimtab[d2] ){
					werror( "illegal array size combination" );
					return;
					}
				if( ISARY(t1) ) ++d1;
				if( ISARY(t2) ) ++d2;
				}
			else break;
			t1 = DECREF(t1);
			t2 = DECREF(t2);
			}
		werror( "illegal pointer combination" );
		}

	}

NODE *
stref( p ) register NODE *p; {

	TWORD t;
	int d, s, dsc;
	OFFSZ off;
	register struct symtab *q;

	/* make p->x */
	/* this is also used to reference automatic variables */

	q = &stab[p->right->rval];
	p->right->op = FREE;
	p->op = FREE;
	p = pconvert( p->left );

	/* make p look like ptr to x */

	if( !ISPTR(p->type)){
		p->type = PTR+UNIONTY;
		}

	t = INCREF( q->stype );
	d = q->dimoff;
	s = q->sizoff;

	p = makety( p, t, d, s );

	/* compute the offset to be added */

	off = q->offset;
	dsc = q->sclass;

	if( dsc & FIELD ){ /* make fields look like ints */
		off = (off/ALINT)*ALINT;
		s = INT;
		}
	if( off != 0 ) p = clocal( block( PLUS, p, offcon( off, t, d, s ), t, d, s ) );

	p = buildtree( UNARY MUL, p, NIL );

	/* if field, build field info */

	if( dsc & FIELD ){
		p = block( FLD, p, NIL, q->stype, 0, q->sizoff );
		p->rval = PKFIELD( dsc&FLDSIZ, q->offset%ALINT );
		}

	return( clocal(p) );
	}

notlval(p) register NODE *p; {

	/* return 0 if p an lvalue, 1 otherwise */

	again:

	switch( p->op ){

	case FLD:
		p = p->left;
		goto again;

	case NAME:
	case OREG:
	case UNARY MUL:
		if( ISARY(p->type) || ISFTN(p->type) ) return(1);
	case REG:
		return(0);

	default:
		return(1);

		}

	}

NODE *
bcon( i ){ /* make a constant node with value i */
	register NODE *p;

	p = block( ICON, NIL, NIL, INT, 0, INT );
	p->lval = i;
	p->rval = NONAME;
	return( clocal(p) );
	}

NODE *
bpsize(p) register NODE *p; {
	return( offcon( psize(p), p->type, p->cdim, p->csiz ) );
	}

OFFSZ
psize( p ) NODE *p; {
	/* p is a node of type pointer; psize returns the
	   size of the thing pointed to */

	if( !ISPTR(p->type) ){
		uerror( "pointer required");
		return( SZINT );
		}
	/* note: no pointers to fields */
	return( tsize( DECREF(p->type), p->cdim, p->csiz ) );
	}

NODE *
convert( p, f )  register NODE *p; {
	/*  convert an operand of p
	    f is either CVTL or CVTR
	    operand has type int, and is converted by the size of the other side
	    */

	register NODE *q, *r;

	q = (f==CVTL)?p->left:p->right;

	r = block( PMCONV,
		q, bpsize(f==CVTL?p->right:p->left), INT, 0, INT );
	r = clocal(r);
	if( f == CVTL )
		p->left = r;
	else
		p->right = r;
	return(p);

	}

econvert( p ) register NODE *p; {

	/* change enums to ints, or appropriate types */

	register TWORD ty;

	if( (ty=BTYPE(p->type)) == ENUMTY || ty == MOETY ) {
		if( dimtab[ p->csiz ] == SZCHAR ) ty = CHAR;
		else if( dimtab[ p->csiz ] == SZINT ) ty = INT;
		else if( dimtab[ p->csiz ] == SZSHORT ) ty = SHORT;
		else ty = LONG;
		ty = ctype( ty );
		p->csiz = ty;
		MODTYPE(p->type,ty);
		if( p->op == ICON && ty != LONG ) p->type = p->csiz = INT;
		}
	}

NODE *
pconvert( p ) register NODE *p; {

	/* if p should be changed into a pointer, do so */

	if( ISARY( p->type) ){
		p->type = DECREF( p->type );
		++p->cdim;
		return( buildtree( UNARY AND, p, NIL ) );
		}
	if( ISFTN( p->type) )
		return( buildtree( UNARY AND, p, NIL ) );

	return( p );
	}

NODE *
oconvert(p) register NODE *p; {
	/* convert the result itself: used for pointer and unsigned */

	switch(p->op) {

	case LE:
	case LT:
	case GE:
	case GT:
		if( ISUNSIGNED(p->left->type) || ISUNSIGNED(p->right->type) )  p->op += (ULE-LE);
	case EQ:
	case NE:
		return( p );

	case MINUS:
		return(  clocal( block( PVCONV,
			p, bpsize(p->left), INT, 0, INT ) ) );
		}

	cerror( "illegal oconvert: %d", p->op );

	return(p);
	}

NODE *
ptmatch(p)  register NODE *p; {

	/* makes the operands of p agree; they are
	   either pointers or integers, by this time */
	/* with MINUS, the sizes must be the same */
	/* with COLON, the types must be the same */

	TWORD t1, t2, t;
	int o, d2, d, s2, s;

	o = p->op;
	t = t1 = p->left->type;
	t2 = p->right->type;
	d = p->left->cdim;
	d2 = p->right->cdim;
	s = p->left->csiz;
	s2 = p->right->csiz;

	switch( o ){

	case ASSIGN:
	case RETURN:
	case CAST:
		{  break; }

	case MINUS:
		{  if( psize(p->left) != psize(p->right) ){
			uerror( "illegal pointer subtraction");
			}
		   break;
		   }
	case COLON:
		{  if( t1 != t2 ) uerror( "illegal types in :");
		   break;
		   }
	default:  /* must work harder: relationals or comparisons */

		if( !ISPTR(t1) ){
			t = t2;
			d = d2;
			s = s2;
			break;
			}
		if( !ISPTR(t2) ){
			break;
			}

		/* both are pointers */
		if( talign(t2,s2) < talign(t,s) ){
			t = t2;
			s = s2;
			}
		break;
		}

	p->left = makety( p->left, t, d, s );
	p->right = makety( p->right, t, d, s );
	if( o!=MINUS && !logop(o) ){

		p->type = t;
		p->cdim = d;
		p->csiz = s;
		}

	return(clocal(p));
	}

int tdebug = 0;

NODE *
tymatch(p)  register NODE *p; {

	/* satisfy the types of various arithmetic binary ops */

	/* rules are:
		if assignment, op, type of LHS
		if any float or doubles, make double
		if any longs, make long
		otherwise, make int
		if either operand is unsigned, the result is...
	*/

	register TWORD t1, t2, t, tu;
	register o, u;

	o = p->op;

	t1 = p->left->type;
	t2 = p->right->type;

	u = 0;
	if( ISUNSIGNED(t1) ){
		u = 1;
		t1 = DEUNSIGN(t1);
		}
	if( ISUNSIGNED(t2) ){
		u = 1;
		t2 = DEUNSIGN(t2);
		}

	if( ( t1 == CHAR || t1 == SHORT ) && o!= RETURN ) t1 = INT;
	if( t2 == CHAR || t2 == SHORT ) t2 = INT;

	if( t1==DOUBLE || t1==FLOAT || t2==DOUBLE || t2==FLOAT ) t = DOUBLE;
	else if( t1==LONG || t2==LONG ) t = LONG;
	else t = INT;

	if( asgop(o) ){
		tu = p->left->type;
		t = t1;
		}
	else {
		tu = (u && UNSIGNABLE(t))?ENUNSIGN(t):t;
		}

	/* because expressions have values that are at least as wide
	   as INT or UNSIGNED, the only conversions needed
	   are those involving FLOAT/DOUBLE, and those
	   from LONG to INT and ULONG to UNSIGNED */

	if( t != t1 ) p->left = makety( p->left, tu, 0, (int)tu );

	if( t != t2 || o==CAST ) p->right = makety( p->right, tu, 0, (int)tu );

	if( asgop(o) ){
		p->type = p->left->type;
		p->cdim = p->left->cdim;
		p->csiz = p->left->csiz;
		}
	else if( !logop(o) ){
		p->type = tu;
		p->cdim = 0;
		p->csiz = t;
		}

	if( tdebug ) printf( "tymatch(%o): %o %s %o => %o\n",p,t1,opst[o],t2,tu );

	return(p);
	}

NODE *
makety( p, t, d, s ) register NODE *p; TWORD t; {
	/* make p into type t by inserting a conversion */

	if( p->type == ENUMTY && p->op == ICON ) econvert(p);
	if( t == p->type ){
		p->cdim = d;
		p->csiz = s;
		return( p );
		}

	if( t & TMASK ){
		/* non-simple type */
		return( block( PCONV, p, NIL, t, d, s ) );
		}

	if( p->op == ICON ){
		if( t==DOUBLE||t==FLOAT ){
			p->op = FCON;
			if( ISUNSIGNED(p->type) ){
				p->dval = /* (unsigned CONSZ) */ p->lval;
				}
			else {
				p->dval = p->lval;
				}

			p->type = p->csiz = t;
			return( clocal(p) );
			}
		}

	return( block( SCONV, p, NIL, t, d, s ) );

	}

NODE *
block( o, l, r, t, d, s ) register NODE *l, *r; TWORD t; {

	register NODE *p;

	p = talloc();
	p->op = o;
	p->left = l;
	p->right = r;
	p->type = t;
	p->cdim = d;
	p->csiz = s;
	return(p);
	}

icons(p) register NODE *p; {
	/* if p is an integer constant, return its value */
	int val;

	if( p->op != ICON ){
		uerror( "constant expected");
		val = 1;
		}
	else {
		val = p->lval;
		if( val != p->lval ) uerror( "constant too big for cross-compiler" );
		}
	tfree( p );
	return(val);
	}

/* 	the intent of this table is to examine the
	operators, and to check them for
	correctness.

	The table is searched for the op and the
	modified type (where this is one of the
	types INT (includes char and short), LONG,
	DOUBLE (includes FLOAT), and POINTER

	The default action is to make the node type integer

	The actions taken include:
		PUN	  check for puns
		CVTL	  convert the left operand
		CVTR	  convert the right operand
		TYPL	  the type is determined by the left operand
		TYPR	  the type is determined by the right operand
		TYMATCH	  force type of left and right to match, by inserting conversions
		PTMATCH	  like TYMATCH, but for pointers
		LVAL	  left operand must be lval
		CVTO	  convert the op
		NCVT	  do not convert the operands
		OTHER	  handled by code
		NCVTR	  convert the left operand, not the right...

	*/

# define MINT 01  /* integer */
# define MDBI 02   /* integer or double */
# define MSTR 04  /* structure */
# define MPTR 010  /* pointer */
# define MPTI 020  /* pointer or integer */
# define MENU 040 /* enumeration variable or member */

opact( p )  NODE *p; {

	register mt12, mt1, mt2, o;

	mt12 = 0;

	switch( optype(o=p->op) ){

	case BITYPE:
		mt12=mt2 = moditype( p->right->type );
	case UTYPE:
		mt12 &= (mt1 = moditype( p->left->type ));

		}

	switch( o ){

	case NAME :
	case STRING :
	case ICON :
	case FCON :
	case CALL :
	case UNARY CALL:
	case UNARY MUL:
		{  return( OTHER ); }
	case UNARY MINUS:
		if( mt1 & MDBI ) return( TYPL );
		break;

	case COMPL:
		if( mt1 & MINT ) return( TYPL );
		break;

	case UNARY AND:
		{  return( NCVT+OTHER ); }
	case INIT:
	case CM:
	case NOT:
	case CBRANCH:
	case ANDAND:
	case OROR:
		return( 0 );

	case MUL:
	case DIV:
		if( mt12 & MDBI ) return( TYMATCH );
		break;

	case MOD:
	case AND:
	case OR:
	case ER:
		if( mt12 & MINT ) return( TYMATCH );
		break;

	case LS:
	case RS:
		if( mt12 & MINT ) return( TYPL+OTHER );
		break;

	case EQ:
	case NE:
	case LT:
	case LE:
	case GT:
	case GE:
		if( (mt1&MENU)||(mt2&MENU) ) return( PTMATCH+PUN+NCVT );
		if( mt12 & MDBI ) return( TYMATCH+CVTO );
		else if( mt12 & MPTR ) return( PTMATCH+PUN );
		else if( mt12 & MPTI ) return( PTMATCH+PUN );
		else break;

	case QUEST:
	case COMOP:
		if( mt2&MENU ) return( TYPR+NCVTR );
		return( TYPR );

	case STREF:
		return( NCVT+OTHER );

	case FORCE:
		return( TYPL );

	case COLON:
		if( mt12 & MENU ) return( NCVT+PUN+PTMATCH );
		else if( mt12 & MDBI ) return( TYMATCH );
		else if( mt12 & MPTR ) return( TYPL+PTMATCH+PUN );
		else if( (mt1&MINT) && (mt2&MPTR) ) return( TYPR+PUN );
		else if( (mt1&MPTR) && (mt2&MINT) ) return( TYPL+PUN );
		else if( mt12 & MSTR ) return( NCVT+TYPL+OTHER );
		break;

	case ASSIGN:
	case RETURN:
		if( mt12 & MSTR ) return( LVAL+NCVT+TYPL+OTHER );
	case CAST:
		if( mt12 & MDBI ) return( TYPL+LVAL+TYMATCH );
		else if( (mt1&MENU)||(mt2&MENU) ) return( LVAL+NCVT+TYPL+PTMATCH+PUN );
		else if( mt1 & MPTR ) return( LVAL+PTMATCH+PUN );
		else if( mt12 & MPTI ) return( TYPL+LVAL+TYMATCH+PUN );
		break;

	case ASG LS:
	case ASG RS:
		if( mt12 & MINT ) return( TYPL+LVAL+OTHER );
		break;

	case ASG MUL:
	case ASG DIV:
		if( mt12 & MDBI ) return( LVAL+TYMATCH );
		break;

	case ASG MOD:
	case ASG AND:
	case ASG OR:
	case ASG ER:
		if( mt12 & MINT ) return( LVAL+TYMATCH );
		break;

	case ASG PLUS:
	case ASG MINUS:
	case INCR:
	case DECR:
		if( mt12 & MDBI ) return( TYMATCH+LVAL );
		else if( (mt1&MPTR) && (mt2&MINT) ) return( TYPL+LVAL+CVTR );
		break;

	case MINUS:
		if( mt12 & MPTR ) return( CVTO+PTMATCH+PUN );
		if( mt2 & MPTR ) break;
	case PLUS:
		if( mt12 & MDBI ) return( TYMATCH );
		else if( (mt1&MPTR) && (mt2&MINT) ) return( TYPL+CVTR );
		else if( (mt1&MINT) && (mt2&MPTR) ) return( TYPR+CVTL );

		}
	uerror( "operands of %s have incompatible types", opst[o] );
	return( NCVT );
	}

moditype( ty ) TWORD ty; {

	switch( ty ){

	case ENUMTY:
	case MOETY:
		return( MENU );

	case STRTY:
	case UNIONTY:
		return( MSTR );

	case CHAR:
	case SHORT:
	case UCHAR:
	case USHORT:
		return( MINT|MDBI );
	case UNSIGNED:
	case ULONG:
	case INT:
	case LONG:
		return( MINT|MDBI|MPTI );
	case FLOAT:
	case DOUBLE:
		return( MDBI );
	default:
		return( MPTR|MPTI );

		}
	}

NODE *
doszof( p )  register NODE *p; {
	/* do sizeof p */
	int i;

	/* whatever is the meaning of this if it is a bitfield? */
	i = tsize( p->type, p->cdim, p->csiz )/SZCHAR;

	tfree(p);
	if( i <= 0 ) werror( "sizeof returns 0" );
	return( bcon( i ) );
	}

eprint( p, down, a, b ) register NODE *p; int *a, *b; {
	register ty;

	*a = *b = down+1;
	while( down > 1 ){
		printf( "\t" );
		down -= 2;
		}
	if( down ) printf( "    " );

	ty = optype( p->op );

	printf("%o) %s, ", p, opst[p->op] );
	if( ty == LTYPE ){
		printf( CONFMT, p->lval );
		printf( ", %d, ", p->rval );
		}
	tprint( p->type );
	printf( ", %d, %d\n", p->cdim, p->csiz );
	}

prtdcon( p ) register NODE *p; {
	int i;

	if( p->op == FCON ){
		locctr( DATA );
		defalign( ALDOUBLE );
		deflab( i = getlab() );
		fincode( p->dval, SZDOUBLE );
		p->lval = 0;
		p->rval = -i;
		p->type = DOUBLE;
		p->op = NAME;
		}
	}


int edebug = 0;
ecomp( p ) register NODE *p; {
	if( edebug ) fwalk( p, eprint, 0 );
	if( !reached ){
		werror( "statement not reached" );
		reached = 1;
		}
	p = optim(p);
	walkf( p, prtdcon );
	locctr( PROG );
	ecode( p );
	tfree(p);
	}

# ifdef STDPRTREE
# ifndef ONEPASS

prtree(p) register NODE *p; {

	register struct symtab *q;
	register ty;

# ifdef MYPRTREE
	MYPRTREE(p);  /* local action can be taken here; then return... */
#endif

	ty = optype(p->op);

	printf( "%d\t", p->op );

	if( ty == LTYPE ) {
		printf( CONFMT, p->lval );
		printf( "\t" );
		}
	if( ty != BITYPE ) {
		if( p->op == NAME || p->op == ICON ) printf( "0\t" );
		else printf( "%d\t", p->rval );
		}

	printf( "%o\t", p->type );

	/* handle special cases */

	switch( p->op ){

	case NAME:
	case ICON:
		/* print external name */
		if( p->rval == NONAME ) printf( "\n" );
		else if( p->rval >= 0 ){
			q = &stab[p->rval];
			printf(  "%s\n", exname(q->sname) );
			}
		else { /* label */
			printf( LABFMT, -p->rval );
			}
		break;

	case STARG:
	case STASG:
	case STCALL:
	case UNARY STCALL:
		/* print out size */
		/* use lhs size, in order to avoid hassles with the structure `.' operator */

		/* note: p->left not a field... */
		printf( CONFMT, (CONSZ) tsize( STRTY, p->left->cdim, p->left->csiz ) );
		printf( "\t%d\t\n", talign( STRTY, p->left->csiz ) );
		break;

	default:
		printf(  "\n" );
		}

	if( ty != LTYPE ) prtree( p->left );
	if( ty == BITYPE ) prtree( p->right );

	}

# else

p2tree(p) register NODE *p; {
	register ty;

# ifdef MYP2TREE
	MYP2TREE(p);  /* local action can be taken here; then return... */
# endif

	ty = optype(p->op);

	switch( p->op ){

	case NAME:
	case ICON:
		if( p->rval == NONAME ) p->name[0] = '\0';
		else if( p->rval >= 0 ){ /* copy name from exname */
			register char *cp;
			register i;
			cp = exname( stab[p->rval].sname );
			for( i=0; i<NCHNAM; ++i ) p->name[i] = *cp++;
			}
		else sprintf( p->name, LABFMT, -p->rval );
		break;

	case STARG:
	case STASG:
	case STCALL:
	case UNARY STCALL:
		/* set up size parameters */
		p->stsize = (tsize(STRTY,p->left->cdim,p->left->csiz)+SZCHAR-1)/SZCHAR;
		p->stalign = talign(STRTY,p->left->csiz)/SZCHAR;
		break;

	case REG:
		rbusy( p->rval, p->type );
	default:
		p->name[0] = '\0';
		}

	p->rall = NOPREF;

	if( ty != LTYPE ) p2tree( p->left );
	if( ty == BITYPE ) p2tree( p->right );
	}

# endif
# endif
