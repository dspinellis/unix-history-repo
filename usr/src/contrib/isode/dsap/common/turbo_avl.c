/* turbo_avl.c */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/turbo_avl.c,v 7.1 91/02/22 09:20:36 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/turbo_avl.c,v 7.1 91/02/22 09:20:36 mrose Interim $
 *
 *
 * $Log:	turbo_avl.c,v $
 * Revision 7.1  91/02/22  09:20:36  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/12/19  09:41:32  mrose
 * *** empty log message ***
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <sys/types.h>

#include "manifest.h"
#include "quipu/util.h"
#include "quipu/attr.h"
#include "quipu/entry.h"
#include "quipu/turbo.h"

extern LLog * log_dsap;


#define ROTATERIGHT(x)	{ \
	Avlnode *tmp;\
	if ( *x == NULL || (*x)->avl_left == NULL ) {\
		(void) printf("RR error\n"); exit(1); \
	}\
	tmp = (*x)->avl_left;\
	(*x)->avl_left = tmp->avl_right;\
	tmp->avl_right = *x;\
	*x = tmp;\
}
#define ROTATELEFT(x)	{ \
	Avlnode *tmp;\
	if ( *x == NULL || (*x)->avl_right == NULL ) {\
		(void) printf("RL error\n"); exit(1); \
	}\
	tmp = (*x)->avl_right;\
	(*x)->avl_right = tmp->avl_left;\
	tmp->avl_left = *x;\
	*x = tmp;\
}


static ravl_insert( iroot, data, taller, fcmp, fdup, depth )
Avlnode 	**iroot;
caddr_t		data;
int		*taller;
IFP		fcmp;		/* comparison function */
IFP		fdup;		/* function to call for duplicates */
int		depth;
{
	int	rc, cmp, tallersub;
	Avlnode	*l, *r;

	if ( *iroot == 0 ) {
		*iroot = ( Avlnode * ) malloc( sizeof( Avlnode ) );
		(*iroot)->avl_left = 0;
		(*iroot)->avl_right = 0;
		(*iroot)->avl_bf = 0;
		(*iroot)->avl_data = data;
		*taller = 1;
		return( OK );
	}

	cmp = (*fcmp)( data, (*iroot)->avl_data );

	/* equal - duplicate name */
	if ( cmp == 0 ) {
		*taller = 0;
		return( (*fdup)( (*iroot)->avl_data, data ) );
	}

	/* go right */
	else if ( cmp > 0 ) {
		rc = ravl_insert( &((*iroot)->avl_right), data, &tallersub,
		   fcmp, fdup, depth );
		if ( tallersub )
			switch ( (*iroot)->avl_bf ) {
			case LH	: /* left high - balance is restored */
				(*iroot)->avl_bf = EH;
				*taller = 0;
				break;
			case EH	: /* equal height - now right heavy */
				(*iroot)->avl_bf = RH;
				*taller = 1;
				break;
			case RH	: /* right heavy to start - right balance */
				r = (*iroot)->avl_right;
				switch ( r->avl_bf ) {
				case LH	: /* double rotation left */
					l = r->avl_left;
					switch ( l->avl_bf ) {
					case LH	: (*iroot)->avl_bf = EH;
						  r->avl_bf = RH;
						  break;
					case EH	: (*iroot)->avl_bf = EH;
						  r->avl_bf = EH;
						  break;
					case RH	: (*iroot)->avl_bf = LH;
						  r->avl_bf = EH;
						  break;
					}
					l->avl_bf = EH;
					ROTATERIGHT( (&r) )
					(*iroot)->avl_right = r;
					ROTATELEFT( iroot )
					*taller = 0;
					break;
				case EH	: /* This should never happen */
					break;
				case RH	: /* single rotation left */
					(*iroot)->avl_bf = EH;
					r->avl_bf = EH;
					ROTATELEFT( iroot )
					*taller = 0;
					break;
				}
				break;
			}
		else
			*taller = 0;
	}

	/* go left */
	else {
		rc = ravl_insert( &((*iroot)->avl_left), data, &tallersub,
		   fcmp, fdup, depth );
		if ( tallersub )
			switch ( (*iroot)->avl_bf ) {
			case LH	: /* left high to start - left balance */
				l = (*iroot)->avl_left;
				switch ( l->avl_bf ) {
				case LH	: /* single rotation right */
					(*iroot)->avl_bf = EH;
					l->avl_bf = EH;
					ROTATERIGHT( iroot )
					*taller = 0;
					break;
				case EH	: /* this should never happen */
					break;
				case RH	: /* double rotation right */
					r = l->avl_right;
					switch ( r->avl_bf ) {
					case LH	: (*iroot)->avl_bf = RH;
						  l->avl_bf = EH;
						  break;
					case EH	: (*iroot)->avl_bf = EH;
						  l->avl_bf = EH;
						  break;
					case RH	: (*iroot)->avl_bf = EH;
						  l->avl_bf = LH;
						  break;
					}
					r->avl_bf = EH;
					ROTATELEFT( (&l) )
					(*iroot)->avl_left = l;
					ROTATERIGHT( iroot )
					*taller = 0;
					break;
				}
				break;
			case EH	: /* equal height - now left heavy */
				(*iroot)->avl_bf = LH;
				*taller = 1;
				break;
			case RH	: /* right high - balance is restored */
				(*iroot)->avl_bf = EH;
				*taller = 0;
				break;
			}
		else
			*taller = 0;
	}

	return( rc );
}

/*
 * avl_insert -- insert a node containing data data into the avl tree
 * with root root.  fcmp is a function to call to compare the data portion
 * of two nodes.  it should take two arguments and return <, >, or == 0,
 * depending on whether its first argument is <, >, or == its second
 * argument (like strcmp, e.g.).  fdup is a function to call when a duplicate
 * node is inserted.  it should return OK, or NOTOK and its return value
 * will be the return value from avl_insert in the case of a duplicate node.
 * the function will be called with the original node's data as its first
 * argument and with the incoming duplicate node's data as its second
 * argument.  this could be used, for example, to keep a count with each
 * node.
 *
 * NOTE: this routine may malloc memory
 */

avl_insert( root, data, fcmp, fdup )
Avlnode	**root;
caddr_t	data;
IFP	fcmp;
IFP	fdup;
{
	int	taller;

	return( ravl_insert( root, data, &taller, fcmp, fdup, 0 ) );
}

/* called from delete when root's right subtree has been shortened */
static right_balance( root )
Avlnode	**root;
{
	int	shorter;
	Avlnode	*r, *l;

	switch( (*root)->avl_bf ) {
	case RH:	/* was right high - equal now */
		(*root)->avl_bf = EH;
		shorter = 1;
		break;
	case EH:	/* was equal - left high now */
		(*root)->avl_bf = LH;
		shorter = 0;
		break;
	case LH:	/* was right high - balance */
		l = (*root)->avl_left;
		switch ( l->avl_bf ) {
		case RH	: /* double rotation left */
			r = l->avl_right;
			switch ( r->avl_bf ) {
			case RH	:
				(*root)->avl_bf = EH;
				l->avl_bf = LH;
				break;
			case EH	:
				(*root)->avl_bf = EH;
				l->avl_bf = EH;
				break;
			case LH	:
				(*root)->avl_bf = RH;
				l->avl_bf = EH;
				break;
			}
			r->avl_bf = EH;
			ROTATELEFT( (&l) )
			(*root)->avl_left = l;
			ROTATERIGHT( root )
			shorter = 1;
			break;
		case EH	: /* right rotation */
			(*root)->avl_bf = LH;
			l->avl_bf = RH;
			ROTATERIGHT( root );
			shorter = 0;
			break;
		case LH	: /* single rotation right */
			(*root)->avl_bf = EH;
			l->avl_bf = EH;
			ROTATERIGHT( root )
			shorter = 1;
			break;
		}
		break;
	}

	return( shorter );
}

/* called from delete when root's left subtree has gotten shorter */
static left_balance( root )
Avlnode	**root;
{
	int	shorter;
	Avlnode	*r, *l;

	switch( (*root)->avl_bf ) {
	case LH:	/* was left high - equal now */
		(*root)->avl_bf = EH;
		shorter = 1;
		break;
	case EH:	/* was equal - right high now */
		(*root)->avl_bf = RH;
		shorter = 0;
		break;
	case RH:	/* was right high - balance */
		r = (*root)->avl_right;
		switch ( r->avl_bf ) {
		case LH	: /* double rotation left */
			l = r->avl_left;
			switch ( l->avl_bf ) {
			case LH	:
				(*root)->avl_bf = EH;
				r->avl_bf = RH;
				break;
			case EH	:
				(*root)->avl_bf = EH;
				r->avl_bf = EH;
				break;
			case RH	:
				(*root)->avl_bf = LH;
				r->avl_bf = EH;
				break;
			}
			l->avl_bf = EH;
			ROTATERIGHT( (&r) )
			(*root)->avl_right = r;
			ROTATELEFT( root )
			shorter = 1;
			break;
		case EH	: /* single rotation left */
			(*root)->avl_bf = RH;
			r->avl_bf = LH;
			ROTATELEFT( root );
			shorter = 0;
			break;
		case RH	: /* single rotation left */
			(*root)->avl_bf = EH;
			r->avl_bf = EH;
			ROTATELEFT( root )
			shorter = 1;
			break;
		}
		break;
	}

	return( shorter );
}

static caddr_t ravl_delete( root, data, fcmp, shorter )
Avlnode	**root;
caddr_t	data;
IFP	fcmp;
int	*shorter;
{
	int	shortersubtree = 0;
	int	cmp;
	caddr_t	savedata;
	Avlnode	*minnode, *savenode;

	if ( *root == NULLAVL )
		return( 0 );

	cmp = (*fcmp)( data, (*root)->avl_data );

	/* found it! */
	if ( cmp == 0 ) {
		savenode = *root;
		savedata = savenode->avl_data;

		/* simple cases: no left child */
		if ( (*root)->avl_left == 0 ) {
			*root = (*root)->avl_right;
			*shorter = 1;
			free( (char *) savenode );
			return( savedata );
		/* no right child */
		} else if ( (*root)->avl_right == 0 ) {
			*root = (*root)->avl_left;
			*shorter = 1;
			free( (char *) savenode );
			return( savedata );
		}

		/* 
		 * avl_getmin will return to us the smallest node greater
		 * than the one we are trying to delete.  deleting this node
		 * from the right subtree is guaranteed to end in one of the
		 * simple cases above.
		 */

		minnode = (*root)->avl_right;
		while ( minnode->avl_left != NULLAVL )
			minnode = minnode->avl_left;

		/* swap the data */
		(*root)->avl_data = minnode->avl_data;
		minnode->avl_data = savedata;

		savedata = ravl_delete( &(*root)->avl_right, data, fcmp,
		    &shortersubtree );

		if ( shortersubtree )
			*shorter = right_balance( root );
		else
			*shorter = 0;
	/* go left */
	} else if ( cmp < 0 ) {
		if ( (savedata = ravl_delete( &(*root)->avl_left, data, fcmp,
		    &shortersubtree )) == 0 ) {
			*shorter = 0;
			return( 0 );
		}

		/* left subtree shorter? */
		if ( shortersubtree )
			*shorter = left_balance( root );
		else
			*shorter = 0;
	/* go right */
	} else {
		if ( (savedata = ravl_delete( &(*root)->avl_right, data, fcmp,
		    &shortersubtree )) == 0 ) {
			*shorter = 0;
			return( 0 );
		}

		if ( shortersubtree ) 
			*shorter = right_balance( root );
		else
			*shorter = 0;
	}

	return( savedata );
}

caddr_t avl_delete( root, data, fcmp )
Avlnode	**root;
caddr_t	data;
IFP	fcmp;
{
	int	shorter;

	return( ravl_delete( root, data, fcmp, &shorter ) );
}

avl_inapply( root, fn, arg, stopflag )
Avlnode	*root;
IFP	fn;
caddr_t	arg;
int	stopflag;
{
	if ( root == 0 )
		return( AVL_NOMORE );

	if ( root->avl_left != 0 )
		if ( avl_inapply( root->avl_left, fn, arg, stopflag ) 
		    == stopflag )
			return( stopflag );

	if ( (*fn)( root->avl_data, arg ) == stopflag )
		return( stopflag );

	if ( root->avl_right == 0 )
		return( AVL_NOMORE );
	else
		return( avl_inapply( root->avl_right, fn, arg, stopflag ) );
}

avl_postapply( root, fn, arg, stopflag )
Avlnode	*root;
IFP	fn;
caddr_t	arg;
int	stopflag;
{
	if ( root == 0 )
		return( AVL_NOMORE );

	if ( root->avl_left != 0 )
		if ( avl_postapply( root->avl_left, fn, arg, stopflag ) 
		    == stopflag )
			return( stopflag );

	if ( root->avl_right != 0 )
		if ( avl_postapply( root->avl_right, fn, arg, stopflag ) 
		    == stopflag )
			return( stopflag );

	return( (*fn)( root->avl_data, arg ) );
}

avl_preapply( root, fn, arg, stopflag )
Avlnode	*root;
IFP	fn;
caddr_t	arg;
int	stopflag;
{
	if ( root == 0 )
		return( AVL_NOMORE );

	if ( (*fn)( root->avl_data, arg ) == stopflag )
		return( stopflag );

	if ( root->avl_left != 0 )
		if ( avl_preapply( root->avl_left, fn, arg, stopflag ) 
		    == stopflag )
			return( stopflag );

	if ( root->avl_right == 0 )
		return( AVL_NOMORE );
	else
		return( avl_preapply( root->avl_right, fn, arg, stopflag ) );
}

/*
 * avl_apply -- avl tree root is traversed, function fn is called with
 * arguments arg and the data portion of each node.  if fn returns stopflag,
 * the traversal is cut short, otherwise it continues.  Do not use -6 as
 * a stopflag.
 */

avl_apply( root, fn, arg, stopflag, type )
Avlnode	*root;
IFP	fn;
caddr_t	arg;
int	stopflag;
int	type;
{
	switch ( type ) {
	case AVL_INORDER:
		return( avl_inapply( root, fn, arg, stopflag ) );
	case AVL_PREORDER:
		return( avl_preapply( root, fn, arg, stopflag ) );
	case AVL_POSTORDER:
		return( avl_postapply( root, fn, arg, stopflag ) );
	default:
		DLOG( log_dsap, LLOG_EXCEPTIONS, ("Invalid traversal type %d",
		    type) );
		return( NOTOK );
	}

	/* NOTREACHED */
}

/*
 * avl_prefixapply - traverse avl tree root, applying function fprefix
 * to any nodes that match.  fcmp is called with data as its first arg
 * and the current node's data as its second arg.  it should return
 * 0 if they match, < 0 if data is less, and > 0 if data is greater.
 * the idea is to efficiently find all nodes that are prefixes of
 * some key...
 */

avl_prefixapply( root, data, fmatch, marg, fcmp, carg )
Avlnode	*root;
caddr_t	data;
IFP	fmatch;
caddr_t	marg;
IFP	fcmp;
caddr_t	carg;
{
	int	cmp;

	if ( root == 0 )
		return;

	cmp = (*fcmp)( data, root->avl_data, carg );
	if ( cmp == 0 ) {
		(*fmatch)( root->avl_data, marg );
		if ( root->avl_left != 0 )
			avl_prefixapply( root->avl_left, data, fmatch, marg,
			    fcmp, carg );
		if ( root->avl_right != 0 )
			avl_prefixapply( root->avl_right, data, fmatch, marg,
			    fcmp, carg );
	} else if ( cmp < 0 ) {
		if ( root->avl_left != 0 )
			avl_prefixapply( root->avl_left, data, fmatch, marg,
			    fcmp, carg );
	} else {
		if ( root->avl_right != 0 )
			avl_prefixapply( root->avl_right, data, fmatch, marg,
			    fcmp, carg );
	}

	return;
}

/*
 * avl_free -- traverse avltree root, freeing the memory it is using.
 * the dfree() is called to free the data portion of each node.  The
 * number of items actually freed is returned.
 */

avl_free( root, dfree )
Avlnode	*root;
IFP	dfree;
{
	int	nleft, nright;

	if ( root == 0 )
		return( 0 );

	nleft = nright = 0;
	if ( root->avl_left != 0 )
		nleft = avl_free( root->avl_left, dfree );

	if ( root->avl_right != 0 )
		nright = avl_free( root->avl_right, dfree );

	if ( dfree )
		(*dfree)( root->avl_data );

	return( nleft + nright + 1 );
}

/*
 * avl_find -- search avltree root for a node with data data.  the function
 * cmp is used to compare things.  it is called with data as its first arg 
 * and the current node data as its second.  it should return 0 if they match,
 * < 0 if arg1 is less than arg2 and > 0 if arg1 is greater than arg2.
 */

caddr_t avl_find( root, data, fcmp )
Avlnode	*root;
caddr_t	data;
IFP	fcmp;
{
	int	cmp;

	while ( root != 0 && (cmp = (*fcmp)( data, root->avl_data )) != 0 ) {
		if ( cmp < 0 )
			root = root->avl_left;
		else
			root = root->avl_right;
	}

	return( root ? root->avl_data : 0 );
}

static caddr_t	*avl_list;
static int	avl_maxlist;
static int	avl_nextlist;

#define AVL_GRABSIZE	100

/* ARGSUSED 1 */
static avl_buildlist( data, arg )
caddr_t	data;
int	arg;
{
	static int	slots;

	if ( avl_list == (caddr_t *) 0 ) {
		avl_list = (caddr_t *) malloc(AVL_GRABSIZE * sizeof(caddr_t));
		slots = AVL_GRABSIZE;
		avl_maxlist = 0;
	} else if ( avl_maxlist == slots ) {
		slots += AVL_GRABSIZE;
		avl_list = (caddr_t *) realloc( (char *) avl_list,
		    (unsigned) slots * sizeof(caddr_t));
	}

	avl_list[ avl_maxlist++ ] = data;

	return( 0 );
}

caddr_t avl_getfirst( root )
Avlnode	*root;
{
	if ( avl_list ) {
		free( (char *) avl_list);
		avl_list = (caddr_t *) 0;
	}
	avl_maxlist = 0;
	avl_nextlist = 0;

	if ( root == 0 )
		return( 0 );

	(void) avl_apply( root, avl_buildlist, (caddr_t) 0, -1, AVL_INORDER );

	return( avl_list[ avl_nextlist++ ] );
}

caddr_t avl_getnext()
{
	if ( avl_list == 0 )
		return( 0 );

	if ( avl_nextlist == avl_maxlist ) {
		free( (caddr_t) avl_list);
		avl_list = (caddr_t *) 0;
		return( 0 );
	}

	return( avl_list[ avl_nextlist++ ] );
}


avl_dup_error()
{
	return( NOTOK );
}
