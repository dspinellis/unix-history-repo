    /*
     *	tCopy.c
     *
     *	functions to copy pi trees to pTrees
     */

#include	"whoami"

#ifdef PTREE

#include	"0.h"

#include	"tree.h"

    /*
     *	tCopy
     *	a mongo switch statement to farm out the actual copying 
     *	to the appropriate routines.
     *	given a pointer to a pi tree branch, it returns a pPointer to
     *	a pTree copy of that branch.
     */
pPointer
tCopy( node )
    int	*node;
    {

	if ( node == NIL )
	    return pNIL;
	switch ( node[ 0 ] ) {
	    case T_PROG:	
	    case T_PDEC:
	    case T_FDEC:	
		return PorFCopy( node );
	    case T_TYPTR:
		return PtrTCopy( node );
	    case T_TYPACK:
		return PackTCopy( node );
	    case T_TYSCAL:
		return EnumTCopy( node );
	    case T_TYRANG:
		return RangeTCopy( node );
	    case T_TYARY:
		return ArrayTCopy( node );
	    case T_TYFILE:
		return FileTCopy( node );
	    case T_TYSET:
		return SetTCopy( node );
	    case T_TYREC:
		return RecTCopy( node );
	    case T_FLDLST:
		return FldlstCopy( node );
	    case T_RFIELD:
		return FieldCopy( node );
	    case T_TYVARPT:
		return VarntCopy( node );
	    case T_TYVARNT:
		return VCaseCopy( node );
	    case T_CSTAT:
		return CasedCopy( node );
	    case T_PVAL:
	    case T_PVAR:
		return ParamCopy( node );
	    case T_CSTRNG:
		return sCopy( node[1] );
	    case T_STRNG:
		return sCopy( node[2] );
	    case T_PLUSC:
	    case T_PLUS:
	    case T_MINUSC:
	    case T_MINUS:
	    case T_NOT:
		return UnOpCopy( node );
	    case T_ID:
		return ThreadSymbol( node[1] );
	    case T_TYID:
		return ThreadSymbol( node[2] );
	    case T_CINT:
	    case T_CBINT:
		return iCopy( node[1] );
	    case T_INT:
	    case T_BINT:
		return iCopy( node[2] );
	    case T_CFINT:
		return fCopy( node[1] );
	    case T_FINT:
		return fCopy( node[2] );
	    case T_LISTPP:
		return ListCopy( node );
	    case T_PCALL:
		return PCallCopy( node );
	    case T_BLOCK:
	    case T_BSTL:
		return ListCopy( node[2] );
	    case T_CASE:
		return CaseSCopy( node );
	    case T_WITH:
		return WithCopy( node );
	    case T_WHILE:
		return WhileCopy( node );
	    case T_REPEAT:
		return RepeatCopy( node );
	    case T_FORU:
	    case T_FORD:
		return ForCopy( node );
	    case T_IF:
	    case T_IFEL:
		return IfCopy( node );
	    case T_GOTO:
		return GotoCopy( node );
	    case T_LABEL:
		return LabelCopy( node );
	    case T_ASRT:
		return AssertCopy( node );
	    case T_ASGN:
		return AssignCopy( node );
	    case T_NIL:
		return NilCopy( node );
	    case T_FCALL:
		return FCallCopy( node );
	    case T_CSET:
		return SetCopy( node );
	    case T_RANG:
		return RangeCopy( node );
	    case T_VAR:
		return VarCopy( node );
	    case T_ARY:
		return SubscCopy( node );
	    case T_FIELD:
		return SelCopy( node );
	    case T_PTR:
		return PtrCopy( node );
	    case T_EQ:
	    case T_LT:
	    case T_GT:
	    case T_LE:
	    case T_GE:
	    case T_NE:
	    case T_IN:
	    case T_ADD:
	    case T_SUB:
	    case T_MULT:
	    case T_DIVD:
	    case T_DIV:
	    case T_MOD:
	    case T_OR:
	    case T_AND:
		return BinOpCopy( node );
	    case T_WEXP:
		return WidthCopy( node );
	    default:
		panic("tCopy");
	}
    }


    /*
     *	copy a list of nodes into ListNodes
     *	(with a hack for appending one list to another
     *	    for example: labelled statements)
     *	listnode[0]	T_LISTPP
     *		[1]	"list_element"
     *		[2]	"list_next"
     */
pPointer
ListCopy( listnode )
    int	*listnode;
    {
	pPointer	First;
	pPointer	After;
	int		*listp;
	pPointer	Item;
	pPointer	List;
	pPointer	Furthur;

	First = pNIL;
	After = pNIL;
	for ( listp = listnode ; listp != NIL ; listp = (int *) listp[2] ) {
	    List = pNewNode( ListTAG , sizeof( struct ListNode ) );
	    if ( First == pNIL )
		First = List;
	    Item = tCopy( listp[1] );
	    pDEF( List ).ListItem = Item;
	    pDEF( List ).ListDown = pNIL;
	    pDEF( List ).ListUp = After;
	    if ( After != pNIL )
		pDEF( After ).ListDown = List;
	    After = List;
	    /*
	     *	if ListItem is a ListNode whose ListUp is non-pNIL
	     *	append that list to this list, using that ListUp
	     *	as an additional ListItem.
	     */
	    Furthur = Item;
	    if (  Furthur != pNIL
	       && pTAG( Furthur ) == ListTAG
	       && pUSE( Furthur ).ListUp != pNIL ) {
		Item = pUSE( Furthur ).ListUp;
		pDEF( List ).ListItem = Item;
		pDEF( Furthur ).ListUp = List;
		pDEF( List ).ListDown = Furthur;
		do {
		    After = Furthur;
		    Furthur = pUSE( After ).ListDown;
		} while ( Furthur != pNIL );
	    }
	}
	return First;
    }

    /*
     *	ListAppend
     *	append a random item to the end of a list
     *	(with a hack for appending one list to another
     *	 e.g. labelled statments)
     */
pPointer
ListAppend( list , item )
    pPointer	list;
    pPointer	item;
    {
	pPointer	List = pNewNode( ListTAG , sizeof( struct ListNode ) );
	pPointer	First;
	pPointer	After;
	pPointer	Furthur;

	pDEF( List ).ListItem = item;
	pDEF( List ).ListDown = pNIL;
	First = After = list;
	if ( First == pNIL ) {
		First = List;
	} else {
		while ( ( Furthur = pUSE( After ).ListDown ) != pNIL )
		    After = Furthur;
		pDEF( After ).ListDown = List;
	}
	pDEF( List ).ListUp = After;
	/*
	 *	if item is a ListNode whose ListUp is non-pNIL
	 *	append that list to this list, using that ListUp
	 *	as an additional ListItem.
	 */
	Furthur = item;
	if (  Furthur != pNIL
	   && pTAG( Furthur ) == ListTAG
	   && pUSE( Furthur ).ListUp != pNIL ) {
	    pDEF( List ).ListDown = Furthur;
	    pDEF( List ).ListItem = pUSE( Furthur ).ListUp;
	    pDEF( Furthur ).ListUp = List;
	}
	return First;
    }

    /*
     *	iCopy
     *	copy an integer (string) to an IntNode
     */
pPointer
iCopy( intstring )
    char *intstring;
    {
	pPointer	Int = pNewNode( IntTAG , sizeof( struct IntNode ) );

	pDEF( Int ).IntValue = atol( intstring );
	return Int;
    }

    /*
     *	fCopy
     *	copy a float (string) to a RealNode
     */
pPointer
fCopy( realstring )
    char *realstring;
    {
	pPointer	Real = pNewNode( RealTAG , sizeof( struct RealNode ) );

	pDEF( Real ).RealValue = atof( realstring );
	return Real;
    }

    /*
     *	sCopy
     *	copy a string to a StringNode
     */
pPointer
sCopy( string )
    char *string;
    {
	pPointer	String;

	if ( string == NIL )
	    return pNIL;
	String = pNewNode( StringTAG , strlen( string ) + 1 );
	strcpy( pDEF( String ).StringValue , string );
	return String;
    }

#endif PTREE
