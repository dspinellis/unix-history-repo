    /*
     *	pCopy.c
     *	a collection of functions
     *	to copy pi's tree nodes
     *	to pTree pNodes.
     */

#include	"whoami"

#ifdef PTREE

#include	"0.h"

    /*
     *	get all the pi tree tags
     */
#include	"tree.h"

    /*	constructs a ThreadNode to a declaration
     *	given an identifier char * symbol
     */
pPointer
ThreadSymbol( symbol )
    char *symbol;
    {
	pPointer Thread;
	struct nl *symbp = nllook( symbol );

	if ( symbp == NIL || symbp -> inTree == pNIL )
	    return pNIL;
	Thread = pNewNode( ThreadTAG , sizeof( struct ThreadNode ) );
	pDEF( Thread ).ThreadPointer = symbp -> inTree;
	return Thread;
    }

    /*	constructs a ThreadNode to a declaration
     *	given a namelist pointer
     */
pPointer
ThreadName( nlp )
    struct nl *nlp;
    {
	pPointer Thread;

	if ( nlp == NIL  || nlp -> inTree == pNIL )
	    return pNIL;
	Thread = pNewNode( ThreadTAG , sizeof( struct ThreadNode ) );
	pDEF( Thread ).ThreadPointer = nlp -> inTree;
	return Thread;
    }

    /*
     *	copies a unary operator node to an appropriate pNode.
     *	unary operators come in two flavors,
     *	those with known constant operands (T_PLUSC and T_MINUSC),
     *	and those (T_PLUS, T_MINUS, and T_NOT)
     *	which carry around with them whether they have a constant operand
     *	unop	[0]	T_PLUSC, T_MINUSC  or   [0]	T_PLUS, T_MINUS, T_NOT
     *		[1]	expression		[1]	constant operand?
     *						[2]	expression
     */
pPointer
UnOpCopy( unop )
    int *unop;
    {
	pPointer    UnOp;
	pPointer    Expr;

	switch ( unop[0] ) {
	    case T_PLUSC:
	    case T_PLUS:
		UnOp = pNewNode( PlusTAG , sizeof( struct PlusNode ) );
		break;
	    case T_MINUSC:
	    case T_MINUS:
		UnOp = pNewNode( MinusTAG , sizeof( struct MinusNode ) );
		break;
	    case T_NOT:
		UnOp = pNewNode( NotTAG , sizeof( struct NotNode ) );
		break;
	    default:
		panic("UnOpCopy");
	}
	switch ( unop[0] ) {
	    case T_PLUSC:
	    case T_MINUSC:
		Expr = tCopy( unop[1] );
		break;
	    case T_PLUS:
	    case T_MINUS:
	    case T_NOT:
		Expr = tCopy( unop[2] );
		break;
	    default:
		panic("UnOpCopy");
	}
	pDEF( UnOp ).UnOpExpr = Expr;
	return UnOp;
    }

    /*
     *	returns an empty PtrTNode
     *	to be filled in by foredecl
     *	typtr	[0]	T_TYPTR
     *		[1]	lineof "^"
     *		[2][0]	T_ID
     *		   [1]	type name
     */
pPointer
PtrTCopy( typtr )
    int	*typtr;
    {
	pPointer    PtrT = pNewNode( PtrTTAG , sizeof( struct PtrTNode ) );

	pDEF( PtrT ).PtrTType = pNIL;
	return PtrT;
    }

    /*
     *	copy a PACKED declaration to a PackTNode
     *	typack	[0]	T_TYPACK
     *		[1]	lineof "packed"
     *		[2]	structured type
     */
pPointer
PackTCopy( typack )
    int	*typack;
    {
	pPointer    PackT = pNewNode( PackTTAG , sizeof( struct PackTNode ) );
	pPointer    Type = tCopy( typack[2] );

	pDEF( PackT ).PackTType = Type;
	return PackT;
    }

    /*
     *	copy a T_TYRANG node to a RangeTNode
     *	tyrang	[0]	T_TYRANG
     *		[1]	lineof ".."
     *		[2]	lower const
     *		[3]	upper const
     */
pPointer
RangeTCopy( tyrang )
    int *tyrang;
    {
	pPointer RangeT = pNewNode( RangeTTAG , sizeof ( struct RangeTNode ) );
	pPointer Lower = tCopy( tyrang[2] );
	pPointer Upper = tCopy( tyrang[3] );

	pDEF( RangeT ).RangeTLower = Lower;
	pDEF( RangeT ).RangeTUpper = Upper;
	return RangeT;
    }

    /*
     *	ArrayTCopy
     *	copy a T_TYARY node to an ArrayTNode
     *	tyary	[0]	T_TYARY
     *		[1]	lineof "array"
     *		[2]	simple_type_list
     *		[3]	type
     */
pPointer
ArrayTCopy( tyary )
    int *tyary;
    {
	pPointer ArrayT = pNewNode( ArrayTTAG , sizeof( struct ArrayTNode ) );
	pPointer Dims = tCopy( tyary[2] );
	pPointer Type = tCopy( tyary[3] );

	pDEF( ArrayT ).ArrayTDims = Dims;
	pDEF( ArrayT ).ArrayTType = Type;
	return ArrayT;
    }

    /*
     *	copy a T_TYFILE node to a FileTNode
     *	tyfile	[0]	T_TYFILE
     *		[1]	lineof file
     *		[2]	type
     */
pPointer
FileTCopy( tyfile )
    int *tyfile;
    {
	pPointer FileT = pNewNode( FileTTAG , sizeof( struct FileTNode ) );
	pPointer Type = tCopy( tyfile[2] );

	pDEF( FileT ).FileTType = Type;
	return FileT;
    }

    /*
     *	copy a T_TYSET node of a SetTNode
     *	tyset	[0]	T_TYSET
     *		[1]	lineof "set"
     *		[2]	simple type
     */
pPointer
SetTCopy( tyset )
    int *tyset;
    {
	pPointer SetT = pNewNode( SetTTAG , sizeof( struct SetTNode ) );
	pPointer Type = tCopy( tyset[2] );

	pDEF( SetT ).SetTType = Type;
	return SetT;
    }

    /*
     *	copy an extended T_TYREC node to a RecTNode
     *	tyrec	[0]	T_TYREC
     *		[1]	lineof field_list
     *		[2]	pointer to field_list
     *		[3]	>>pTree extension<< struct nl * to record in namelist
     *	sets extern inrecord so fields know in which record they are.
     *	saved previous inrecord to deal with nested records.
     */
pPointer
RecTCopy( tyrec )
    int *tyrec;
    {
	extern struct nl *inrecord;
	struct nl *saverecord;
	pPointer RecT = pNewNode( RecTTAG , sizeof( struct RecTNode ) );
	pPointer Fldlst;

	saverecord = inrecord;
	inrecord = (struct nl *) tyrec[3];
	Fldlst = tCopy( tyrec[2] );
	pDEF( RecT ).RecTFldlst = Fldlst;
	inrecord = saverecord;
	return RecT;
    }

    /*
     *	copy an extended T_FLDLST node to a FldlstNode
     *	fldlst	[0]	T_FLDLST
     *		[1]	0
     *		[2]	fixed part (list of T_RFIELDs)
     *		[3]	variant part (pointer to T_TYVARPT node)
     */
pPointer
FldlstCopy( fldlst )
    int *fldlst;
    {
	pPointer Fldlst = pNewNode( FldlstTAG , sizeof( struct FldlstNode ) );
	pPointer Fixed;
	pPointer Varpt;

	Fixed = tCopy( fldlst[2] );
	Varpt = tCopy( fldlst[3] );
	pDEF( Fldlst ).FldlstFixed = Fixed;
	pDEF( Fldlst ).FldlstVariant = Varpt;
	return Fldlst;
    }

    /*
     *	copies a T_TYVARNT to a VCaseNode
     *	tyvarnt	[0]	T_TYVARNT
     *		[1]	lineof ":"
     *		[2]	constant list
     *		[3]	nil or &(T_FLDLST node)
     */
pPointer
VCaseCopy( tyvarnt )
    int	*tyvarnt;
    {
	pPointer VCase = pNewNode( VCaseTAG , sizeof( struct VCaseNode ) );
	pPointer Consts = tCopy( tyvarnt[2] );
	pPointer Rec = tCopy( tyvarnt[3] );

	pDEF( VCase ).VCaseConsts = Consts;
	pDEF( VCase ).VCaseRec = Rec;
	return VCase;
    }

    /*
     *	copy a T_CSTAT to a CasedNode
     *	cstat	[0]	T_CSTAT
     *		[1]	lineof ":"
     *		[2]	constant list
     *		[3]	statement
     */
pPointer
CasedCopy( cstat )
    int	*cstat;
    {
	pPointer Cased = pNewNode( CasedTAG , sizeof( struct CasedNode ) );
	pPointer Label = tCopy( cstat[2] );
	pPointer Stat = tCopy( cstat[3] );

	pDEF( Cased ).CasedLabel = Label;
	pDEF( Cased ).CasedStat = Stat;
	return Cased;
    }

    /*
     *	copy a T_LABEL to a ListNode
     *	whose ListUp is a LabelNode and 
     *	whose ListDown is the labelled statement
     *	(cf. the hack in ListAppend )
     *	label	[0]	T_LABEL
     *		[1]	lineof :
     *		[2]	hash of integer label
     *		[3]	statement
     */
pPointer
LabelCopy( label )
    int	*label;
    {
	pPointer List;
	pPointer Label = pNewNode( LabelTAG , sizeof( struct LabelNode ) );
	pPointer Stat;

	Stat = tCopy( label[3] );
	pDEF( Label ).LabelLabel = nllook( label[2] ) -> inTree;
	List = ListAppend( pNIL , Stat );
	pDEF( List ).ListUp = Label;
	return List;
    }

    /*
     *	copy a T_PCALL node to a PCallNode
     *	pcall	[0]	T_PCALL
     *		[1]	lineof yyline or "("
     *		[2]	proc_id
     *		[3]	actual list
     */
pPointer
PCallCopy( pcall )
    int	*pcall;
    {
	pPointer PCall = pNewNode( PCallTAG , sizeof( struct PCallNode ) );
	pPointer Id = ThreadSymbol( pcall[2] );
	pPointer Actuals = tCopy( pcall[3] );

	pDEF( PCall ).PCallId = Id;
	pDEF( PCall ).PCallActuals = Actuals;
	return PCall;
    }

    /*
     *	copy a T_CASE node to a CaseSNode
     *	tcase	[0]	T_CASE
     *		[1]	lineof "case"
     *		[2]	expression
     *		[3]	list of cases
     */
pPointer
CaseSCopy( tcase )
    int	*tcase;
    {
	pPointer CaseS = pNewNode( CaseSTAG , sizeof( struct CaseSNode ) );
	pPointer Expr = tCopy( tcase[2] );
	pPointer Caselist = tCopy( tcase[3] );

	pDEF( CaseS ).CaseSExpr = Expr;
	pDEF( CaseS ).CaseSStat = Caselist;
	return CaseS;
    }

    /*
     *	copy a T_WITH node to a WithNode
     *	with	[0]	T_WITH
     *		[1]	lineof "with"
     *		[2]	variable list
     *		[3]	statement
     */
pPointer
WithCopy( with )
    int	*with;
    {
	pPointer With = pNewNode( WithTAG , sizeof( struct WithNode ) );
	pPointer Vars = tCopy( with[2] );
	pPointer Stat = tCopy( with[3] );

	pDEF( With ).WithVars = Vars;
	pDEF( With ).WithStat = Stat;
	return With;
    }

    /*
     *	copy a T_WHILE node to a WhileNode
     *	twhile	[0]	T_WHILE
     *		[1]	lineof "while"
     *		[2]	expression
     *		[3]	statement
     */
pPointer
WhileCopy( twhile )
    int	*twhile;
    {
	pPointer While = pNewNode( WhileTAG , sizeof( struct WhileNode ) );
	pPointer Expr = tCopy( twhile[2] );
	pPointer Stat = tCopy( twhile[3] );

	pDEF( While ).WhileExpr = Expr;
	pDEF( While ).WhileStat = Stat;
	return While;
    }

    /*
     *	copy a T_REPEAT node to a RepeatNode
     *	trepeat	[0]	T_REPEAT
     *		[1]	lineof "repeat"
     *		[2]	statement list
     *		[3]	expression
     */
pPointer
RepeatCopy( trepeat )
    int	*trepeat;
    {
	pPointer Repeat = pNewNode( RepeatTAG , sizeof( struct RepeatNode ) );
	pPointer Stat = tCopy( trepeat[2] );
	pPointer Expr = tCopy( trepeat[3] );

	pDEF( Repeat ).RepeatStat = Stat;
	pDEF( Repeat ).RepeatExpr = Expr;
	return Repeat;
    }

    /*
     *	copy a T_FORU or T_FORD node to a ForUNode or a ForDNode
     *	tfor	[0]	T_FORU or T_FORD
     *		[1]	lineof "for"
     *		[2]	assignment
     *		[3]	termination expression
     *		[4]	statement
     */
pPointer
ForCopy( tfor )
    int	*tfor;
    {
	pPointer For;
	pPointer Assign;
	pPointer Expr;
	pPointer Stat;

	switch ( tfor[0] ) {
	    case T_FORU:
		For = pNewNode( ForUTAG , sizeof( struct ForUNode ) );
		break;
	    case T_FORD:
		For = pNewNode( ForDTAG , sizeof( struct ForDNode ) );
		break;
	    default:
		panic("ForCopy");
	}
	Assign = tCopy( tfor[2] );
	Expr = tCopy( tfor[3] );
	Stat = tCopy( tfor[4] );
	pDEF( For ).ForUAssign = Assign;
	pDEF( For ).ForUExpr = Expr;
	pDEF( For ).ForUStat = Stat;
	return For;
    }

    /*
     *	copy a T_FORD node to a ForDNode
     *	ford	[0]	T_FORD
     *		[1]	lineof "for"
     *		[2]	assignment
     *		[3]	termination expression
     *		[4]	statement
     */
pPointer
ForDCopy( ford )
    int	*ford;
    {
	pPointer ForD = pNewNode( ForDTAG , sizeof( struct ForDNode ) );
	pPointer Assign = tCopy( ford[2] );
	pPointer Expr = tCopy( ford[3] );
	pPointer Stat = tCopy( ford[4] );

	pDEF( ForD ).ForDAssign = Assign;
	pDEF( ForD ).ForDExpr = Expr;
	pDEF( ForD ).ForDStat = Stat;
	return ForD;
    }

    /*
     *	copy a T_GOTO node to a GotoNode
     *	tgoto	[0]	T_GOTO
     *		[1]	lineof "goto"
     *		[2]	hash of integer label
     */
pPointer
GotoCopy( tgoto )
    int	*tgoto;
    {
	pPointer Goto = pNewNode( GotoTAG , sizeof( struct GotoNode ) );

	pDEF( Goto ).GotoLabel = nllook( tgoto[2] ) -> inTree;
	return Goto;
    }

    /*
     *	copy T_IF or T_IFEL nodes to IfNodes
     *	tif	[0]	T_IF or T_IFEL
     *		[1]	lineof "if"
     *		[2]	expression
     *		[3]	then statement
     *		[4]	else statement
     */
pPointer
IfCopy( tif )
    int	*tif;
    {
	pPointer If = pNewNode( IfTAG , sizeof( struct IfNode ) );
	pPointer Cond = tCopy( tif[2] );
	pPointer Then = tCopy( tif[3] );
	pPointer Else = tCopy( tif[4] );

	pDEF( If ).IfCond = Cond;
	pDEF( If ).IfThen = Then;
	pDEF( If ).IfElse = Else;
	return If;
    }

    /*
     *	copy a T_ASRT node to an AssertNode
     *	asrt	[0]	T_ASRT
     *		[1]	lineof "assert"
     *		[2]	expression
     */
pPointer
AssertCopy( asrt )
    int	*asrt;
    {
	pPointer Assert = pNewNode( AssertTAG , sizeof( struct AssertNode ) );
	pPointer Expr = tCopy( asrt[2] );

	pDEF( Assert ).AssertExpr = Expr;
	return Assert;
    }

    /*
     *	copy a T_ASGN node to an AssignNode
     *	asgn	[0]	T_ASGN
     *		[1]	lineof ":" (of ":=")
     *		[2]	variable
     *		[3]	expression
     */
pPointer
AssignCopy( asgn )
    int	*asgn;
    {
	pPointer Assign = pNewNode( AssignTAG , sizeof( struct AssignNode ) );
	pPointer Var = tCopy( asgn[2] );
	pPointer Expr = tCopy( asgn[3] );

	pDEF( Assign ).AssignVar = Var;
	pDEF( Assign ).AssignExpr = Expr;
	return Assign;
    }

    /*
     *	copy a binary operator to an appropriate pNode
     *	binop	[0]	T_EQ, T_LT, T_GT, T_LE, T_GE, T_NE,
     *			T_ADD, T_SUB, T_MULT, T_DIV, T_DIVD, T_MOD
     *			T_IN, T_OR, T_AND
     *		[1]	SAWCON
     *		[2]	left operand expression
     *		[3]	right operand expression
     */
pPointer
BinOpCopy( binop )
    int	*binop;
    {
	pPointer BinOp;
	pPointer Left;
	pPointer Right;

	switch ( binop[0] ) {
	    case T_EQ:
		BinOp = pNewNode( EqTAG , sizeof( struct EqNode ) );
		break;
	    case T_LT:
		BinOp = pNewNode( LtTAG , sizeof( struct LtNode ) );
		break;
	    case T_GT:
		BinOp = pNewNode( GtTAG , sizeof( struct GtNode ) );
		break;
	    case T_LE:
		BinOp = pNewNode( LeTAG , sizeof( struct LeNode ) );
		break;
	    case T_GE:
		BinOp = pNewNode( GeTAG , sizeof( struct GeNode ) );
		break;
	    case T_NE:
		BinOp = pNewNode( NeTAG , sizeof( struct NeNode ) );
		break;
	    case T_ADD:
		BinOp = pNewNode( AddTAG , sizeof( struct AddNode ) );
		break;
	    case T_SUB:
		BinOp = pNewNode( SubTAG , sizeof( struct SubNode ) );
		break;
	    case T_MULT:
		BinOp = pNewNode( MultTAG , sizeof( struct MultNode ) );
		break;
	    case T_DIV:
		BinOp = pNewNode( DivTAG , sizeof( struct DivNode ) );
		break;
	    case T_DIVD:
		BinOp = pNewNode( DivdTAG , sizeof( struct DivdNode ) );
		break;
	    case T_MOD:
		BinOp = pNewNode( ModTAG , sizeof( struct ModNode ) );
		break;
	    case T_IN:
		BinOp = pNewNode( InTAG , sizeof( struct InNode ) );
		break;
	    case T_OR:
		BinOp = pNewNode( OrTAG , sizeof( struct OrNode ) );
		break;
	    case T_AND:
		BinOp = pNewNode( AndTAG , sizeof( struct AndNode ) );
		break;
	    default:
		panic("BinOpCopy");
	}
	Left = tCopy( binop[2] );
	Right = tCopy( binop[3] );
	pDEF( BinOp ).BinOpLeft = Left;
	pDEF( BinOp ).BinOpRight = Right;
	return BinOp;
    }

    /*
     *	copy a T_NIL node to a NilNode
     *	tnil	[0]	T_NIL
     *		[1]	NOCON
     */
pPointer
NilCopy( tnil )
    int	*tnil;
    {
	pPointer Nil = pNewNode( NilTAG , 0 );

	return Nil;
    }

    /*
     *	copy an T_FCALL node to an FCallNode
     *	fcall	[0]	T_FCALL
     *		[1]	NOCON
     *		[2]	func_id
     *		[3]	actual expression list
     */
pPointer
FCallCopy( fcall )
    int	*fcall;
    {
	pPointer FCall = pNewNode( FCallTAG , sizeof( struct FCallNode ) );
	pPointer Id = ThreadSymbol( fcall[2] );
	pPointer Actuals = tCopy( fcall[3] );

	pDEF( FCall ).FCallId = Id;
	pDEF( FCall ).FCallActuals = Actuals;
	return FCall;
    }

    /*
     *	copy a T_CSET node to a SetNode
     *	cset	[0]	T_CSET
     *		[1]	SAWCON
     *		[2]	element list
     */
pPointer
SetCopy( cset )
    int	*cset;
    {
	pPointer Set = pNewNode( SetTAG , sizeof( struct SetNode ) );
	pPointer Elements = tCopy( cset[2] );

	pDEF( Set ).SetElements = Elements;
	return Set;
    }

    /*
     *	copy a T_RANG node to a RangeNode
     *	rang	[0]	T_RANG
     *		[1]	lower limit
     *		[2]	upper limit
     */
pPointer
RangeCopy( rang )
    int	*rang;
    {
	pPointer Range = pNewNode( RangeTAG , sizeof( struct RangeNode ) );
	pPointer Lower = tCopy( rang[1] );
	pPointer Upper = tCopy( rang[2] );

	pDEF( Range ).RangeLower = Lower;
	pDEF( Range ).RangeUpper = Upper;
	return Range;
    }

    /*
     *	copies an extended T_VAR node to a VarNode
     *	var	[0]	T_VAR
     *		[1]	NOCON
     *		[2]	variable id (may be variable or field name in WITH)
     *		[3]	qualifications list
     *		[4]	>>pTree extension<< struct nl * or NIL.
     *			NIL if this is a real variable,
     *			struct nl * to field,
     *			otherwise we'd never find it in the right record.
     *			see setupvar
     */
pPointer
VarCopy( var )
    int	*var;
    {
	pPointer Var = pNewNode( VarTAG , sizeof( struct VarNode ) );
	pPointer Id;
	pPointer Quals = tCopy( var[3] );

	if ( var[4] == NIL )
		Id = ThreadSymbol( var[2] );
	  else  Id = ThreadName( var[4] );
	pDEF( Var ).VarId = Id;
	pDEF( Var ).VarQuals = Quals;
	return Var;
    }

    /*
     *	copy a T_ARY qualification to a SubscNode
     *	ary	[0]	T_ARY
     *		[1]	subscript expression list
     */
pPointer
SubscCopy( ary )
    int	*ary;
    {
	pPointer Subsc = pNewNode( SubscTAG , sizeof( struct SubscNode ) );
	pPointer Exprs = tCopy( ary[1] );

	pDEF( Subsc ).SubscSubsc = Exprs;
	return Subsc;
    }

    /*
     *	copy an extended T_FIELD qualification to a SelNode
     *	field	[0]	T_FIELD
     *		[1]	field_id
     *		[2]	NIL
     *		[3]	>>pTree extension<< struct nl * to field
     *			otherwise we'd never know where it is
     *			put in by lvalue
     */
pPointer
SelCopy( field )
    int *field;
    {
	pPointer Sel = pNewNode( SelTAG , sizeof( struct SelNode ) );
	pPointer Field = ThreadName( field[3] );

	pDEF( Sel ).SelField = Field;
	return Sel;
    }

    /*
     *	copy a T_PTR qualification to a PtrNode
     *	ptr	[0]	T_PTR
     */
pPointer
PtrCopy( ptr )
    int *ptr;
    {
	pPointer Ptr = pNewNode( PtrTAG , 0 );

	return Ptr;
    }

    /*
     *	copy a T_WEXP node to a WidthNode and maybe an OctNode or a HexNode
     *	for expr : width
     *	 or expr : width : places
     *	 or expr radix
     *	 or expr : width radix
     *	wexp	[0]	T_WEXP
     *		[1]	expr
     *		[2]	width or NIL
     *		[3]	places or OCT or HEX
     */
pPointer
WidthCopy( wexp )
    int *wexp;
    {
	pPointer Width = pNewNode( WidthTAG , sizeof( struct WidthNode ) );
	pPointer expr = tCopy( wexp[1] );
	pPointer width = tCopy( wexp[2] );
	pPointer places;
	pPointer radix;

	pDEF( Width ).WidthExpr = expr;
	pDEF( Width ).WidthWidth = width;
	switch ( wexp[3] ) {
	    default:
		places = tCopy( wexp[3] );
		radix = pNIL;
		break;
	    case OCT:
		places = pNIL;
		radix = pNewNode( OctTAG , 0 );
		break;
	    case HEX:
		places = pNIL;
		radix = pNewNode( HexTAG , 0 );
		break;
	}
	pDEF( Width ).WidthPlaces = places;
	pDEF( Width ).WidthRadix = radix;
	return Width;
    }

#endif PTREE
