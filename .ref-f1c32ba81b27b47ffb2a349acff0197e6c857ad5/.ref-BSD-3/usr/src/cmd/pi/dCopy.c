    /*
     *	dCopy.c
     *
     *	functions to copy pi declarations to pTrees
     *	these should be all the functions
     *	that mark the inTree field of the namelist
     */

#include	"whoami"

#ifdef PTREE

#include	"0.h"

#include	"tree.h"

    /*
     *	copy a T_PROG, T_PDEC, or T_FDEC into a PorFNode
     *	porf	[0]	T_PROG T_PDEC or T_FDEC
     *		[1]	lineof "program" or trailing ";"
     *		[2]	program, procedure, or function name
     *		[3]	file or formal parameter list
     *		[4]	function return type or pNIL
     */
pPointer
PorFCopy( porf )
    int	*porf;
    {
	pPointer		PorF;
	union pNodeBodies	*PorFp;
	struct nl		*nlporf;
	extern struct nl	*program;

	    /*
	     *	programs are defnl'ed but not entered, but extern program works
	     */
	if ( porf[0] == T_PROG )
		nlporf = program;
	  else	nlporf = nllook( porf[2] );
	if ( nlporf -> inTree != pNIL ) {
	    pDEF( nlporf -> inTree ).PorFForward = TRUE;
	    return;
	}
	PorF = pNewNode( PorFTAG , sizeof( struct PorFNode ) );
	pSeize( PorF );
	PorFp = &( pDEF( PorF ) );
	PorFp -> PorFName = sCopy( porf[2] );
	PorFHeader[ ++ nesting ] = PorF;
	if ( porf[0] != T_PROG )
		PorFp -> PorFParams = tCopy( porf[3] );
	  else	PorFp -> PorFParams = FileCopy( porf[3] );
	nesting --;
	PorFp -> PorFLabels = pNIL;
	PorFp -> PorFConsts = pNIL;
	PorFp -> PorFTypes = pNIL;
	PorFp -> PorFVars = pNIL;
	PorFp -> PorFPFs = pNIL;
	PorFp -> PorFBody = pNIL;
	PorFp -> PorFReturns = tCopy( porf[4] );
	PorFp -> PorFForward = FALSE;
	pRelease( PorF );
	nlporf -> inTree = PorF;
	return PorF;
    }

    /*
     *	looks for defined (but not entered) symbols
     *	(either files or formal parameters)
     *	which hang down the chain field of
     *	program, procedure or function namelist entry.
     */
struct nl *
chainlookup( porf , symb )
    struct nl	*porf;
    char	*symb;
    {
	struct nl *paramp;
	
	for ( paramp = porf->chain ; paramp != NIL ; paramp = paramp->chain )
	    if ( paramp -> symbol == symb )
		break;
	return paramp;
    }

    /*
     *	copy a list of file names to a list of threads to VarDNodes
     *	(or threads to the BVarNodes for input or output)
     *	for later inclusion in the variable declaration list.
     *	as a special case, the files are found chained to the program nl entry.
     */
pPointer
FileCopy( files )
    int	*files;
    {
	int		    *filep;
	pPointer	    Thread;
	pPointer	    List;
	pPointer	    First;
	pPointer	    After;
	extern struct nl    *program;
	extern struct nl    *input;
	extern struct nl    *output;

	First = After = pNIL;
	for ( filep = files ; filep != NIL ; filep = (int *) filep[2] ) {
	    struct nl	*file = chainlookup( program , filep[1] );

		    if ( filep[1] == input -> symbol ) {
			file -> inTree = input -> inTree;
			Thread = ThreadName( input );
	    } else  if ( filep[1] == output -> symbol ) {
			file -> inTree = output -> inTree;
			Thread = ThreadName( output );
	    } else  {
			pPointer File = pNewNode( VarDTAG
						, sizeof( struct VarDNode ) );
			pPointer Name = sCopy( filep[1] );

			pDEF( File ).VarDName = Name;
			pDEF( File ).VarDType = pNIL;
			file -> inTree = File;
			Thread = ThreadName( file );
	    }
	    List = pNewNode( ListTAG , sizeof( struct ListNode ) );
	    pDEF( List ).ListItem = Thread;
	    pDEF( List ).ListUp = After;
	    pDEF( List ).ListDown = pNIL;
	    if ( After == pNIL )
		    First = List;
	      else  pDEF( After ).ListDown = List;
	    After = List;
	}
	return First;
    }

    /*
     *	copy a formal parameter declaration to a TypedNode
     *	and a list of ValPNodes or VarPNodes.
     *	param	[0]	T_PVAL or T_PVAR
     *		[1]	id_list
     *		[2]	type
     */
pPointer
ParamCopy( param )
    int     *param;
    {
	int	    *idl;
	pPointer    Param;
	pPointer    Name;
	pPointer    Typed = pNewNode( TypedTAG , sizeof( struct TypedNode ) );
	pPointer    Type = tCopy( param[2] );
	pPointer    List;
	pPointer    After;
	char	    *name;
	struct nl   *porf;

	Name = pUSE( PorFHeader[ nesting ] ).PorFName;
	name = *hash( pUSE( Name ).StringValue , 0 );
	porf = nllook( name );
	if ( porf == NIL )
	    panic( "ParamCopy:nllook" );
	pDEF( Typed ).TypedType = Type;
	After = pNIL;
	for ( idl = (int *)param[1] ; idl != NIL ; idl = (int *)idl[2] ) {
	    switch ( param[0] ) {
		case T_PVAL:
		    Param = pNewNode( ValPTAG , sizeof( struct ValPNode ) );
		    break;
		case T_PVAR:
		    Param = pNewNode( VarPTAG , sizeof( struct VarPNode ) );
		    break;
		default:
		    panic("ParamCopy:param[0]");
	    };
	    Name = sCopy( idl[1] );
	    pDEF( Param ).ParamDName = Name;
	    pDEF( Param ).ParamDType = Type;
	    chainlookup( porf , idl[1] ) -> inTree = Param;
	    List = pNewNode( ListTAG , sizeof( struct ListNode ) );
	    pDEF( List ).ListItem = Param;
	    pDEF( List ).ListUp = After;
	    pDEF( List ).ListDown = pNIL;
	    if ( After == pNIL )
		    pDEF( Typed ).TypedNames = List;
	      else  pDEF( After ).ListDown = List;
	    After = List;
	}
	return Typed;
    }

    /*
     *	construct a list of LabelDNodes from a list of YINTs
     */
pPointer
LabelDCopy( labels )
    int	*labels;
    {
	int		*labelp;
	pPointer	Label;
	pPointer	Name;
	pPointer	List;
	pPointer	First;
	pPointer	After;

	After = pNIL;
	for ( labelp = labels ; labelp != NIL ; labelp = (int *) labelp[2] ) {
	    Label = pNewNode( LabelDTAG , sizeof( struct LabelDNode ) );
	    Name = sCopy( labelp[1] );
	    pDEF( Label ).LabelDName = Name;
	    nllook( labelp[1] ) -> inTree = Label;
	    List = pNewNode( ListTAG , sizeof( struct ListNode ) );
	    pDEF( List ).ListItem = Label;
	    pDEF( List ).ListUp = After;
	    pDEF( List ).ListDown = pNIL;
	    if ( After == pNIL )
		    First = List;
	      else  pDEF( After ).ListDown = List;
	    After = List;
	}
	return First;
    }

    /*
     *	copy a constant declaration to a ConstDNode
     */
pPointer
ConstDecl( id , decl )
    char	*id;
    int		*decl;
    {
	pPointer    Const = pNewNode( ConstDTAG , sizeof( struct ConstDNode ) );
	pPointer    Name = sCopy( id );
	pPointer    ConstValue = tCopy( decl );

	pDEF( Const ).ConstDName = Name;
	pDEF( Const ).ConstDValue = ConstValue;
	nllook( id ) -> inTree = Const;
	return Const;
    }

    /*
     *	copy a type declaration to a TypeDNode.
     *	note that pointers' types are filled in later.
     */
pPointer
TypeDecl( id , decl )
    char	*id;
    int		*decl;
    {
	pPointer    Type = pNewNode( TypeDTAG , sizeof( struct TypeDNode ) );
	pPointer    Name = sCopy( id );
	pPointer    TypeType = tCopy( decl );
	struct nl   *np = nllook( id );
		
	pDEF( Type ).TypeDName = Name;
	pDEF( Type ).TypeDType = TypeType;
	np -> inTree = Type;
	if ( ( np -> type ) -> class == PTR ) {
	    ( np -> type ) -> inTree = TypeType;
	}
	return Type;
    }

    /*
     *	copies a T_RFIELD node to a TypedNode
     *	with a type and a list of FieldDNodes
     *	rfield[0]	T_RFIELD
     *	      [1]	lineof ":"
     *	      [2]	id_list
     *	      [3]	type
     *	uses the extern inrecord to know which record its in.
     */
pPointer
FieldCopy( rfield )
    int	*rfield;
    {
	extern struct nl *inrecord;
	pPointer    Typed = pNewNode( TypedTAG , sizeof( struct TypedNode ) );
	int	    *idlp;
	pPointer    Type = tCopy( rfield[3] );
	pPointer    List;
	pPointer    After;

	pDEF( Typed ).TypedNames = pNIL;
	pDEF( Typed ).TypedType = Type;
	After = pNIL;
	for ( idlp = (int *)rfield[2] ; idlp != NIL ; idlp = (int *)idlp[2] ) {
	    pPointer FieldD
			= pNewNode( FieldDTAG , sizeof( struct FieldDNode ) );
	    pPointer Name = sCopy( idlp[1] );
	    struct nl *field;

	    if ( inrecord == NIL )
		panic( "FieldCopy:inrecord" );
	    field = reclook( inrecord , idlp[1] );
	    if ( field == NIL )
		panic( "FieldCopy:reclook" );
	    pDEF( FieldD ).FieldDName = Name;
	    pDEF( FieldD ).FieldDType = Type;
	    field -> inTree = FieldD;
	    List = pNewNode( ListTAG , sizeof( struct ListNode ) );
	    pDEF( List ).ListItem = FieldD;
	    pDEF( List ).ListUp = After;
	    pDEF( List ).ListDown = pNIL;
	    if ( After == pNIL )
		    pDEF( Typed ).TypedNames = List;
	      else  pDEF( After ).ListDown = List;
	    After = List;
	}
	return Typed;
    }

    /*
     *	copies a T_VARPT node to a VarntNode and a FieldDNode
     *	varpt	[0]	T_TYVARPT
     *		[1]	lineof "case"
     *		[2]	tag id (or nil)
     *		[3]	tag type
     *		[4]	list of variant cases
     *	uses the extern inrecord to know which record its in.
     */
pPointer
VarntCopy( tyvarpt )
    int	*tyvarpt;
    {
	extern struct nl *inrecord;
	pPointer Varnt = pNewNode( VarntTAG , sizeof( struct VarntNode ) );
	pPointer Tag = pNewNode( FieldDTAG , sizeof( struct FieldDNode ) );
	pPointer Name = sCopy( tyvarpt[2] );
	pPointer Type = tCopy( tyvarpt[3] );
	pPointer Cases = tCopy( tyvarpt[4] );

	pDEF( Tag ).FieldDName = Name;
	pDEF( Tag ).FieldDType = Type;
	if ( tyvarpt[2] != NIL )
	    reclook( inrecord , tyvarpt[2] ) -> inTree = Tag;
	pDEF( Varnt ).VarntTag = Tag;
	pDEF( Varnt ).VarntCases = Cases;
	return Varnt;
    }

    /*
     *	copies a T_TYSCAL node to an EnumTNode and a list of ScalDNodes
     *	tyscal	[0]	T_TYSCAL
     *		[1]	lineof "("
     *		[2]	id_list
     */
pPointer
EnumTCopy( tyscal )
    int *tyscal;
    {
	pPointer    EnumT = pNewNode( EnumTTAG , sizeof( struct EnumTNode ) );
	pPointer    ScalD;
	pPointer    Name;
	int	    *idp;
	pPointer    List;
	pPointer    After;

	After = pNIL;
	for ( idp = (int *) tyscal[2] ; idp != NIL ; idp = (int *) idp[2] ) {
	    ScalD = pNewNode( ScalDTAG , sizeof( struct ScalDNode ) );
	    Name = sCopy( idp[1] );
	    pDEF( ScalD ).ScalDName = Name;
	    nllook( idp[1] ) -> inTree = ScalD;
	    List = pNewNode( ListTAG , sizeof( struct ListNode ) );
	    pDEF( List ).ListItem = ScalD;
	    pDEF( List ).ListUp = After;
	    pDEF( List ).ListDown = pNIL;
	    if ( After == pNIL )
		    pDEF( EnumT ).EnumTScalars = List;
	      else  pDEF( After ).ListDown = List;
	    After = List;
	}
	return EnumT;
    }

    /*
     *	copies a variable declaration to a TypedNode
     *	with a type and a list of VarDNodes
     *	also, deals with previously declared (e.g. program files) variables.
     */
pPointer
VarDecl( idl , type )
    int	*idl;
    int	*type;
    {
	pPointer    Typed = pNewNode( TypedTAG , sizeof( struct TypedNode ) );
	int	    *idlp;
	struct nl   *var;
	pPointer    VarD;
	pPointer    Name;
	pPointer    Type = tCopy( type );
	pPointer    List;
	pPointer    After;

	pDEF( Typed ).TypedNames = pNIL;
	pDEF( Typed ).TypedType = Type;
	After = pNIL;
	for ( idlp = (int *) idl ; idlp != NIL ; idlp = (int *) idlp[2] ) {
	    if ( ( var = nllook( idlp[1] ) ) -> inTree == pNIL ) {
		    /*
		     *	usual case, a new variable
		     */
		    VarD = pNewNode( VarDTAG , sizeof( struct VarDNode ) );
		    Name = sCopy( idlp[1] );
		    pDEF( VarD ).VarDName = Name;
		    var -> inTree = VarD;
	    } else {
		    /*
		     *	previously declared (file) variable, already in tree
		     *	gets hung on list of variables, in addition
		     */
		    VarD = var -> inTree;
	    }
	    pDEF( VarD ).VarDType = Type;
	    if ( ( var -> type ) -> class == PTR ) {
		( var -> type ) -> inTree = Type;
	    }
	    List = pNewNode( ListTAG , sizeof( struct ListNode ) );
	    pDEF( List ).ListItem = VarD;
	    pDEF( List ).ListUp = After;
	    pDEF( List ).ListDown = pNIL;
	    if ( After == pNIL )
		    pDEF( Typed ).TypedNames = List;
	      else  pDEF( After ).ListDown = List;
	    After = List;
	}
	return Typed;
    }


    /*
     *	initialize the pTree
     *	including cheapo versions of all the builtins (eech!)
     */
pTreeInit()
    {
	extern char	*in_consts[];
	extern char	*in_types[];
	extern char	*in_ctypes[];
	extern char	*in_vars[];
	extern char	*in_funcs[];
	extern char	*in_procs[];
	union pNodeBodies	*Glob;
	pPointer	List;
	char 		**cp;

	nesting = 0;
	PorFHeader[ nesting ] = pNewNode( GlobTAG , sizeof( struct GlobNode ) );
	pWorld = PorFHeader[ nesting ];
	pSeize( PorFHeader[ nesting ] );
	Glob = &( pDEF( PorFHeader[ nesting ] ) );
	    /*
	     *	built in constants
	     */
	dumpnl( NIL , "pTreeInit" );
	List = pNIL;
	for ( cp = in_consts ; *cp ; cp ++ ) {
	    pPointer BCon = pNewNode( BConstTAG , sizeof( struct BConstNode ) );
	    pPointer Name = sCopy( *cp );

	    pDEF( BCon ).BConstName = Name;
	    List = ListAppend( List , BCon );
	    nllook( *cp ) -> inTree = BCon;
	}
	Glob -> GlobConsts = List;
	    /*
	     *	built in simple and constructed types
	     */
	List = pNIL;
	    /*
	     *	simple types
	     */
	for ( cp = in_types ; *cp ; cp ++ ) {
	    pPointer BType = pNewNode( BTypeTAG , sizeof( struct BTypeNode ) );
	    pPointer Name = sCopy( *cp );

	    pDEF( BType ).BTypeName = Name;
	    List = ListAppend( List , BType );
	    nllook( *cp ) -> inTree = BType;
	}
	    /*
	     *	constructed types (aren't any more difficult)
	     */
	for ( cp = in_ctypes ; *cp ; cp ++ ) {
	    pPointer BType = pNewNode( BTypeTAG , sizeof( struct BTypeNode ) );
	    pPointer Name = sCopy( *cp );

	    pDEF( BType ).BTypeName = Name;
	    List = ListAppend( List , BType );
	    nllook( *cp ) -> inTree = BType;
	}
	Glob -> GlobType = List;
	    /*
	     *	built in variables
	     */
	List = pNIL;
	for ( cp = in_vars ; *cp ; cp ++ ) {
	    pPointer BVar = pNewNode( BVarTAG , sizeof( struct BVarNode ) );
	    pPointer Name = sCopy( *cp );

	    pDEF( BVar ).BVarName = Name;
	    List = ListAppend( List , BVar );
	    nllook( *cp ) -> inTree = BVar;
	}
	Glob -> GlobVars = List;
	    /*
	     *	built in functions and procedures
	     */
	List = pNIL;
	    /*
	     *	built in functions
	     */
	for ( cp = in_funcs ; *cp ; cp ++ ) {
	    pPointer BFunc = pNewNode( BFuncTAG , sizeof( struct BFuncNode ) );
	    pPointer Name = sCopy( *cp );

	    pDEF( BFunc ).BFuncName = Name;
	    List = ListAppend( List , BFunc );
	    nllook( *cp ) -> inTree = BFunc;
	}
	    /*
	     *	built in procedures
	     */
	for ( cp = in_procs ; *cp ; cp ++ ) {
	    pPointer BProc = pNewNode( BProcTAG , sizeof( struct BProcNode ) );
	    pPointer Name = sCopy( *cp );

	    pDEF( BProc ).BProcName = Name;
	    List = ListAppend( List , BProc );
	    nllook( *cp ) -> inTree = BProc;
	}
	Glob -> GlobPFs = List;
	pRelease( PorFHeader[ nesting ] );
    }

    /*
     *	find a symbol in the
     *	block structure symbol
     *	table and returns a pointer to
     *	its namelist entry.
     *	[this is a copy of lookup, except it calls nllook1
     *	whose only variation from lookup1 is that it doesn't set NUSED]
     */
struct nl *
nllook(s)
	register char *s;
{
	register struct nl *p;
	register struct udinfo *udp;

	if (s == NIL) {
		nocascade();
		return (NIL);
	}
	p = nllook1(s);
	if (p == NIL) {
		derror("%s is undefined", s);
		return (NIL);
	}
	if (p->class == FVAR) {
		p = p->chain;
		bn--;
	}
	return (p);
}

/*
 * an internal nllook.
 * It is not an error to call nllook1 if the symbol is not defined.
 * Also nllook1 will return FVARs while nllook never will.
 * [this is a copy of lookup1, except that it doesn't set NUSED]
 */

struct nl *
nllook1(s)
	register char *s;
{
	register struct nl *p;
#	ifndef PI0
	    register struct nl *q;
#	endif
	register int i;

	if (s == NIL)
		return (NIL);
	bn = cbn;
#ifndef PI0
	/*
	 * We first check the field names
	 * of the currently active with
	 * statements (expensive since they
	 * are not hashed).
	 */
	for (p = withlist; p != NIL; p = p->nl_next) {
		q = p->type;
		if (q == NIL)
			continue;
		if (reclook(q, s) != NIL)
			/*
			 * Return the WITHPTR, lvalue understands.
			 */
			return (p);
	}
#endif
	/*
	 * Symbol table is a 64 way hash
	 * on the low bits of the character
	 * pointer value. (Simple, but effective)
	 */
	i = (int) s & 077;
	for (p = disptab[i]; p != NIL; p = p->nl_next)
		if (p->symbol == s && p->class != FIELD && p->class != BADUSE) {
			bn = (p->nl_block & 037);
			return (p);
		}
	return (NIL);
}

#endif PTREE
