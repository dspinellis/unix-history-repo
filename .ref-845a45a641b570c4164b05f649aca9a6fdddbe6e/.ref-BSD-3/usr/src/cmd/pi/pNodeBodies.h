union pNodeBodies {

    struct IntNode {
	long 		IntValue;
    };

    struct RealNode {
	float		RealValue;
    };

	/*
	 *	these are actually allocated by
	 *	pNewNode( StringTAG , strlen( string ) + 1 )
	 */
    struct StringNode {
	char		StringValue[1];
    };

    struct ListNode {
	pPointer	ListUp;
	pPointer	ListDown;
	pPointer	ListItem;
    };

    struct ThreadNode {
	pPointer	ThreadPointer;
    };

	/*
	 *	note that the first several GlobNodes fields are
	 *	one pPointer off from PorFNodes.
	 *	this so the simple-minded code in fdec works,
	 *	attaching the program to GlobProg as a PorFPFs
	 */
    struct GlobNode {
	pPointer	GlobaAlign;
	pPointer	GlobbAlign;
	pPointer	GlobConsts;
	pPointer	GlobTypes;
	pPointer	GlobVars;
	pPointer	GlobPFs;
	pPointer	GlobProg;
    };


    struct PorFNode {
	pPointer	PorFName;
	pPointer	PorFParams;
	pPointer	PorFLabels;
	pPointer	PorFConsts;
	pPointer	PorFTypes;
	pPointer	PorFVars;
	pPointer	PorFPFs;
	pPointer	PorFBody;
	pPointer	PorFReturns;
	short		PorFForward;
    };

    struct BConstNode {
	pPointer	BConstName;
    };

    struct BTypeNode {
	pPointer	BTypeName;
    };

    struct BVarNode {
	pPointer	BVarName;
    };

    struct BFuncNode {
	pPointer	BFuncName;
    };

    struct BProcNode {
	pPointer	BProcName;
    };

    struct LabelDNode {
	pPointer	LabelDName;
    };

    struct ConstDNode {
	pPointer	ConstDName;
	pPointer	ConstDValue;
    };

	/*
	 * used to head lists of identically typed names: e.g. vars and fields.
	 * also for overlays with TypeDNodes, VarDNodes, FieldDNodes, etc.
	 */
    struct TypedNode {
	pPointer	TypedNames;
	pPointer	TypedType;
    };

    struct TypeDNode {
	pPointer	TypeDName;
	pPointer	TypeDType;
    };

    struct EnumTNode {
	pPointer	EnumTScalars;
    };

    struct ScalDNode {
	pPointer	ScalDName;
    };

    struct RangeTNode {
	pPointer	RangeTLower;
	pPointer	RangeTUpper;
    };

    struct SetTNode {
	pPointer	SetTType;
    };

    struct FileTNode {
	pPointer	FileTType;
    };

    struct PtrTNode {
	pPointer	PtrTType;
    };

    struct PackTNode {
	pPointer	PackTType;
    };

    struct ArrayTNode {
	pPointer	ArrayTDims;
	pPointer	ArrayTType;
    };

    struct RecTNode {
	pPointer	RecTFldlst;
    };

    struct FldlstNode {
	pPointer	FldlstFixed;
	pPointer	FldlstVariants;
    };

    struct FieldDNode {
	pPointer	FieldDName;
	pPointer	FieldDType;
    };

    struct VarntNode {
	pPointer	VarntTag;
	pPointer	VarntCases;
    };

    struct VCaseNode {
	pPointer	VCaseConst;
	pPointer	VCaseRec;
    };

    struct VarDNode {
	pPointer	VarDName;
	pPointer	VarDType;
    };

	/*
	 *	not really in the tree,
	 *	used as an overlay for ValPNodes and VarPNodes in ParamCopy
	 */
    struct ParamDNode {
	pPointer	ParamDName;
	pPointer	ParamDType;
    };

    struct ValPNode {
	pPointer	ValPName;
	pPointer	ValPType;
    };

    struct VarPNode {
	pPointer	VarPName;
	pPointer	VarPType;
    };

    struct AssignNode {
	pPointer	AssignVar;
	pPointer	AssignExpr;
    };

    struct PCallNode {
	pPointer	PCallId;
	pPointer	PCallActuals;
    };

    struct IfNode {
	pPointer	IfCond;
	pPointer	IfThen;
	pPointer	IfElse;
    };

    struct WhileNode {
	pPointer	WhileExpr;
	pPointer	WhileStat;
    };

    struct RepeatNode {
	pPointer	RepeatStat;
	pPointer	RepeatExpr;
    };

    struct ForUNode {
	pPointer	ForUAssign;
	pPointer	ForUExpr;
	pPointer	ForUStat;
    };

    struct ForDNode {
	pPointer	ForDAssign;
	pPointer	ForDExpr;
	pPointer	ForDStat;
    };

    struct CaseSNode {
	pPointer	CaseSExpr;
	pPointer	CaseSStat;
    };

    struct CasedNode {
	pPointer	CasedLabel;
	pPointer	CasedStat;
    };

    struct GotoNode {
	pPointer	GotoLabel;
    };

    struct LabelNode {
	pPointer	LabelLabel;
    };

    struct WithNode {
	pPointer	WithVars;
	pPointer	WithStat;
    };

    struct AssertNode {
	pPointer	AssertExpr;
    };

	/*
	 * not actually in the tree,
	 * for use as overlay with binary operator pNodes
	 */
    struct BinOpNode {
	pPointer	BinOpLeft;
	pPointer	BinOpRight;
    };

    struct AndNode {
	pPointer	AndLeft;
	pPointer	AndRight;
    };

    struct OrNode {
	pPointer	OrLeft;
	pPointer	OrRight;
    };

    struct EqNode {
	pPointer	EqLeft;
	pPointer	EqRight;
    };

    struct NeNode {
	pPointer	NeLeft;
	pPointer	NeRight;
    };

    struct LtNode {
	pPointer	LtLeft;
	pPointer	LtRight;
    };

    struct GtNode {
	pPointer	GtLeft;
	pPointer	GtRight;
    };

    struct LeNode {
	pPointer	LeLeft;
	pPointer	LeRight;
    };

    struct GeNode {
	pPointer	GeLeft;
	pPointer	GeRight;
    };

    struct InNode {
	pPointer	InLeft;
	pPointer	InRight;
    };

    struct AddNode {
	pPointer	AddLeft;
	pPointer	AddRight;
    };

    struct SubNode {
	pPointer	SubLeft;
	pPointer	SubRight;
    };

    struct MultNode {
	pPointer	MultLeft;
	pPointer	MultRight;
    };

    struct DivdNode {
	pPointer	DivdLeft;
	pPointer	DivdRight;
    };

    struct DivNode {
	pPointer	DivLeft;
	pPointer	DivRight;
    };

    struct ModNode {
	pPointer	ModLeft;
	pPointer	ModRight;
    };

    /*
     * not actually in the tree,
     * for use as overlay with unary operator pNodes
     */
    struct UnOpNode {
	pPointer	UnOpExpr;
    };

    struct PlusNode {
	pPointer	PlusExpr;
    };

    struct MinusNode {
	pPointer	MinusExpr;
    };

    struct NotNode {
	pPointer	NotExpr;
    };

    struct FCallNode {
	pPointer	FCallId;
	pPointer	FCallActuals;
    };

    struct SetNode {
	pPointer	SetElements;
    };

    struct RangeNode {
	pPointer	RangeLower;
	pPointer	RangeUpper;
    };

    struct VarNode {
	pPointer	VarId;
	pPointer	VarQuals;
    };

	/*
	 *	one of these indicates pointer indirection.
	 *	they are actually allocated by pNewNode( PtrTAG , 0 )
	 */
    struct PtrNode {
	char		PtrPrescence;
    };

    struct SubscNode {
	pPointer	SubscSubsc;
    };

    struct SelNode {
	pPointer	SelField;
    };

	/*
	 *	one of these indicates the value NIL
	 *	they are actually allocated by pNewNode( NilTAG , 0 )
	 */
    struct NilNode {
	char		NilValue;
    };

    struct WidthNode {
	pPointer	WidthExpr;
	pPointer	WidthWidth;
	pPointer	WidthPlaces;
	pPointer	WidthRadix;
    };

	/*
	 *	one of these indicates octal radix conversion
	 *	they are actually allocated by pNewNode( OctTAG , 0 )
	 */
    struct OctNode {
	char		OctRadix;
    };

	/*
	 *	one of these indicates hexadecimal radix conversion
	 *	they are actually allocated by pNewNode( HexTAG , 0 )
	 */
    struct HexNode {
	char		HexRadix;
    };

};

