    /*
     *	pTree.h
     *
     *	global declarations for copying pi trees to pTrees
     */

#include	<pAddr.h>

#include	"pTags.h"

    /*
     *	stack of nested program/procedure/function declaration pNodes
     *	and a stack pointer
     */
#define		MAXNESTING	64
pPointer	PorFHeader[ MAXNESTING ];
short		nesting;

    /*
     *	pointer to namelist entry of record in progress
     *	see RecTCopy, FieldCopy and VarntCopy
     */
struct nl *inrecord;

    /*
     *	functions which are of type other than int
     */
struct nl	*nllook();
struct nl	*nllook1();
struct nl	*chainlookup();
pPointer	PorFCopy();
pPointer	FileCopy();
pPointer	ParamCopy();
pPointer	LabelDCopy();
pPointer	ConstDecl();
pPointer	TypeDecl();
pPointer	FieldCopy();
pPointer	VarntCopy();
pPointer	EnumTCopy();
pPointer	VarDecl();
pPointer	ThreadSymbol();
pPointer	ThreadName();
pPointer	UnOpCopy();
pPointer	PtrTCopy();
pPointer	PackTCopy();
pPointer	RangeTCopy();
pPointer	ArrayTCopy();
pPointer	FileTCopy();
pPointer	SetTCopy();
pPointer	RecTCopy();
pPointer	VCaseCopy();
pPointer	CasedCopy();
pPointer	LabelCopy();
pPointer	PCallCopy();
pPointer	CaseSCopy();
pPointer	WithCopy();
pPointer	WhileCopy();
pPointer	RepeatCopy();
pPointer	ForCopy();
pPointer	ForDCopy();
pPointer	GotoCopy();
pPointer	IfCopy();
pPointer	AssertCopy();
pPointer	AssignCopy();
pPointer	BinOpCopy();
pPointer	NilCopy();
pPointer	FCallCopy();
pPointer	SetCopy();
pPointer	RangeCopy();
pPointer	VarCopy();
pPointer	SubscCopy();
pPointer	SelCopy();
pPointer	PtrCopy();
pPointer	WidthCopy();
pPointer	tCopy();
pPointer	ListCopy();
pPointer	ListAppend();
pPointer	iCopy();
pPointer	fCopy();
pPointer	sCopy();
