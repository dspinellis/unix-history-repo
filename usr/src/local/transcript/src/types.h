/* PostScript type and data structure defs

		Copyright 1983 -- Adobe Systems, Inc.
	    PostScript is a trademark of Adobe Systems, Inc.
NOTICE:  All information contained herein or attendant hereto is, and
remains, the property of Adobe Systems, Inc.  Many of the intellectual
and technical concepts contained herein are proprietary to Adobe Systems,
Inc. and may be covered by U.S. and Foreign Patents or Patents Pending or
are protected as trade secrets.  Any dissemination of this information or
reproduction of this material are strictly forbidden unless prior written
permission is obtained from Adobe Systems, Inc.

Original version: Chuck Geschke: January 20, 1983
Edit History:
Chuck Geschke: Tue Sep  4 14:16:04 1984
Tom Boynton: Thu Mar  3 15:47:27 1983
Doug Brotz: Wed Jul 11 17:28:07 1984
Ed Taft: Sun Sep 23 15:32:14 1984
End Edit History.

Note: types.h is intended to be usable either as an inclusion in
postscript.h or by itself. Nothing in types.h should depend on
definitions in any other .h file.

Note: for the moment we depend absolutely on an int being the same
as a long int and being at least 32 bits.
*/

#ifndef TYPES_H
#define TYPES_H

/* generally useful macros */

#define endswitch default:;
#define until(x) while (!(x))
#define abs(x) ((x)<0?-(x):(x))

#if IEEESOFT
#define RtoILOOPHOLE(r) (*(integer *)(&(r)))
#define RealEq0(r) ((RtoILOOPHOLE(r)<<1)==0)
#define RealNe0(r) ((RtoILOOPHOLE(r)<<1)!=0)
#define RealGt0(r) (RtoILOOPHOLE(r)>0)
#define RealGe0(r) ((RtoILOOPHOLE(r)>=0)||RealEq0(r))
#define RealLt0(r) ((RtoILOOPHOLE(r)<0)&&RealNe0(r))
#define RealLe0(r) (RtoILOOPHOLE(r)<=0)
#else IEEESOFT
#define RealEq0(r) ((r)==0.0)
#define RealNe0(r) ((r)!=0.0)
#define RealGt0(r) ((r)>0.0)
#define RealGe0(r) ((r)>=0.0)
#define RealLt0(r) ((r)<0.0)
#define RealLe0(r) ((r)<=0.0)
#endif IEEESOFT

#define fabs(x) (RealLt0(x)?-(x):(x))
#define MIN(a,b) ((a)<(b)?(a):(b))
#define MAX(a,b) ((a)>(b)?(a):(b))
#define forward extern
#define private static
#define public
#define global extern
typedef unsigned int boolean;
#define true 1
#define false 0
typedef long int word;
typedef word *wordptr;
#define procedure integer	/* actually void */
typedef unsigned char character;
typedef character *string, *charptr;
typedef long int integer;
typedef float real;
typedef double longreal;
typedef unsigned short int cardinal;
typedef unsigned long int longcardinal;
typedef integer Fixed;	/*  16 bits of integer, 16 bits of fraction */
typedef integer (*PIntProc)();	/* pointer to procedure returning integer */
#ifdef STREAM_H
typedef StmRec *StreamHandle;
#endif STREAM_H
#define MAXlevel 15		/* max save level */
#define MAXexecLevel 10		/* max recursion level of psExecute */
#define MAXinteger 017777777777
#define MINinteger 020000000000
#define MAXunsignedinteger ((unsigned)037777777777)
#define MAXcardinal 0177777
#define MAXlongcardinal 037777777777
#define MAXdctLength 2000
#define MAXnameLength 127
#define MAXnumeralString 35	/* must be at least 32 on a VAX */
#define MAXtimeString 24	/* must be at least 24 on UNIX */
#define MAXstringLength MAXcardinal
#define MAXarrayLength MAXcardinal
#define MAXdctCount MAXcardinal
#define MAXcmdIndex 10000	/* to distinguish cmd index from raw PC */
#define NUL '\0'

#define chPerPg 1024		/* VAX specific */
#define chPerWd 4
#define wdPerPg chPerPg/chPerWd

/* the following defs are for relative pointers into PS VM */

typedef unsigned long int Offset;
typedef Offset StrOffset, ArrayOffset, DictOffset, KeyValOffset, NameOffset,
		SROoffset, SRCoffset, SRDoffset, SRoffset;
#define NIL NULL

/* the definition of basic PostScript objects */

#define nullObj 0
#define intObj 1
#define realObj 2
#define nameObj 3
#define boolObj 4
#define strObj 5
#define stmObj 6
#define cmdObj 7
#define dictObj 8
#define arrayObj 9
#define markObj 10
#define saveObj 11
#define fontObj 12
#define nObTypes 13

#define nBitVectorBits 32
typedef unsigned long int BitVector;

typedef unsigned char Level;

typedef unsigned int Access;

#define nAccess 0
#define rAccess 1
#define wAccess 2
#define xAccess 4
#define aAccess 7

#define Lobj 0	/* tag for literal object */
#define Xobj 1	/* tag for executable object */

typedef struct object{
		cardinal tag:1;
		Access access:3;
		cardinal type:4;
		Level level;
		cardinal length;
		union{
		/*null*/  int nullval;
		/*int*/   integer ival;
		/*real*/  real rval;
		/*name*/  NameOffset nmval;
		/*bool*/  boolean bval;
		/*str*/   StrOffset strval;
		/*stm*/   integer stmval;
		/*cmd*/   NameOffset cmdval;
		/*dict*/  DictOffset dictval;
		/*array*/ ArrayOffset arrayval;
		/*mark*/  int markval;
		/*save*/  int saveval;
		/*font*/  int fontval;} val;
		} Object, *PObject, NullObj, *PNullObj,
		  IntObj, *PIntObj, RealObj, *PRealObj,
		  NameObj, *PNameObj, BoolObj, *PBoolObj,
		  StrObj, *PStrObj, StmObj, *PStmObj,
		  CmdObj, *PCmdObj, DictObj, *PDictObj,
		  AryObj, *PAryObj, MarkObj, *PMarkObj,
		  SaveObj, *PSaveObj, FontObj, *PFontObj;

/* following macros are useful for contructing Objects */

#define LNullObj(o) o = iLNullObj; o.level=LEVEL
#define LIntObj(o,v) o = iLIntObj; o.level=LEVEL; o.val.ival=v
#define LRealObj(o,v) o = iLRealObj; o.level=LEVEL; o.val.rval=v
#define LNameObj(o,v) o = iLNameObj; o.level=LEVEL; o.val.nmval=v
#define LBoolObj(o,v) o = iLBoolObj; o.level=LEVEL; o.val.bval=v
#define LStrObj(o,l,v) o = iLStrObj; o.level=LEVEL; o.length=l; o.val.strval=v
#define LStmObj(o,i,s) o = iLStmObj; o.level=LEVEL; o.length=i; o.val.stmval=s
#define LDictObj(o,v) o = iLDictObj; o.level=LEVEL; o.val.dictval=v
#define LAryObj(o,l,v) o = iLAryObj; o.level=LEVEL; o.length=l; o.val.arrayval=v
#define LMarkObj(o) o = iLMarkObj; o.level=LEVEL
#define LSaveObj(o,v) o = iLSaveObj; o.level=LEVEL; o.val.saveval=v
#define LFontObj(o,v) o = iLFontObj; o.level=LEVEL; o.val.fontval=v
#define XNameObj(o,v) o = iXNameObj; o.level=LEVEL; o.val.nmval=v
#define XStrObj(o,l,v) o = iXStrObj; o.level=LEVEL; o.length=l; o.val.strval=v
#define XCmdObj(o,i,v) o = iXCmdObj; o.level=LEVEL; o.val.cmdval=i; o.length=v
#define XAryObj(o,l,v) o = iXAryObj; o.level=LEVEL; o.length=l; o.val.arrayval=v

#ifndef LEVEL
#define LEVEL level
#endif

extern NullObj iLNullObj;
extern IntObj iLIntObj;
extern RealObj iLRealObj;
extern NameObj iLNameObj, iXNameObj;
extern BoolObj iLBoolObj;
extern StrObj iLStrObj, iXStrObj;
extern StmObj iLStmObj;
extern CmdObj iXCmdObj;
extern DictObj iLDictObj;
extern AryObj iLAryObj, iXAryObj;
extern MarkObj iLMarkObj;
extern SaveObj iLSaveObj;
extern FontObj iLFontObj;


/* graphics data types */

typedef real Component;
typedef struct {Component x, y;} Cd;
typedef struct {integer x, y;} DevCd;

typedef struct {real a, b, c, d, tx, ty;} Mtx, *PMtx;

/* initialization */

typedef enum {init, romreg, ramreg} InitReason;

#endif TYPES_H
