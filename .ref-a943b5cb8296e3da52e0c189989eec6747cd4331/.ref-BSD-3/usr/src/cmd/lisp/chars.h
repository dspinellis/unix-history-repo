/* lexical table for input and output ***********************************/
/* the format of the entries are:       ab..xxxx			*/
/*									*/
/* where a is set iff the atom containing the symbol must be quoted	*/
/* where b is set iff the character separates atoms normally		*/
/* where xxxx is a number unique to the class of symbol			*/

#define	VNUM	0000
#define VMINUS	0001
#define VSIGN   0001
#define VCHAR	0002
#define VSCA	0102
#define	VLPARA	0303
#define VRPARA	0304
#define VPERD	0305
#define	VLBRCK	0306
#define	VRBRCK	0307
#define	VEOF	0310
#define	VSQ	0311
#define	VDQ	0212
#define VSD	0211
#define	VERR	0313
#define	VSEP	0314
#define VSPL	0315
#define VMAC	0316
#define VESC	0217
#define VQUO	0326


#define QUTMASK	0200
#define	SEPMASK	0100

#define TSCA	1
#define TLPARA	2
#define TRPARA	3
#define TPERD	4
#define TEOF	5
#define TSPL	6
#define TMAC	7
#define TSQ	8
#define TLBKT	9
