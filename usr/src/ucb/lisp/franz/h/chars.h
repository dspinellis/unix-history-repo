/*					-[Sat Jan 29 13:52:05 1983 by jkf]-
 * 	chars.h				$Locker:  $
 * lexical table 
 *
 * $Header: /na/franz/franz/h/chars.h,v 1.1 83/01/29 14:03:08 jkf Exp $
 *
 * (c) copyright 1982, Regents of the University of California
 */

/* the format of the entries are:       ab..xxxx			*/
/*									*/
/* where a is set iff the atom containing the symbol must be quoted	*/
/* where b is set iff the character separates atoms normally		*/
/* where xxxx is a number unique to the class of symbol			*/

#define	CNUM	0000
#define CSIGN   0001
#define CCHAR	0002
#define	CLPARA	0003
#define CRPARA	0004
#define CPERD	0005
#define	CLBRCK	0006
#define	CRBRCK	0007
#define	CSQ	0011
#define	CDQ	0012
#define	CERR	0013
#define	CSEP	0014
#define CSPL	0015
#define CMAC	0016
#define CESC	0017
#define CSCA	0020
#define CSD	0021
#define CSMAC	0022
#define CSSPL	0023
#define CINF	0024
#define CSINF	0025

#define	VNUM	0000
#define VMINUS	0001
#define VSIGN   0001
#define VCHAR	0002
#define	VLPARA	(CLPARA|QALWAYS|SEPMASK)
#define VRPARA	(CRPARA|QALWAYS|SEPMASK)
#define VPERD	(CPERD|QWNUNIQ)
#define	VLBRCK	(CLBRCK|QALWAYS|SEPMASK)
#define	VRBRCK	(CRBRCK|QALWAYS|SEPMASK)
#define	VSQ	(CSQ|QALWAYS|SEPMASK)
#define	VDQ	(CDQ|QALWAYS)
#define	VERR	(CERR|QALWAYS|SEPMASK)
#define	VSEP	(CSEP|QALWAYS|SEPMASK)
#define VSPL	(CSPL|QALWAYS|SEPMASK)
#define VMAC	(CMAC|QALWAYS|SEPMASK)
#define VESC	(CESC|QALWAYS)
#define VSCA	(CSCA|SEPMASK)
#define VSD	(CSD|QALWAYS)
#define VSMAC	(CSMAC|QWNUNIQ)
#define VSSPL	(CSSPL|QWNUNIQ)
#define VINF	0024
#define VSINF	(CSINF|QWNUNIQ)


#define QUTMASK	0300
#define	SEPMASK	0040
#define QALWAYS 0300
#define QWNUNIQ 0100
#define QWNFRST 0200

#define synclass(p) (p & 037)

#define TSCA	1
#define TLPARA	2
#define TRPARA	3
#define TPERD	4
#define TEOF	5
#define TSPL	6
#define TMAC	7
#define TSQ	8
#define TLBKT	9
#define TINF	10
