/****** A Version of the famous dhrystone benchmark ******* */
/* 
 Sun Oct  1 10:40:53 1989  Doug Lea  (dl at g.oswego.edu)
 Changes made to the standard version to run under C++:

 * include standard  prototypes for printf and exit
 * include Int.h and Char.h
 * convert all `int' and `char' to `Int' and `Char' 
    (except those needed for timing)
 * predeclare all functions
 * convert all function headers from old-C
 * Change name of struct tms tms to Tms
 * wrote mystrcpy and mystrcmp to handle Chars, not chars
 * added coercion to bad-looking Proc3 call in Proc1
 * initialized String1Loc in Proc0
 * use `new' instead of malloc


This program is marginally useful in looking at the effects of various
C++ constructs when defining a heavily constructive class like
Int and Char. The Int.h and Char.h files can be compiled several
ways, as listed at the top of each

*/

#include <_G_config.h>
#include "Int.h"
#include "Char.h"

/*
 *	"DHRYSTONE" Benchmark Program
 *
 *	Version:	C/1, 12/01/84
 *
 *	Date:		PROGRAM updated 11/02/85, RESULTS updated 12/13/85
 *
 *	Author:		Reinhold P. Weicker,  CACM Vol 27, No 10, 10/84 pg. 1013
 *			Translated from ADA by Rick Richardson
 *			Every method to preserve ADA-likeness has been used,
 *			at the expense of C-ness.
 *
 *	Compile:	cc -O dry.c -o drynr			: No registers
 *			cc -O -DREG=register dry.c -o dryr	: Registers
 *
 *	Defines:	Defines are provided for old C compiler's
 *			which don't have enums, and can't assign structures.
 *			The time(2) function is library dependant; Most
 *			return the time in seconds, but beware of some, like
 *			Aztec C, which return other units.
 *			The LOOPS define is initially set for 50000 loops.
 *			If you have a machine with large integers and is
 *			very fast, please change this number to 500000 to
 *			get better accuracy.  Please select the way to
 *			measure the execution time using the TIME define.
 *			For single user machines, time(2) is adequate. For
 *			multi-user machines where you cannot get single-user
 *			access, use the times(2) function.  If you have
 *			neither, use a stopwatch in the dead of night.
 *			Use a "printf" at the point marked "start timer"
 *			to begin your timings. DO NOT use the UNIX "time(1)"
 *			command, as this will measure the total time to
 *			run this program, which will (erroneously) include
 *			the time to malloc(3) storage and to compute the
 *			time it takes to do nothing.
 *
 *	Run:		drynr; dryr
 *
 *	Results:	If you get any new machine/OS results, please send to:
 *
 *				{ihnp4,vax135,..}!houxm!castor!pcrat!rer
 *
 *			and thanks to all that do.  Space prevents listing
 *			the names of those who have provided some of these
 *			results.
 *
 *	Note:		I order the list in increasing performance of the
 *			"with registers" benchmark.  If the compiler doesn't
 *			provide register variables, then the benchmark
 *			is the same for both REG and NOREG.  I'm not going
 *			to list a compiler in a better place because if it
 *			had register variables it might do better. No
 *			register variables is a big loss in my book.
 *
 *	PLEASE:		Send complete information about the machine type,
 *			clock speed, OS and C manufacturer/version.  If
 *			the machine is modified, tell me what was done.
 *			On UNIX, execute uname -a and cc -V to get this info.
 *
 *	80x8x NOTE:	80x8x benchers: please try to do all memory models
 *			for a particular compiler.
 *
 *--------------------------------RESULTS BEGIN--------------------------------
 *
 * MACHINE	MICROPROCESSOR	OPERATING	COMPILER	DHRYSTONES/SEC.
 * TYPE				SYSTEM				NO REG	REGS
 * --------------------------	------------	-----------	---------------
 * Commodore 64	6510-1MHz	C64 ROM		C Power 2.8	  36	  36
 * HP-110	8086-5.33Mhz	MSDOS 2.11	Lattice 2.14	 284	 284
 * IBM PC/XT	8088-4.77Mhz	PC/IX		cc		 257	 287
 * P-E 3205	?		Xelos(SVR2) 	cc		 279	 296
 * Perq-II	2901 bitslice	Accent S5c 	cc (CMU)	 301	 301
 * IBM PC/XT	8088-4.77Mhz	COHERENT 2.3.43	MarkWilliams cc  296	 317
 * Cosmos	68000-8Mhz	UniSoft		cc		 305	 322
 * IBM PC/XT	8088-4.77Mhz	Venix/86 2.0	cc		 297	 324
 * DEC PRO 350  11/23           Venix/PRO SVR2  cc               299     325
 * PC/XT        8088-4.77Mhz    Venix/86 SYS V  cc               339     377
 * IBM PC	8088-4.77Mhz	MSDOS 2.0	b16cc 2.0	 310	 340
 * Commodore Amiga		?		Lattice 3.02	 368	 371
 * IBM PC	8088-4.77Mhz	MSDOS 2.0	CI-C86 2.20M	 390	 390
 * IBM PC/XT	8088-4.77Mhz	PCDOS 2.1	Wizard 2.1	 367	 403
 * IBM PC/XT	8088-4.77Mhz	PCDOS 3.1	Lattice 2.15	 403	 403 @
 * IBM PC	8088-4.77Mhz	PCDOS 3.1	Datalight 1.10	 416	 416
 * IBM PC/XT	8088-4.77Mhz	PCDOS 2.1	Microsoft 3.0	 390	 427
 * PDP-11/34	-		UNIX V7M	cc		 387	 438
 * IBM PC	8088, 4.77mhz	PC-DOS 2.1	Aztec C v3.2d	 423	 454
 * Tandy 1000	V20, 4.77mhz	MS-DOS 2.11	Aztec C v3.2d	 423	 458
 * PDP-11/34	-		RSTS/E		decus c		 438	 495
 * Onyx C8002	Z8000-4Mhz	IS/1 1.1 (V7)	cc		 476	 511
 * Perkin-Elmer 3230		Xelos (SysV.2)	cc		 507	 565
 * DEC PRO 380  11/73           Venix/PRO SVR2  cc               577     628
 * FHL QT+	68000-10Mhz	Os9/68000	version 1.3	 603	 649 FH
 * Apollo DN550	68010-?Mhz	AegisSR9/IX	cc 3.12		 666	 666
 * HP-110	8086-5.33Mhz	MSDOS 2.11	Aztec-C		 641	 676 
 * ATT PC6300	8086-8Mhz	MSDOS 2.11	b16cc 2.0	 632	 684
 * IBM PC/AT	80286-6Mhz	PCDOS 3.0	CI-C86 2.1	 666	 684
 * Tandy 6000	68000-8Mhz	Xenix 3.0	cc		 694	 694
 * IBM PC/AT	80286-6Mhz	Xenix 3.0	cc		 684	 704 MM
 * Macintosh	68000-7.8Mhz 2M	Mac Rom		Mac C 32 bit int 694	 704
 * Macintosh	68000-7.7Mhz	-		MegaMax C 2.0	 661	 709
 * IBM PC/AT	80286-6Mhz	Xenix 3.0	cc		 704	 714 LM
 * Codata 3300	68000-8Mhz	UniPlus+ (v7)	cc		 678	 725
 * Cadmus 9000	68010-10Mhz	UNIX		cc		 714	 735
 * AT&T 6300    8086-8Mhz       Venix/86 SVR2   cc               668     743
 * Cadmus 9790	68010-10Mhz 1MB	SVR0,Cadmus3.7	cc		 720	 747
 * NEC PC9801F	8086-8Mhz	PCDOS 2.11	Lattice 2.15	 768	  -  @
 * ATT PC6300	8086-8Mhz	MSDOS 2.11	CI-C86 2.20M	 769	 769
 * Burroughs XE550 68010-10Mhz	Centix 2.10	cc		 769	 769 CT1
 * EAGLE/TURBO  8086-8Mhz       Venix/86 SVR2   cc               696     779
 * ALTOS 586	8086-10Mhz	Xenix 3.0b	cc 		 724	 793
 * DEC 11/73	J-11 micro	Ultrix-11 V3.0	System V	 735	 793
 * ATT 3B2/300	WE32000-?Mhz	UNIX 5.0.2	cc		 735	 806
 * Apollo DN320	68010-?Mhz	AegisSR9/IX	cc 3.12		 806	 806
 * IRIS-2400	68010-10Mhz	UNIX System V	cc		 772	 829
 * Atari 520ST  68000-8Mhz      TOS             DigResearch      839     846
 * IBM PC/AT	80286-6Mhz	PCDOS 3.0	MS 3.0(large)	 833	 847 LM
 * VAX 11/750	-		Ultrix 1.1	4.2BSD cc	 781	 862
 * P-E  7350A	68000-8MHz	UniSoft V.2	cc		 821	 875
 * VAX 11/750	-		UNIX 4.2bsd	cc		 862	 877
 * Fast Mac	68000-7.7Mhz	-		MegaMax C 2.0	 839	 904 +
 * IBM PC/XT	8086-9.54Mhz	PCDOS 3.1	Microsoft 3.0	 833	 909 C1
 * DEC 11/44			Ultrix-11 V3.0	System V	 862	 909
 * Macintosh	68000-7.8Mhz 2M	Mac Rom		Mac C 16 bit int 877	 909 S
 * P-E 3210	?		Xelos R01(SVR2)	cc		 849	 924
 * P-E 3220	?               Ed. 7 v2.3      cc		 892	 925
 * IBM PC/AT	80286-6Mhz	Xenix 3.0	cc -i		 909	 925
 * AT&T 6300	8086, 8mhz	MS-DOS 2.11	Aztec C v3.2d	 862	 943
 * IBM PC/AT	80286-6Mhz	Xenix 3.0	cc		 892	 961
 * VAX 11/750	w/FPA		Eunice 3.2	cc		 914	 976
 * IBM PC/XT	8086-9.54Mhz	PCDOS 3.1	Wizard 2.1	 892	 980 C1
 * IBM PC/XT	8086-9.54Mhz	PCDOS 3.1	Lattice 2.15	 980	 980 C1
 * Plexus P35	68000-10Mhz	UNIX System III cc		 984	 980
 * PDP-11/73	KDJ11-AA 15Mhz	UNIX V7M 2.1	cc		 862     981
 * VAX 11/750	w/FPA		UNIX 4.3bsd	cc		 994	 997
 * IRIS-1400	68010-10Mhz	UNIX System V	cc		 909	1000
 * IBM PC/AT	80286-6Mhz	Venix/86 2.1	cc		 961	1000
 * IBM PC/AT	80286-6Mhz	PCDOS 3.0	b16cc 2.0	 943	1063
 * Zilog S8000/11 Z8001-5.5Mhz	Zeus 3.2	cc		1011	1084
 * NSC ICM-3216 NSC 32016-10Mhz	UNIX SVR2	cc		1041	1084
 * IBM PC/AT	80286-6Mhz	PCDOS 3.0	MS 3.0(small)	1063	1086
 * VAX 11/750	w/FPA		VMS		VAX-11 C 2.0	 958	1091
 * Stride	68000-10Mhz	System-V/68	cc		1041	1111
 * ATT PC7300	68010-10Mhz	UNIX 5.2	cc		1041	1111
 * P-E 3230	?		Xelos R01(SVR2)	cc		1040	1126
 * Stride	68000-12Mhz	System-V/68	cc		1063	1136
 * IBM PC/AT    80286-6Mhz      Venix/286 SVR2  cc              1056    1149
 * IBM PC/AT	80286-6Mhz	PCDOS 3.0	Datalight 1.10	1190	1190
 * ATT PC6300+	80286-6Mhz	MSDOS 3.1	b16cc 2.0	1111	1219
 * IBM PC/AT	80286-6Mhz	PCDOS 3.1	Wizard 2.1	1136	1219
 * Sun2/120	68010-10Mhz	Sun 4.2BSD	cc		1136	1219
 * IBM PC/AT	80286-6Mhz	PCDOS 3.0	CI-C86 2.20M	1219	1219
 * MASSCOMP 500	68010-10MHz	RTU V3.0	cc (V3.2)	1156	1238
 * Cyb DataMate	68010-12.5Mhz	Uniplus 5.0	Unisoft cc	1162	1250
 * PDP 11/70	-		UNIX 5.2	cc		1162	1250
 * IBM PC/AT	80286-6Mhz	PCDOS 3.1	Lattice 2.15	1250	1250
 * IBM PC/AT	80286-7.5Mhz	Venix/86 2.1	cc		1190	1315 *15
 * Sun2/120	68010-10Mhz	Standalone	cc		1219	1315
 * Intel 380	80286-8Mhz	Xenix R3.0up1	cc		1250	1315 *16
 * ATT 3B2/400	WE32100-?Mhz	UNIX 5.2	cc		1315	1315
 * P-E 3250XP	-		Xelos R01(SVR2)	cc		1215	1318
 * DG MV4000	-		AOS/VS 5.00	cc		1333	1333
 * IBM PC/AT	80286-8Mhz	Venix/86 2.1	cc		1275	1380 *16
 * IBM PC/AT	80286-6Mhz	MSDOS 3.0	Microsoft 3.0	1250	1388
 * ATT PC6300+	80286-6Mhz	MSDOS 3.1	CI-C86 2.20M	1428	1428
 * COMPAQ/286   80286-8Mhz      Venix/286 SVR2  cc              1326    1443
 * IBM PC/AT    80286-7.5Mhz    Venix/286 SVR2  cc              1333    1449 *15
 * Cyb DataMate	68010-12.5Mhz	Uniplus 5.0	Unisoft cc	1470	1562 S
 * VAX 11/780	-		UNIX 5.2	cc		1515	1562
 * MicroVAX-II	-		-		-		1562	1612
 * VAX 11/780	-		UNIX 4.3bsd	cc		1646	1662
 * Apollo DN660	-		AegisSR9/IX	cc 3.12		1666	1666
 * ATT 3B20	-		UNIX 5.2	cc		1515	1724
 * NEC PC-98XA	80286-8Mhz	PCDOS 3.1	Lattice 2.15	1724	1724 @
 * HP9000-500	B series CPU	HP-UX 4.02	cc		1724	-
 * IBM PC/STD	80286-8Mhz	MSDOS 3.0 	Microsoft 3.0	1724	1785 C2
 * DEC-2065	KL10-Model B	TOPS-20 6.1FT5	Port. C Comp.	1937	1946
 * Gould PN6005	-		UTX 1.1(4.2BSD)	cc		1675	1964
 * DEC2060	KL-10		TOPS-20		cc		2000	2000 &
 * VAX 11/785	-		UNIX 5.2	cc		2083	2083
 * VAX 11/785	-		VMS		VAX-11 C 2.0	2083	2083
 * VAX 11/785	-		UNIX SVR2	cc		2123	2083
 * VAX 11/785	-		UNIX 4.3bsd	cc		2135	2136
 * Pyramid 90x	-		OSx 2.3		cc		2272	2272
 * Pyramid 90x	FPA,cache,4Mb	OSx 2.5		cc no -O	2777	2777
 * Alliant FX-8 CE		?		?		2622	2901 FX
 * Pyramid 90x	w/cache		OSx 2.5		cc w/-O		3333	3333
 * IBM-4341-II	-		VM/SP3		Waterloo C 1.2  3333	3333
 * IRIS-2400T	68020-16.67Mhz	UNIX System V	cc		3105	3401
 * SUN 3/75	68020-16.67Mhz	SUN 4.2 V3	cc		3333	3571
 * IBM-4341	Model 12	UTS 5.0		?		3685	3685
 * SUN-3/160    68020-16.67Mhz  Sun 4.2 V3.0A   cc		3381    3764
 * Sun 3/180	68020-16.67Mhz	Sun 4.2		cc		3333	3846
 * IBM-4341	Model 12	UTS 5.0		?		3910	3910 MN
 * MC 5400	68020-16.67MHz	RTU V3.0	cc (V4.0)	3952	4054
 * NCR Tower32  68020-16.67Mhz  SYS 5.0 Rel 2.0 cc              3846	4545
 * Gould PN9080	-		UTX-32 1.1c	cc		-	4629
 * MC 5600/5700	68020-16.67MHz	RTU V3.0	cc (V4.0)	4504	4746 %
 * Gould 1460-342 ECL proc      UTX/32 1.1/c    cc              5342    5677 G1
 * VAX 8600	-		UNIX 4.3bsd	cc		7024	7088
 * VAX 8600	-		VMS		VAX-11 C 2.0	7142	7142
 * CCI POWER 6/32		COS(SV+4.2)	cc		7500	7800
 * CCI POWER 6/32		POWER 6 UNIX/V	cc		8236	8498
 * CCI POWER 6/32		4.2 Rel. 1.2b	cc		8963	9544
 * Sperry (CCI Power 6)		4.2BSD		cc		9345   10000
 * CRAY-X-MP/12	   105Mhz	COS 1.14	Cray C         10204   10204
 * IBM-3083	-		UTS 5.0 Rel 1	cc	       16666   12500
 * CRAY-1A	    80Mhz	CTSS		Cray C 2.0     12100   13888
 * IBM-3083	-		VM/CMS HPO 3.4	Waterloo C 1.2 13889   13889
 * Amdahl 470 V/8 		UTS/V 5.2       cc v1.23       15560   15560
 * CRAY-X-MP/48	   105Mhz	CTSS		Cray C 2.0     15625   17857
 * Amdahl 580	-		UTS 5.0 Rel 1.2	cc v1.5        23076   23076
 * Amdahl 5860	 		UTS/V 5.2       cc v1.23       28970   28970
 *
 *   *   Crystal changed from 'stock' to listed value.
 *   +   This Macintosh was upgraded from 128K to 512K in such a way that
 *       the new 384K of memory is not slowed down by video generator accesses.
 *   %   Single processor; MC == MASSCOMP
 *   &   A version 7 C compiler written at New Mexico Tech.
 *   @   vanilla Lattice compiler used with MicroPro standard library
 *   S   Shorts used instead of ints
 *   LM  Large Memory Model. (Otherwise, all 80x8x results are small model)
 *   MM  Medium Memory Model. (Otherwise, all 80x8x results are small model)
 *   C1  Univation PC TURBO Co-processor; 9.54Mhz 8086, 640K RAM
 *   C2  Seattle Telecom STD-286 board
 *   C?  Unknown co-processor board?
 *   CT1 Convergent Technologies MegaFrame, 1 processor.
 *   MN  Using Mike Newtons 'optimizer' (see net.sources).
 *   G1  This Gould machine has 2 processors and was able to run 2 dhrystone
 *       Benchmarks in parallel with no slowdown.
 *   FH  FHC == Frank Hogg Labs (Hazelwood Uniquad 2 in an FHL box).
 *   FX  The FX-8 has two kinds of processors.  This figure is for CE's
 *	 (computation engines).  The other processor type is an IP (interactive
 *	 processor) which is a 68010-12Mhz. Figures were not precisely
 *	 determined for the IP.
 *   ?   I don't trust results marked with '?'.  These were sent to me with
 *       either incomplete info, or with times that just don't make sense.
 *	 ?? means I think the performance is too poor, ?! means too good.
 *       If anybody can confirm these figures, please respond.
 *
 *--------------------------------RESULTS END----------------------------------
 *
 *	The following program contains statements of a high-level programming
 *	language (C) in a distribution considered representative:
 *
 *	assignments			53%
 *	control statements		32%
 *	procedure, function calls	15%
 *
 *	100 statements are dynamically executed.  The program is balanced with
 *	respect to the three aspects:
 *		- statement type
 *		- operand type (for simple data types)
 *		- operand access
 *			operand global, local, parameter, or constant.
 *
 *	The combination of these three aspects is balanced only approximately.
 *
 *	The program does not compute anything meaningfull, but it is
 *	syntactically and semantically correct.
 *
 */

/* Accuracy of timings and human fatigue controlled by next two lines */
#ifdef QUICK
#define LOOPS	50000		/* Use this for slow or 16 bit machines */
#else
#define LOOPS	500000		/* Use this for faster machines */
#endif

/* Compiler dependent options */
#undef	NOENUM			/* Define if compiler has no enum's */
#undef	NOSTRUCTASSIGN		/* Define if compiler can't assign structures */

/* define only one of the next two defines */
#ifndef _G_SYSV
#define TIMES			/* Use times(2) time function */
#else
#define TIME			/* Use time(2) time function */
#endif

#ifdef TIMES
#include <sys/types.h>
#include <sys/times.h>
#endif
#ifdef TIME
#include <time.h>
#endif

/* define the granularity of your times(2) function (when used) */
#ifndef HZ
#if 1
#define HZ	60		/* times(2) returns 1/60 second (most) */
#else
#define HZ	100		/* times(2) returns 1/100 second (WECo) */
#endif
#endif


#ifdef	NOSTRUCTASSIGN
#define	structassign(d, s)	memcpy(&(d), &(s), sizeof(d))
#else
#define	structassign(d, s)	d = s
#endif

#ifdef	NOENUM
#define	Ident1	1
#define	Ident2	2
#define	Ident3	3
#define	Ident4	4
#define	Ident5	5
typedef int	Enumeration;
#else
typedef enum	{Ident1, Ident2, Ident3, Ident4, Ident5} Enumeration;
#endif



typedef Int	OneToThirty;
typedef Int	OneToFifty;
typedef Char	CapitalLetter;
typedef Char	String30[31];
typedef Int	Array1Dim[51];
typedef Int	Array2Dim[51][51];

struct	Record
{
	struct Record		*PtrComp;
	Enumeration		Discr;
	Enumeration		EnumComp;
	OneToFifty		IntComp;
	String30		StringComp;
};

typedef struct Record 	RecordType;
typedef RecordType *	RecordPtr;
typedef int		boolean;

#define	TRUE		1
#define	FALSE		0

#ifndef REG
#define	REG
#endif


/* added: - dl */

extern "C" {
extern int printf(const char* ...);
extern void exit(int);
}

void Proc0();
void Proc1(RecordPtr PtrParIn);
void Proc2(OneToFifty	*IntParIO);
void Proc3(RecordPtr	*PtrParOut);
void Proc4();
void Proc5();
boolean Func3(Enumeration	EnumParIn);
void Proc6(REG Enumeration	EnumParIn, REG Enumeration	*EnumParOut);
void Proc7(OneToFifty IntParI1, OneToFifty IntParI2, OneToFifty *IntParOut);
void Proc8(Array1Dim	Array1Par, 
      Array2Dim	Array2Par, 
      OneToFifty IntParI1, 
      OneToFifty IntParI2);
Enumeration Func1(CapitalLetter	CharPar1, CapitalLetter	CharPar2);
boolean Func2(String30	StrParI1, String30	StrParI2);
boolean Func3(Enumeration	EnumParIn);

void mystrcpy(String30 s, char* t)
{
  for (; *t != '\0'; ++s, ++t) *s = *t;
  *s = '\0';
}

char mystrcmp(String30 s, String30 t)
{
  for (; *s == *t; ++s, ++t) if (*s == '\0') return 0;
  return char(*s - *t);
}

/*end - dl */

main()
{
	Proc0();
	exit(0);
}

/*
 * Package 1
 */
Int		IntGlob;
boolean		BoolGlob;
char		Char1Glob;
char		Char2Glob;
Array1Dim	Array1Glob;
Array2Dim	Array2Glob;
RecordPtr	PtrGlb;
RecordPtr	PtrGlbNext;

void Proc0()
{
	OneToFifty		IntLoc1;
	REG OneToFifty		IntLoc2;
	OneToFifty		IntLoc3;
	REG char		CharLoc;
	REG char		CharIndex;
	Enumeration	 	EnumLoc;
	String30		String1Loc;
	String30		String2Loc;

#ifdef TIME
	long			starttime;
	long			benchtime;
	long			nulltime;
	register unsigned int	i;

	starttime = time( (_G_time_t *) 0);
	for (i = 0; i < LOOPS; ++i);
	nulltime = time( (_G_time_t*) 0) - starttime; /* Computes o'head of loop */
#endif
#ifdef TIMES
	time_t			starttime;
	time_t			benchtime;
	time_t			nulltime;
	struct tms		Tms;
	register unsigned int	i;

	times(&Tms); starttime = Tms.tms_utime;
	for (i = 0; i < LOOPS; ++i);
	times(&Tms);
	nulltime = Tms.tms_utime - starttime; /* Computes overhead of looping */
#endif

	PtrGlbNext = new Record;
	PtrGlb = new Record;
	PtrGlb->PtrComp = PtrGlbNext;
	PtrGlb->Discr = Ident1;
	PtrGlb->EnumComp = Ident3;
	PtrGlb->IntComp = 40;
	mystrcpy(PtrGlb->StringComp, "DHRYSTONE PROGRAM, SOME STRING");
	mystrcpy(String1Loc, "JUST INITIALIZED TO SOME JUNK.");

/*****************
-- Start Timer --
*****************/
#ifdef TIME
	starttime = time( (_G_time_t*) 0);
#endif
#ifdef TIMES
	times(&Tms); starttime = Tms.tms_utime;
#endif
	for (i = 0; i < LOOPS; ++i)
	{

		Proc5();
		Proc4();
		IntLoc1 = 2;
		IntLoc2 = 3;
		mystrcpy(String2Loc, "DHRYSTONE PROGRAM, 2'ND STRING");
		EnumLoc = Ident2;
		BoolGlob = ! Func2(String1Loc, String2Loc);
		while (IntLoc1 < IntLoc2)
		{
			IntLoc3 = 5 * IntLoc1 - IntLoc2;
			Proc7(IntLoc1, IntLoc2, &IntLoc3);
			++IntLoc1;
		}
		Proc8(Array1Glob, Array2Glob, IntLoc1, IntLoc3);
		Proc1(PtrGlb);
		for (CharIndex = 'A'; CharIndex <= Char2Glob; ++CharIndex)
			if (EnumLoc == Func1(CharIndex, 'C'))
				Proc6(Ident1, &EnumLoc);
		IntLoc3 = IntLoc2 * IntLoc1;
		IntLoc2 = IntLoc3 / IntLoc1;
		IntLoc2 = 7 * (IntLoc3 - IntLoc2) - IntLoc1;
		Proc2(&IntLoc1);
	}

/*****************
-- Stop Timer --
*****************/

#ifdef TIME
	benchtime = time( (_G_time_t *) 0) - starttime - nulltime;
	printf("Dhrystone time for %ld passes = %ld\n",
		(long) LOOPS, benchtime);
	printf("This machine benchmarks at %ld dhrystones/second\n",
		((long) LOOPS) / benchtime);
#endif
#ifdef TIMES
	times(&Tms);
	benchtime = Tms.tms_utime - starttime - nulltime;
	printf("Dhrystone time for %ld passes = %ld\n",
		(long) LOOPS, benchtime/HZ);
	printf("This machine benchmarks at %ld dhrystones/second\n",
		((long) LOOPS) * HZ / benchtime);
#endif

}

void Proc1(RecordPtr PtrParIn)
{
#define	NextRecord	(*(PtrParIn->PtrComp))

	structassign(NextRecord, *PtrGlb);
	PtrParIn->IntComp = 5;
	NextRecord.IntComp = PtrParIn->IntComp;
	NextRecord.PtrComp = PtrParIn->PtrComp;
/* - added coercion (glossing over error in original code) - dl */
	Proc3(&(NextRecord.PtrComp));
	if (NextRecord.Discr == Ident1)
	{
		NextRecord.IntComp = 6;
		Proc6(PtrParIn->EnumComp, &NextRecord.EnumComp);
		NextRecord.PtrComp = PtrGlb->PtrComp;
		Proc7(NextRecord.IntComp, 10, &NextRecord.IntComp);
	}
	else
		structassign(*PtrParIn, NextRecord);

#undef	NextRecord
}

void Proc2(OneToFifty	*IntParIO)
{
	REG OneToFifty		IntLoc;
	REG Enumeration		EnumLoc;

	IntLoc = *IntParIO + 10;
	for(;;)
	{
		if (Char1Glob == 'A')
		{
			--IntLoc;
			*IntParIO = IntLoc - IntGlob;
			EnumLoc = Ident1;
		}
		if (EnumLoc == Ident1)
			break;
	}
}

void Proc3(RecordPtr	*PtrParOut)
{
	if (PtrGlb)
		*PtrParOut = PtrGlb->PtrComp;
	else
		IntGlob = 100;
	Proc7(10, IntGlob, &PtrGlb->IntComp);
}

void Proc4()
{
	REG boolean	BoolLoc;

	BoolLoc = Char1Glob == 'A';
	BoolLoc |= BoolGlob;
	Char2Glob = 'B';
}

void Proc5()
{
	Char1Glob = 'A';
	BoolGlob = FALSE;
}




void Proc6(REG Enumeration	EnumParIn, REG Enumeration	*EnumParOut)
{
	*EnumParOut = EnumParIn;
	if (! Func3(EnumParIn) )
		*EnumParOut = Ident4;
	switch (EnumParIn)
	{
	case Ident1:	*EnumParOut = Ident1; break;
	case Ident2:	if (IntGlob > 100) *EnumParOut = Ident1;
			else *EnumParOut = Ident4;
			break;
	case Ident3:	*EnumParOut = Ident2; break;
	case Ident4:	break;
	case Ident5:	*EnumParOut = Ident3;
	}
}

void Proc7(OneToFifty IntParI1, OneToFifty IntParI2, OneToFifty *IntParOut)
{
	REG OneToFifty	IntLoc;

	IntLoc = IntParI1 + 2;
	*IntParOut = IntParI2 + IntLoc;
}

void Proc8(Array1Dim	Array1Par, 
      Array2Dim	Array2Par, 
      OneToFifty IntParI1, 
      OneToFifty IntParI2)
{
	REG OneToFifty	IntLoc;
	REG OneToFifty	IntIndex;

	IntLoc = IntParI1 + 5;
	Array1Par[IntLoc] = IntParI2;
	Array1Par[IntLoc+1] = Array1Par[IntLoc];
	Array1Par[IntLoc+30] = IntLoc;
	for (IntIndex = IntLoc; IntIndex <= (IntLoc+1); ++IntIndex)
		Array2Par[IntLoc][IntIndex] = IntLoc;
	++Array2Par[IntLoc][IntLoc-1];
	Array2Par[IntLoc+20][IntLoc] = Array1Par[IntLoc];
	IntGlob = 5;
}

Enumeration Func1(CapitalLetter	CharPar1, CapitalLetter	CharPar2)
{
	REG CapitalLetter	CharLoc1;
	REG CapitalLetter	CharLoc2;

	CharLoc1 = CharPar1;
	CharLoc2 = CharLoc1;
	if (CharLoc2 != CharPar2)
		return (Ident1);
	else
		return (Ident2);
}

boolean Func2(String30	StrParI1, String30	StrParI2)
{
	REG OneToThirty		IntLoc;
	REG CapitalLetter	CharLoc;

	IntLoc = 1;
	while (IntLoc <= 1)
		if (Func1(StrParI1[IntLoc], StrParI2[IntLoc+1]) == Ident1)
		{
			CharLoc = 'A';
			++IntLoc;
		}
	if (CharLoc >= 'W' && CharLoc <= 'Z')
		IntLoc = 7;
	if (CharLoc == 'X')
		return(TRUE);
	else
	{
		if (mystrcmp(StrParI1, StrParI2) > 0)
		{
			IntLoc += 7;
			return (TRUE);
		}
		else
			return (FALSE);
	}
}

boolean Func3(Enumeration	EnumParIn)
{
	REG Enumeration	EnumLoc;

	EnumLoc = EnumParIn;
	if (EnumLoc == Ident3) return (TRUE);
	return (FALSE);
}

#ifdef	NOSTRUCTASSIGN
memcpy(d, s, l)
register char	*d;
register char	*s;
int	l;
{
	while (l--) *d++ = *s++;
}
#endif


