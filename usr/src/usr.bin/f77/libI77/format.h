/*
char id_format[] = "@(#)format.h	1.1";
 *
 * format parser definitions
 */

struct syl
{
	int op,p1,p2,p3;
};

#define RET	1
#define REVERT 	2
#define GOTO 	3
#define X 	4
#define SLASH 	5
#define STACK 	6
#define I 	7
#define ED 	8
#define NED 	9
#define IM 	10
#define APOS 	11
#define H 	12
#define TL 	13
#define TR 	14
#define T 	15
#define COLON 	16
#define S 	17
#define SP 	18
#define SS 	19
#define P 	20
#define BN 	21
#define BZ 	22
#define F 	23
#define E 	24
#define EE 	25
#define D 	26
#define DE	27		/*** NOT STANDARD FORTRAN ***/
#define G 	28
#define GE 	29
#define L 	30
#define A 	31
#define AW	32
#define R	33		/*** NOT STANDARD FORTRAN ***/
#define DOLAR	34		/*** NOT STANDARD FORTRAN ***/
#define SU	35		/*** NOT STANDARD FORTRAN ***/

#define FMTOK	1
#define FMTERR	0

extern struct syl syl[];
extern int pc,parenlvl,revloc;

