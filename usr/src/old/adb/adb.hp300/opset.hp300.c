/*
 * Copyright (c) 1988, 1989 University of Utah.
 * All rights reserved.  The Utah Software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	Utah $Hdr: opset.hp300.c 1.26 90/02/21$
 */

/*
 * 68020 + 68881 instruction decoder
 * Originated from the mit adb68 code
 */
#include "defs.h"

INT	dotinc;
POS	space;

char *badop = "\t???";

static char _areg[3] = "a0";
static char _dreg[3] = "d0";
#define A(reg)	((reg) == 7 ? "sp" : (_areg[1] = '0' + (reg), _areg))
#define D(reg)	(_dreg[1] = '0' + (reg), _dreg)

/* 68881 data types */
#define	TLONG	0
#define	TSPREAL	1
#define	TXPREAL	2
#define	TPDREAL	3
#define	TWORD	4
#define	TDPREAL	5
#define	TBYTE	6
#define	TDPDREAL	7

char *bname[16] = { "ra", "sr", "hi", "ls", "cc", "cs", "ne",
		    "eq", "vc", "vs", "pl", "mi", "ge", "lt", "gt", "le" };

char *shro[4] = { "as", "ls", "rox", "ro" };

char *bit[4] = { "btst", "bchg", "bclr", "bset" };

char *creg1[] = { "sfc", "dfc", "cacr", "tc", "itt0", "itt1", "dtt0", "dtt1" };
char *creg2[] = { "usp", "vbr", "caar", "msp", "isp", "mmusr", "urp", "srp" };

int omove(),obranch(),oimmed(),ouimmed(),oprint(),oneop(),soneop(),oreg();
int ochk(),olink(),omovem(),oquick(),omoveq(),otrap(),oscc(),opmode(),shroi();
int extend(),biti(),odbcc(),omovec(),odivmul(), omoves();
int ochk2(), ocall(), ocas(), otrapcc(), ortm(), ortd(), obitfield();
int omovep(), omover(), odivmull();
int ofloat(), ofdbcc(), oftrapcc(), ofscc(), ofbranch(), ofsaverestore();
int ocpgen(), ocpdbcc(), ocptrapcc(), ocpscc(), ocpbranch(), ocpsaverestore();

struct opdesc
{			
	unsigned short mask, match;
	int (*opfun)();
	char *farg;
} opdecode[] =
{	/* opcodes are grouped by high nibbles */
	/* within groups, masks and compares are performed serially */
  0xF9C0, 0x00C0, ochk2, 0,
  0xFF00, 0x0000, ouimmed, "or",	/* op class 0  */
  0xF138, 0x0108, omovep, 0,		/* ordered before btst (biti) */
  0xF100, 0x0100, biti, 0,
  0xFF00, 0x0200, ouimmed, "and",
  0xFF00, 0x0400, oimmed, "sub",
  0xFFF0, 0x06C0, ortm, 0,		/* ordered before addi */
  0xFFC0, 0x06C0, ocall, 0,		/* ordered before addi */
  0xFF00, 0x0600, oimmed, "add",
  0xFF00, 0x0800, biti, 0,
  0xF9C0, 0x08C0, ocas, 0,		/* 0x0AC0, 0x0CC0, 0x0EC0 */
  0xFF00, 0x0A00, ouimmed, "eor",
  0xFF00, 0x0C00, oimmed, "cmp",
  0xFF00, 0x0E00, omoves, 0,
  0xF000, 0x1000, omove, "b",		/* move instructions */
  0xF000, 0x2000, omove, "l",
  0xF000, 0x3000, omove, "w",
  0xF9C0, 0x40C0, omover, 0,		/* 0x40C0, 0x42C0, 0x44C0, 0x46C0 */
  0xFF00, 0x4000, soneop, "negx",
  0xFF00, 0x4200, soneop, "clr",
  0xFF00, 0x4400, soneop, "neg",
  0xFF00, 0x4600, soneop, "not",
  0xFFF8, 0x4808, olink, "l",
  0xFFC0, 0x4800, oneop, "nbcd",
  0xFFF8, 0x4840, oreg, "\tswap\td%X",
  0xFFC0, 0x4840, oneop, "pea",
  0xFFF8, 0x4848, oreg, "\tbkpt\t#%X",
  0xFFF8, 0x4880, oreg, "\textw\td%X",
  0xFFF8, 0x48C0, oreg, "\textl\td%X",
  0xFB80, 0x4880, omovem, 0,
  0xFFF8, 0x49C0, oreg, "\textbl\td%X",
  0xFFFF, 0x4AFC, oprint, "illegal",
  0xFFC0, 0x4AC0, oneop, "tas",
  0xFF00, 0x4A00, soneop, "tst",
  0xFFC0, 0x4C40, odivmull, "div",
  0xFFC0, 0x4C00, odivmull, "mul",
  0xFFF0, 0x4E40, otrap, 0,
  0xFFF8, 0x4E50, olink, "w",
  0xFFF8, 0x4E58, oreg, "\tunlk\ta%X",
  0xFFF8, 0x4E60, oreg, "\tmovl\ta%X,usp",
  0xFFF8, 0x4E68, oreg, "\tmovl\tusp,a%X",
  0xFFFF, 0x4E70, oprint, "reset",
  0xFFFF, 0x4E71, oprint, "nop",
  0xFFFF, 0x4E72, oprint, "stop",
  0xFFFF, 0x4E73, oprint, "rte",
  0xFFFF, 0x4E74, ortd, 0,
  0xFFFF, 0x4E75, oprint, "rts",
  0xFFFF, 0x4E76, oprint, "trapv",
  0xFFFF, 0x4E77, oprint, "rtr",
  0xFFFE, 0x4E7A, omovec, 0,
  0xFFC0, 0x4E80, oneop, "jsr",
  0xFFC0, 0x4EC0, oneop, "jmp",
  0xF1C0, 0x4100, ochk, "chkl",		/* keeping company with lea */
  0xF1C0, 0x4180, ochk, "chkw",		/* keeping company with lea */
  0xF1C0, 0x41C0, ochk, "lea",		/* ordered after extbl */
  0xF0F8, 0x50C8, odbcc, 0,
  0xF0F8, 0x50F8, otrapcc, 0,
  0xF0C0, 0x50C0, oscc, 0,
  0xF100, 0x5000, oquick, "addq",	/* ordered after cc instructions */
  0xF100, 0x5100, oquick, "subq",
  0xF000, 0x6000, obranch, 0,		/* branches */
  0xF000, 0x7000, omoveq, 0,
  0xF0C0, 0x80C0, odivmul, "div",
  0xF1F0, 0x8100, extend, "sbcd",
  0xF1F0, 0x8140, extend, "pack",
  0xF1F0, 0x8180, extend, "unpk",
  0xF000, 0x8000, opmode, "or",		/* ordered last in this group */
  0xF1C0, 0x91C0, opmode, "sub",
  0xF130, 0x9100, extend, "subx",
  0xF000, 0x9000, opmode, "sub",	/* ordered last in this group */
  0xF100, 0xB000, opmode, "cmp",
  0xF1C0, 0xB1C0, opmode, "cmp",
  0xF138, 0xB108, extend, "cmpm",
  0xF100, 0xB100, opmode, "eor",	/* ordered after special cmps */
  0xF0C0, 0xC0C0, odivmul, "mul",
  0xF1F0, 0xC100, extend, "abcd",
  0xF1F0, 0xC140, extend, "exg",
  0xF1F8, 0xC188, extend, "exg",
  0xF000, 0xC000, opmode, "and",	/* ordered last in this group */
  0xF1C0, 0xD1C0, opmode, "add",	/* adda; precedes addx */
  0xF130, 0xD100, extend, "addx",	/* precedes add */
  0xF000, 0xD000, opmode, "add",
  0xF8C0, 0xE8C0, obitfield, 0,		/* bitfields; precedes shifts */
  0xF100, 0xE000, shroi, "r",
  0xF100, 0xE100, shroi, "l",
  0xFFC0, 0xF200, ofloat, 0,		/* floating point */
  0xFFF8, 0xF248, ofdbcc, 0,		/* ordered before fscc */
  0xFFF8, 0xF278, oftrapcc, 0,		/* ordered before fscc */
  0xFFC0, 0xF240, ofscc, 0,
  0xFFC0, 0xF280, ofbranch, "w",
  0xFFC0, 0xF2C0, ofbranch, "l",
  0xFFC0, 0xF300, ofsaverestore, "save",
  0xFFC0, 0xF340, ofsaverestore, "restore",
  0xFFC0, 0xF000, ocpgen, 0,		/* generic coprocessor support */
  0xF1F8, 0xF048, ocpdbcc, 0,
  0xF1F8, 0xF078, ocptrapcc, 0,
  0xF1C0, 0xF040, ocpscc, 0,
  0xF1C0, 0xF080, ocpbranch, "w",
  0xF1C0, 0xF0C0, ocpbranch, "l",
  0xF1C0, 0xF100, ocpsaverestore, "save",
  0xF1C0, 0xF140, ocpsaverestore, "restore",
  0, 0, 0, 0
};

extern struct opdesc *ioptab[0x10];

mkioptab() {
	register struct opdesc *p;
	register int i;

	for (p = &opdecode[0]; p->mask; ++p) {
		i = (p->match >> 12) & 0xff;
		if (ioptab[i] == 0)
			ioptab[i] = p;
	}
	for (i = 0; i < 0x10; ++i)
		if (ioptab[i] == 0)
			ioptab[i] = &opdecode[0];
}

printins(f, idsp, inst)
	register int inst;
{
	register struct opdesc *p;

	space = idsp; dotinc = 2;
	for (p = ioptab[(inst >> 12) & 0xff]; p->mask; p++)
		if ((inst & p->mask) == p->match) break;
	if (p->mask != 0) (*p->opfun)(inst, p->farg);
	else printf(badop);
}

long
instfetch(size)
int size;
{
	long l1, l2;

	if (size==4)
	{
		l1 = leng(chkget(inkdot(dotinc), space));
		l1 <<= 16;
		l2 = leng(chkget(inkdot(dotinc += 2), space));
		l1 = (l1 | l2);
	}
	else
	{
		l1 = (long)(chkget(inkdot(dotinc), space)) & 0xFFFF;
	}
	dotinc += 2;
	return(l1);
}

printea(mode,reg,type)
long mode, reg;
int type;
{
	long index;
	union { int i[2]; float f; double d; } uifd;

	switch ((int)(mode)) {
	  case 0:	printf("d%D",reg);
			break;

	  case 1:	printf("%s",A(reg));
			break;

	  case 2:	printf("%s@",A(reg));
			break;

	  case 3:	printf("%s@+",A(reg));
			break;

	  case 4:	printf("%s@-",A(reg));
			break;

	  case 5:	printf("%s@(%w)",A(reg),instfetch(2));
			break;

	  case 6:	printindex(mode, reg);
			break;

	  case 7:	switch ((int)(reg))
			{
			  case 0:	index = instfetch(2);
					printf("%x:w",index);
					break;

			  case 1:	index = instfetch(4);
					psymoff(index, ISYM, "");
					break;

			  case 2:	printf("pc@(%w)",instfetch(2));
					break;

			  case 3:	printindex(mode, reg);
					break;

			  case 4:
				switch (type)
				{
				default:
				case TLONG:	printf("#%W", instfetch(4));
						break;
				case TBYTE:
				case TWORD:	printf("#%w", instfetch(2));
						break;
				case TDPDREAL:
				case TPDREAL:	printf("#pd");
						goto funnyconstant;
				case TXPREAL:	printf("#xp");
				funnyconstant:	printf("<%X,%X,%X>",
							instfetch(4),
							instfetch(4),
							instfetch(4));
						break;
				case TSPREAL:	uifd.i[0] = instfetch(4);
						printf("#%F", uifd.f);
						break;
				case TDPREAL:	uifd.i[0] = instfetch(4);
						uifd.i[1] = instfetch(4);
						printf("#%F", uifd.d);
						break;
				}
				break;

			  default:	printf("???");
					break;
			}
			break;

	  default:	printf("???");
	}
}

printEA(ea,type)
long ea;
int type;
{
	printea((ea>>3)&07,ea&07,type);
}

/*
 * 68020 indexed addressing modes
 */
printindex(mode, breg)
	long mode, breg;
{
	register int ext;
	int scale, regtype, size, ireg;
	int disp;
	int bs, is, bdsize, iis;
#define printbd(bds)	if ((bds) > 2) printf("%W", instfetch(4)); \
			else if ((bds) == 2) printf("%w", instfetch(2))
	char *bregname;

	bregname = mode == 7 ? "pc" : A(breg);

	ext = instfetch(2);
	regtype = (ext & 0x8000) ? 'a' : 'd';
	ireg = (ext >> 12) & 7;
	size = (ext & 0x800) ? 'l' : 'w';
	scale = 1 << ((ext >> 9) & 3);
	if ((ext & 0x100) == 0) {
		/* 'brief' format */
		printf("%s@(", bregname);
		if (disp = (char)(ext & 0377))
			printf("%w,", disp);
		printf("%c%x:%c:%x)", regtype, ireg, size, scale);
		return;
	}
	bs = ext & 0x80;
	is = ext & 0x40;
	bdsize = (ext >> 4) & 3;
	iis = (ext & 7) + (is >> 3);

	if (!bs)
		printf("%s@", bregname);
	printf("(");
	printbd(bdsize);

	switch (iis) {
	case 0: /* no memory indirection */
		if (bdsize >= 2) printf(",");
		printf("%c%x:%c:%x)", regtype, ireg, size, scale);
		break;

	case 1:
	case 2:
	case 3:
		if (bdsize >= 2) printf(",");
		printf("%c%x:%c:%x)@", regtype, ireg, size, scale);
		if (iis == 1)
			break;
		goto od;

	case 5:
	case 6:
	case 7:
		if (bdsize < 2) printf("0");
		printf(")@(");
		printbd(iis & 3);
		if ((iis & 3) >= 2) printf(",");
		printf("%c%x:%c:%x)", regtype, ireg, size, scale);
		break;

	/* index suppressed modes */

	case 8: /* no memory indirection */
		printf(")");
		break;

	case 9:
	case 10:
	case 11:
		if (bdsize < 2) printf("0");
		printf(")@");
		if (iis == 9)
			break;
	od:
		printf("(");
		printbd(iis & 3);
		printf(")");
		break;

	default:
		printf("???");
		break;
	}
}

mapsize(inst)
long inst;
{
	inst >>= 6;
	inst &= 03;
	return((inst==0) ? TBYTE : (inst==1) ? TWORD : (inst==2) ? TLONG : -1);
}

char suffix(type)
register int type;
{
	static char suffices[] = "lsxpwdbp";

	if (type < 0 || type >= sizeof suffices - 1)
		return ('?');
	return (suffices[type]);
}

omove(inst, s)
long inst;
char *s;
{
	int type;

	printf("\tmov%c\t",*s);
	type = ((*s == 'b') ? TBYTE : (*s == 'w') ? TWORD : TLONG);
	printea((inst>>3)&07,inst&07,type);
	printc(',');
	printea((inst>>6)&07,(inst>>9)&07,type);
}

omovec(inst,dummy)
long inst;
{
	long ext = instfetch(2);
	int reg = (ext >> 12) & 07;

	printf("\tmovc\t");
	if (inst&1)
		printf("%s,", ext & 0x8000 ? A(reg) : D(reg));
	printf(((ext&0x800)?creg2:creg1)[ext&0x7FF]);
	if (!(inst&1))
		printf(",%s", ext & 0x8000 ? A(reg) : D(reg));
}

omovep(inst,dummy)
long inst;
{
	long ext = instfetch(2);
	int d = (inst >> 9) & 07;
	int a = inst & 07;

	printf("\tmovep%c\t", inst & 0100 ? 'l' : 'w');
	if (inst & 0200)
		printf("d%X,%s@(%w)", d, A(a), ext);
	else
		printf("%s@(%w),d%X", A(a), ext, d);
}

omover(inst,dummy)
long inst;
{
	int to = inst & 02000;
	char *r = ((inst & 01000) << 1) ^ to ? "ccr" : "sr";

	printf("\tmovw\t");
	if (!to)
		printf("%s,", r);
	printEA(inst, TWORD);
	if (to)
		printf(",%s", r);
}

omoves(inst,dummy)
long inst;
{
	int ext = instfetch(2);
	int reg = (ext >> 12) & 07;
	int type = mapsize(inst);

	printf("\tmovs%c\t", suffix(type));
	if (ext&0x800)
		printf("%s,", ext & 0x8000 ? A(reg) : D(reg));
	printEA(inst, type);
	if (!(ext&0x800))
		printf(",%s", ext & 0x8000 ? A(reg) : D(reg));
}

/* also handles cmp2 */
ochk2(inst, dummy)
long inst;
{
	int ext = instfetch(2);
	int reg = (ext >> 12) & 07;
	int type, typename;

	type = (inst >> 9) & 03;
	type = type > 2 ? TLONG : type == 2 ? TWORD : TBYTE;
	typename = type == TLONG ? 'l' : type == TWORD ? 'w' : 'b';
	printf("\t%s2%c\t", ext & 0x800 ? "chk" : "cmp", typename);
	printEA(inst, type);
	printf(",%s", ext & 0x8000 ? A(reg) : D(reg));
}

ocall(inst, dummy)
long inst;
{
	printf("\tcallm\t#%X,", instfetch(2));
	printEA(inst, TLONG);
}

ocas(inst, dummy)
long inst;
{
	int ext, ext2;
	int type, typename;

	if ((inst & 077) == 074) {
		/* cas2 */
		type = (inst >> 9) & 03;
		type = type > 2 ? TLONG : TWORD;
		typename = type == TLONG ? 'l' : 'w';
		ext = instfetch(2);
		ext2 = instfetch(2);
		printf("\tcas2%c\td%X:d%X,d%X:d%X,(%s):(%s)",
		 typename,
		 ext & 07, ext2 & 07,
		 (ext >> 6) & 07, (ext2 >> 6) & 07,
		 ext & 0x8000 ? A((ext >> 12) & 07) : D((ext >> 12) & 07),
		 ext2 & 0x8000 ? A((ext2 >> 12) & 07) : D((ext2 >> 12) & 07));
		return;
	}
	/* cas */
	type = (inst >> 9) & 03;
	type = type > 2 ? TLONG : type == 2 ? TWORD : TBYTE;
	typename = type == TLONG ? 'l' : type == TWORD ? 'w' : 'b';
	ext = instfetch(2);
	printf("\tcas%c\td%X,d%X,", typename, ext & 07, (ext >> 6) & 07);
	printEA(inst, type);
}

obranch(inst,dummy)
long inst;
{
	long disp = inst & 0377;
	char *s; 


	s = "b ";
	if (disp == 0) {
		s = "w ";
		disp = instfetch(2);
		if (disp > 0x7fff)
			disp |= ~0xffff;
	}
	else if (disp == 0xFF) {
		s = "l ";
		disp = instfetch(4);
	}
	else if (disp > 0x7f)
		disp |= ~0xff;
	printf("\tb%s%s\t",bname[(int)((inst>>8)&017)],s);
	psymoff(disp+inkdot(2), ISYM, "");
}

char *
ccname(cond)
long cond;
{
	char *s;

	if (cond == 0) s = "t";
	else if (cond == 1) s = "f";
	else s = bname[cond];

	return s;
}

odbcc(inst,dummy)
long inst;
{
	long cond = (inst>>8)&0xF;
	long disp;

	printf("\tdb%s\td%X,", ccname(cond), inst&07);
	disp = instfetch(2);
	if (disp > 0x7fff)
		disp |= ~0xffff;
	psymoff(disp+inkdot(2), ISYM, "");
}

oscc(inst,dummy)
long inst;
{
	long cond = (inst>>8)&0xF;

	printf("\ts%s\t",ccname(cond));
	printea((inst>>3)&07,inst&07,TBYTE);
}

otrapcc(inst,dummy)
long inst;
{
	long cond = (inst>>8)&0xF;
	int size = inst & 07;

	printf("\ttrap%s\t",ccname(cond));
	if (size == 2)
		printf("#%w", instfetch(2));
	else if (size == 3)
		printf("#%W", instfetch(4));
}

biti(inst, dummy)
long inst;
{
	printf("\t%s\t", bit[(int)((inst>>6)&03)]);
	if (inst&0x0100) printf("d%D,", inst>>9);
	else { printf("#%x", instfetch(2)); printc(','); }
	printEA(inst, TLONG);
}

opmode(inst,opcode)
long inst;
char *opcode;
{
	register int mode = (int)((inst>>6) & 07);
	register int reg = (int)((inst>>9) & 07);
	int type;

	type = (mode==0 || mode==4) ?
		TBYTE : (mode==1 || mode==3 || mode==5) ? TWORD : TLONG;
	printf("\t%s%c\t", opcode, suffix(type));
	if (mode>=4 && mode<=6)
	{
		printf("d%d,",reg);
		printea((inst>>3)&07,inst&07,type);
	}
	else
	{
		printea((inst>>3)&07,inst&07,type);
		printf(",%s",(mode<=2)? D(reg) : A(reg));
	}
}


char *bfnames[] = { "tst", "extu", "chg", "exts", "clr", "ffo", "set", "ins" };

#define BFTST	0
#define BFEXTU	1
#define BFCHG	2
#define BFEXTS	3
#define BFCLR	4
#define BFFFO	5
#define BFSET	6
#define BFINS	7

obitfield(inst, dummy)
long inst;
{
	long bfop = (inst >> 8) & 7;
	long d_o, d_w, o, w;
	register long ext = instfetch(2);

	d_o = ext & 04000;
	o = (ext >> 6) & 037;
	d_w = ext & 040;
	w = ext & 037;

	printf("\tbf%s\t", bfnames[bfop]);

	if (bfop == BFINS)
		printf("d%X,", (ext >> 12) & 7);

	printEA(inst, TBYTE);
	printf("{%c%X:%c%X}", d_o ? 'd' : '#', o, d_w ? 'd' : '#', w);

	/* sleazy, but it works */
	if (bfop != BFINS && (bfop & 1))
		printf(",d%X", (ext >> 12) & 7);
}

shroi(inst,ds)
long inst;
char *ds;
{
	int rx, ry;
	char *opcode;
	if ((inst & 0xC0) == 0xC0)
	{
		opcode = shro[(int)((inst>>9)&03)];
		printf("\t%s%s\t", opcode, ds);
		printEA(inst, TLONG);
	}
	else
	{
		opcode = shro[(int)((inst>>3)&03)];
		printf("\t%s%s%c\t", opcode, ds, suffix(mapsize(inst)));
		rx = (int)((inst>>9)&07); ry = (int)(inst&07);
		if ((inst>>5)&01) printf("d%d,d%d", rx, ry);
		else
		{
			printf("#%X", (rx ? rx : 8));
			printf(",d%d", ry);
		}
	}
}		

oimmed(inst,opcode) 
long inst;
register char *opcode;
{
	register int type = mapsize(inst);
	long con;

	if ((unsigned) type <= TDPDREAL)
	{
		con = instfetch(type==TLONG?4:2);
		printf("\t%s%c\t", opcode, suffix(type));
		printf(type == TWORD ? "#%w" : "#%W", con);
		printc(',');
		printEA(inst,type);
	}
	else printf(badop);
}

ouimmed(inst,opcode) 
long inst;
register char *opcode;
{
	register int type = mapsize(inst);
	long con;

	if ((unsigned) type <= TDPDREAL)
	{
		con = instfetch(type==TLONG?4:2);
		if ((inst & 077) == 074) {
			printf("\t%si\t", opcode);
			printf("#%X,", con);
			printf((inst & 0300) == 0 ? "cc" : "sr");
			return;
		}
		printf("\t%s%c\t", opcode, suffix(type));
		printf("#%X", con); printc(',');
		printEA(inst,type);
	}
	else printf(badop);
}

oreg(inst,opcode)
long inst;
register char *opcode;
{
	printf(opcode, (inst & 07));
}

extend(inst, opcode)
long	inst;
char	*opcode;
{
	register int type = mapsize(inst);
	int ry = (inst&07), rx = ((inst>>9)&07);
	char c;

	c = ((inst & 0x1000) ? suffix(type) : ' ');
	printf("\t%s%c\t", opcode, c);
	if (*opcode == 'e')
	{
		if (inst & 0x0080) printf("d%D,%s", rx, A(ry));
		else if (inst & 0x0008) {
			printf("%s,", A(rx));
			printf("%s", A(ry));
		} else printf("d%D,d%D", rx, ry);
	}
	else if ((inst & 0xF000) == 0xB000) {
		printf("%s@+,", A(ry));
		printf("%s@+", A(rx));
	} else if (inst & 0x8) {
		printf("%s@-,", A(ry));
		printf("%s@-", A(rx));
	} else printf("d%D,d%D", ry, rx);
	if (*opcode == 'p' || *opcode == 'u') /* pack and unpk */
		printf(",#%X", instfetch(2));
}

olink(inst,s)
long inst;
char *s;
{
	int size = (*s == 'w') ? 2 : 4;

	printf("\tlink%c\ta%D,", *s, inst&07);
	printf(size == 2 ? "#%w" : "#%W", instfetch(size));
}

ortd(inst, dummy)
long inst;
{
	printf("\trtd\t#%W", instfetch(2));
}

ortm(inst, dummy)
long inst;
{
	printf("\trtm\t%s", inst & 8 ? A(inst & 7) : D(inst & 7));
}

otrap(inst,dummy)
long inst;
{
	printf("\ttrap\t");
	printf("#%X", inst&017);
}

oneop(inst,opcode)
long inst;
register char *opcode;
{
	printf("\t%s\t",opcode);
	printEA(inst,TLONG);
}

pregmask(mask)
register int mask;
{
	register int i;
	register int flag = 0;

	printf("#<");
	for (i=0; i<16; i++)
	{
		if (mask&1)
		{
			if (flag) printc(','); else flag++;
			printf("%s",(i<8)? D(i) : A(i&7));
		}
		mask >>= 1;
	}
	printf(">");
}

omovem(inst,dummy)
long inst;
{
	register int i, list = 0, mask = 0100000;
	register int reglist = (int)(instfetch(2));

	if ((inst & 070) == 040)	/* predecrement */
	{
		for(i = 15; i > 0; i -= 2)
		{ list |= ((mask & reglist) >> i); mask >>= 1; }
		for(i = 1; i < 16; i += 2)
		{ list |= ((mask & reglist) << i); mask >>= 1; }
		reglist = list;
	}
	printf("\tmovem%c\t",(inst&100)?'l':'w');
	if (inst&02000)
	{
		printEA(inst,TLONG);
		printc(',');
		pregmask(reglist);
	}
	else
	{
		pregmask(reglist);
		printc(',');
		printEA(inst,TLONG);
	}
}

ochk(inst,opcode)
long inst;
register char *opcode;
{
	int reg = (inst>>9)&07;

	printf("\t%s\t",opcode);
	printEA(inst, (inst & 0700) == 0600 ? TWORD : TLONG);
	printf(",%s", (*opcode=='l') ? A(reg) : D(reg));
}

odivmul(inst,opcode)
long inst;
register char *opcode;
{
	printf("\t%s%c",opcode,(inst&0x0100)?'s':'u');
	printc('w');
	printc('\t');
	printEA(inst,TWORD);
	printf(",d%D",(inst>>9)&07);
}

odivmull(inst, opcode)
long inst;
register char *opcode;
{
	long inst2 = instfetch(2);
	int dl, dr;

	dl = inst2 & 07;
	dr = (inst2>>12)&07;
	printf("\t%s%cl", opcode, (inst2 & 0x0800) ? 's' : 'u');
	if ((inst2 & 0x0400) == 0 && *opcode == 'd' && dl != dr)
		printc('l');
	printc('\t');
	printEA(inst,TLONG);
	printc(',');
	if ((inst2 & 0x0400) || (*opcode == 'd' && dl != dr))
		printf("d%D:", dl);
	printf("d%D", dr);
}

soneop(inst,opcode)
long inst;
register char *opcode;
{
	register int type = mapsize(inst);

	if ((unsigned) type <= TDPDREAL)
	{
		printf("\t%s%c\t",opcode,suffix(type));
		printEA(inst,type);
	}
	else printf(badop);
}

oquick(inst,opcode)
long inst;
register char *opcode;
{
	register int type = mapsize(inst);
	register int data = (int)((inst>>9) & 07);

	if (data == 0) data = 8;
	if ((unsigned) type <= TDPDREAL)
	{
		printf("\t%s%c\t", opcode, suffix(type));
		printf("#%X", data); printc(',');
		printEA(inst, type);
	}
	else printf(badop);
}

omoveq(inst,dummy)
long inst;
{
	register int data = (int)(inst & 0377);

	if (data > 127) data |= ~0377;
	printf("\tmoveq\t"); printf("#%W", data);
	printf(",d%D", (inst>>9)&07);
}

oprint(inst,opcode)
long inst;
register char *opcode;
{
	printf("\t%s",opcode);
}

/*
 * Print 68881 floating point instructions.
 */

extern fop(), foneop(), fmovem(), fmovex(), fmemmove();
extern fmover(), fmovec(), fsincos();

struct opdesc fopdecode[] = {
  0xc700, 0xc000, fmovem, 0,
  0xe000, 0x2000, fmovex, 0,
  0xe000, 0x6000, fmemmove, 0,
  0xc3ff, 0x8000, fmover, 0,
  0xfc00, 0x5c00, fmovec, 0,
  0xa07f, 0x0000, fop, "move",
  0xa07f, 0x0001, fop, "int",
  0xa07f, 0x0002, fop, "sinh",
  0xa07f, 0x0003, fop, "intrz",
  0xa07f, 0x0004, fop, "sqrt",
  0xa07f, 0x0006, fop, "lognp1",
  0xa07f, 0x0008, fop, "etoxm1",
  0xa07f, 0x0009, fop, "tanh",
  0xa07f, 0x000a, fop, "atan",
  0xa07f, 0x000c, fop, "asin",
  0xa07f, 0x000d, fop, "atanh",
  0xa07f, 0x000e, fop, "sin",
  0xa07f, 0x000f, fop, "tan",
  0xa07f, 0x0010, fop, "etox",
  0xa07f, 0x0011, fop, "twotox",
  0xa07f, 0x0012, fop, "tentox",
  0xa07f, 0x0014, fop, "logn",
  0xa07f, 0x0015, fop, "log10",
  0xa07f, 0x0016, fop, "log2",
  0xa07f, 0x0018, fop, "abs",
  0xa07f, 0x0019, fop, "cosh",
  0xa07f, 0x001a, fop, "neg",
  0xa07f, 0x001c, fop, "acos",
  0xa07f, 0x001d, fop, "cos",
  0xa07f, 0x001e, fop, "getexp",
  0xa07f, 0x001f, fop, "getman",
  0xa07f, 0x0020, fop, "div",
  0xa07f, 0x0021, fop, "mod",
  0xa07f, 0x0022, fop, "add",
  0xa07f, 0x0023, fop, "mul",
  0xa07f, 0x0024, fop, "sgldiv",
  0xa07f, 0x0025, fop, "rem",
  0xa07f, 0x0026, fop, "scale",
  0xa07f, 0x0027, fop, "sglmul",
  0xa07f, 0x0028, fop, "sub",
  0xa078, 0x0030, fsincos, 0,
  0xa07f, 0x0038, fop, "cmp",
  0xa07f, 0x003a, foneop, "tst",
  0, 0, 0, 0
};

ofloat(inst, dummy)
	long inst;
{
	register struct opdesc *p;
	register long inst2 = instfetch(2);

	for (p = fopdecode; p->mask; p++)
		if ((inst2 & p->mask) == p->match) break;
	if (p->mask != 0) (*p->opfun)(inst, inst2, p->farg);
	else printf(badop);
}

#define	fmemsrc(i2)	((i2) & 0x4000)
#define	fmode(i)	(((i) >> 3) & 7)
#define	freg(i)		((i) & 7)
#define	ftype(i2)	(int)(((i2) >> 10) & 7)
#define	fregsrc(i2)	ftype(i2)
#define	fregdst(i2)	(((i2) >> 7) & 7)

fop(inst, inst2, opcode)
	long inst, inst2;
	char *opcode;
{
	if (fmemsrc(inst2)) {
		printf("\tf%s%c\t", opcode, suffix(ftype(inst2)));
		printea(fmode(inst), freg(inst), ftype(inst2));
	} else
		printf("\tf%sx\tfp%X", opcode, fregsrc(inst2));
	if (fmemsrc(inst2) || fregsrc(inst2) != fregdst(inst2))
		printf(",fp%X", fregdst(inst2));
}

foneop(inst, inst2, opcode)
	long inst, inst2;
	char *opcode;
{
	if (fmemsrc(inst2)) {
		printf("\tf%s%c\t", opcode, suffix(ftype(inst2)));
		printea(fmode(inst), freg(inst), ftype(inst2));
	} else
		printf("\tf%sx\tfp%X", opcode, fregsrc(inst2));
}

/*ARGSUSED*/
fmemmove(inst, inst2, dummy)
	long inst, inst2;
	char *dummy;
{
	int type = ftype(inst2);

	printf("\tfmove%c\tfp%X,", suffix(type), fregdst(inst2));
	printea(fmode(inst), freg(inst), type);
	if (type == 3)
		printf("{#%d}", (inst2&0x7f));
	else if (type == 7)
		printf("{d%D}", ((inst2>>4)&0x7));
}

fpregmask(mask)
	register int mask;
{
	register int i;
	register int flag = 0;

	printf("#<");
	for (i=0; i<8; i++) {
		if (mask&1) {
			if (flag)
				printc(',');
			else
				flag++;
			printf("fp%X", i);
		}
		mask >>= 1;
	}
	printf(">");
}

#define ffromreg(i2)	((i2) & 0x2000)
#define	fmmode(i2)	(((i2) >> 11) & 3)
#define	freglist(i2)	((i2) & 0xff)
#define	fdynreg(i2)	(((i2) >> 4) & 7)

/*ARGSUSED*/
fmovem(inst, inst2, dummy)
	long inst, inst2;
	char *dummy;
{
	register int i, reglist, oreglist;
	int dynreg;

	switch ((int)fmmode(inst2)) {

	case 0:	/* static + predecrement */
		dynreg = -1;
		reglist = freglist(inst2);
		break;

	case 2:	/* static + postincrement or control mode */
		/* reverse the register mask */
		/* is this really backwards from movem?!? */
		dynreg = -1;
		oreglist = freglist(inst2);
		reglist = 0;
		for (i = 0; i < 8; ++i)
			if (oreglist & 1 << (7 - i))
				reglist |= 1 << i;
		break;

	case 1:	/* dynamic + predecrement */
	case 3:	/* dynamic + postincrement or control */
		dynreg = fdynreg(inst2);
		break;
	}

	printf("\tfmovem\t");
	if (ffromreg(inst2)) {
		if (dynreg >= 0)
			printf("d%X<>", dynreg);
		else
			fpregmask(reglist);
		printf(",");
		printea(fmode(inst), freg(inst), TXPREAL);
	} else {
		printea(fmode(inst), freg(inst), TXPREAL);
		printf(",");
		if (dynreg >= 0)
			printf("d%X<>", dynreg);
		else
			fpregmask(reglist);
	}
}

fpintregname(regnum)
	int regnum;
{
	switch (regnum) {
	case 1:	printf("FPIAR"); break;
	case 2:	printf("FPSR"); break;
	case 4:	printf("FPCR"); break;
	default: printf("???"); break;
	}
}

#define	fintregname(i2)	(int)(((i2) >> 10) & 7)

/*ARGSUSED*/
fmovex(inst, inst2, dummy)
	long inst, inst2;
	char *dummy;
{
	printf("\tfmover\t");
	if (ffromreg(inst2)) {
		fpintregname(fintregname(inst2));
		printf(",");
		printea(fmode(inst), freg(inst), TLONG);
	} else {
		printea(fmode(inst), freg(inst), TLONG);
		printf(",");
		fpintregname(fintregname(inst2));
	}
}

fpintregmask(reglist)
	register int reglist;
{
	printf("#<");
	if (reglist & 1) {
		printf("FPIAR");
		if (reglist & 6)
			printf(",");
	}
	if (reglist & 2)
		printf("FPSR");
	if (reglist & 4) {
		if (reglist & 3)
			printf(",");
		printf("FPCR");
	}
	printf(">");
}

#define	fintreglist(i2)	(int)(((i2) >> 10) & 7)

/*ARGSUSED*/
fmover(inst, inst2, dummy)
	long inst, inst2;
	char *dummy;
{
	printf("\tfmover\t");
	if (ffromreg(inst2)) {
		fpintregmask(fintreglist(inst2));
		printf(",");
		printea(fmode(inst), freg(inst), TLONG);
	} else {
		printea(fmode(inst), freg(inst), TLONG);
		printf(",");
		fpintregmask(fintreglist(inst2));
	}
}

static struct romtab {
	int romoff;
	char *romname;
} romtab[] = {
	0x00,	"pi",
	0x0b,	"log10_2",
	0x0c,	"e",
	0x0d,	"log2_e",
	0x0e,	"log10_e",
	0x0f,	"0",
	0x30,	"ln_2",
	0x31,	"ln_10",
	0x32,	"1",
	0x33,	"10",
	0x34,	"10e2",
	0x35,	"10e4",
	0x36,	"10e8",
	0x37,	"10e16",
	0x38,	"10e32",
	0x39,	"10e64",
	0x3a,	"10e128",
	0x3b,	"10e256",
	0x3c,	"10e512",
	0x3d,	"10e1024",
	0x3e,	"10e2048",
	0x3f,	"10e4096",
	-1,	"???"
};

#define	foffrom(i2)	((i2) & 0x7f)

/*ARGSUSED*/
fmovec(inst, inst2, dummy)
	long inst, inst2;
	char *dummy;
{
	register struct romtab *p;
	register int off;

	off = foffrom(inst2);
	for (p = romtab; p->romoff >= 0 && p->romoff != off; ++p)
		;
	printf("\tfmovec\t#%s,fp%X", p->romname, fregdst(inst2));
}

#define	fcosdst(i2)	((i2) & 7)

/*ARGSUSED*/
fsincos(inst, inst2, dummy)
	long inst, inst2;
	char *dummy;
{
	if (fmemsrc(inst2)) {
		printf("\tfsincos%c\t", suffix(ftype(inst2)));
		printea(fmode(inst), freg(inst), ftype(inst2));
	} else
		printf("\tfsincosx\tfp%X", fregsrc(inst2));
	/* so why does motorola insist on the order cos:sin? */
	printf(",fp%X:fp%X", fcosdst(inst2), fregdst(inst2));
}

static char *fbnames[32] = {
	"f", "eq", "ogt", "oge", "olt", "ole", "ogl", "or",
	"un", "ueq", "ugt", "uge", "ult", "ule", "ne", "t",
	"sf", "seq", "gt", "ge", "lt", "le", "gl", "gle",
	"ngle", "ngl", "nle", "nlt", "nge", "ngt", "sne", "st"
};

#define	fbname(c)	((unsigned)(c) < 32 ? fbnames[(c)] : "???")
#define	fcountreg(i)	((i) & 7)

/*ARGSUSED*/
ofdbcc(inst, dummy)
	long inst;
	char *dummy;
{
	int cond = instfetch(2);
	long disp = instfetch(2);

	printf("\tfdb%s\td%X,", fbname(cond), fcountreg(inst));
	if (disp > 0x7fff)
		disp |= ~0xffff;
	psymoff(disp+inkdot(4), ISYM, "");
}

#define	ftrapmode(i)	((i) & 7)

/*ARGSUSED*/
oftrapcc(inst, dummy)
	long inst;
	char *dummy;
{
	int cond;

	if (ftrapmode(inst) < 2)		/* XXX */
		ofscc(inst, dummy);
	else {
		cond = instfetch(2);
		printf("\tftrap%s", fbname(cond));
		if (ftrapmode(inst) == 2)
			printf("w\t#%w", instfetch(2));
		else if (ftrapmode(inst) == 3)
			printf("l\t#%W", instfetch(4));
	}
}

/*ARGSUSED*/
ofscc(inst, dummy)
	long inst;
	char *dummy;
{
	int cond = instfetch(2);

	printf("\tfs%s\t", fbname(cond));
	printea(fmode(inst), freg(inst), TBYTE);
}

#define	fbcond(i)	((i) & 0x3f)

ofbranch(inst, size)
	long inst;
	char *size;
{
	int cond = fbcond(inst);
	long disp;

	if (*size == 'w') {
		disp = instfetch(2);
		if (disp > 0x7fff)
			disp |= ~0xffff;
	} else
		disp = instfetch(4);
	if (inst == 0xf280 && disp == 0)	/* XXX */
		printf("\tfnop");
	else {
		printf("\tfb%s%s\t", fbname(cond), size);
		psymoff(disp+inkdot(2), ISYM, "");
	}
}

ofsaverestore(inst, opcode)
	long inst;
	char *opcode;
{
	printf("\tf%s\t", opcode);
	printea(fmode(inst), freg(inst), TLONG);
}



/*
 * Generic coprocessor instruction support.
 */

ocpgen(inst, dummy)
	long inst;
{
	register long inst2 = instfetch(2);

	printf("\tcp%Xgen_%x\t", (inst >> 9) & 07, inst2);
	printea(fmode(inst), freg(inst), TLONG);
}

/*ARGSUSED*/
ocpdbcc(inst, dummy)
	long inst;
	char *dummy;
{
	int cond = instfetch(2);
	long disp = instfetch(2);

	printf("\tcp%Xdb%s\td%X,", (inst >> 9) & 07,
		fbname(cond), fcountreg(inst));
	if (disp > 0x7fff)
		disp |= ~0xffff;
	psymoff(disp+inkdot(4), ISYM, "");
}

/*ARGSUSED*/
ocptrapcc(inst, dummy)
	long inst;
	char *dummy;
{
	int cond;

	if (ftrapmode(inst) < 2)		/* XXX */
		ocpscc(inst, dummy);
	else {
		cond = instfetch(2);
		printf("\tcp%Xtrap%s", (inst >> 9) & 07, fbname(cond));
		if (ftrapmode(inst) == 2)
			printf("w\t#%w", instfetch(2));
		else if (ftrapmode(inst) == 3)
			printf("l\t#%W", instfetch(4));
	}
}

/*ARGSUSED*/
ocpscc(inst, dummy)
	long inst;
	char *dummy;
{
	int cond = instfetch(2);

	printf("\tcp%Xs%s\t", (inst >> 9) & 07, fbname(cond));
	printea(fmode(inst), freg(inst), TBYTE);
}

ocpbranch(inst, size)
	long inst;
	char *size;
{
	int cond = fbcond(inst);
	int cpid = (inst >> 9) & 07;
	long disp;

	if (*size == 'w') {
		disp = instfetch(2);
		if (disp > 0x7fff)
			disp |= ~0xffff;
	} else
		disp = instfetch(4);
	if ((inst & 0x7f) == 0 && disp == 0)	/* XXX */
		printf("\tcp%Xnop", cpid);
	else {
		printf("\tcp%Xb%s%s\t", cpid, fbname(cond), size);
		psymoff(disp+inkdot(2), ISYM, "");
	}
}

ocpsaverestore(inst, opcode)
	long inst;
	char *opcode;
{
	printf("\tcp%X%s\t", (inst >> 9) & 07, opcode);
	printea(fmode(inst), freg(inst), TLONG);
}
