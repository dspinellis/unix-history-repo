/* ns32k-opcode.h */

#ifndef ns32k_opcodeT
#define ns32k_opcodeT int
#endif /* no ns32k_opcodeT */

struct not_wot			/* ns32k opcode table: wot to do with this */
				/* particular opcode */
{
  int obits;		/* number of opcode bits */
  int ibits;		/* number of instruction bits */
  ns32k_opcodeT	code;	/* op-code (may be > 8 bits!) */
  char *args;		/* how to compile said opcode */
};

struct not			/* ns32k opcode text */
{
  char *            name;	/* opcode name: lowercase string  [key]  */
  struct not_wot    detail;	/* rest of opcode table          [datum] */
};

/* Instructions look like this:
    
   basic instruction--1, 2, or 3 bytes
   index byte for operand A, if operand A is indexed--1 byte
   index byte for operand B, if operand B is indexed--1 byte
   addressing extension for operand A
   addressing extension for operand B
   implied operands

   Operand A is the operand listed first in the following opcode table.
   Operand B is the operand listed second in the following opcode table.
   All instructions have at most 2 general operands, so this is enough.
   The implied operands are associated with operands other than A and B.

   Each operand has a digit and a letter.
   
   The digit gives the position in the assembly language.  The letter,
   one of the following, tells us what kind of operand it is.  */

/* F : 32 bit float
 * L : 64 bit float
 * B : byte
 * W : word
 * D : double-word
 * Q : quad-word
 * d : displacement
 * q : quick
 * i : immediate (8 bits)
 * r : register number (3 bits)
 * p : displacement - pc relative addressing
*/
static struct not
notstrs[] =
{
  { "absf",	14,24,	0x35be,	"1F2F" },
  { "absl",	14,24,	0x34be,	"1L2L" },
  { "absb",	14,24,	0x304e, "1B2B" },
  { "absw",	14,24,	0x314e, "1W2W" },
  { "absd",	14,24,	0x334e, "1D2D" },
  { "acbb",	 7,16,	0x4c,	"2B1q3p" },
  { "acbw",      7,16,  0x4d,   "2W1q3p" },
  { "acbd",      7,16,  0x4f,   "2D1q3p" },
  { "addf",	14,24,	0x01be,	"1F2F" },
  { "addl",	14,24,	0x00be, "1L2L" },
  { "addb",	 6,16,	0x00,	"1B2B" },
  { "addw",	 6,16,	0x01,	"1W2W" },
  { "addd",	 6,16,	0x03,	"1D2D" },
  { "addcb",	 6,16,	0x10,	"1B2B" },
  { "addcw",	 6,16,	0x11,	"1W2W" },
  { "addcd",	 6,16,	0x13,	"1D2D" },
  { "addpb",	14,24,	0x3c4e,	"1B2B" },
  { "addpw",	14,24,	0x3d4e,	"1W2W" },
  { "addpd",	14,24,	0x3f4e,	"1D2D" },
  { "addqb",	 7,16,	0x0c,	"2B1q" },
  { "addqw",	 7,16,	0x0d,	"2W1q" },
  { "addqd",	 7,16,	0x0f,	"2D1q" },
  { "addr",	 6,16,	0x27,	"1D2D" },
  { "adjspb",	11,16,	0x057c,	"1B" },
  { "adjspw",	11,16,	0x057d,	"1W" },
  { "adjspd",	11,16,	0x057f,	"1D" },
  { "andb",	 6,16,	0x28,	"1B2B" },
  { "andw",	 6,16,	0x29,	"1W2W" },
  { "andd",	 6,16,	0x2b,	"1D2D" },
  { "ashb",	14,24,	0x044e,	"1B2B" },
  { "ashw",	14,24,	0x054e,	"1B2W" },
  { "ashd",	14,24,	0x074e,	"1B2D" },
  { "beq",	 8,8,	0x0a,	"1p" },
  { "bne",	 8,8,	0x1a,	"1p" },
  { "bcs",	 8,8,	0x2a,	"1p" },
  { "bcc",	 8,8,	0x3a,	"1p" },
  { "bhi",	 8,8,	0x4a,	"1p" },
  { "bls",	 8,8,	0x5a,	"1p" },
  { "bgt",	 8,8,	0x6a,	"1p" },
  { "ble",	 8,8,	0x7a,	"1p" },
  { "bfs",	 8,8,	0x8a,	"1p" },
  { "bfc",	 8,8,	0x9a,	"1p" },
  { "blo",	 8,8,	0xaa,	"1p" },
  { "bhs",	 8,8,	0xba,	"1p" },
  { "blt",	 8,8,	0xca,	"1p" },
  { "bge",	 8,8,	0xda,	"1p" },
  { "bicb",	 6,16,	0x08,	"1B2B" },
  { "bicw",	 6,16,	0x09,	"1W2W" },
  { "bicd",	 6,16,	0x0b,	"1D2D" },
  { "bicpsrb",	11,16,	0x17c,	"1B" },
  { "bicpsrw",	11,16,	0x17d,	"1W" },
  { "bispsrb",	11,16,	0x37c,	"1B" },
  { "bispsrw",	11,16,	0x37d,	"1W" },
  { "bpt",	 8,8,	0xf2,	"" },
  { "br",	 8,8,	0xea,	"1p" },
  { "bsr",	 8,8,	0x02,	"1p" },
  { "caseb",	11,16,	0x77c,	"1B" },
  { "casew",	11,16,	0x77d,	"1W" },
  { "cased",	11,16,	0x77f,	"1D" },
  { "cbitb",	14,24,	0x084e,	"1B2D" },
  { "cbitw",	14,24,	0x094e,	"1W2D" },
  { "cbitd",	14,24,	0x0b4e,	"1D2D" },
  { "cbitib",	14,24,	0x0c4e,	"1B2D" },
  { "cbitiw",	14,24,	0x0d4e,	"1W2D" },
  { "cbitid",	14,24,	0x0f4e,	"1D2D" },
  { "checkb",	11,24,	0x0ee,	"2A3B1r" },
  { "checkw",	11,24,	0x1ee,	"2A3B1r" },
  { "checkd",	11,24,	0x3ee,	"2A3D1r" },
  { "cmpf",	14,24,	0x09be,	"1F2F" },
  { "cmpl",	14,24,	0x08be,	"1L2L" },
  { "cmpb",	 6,16,	0x04,	"1B2B" },
  { "cmpw",	 6,16,	0x05,	"1W2W" },
  { "cmpd",	 6,16,	0x07,	"1D2D" },
  { "cmpmb",	14,24,	0x04ce,	"1D2D3d" },
  { "cmpmw",	14,24,	0x05ce,	"1D2D3d" },
  { "cmpmd",	14,24,	0x07ce,	"1D2D3d" },
  { "cmpqb",	 7,16,	0x1c,	"2B1q" },
  { "cmpqw",	 7,16,	0x1d,	"2W1q" },
  { "cmpqd",	 7,16,	0x1f,	"2D1q" },
  { "cmpsb",	16,16,	0x040e,	"1i" },
  { "cmpsw",	16,16,	0x050e,	"1i" },
  { "cmpsd",	16,16,	0x070e,	"1i" },
  { "cmpst",	16,16,	0x840e,	"1i" },
  { "comb",	14,24,	0x344e,	"1B2B" },
  { "comw",	14,24,	0x354e,	"1W2W" },
  { "comd",	14,24,	0x374e,	"1D2D" },
  { "cvtp",	11,24,	0x036e,	"2D3D1r" },
  { "cxp",	 8,8,	0x22,	"1p" },
  { "cxpd",	11,16,	0x07f,	"1D" },
  { "deib",	14,24,	0x2cce,	"1B2W" },
  { "deiw",	14,24,	0x2cce,	"1W2D" },
  { "deid",	14,24,	0x2cce,	"1D2Q" },
  { "dia",	 8,8,	0xc2,	"" },
  { "divf",	14,24,	0x21be,	"1F2F" },
  { "divl",	14,24,	0x20be,	"1L2L" },
  { "divb",	14,24,	0x3cce,	"1B2B" },
  { "divw",	14,24,	0x3dce,	"1W2W" },
  { "divd",	14,24,	0x3fce,	"1D2D" },
  { "enter",	 8,8,	0x82,	"1i2d" },
  { "exit",	 8,8,	0x92,	"1i" },
  { "extb",	11,24,	0x02e,	"2D3B1r4d" },
  { "extw",	11,24,	0x12e,	"2D3W1r4d" },
  { "extd",	11,24,	0x32e,	"2D3D1r4d" },
  { "extsb",	14,24,	0x0cce,	"1D2B3i" },
  { "extsw",	14,24,	0x0dce,	"1D2W3i" },
  { "extsd",	14,24,	0x0fce,	"1D2D3i" },
  { "ffsb",	14,24,	0x046e,	"1B2B" },
  { "ffsw",	14,24,	0x056e,	"1W2B" },
  { "ffsd",	14,24,	0x076e,	"1D2B" },
  { "flag",	 8,8,	0xd2,	"" },
  { "floorfb",	14,24,	0x3c3e,	"1F2B" },
  { "floorfw",	14,24,	0x3d3e,	"1F2W" },
  { "floorfd",	14,24,	0x3f3e,	"1F2D" },
  { "floorlb",	14,24,	0x383e,	"1L2B" },
  { "floorlw",	14,24,	0x393e,	"1L2W" },
  { "floorld",	14,24,	0x3b3e,	"1L2D" },
  { "ibitb",	14,24,	0x384e,	"1B2D" },
  { "ibitw",	14,24,	0x394e,	"1W2D" },
  { "ibitd",	14,24,	0x3b4e,	"1D2D" },
  { "indexb",	11,24,	0x42e,	"2B3B1r" },
  { "indexw",	11,24,	0x52e,	"2W3W1r" },
  { "indexd",	11,24,	0x72e,	"2D3D1r" },
  { "insb",	11,24,	0x0ae,	"2B3B1r4d" },
  { "insw",	11,24,	0x1ae,	"2W3W1r4d" },
  { "insd",	11,24,	0x3ae,	"2D3D1r4d" },
  { "inssb",	14,24,	0x08ce,	"1B2D3i" },
  { "inssw",	14,24,	0x09ce,	"1W2D3i" },
  { "inssd",	14,24,	0x0bce,	"1D2D3i" },
  { "jsr",	11,16,	0x67f,	"1A" },
  { "jump",	11,16,	0x27f,	"1A" },
  { "lfsr",	19,24,	0x00f3e,"1D" },
  { "lmr",	15,24,	0x0b1e,	"2D1q" },
  { "lprb",	 7,16,	0x6c,	"2B1q" },
  { "lprw",	 7,16,	0x6d,	"2W1q" },
  { "lprd",	 7,16,	0x6f,	"2D1q" },
  { "lshb",	14,24,	0x144e,	"1B2B" },
  { "lshw",	14,24,	0x154e,	"1B2W" },
  { "lshd",	14,24,	0x174e,	"1B2D" },
  { "meib",	14,24,	0x24ce,	"1B2W" },
  { "meiw",	14,24,	0x25ce,	"1W2D" },
  { "meid",	14,24,	0x27ce,	"1D2Q" },
  { "modb",	14,24,	0x38ce,	"1B2B" },
  { "modw",	14,24,	0x39ce,	"1W2W" },
  { "modd",	14,24,	0x3bce,	"1D2D" },
  { "movf",	14,24,	0x05be,	"1F2F" },
  { "movl",	14,24,	0x04be,	"1L2L" },
  { "movb",	 6,16,	0x14,	"1B2B" },
  { "movw",	 6,16,	0x15,	"1W2W" },
  { "movd",	 6,16,	0x17,	"1D2D" },
  { "movbf",	14,24,	0x043e,	"1B2F" },
  { "movwf",	14,24,	0x053e,	"1W2F" },
  { "movdf",	14,24,	0x073e,	"1D2F" },
  { "movbl",	14,24,	0x003e,	"1B2L" },
  { "movwl",	14,24,	0x013e,	"1W2L" },
  { "movdl",	14,24,	0x033e,	"1D2L" },
  { "movfl",	14,24,	0x1b3e,	"1F2L" },
  { "movlf",	14,24,	0x163e,	"1L2F" },
  { "movmb",	14,24,	0x00ce,	"1D2D3d" },
  { "movmw",	14,24,	0x00de,	"1D2D3d" },
  { "movmd",	14,24,	0x00fe,	"1D2D3d" },
  { "movqb",	 7,16,	0x5c,	"2B1q" },
  { "movqw",	 7,16,	0x5d,	"2B1q" },
  { "movqd",	 7,16,	0x5f,	"2B1q" },
  { "movsb",	16,16,	0x000e,	"1i" },
  { "movsw",	16,16,	0x010e,	"1i" },
  { "movsd",	16,16,	0x030e,	"1i" },
  { "movst",	16,16,	0x800e,	"1i" },
  { "movsub",	14,24,	0x0cae,	"1A1A" },
  { "movsuw",	14,24,	0x0dae,	"1A1A" },
  { "movsud",	14,24,	0x0fae,	"1A1A" },
  { "movusb",	14,24,	0x1cae,	"1A1A" },
  { "movusw",	14,24,	0x1dae,	"1A1A" },
  { "movusd",	14,24,	0x1fae,	"1A1A" },
  { "movxbd",	14,24,	0x1cce,	"1B2D" },
  { "movxwd",	14,24,	0x1dce,	"1W2D" },
  { "movxbw",	14,24,	0x10ce,	"1B2W" },
  { "movzbd",	14,24,	0x18ce,	"1B2D" },
  { "movzwd",	14,24,	0x19ce,	"1W2D" },
  { "movzbw",	14,24,	0x14ce,	"1B2W" },
  { "mulf",	14,24,	0x31be,	"1F2F" },
  { "mull",	14,24,	0x30be,	"1L2L" },
  { "mulb",	14,24,	0x20ce, "1B2B" },
  { "mulw",	14,24,	0x21ce, "1W2W" },
  { "muld",	14,24,	0x23ce, "1D2D" },
  { "negf",	14,24,	0x15be, "1F2F" },
  { "negl",	14,24,	0x14be, "1L2L" },
  { "negb",	14,24,	0x204e, "1B2B" },
  { "negw",	14,24,	0x214e, "1W2W" },
  { "negd",	14,24,	0x234e, "1D2D" },
  { "nop",	 8,8,	0xa2,	"" },
  { "notb",	14,24,	0x244e, "1B2B" },
  { "notw",	14,24,	0x254e, "1W2W" },
  { "notd",	14,24,	0x274e, "1D2D" },
  { "orb",	 6,16,	0x18,	"1B1B" },
  { "orw",	 6,16,	0x19,	"1W1W" },
  { "ord",	 6,16,	0x1b,	"1D2D" },
  { "quob",	14,24,	0x30ce,	"1B2B" },
  { "quow",	14,24,	0x31ce,	"1W2W" },
  { "quod",	14,24,	0x33ce,	"1D2D" },
  { "rdval",	19,24,	0x0031e,"1A" },
  { "remb",	14,24,	0x34ce,	"1B2B" },
  { "remw",	14,24,	0x35ce,	"1W2W" },
  { "remd",	14,24,	0x37ce,	"1D2D" },
  { "restore",	 8,8,	0x72,	"1i" },
  { "ret",	 8,8,	0x12,	"1d" },
  { "reti",	 8,8,	0x52,	"" },
  { "rett",	 8,8,	0x42,	"" },
  { "rotb",	14,24,	0x004e,	"1B2B" },
  { "rotw",	14,24,	0x014e,	"1B2W" },
  { "rotd",	14,24,	0x034e,	"1B2D" },
  { "roundfb",	14,24,	0x243e,	"1F2B" },
  { "roundfw",	14,24,	0x253e,	"1F2W" },
  { "roundfd",	14,24,	0x273e,	"1F2D" },
  { "roundlb",	14,24,	0x203e,	"1L2B" },
  { "roundlw",	14,24,	0x213e,	"1L2W" },
  { "roundld",	14,24,	0x233e,	"1L2D" },
  { "rxp",	 8,8,	0x32,	"1d" },
  { "sCONDb",	 7,16,	0x3c,	"2B1q" },
  { "sCONDw",	 7,16,	0x3d,	"2D1q" },
  { "sCONDd",	 7,16,	0x3f,	"2D1q" },
  { "save",	 8,8,	0x62,	"1i" },
  { "sbitb",	14,24,	0x184e,	"1B2A" },
  { "sbitw",	14,24,	0x194e,	"1W2A" },
  { "sbitd",	14,24,	0x1b4e,	"1D2A" },
  { "sbitib",	14,24,	0x1c4e,	"1B2A" },
  { "sbitiw",	14,24,	0x1d4e,	"1W2A" },
  { "sbitid",	14,24,	0x1f4e,	"1D2A" },
  { "setcfg",	15,24,	0x0b0e,	"5D1q" },
  { "sfsr",	14,24,	0x673e,	"5D1D" },
  { "skpsb",	16,16,	0x0c0e,	"1i" },
  { "skpsw",	16,16,	0x0d0e,	"1i" },
  { "skpsd",	16,16,	0x0f0e, "1i" },
  { "skpst",	16,16,	0x8c0e,	"1i" },
  { "smr",	15,24,	0x0f1e,	"2D1q" },
  { "sprb",	 7,16,	0x2c,	"2B1q" },
  { "sprw",	 7,16,	0x2d,	"2W1q" },
  { "sprd",	 7,16,	0x2f,	"2D1q" },
  { "subf",	14,24,	0x11be,	"1F2F" },
  { "subl",	14,24,	0x10be,	"1L2L" },
  { "subb",	 6,16,	0x20,	"1B2B" },
  { "subw",	 6,16,	0x21,	"1W2W" },
  { "subd",	 6,16,	0x23,	"1D2D" },
  { "subcb",	 6,16,	0x30,	"1B2B" },
  { "subcw",	 6,16,	0x31,	"1W2W" },
  { "subcd",	 6,16,	0x33,	"1D2D" },
  { "subpb",	14,24,	0x2c4e,	"1B2B" },
  { "subpw",	14,24,	0x2d4e,	"1W2W" },
  { "subpd",	14,24,	0x2f4e,	"1D2D" },
#ifndef NS32K_SVC_IMMED_OPERANDS
  { "svc",	 8,8,	0xe2,	"2i1i" }, /* not really, but unix uses it */
#else
  { "svc",	 8,8,	0xe2,	"" }, /* not really, but unix uses it */
#endif
  { "tbitb",	 6,16,	0x34,	"1B2A" },
  { "tbitw",	 6,16,	0x35,	"1W2A" },
  { "tbitd",	 6,16,	0x37,	"1D2A" },
  { "truncfb",	14,24,	0x2c3e,	"1F2B" },
  { "truncfw",	14,24,	0x2d3e,	"1F2W" },
  { "truncfd",	14,24,	0x2f3e,	"1F2D" },
  { "trunclb",	14,24,	0x283e,	"1L2B" },
  { "trunclw",	14,24,	0x293e,	"1L2W" },
  { "truncld",	14,24,	0x2b3e,	"1L2D" },
  { "wait",	 8,8,	0xb2,	"" },
  { "wrval",	19,24,	0x0071e,"1A" },
  { "xorb",	 6,16,	0x38,	"1B2B" },
  { "xorw",	 6,16,	0x39,	"1W2W" },
  { "xord",	 6,16,	0x3b,	"1D2D" },
};				/* notstrs */

/* end: ns32k.opcode.h */

#define MAX_ARGS 4
#define ARG_LEN 50
