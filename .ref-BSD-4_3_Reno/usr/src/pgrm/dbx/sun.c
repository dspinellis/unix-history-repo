/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)sun.c	5.2 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * Target machine dependent stuff.
 */

#include "defs.h"
#include "machine.h"
#include "process.h"
#include "runtime.h"
#include "events.h"
#include "main.h"
#include "symbols.h"
#include "source.h"
#include "mappings.h"
#include "object.h"
#include "tree.h"
#include "eval.h"
#include "keywords.h"
#include "ops.h"

#ifndef public
typedef unsigned int Address;
typedef unsigned char Byte;
typedef unsigned int Word;

/*
 * On the 68000, the pc isn't in a register, but we make believe
 * so there's one more register.
 *
 * Note that there's also no argument pointer, this means code
 * involving "ARGP" should always be #ifdef'd.
 *
 * The address corresponding to the beginning of a function is recorded
 * as the address + FUNCOFFSET (skip the link instruction so that
 * local information is available).
 */

#define NREG 17

#define FRP 14
#define STKP 15
#define PROGCTR 16

#define CALL_RETADDR	0x800c		/* Return address for 'call' command */
#define FUNCOFFSET 4

#ifdef sun
#    define CODESTART 0x8000
#else /* IRIS */
#   define CODESTART 0x1000
#endif

#define optab_init()

#define BITSPERBYTE 8
#define BITSPERWORD (BITSPERBYTE * sizeof(Word))

/*
 * This magic macro enables us to look at the process' registers
 * in its user structure.
 */

#define regloc(reg)	(ctob(UPAGES) + (sizeof(Word) * ((reg) - PC)) - 10)

#include "source.h"
#include "symbols.h"
#include <signal.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <machine/psl.h>
#include <machine/pte.h>
#include <sys/user.h>
#undef DELETE /* XXX */
#include <sys/vm.h>
#include <machine/reg.h>

Address pc;
Address prtaddr;

#endif

/*
 * Indices into u. for use in collecting registers values.
 */
public int rloc[] ={
#ifdef sun
    R0, R1, R2, R3, R4, R5, R6, R7, AR0, AR1, AR2, AR3, AR4, AR5, AR6, AR7, PC
#else /* IRIS */
    R0, R1, R2, R3, R4, R5, R6, R7, AR0, AR1, AR2, AR3, AR4, AR5, AR6, AR7, 16
#endif
};

private Address printop();

/*
 * Decode and print the instructions within the given address range.
 */

public printinst(lowaddr, highaddr)
Address lowaddr;
Address highaddr;
{
    register Address addr;

    for (addr = lowaddr; addr <= highaddr; ) {
	addr = printop(addr);
    }
    prtaddr = addr;
}

/*
 * Another approach:  print n instructions starting at the given address.
 */

public printninst(count, addr)
int count;
Address addr;
{
    register Integer i;
    register Address newaddr;

    if (count <= 0) {
	error("non-positive repetition count");
    } else {
	newaddr = addr;
	for (i = 0; i < count; i++) {
	    newaddr = printop(newaddr);
	}
	prtaddr = newaddr;
    }
}

/*
 * Print the contents of the addresses within the given range
 * according to the given format.
 */

typedef struct {
    String name;
    String printfstring;
    int length;
} Format;

private Format fmt[] = {
    { "d", " %d", sizeof(short) },
    { "D", " %ld", sizeof(long) },
    { "o", " %o", sizeof(short) },
    { "O", " %lo", sizeof(long) },
    { "x", " %04x", sizeof(short) },
    { "X", " %08x", sizeof(long) },
    { "b", " \\%o", sizeof(char) },
    { "c", " '%c'", sizeof(char) },
    { "s", "%c", sizeof(char) },
    { "f", " %f", sizeof(float) },
    { "g", " %g", sizeof(double) },
    { nil, nil, 0 }
};

private Format *findformat(s)
String s;
{
    register Format *f;

    f = &fmt[0];
    while (f->name != nil and not streq(f->name, s)) {
	++f;
    }
    if (f->name == nil) {
	error("bad print format \"%s\"", s);
    }
    return f;
}

/*
 * Retrieve and print out the appropriate data in the given format.
 * Floats have to be handled specially to allow the compiler to
 * convert them to doubles when passing to printf.
 */

private printformat (f, addr)
Format *f;
Address addr;
{
    union {
	char charv;
	short shortv;
	int intv;
	float floatv;
	double doublev;
    } value;

    value.intv = 0;
    dread(&value, addr, f->length);
    if (streq(f->name, "f")) {
	printf(f->printfstring, value.floatv);
    } else {
	printf(f->printfstring, value);
    }
}

public Address printdata(lowaddr, highaddr, format)
Address lowaddr;
Address highaddr;
String format;
{
    int n;
    register Address addr;
    Format *f;

    if (lowaddr > highaddr) {
	error("first address larger than second");
    }
    f = findformat(format);
    n = 0;
    for (addr = lowaddr; addr <= highaddr; addr += f->length) {
	if (n == 0) {
	    printf("%08x: ", addr);
	}
	printformat(f, addr);
	++n;
	if (n >= (16 div f->length)) {
	    printf("\n");
	    n = 0;
	}
    }
    if (n != 0) {
	printf("\n");
    }
    prtaddr = addr;
    return addr;
}

/*
 * The other approach is to print n items starting with a given address.
 */

public printndata(count, startaddr, format)
int count;
Address startaddr;
String format;
{
    int i, n;
    Address addr;
    Format *f;
    Boolean isstring;
    char c;

    if (count <= 0) {
	error("non-positive repetition count");
    }
    f = findformat(format);
    isstring = (Boolean) streq(f->name, "s");
    n = 0;
    addr = startaddr;
    for (i = 0; i < count; i++) {
	if (n == 0) {
	    printf("%08x: ", addr);
	}
	if (isstring) {
	    printf("\"");
	    dread(&c, addr, sizeof(char));
	    while (c != '\0') {
		printchar(c);
		++addr;
		dread(&c, addr, sizeof(char));
	    }
	    printf("\"\n");
	    n = 0;
	    addr += sizeof(String);
	} else {
	    printformat(f, addr);
	    ++n;
	    if (n >= (16 div f->length)) {
		printf("\n");
		n = 0;
	    }
	    addr += f->length;
	}
    }
    if (n != 0) {
	printf("\n");
    }
    prtaddr = addr;
}

/*
 * Print out a value according to the given format.
 */

public printvalue(v, format)
long v;
String format;
{
    Format *f;
    char *p, *q;

    f = findformat(format);
    if (streq(f->name, "s")) {
	putchar('"');
	p = (char *) &v;
	q = p + sizeof(v);
	while (p < q) {
	    printchar(*p);
	    ++p;
	}
	putchar('"');
    } else {
	printf(f->printfstring, v);
    }
    putchar('\n');
}

/*
 * Print out an execution time error.
 * Assumes the source position of the error has been calculated.
 *
 * Have to check if the -r option was specified; if so then
 * the object file information hasn't been read in yet.
 */

public printerror()
{
    extern Integer sys_nsig;
    extern String sys_siglist[];
    integer err;

    if (isfinished(process)) {
	err = exitcode(process);
	if (err == 0) {
	    printf("\"%s\" terminated normally\n", objname);
	} else {
	    printf("\"%s\" terminated abnormally (exit code %d)\n",
		objname, err
	    );
	}
	erecover();
    }
    err = errnum(process);
    putchar('\n');
    printsig(err);
    putchar(' ');
    printloc();
    putchar('\n');
    if (curline > 0) {
	printlines(curline, curline);
    } else {
	printinst(pc, pc);
    }
    erecover();
}

/*
 * Print out a signal.
 */

private String illinames[] = {
    "reserved addressing fault",
    "privileged instruction fault",
    "reserved operand fault"
};

private String fpenames[] = {
    nil,
    "integer overflow trap",
    "integer divide by zero trap",
    "floating overflow trap",
    "floating/decimal divide by zero trap",
    "floating underflow trap",
    "decimal overflow trap",
    "subscript out of range trap",
    "floating overflow fault",
    "floating divide by zero fault",
    "floating underflow fault"
};

public printsig (signo)
integer signo;
{
    integer code;

    if (signo < 0 or signo > sys_nsig) {
	printf("[signal %d]", signo);
    } else {
	printf("%s", sys_siglist[signo]);
    }
    code = errcode(process);
    if (signo == SIGILL) {
	if (code >= 0 and code < sizeof(illinames) / sizeof(illinames[0])) {
	    printf(" (%s)", illinames[code]);
	}
    } else if (signo == SIGFPE) {
	if (code > 0 and code < sizeof(fpenames) / sizeof(fpenames[0])) {
	    printf(" (%s)", fpenames[code]);
	}
    }
}

/*
 * Note the termination of the program.  We do this so as to avoid
 * having the process exit, which would make the values of variables
 * inaccessible.  We do want to flush all output buffers here,
 * otherwise it'll never get done.
 */

public endprogram()
{
    Integer exitcode;

    stepto(nextaddr(pc, true));
    printnews();
    exitcode = argn(1, nil);
    if (exitcode != 0) {
	printf("\nexecution completed (exit code %d)\n", exitcode);
    } else {
	printf("\nexecution completed\n");
    }
    getsrcpos();
    erecover();
}

/*
 * Single step the machine a source line (or instruction if "inst_tracing"
 * is true).  If "isnext" is true, skip over procedure calls.
 */

private Address getcall();

public dostep(isnext)
Boolean isnext;
{
    register Address addr;
    register Lineno line;
    String filename;
    Address startaddr;

    startaddr = pc;
    addr = nextaddr(pc, isnext);
    if (not inst_tracing and nlhdr.nlines != 0) {
	line = linelookup(addr);
	while (line == 0) {
	    addr = nextaddr(addr, isnext);
	    line = linelookup(addr);
	}
	curline = line;
    } else {
	curline = 0;
    }
    stepto(addr);
    filename = srcfilename(addr);
    setsource(filename);
}

typedef short Bpinst;

extern Bpinst BP_OP;
#ifdef sun
	asm("_BP_OP: trap #15");
#else /* IRIS */
	asm("_BP_OP: trap #1");
#endif

#define BP_ERRNO    SIGTRAP     /* signal received at a breakpoint */

/*
 * Setting a breakpoint at a location consists of saving
 * the word at the location and poking a BP_OP there.
 *
 * We save the locations and words on a list for use in unsetting.
 */

typedef struct Savelist *Savelist;

struct Savelist {
    Address location;
    Bpinst save;
    short refcount;
    Savelist link;
};

private Savelist savelist;

/*
 * Set a breakpoint at the given address.  Only save the word there
 * if it's not already a breakpoint.
 */

public setbp(addr)
Address addr;
{
    Bpinst w, save;
    register Savelist newsave, s;

    for (s = savelist; s != nil; s = s->link) {
	if (s->location == addr) {
	    s->refcount++;
	    return;
	}
    }
    iread(&save, addr, sizeof(save));
    newsave = new(Savelist);
    newsave->location = addr;
    newsave->save = save;
    newsave->refcount = 1;
    newsave->link = savelist;
    savelist = newsave;
    w = BP_OP;
    iwrite(&w, addr, sizeof(w));
}

/*
 * Unset a breakpoint; unfortunately we have to search the SAVELIST
 * to find the saved value.  The assumption is that the SAVELIST will
 * usually be quite small.
 */

public unsetbp(addr)
Address addr;
{
    register Savelist s, prev;

    prev = nil;
    for (s = savelist; s != nil; s = s->link) {
	if (s->location == addr) {
	    iwrite(&s->save, addr, sizeof(s->save));
	    s->refcount--;
	    if (s->refcount == 0) {
		if (prev == nil) {
		    savelist = s->link;
		} else {
		    prev->link = s->link;
		}
		dispose(s);
	    }
	    return;
	}
	prev = s;
    }
    panic("unsetbp: couldn't find address %d", addr);
}

/*
 * Instruction decoding routines for 68000, derived from adb.
 *
 * The shared boolean variable "printing" is true if the decoded
 * instruction is to be printed, false if not.  In either case,
 * the address of the next instruction after the given one is returned.
 */

private Boolean printing;
private Boolean following;
private Boolean followcalls;
private Address instaddr;

#define instread(var) \
{ \
    iread(&var, instaddr, sizeof(var)); \
    instaddr += sizeof(var); \
}

private Optab *decode(inst, addr)
Word inst;
Address addr;
{
    register Optab *o;

    o = &optab[0];
    while (o->mask != 0 and (inst&o->mask) != o->match) {
	++o;
    }
    return o;
}

private Address printop(addr)
Address addr;
{
    Optab *o;
    short inst;

    printf("%08x  ", addr);
    iread(&inst, addr, sizeof(inst));
    o = decode(inst, addr);
    if (o->mask == 0) {
	printf("\tbadop");
	instaddr = addr + sizeof(inst);
    } else {
	printing = true;
	following = false;
	instaddr = addr + sizeof(inst);
	(*o->opfun)(inst, o->farg);
	printing = false;
    }
    printf("\n");
    return instaddr;
}

/*
 * Quickly find the return address of the current procedure or function
 * while single stepping.  Just get the word pointed at by sp.
 */

private Address currtnaddr ()
{
    Address retaddr;

    dread(&retaddr, reg(STKP), sizeof(retaddr));
    return retaddr;
}

/*
 * Print out the effective address for the given parameters.
 */

private printea(mode, reg, size)
long mode, reg;
int size;
{
    long index, disp;
    static char *aregs[] = { "a0", "a1", "a2", "a3", "a4", "a5", "a6", "sp" };
    Byte b;
    short w;
    long l;

    switch ((int)(mode)) {
	case 0:
	    if (printing) {
		printf("d%D", reg);
	    }
	    break;

	case 1:
	    if (printing) {
		printf("%s", aregs[reg]);
	    }
	    break;

	case 2:
	    if (printing) {
		printf("%s@", aregs[reg]);
	    }
	    break;

	case 3:
	    if (printing) {
		printf("%s@+", aregs[reg]);
	    }
	    break;

	case 4:
	    if (printing) {
		printf("%s@-", aregs[reg]);
	    }
	    break;

	case 5:
	    instread(w);
	    if (printing) {
		printf("%s@(%D)", aregs[reg], w);
	    }
	    break;

	case 6:
	    instread(w);
	    if (printing) {
		index = w;
		disp = (char)(index&0377);
		printf("%s@(%d,%c%D:%c)", aregs[reg], disp,
		    (index&0100000)?'a':'d',(index>>12)&07,
		    (index&04000)?'l':'w');
	    }
	    break;

	case 7:
	    switch ((int)(reg)) {
		case 0:
		    instread(w);
		    if (printing) {
			index = w;
			psymoff(index);
		    }
		    break;

		case 1:
		    instread(l);
		    if (printing) {
			index = l;
			psymoff(index);
		    }
		    break;

		case 2:	
		    instread(w);
		    if (printing) {
			disp = w;
			psymoff(disp + instaddr);
		    }
		    break;

		case 3:
		    instread(w);
		    if (printing) {
			index = w;
			disp = (char)(index&0377);
			printf("pc@(%D,%c%D:%c)", disp,
			    (index&0100000)?'a':'d',(index>>12)&07,
			    (index&04000)?'l':'w');
		    }
		    break;

		case 4:
		    switch (size) {
			case sizeof(b):
			    instread(w);
			    index = (w&0xff);
			    break;

			case sizeof(w):
			    instread(w);
			    index = w;
			    break;

			case sizeof(l):
			    instread(l);
			    index = l;
			    break;

			default:
			    if (printing) {
			    	printf("unexpected size %d in printea\n", size);
			    }
			    instread(l);
			    index = l;
			    break;
		    }
		    if (printing) {
			printf(IMDF, index);
		    }
		    break;

		default:
		    if (printing) {
			printf("???");
		    }
		    break;
	    }
	    break;

	default:
	    if (printing) {
		printf("???");
	    }
	    break;
    }
}

private printEA(ea, size)
long ea;
int size;
{
    printea((ea>>3)&07, ea&07, size);
}

private mapsize(inst)
register long inst;
{
    int m;

    inst >>= 6;
    inst &= 03;
    switch (inst) {
	case 0:
	    m = 1;
	    break;

	case 1:
	    m = 2;
	    break;

	case 2:
	    m = 4;
	    break;

	default:
	    m = -1;
	    break;
    }
    return m;
}

private char suffix(size)
int size;
{
    char c;

    switch (size) {
	case 1:
	    c = 'b';
	    break;

	case 2:
	    c = 'w';
	    break;

	case 4:
	    c = 'l';
	    break;

	default:
	    panic("bad size %d in suffix", size);
    }
    return c;
}

/*
 * Print an address offset.  Eventually this should attempt to be symbolic,
 * but for now its just printed in hex.
 */

private psymoff (off)
Word off;
{
    Symbol f;

    f = whatblock((Address) (off + FUNCOFFSET));
    if (codeloc(f) == off + FUNCOFFSET) {
	printf("%s", symname(f));
    } else {
	printf("0x%x", off);
    }
}

/*
 * Instruction class specific routines.
 */

public omove(inst, s)
long inst;
String s;
{
    register int c;
    int size;

    c = s[0];
    if (printing) {
	printf("\tmov%c\t", c);
    }
    size = ((c == 'b') ? 1 : (c == 'w') ? 2 : 4);
    printea((inst>>3)&07, inst&07, size);
    if (printing) {
	printf(",");
    }
    printea((inst>>6)&07, (inst>>9)&07, size);
}

/* 
 * Two types: bsr (4 bytes) and bsrs (2 bytes)
 */

public obranch(inst, dummy)
long inst;
{
    long disp;
    String s; 
    short w;
    Address startingaddr;	/* address of branch instruction */
    int branchtype;		/* type of branch (0 = unconditional) */
    Address dest;
    Address retaddr;		/* for bsr instruction */

    startingaddr = instaddr - 2;
    disp = inst&0377;
    s = "s ";
    if (disp == 0) {
	retaddr = startingaddr + 4;
    } else {
	retaddr = startingaddr + 2;
    }
    if (disp > 127) {
	disp |= ~0377;
    } else if (disp == 0){
	s = " ";
	instread(w);
	disp = w;
    }
    branchtype = (int)((inst>>8)&017);
    dest = startingaddr + 2 + disp;
    if (printing) {
	printf("\tb%s%s\t", bname[branchtype], s);
	psymoff(dest);
    }
    if (following) {
	/*
	 * If we're to follow the dynamic flow of instructions,
	 * we must see where the branch leads.  A branchtype of 0
	 * indicates an unconditional branch which we simply take
	 * as the new instruction address.  For a conditional branch,
	 * we continue execution up to the current address, single step,
	 * and keep going.
	 */
	if (branchtype == 0) {
	    instaddr = dest;
	} else if (branchtype == 01) {		/* bsr */
	    if (followcalls) {
		steppast(startingaddr);
		curfunc = whatblock(pc, true);
		if (not isbperr()) {
		    printstatus();
		    /* NOTREACHED */
		}
		bpact();
		if (nosource(curfunc) and canskip(curfunc) and
		  nlhdr.nlines != 0) {
		    stepto(retaddr);
		    instaddr = pc;
		    bpact();
		} else {
		    callnews(/* iscall = */ true);
		}
	    }
	} else {
	    steppast(startingaddr);
	}
    }
}

public odbcc(inst, form)
long inst;
String form;
{
    long disp;
    short w;

    instread(w);
    if (printing) {
    	printf(form, dbname[(int)((inst>>8)&017)], inst&07);
	psymoff(w + sizeof(w));
    }
}

public oscc(inst, dummy)
long inst;
long dummy;
{
    if (printing) {
	printf("\ts%s\t", cname[(int)((inst>>8)&017)]);
    }
    printea((inst>>3)&07, inst&07, 1);
}

public biti(inst, dummy)
long inst;
long dummy;
{
    short w;

    if (printing) {
	printf("\t%s\t", bit[(int)((inst>>6)&03)]);
    }
    if (inst&0x0100) {
	if (printing) {
	    printf("d%D,", inst>>9);
	}
    } else {
	instread(w);
	if (printing) {
	    printf(IMDF, w);
	    printf(",");
	}
    }
    printEA(inst);
}

public opmode(inst, opcode)
long inst;
long opcode;
{
    register int opmode;
    register int reg;
    int size;

    opmode = (int)((inst>>6) & 07);
    reg = (int)((inst>>9) & 07);
    if (opmode == 0 or opmode == 4) {
	size = 1;
    } else if (opmode == 1 or opmode == 3 or opmode == 5) {
	size = 2;
    } else {
	size = 4;
    }
    if (printing) {
	printf("\t%s%c\t", opcode, suffix(size));
    }
    if (opmode >= 4 and opmode <= 6) {
	if (printing) {
	    printf("d%d,", reg);
	}
	printea((inst>>3)&07, inst&07, size);
    } else {
	printea((inst>>3)&07, inst&07, size);
	if (printing) {
	    printf(",%c%d",(opmode<=2) ? 'd' : 'a', reg);
	}
    }
}

public shroi(inst, ds)
long inst;
String ds;
{
    int rx, ry;
    String opcode;

    if ((inst & 0xC0) == 0xC0) {
	opcode = shro[(int)((inst>>9)&03)];
	if (printing) {
	    printf("\t%s%s\t", opcode, ds);
	}
	printEA(inst);
    } else {
	if (printing) {
	    opcode = shro[(int)((inst>>3)&03)];
	    printf("\t%s%s%c\t", opcode, ds, suffix(mapsize(inst)));
	    rx = (int)((inst>>9)&07); ry = (int)(inst&07);
	    if ((inst>>5)&01) {
		printf("d%d,d%d", rx, ry);
	    } else {
		printf(IMDF, (rx ? rx : 8));
		printf(",d%d", ry);
	    }
	}
    }
}		

public oimmed(inst, opcode)
long inst;
register String opcode;
{
    register int size;
    long const;
    short w;

    size = mapsize(inst);
    if (size > 0) {
	if (size == 4) {
	    instread(const);
	} else {
	    instread(w);
	    const = w;
	}
	if (printing) {
	    printf("\t%s%c\t", opcode, suffix(size));
	    printf(IMDF, const);
	    printf(",");
	}
	printEA(inst, size);
    } else {
	if (printing) {
	    printf("\tbadop");
	}
    }
}

public oreg(inst, opcode)
long inst;
register String opcode;
{
    if (printing) {
	printf(opcode, (inst & 07));
    }
}

public extend(inst, opcode)
long inst;
String opcode;
{
    register int size;
    int ry, rx;
    char c;

    if (printing) {
	size = mapsize(inst);
	ry = (inst&07);
	rx = ((inst>>9)&07);
	c = ((inst & 0x1000) ? suffix(size) : ' ');
	printf("\t%s%c\t", opcode, c);
	if (opcode[0] == 'e') {
	    if (inst & 0x0080) {
		printf("d%D,a%D", rx, ry);
	    } else if (inst & 0x0008) {
		printf("a%D,a%D", rx, ry);
	    } else {
		printf("d%D,d%D", rx, ry);
	    }
	} else if ((inst & 0xF000) == 0xB000) {
	    printf("a%D@+,a%D@+", ry, rx);
	} else if (inst & 0x8) {
	    printf("a%D@-,a%D@-", ry, rx);
	} else {
	    printf("d%D,d%D", ry, rx);
	}
    }
}

public olink(inst, dummy)
long inst;
long dummy;
{
    short w;

    instread(w);
    if (printing) {
	printf("\tlink\ta%D,", inst&07);
	printf(IMDF, w);
    }
}

public otrap(inst, dummy)
long inst;
{
    if (printing) {
	printf("\ttrap\t");
	printf(IMDF, inst&017);
    }
}

public oneop(inst, opcode)
long inst;
register String opcode;
{
    if (printing) {
	printf("\t%s",opcode);
    }
    printEA(inst);
}

public jsrop(inst, opcode)
long inst;
register String opcode;
{
    Address startingaddr;	/* beginning of jsr instruction */
    Address retaddr; /* can't call return_addr (frame not set up yet) */

    startingaddr = instaddr - 2;
    switch ((inst >> 3) & 07) {
	case 2:
	    retaddr = instaddr;		/* two byte instruction */
	    break;
	case 5:
	case 6:
	    retaddr = instaddr + 2;	/* four byte instruction */
	    break;
	case 7:
	default:
	    switch (inst & 07) {
		case 0:
		case 2:
		case 3:
		    retaddr = instaddr + 2;
		    break;
		case 1:
		default:
		    retaddr = instaddr + 4;	/* six byte instruction */
		    break;
	    }
	    break;
    }
    if (printing) {
	printf("\t%s",opcode);
    }
    printEA(inst);
    if (following and followcalls) {
	steppast(startingaddr);
	curfunc = whatblock(pc, true);
	if (not isbperr()) {
	    printstatus();
	    /* NOTREACHED */
	}
	bpact();
	if (nosource(curfunc) and canskip(curfunc) and nlhdr.nlines != 0) {
	    stepto(retaddr);
	    instaddr = pc;
	    bpact();
	} else {
	    callnews(/* iscall = */ true);
	}
    }
}

public jmpop(inst, opcode)
long inst;
register String opcode;
{
    Address startingaddr;	/* beginning of jump instruction */

    startingaddr = instaddr - 2;
    if (printing) {
	printf("\t%s",opcode);
    }
    printEA(inst);
    if (following) {
	steppast(startingaddr);
    }
}

public pregmask(mask)
register int mask;
{
    register int i;
    register int flag = 0;

    if (printing) {
	printf("#<");
	for (i=0; i<16; i++) {
	    if (mask&1) {
		if (flag) {
		    printf(",");
		} else {
		    ++flag;
		}
		printf("%c%d",(i<8) ? 'd' : 'a', i&07);
	    }
	    mask >>= 1;
	}
	printf(">");
    }
}

public omovem(inst, dummy)
long inst;
long dummy;
{
    register int i, list, mask;
    register int reglist;
    short w;

    i = 0;
    list = 0;
    mask = 0100000;
    instread(w);
    reglist = w;
    if ((inst & 070) == 040) {	/* predecrement */
	for (i = 15; i > 0; i -= 2) {
	    list |= ((mask & reglist) >> i);
	    mask >>= 1;
	}
	for (i = 1; i < 16; i += 2) {
	    list |= ((mask & reglist) << i);
	    mask >>= 1;
	}
	reglist = list;
    }
    if (printing) {
	printf("\tmovem%c\t",(inst&100)?'l':'w');
    }
    if (inst&02000) {
	printEA(inst);
	if (printing) {
	    printf(",");
	}
	pregmask(reglist);
    } else {
	pregmask(reglist);
	if (printing) {
	    printf(",");
	}
	printEA(inst);
    }
}

public ochk(inst, opcode)
long inst;
register String opcode;
{
    if (printing) {
	printf("\t%s\t", opcode);
    }
    printEA(inst, sizeof(Byte));
    if (printing) {
	printf(",%c%D", (opcode[0] == 'l') ? 'a' : 'd', (inst>>9)&07);
    }
}

public soneop(inst, opcode)
long inst;
register String opcode;
{
    register int size;

    size = mapsize(inst);
    if (size > 0) {
	if (printing) {
	    printf("\t%s%c\t", opcode, suffix(size));
	}
	printEA(inst);
    } else {
	if (printing) {
	    printf("\tbadop");
	}
    }
}

public oquick(inst, opcode)
long inst;
register String opcode;
{
    register int size;
    register int data;

    size = mapsize(inst);
    data = (int)((inst>>9) & 07);
    if (data == 0) {
	data = 8;
    }
    if (size > 0) {
	if (printing) {
	    printf("\t%s%c\t", opcode, suffix(size));
	    printf(IMDF, data);
	    printf(",");
	}
	printEA(inst);
    } else {
	if (printing) {
	    printf("\tbadop");
	}
    }
}

public omoveq(inst, dummy)
long inst;
long dummy;
{
    register int data;

    if (printing) {
	data = (int)(inst & 0377);
	if (data > 127) {
	    data |= ~0377;
	}
	printf("\tmoveq\t");
	printf(IMDF, data);
	printf(",d%D", (inst>>9)&07);
    }
}

public oprint(inst, opcode)
long inst;
register String opcode;
{
    if (printing) {
	printf("\t%s",opcode);
    }
}

public ostop(inst, opcode)
long inst;
register String opcode;
{
    short w;

    instread(w);
    if (printing) {
	printf(opcode, w);
    }
}

public orts(inst, opcode)
long inst;
register String opcode;
{
    Address addr;

    if (following) {
	callnews(/* iscall = */ false);
    	if (inst_tracing) {
	    addr = currtnaddr();
    	} else {
	    addr = return_addr();
	    if (addr == 0) {
		stepto(instaddr - 2);
		addr = currtnaddr();
	    }
	}
	stepto(addr);
	instaddr = pc;
    }
    if (printing) {
	printf("\t%s",opcode);
    }
}

/*
 * Not used by C compiler; does an rts but before doing so, pops
 * arg bytes from the stack.
 */

public ortspop(inst, opcode)
long inst;
register String opcode;
{
    Address addr;
    short w;

    instread(w);
    if (following) {
	callnews(/* iscall = */ false);
    	if (inst_tracing) {
	    addr = currtnaddr();
    	} else {
	    addr = return_addr();
	}
	stepto(addr);
	instaddr = pc;
    }
    if (printing) {
	printf(opcode, w);
    }
}

public omovs(inst, opcode)
long inst;
String opcode;
{
    register int size;
    register unsigned int controlword;
    short w;

    size = mapsize(inst);
    instread(w);
    controlword = w >> 11;
    if (printing) {
	printf("\t%s%c\t", opcode, suffix(size));
    }
    if (controlword & 1){
	controlword >>= 1;
	if (printing) {
	    printf((controlword&0x8) ? "a%D," : "d%D,", controlword&7 );
	}
	printEA(inst&0xff, size);
    } else {
	controlword >>= 1;
	printEA(inst&0xff, size);
	if (printing) {
	    printf((controlword&0x8) ? ",a%D" : ",d%D", controlword&7);
	}
    }
}

public omovc(inst, opcode)
long inst;
String opcode;
{
    register unsigned int controlword;
    String creg;
    short w;

    instread(w);
    if (printing) {
	controlword = w;
	switch (controlword & 0xfff) {
	    case 0:
		creg = "sfc";
		break;

	    case 1:
		creg = "dfc";
		break;

	    case 0x800:
		creg = "usp";
		break;

	    case 0x801:
		creg = "vbr";
		break;

	    default:
		creg = "???";
		break;
	}
	controlword >>= 12;
	if (inst & 1){
	    printf((controlword&0x8) ? "%sa%D,%s" : "%sd%D,%s",
		opcode, controlword&7, creg );
	} else {
	    printf((controlword&0x8) ? "%s%s,a%D" : "%s%s,d%D",
		opcode, creg, controlword&7 );
	}
    }
}

/*
 * Compute the next address that will be executed from the given one.
 * If "isnext" is true then consider a procedure call as straight line code.
 *
 * Unconditional branches we just follow, for conditional branches
 * we continue execution to the current location and then single step
 * the machine.
 */

public Address nextaddr(startaddr, isnext)
Address startaddr;
Boolean isnext;
{
    Optab *o;
    short inst;

    instaddr = usignal(process);
    if (instaddr == 0 or instaddr == 1) {
	following = true;
	followcalls = (Boolean) (not isnext);
	printing = false;
	iread(&inst, startaddr, sizeof(inst));
	instaddr = startaddr + sizeof(inst);
	o = decode(inst, startaddr);
	if (o->mask == 0) {
	    fprintf(stderr,
		"[internal error: undecodable op at 0x%x]\n", startaddr);
	    fflush(stderr);
	} else {
	    (*o->opfun)(inst, o->farg);
	}
	following = false;
    }
    return instaddr;
}

/*
 * Step to the given address and then execute one instruction past it.
 * Set instaddr to the new instruction address.
 */

private steppast(addr)
Address addr;
{
    stepto(addr);
    pstep(process, DEFSIG);
    pc = reg(PROGCTR);
    instaddr = pc;
}

/*
 * Enter a procedure by creating and executing a call instruction.
 */

#define CALLSIZE 6	/* size of call instruction */

public beginproc(p)
Symbol p;
{
    char save[CALLSIZE];
    struct {
	short op;
	char addr[sizeof(long)];	/* unaligned long */
    } call;
    long dest;

    pc = CODESTART + 6;
    iread(save, pc, sizeof(save));
    call.op = 0x4eb9;			/* jsr */
    dest = codeloc(p) - FUNCOFFSET;
    mov(&dest, call.addr, sizeof(call.addr));
    iwrite(&call, pc, sizeof(call));
    setreg(PROGCTR, pc);
    pstep(process, DEFSIG);
    iwrite(save, pc, sizeof(save));
    pc = reg(PROGCTR);
    if (not isbperr()) {
	printstatus();
    }
    /*
     * Execute link instruction so the return addr is visible.
     */
    pstep(process, DEFSIG);
    pc = reg(PROGCTR);
    if (not isbperr()) {
	printstatus();
    }
}

/*
 * Special variables for debugging the kernel.
 */

public integer masterpcbb;
public integer slr;
public struct pte *sbr;
private struct pcb pcb;

public getpcb ()
{
    integer i;

    fseek(corefile, masterpcbb & ~0x80000000, 0);
    get(corefile, pcb);
    pcb.pcb_p0lr &= ~AST_CLR;
    printf("p0br %lx p0lr %lx p1br %lx p1lr %lx\n",
	pcb.pcb_p0br, pcb.pcb_p0lr, pcb.pcb_p1br, pcb.pcb_p1lr
    );
#   ifdef sun
    for (i = 0; i < 14; i++) {
	setreg(i, pcb.pcb_regs.val[i]);
    }
#   else /* IRIS */
    for (i = 0; i < 14; i++) {
	setreg(i, pcb.pcb_regs[i]);
    }
#   endif
}

public copyregs (savreg, reg)
Word savreg[], reg[];
{
    reg[0] = savreg[R0];
    reg[1] = savreg[R1];
    reg[2] = savreg[R2];
    reg[3] = savreg[R3];
    reg[4] = savreg[R4];
    reg[5] = savreg[R5];
    reg[6] = savreg[R6];
    reg[7] = savreg[R7];
    reg[8] = savreg[AR0];
    reg[9] = savreg[AR1];
    reg[10] = savreg[AR2];
    reg[11] = savreg[AR3];
    reg[12] = savreg[AR4];
    reg[13] = savreg[AR5];
    reg[14] = savreg[AR6];
    reg[15] = savreg[AR7];
    reg[PROGCTR] = savreg[PC];
}

/*
 * Map a virtual address to a physical address.
 * XXX THIS CAN'T BE RIGHT... XXX
 */

public Address vmap (addr)
Address addr;
{
    Address r;
    integer v, n;
    struct pte pte;

    r = addr & ~0xc0000000;
    v = btop(r);
    switch (addr&0xc0000000) {
	case 0xc0000000:
	case 0x80000000:
	    /*
	     * In system space, so get system pte.
	     * If it is valid or reclaimable then the physical address
	     * is the combination of its page number and the page offset
	     * of the original address.
	     */
	    if (v >= slr) {
		error("address %x out of segment", addr);
	    }
	    r = ((long) (sbr + v)) & ~0x80000000;
	    goto simple;

	case 0x40000000:
	    /*
	     * In p1 space, must not be in shadow region.
	     */
	    if (v < pcb.pcb_p1lr) {
		error("address %x out of segment", addr);
	    }
	    r = (Address) (pcb.pcb_p1br + v);
	    break;

	case 0x00000000:
	    /*
	     * In p0 space, must not be off end of region.
	     */
	    if (v >= pcb.pcb_p0lr) {
		error("address %x out of segment", addr);
	    }
	    r = (Address) (pcb.pcb_p0br + v);
	    break;

	default:
	    /* do nothing */
	    break;
    }
    /*
     * For p0/p1 address, user-level page table should be in
     * kernel virtual memory.  Do second-level indirect by recursing.
     */
    if ((r & 0x80000000) == 0) {
	error("bad p0br or p1br in pcb");
    }
    r = vmap(r);
simple:
    /*
     * "r" is now the address of the pte of the page
     * we are interested in; get the pte and paste up the physical address.
     */
    fseek(corefile, r, 0);
    n = fread(&pte, sizeof(pte), 1, corefile);
    if (n != 1) {
	error("page table botch (fread at %x returns %d)", r, n);
    }
    if (pte.pg_v == 0 and (pte.pg_fod != 0 or pte.pg_pfnum == 0)) {
	error("page no valid or reclamable");
    }
    return (addr&PGOFSET) + ((Address) ptob(pte.pg_pfnum));
}

/*
 * Extract a bit field from an integer.
 */

public integer extractField (s)
Symbol s;
{
    integer nbytes, nbits, n, r, off, len;

    off = s->symvalue.field.offset;
    len = s->symvalue.field.length;
    nbytes = size(s);
    n = 0;
    if (nbytes > sizeof(n)) {
	printf("[bad size in extractField -- word assumed]\n");
	nbytes = sizeof(n);
    }
    popn(nbytes, ((char *) &n) + (sizeof(Word) - nbytes));
    nbits = nbytes * BITSPERBYTE;
    r = n >> (nbits - ((off mod nbits) + len));
    r &= ((1 << len) - 1);
    return r;
}

/*
 * Change the length of a value in memory according to a given difference
 * in the lengths of its new and old types.
 */

public loophole (oldlen, newlen)
integer oldlen, newlen;
{
    integer i, n;
    Stack *oldsp;

    n = newlen - oldlen;
    oldsp = sp - oldlen;
    if (n > 0) {
	for (i = oldlen - 1; i >= 0; i--) {
	    oldsp[n + i] = oldsp[i];
	}
	for (i = 0; i < n; i++) {
	    oldsp[i] = '\0';
	}
    } else {
	for (i = 0; i < newlen; i++) {
	    oldsp[i] = oldsp[i - n];
	}
    }
    sp += n;
}
