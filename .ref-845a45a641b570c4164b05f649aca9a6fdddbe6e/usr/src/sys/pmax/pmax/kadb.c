/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kadb.c	7.5 (Berkeley) %G%
 */

/*
 * Define machine dependent primitives for kdb.
 */

#include <kdb/defs.h>
#undef SP
#include <machine/reg.h>
#include <machine/trap.h>
#include <machine/mips_opcode.h>

REGLIST	kdbreglist[] = {
	/* register_name, address */
	"AT", &kdbpcb.pcb_regs[AST],
	"v0", &kdbpcb.pcb_regs[V0],
	"v1", &kdbpcb.pcb_regs[V1],
	"a0", &kdbpcb.pcb_regs[A0],
	"a1", &kdbpcb.pcb_regs[A1],
	"a2", &kdbpcb.pcb_regs[A2],
	"a3", &kdbpcb.pcb_regs[A3],
	"t0", &kdbpcb.pcb_regs[T0],
	"t1", &kdbpcb.pcb_regs[T1],
	"t2", &kdbpcb.pcb_regs[T2],
	"t3", &kdbpcb.pcb_regs[T3],
	"t4", &kdbpcb.pcb_regs[T4],
	"t5", &kdbpcb.pcb_regs[T5],
	"t6", &kdbpcb.pcb_regs[T6],
	"t7", &kdbpcb.pcb_regs[T7],
	"t8", &kdbpcb.pcb_regs[T8],
	"t9", &kdbpcb.pcb_regs[T9],
	"s0", &kdbpcb.pcb_regs[S0],
	"s1", &kdbpcb.pcb_regs[S1],
	"s2", &kdbpcb.pcb_regs[S2],
	"s3", &kdbpcb.pcb_regs[S3],
	"s4", &kdbpcb.pcb_regs[S4],
	"s5", &kdbpcb.pcb_regs[S5],
	"s6", &kdbpcb.pcb_regs[S6],
	"s7", &kdbpcb.pcb_regs[S7],
	"s8", &kdbpcb.pcb_regs[S8],
	"sp", &kdbpcb.pcb_regs[SP],
	"gp", &kdbpcb.pcb_regs[GP],
	"ra", &kdbpcb.pcb_regs[RA],
	"mullo", &kdbpcb.pcb_regs[MULLO],
	"mulhi", &kdbpcb.pcb_regs[MULHI],
	"SR", &kdbpcb.pcb_regs[SR],
	"pc", &kdbpcb.pcb_regs[PC],
	"f0", &kdbpcb.pcb_regs[F0],
	"f1", &kdbpcb.pcb_regs[F1],
	"f2", &kdbpcb.pcb_regs[F2],
	"f3", &kdbpcb.pcb_regs[F3],
	"f4", &kdbpcb.pcb_regs[F4],
	"f5", &kdbpcb.pcb_regs[F5],
	"f6", &kdbpcb.pcb_regs[F6],
	"f7", &kdbpcb.pcb_regs[F7],
	"f8", &kdbpcb.pcb_regs[F8],
	"f9", &kdbpcb.pcb_regs[F9],
	"f10", &kdbpcb.pcb_regs[F10],
	"f11", &kdbpcb.pcb_regs[F11],
	"f12", &kdbpcb.pcb_regs[F12],
	"f13", &kdbpcb.pcb_regs[F13],
	"f14", &kdbpcb.pcb_regs[F14],
	"f15", &kdbpcb.pcb_regs[F15],
	"f16", &kdbpcb.pcb_regs[F16],
	"f17", &kdbpcb.pcb_regs[F17],
	"f18", &kdbpcb.pcb_regs[F18],
	"f19", &kdbpcb.pcb_regs[F19],
	"f20", &kdbpcb.pcb_regs[F20],
	"f21", &kdbpcb.pcb_regs[F21],
	"f22", &kdbpcb.pcb_regs[F22],
	"f23", &kdbpcb.pcb_regs[F23],
	"f24", &kdbpcb.pcb_regs[F24],
	"f25", &kdbpcb.pcb_regs[F25],
	"f26", &kdbpcb.pcb_regs[F26],
	"f27", &kdbpcb.pcb_regs[F27],
	"f28", &kdbpcb.pcb_regs[F28],
	"f29", &kdbpcb.pcb_regs[F29],
	"f30", &kdbpcb.pcb_regs[F30],
	"f31", &kdbpcb.pcb_regs[F31],
	0, 0
};

static char *op_name[64] = {
/* 0 */	"spec",	"bcond","j",	"jal",	"beq",	"bne",	"blez",	"bgtz",
/* 8 */	"addi",	"addiu","slti",	"sltiu","andi",	"ori",	"xori",	"lui",
/*16 */	"cop0",	"cop1",	"cop2",	"cop3",	"op24",	"op25",	"op26",	"op27",
/*24 */	"op30",	"op31",	"op32",	"op33",	"op34",	"op35",	"op36",	"op37",
/*32 */	"lb",	"lh",	"lwl",	"lw",	"lbu",	"lhu",	"lwr",	"ld",
/*40 */	"sb",	"sh",	"swl",	"sw",	"op54",	"op55",	"swr",	"sd",
/*48 */	"lwc0",	"lwc1",	"lwc2",	"lwc3",	"ldc0",	"ldc1",	"ldc2",	"ldc3",
/*56 */	"swc0",	"swc1",	"swc2",	"swc3",	"sdc0",	"sdc1",	"sdc2",	"sdc3"
};

static char *spec_name[64] = {
/* 0 */	"sll",	"spec01","srl",	"sra",	"sllv",	"spec05","srlv","srav",
/* 8 */	"jr",	"jalr",	"spec12","spec13","syscall","break","spec16","tas",
/*16 */	"mfhi",	"mthi",	"mflo",	"mtlo",	"spec24","spec25","spec26","spec27",
/*24 */	"mult",	"multu","div",	"divu",	"spec34","spec35","spec36","spec37",
/*32 */	"add",	"addu",	"sub",	"subu",	"and",	"or",	"xor",	"nor",
/*40 */	"spec50","spec51","slt","sltu",	"spec54","spec55","spec56","spec57",
/*48 */	"spec60","spec61","spec62","spec63","spec64","spec65","spec66","spec67",
/*56 */	"spec70","spec71","spec72","spec73","spec74","spec75","spec76","spec77"
};

static char *bcond_name[32] = {
/* 0 */	"bltz",	"bgez", "?", "?", "?", "?", "?", "?",
/* 8 */	"?", "?", "?", "?", "?", "?", "?", "?",
/*16 */	"bltzal", "bgezal", "?", "?", "?", "?", "?", "?",
/*24 */	"?", "?", "?", "?", "?", "?", "?", "?",
};

static char *cop1_name[64] = {
/* 0 */	"fadd",	"fsub",	"fmpy",	"fdiv",	"fsqrt","fabs",	"fmov",	"fneg",
/* 8 */	"fop08","fop09","fop0a","fop0b","fop0c","fop0d","fop0e","fop0f",
/*16 */	"fop10","fop11","fop12","fop13","fop14","fop15","fop16","fop17",
/*24 */	"fop18","fop19","fop1a","fop1b","fop1c","fop1d","fop1e","fop1f",
/*32 */	"fcvts","fcvtd","fcvte","fop23","fcvtw","fop25","fop26","fop27",
/*40 */	"fop28","fop29","fop2a","fop2b","fop2c","fop2d","fop2e","fop2f",
/*48 */	"fcmp.f","fcmp.un","fcmp.eq","fcmp.ueq","fcmp.olt","fcmp.ult",
	"fcmp.ole","fcmp.ule",
/*56 */	"fcmp.sf","fcmp.ngle","fcmp.seq","fcmp.ngl","fcmp.lt","fcmp.nge",
	"fcmp.le","fcmp.ngt"
};

static char *fmt_name[16] = {
	"s",	"d",	"e",	"fmt3",
	"w",	"fmt5",	"fmt6",	"fmt7",
	"fmt8",	"fmt9",	"fmta",	"fmtb",
	"fmtc",	"fmtd",	"fmte",	"fmtf"
};

static char *reg_name[32] = {
	"zero",	"at",	"v0",	"v1",	"a0",	"a1",	"a2",	"a3",
	"t0",	"t1",	"t2",	"t3",	"t4",	"t5",	"t6",	"t7",
	"s0",	"s1",	"s2",	"s3",	"s4",	"s5",	"s6",	"s7",
	"t8",	"t9",	"k0",	"k1",	"gp",	"sp",	"s8",	"ra"
};

static char *c0_opname[64] = {
	"c0op00","tlbr",  "tlbwi", "c0op03","c0op04","c0op05","tlbwr", "c0op07",
	"tlbp",  "c0op11","c0op12","c0op13","c0op14","c0op15","c0op16","c0op17",
	"rfe",   "c0op21","c0op22","c0op23","c0op24","c0op25","c0op26","c0op27",
	"c0op30","c0op31","c0op32","c0op33","c0op34","c0op35","c0op36","c0op37",
	"c0op40","c0op41","c0op42","c0op43","c0op44","c0op45","c0op46","c0op47",
	"c0op50","c0op51","c0op52","c0op53","c0op54","c0op55","c0op56","c0op57",
	"c0op60","c0op61","c0op62","c0op63","c0op64","c0op65","c0op66","c0op67",
	"c0op70","c0op71","c0op72","c0op73","c0op74","c0op75","c0op77","c0op77",
};

static char *c0_reg[32] = {
	"index","random","tlblo","c0r3","context","c0r5","c0r6","c0r7",
	"badvaddr","c0r9","tlbhi","c0r11","sr",	"cause","epc",	"c0r15",
	"c0r16","c0r17","c0r18","c0r19","c0r20","c0r21","c0r22","c0r23",
	"c0r24","c0r25","c0r26","c0r27","c0r28","c0r29","c0r30","c0r31"
};

/*
 * Print the cause of the trap to kdb.
 */
void
kdbprinttrap(causeReg, vadr)
	unsigned causeReg, vadr;
{
	int type, pc;
	extern char *trap_type[];

	type = (causeReg & MACH_CR_EXC_CODE) >> MACH_CR_EXC_CODE_SHIFT;

	/* check to see if we are entering kdb via kdbpanic() */
	pc = kdbpcb.pcb_regs[PC];
	if (type == T_BREAK && pc < 0) {
		if (kdbpeek(pc) == MACH_BREAK_KDB)
			kdbpcb.pcb_regs[PC] = pc + 4;
	}

	kdbprintf("trap: %s\n", trap_type[type]);
}

unsigned kdb_ss_addr;
unsigned kdb_ss_instr;

void
kdbsetsstep()
{
	register unsigned va;
	register int *locr0 = kdbpcb.pcb_regs;
	int i;

	/* compute next address after current location */
	va = MachEmulateBranch(locr0, locr0[PC], 0, 1);
	if (kdb_ss_addr) {
		kdbprintf("kdbsetsstep: breakpoint already set at %x (va %x)\n",
			kdb_ss_addr, va);
		return;
	}
	kdb_ss_addr = va;

	if ((int)va < 0) {
		/* kernel address */
		kdb_ss_instr = kdbpeek(va);
		kdbpoke((caddr_t)va, MACH_BREAK_SSTEP);
		return;
	}

	kdb_ss_instr = fuiword(va);
	i = suiword((caddr_t)va, MACH_BREAK_SSTEP);
	if (i < 0) {
		register struct proc *p = curproc;
		vm_offset_t sa, ea;
		int rv;

		sa = trunc_page((vm_offset_t)va);
		ea = round_page((vm_offset_t)va+sizeof(int)-1);
		rv = vm_map_protect(&p->p_vmspace->vm_map, sa, ea,
			VM_PROT_DEFAULT, FALSE);
		if (rv == KERN_SUCCESS) {
			i = suiword((caddr_t)va, MACH_BREAK_SSTEP);
			(void) vm_map_protect(&p->p_vmspace->vm_map,
				sa, ea, VM_PROT_READ|VM_PROT_EXECUTE, FALSE);
		}
	}
	if (i < 0)
		return;
}

void
kdbclrsstep()
{
	register unsigned cr, pc, va;
	unsigned instr;
	int i;

	/* ignore this trap if it is not a break trap */
	cr = kdbvar[kdbvarchk('t')];
	if ((cr & MACH_CR_EXC_CODE) != (T_BREAK << MACH_CR_EXC_CODE_SHIFT))
		return;

	/* fix pc if break instruction is in the delay slot */
	pc = kdbpcb.pcb_regs[PC];
	if ((int)cr < 0)
		pc += 4;

	/* check to be sure its the one we are expecting */
	va = kdb_ss_addr;
	if (!va || va != pc)
		return;

	/* read break instruction */
	instr = kdbpeek(va);
	if (instr != MACH_BREAK_SSTEP)
		return;

	if ((int)va < 0) {
		/* kernel address */
		kdbpoke((caddr_t)va, kdb_ss_instr);
		kdb_ss_addr = 0;
		return;
	}

	/* restore original instruction and clear BP */
	i = suiword((caddr_t)va, kdb_ss_instr);
	if (i < 0) {
		register struct proc *p = curproc;
		vm_offset_t sa, ea;
		int rv;

		sa = trunc_page((vm_offset_t)va);
		ea = round_page((vm_offset_t)va+sizeof(int)-1);
		rv = vm_map_protect(&p->p_vmspace->vm_map, sa, ea,
			VM_PROT_DEFAULT, FALSE);
		if (rv == KERN_SUCCESS) {
			i = suiword((caddr_t)va, p->p_md.md_ss_instr);
			(void) vm_map_protect(&p->p_vmspace->vm_map,
				sa, ea, VM_PROT_READ|VM_PROT_EXECUTE,
				FALSE);
		}
	}
	if (i < 0)
		kdbprintf("can't clear break at %x\n", va);
	kdb_ss_addr = 0;
}

void
kdbreadc(lp)
	char *lp;
{
	int c;

	c = cngetc();
	if (c == '\r')
		c = '\n';
	*lp = c;
}

void
kdbwrite(lp, len)
	char *lp;
	int len;
{
	while (len-- > 0)
		cnputc(*lp++);
}

/* ARGSUSED */
void
kdbprintins(space, ins)
	int space;
	long ins;
{
	InstFmt i;

	i.word = ins;

	switch (i.JType.op) {
	case OP_SPECIAL:
		if (i.word == 0) {
			kdbprintf("nop");
			break;
		}
		if (i.RType.func == OP_ADDU && i.RType.rt == 0) {
			kdbprintf("move\t%s,%s",
				reg_name[i.RType.rd],
				reg_name[i.RType.rs]);
			break;
		}
		kdbprintf("%s", spec_name[i.RType.func]);
		switch (i.RType.func) {
		case OP_SLL:
		case OP_SRL:
		case OP_SRA:
			kdbprintf("\t%s,%s,%d",
				reg_name[i.RType.rd],
				reg_name[i.RType.rt],
				i.RType.shamt);
			break;

		case OP_SLLV:
		case OP_SRLV:
		case OP_SRAV:
			kdbprintf("\t%s,%s,%s",
				reg_name[i.RType.rd],
				reg_name[i.RType.rt],
				reg_name[i.RType.rs]);
			break;

		case OP_MFHI:
		case OP_MFLO:
			kdbprintf("\t%s", reg_name[i.RType.rd]);
			break;

		case OP_JR:
		case OP_JALR:
		case OP_MTLO:
		case OP_MTHI:
			kdbprintf("\t%s", reg_name[i.RType.rs]);
			break;

		case OP_MULT:
		case OP_MULTU:
		case OP_DIV:
		case OP_DIVU:
			kdbprintf("\t%s,%s",
				reg_name[i.RType.rs],
				reg_name[i.RType.rt]);
			break;

		case OP_SYSCALL:
			break;

		case OP_BREAK:
			kdbprintf("\t%d", (i.RType.rs << 5) | i.RType.rt);
			break;

		default:
			kdbprintf("\t%s,%s,%s",
				reg_name[i.RType.rd],
				reg_name[i.RType.rs],
				reg_name[i.RType.rt]);
		};
		break;

	case OP_BCOND:
		kdbprintf("%s\t%s,", bcond_name[i.IType.rt],
			reg_name[i.IType.rs]);
		goto pr_displ;

	case OP_BLEZ:
	case OP_BGTZ:
		kdbprintf("%s\t%s,", op_name[i.IType.op],
			reg_name[i.IType.rs]);
		goto pr_displ;

	case OP_BEQ:
		if (i.IType.rs == 0 && i.IType.rt == 0) {
			kdbprintf("b\t");
			goto pr_displ;
		}
		/* FALLTHROUGH */
	case OP_BNE:
		kdbprintf("%s\t%s,%s,", op_name[i.IType.op],
			reg_name[i.IType.rs],
			reg_name[i.IType.rt]);
	pr_displ:
		kdbpsymoff(kdbdot + 4 + ((short)i.IType.imm << 2), ISYM, "");
		break;

	case OP_COP0:
		switch (i.RType.rs) {
		case OP_BCx:
		case OP_BCy:
			kdbprintf("bc0%c\t",
				"ft"[i.RType.rt & COPz_BC_TF_MASK]);
			goto pr_displ;

		case OP_MT:
			kdbprintf("mtc0\t%s,%s",
				reg_name[i.RType.rt],
				c0_reg[i.RType.rd]);
			break;

		case OP_MF:
			kdbprintf("mfc0\t%s,%s",
				reg_name[i.RType.rt],
				c0_reg[i.RType.rd]);
			break;

		default:
			kdbprintf("%s", c0_opname[i.FRType.func]);
		};
		break;

	case OP_COP1:
		switch (i.RType.rs) {
		case OP_BCx:
		case OP_BCy:
			kdbprintf("bc1%c\t",
				"ft"[i.RType.rt & COPz_BC_TF_MASK]);
			goto pr_displ;

		case OP_MT:
			kdbprintf("mtc1\t%s,f%d",
				reg_name[i.RType.rt],
				i.RType.rd);
			break;

		case OP_MF:
			kdbprintf("mfc1\t%s,f%d",
				reg_name[i.RType.rt],
				i.RType.rd);
			break;

		case OP_CT:
			kdbprintf("ctc1\t%s,f%d",
				reg_name[i.RType.rt],
				i.RType.rd);
			break;

		case OP_CF:
			kdbprintf("cfc1\t%s,f%d",
				reg_name[i.RType.rt],
				i.RType.rd);
			break;

		default:
			kdbprintf("%s.%s\tf%d,f%d,f%d",
				cop1_name[i.FRType.func],
				fmt_name[i.FRType.fmt],
				i.FRType.fd, i.FRType.fs, i.FRType.ft);
		};
		break;

	case OP_J:
	case OP_JAL:
		kdbprintf("%s\t", op_name[i.JType.op]);
		kdbpsymoff((kdbdot & 0xF0000000) | (i.JType.target << 2),
			ISYM, "");
		break;

	case OP_LWC1:
	case OP_SWC1:
		kdbprintf("%s\tf%d,", op_name[i.IType.op],
			i.IType.rt);
		goto loadstore;

	case OP_LB:
	case OP_LH:
	case OP_LW:
	case OP_LBU:
	case OP_LHU:
	case OP_SB:
	case OP_SH:
	case OP_SW:
		kdbprintf("%s\t%s,", op_name[i.IType.op],
			reg_name[i.IType.rt]);
	loadstore:
		kdbprintf("%d(%s)", (short)i.IType.imm,
			reg_name[i.IType.rs]);
		break;

	case OP_ORI:
	case OP_XORI:
		if (i.IType.rs == 0) {
			kdbprintf("li\t%s,0x%X",
				reg_name[i.IType.rt],
				i.IType.imm);
			break;
		}
		/* FALLTHROUGH */
	case OP_ANDI:
		kdbprintf("%s\t%s,%s,0x%X", op_name[i.IType.op],
			reg_name[i.IType.rt],
			reg_name[i.IType.rs],
			i.IType.imm);
		break;

	case OP_LUI:
		kdbprintf("%s\t%s,0x%X", op_name[i.IType.op],
			reg_name[i.IType.rt],
			i.IType.imm);
		break;

	case OP_ADDI:
	case OP_ADDIU:
		if (i.IType.rs == 0) {
 			kdbprintf("li\t%s,%D",
				reg_name[i.IType.rt],
				(short)i.IType.imm);
			break;
		}
		/* FALLTHROUGH */
	default:
		kdbprintf("%s\t%s,%s,%D", op_name[i.IType.op],
			reg_name[i.IType.rt],
			reg_name[i.IType.rs],
			(short)i.IType.imm);
	}
	kdbdotinc = 4;
}

#define MIPS_JR_RA	0x03e00008	/* instruction code for jr ra */

/*
 * Print a stack backtrace.
 */
void
kdbstacktrace(printlocals)
	int printlocals;
{
	unsigned pc, sp, ra, va, subr;
	int a0, a1, a2, a3;
	unsigned instr, mask;
	InstFmt i;
	int more, stksize;
	extern MachKernGenException();
	extern MachUserGenException();
	extern MachKernIntr();
	extern MachUserIntr();
	extern setsoftclock();

	/* get initial values from the exception frame */
	sp = kdbpcb.pcb_regs[SP];
	pc = kdbpcb.pcb_regs[PC];
	ra = kdbpcb.pcb_regs[RA];
	a0 = kdbpcb.pcb_regs[A0];
	a1 = kdbpcb.pcb_regs[A1];
	a2 = kdbpcb.pcb_regs[A2];
	a3 = kdbpcb.pcb_regs[A3];

loop:
	/* check for current PC in the kernel interrupt handler code */
	if (pc >= (unsigned)MachKernIntr && pc < (unsigned)MachUserIntr) {
		/* NOTE: the offsets depend on the code in locore.s */
		kdbprintf("interupt\n");
		a0 = kdbchkget(sp + 36, DSP);
		a1 = kdbchkget(sp + 40, DSP);
		a2 = kdbchkget(sp + 44, DSP);
		a3 = kdbchkget(sp + 48, DSP);
		pc = kdbchkget(sp + 20, DSP);
		ra = kdbchkget(sp + 92, DSP);
		sp = kdbchkget(sp + 100, DSP);
	}

	/* check for current PC in the exception handler code */
	if (pc >= 0x80000000 && pc < (unsigned)setsoftclock) {
		ra = 0;
		subr = 0;
		goto done;
	}
	/*
	 * Find the beginning of the current subroutine by scanning backwards
	 * from the current PC for the end of the previous subroutine.
	 */
	va = pc - sizeof(int);
	while ((instr = kdbchkget(va, ISP)) != MIPS_JR_RA)
		va -= sizeof(int);
	va += 2 * sizeof(int);	/* skip back over branch & delay slot */
	/* skip over nulls which might separate .o files */
	while ((instr = kdbchkget(va, ISP)) == 0)
		va += sizeof(int);
	subr = va;

	/* scan forwards to find stack size and any saved registers */
	stksize = 0;
	more = 3;
	mask = 0;
	for (; more; va += sizeof(int), more = (more == 3) ? 3 : more - 1) {
		/* stop if hit our current position */
		if (va >= pc)
			break;
		instr = kdbchkget(va, ISP);
		i.word = instr;
		switch (i.JType.op) {
		case OP_SPECIAL:
			switch (i.RType.func) {
			case OP_JR:
			case OP_JALR:
				more = 2; /* stop after next instruction */
				break;

			case OP_SYSCALL:
			case OP_BREAK:
				more = 1; /* stop now */
			};
			break;

		case OP_BCOND:
		case OP_J:
		case OP_JAL:
		case OP_BEQ:
		case OP_BNE:
		case OP_BLEZ:
		case OP_BGTZ:
			more = 2; /* stop after next instruction */
			break;

		case OP_COP0:
		case OP_COP1:
		case OP_COP2:
		case OP_COP3:
			switch (i.RType.rs) {
			case OP_BCx:
			case OP_BCy:
				more = 2; /* stop after next instruction */
			};
			break;

		case OP_SW:
			/* look for saved registers on the stack */
			if (i.IType.rs != 29)
				break;
			/* only restore the first one */
			if (mask & (1 << i.IType.rt))
				break;
			mask |= 1 << i.IType.rt;
			switch (i.IType.rt) {
			case 4: /* a0 */
				a0 = kdbchkget(sp + (short)i.IType.imm, DSP);
				break;

			case 5: /* a1 */
				a1 = kdbchkget(sp + (short)i.IType.imm, DSP);
				break;

			case 6: /* a2 */
				a2 = kdbchkget(sp + (short)i.IType.imm, DSP);
				break;

			case 7: /* a3 */
				a3 = kdbchkget(sp + (short)i.IType.imm, DSP);
				break;

			case 31: /* ra */
				ra = kdbchkget(sp + (short)i.IType.imm, DSP);
			}
			break;

		case OP_ADDI:
		case OP_ADDIU:
			/* look for stack pointer adjustment */
			if (i.IType.rs != 29 && i.IType.rt != 29)
				break;
			stksize = (short)i.IType.imm;
		}
	}

done:
#if 0
	kdbpsymoff((long)pc, ISYM, "");
#else
	kdbprintf("%X+%X ", subr, pc - subr); /* XXX */
#endif
	kdbprintf("(%X,%X,%X,%X)\n", a0, a1, a2, a3);

	if (ra) {
		pc = ra;
		sp -= stksize;
		goto loop;
	}
}

/*
 * Very simple memory allocator for kdb.
 */
char *
kdbmalloc(size)
	int size;
{
	static char buffer[4096];
	static char *bufp = buffer;
	char *p;

	/* round size up to sizeof(int) */
	size = (size + sizeof(int) - 1) & ~(sizeof(int) - 1);
	p = bufp;
	bufp = p + size;
	return (p);
}

/*
 * Machine dependent printing '$'.
 * Return zero if modif character not recognized.
 */
kdbprintmachdep(modif)
{
	register int i, j;
	extern int tlbhi, tlblo;

	switch (modif) {
	case 'p':
	case 'P': /* print TLB entries */
		if (kdbadrflg) {
			i = kdbadrval;
			if (i < 0 || i > VMMACH_NUM_TLB_ENTRIES) {
				extern char *kdbBADMOD;

				kdberror(kdbBADMOD);
				break;
			}
			if (kdbcntflg == 0)
				j = i + 1;
			else {
				j = i + kdbcntval;
				if (j > VMMACH_NUM_TLB_ENTRIES)
					j = VMMACH_NUM_TLB_ENTRIES;
			}
		} else {
			i = 0;
			j = VMMACH_NUM_TLB_ENTRIES;
		}
		for (; i < j; i++) {
			MachTLBRead(i);
			if (modif == 'p' && !(tlblo & PG_V))
				continue;
			kdbprintf("TLB %2d: hi %8X low %8X\n", i, tlbhi, tlblo);
		}
		kdbprintf("TLB PID %x\n", MachTLBGetPID());
		break;

	case 'f': /* find a TLB entry by virtaddr */
	case 'F': /* find a TLB entry by physaddr */
		j = kdbdot & PG_FRAME;
		for (i = 0; i < VMMACH_NUM_TLB_ENTRIES; i++) {
			MachTLBRead(i);
			if (modif == 'f') {
				if ((tlbhi & PG_FRAME) != j)
					continue;
			} else {
				if ((tlblo & PG_FRAME) != j)
					continue;
			}
			kdbprintf("TLB %2d: hi %8X low %8X\n", i, tlbhi, tlblo);
		}
		break;

	default:
		return (0);
	}
	return (1);
}
