/*	Aoperand.c	1.1	86/07/20	*/

#include	"../tahoealign/align.h"
#define	illegal(x) ((look_at->add_modes & x)==0)
#define	legal(x) !illegal(x)

struct oprnd *operand(infop, number)
register process_info *infop;
int	number;
/*
 *	Enter with pc pointing to an operand descriptor
 *	in the 'text'. Decode the addressing mode, get
 *	the effective address and some data from there.
 *	Leave pc on the next oerand specifier or opcode.
 *	Returns a pointer to a 'decoded operand' structure,
 *	actually one of the 4 pre-allocated .
 *
 *	This routine should be called in such a sequence
 *	that pc will not have to be backed up to get some
 *	operand. For example, operand(0) and then operand(1)
 *	and then operand(2) is OK. Even operand(0), operand(1),
 *	operand(1) is OK. The rule is that operand(N) should not
 *	be called before operand(N-1) was.
 *	
 ***********************************************************/
{
	register struct oprnd *next;
	register struct operand_des *look_at;
	register int header,reg_num,shift_count, displ;
	register int keep_last;

	next = &decoded[number]; 
	if (number <= last_operand) return(next);
	if (number == last_operand+1) last_operand = number;
	else
	{
		printf ("Wrong sequence of OPERAND calls (alignment code)\n");
		return (&decoded[number]);
	};
	look_at  = &Table[opCODE].operand[number];
	next->data2 = 0;		/* Prepare for quad fetch */
	next->length = look_at->length;
	if (look_at->add_modes == Brd)
	{ 
		next->mode = Add;
		switch(look_at->length)
		{ 
		case 1: 
			displ = get_byte(infop, pc);
			pc++; 
			break; 
		case 2: 
			displ = get_word(infop, pc); 
			pc +=2; 
			break;
		default: 
			printf ("Wrong branch displacement(alignment code)\n");
		}; 
		next->address = pc+displ; 
		return(next); 
	};
	
	/* Not branch displacement, real operand */
	header = get_byte(infop, pc) & 0xff; 
	pc++; 
	reg_num = header & 0xf;
	switch (header >> 4 & 0xf) { 
	case 0: 			/* Short literals */
	case 1: 
	case 2: 
	case 3:
		if (illegal(Lit)) exception(infop, ILL_ADDRMOD);
		next->mode = Imm; 
		next->data = header; 
		break;

	case 4: 			/* Indexed register */
		if (illegal(Add) || reg_num==PCOUNTER || reg_num==SPOINTER)
			exception (infop, ILL_ADDRMOD);
		keep_last = last_operand;
		last_operand = number - 1; /* To get real results */
		next = operand(infop, number);   /* Get base address (recursive) */
		last_operand = keep_last;
		if
		    (! (next->mode & Indx)) exception (infop, ILL_ADDRMOD);
		switch (look_at->length)
		{ 
		case 1: 
			shift_count = 0; 
			break; 
		case 2: 
			shift_count = 1; 
			break;
		case 4: 
			shift_count = 2; 
			break; 
		case 8: 
			shift_count = 3; 
			break;
		default: 
			printf("Wrong data length in table(alignment code)\n");
		}; 
		next->address += (Register(infop,reg_num) << shift_count);
		next->mode |= (look_at->add_modes & M); /* Set R/W bits */
		trytoread (infop,next,number);
		break;

	case 5: 			/* Direct register */
		if (illegal (Dir) || reg_num==PCOUNTER ||
		    reg_num==SPOINTER && legal(R)) exception (infop, ILL_ADDRMOD);
		next->mode = Dir; 
		next->data = Register(infop,reg_num); 
		next->mode |= (look_at->add_modes & M); /* Set R/W bits */
		next->reg_number = reg_num; 
		if (look_at->length == 8)
		{
			if (reg_num >= SPOINTER-1 || (reg_num & 1)==1 ) 
				exception (infop, ILL_ADDRMOD);
			else next->data2 = Register(infop,reg_num+1);
		};
		break;

	case 6: 			/* Indirect register */
		if (illegal(Add) || reg_num==PCOUNTER )
			exception (infop, ILL_ADDRMOD); 
		next->mode = Add; 
		next->mode |= (look_at->add_modes & M); /* Set R/W bits */
		if (reg_num != SPOINTER) next->mode |= Indx;  /* (sp) not indexable*/
		next->reg_number = reg_num;
		next->address = Register(infop,reg_num);
		trytoread (infop,next,number);
		break;

	case 7: 			/* Autodecrement SP */
		if (illegal(Add) || reg_num!=SPOINTER || look_at->length != 4 ||
		    legal(R)) exception (infop, ILL_ADDRMOD);
		next->mode = SPmode;  	/* Implies Add */
		next->mode |= W; 	/* Set R/W bits */
		next->reg_number = SPOINTER; 
		next->length = 4;	/* Regardless of big table */
		sp -= 4;
		next->address = sp; 
		break;

	case 8: 			/* Immediate or (sp)+ */
		switch (reg_num) { 
		case 8: 		/* Immediate byte */
			if (illegal(Imm)) exception (infop, ILL_ADDRMOD);
			next->mode = Imm; 
			next->data = get_byte(infop, pc); 
			pc++; 
			break;
		case 9: 		/* Immediate word */
			if (illegal(Imm)) exception (infop, ILL_ADDRMOD);
			next->mode = Imm; 
			next->data = get_word(infop, pc); 
			pc +=2; 
			break;
		case 0xf : 		/* Immediate longword */
			if (illegal(Imm)) exception (infop, ILL_ADDRMOD);
			next->mode = Imm; 
			next->data = get_longword(infop, pc); 
			pc +=4; 
			break;
		case 0xe: 		/* Autoincrement sp */
			if (illegal(Add) || legal(W) ||
				look_at->length != 4) exception (infop, ILL_ADDRMOD);
			next->mode = SPmode;	/* Implies Add */
			next->reg_number = SPOINTER;
			next->address = sp; 
		        next->data = get_longword(infop, sp);
			next->length = 4;	/* Regardless of big table */
			sp += 4; 
			break;
		default: 
			exception (infop, ILL_ADDRMOD);
		};
		if (look_at -> length == 8)	/* Quadword fetch,not (sp)+ */
		{
			next->data2 = next->data;
			if (next->data2 >= 0) next->data = 0;
			else next->data = -1;
		}
		break;

	case 9: 			/* Autoincrement deferred SP or PC */
		if (reg_num !=PCOUNTER && reg_num !=SPOINTER )
			exception (infop, ILL_ADDRMOD);
		if (reg_num == PCOUNTER && illegal(Abs) ||
		    reg_num == SPOINTER && illegal(Add))
			exception (infop, ILL_ADDRMOD);
		next->mode = Add | (look_at->add_modes & M) | Indx; 
		next->address = get_longword (infop, (reg_num == PCOUNTER)?pc : sp ); 
		Replace (infop,reg_num, Register(infop,reg_num)+4);
		trytoread (infop,next,number);
		break;

	case 0xa:			/* Register or PC + byte displacement */
		if (reg_num != PCOUNTER && illegal(Add) ||
		    reg_num == PCOUNTER && illegal(Pcrel) ) exception (infop, ILL_ADDRMOD);
		next->mode = Add | (look_at->add_modes & M); 
		if (reg_num != SPOINTER &&
			look_at->add_modes != PR) next->mode |= Indx;
		displ = get_byte(infop,pc); 
		pc++;
		next->address = Register(infop,reg_num)+displ;
		trytoread (infop,next,number);
		break;

	case 0xb:			/* Same, indirect */ 
		if (illegal(Add)) exception (infop, ILL_ADDRMOD);
		next->mode = Add | (look_at->add_modes & M) | Indx; 
		displ = get_byte(infop,pc); 
		pc++;
		next->address = get_longword(infop, Register(infop,reg_num)+displ);
		trytoread (infop,next,number);
		break;

	case 0xc:			/* Register or PC + word displacement */
		if (reg_num != PCOUNTER && illegal(Add) ||
		    reg_num == PCOUNTER && illegal(Pcrel) ) exception (infop, ILL_ADDRMOD);
		next->mode = Add | (look_at->add_modes & M); 
		if (reg_num != SPOINTER &&
			look_at->add_modes != PR) next->mode |= Indx;
		displ = get_word(infop,pc); 
		pc +=2;
		next->address = Register(infop,reg_num)+displ;
		trytoread (infop,next,number);
		break;

	case 0xd:			/* Same, indirect */ 
		if (illegal(Add)) exception (infop, ILL_ADDRMOD);
		next->mode =Add | (look_at->add_modes & M) | Indx ; 
		displ = get_word(infop,pc); 
		pc +=2;
		next->address = get_longword (infop,Register(infop,reg_num)+displ);
		trytoread (infop,next,number);
		break;


	case 0xe:		/* Register or PC + longword displacement */
		if (reg_num != PCOUNTER && illegal(Add) ||
		    reg_num == PCOUNTER && illegal(Pcrel) ) exception (infop, ILL_ADDRMOD);
		next->mode = Add | (look_at->add_modes & M); 
		if (reg_num != SPOINTER &&
			look_at->add_modes != PR) next->mode |= Indx;
		displ = get_longword(infop,pc); 
		pc += 4;
		next->address = Register(infop,reg_num)+displ;
		trytoread (infop,next,number);
		break;

	case 0xf:			/* Same, indirect */ 
		if (illegal(Add)) exception (infop, ILL_ADDRMOD);
		next->mode = Add | (look_at->add_modes & M) | Indx; 
		displ = get_longword(infop,pc); 
		pc +=4;
		next->address = get_longword(infop, Register(infop,reg_num)+displ);
		trytoread (infop,next,number);
	};
	return(next);
}


trytoread (infop,pointer,number)
process_info	*infop;
struct oprnd	*pointer;
int		number;
/*
/*	Receives the opcode operand number and a pointer
/*	to the 'decoded' operand structure.
/*	If it's defined as readable data in the big table,
/*	it returns the data, sign extended.
/*
/**********************************************************/

{
	register struct operand_des *look_at;
	

	look_at  = &Table[opCODE].operand[number];
	if (legal(R))
	switch (look_at->length)
	{
	case 1:
		pointer->data = get_byte (infop,pointer->address);
		break;
	case 2:
		pointer->data = get_word (infop,pointer->address);
		break;
	case 4: 
		pointer->data = get_longword (infop,pointer->address);
		break;
	case 8: 
		pointer->data = get_longword (infop,pointer->address);
		pointer->data2 = get_longword (infop,pointer->address+4);
		break;
	default:
		printf ("Wrong data length in table (alignment code)\n");
	};
}
