/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kdbparam.h	7.2 (Berkeley) %G%
 */

/*
 * Machine dependent definitions for kdb.
 */

#if BYTE_ORDER == LITTLE_ENDIAN
#define kdbshorten(w)	((w) & 0xFFFF)
#define	kdbbyte(w)	((w) & 0xFF)
#define	kdbitol(a,b)	((long)(((b) << 16) | ((a) & 0xFFFF)))
#define	kdbbtol(a)	((long)(a))
#endif

#define LPRMODE		"%R"
#define OFFMODE		"+%R"

#define	SETBP(ins)	MACH_BREAK_BRKPT

/* return the program counter value modified if we are in a delay slot */
#define	kdbgetpc(pcb)		(kdbvar[kdbvarchk('t')] < 0 ? \
	(pcb).pcb_regs[34] + 4 : (pcb).pcb_regs[34])
#define	kdbishiddenreg(p)	((p) >= &kdbreglist[33])
#define	kdbisbreak(type)	(((type) & MACH_CR_EXC_CODE) == 0x24)

/* check for address wrap around */
#define	kdbaddrwrap(addr,newaddr)	(((addr)^(newaddr)) >> 31)

/* declare machine dependent routines defined in kadb.c */
void	kdbprinttrap __P((unsigned, unsigned));
void	kdbsetsstep __P((void));
void	kdbclrsstep __P((void));
void	kdbreadc __P((char *));
void	kdbwrite __P((char *, int));
void	kdbprintins __P((int, long));
void	kdbstacktrace __P((int));
char	*kdbmalloc __P((int));
