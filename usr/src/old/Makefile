#	@(#)Makefile	5.44 (Berkeley) 6/11/93

# These programs are the only ones in src/old that we believe are worth
# installing on the system.  (Most of the other programs in src/old don't
# even compile on 4.4BSD.)  They are installed into /usr/old in case their
# replacements prove to have compatibility or stability problems.

SUBDIR=	awk compact cpp crypt eqn lex make more ms od \
	pr refer roff sed sh sum symorder tar tbl test \
	tmac which yacc

.if   (${MACHINE} == "hp300")
SUBDIR+=adb as.hpux ld
.elif (${MACHINE} == "i386")
SUBDIR+=ld
.elif (${MACHINE} == "luna68k")
SUBDIR+=ld
.elif (${MACHINE} == "mips")
SUBDIR+=ld
.elif (${MACHINE} == "tahoe")
SUBDIR+=adb as.tahoe dbx dlmpcc ld pcc
.elif ${MACHINE} == "vax"
SUBDIR+=adb arff as.vax dbx flcopy ld pcc rxformat talk tp
.endif

.include <bsd.subdir.mk>
