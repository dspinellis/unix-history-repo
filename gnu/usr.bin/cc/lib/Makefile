LIB = gcc2

CFLAGS += -I${.CURDIR} -DNOFPU

SRCS =  toplev.c version.c tree.c print-tree.c \
	stor-layout.c fold-const.c function.c stmt.c expr.c \
	calls.c expmed.c explow.c optabs.c varasm.c \
	rtl.c print-rtl.c rtlanal.c emit-rtl.c real.c \
	dbxout.c sdbout.c dwarfout.c xcoffout.c\
	integrate.c jump.c cse.c loop.c unroll.c flow.c \
	stupid.c combine.c regclass.c local-alloc.c \
	global.c reload.c reload1.c caller-save.c \
	insn-peep.c reorg.c sched.c final.c recog.c reg-stack.c \
	insn-opinit.c insn-recog.c insn-extract.c insn-output.c insn-emit.c \
	insn-attrtab.c aux-output.c getpwd.c convert.c \
	c-common.c obstack.c 

install:
	@echo -n

.include <bsd.lib.mk>
