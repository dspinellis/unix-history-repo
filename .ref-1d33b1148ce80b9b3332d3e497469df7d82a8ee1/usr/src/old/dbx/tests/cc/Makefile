#	@(#)Makefile	5.3 (Berkeley) %G%

# Makefile for testing dbx.

.SUFFIXES:

.SUFFIXES: .c .h .s .o .x .t .in .tst .out

#
# Suffix conventions:
#
#	.c	C source module
#	.h	C definitions file
#	.s	assembly code file
#	.o	object file
#	.x	executable file
#	.t	dummy file represented last time tested
#	.in	input for testing
#	.tst	test output
#	.out	expected output
#

CC = cc

TESTDBX = ../../tdbx
TESTS = \
    aliases.t args.t arrays.t bitfields.t \
    dump.t enum.t float.t loophole.t nested.t own.t reg.t \
    same.t signal.t sleep.t stkcmds.t strings.t struct.t user.t ushort.t
#   call.t - deleted as it crashes the CCI microcode.

.c.x:
	${CC} -g $*.c -o $*.x

.c.o:
	${CC} -c -g $*.c

.in.tst:
	@rm -f $*.tst
	../runtest ${TESTDBX} $* > $*.tst

.x.t:
	@echo "    $*"
	@rm -f tmp
	@../runtest ${TESTDBX} $* > tmp
	@../check tmp out/out.`machine`/$*.out
	@rm -f tmp
	@rm -f $*.t
	@touch $*.t

passed: beforemsg ${TESTS}
	@echo "passed C tests"

beforemsg:
	@echo ""
	@echo "C tests:"

strings.x: strings.c
	cc -g -R strings.c -o strings.x

clean:
	rm -f *.x *.t *.o *.tst strings.core

aliases.t: aliases.x ${TESTDBX}
args.t: args.x ${TESTDBX}
arrays.t: arrays.x ${TESTDBX}
bitfields.t: bitfields.x ${TESTDBX}
call.t: call.x ${TESTDBX}
dump.t: dump.x ${TESTDBX}
enum.t: enum.x ${TESTDBX}
float.t: float.x ${TESTDBX}
loophole.t: loophole.x ${TESTDBX}
nested.t: nested.x ${TESTDBX}
own.t: own.x ${TESTDBX}
reg.t: reg.x ${TESTDBX}
same.t: same.x ${TESTDBX}
signal.t: signal.x ${TESTDBX}
sleep.t: sleep.x ${TESTDBX}
stkcmds.t: stkcmds.x ${TESTDBX}
struct.t: struct.x ${TESTDBX}
user.t: user.x ${TESTDBX}
ushort.t: ushort.x ${TESTDBX}

strings.t: strings.x strings.core ${TESTDBX}
	@echo "    $*"
	@rm -f tmp
	@${TESTDBX} $*.x $*.core < $*.in | tail +3 > tmp
	@../check tmp out/out.`machine`/$*.out
	@rm -f tmp
	@rm -f $*.t
	@touch $*.t

strings.core: strings.x
	-csh -f -c "limit coredumpsize 6m >& /dev/null; strings.x"
	mv core strings.core

aliases.x: arrays.x
	ln -s arrays.x aliases.x
