#
# Makefile for testing dbx.
#

.SUFFIXES:

.SUFFIXES: .p .i .s .o .x .t .in .tst .out

#
# Suffix conventions:
#
#	.p	Pascal source
#	.i	Pascal definitions file
#	.s	assembly code file
#	.o	object file
#	.x	executable file
#	.t	dummy file represented last time tested
#	.in	input for testing
#	.tst	test output
#	.out	expected output
#

PC = pc
PCFLAGS = -g -w
TESTDBX = ../../tdbx
TESTS = arrays.t assign.t bigsym.t bool.t call.t enum.t incltest.t longsym.t \
    nested.t reals.t records.t sets.t const.t

PROGS = arrays.x assign.x bigsym.x bool.x call.x enum.x incltest.x longsym.x \
    nested.x reals.x records.x sets.x const.x

.p.x:
	${PC} ${PCFLAGS} $*.p -o $*.x

.p.o:
	${PC} -c ${PCFLAGS} $*.p

.in.tst:
	@rm -f $*.tst
	../runtest ${TESTDBX} $* > $*.tst

.x.t:
	@echo "    $*"
	@rm -f tmp
	@../runtest ${TESTDBX} $* > tmp
	@../check tmp $*.out
	@rm -f tmp
	@rm -f $*.t
	@touch $*.t

passed: ${PROGS} beforemsg ${TESTS}
	@echo "passed Pascal tests"

beforemsg:
	@echo ""
	@echo "Pascal tests:"

INCLOBJ = includes.o included.o

incltest.x: ${INCLOBJ}
	${PC} -g ${INCLOBJ} -o incltest.x

arrays.t: arrays.x ${TESTDBX}
assign.t: assign.x ${TESTDBX}
bigsym.t: bigsym.x ${TESTDBX}
bool.t: bool.x ${TESTDBX}
call.t: call.x ${TESTDBX}
enum.t: enum.x ${TESTDBX}
incltest.t: incltest.x ${TESTDBX}
longsym.t: longsym.x ${TESTDBX}
nested.t: nested.x ${TESTDBX}
reals.t: reals.x ${TESTDBX}
records.t: records.x ${TESTDBX}
sets.t: sets.x ${TESTDBX}
const.t: const.x ${TESTDBX}
