#
# Makefile for testing dbx.
#

.SUFFIXES:

.SUFFIXES: .mod .def .pcd .s .o .x .t .in .tst .out

MOD = mod
TESTDBX = ../../tdbx
TESTS = \
    arrayrec.t arrays.t assign.t bool.t bitfields.t \
    call.t const.t dynarrays.t \
    imports.t jsb.t multirefs.t nested.t opaque.t \
    procvars.t proftrace.t ptrarray.t \
    reals.t records.t recur.t \
    sets.t stkcmds.t trace.t

PROGS = \
    arrayrec.x arrays.x assign.x bool.x bitfields.x \
    call.x const.x dynarrays.x \
    imports.x jsb.x multirefs.x nested.x opaque.x \
    procvars.x proftrace.x ptrarray.x \
    reals.x records.x recur.x \
    sets.x stkcmds.x trace.x

#
# Suffix conventions:
#
#	.mod	Modula-2 source
#	.def	Modula-2 definitions file
#	.pcd	P-code intermediate source
#	.s	assembly code file
#	.o	object file
#	.x	executable file
#	.t	dummy file represented last time tested
#	.in	input for testing
#	.tst	test output
#	.out	expected output
#

.mod.x:
	${MOD} -g $*.mod -o tmp
	mv tmp $*.x

.mod.o:
	@echo "compiling $*.mod"
	@${MOD} -c -g $*.mod

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
	@echo "passed Modula-2 tests"

beforemsg:
	@echo ""
	@echo "Modula-2 tests:"

IMPORTSOBJ = imports.o imported.o imptypes.o

imports.x: ${IMPORTSOBJ}
	@echo "linking imports.x"
	@${MOD} -g ${IMPORTSOBJ} -o tmp
	@mv tmp imports.x

jsb.x: jsb.mod
	${MOD} -O -g jsb.mod -o tmp
	mv tmp jsb.x

opaque.x: opaque.o imptypes.o
	@echo "linking opaque.x"
	@${MOD} -g opaque.o imptypes.o -o tmp
	@mv tmp opaque.x

proftrace.x: proftrace.mod
	${MOD} -pg -g proftrace.mod -o tmp
	mv tmp proftrace.x

MULTIOBJ = multirefs.o imp1.o imp2.o exp.o

multirefs.x: ${MULTIOBJ}
	@echo "linking multirefs.x"
	@${MOD} -g ${MULTIOBJ} -o tmp
	@mv tmp multirefs.x

arrayrec.t: arrayrec.x ${TESTDBX}
arrays.t: arrays.x ${TESTDBX}
assign.t: assign.x ${TESTDBX}
bool.t: bool.x ${TESTDBX}
bitfields.t: bitfields.x ${TESTDBX}
const.t: call.x ${TESTDBX}
call.t: call.x ${TESTDBX}
const.t: const.x ${TESTDBX}
dynarrays.t: dynarrays.x ${TESTDBX}
jsb.t: jsb.x ${TESTDBX}
imports.t: imports.x ${TESTDBX}
multirefs.t: multirefs.x ${TESTDBX}
nested.t: nested.x ${TESTDBX}
opaque.t: opaque.x ${TESTDBX}
procvars.t: procvars.x ${TESTDBX}
proftrace.t: proftrace.x ${TESTDBX}
ptrarray.t: ptrarray.x ${TESTDBX}
reals.t: reals.x ${TESTDBX}
records.t: records.x ${TESTDBX}
recur.t: recur.x ${TESTDBX}
sets.t: sets.x ${TESTDBX}
stkcmds.t: stkcmds.x ${TESTDBX}
trace.t: trace.x ${TESTDBX}
