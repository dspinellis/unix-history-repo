# @(#)makefile	1.1
U370 =
DATA=data
FIXDATA=text
TESTDIR = .
INS = /etc/install
INSDIR =
CFLAGS = -O 
LFLAGS = -n
ARK = lib.a
SHLIB = ../shlib
D=  -DKSHELL
D4_2 =
sh = ksh
# The following line must be set to make either the emacs or vi version of ksh
EDIT = edit.o
# The following two lines are used for one line emacs editor
EMACS = emacs.o
DESH = -DESH
# The following two lines are used for one line vi editor
VI = vi.o
DVSH = -DVSH
# The following line is used for vi mode always raw (u370 requires this)
# VIRAW = -DRAWONLY
VIRAW =
# The following two to four lines are used for BSD Unix
# DBSD = -DBSD 
# LFLAGS = -z
# D4_2 = -DBSD_4_2
# ECHO_N = -DECHO_N
# The following two (three for BSD) lines are used for Job Control
DJOBS = -DJOBS
JOBS = jobs.o
JOBLIB = -ljobs
# The following is used if the stdio routines are taken from stdio.c
# MYIO=stdio.o
# The following line is used for machines with 16 bit integers
# INT16=-DINT16
# The following line is used for machines which do not define _sobuf & _sibuf
# NOBUF=-DNOBUF
# The following line is used for machines which can't recover for SIGSEGV
# NOSEGV=-DNOSEGV
# The following line is used to enable execute only and suid/sgid shell scripts
SUID_EXEC=-DSUID_EXEC
C = $(CC) $D $(DVSH) $(DESH) $(DJOBS) $(DBSD) $(D4_2) $(VIRAW) $(NOBUF) $(INT16) $(VENIX)$(XENIX) $(ECHO_N) $(DVFORK) $(DEVFD) $(NOSEGV) $(SXT) $(MULTIBYTE) $(ACCT) $(SUID_EXEC)
CC_CMD = $(C) $(CFLAGS) -c
OFILES = builtin.o \
args.o \
blok.o \
cmd.o \
ctype.o \
defs.o \
echo.o \
error.o \
expand.o\
fault.o \
history.o\
io.o \
macro.o \
main.o \
msg.o \
name.o \
print.o \
service.o \
string.o \
stak.o \
test.o \
xec.o \
word.o \
$(EDIT) \
$(EMACS) \
$(VI) \
$(VFORK) \
$(JOBS) \
$(MYIO)


all: $(sh)

$(sh): $(OFILES) $(SHLIB)/$(ARK)
	$(C) $(LFLAGS) -o $(TESTDIR)/$(sh) $(OFILES) $(SHLIB)/$(ARK) \
		$(SHLIB)/$(ARK) $(JOBLIB)
		
	file $(TESTDIR)/$(sh)
	size $(TESTDIR)/$(sh)

ctype.o:	ctype.c
		-set -ex; if test  "$(FIXDATA)" != "" ;\
		then \
		$(C) -O -S -c ctype.c ;\
		sed 's/^\([ 	]*\.*\)$(DATA)/\1$(FIXDATA)/g' ctype.s > temp.s ;\
		$(AS) -o ctype.o temp.s ;\
		rm ctype.s temp.s ;\
		else \
		$(CC_CMD) ctype.c ;\
		fi

msg.o:		msg.c
		-set -ex; if test  "$(FIXDATA)" != "" ;\
		then \
		$(C) -O -S -c msg.c ;\
		sed 's/^\([ 	]*\.*\)$(DATA)/\1$(FIXDATA)/g' msg.s > temp.s ;\
		$(AS) -o msg.o temp.s ;\
		rm msg.s temp.s ;\
		else \
		$(CC_CMD) msg.c ;\
		fi

emacs.o:	emacs.c
	-set -ex; if test  "$(U370)" = "" ;\
	then \
	$(CC_CMD) emacs.c ;\
	else \
	$(CC_CMD)  -b2,0 emacs.c ;\
	fi

service.o:	service.c
	-set -ex; if test  "X$(ACCT)" = "X" ;\
	then \
	$(CC_CMD) service.c ;\
	else \
	$(CC_CMD)  -I/usr/src/cmd/acct service.c ;\
	fi

builtin.o:		builtin.c
	-set -ex; if test  "$(U370)" = "" ;\
	then \
	$(CC_CMD) builtin.c ;\
	else \
	$(CC_CMD) -b2,0 builtin.c ;\
	fi

xec.o:		xec.c
	-set -ex; if test  "$(U370)" = "" ;\
	then \
	$(CC_CMD) xec.c ;\
	else \
	$(CC_CMD) -b1,0 xec.c ;\
	fi

suid_exec:	suid_exec.o
	$(C) $(LFLAGS) -o $(TESTDIR)/suid_exec suid_exec.o
	strip suid_exec
	-ln suid_exec ..

suid_exec.o:	suid_exec.c
	$(CC_CMD) suid_exec.c

install:  all
	  $(INS) -o -f $(INSDIR) $(TESTDIR)/$(sh)
	  -rm -f $(INSDIR)/r$(sh)
	  -ln $(INSDIR)/$(sh) $(INSDIR)/r$(sh)

clean:
	  -rm -f *.o

clobber:  clean
	  -rm -f $(TESTDIR)/$(sh) option.init suid_exec

########
#
#	All dependencies and rules not explicitly stated
#	(including header and nested header dependencies)
#
########

args.o:	args.c
args.o:	brkincr.h
args.o:	builtins.h
args.o:	defs.h
args.o:	flags.h
args.o:	io.h
args.o:	mode.h
args.o:	sym.h
	$(CC_CMD) args.c

blok.o:	blok.c
blok.o:	brkincr.h
blok.o:	defs.h
blok.o:	stak.h
	$(CC_CMD) blok.c

builtin.o:	brkincr.h
builtin.o:	builtins.h
builtin.o:	defs.h
builtin.o:	flags.h
builtin.o:	history.h
builtin.o:	io.h
builtin.o:	jobs.h
builtin.o:	mode.h
builtin.o:	name.h
builtin.o:	shtype.h
builtin.o:	stak.h
builtin.o:	sym.h

cmd.o:	brkincr.h
cmd.o:	builtins.h
cmd.o:	cmd.c
cmd.o:	defs.h
cmd.o:	flags.h
cmd.o:	history.h
cmd.o:	io.h
cmd.o:	mode.h
cmd.o:	name.h
cmd.o:	shtype.h
cmd.o:	stak.h
cmd.o:	sym.h
	$(CC_CMD) cmd.c

ctype.o:	shtype.h

defs.o:	brkincr.h
defs.o:	defs.h
defs.o:	defs.c
defs.o:	edit.h
defs.o:	flags.h
defs.o:	history.h
defs.o:	jobs.h
defs.o:	io.h
defs.o:	name.h
defs.o:	sym.h
defs.o:	timeout.h
	$(CC_CMD) defs.c

edit.o:	brkincr.h
edit.o:	builtins.h
edit.o:	defs.h
edit.o:	edit.c
edit.o:	edit.h
edit.o:	flags.h
edit.o:	history.h
edit.o:	io.h
edit.o:	mode.h
edit.o:	name.h
edit.o:	national.h
edit.o:	shtype.h
edit.o:	sym.h
	$(CC_CMD) edit.c

emacs.o:	defs.h
emacs.o:	edit.h
emacs.o:	history.h
emacs.o:	io.h
emacs.o:	shtype.h

echo.o:		brkincr.h
echo.o:		defs.h
	$(CC_CMD) echo.c

error.o:	brkincr.h
error.o:	defs.h
error.o:	error.c
error.o:	flags.h
error.o:	io.h
error.o:	jobs.h
error.o:	sym.h
	$(CC_CMD) error.c

expand.o:	brkincr.h
expand.o:	defs.h
expand.o:	expand.c
expand.o:	shtype.h
expand.o:	stak.h
expand.o:	sym.h
	$(CC_CMD) expand.c

fault.o:	brkincr.h
fault.o:	defs.h
fault.o:	fault.c
fault.o:	flags.h
fault.o:	jobs.h
fault.o:	stak.h
fault.o:	sym.h
fault.o:	timeout.h
	$(CC_CMD) fault.c

history.o:	brkincr.h
history.o:	builtins.h
history.o:	defs.h
history.o:	flags.h
history.o:	history.c
history.o:	history.h
history.o:	io.h
history.o:	name.h
history.o:	shtype.h
history.o:	stak.h
	$(CC_CMD) history.c

io.o:	defs.h
io.o:	flags.h
io.o:	io.c
io.o:	io.h
io.o:	shtype.h
	$(CC_CMD) io.c

jobs.o:	brkincr.h
jobs.o:	defs.h
jobs.o:	flags.h
jobs.o:	history.h
jobs.o:	io.h
jobs.o:	jobs.c
jobs.o:	jobs.h
jobs.o:	shtype.h
	$(CC_CMD) jobs.c

macro.o:	brkincr.h
macro.o:	builtins.h
macro.o:	defs.h
macro.o:	flags.h
macro.o:	io.h
macro.o:	jobs.h
macro.o:	macro.c
macro.o:	mode.h
macro.o:	name.h
macro.o:	national.h
macro.o:	shtype.h
macro.o:	stak.h
macro.o:	sym.h
	$(CC_CMD) macro.c

main.o:	brkincr.h
main.o:	builtins.h
main.o:	defs.h
main.o:	flags.h
main.o:	history.h
main.o:	io.h
main.o:	main.c
main.o:	mode.h
main.o:	name.h
main.o:	stak.h
main.o:	sym.h
main.o:	timeout.h
	$(CC_CMD) main.c

msg.o:	brkincr.h
msg.o:	builtins.h
msg.o:	defs.h
msg.o:	flags.h
msg.o:	name.h
msg.o:	national.h
msg.o:	sym.h
msg.o:	test.h
msg.o:	timeout.h

name.o:	brkincr.h
name.o:	builtins.h
name.o:	defs.h
name.o:	flags.h
name.o:	io.h
name.o:	name.c
name.o:	name.h
name.o:	shtype.h
name.o:	stak.h
name.o:	sym.h
name.o:	timeout.h
	$(CC_CMD) name.c

print.o:	builtins.h
print.o:	defs.h
print.o:	flags.h
print.o:	io.h
print.o:	print.c
print.o:	shtype.h
	$(CC_CMD) print.c

service.o:	brkincr.h
service.o:	builtins.h
service.o:	defs.h
service.o:	flags.h
service.o:	io.h
service.o:	jobs.h
service.o:	mode.h
service.o:	name.h
service.o:	service.c
service.o:	shtype.h
service.o:	stak.h
service.o:	sym.h

string.o:	string.c
	$(CC_CMD) string.c

stak.o:	brkincr.h
stak.o:	defs.h
stak.o:	stak.c
stak.o:	stak.h
	$(CC_CMD) stak.c

stdio.o:	stdio.c
	$(CC_CMD) stdio.c

test.o:	defs.h
test.o:	shtype.h
test.o:	sym.h
test.o:	test.h
test.o:	test.c
	$(CC_CMD) test.c

vfork.o:	brkincr.h
vfork.o:	builtins.h
vfork.o:	defs.h
vfork.o:	flags.h
vfork.o:	io.h
vfork.o:	jobs.h
vfork.o:	mode.h
vfork.o:	name.h
vfork.o:	stak.h
vfork.o:	sym.h
vfork.o:	vfork.c
	$(CC_CMD) vfork.c

vi.o:	defs.h
vi.o:	edit.h
vi.o:	flags.h
vi.o:	io.h
vi.o:	history.h
vi.o:	shtype.h
vi.o:	vi.c
	$(CC_CMD) vi.c

word.o:	brkincr.h
word.o:	builtins.h
word.o:	defs.h
word.o:	flags.h
word.o:	history.h
word.o:	io.h
word.o:	name.h
word.o:	shtype.h
word.o:	stak.h
word.o:	sym.h
word.o:	word.c
	$(CC_CMD) word.c

xec.o:	brkincr.h
xec.o:	builtins.h
xec.o:	defs.h
xec.o:	flags.h
xec.o:	io.h
xec.o:	jobs.h
xec.o:	mode.h
xec.o:	name.h
xec.o:	shtype.h
xec.o:	stak.h
xec.o:	sym.h
