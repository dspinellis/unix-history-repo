S =		../src
LENGTH =	7
FILES =		$S/*.[ch]
NAMES =		names $(LENGTH)
DUPS =		dups $(LENGTH)
INC =		/usr/include

remap.h :	duplist2 defines
		cat header.h >$@
		defines <duplist2 >>$@

duplist2 :	duplist exclude
		comm -13 exclude duplist >$@

duplist :	dups names
		cat $(FILES) | $(NAMES) | sort | uniq | $(DUPS) | sort | uniq >$@

# There are two files, reserved and special, which contain the names of
# strings which must not be remapped.  In general, reserved contains strings
# which are meaningful to the C preprocessor or compiler, and "special"
# contains special emacs strings that must not be remapped (they are the
# basename of an include file for example; actually this is a holdover from
# when sed or m4 was used to do the remapping, not needed with cpp remapping).
#
# Note, because "cut" is braindamaged and does not return explicit status,
# and since the status returned to make is that of the last program in the
# pipeline, we filter the final result through cat to avoid killing make.

exclude :	reserved special
		cat reserved special >tempfile
		grep "^#[ \t]*define" $(INC)/*.h | tr -s "\040\011#()" ":::::" | cut -f3 -d: | cat >>tempfile
		grep "^#[ \t]*define" $(INC)/sys/*.h | tr -s "\040\011#()" ":::::" | cut -f3 -d: | cat >>tempfile
		grep "^#[ \t]*define" $S/*.[ch] | tr -s "\040\011#()" ":::::" | cut -f3 -d: | cat >>tempfile
		sort <tempfile | uniq >$@
		rm -f tempfile

dups :		dups.c
		$(CC) -O -o $@ $?

names :		names.c
		$(CC) -O -o $@ $?

defines :	defines.c
		$(CC) -O -o $@ $?
