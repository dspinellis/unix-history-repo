#! /bin/csh -f
#
# $Header$
#
# ptest - test a project module
#
# Author: Peter J. Nicklin
#
set template = t.Makefile

top:

if ($#argv > 0) then
	switch ($argv[1])
		case -d:
			set debug
			shift
			breaksw
		case -F*:
			if ($argv[1] != -F) then
				set argv[1] = `echo $argv[1] | sed -e s/-F//`
			else if ($#argv < 2) then
				set error
				breaksw
			else
				shift
			endif
			set template = $argv[1]
			shift
			breaksw
		case -P*:
			if ($argv[1] != -P) then
				set argv[1] = `echo $argv[1] | sed -e s/-P//`
			else if ($#argv < 2) then
				set error
				breaksw
			else
				shift
			endif
			pushd . >& /dev/null
			eval `chproject -f $argv[1]`
			if ($status != 0) exit(1)
			popd >& /dev/null
			shift
			breaksw
		case -*:
			echo "ptest: bad option $argv[1]"
			set error
			shift
			breaksw
		default:
			goto next
			breaksw
		endsw
		goto top
endif

next:

if ($?error) then
	echo "ptest: usage: ptest [-d] [-F template] [-P projectname] [module ...]"
	exit(1)
endif

if (!($?PROJECT)) then
	echo "ptest: no project environment"
	exit(1)
endif

# does the test case directory exist?
if (!(-e $PROJECT/test)) then
	echo "ptest: $PROJECT/test: No such file or directory"
	exit(1)
endif

# test all modules
if ($#argv < 1) then
	pushd $PROJECT/test >& /dev/null
	foreach test (`ls`)
		if ($test =~ *.a) set argv = ($argv $test:r)
	end
	if ($#argv < 1) then
		echo "ptest: no test cases available"
		exit(1)
	endif
	popd >& /dev/null
endif

# remove compilable test programs
foreach file (`ls`)
	switch ($file)
		case T*.sh:
			breaksw
		case T*.*:
			rm -f $file
			breaksw
	endsw
end

foreach test ($argv[*])
	# extract test files from archive
	if (!(-e $PROJECT/test/$test.a)) then
		echo "ptest: test case $test not found"
		continue
	endif
	echo -n "$test:q: extracting archive ... "
	ar xo $PROJECT/test/$test.a

	# Compile test program
	unset userscript
	foreach program (T*.*)
		switch ($program)
			case T*.sh:
				set userscript
				breaksw
			default:
				echo -n "compiling test ... "
				mkmf -cd -fT_makefile -F$template PROGRAM=T$test
				make -f T_makefile -s >& E$test
				if ($status != 0) goto badtest
				breaksw
		endsw
	end

	# Run test
	echo -n "executing test ... "
	if ($?userscript) then
		# User supplied test script
		T$test.sh >>& E$test
		if ($status != 0) goto badtest
	else if (-e I$test) then
		# Default test script with input
		T$test < I$test |& diff - O$test >>& E$test
		if ($status != 0) goto badtest
	else
		# Default test script without input
		T$test |& diff - O$test >>& E$test
		if ($status != 0) goto badtest
	endif

	if (!($?debug)) rm -f E$test T$test* I$test O$test T_makefile
	echo done
	continue

badtest:
	if (!($?debug)) rm -f T$test* I$test O$test T_makefile
	echo failed
	set error
end

if ($?error) exit(1)
exit(0)
