# print mail file
cd /usr/spool/mail
set multi=$#argv
if ($#argv == 0) then
	set me=`whoami`
	if (! -r $me) then
		echo No mail.
		exit(1)
	endif
	/usr/ucb/more $me
	echo 'Remove this mail [yn] ? \c'
	if (`gets n` == y) then
		rm -f $me
	endif
else
	while ($#argv > 0)
		if (! -e $argv[1] || -z $argv[1]) then
			echo No mail for $argv[1]
		else
			echo '>>>' $argv[1] '<<<'
			/usr/ucb/more $argv[1]
# Removed per RJ Fateman
#			if { /usr/ucb/more $argv[1] } then
#				echo 'Remove this mail [yn] ? \c'
#				if (`gets n` == y) then
#					rm -f $argv[1]
#				endif
#			endif
		endif
		shift argv
		if ($#argv > 0) then
			echo '-----\n\n'
		endif
	end
endif
