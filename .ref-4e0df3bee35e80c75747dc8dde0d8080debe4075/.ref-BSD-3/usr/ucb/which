#
#	which : tells you which program you get
#	hacked to do aliases (from .cshrc file only!)
set noglob
foreach arg ( $argv )
    set alius = `alias $arg`
    switch ( $#alius )
	case 0 :
	    breaksw
	case 1 :
	    set arg = $alius[1]
	    breaksw
        default :
	    echo ${arg}: "	" aliased to $alius
	    continue
    endsw
    unset found
    if ( $arg:h != $arg:t ) then
	if ( -e $arg ) then
	    echo $arg
	else
	    echo $arg not found
	endif
	continue
    else
	foreach i ( $path )
	    if ( -x $i/$arg && ! -d $i/$arg ) then
		echo $i/$arg
		set found
		break
	    endif
	end
    endif
    if ( ! $?found ) then
	echo no $arg in $path
    endif
end
