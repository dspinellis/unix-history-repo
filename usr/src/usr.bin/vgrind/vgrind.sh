#!/bin/csh -f
#
# Copyright (c) 1980 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)vgrind.sh	5.5 (Berkeley) %G%
#
set voptions=
set options=
set files=
set f=''
set head=""
set vf=/usr/libexec/vfontedpr
set tm=/usr/share/lib/tmac
top:
if ($#argv > 0) then
    switch ($1:q)

    case -f:
	set f='filter'
	set options = "$options $1:q"
	shift
	goto top

    case -t:
	set voptions = "$voptions -t"
	shift
	goto top

    case -o*:
	set voptions="$voptions $1:q"
	shift
	goto top

    case -W:
	set voptions = "$voptions -W"
	shift
	goto top

    case -d:
	if ($#argv < 2) then
	    echo "vgrind: $1:q option must have argument"
	    goto done
	else
	    set options = ($options $1:q $2)
	    shift
	    shift
	    goto top
	endif
			
    case -h:
	if ($#argv < 2) then
	    echo "vgrind: $1:q option must have argument"
	    goto done
	else
	    set head="$2"
	    shift
	    shift
	    goto top
	endif
			
    case -*:
	set options = "$options $1:q"
	shift
	goto top

    default:
	set files = "$files $1:q"
	shift
	goto top
    endsw
endif
if (-r index) then
    echo > nindex
    foreach i ($files)
	#	make up a sed delete command for filenames
	#	being careful about slashes.
	echo "? $i ?d" | sed -e "s:/:\\/:g" -e "s:?:/:g" >> nindex
    end
    sed -f nindex index >xindex
    if ($f == 'filter') then
	if ("$head" != "") then
	    $vf $options -h "$head" $files | cat $tm/tmac.vgrind -
	else
	    $vf $options $files | cat $tm/tmac.vgrind -
	endif
    else
	if ("$head" != "") then
	    $vf $options -h "$head" $files | \
		sh -c "vtroff -rx1 $voptions -i -mvgrind 2>> xindex"
	else
	    $vf $options $files | \
		sh -c "vtroff -rx1 $voptions -i -mvgrind 2>> xindex"
	endif
    endif
    sort -df +0 -2 xindex >index
    rm nindex xindex
else
    if ($f == 'filter') then
	if ("$head" != "") then
	    $vf $options -h "$head" $files | cat $tm/tmac.vgrind -
	else
	    $vf $options $files | cat $tm/tmac.vgrind -
	endif
    else
	if ("$head" != "") then
	    $vf $options -h "$head" $files | vtroff -i $voptions -mvgrind
	else
	    $vf $options $files | vtroff -i $voptions -mvgrind
	endif
    endif
endif

done:
