# Copyright (c) 1983 Regents of the University of California
#
#	@(#)newversion.awk	2.2	(Berkeley)	84/02/08
#
/^VERSION = /	{
		    n = split( $3, version, ".")
		    print "VERSION = " version[1] "." version[2]+1
		    break
		}
		{
		    print $0
		}
