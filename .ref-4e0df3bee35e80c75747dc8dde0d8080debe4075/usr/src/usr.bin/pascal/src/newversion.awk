# Copyright (c) 1983 Regents of the University of California
#
#	@(#)newversion.awk	2.1	(Berkeley)	83/02/06
#
/^VERSION = /	{
		    n = split( $3, version, ".")
		    print "VERSION = " version[1] "." version[2]+1
		    break
		}
		{
		    print $0
		}
