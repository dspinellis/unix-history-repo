#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)newversion.awk	5.1 (Berkeley) %G%
#
/^VERSION = /	{
		    n = split( $3, version, ".")
		    print "VERSION = " version[1] "." version[2]+1
		    break
		}
		{
		    print $0
		}
