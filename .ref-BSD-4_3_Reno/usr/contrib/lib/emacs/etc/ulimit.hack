#!/bin/sh
#
# ulimit.hack: Create an intermediate program for use in
# between kernel initialization and init startup.
# This is needed on a 3b system if the standard CDLIMIT is
# so small that the dumped Emacs file cannot be written.
# This program causes everyone to get a bigger CDLIMIT value
# so that the dumped Emacs can be written out.
#
# Users of V.3.1 and later should not use this; see etc/MACHINES
# and reconfig your kernel's CDLIMIT parameter instead.
#
# Caveat: Heaven help you if you screw this up.  This puts
# a new program in as /etc/init, which then execs the real init.
#
cat > ulimit.init.c << \EOF
main(argc, argv)
int argc;
char *argv[];
{
	ulimit(2, 262144L);	/* "2" is the "set" command. */
	/* 262,144 allows for 128Mb files to be written. */
	/* If that value isn't suitable, roll your own.  */
	execv("/etc/real.init", argv);
}
EOF
#
# Compile it and put it in place of the usual init program.
#
cc ulimit.init.c -o ulimit.init
mv /etc/init /etc/real.init
mv ulimit.init /etc/ulimit.init
ln /etc/ulimit.init /etc/init
mv ulimit.init.c /etc/ulimit.init.c	# to keep src for this hack nearby.
chmod 0754 /etc/init
exit 0
#
# Upon system reboot, all processes will inherit the new large ulimit.
