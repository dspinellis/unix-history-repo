#!/usr/bin/perl
#
# Turn name server debugging on or off.  
#
# To turn off debugging, use "binddebug 0"
# To turn on debugging to level 3, use "binddebug 3"

# Find the process ID in /etc/named.pid.
open(PID, "/etc/named.pid") || die "Can\'t open /etc/named.pid\n";
$pid = <PID>;
chop($pid);
$pid || die "No process ID in /etc/named.pid\n";

# Get the debugging level from the command line.
$savelevel = $level = $ARGV[0];
$level =~ /^[0-9]+$/ || die "Integer argument required\n";

# Turn off debugging if it is on.
if(!kill 'USR2', $pid){ die "Kill of process ID $pid failed.\n"; }

if($level == 0){
  print "Debugging turned off.\n";
} else {
  while($level-- > 0) { 
    select(undef, undef, undef, 0.25);  # spread out signals
    kill 'USR1', $pid; 
  }
  printf "Name server now at debugging level $savelevel.\n";
}
