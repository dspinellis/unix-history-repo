#!/usr/bin/perl

require 'sys/socket.ph';

while (<>) {

    if (/^datagram from/) {

	split;
	if ($_[4] == 53)
	{
	    $nsqueriers{$_[2]}++;
	}
	else
	{
	    $resqueriers{$_[2]}++;
	}
    }

}

print "Name server queriers\n\n";

while (($ip, $count) = each(%nsqueriers)) {
    $addr = pack('C4', split(/\./, $ip));
    ($host, $rest) = gethostbyaddr($addr, &AF_INET);
    if ($host eq "")
    {
    	printf "unknown (%s): %d queries\n", $ip, $count;
    } else {
    	printf "%s (%s): %d queries\n", $host, $ip, $count;
    }
}

print "\nResolver queriers:\n\n";

while (($ip, $count) = each(%resqueriers)) {
    $addr = pack('C4', split(/\./, $ip));
    ($host, $rest) = gethostbyaddr($addr, &AF_INET);
    if ($host eq "")
    {
    	printf "unknown (%s): %d queries\n", $ip, $count;
    } else {
    	printf "%s (%s): %d queries\n", $host, $ip, $count;
    }
}
