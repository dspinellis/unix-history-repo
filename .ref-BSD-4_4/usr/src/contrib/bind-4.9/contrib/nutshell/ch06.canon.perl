#!/usr/bin/perl

# Perl canonicalization filter
#
# Expects one hostname per line, in the first field (a la .rhosts,
# X0.hosts)

require 'sys/socket.ph';

while(<>){
    if(($hostname, $null) = split){
      ($domainname, $aliases, $addrtype, $length, @addrs) =
      	gethostbyname($hostname);
      if($domainname) {s/$hostname/$domainname/;}
    }
    print;
}
