#!/usr/bin/perl

require 'sys/socket.ph';

while(<>){
    if(/\b([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+)\b/){
      $addr = pack('C4', split(/\./, $1));
      ($name, $rest) = gethostbyaddr($addr, &AF_INET);
      if($name) {s/$1/$name/;}
    }
    print;
}
