#!/usr/bin/perl
#
# $Source: /f/osi/others/ntp/RCS/extract.pl,v $ $Revision: 7.1 $ $Date: 91/02/22 09:33:38 $
#
$HOST = '10.2.0.96';
if ($#ARGV != 0) {
	die "Must specify internet address of host.";
}
$HOST = $ARGV[1];
while(<stdin>) {
	if(/^host: $HOST/) {
		s/host: //;
		s/\(/ /g;
		s/\)/ /g;
		s/:/ /g;
		@A = split(' ');
		print $A[3],"\n";
	 }
}
