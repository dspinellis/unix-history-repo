#! /usr/bin/perl

# Originally written by Paul Vixie of DEC WRL on June 28 1991

# $Header: mkservers,v 1.3 91/09/06 18:07:53 root Exp $

if ($#ARGV != 0) {
	print stderr "usage:  $0 serverfile\n";
	exit(1);
}

$basename = $ARGV[0];

%arch = (	'vax', 1,
		'mips', 1
	);

open(servers, "<$basename") || die "$basename: $!";
open(serversdb, ">$basename.db.NEW") || die "$basename.db.NEW: $!";
foreach (keys(%arch)) {
	$f = "$basename.$_.NEW";
	open($f, ">$f") || die "$f: $!";
}

while (<servers>) {
	chop;
	next if /^#/ || /^$/;
	($host, $rdist) = split;
	if ($host !~ /\./) {
		print  stderr "unqualified servername in `$_'\n";
		next;
	}
	print serversdb "@ IN NS $host.\n";
	next if ($rdist eq '-');
	if (!$arch{$rdist}) {
		print stderr "second token isn't a valid architecture in `$_'\n";
		next;
	}
	$f = "$basename.$rdist.NEW";
	$oldfh = select($f);
	print "$host\n";
	select($oldfh);
}

close(servers);

close(serversdb) || die "close $basenaame.db.NEW: $!";
rename("$basename.db", "$basename.db.BAK");
rename("$basename.db.NEW", "$basename.db");

foreach (keys(%arch)) {
	$f = "$basename.$_";
	$new = "$f.NEW";
	close($new) || die "$new: $!";
	rename($f, "$f.BAK");
	rename("$f.NEW", $f);
}

exit 0;
