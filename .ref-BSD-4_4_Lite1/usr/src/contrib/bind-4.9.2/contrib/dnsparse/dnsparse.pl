#!/usr/bin/perl
#
# $Id: dnsparse.pl,v 2.0 90/09/11 11:07:36 hakanson Rel $
#
# Subroutines to parse DNS master (RFC-1035) format files.
#   Marion Hakanson (hakanson@cse.ogi.edu)
#   Oregon Graduate Institute of Science and Technology
#
# Copyright (c) 1990, Marion Hakanson.
#
# You may distribute under the terms of the GNU General Public License
# as specified in the README file that comes with the dnsparse kit.
#
# Note that this file is not standalone.  It requires the dnslex C program,
# and it provides subroutines for a calling Perl program.
#
# One calls dns_init() with a list of input master file names, each
# optionally with an origin domain following it after a comma.  The
# typical calling program might pass those from its @ARGV, something
# like "dnstest zone.x,x.edu zone.y.x,y.x.edu".
#
# Then the calling program repeatedly calls dns_getrr() until it returns
# the null array, at which point all the input files are exhausted.  Some
# type checking is done, and some minor canonicalization is done (e.g. the
# RR types are capitalized and domain names lower-cased), but more of both
# should be added to catch errors.
#
# Apologies for the ugly code.  It was originally designed to take only
# a single input file per invocation, and should really be reworked to
# deal with multiple files more gracefully.

package dns;

$FALSE = 0;
$TRUE  = 1;

$prog = $main'0;
$prog =~ s?^.*/??;

# Defaults
$dnslex = 'dnslex';
$delim  = ':';

# Package globals
$initialized = $FALSE;
$fileopen    = $FALSE;
$alldone     = $FALSE;
$pid         = 0;


sub main'dns_init {
    if ( $#_ < $[ ) {
        @dns_argv = (',');
    } else {
        @dns_argv = @_;
    }
    $initialized = $TRUE;
}


sub main'dns_getrr {
    local (@data);
    local ($tmp,$data);
    local ($ttl,$class,$type);

 die "$prog: dns_init() not called, aborted" unless ($initialized);

 #print STDERR "inside dns_getrr()\n";
 while (1) {
  #print STDERR "inside outer-while\n";
  tryopen: until ( $fileopen || $alldone ) {
    #print STDERR "inside tryopen\n";
    if ( $#dns_argv < $[ ) {
      $alldone = $TRUE;
      next tryopen;
    }

    ($ifile,$origin1) = do main'dns_commasplit(shift(@dns_argv));

    if ( $ifile eq '' || $ifile eq '-' ) {
        $ifile = '';
        @dns_argv = ();	# STDIN must be last
    } else {
        unless ( -r $ifile ) {
            print STDERR "$prog: $ifile: $!, trying another\n";
	    next tryopen;
	}
	$ifile = "< $ifile";
    }

    $pid = open(DNS_IN, "$dnslex -d$delim $ifile |");
    unless ( defined($pid) ) {
        print STDERR "$prog: Can't start '$dnslex $ifile', trying another\n";
        next tryopen;
    }
    $origin = do main'dns_makefqdn($origin1, '');	# '' is root
    $domain = $origin;
    $fileopen = $TRUE;
  }

  #print STDERR "tryopen() done\n";
  return () unless ( $fileopen );
  #print STDERR "fileopen test passed\n";

  dline: while ( <DNS_IN> ) {
			#print STDERR $_;
    chop;
    @data = split(/$delim/o);		# split on $delim
			#print STDERR "$data[0] $data[1] $data[2]\n";
    s/$delim/ /go;			# for error msgs
    
    if ( $data[0] =~ /^\$/ ) {		# special "$" directives
	if ( $data[0] =~ /^\$ORIGIN$/i
		&& $data[1] ) {
	    $origin = do main'dns_makefqdn($data[1], $origin);
	} else {
	    print STDERR "$prog: unknown directive ignored: $_\n";
	}
	next dline;
    }

    # Set $domain for the current record.  After doing so,
    # $data[0] should contain the next field to parse.

    dom: {
	if ( $data[0] eq "." ) {	# root domain
	    $domain = "";
	    last dom;
	}
	if ( $data[0] eq "@" ) {	# use $origin
	    $domain = $origin;
	    last dom;
	}
	if ( $data[0] ne "" ) {
	    $domain = do main'dns_makefqdn($data[0], $origin);
	    last dom;
	}
	# otherwise use current domain
    }
    shift(@data);

    if ( $data[0] =~ /^[0-9]+/ ) {	# numeric ttl
	$ttl = shift(@data);
    } else {
	$ttl = 0;			# default
    }

    # This defaulting looks strange, but it's what named does
    if ( $data[0] =~ /IN/i ||
	 $data[0] =~ /CHAOS/i ) {
	$class = shift(@data);
	$class =~ tr/a-z/A-Z/;
    } else {
	$class = "IN";
    }

    $type = shift(@data);
    $type =~ tr/a-z/A-Z/;
    typ: {
	if ( $type eq "A" ||
	     $type eq "WKS" ||
	     $type eq "HINFO" ||
	     $type eq "UID" ||
	     $type eq "GID" ) {
	    last typ;			# no further processing
	}
	if ( $type eq "SOA" ||
	     $type eq "MINFO" ) {
	    $data[0] = do main'dns_makefqdn($data[0], $origin);
	    $data[1] = do main'dns_makefqdn($data[1], $origin);
	    last typ;
	}
	if ( $type eq "NS" ||
	     $type eq "CNAME" ||
	     $type eq "MB" ||
	     $type eq "MG" ||
	     $type eq "MR" ||
	     $type eq "PTR" ) {
	    $data[0] = do main'dns_makefqdn($data[0], $origin);
	    last typ;
	}
	if ( $type eq "MX" ) {
	    if ( $data[0] !~ /^[0-9]/ || $data[0] > 64535 ) {
		print STDERR "$prog: bad MX ignored: $_\n";
		next dline;
	    }
	    $data[1] = do main'dns_makefqdn($data[1], $origin);
	    last typ;
	}
	if ( $type eq "UINFO" ) {
	    # need to check for escaped dot here !!!
	    ($tmp) = split(/./,$domain,1);
	    $data[0] =~ s/&/$tmp/e;
	    last typ;
	}
	# otherwise
	print STDERR "$prog: unrecognized type '$type' ignored: $_\n";
	next dline;
    }
    return ($domain,$ttl,$class,$type,@data);
  }
  close(DNS_IN);
  $fileopen = $FALSE;
  # now we've hit eof & must open the next file
  # to satisfy the getrr() request.
 }  
}




sub main'dns_makefqdn {
    local ($name, $origin) = @_;
    
    return ("") if ( $name eq "." ||	# root domain
		     $name eq "" );	# should not happen
    # check for non-escaped trailing dot
    if ( $name =~ /(.*)(\\*)\.$/
		&& (length($2) % 2 == 0) ) {
	return ($1.$2);			# strip trailing dot
    }
    $origin =~ s/^\.//;			# strip leading dot
    return ($name) if ( $origin eq "" );
    return ($origin) if ( $name eq "@" );
    return ("$name.$origin");
}


# The file args may be of the form 'file,domain', where ',' is
# the first un-doubled comma (later commas are not processed).

sub main'dns_commasplit {
    local ($_) = @_;
    local ($first,$secnd);

    $first = '';
    $secnd = '';
    
    commasplit: while ( /,/ ) {
        $first .= $`;	# before the comma
        $_ = $';	# and after it

        if ( s/^,// ) {	# turn double into a single & continue
            $first .= ',';
        } else {	# make the split
            $secnd = $_;
            $_ = '';	# remainder goes above
            last commasplit;
        }
    }
    $first .= $_;	# in case no single comma was found
    ($first,$secnd);
}

