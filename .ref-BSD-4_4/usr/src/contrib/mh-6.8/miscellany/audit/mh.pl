

# =====
# Subroutine mh_profile
#	Parse the user's .mh_profile and get arguments and settings
#
sub mh_profile {
    local($PROFILE);

    ($PROFILE = $ENV{"MH"}) || ($PROFILE = $ENV{"HOME"} . "/.mh_profile");

    open PROFILE || "$0: can't read mh_profile $PROFILE: $!\n";

    while (<PROFILE>) {
	next if /^#/;
	next unless ($key, $value) = /([^:\s]+):\s*(.+)/;
	$key =~ tr/A-Z/a-z/;
	$MH{$key} = $value;
    } 
    close PROFILE;

    $MH{'path'} = $ENV{'HOME'} . '/' . $MH{'path'};
} 


# =====
# Subroutine rcvstore
#	Convenience routine for MH users. Pipes incoming
#	mail message to rcvstore. Expects one argument - the 
#	name of the folder to rcvstore into.
#
sub rcvstore {
    local($folder) = @_;

    &openpipe("/usr/local/bin/mh/lib/rcvstore +$folder -create");
}


# =====
# Subroutine rcvdist
#	Convenience routine for MH users. Pipes incoming
#	mail message to rcvdist. Expects one argument - the 
#	list of users to distribute the mail message to
#
sub rcvdist {
    local($recips) = @_;

    &openpipe("/usr/local/bin/mh/lib/rcvdist $recips");
}


# =====
# Subroutine rcvtty
#	Convenience routine for MH users. Pipes incoming
#	mail message to rcvtty. This is MH's version of biff.
#
sub rcvtty {

    &openpipe("/usr/local/bin/mh/lib/rcvtty");
}


# =====
# Subroutine ali
#       Expand an MH alias into a list of names usable by
#       rcvdist
#
sub ali {
    local($alias) = @_;
    local($recips); 
    local(@list) = ();

    $recips = `/usr/local/bin/mh/ali $alias`;
    chop $recips;
    return(@list) if ($alias eq $recips);

    @list = split(/,/, $recips);
    return(@list);
}


# =====
# Subroutine refile_from
#	Refile a message into a folder by organization and 
#	sender name. The top-level folder is an argument
#	the user can specify.
#
sub refile_from {
    local($toplevel) = @_;

    return if (length($from) <= 0);
    return if ($from eq $user);

    $toplevel = "log" if ($toplevel eq '');
    &rcvstore("$toplevel/$organization/$from");
}

# =====
# Subroutine make_mhpath
#	Make a directory path recursively. 
#
sub make_mhpath {
    local($dir) = @_;
    local($i);
    local($mode) = 0755;

    $mode = oct($MH{'folder-protect'}) if (defined $MH{'folder-protect'});

    $_ = $dir;
    s#^/.*#/# || s#^[^/].*#.#;
    $start = $_;
    foreach $i (split('/', $dir)) {
	$start = $start . '/' . $i;
	next if (-d $start);
	mkdir($start, $mode) || return(1);
    };

    return(0);
} 


# =====
# Subroutine mh_parse
#	Parse the command line options
#
sub mh_parse {
    local(@argdesc) =  @SW;
    local($wantarg);

    while (($#ARGV >= 0) && ($ARGV[0] !~ /^-.+/)) { # must be a message list
	push(@MSGS, shift @ARGV);
    };

    grep(s/(\W)/\\$1/g, @argdesc);

    @ARGV = (split(' ', $MH{$program}), @ARGV) if defined($MH{$program});

    return if ($#ARGV < 0);

    while ($ARGV[0] =~ /^-.+/) {

        $ARGV = shift @ARGV;

        unless (@matches = grep(/$ARGV/, @argdesc)) {
            print "$program: unknown option: $ARGV\n";
            exit 1;
            &usage;
        } 

        for (@matches) { s/\\(\W)/$1/g; } 

        if ($#matches > $[) {
            print "$program: ambiguous switch $ARGV matches:\n";
            for (@matches) { 
                print "\    ", $_, "\n"; 
            }
            exit 1;
        } 

        ($switch,$wantarg) = $matches[$[] =~ /^-(\S+)\s*(\S*)/;

        $SW{$switch} = $wantarg ? shift @ARGV : 1;
        if ($SW{$switch} =~ /^(['"]).*$/ && $SW{$switch} !~ /^(['"]).*\1$/) {
            do {
                $SW{$switch} .= ' ' . (shift @ARGV);
            } until $#ARGV < 0 || $SW{$switch} =~ /^(['"]).*\1$/;
            $SW{$switch} =~ s/^(['"])(.*)\1$/$2/;
        } 
    }
}


# =====
# Subroutine print_switches
#	print the valid command line switches
#
sub print_switches {
    local(@argdesc) = @SW;

    print "   switches are:\n";
    for (sort @SW) {
	print "   $_\n";
    };
    print "\n";
}


1;
