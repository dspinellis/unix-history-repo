# newgetopt.pl -- new options parsing

# SCCS Status     : @(#)@ newgetopt.pl	1.8
# Author          : Johan Vromans
# Created On      : Tue Sep 11 15:00:12 1990
# Last Modified By: Johan Vromans
# Last Modified On: Thu Sep 26 20:10:41 1991
# Update Count    : 35
# Status          : Okay

# This package implements a new getopt function. This function adheres
# to the new syntax (long option names, no bundling).
#
# Arguments to the function are:
#
#  - a list of possible options. These should designate valid perl
#    identifiers, optionally followed by an argument specifier ("="
#    for mandatory arguments or ":" for optional arguments) and an
#    argument type specifier: "n" or "i" for integer numbers, "f" for
#    real (fix) numbers or "s" for strings.
#
#  - if the first option of the list consists of non-alphanumeric
#    characters only, it is interpreted as a generic option starter.
#    Everything starting with one of the characters from the starter
#    will be considered an option.
#    Likewise, a double occurrence (e.g. "--") signals end of
#    the options list.
#    The default value for the starter is "-".
#
# Upon return, the option variables, prefixed with "opt_", are defined
# and set to the respective option arguments, if any.
# Options that do not take an argument are set to 1. Note that an
# option with an optional argument will be defined, but set to '' if
# no actual argument has been supplied.
# A return status of 0 (false) indicates that the function detected
# one or more errors.
#
# Special care is taken to give a correct treatment to optional arguments.
#
# E.g. if option "one:i" (i.e. takes an optional integer argument),
# then the following situations are handled:
#
#    -one -two		-> $opt_one = '', -two is next option
#    -one -2		-> $opt_one = -2
#
# Also, assume "foo=s" and "bar:s" :
#
#    -bar -xxx		-> $opt_bar = '', '-xxx' is next option
#    -foo -bar		-> $opt_foo = '-bar'
#    -foo --		-> $opt_foo = '--'
#

# HISTORY 
# 20-Sep-1990		Johan Vromans	
#    Set options w/o argument to 1.
#    Correct the dreadful semicolon/require bug.


package newgetopt;

$debug = 0;			# for debugging

sub main'NGetOpt {
    local (@optionlist) = @_;
    local ($[) = 0;
    local ($genprefix) = "-";
    local ($error) = 0;
    local ($opt, $optx, $arg, $type, $mand, @hits);

    # See if the first element of the optionlist contains option
    # starter characters.
    $genprefix = shift (@optionlist) if $optionlist[0] =~ /^\W+$/;

    # Turn into regexp.
    $genprefix =~ s/(\W)/\\\1/g;
    $genprefix = "[" . $genprefix . "]";

    # Verify correctness of optionlist.
    @hits = grep ($_ !~ /^\w+([=:][infse])?$/, @optionlist);
    if ( $#hits >= 0 ) {
	foreach $opt ( @hits ) {
	    print STDERR ("Error in option spec: \"", $opt, "\"\n");
	    $error++;
	}
	return 0;
    }

    # Process argument list

    while ( $#main'ARGV >= 0 ) {		#'){

	# >>> See also the continue block <<<

	# Get next argument
	$opt = shift (@main'ARGV);		#');
	print STDERR ("=> option \"", $opt, "\"\n") if $debug;
	$arg = undef;

	# Check for exhausted list.
	if ( $opt =~ /^$genprefix/o ) {
	    # Double occurrence is terminator
	    return ($error == 0) if $opt eq "$+$+";
	    $opt = $';		# option name (w/o prefix)
	}
	else {
	    # Apparently not an option - push back and exit.
	    unshift (@main'ARGV, $opt);		#');
	    return ($error == 0);
	}

	# Grep in option list. Hide regexp chars from option.
	($optx = $opt) =~ s/(\W)/\\\1/g;
	@hits = grep (/^$optx([=:].+)?$/, @optionlist);
	if ( $#hits != 0 ) {
	    print STDERR ("Unknown option: ", $opt, "\n");
	    $error++;
	    next;
	}

	# Determine argument status.
	undef $type;
	$type = $+ if $hits[0] =~ /[=:].+$/;
	print STDERR ("=> found \"$hits[0]\" for ", $opt, "\n") if $debug;

	# If it is an option w/o argument, we're almost finished with it.
	if ( ! defined $type ) {
	    $arg = 1;		# supply explicit value
	    next;
	}

	# Get mandatory status and type info.
	($mand, $type) = $type =~ /^(.)(.)$/;

	# Check if the argument list is exhausted.
	if ( $#main'ARGV < 0 ) {		#'){

	    # Complain if this option needs an argument.
	    if ( $mand eq "=" ) {
		print STDERR ("Option ", $opt, " requires an argument\n");
		$error++;
	    }
	    if ( $mand eq ":" ) {
		$arg = $type eq "s" ? "" : 0;
	    }
	    next;
	}

	# Get (possibly optional) argument.
	$arg = shift (@main'ARGV);		#');

	# Check if it is a valid argument. A mandatory string takes
 	# anything. 
	if ( "$mand$type" ne "=s" && $arg =~ /^$genprefix/o ) {

	    # Check for option list terminator.
	    if ( $arg eq "$+$+" ) {
		# Complain if an argument is required.
		if ($mand eq "=") {
		    print STDERR ("Option ", $opt, " requires an argument\n");
		    $error++;
		}
		# Push back so the outer loop will terminate.
		unshift (@main'ARGV, $arg);	#');
		$arg = "";	# don't assign it
		next;
	    }

	    # Maybe the optional argument is the next option?
	    if ( $mand eq ":" && $' =~ /[a-zA-Z_]/ ) {
		# Yep. Push back.
		unshift (@main'ARGV, $arg);	#');
		$arg = "";	# don't assign it
		next;
	    }
	}

	if ( $type eq "n" || $type eq "i" ) { # numeric/integer
	    if ( $arg !~ /^-?[0-9]+$/ ) {
		print STDERR ("Value \"", $arg, "\" invalid for option ",
			       $opt, " (numeric required)\n");
		$error++;
	    }
	    next;
	}

	if ( $type eq "f" ) { # fixed real number, int is also ok
	    if ( $arg !~ /^-?[0-9.]+$/ ) {
		print STDERR ("Value \"", $arg, "\" invalid for option ",
			       $opt, " (real number required)\n");
		$error++;
	    }
	    next;
	}

	if ( $type eq "s" ) { # string
	    next;
	}

    }
    continue {
	print STDERR ("=> \$main'opt_$opt = $arg\n") if $debug;
	eval ("\$main'opt_$opt = \$arg");
    }

    return ($error == 0);
}
1;
