#
#
# $Revision: 1.13 $
# $Date: 92/05/12 14:34:18 $
#
#

# =====
# Subroutine initialize
#	Set up the environment for the user and parse the incoming
#	mail message. 
#
sub initialize {
    local($passwd, $uid, $gid, $quota, $comment, $gcos);

    ($user, $passwd, $uid, $gid, $quota, $comment, $gcos, $home, $shell) = 
	getpwnam($ARGV[0]); shift @ARGV;

    $ENV{'USER'} = $user;
    $ENV{'HOME'} = $home;
    $ENV{'SHELL'} = $shell;
    $ENV{'TERM'} = "vt100";

    &parse_message(STDIN);
}


# =====
# Subroutine parse_message
#	Parse a message into headers, body and special variables
#
sub parse_message {
    local(*INFILE) = @_;

    $/ = '';		# read input in paragraph mode
    %headers = ( );
    @received = ( );
    undef($body);

    $header = <INFILE>;

    $* = 1;
    while (<INFILE>) { 
	s/^From />From /g;
	$body = "" if !defined($body);
	$body .= $_; 
    };
    $/ = "\n";		
    $* = 0;


    ;# -----
    ;# $sender comes from the UNIX-style From line (From strike...)
    ;#
    ($sender) = ($header =~ /^From\s+(\S+)/); 


    ;# -----
    ;# fill out the headers associative array with fields from the mail
    ;# header.
    ;#
    $_ = $header;
    s/\n\s+//g;
    @lines = split('\n');
    for ( @lines ) {
	/^([\w-]*):\s*(.*)/ && do {
	    $mheader = $1;
	    $mheader =~ tr/A-Z/a-z/;
	    if (($mheader eq "cc" || $mheader eq "to") && $headers{$mheader}) {
		$headers{$mheader} .= ", $2";
	    } elsif ($mheader eq "received") {
		push(@received, $2);
	    } else {
		$headers{$mheader} = $2;
	    };
	};
    }
    @received = reverse(@received);


    ;# -----
    ;# for convenience, $subject is $headers{'subject'} and $precedence is
    ;# $headers{'precedence'}
    ;#
    $subject = $headers{'subject'};
    $subject = "(No subject)" unless $subject;
    $subject =~ s/\s+$//;
    $precedence = $headers{'precedence'};


    ;# -----
    ;# create arrays for who was on the To, Cc lines
    ;#
    @cc = &expand($headers{'cc'});
    @to = &expand($headers{'to'}); 
    defined($headers{"apparently-to"}) && do {
	$apparentlyto = $headers{"apparently-to"};
	push(@to, &expand($apparentlyto));
    };

    ;# -----
    ;# $from comes from From: line. $address is their email address.
    ;# $organization is their site. for example, strike@pixel.convex.com 
    ;# yields an organization of convex.
    ;#
    $_ = $headers{'from'} ||
         $headers{'resent-from'} ||
         $headers{'sender'} ||
         $headers{'resent-sender'} ||
         $headers{'return-path'} ||
         $headers{'reply-to'};

    if ($_ eq "") {
       $friendly = $from = $address = $organization = "unknown";
       return;
    };

    ($friendly, $address, $from, $organization) = &parse_email_address($_);
}


# =====
# Subroutine parse_email_address
#	Parse an email address into address, from, organization
#	address is full Internet address, from is just the login
#	name and organization is Internet hostname (without final domain)
#
sub parse_email_address {
    local($_) = @_;
    local($friendly, $address, $from, $organization);

    $organization = "local";
    $friendly = "unknown";

# From: Disk Monitor Daemon (/usr/adm/bin/dfbitch) <daemon@hydra.convex.com>?

    s/^\s*//;
    s/\s*$//;
    if (/(.*)\s*<[^>]+>$|<[^>]+>\s*(.*)$/) {
	$friendly = $+;
	$friendly =~ s/\"//g;
    } elsif (/\(([^\)]+)\)/) {
	$friendly = $1;
    };

    s/.*<([^>]+)>.*/$1/;
    s/\(.*\)//;
    s/\s*$//;
    $address = $_;

    s/@.*//;
    s/%.*//;
    s/.*!//;
    s/\s//g;
    $from = $_;

    $_ = $address;
    tr/A-Z/a-z/;
    if (/!/ && /@/) {
        s/\s//g;
        s/!.*//;
        $organization = $_;
    } elsif (/!/) {
        s/\s//g;
        s/![A-Za-z0-9_@]*$//;
        s/.*!//;
        s/\..*//;
        $organization = $_;
    } elsif (/@/) {
        s/.*@//;
        s/\s//g;
        if (! /\./) {
            $organization = "unknown";
        } else {
            if (/\.(com|edu)$/) {
                s/\.[A-Za-z0-9_]*$//;
                s/.*\.//;
            } else {
                s/\.[A-Za-z0-9_]*$//;
                s/\.[A-Za-z0-9_]*$//;
                s/.*\.//;
            };
            $organization = $_;
        };
    };

    return ($friendly, $address, $from, $organization);
};


# ====
# Subroutine vacation
#	deliver a vacation message to the sender of this mail
#	message.
#
sub vacation {
    local($vacfile) = $ENV{'HOME'} . "/" . ".vacation.msg";
    local($msubject) = "\"Vacation mail for $ENV{'USER'} [Re: $subject]\" ";
    local($vacaudit, $astat, $mstat);
    local(@ignores);
    local(@names);

    return if (length($from) <= 0);
    return if ($precedence =~ /(bulk|junk)/i);
    return if ($from =~ /-REQUEST@/i);

    @ignores = ('daemon', 'postmaster', 'mailer-daemon', 'mailer', 'root',);
    grep(do {return if ($_ eq $from);}, @ignores);

    if (-e $vacfile) {
	($vacaudit = $vacfile) =~ s/\.msg/\.log/;

	$mstat = (stat($vacfile))[9];
	$astat = (stat($vacaudit))[9];
	unlink($vacaudit) if ($mstat > $astat);

        if (-f $vacaudit) {
	    open(VACAUDIT, "< $vacaudit") && do {
		while (<VACAUDIT>) {
		    chop; 
		    return if ($_ eq $from);
		};
		close(VACAUDIT);
	    };
        };

        open(MAIL,"| /usr/ucb/Mail -s $msubject $address") || return;
        open(VACFILE, "< $vacfile") || return;    
        while (<VACFILE>) {
	    s/\$SUBJECT/$subject/g;
            print MAIL $_;
        };
        close(VACFILE);
        close(MAIL);

        open(VACAUDIT, ">> $vacaudit") || return;
        print VACAUDIT "$from\n";
        close(VACAUDIT);
    };
}


# =====
# Subroutine expand
# 	expand a line (To, Cc, etc.) into a list of addressees.
#
sub expand {
    local($_) = @_;
    local(@fccs) = ( );

    return(@fccs) if /^$/;

    for (split(/\s*,\s*/)) {
	s/.*<([^>]+)>.*/$1/;
	s/@.*//;
	s/.*!//;
	s/\(.*\)//;
	s/\s//g;
	push(@fccs,$_) unless $seen{$_}++;
    } 

    return(@fccs);
} 


# =====
# Subroutine deliver
#	Deliver the incoming mail message to the user's mail drop
#
sub deliver {

    &deposit("/usr/spool/mail/$user");
}


# =====
#	Put the incoming mail into the specified mail drop (file)
#
sub deposit {
    local($drop) = @_;
    local($LOCK_EX) = 2;
    local($LOCK_UN) = 8;

    open(MAIL, ">> $drop") || die "open: $!\n";
    flock(MAIL, $LOCK_EX);
    seek(MAIL, 0, 2);

    print MAIL "$header";
    print MAIL "$body\n\n" if defined($body);

    flock(MAIL, $LOCK_UN);
    close(MAIL);
}


# =====
# Subroutine file_from
#	Add the mail message to another mail drop in a log directory.
#	The path of the mail drop is toplevel/organization/user
#
sub file_from {
    local($toplevel) = @_;
    local($dir);

    return if (length($from) <= 0);
    return if ($from eq $user);

    $toplevel = "log" if ($toplevel eq '');

    $dir = "$home/$toplevel";
    (!-d $dir) && mkdir($dir, 0700);
    $dir .= "/$organization";
    (!-d $dir) && mkdir($dir, 0700);

    &deposit("$dir/$from");
}


# =====
# Subroutine openpipe
#	Open a pipe to a command and write the mail message to it.
#
sub openpipe{
    local($command) = @_;

    open(CMD, "| $command") || die;
    print CMD "$header\n";
    print CMD "$body\n\n" if defined($body);
}

1;
