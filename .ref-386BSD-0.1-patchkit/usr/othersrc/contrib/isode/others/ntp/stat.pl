#!/usr/bin/perl  
# $Source: /f/osi/others/ntp/RCS/stat.pl,v $ $Revision: 7.1 $ $Date: 91/02/22 09:34:10 $
#
#  Make plots from ntpd syslog messages.  Invoked as:
#
#   stat.pl [-o outputfile] [-i interval] [-t termtype] [-S] [-l] < logmessages
#
#  Where interval is the number of hours per plot, default is all data on
#  on set of plots.
#
#  termtype is the terminal type passed to gnuplot.  Default is postscript,
#  other useful alternative include 'tek', or 'unixplot'
#
#  output file is the name of the file which will receive plot data.  The
#  default is "stats.plot".
#
#  The -l option will also create log scale plots.
#
#  The -S option will "save" the intermedite data files, which are normally
#  deleted.
#
#  Louis A. Mamakos <louie@TRANTOR.UMD.EDU>
#  with many thanks to Larry Wall for `perl', a wonderful tool for hacking
#  up things like this so easily.
#

#
# Mar  7 18:46:58 trantor ntpd[20838]: adjust: SLEW 192.41.177.92 st 2 
#	off -0.015756 drft 0.000000 cmpl 0.000000
# Mar 10 08:56:19 trantor ntpd[27755]: clock: select peer 128.8.10.1 stratum 1
#	was 130.126.174.40 stratum 1
# Mar 31 16:55:19 trantor ntpd[2195]: /usr/local/etc/ntpd version $Revision:
#	3.4.1.5 $#
#
$scriptfile = $0;

$month{'Jan'} = 0; $month{'Feb'} = 1; $month{'Mar'} = 2; $month{'Apr'} = 3;
$month{'May'} = 4; $month{'Jun'} = 5; $month{'Jul'} = 6; $month{'Aug'} = 7;
$month{'Sep'} = 8; $month{'Oct'} = 9; $month{'Nov'} = 10; $month{'Dec'} = 11;

#
#  Currently, the year is not included in the syslog messages.  We'll have to
#  assume that the log files be processed were written this year.
#
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);

if ($year % 4) {
	# not leap year
	$days[0] = 0; $days[1] = 31; $days[2] = 59; $days[3] = 90;
	$days[4] = 120; $days[5] = 151; $days[6] = 181; $days[7] = 212;
	$days[8] = 243; $days[9] = 273; $days[10] = 304; $days[11] = 334; 
} else {
	# leap year
	$days[0] = 0; $days[1] = 31; $days[2] = 60; $days[3] = 91;
	$days[4] = 121; $days[5] = 152; $days[6] = 182; $days[7] = 213;
	$days[8] = 244; $days[9] = 274; $days[10] = 305; $days[11] = 335;
}

die "Can't open drift compansation file\n" unless open(DRIFT, ">stats.drift");
die "Can't open offset file\n" unless open(OFF, ">stats.off");
die "Can't open compliance file\n" unless open(COMP, ">stats.comp");
die "Can't open clock file\n" unless open(CLK, ">stats.clk");

$# = '%.6g';
$plottype = "postscript";
$recs = 0;
$start = 0;
$clocks = 1;
$clk{'UNSYNCED'} = 0;

do Getopt('tio');

if ($opt_h) {
   die "Usage: $scriptfile [-o outputfile] [-i interval] [-t termtype] [-l] [-S] < logmessages\n";
}

if ($opt_t) {
	$plottype = $opt_t;
}

if ($opt_i) {
	$interval = $opt_i;
} else {
	$interval = 99999999;
}

while (<>) {
	if (/.* ntpd\[[0-9]*\]: (.*\/ntpd) version \$Revision: \b(.*)\b/) {
		chop;
		@in = split;
		$recs++;
		$revision = $1 . " " . $2;

		print "Revision $revision\n";

		@time = split(/:/,$in[2]);
		$t = $in[1]*24 + $time[0] + $time[1]/60 + $time[2]/3600 +
			$days[$month{$in[0]}]*24;

		if (!$start) {
			$start = $t;
			printf "Start time is %s %s %s\n",$in[0],$in[1],$in[2];
		}
	}
	if (/.* ntpd\[[0-9]*\]: clock:/) {
		chop;
		@in = split;
		@time = split(/:/,$in[2]);
		$t = $in[1]*24 + $time[0] + $time[1]/60 + $time[2]/3600 +
			$days[$month{$in[0]}]*24;

		if (!$start) {
			$start = $t;
			printf "Start time is %s %s %s\n",$in[0],$in[1],$in[2];
		}
		if (!$clk{$in[8]}) {
			printf "Clock %d is %s\n", $clocks, $in[8];
			$clk{$in[8]} = $clocks++;
		}
		$t = $t - $start;
		print CLK $t," ",$clk{$in[8]},"\n";

	}
	if (/.* ntpd\[[0-9]*\]: adjust:/) {
		chop;
		@in = split;
		$recs++;

		@time = split(/:/,$in[2]);
		$t = $in[1]*24 + $time[0] + $time[1]/60 + $time[2]/3600 +
			$days[$month{$in[0]}]*24;
		if (!$start) {
			$start = $t;
			printf "Start time is %s %s %s\n",$in[0],$in[1],$in[2];
		}

		$t = $t - $start;

		# offset=11 drift=13 compliance=15

		print OFF $t, " ", $in[11],"\n";
		#
		#  Scale the drift compensation by 256.0 to convert to PPM.
		#
		print DRIFT $t," ", $in[13]/256.0, "\n";

		#
		#  Scale compliance by T (2**18)
		#
		if ( $in[15] < 0 ) {
			$in[15] = -$in[15];
		}
		print COMP $t," ",$in[15] * 2**18, "\n";
	}
}

if ($t = int($t)) {
	$last = int($t) + 1;
} else {
	$last = int($t);
}
print "$recs records spanning $t hours.\n";

close OFF;
close DRIFT;
close COMP;
close CLK;

if ($last == $start) {
	unlink "stats.script";
	unlink "stats.drift";
	unlink "stats.off";
	unlink "stats.comp";
	unlink "stats.clk";
	die "No statistics records found\n";
}

die "Can't open script file\n" unless open(TMP, ">stats.script");
#
#  Write script file for GNU plot.  Generate multiple sets of plots, each set
#  displaying the data over a specified interval.
#
print TMP "set samples ",$recs,"\n";
print TMP "set term $plottype\n";

if ($opt_o) {
	printf TMP 'set output "%s"', $opt_o; print TMP "\n";
} else {
	print TMP 'set output "stats.plot"'; print TMP "\n";
}

if ($interval > $last) {
	if ($interval != 99999999) {
		print "Interval truncated to available data ($last)\n";
	}
	$interval = $last;
}
#
#  Plot multiple sets of plots, each set of which covers the specified number
#  of hours.
#
$start = 0;
$end = $interval;
while (($start < $last)) {
	print TMP "set autoscale\n";
	print TMP "plot [$start:$end] ",'"stats.drift"'," with lines\n";
	print TMP "plot [$start:$end] ",'"stats.comp"'," with lines\n";
	if ($opt_l) {
		print TMP "set logscale y\n";
		print TMP "plot [$start:$end] ",'"stats.comp"'," with lines\n";
		print TMP "set nologscale\n";
	}
	print TMP "plot [$start:$end] [-0.1:0.1] ",
		'"stats.off"'," with lines\n";
	print TMP "plot [$start:$end] [0:$clocks]",
		'"stats.clk"'," with impulse\n";
	$start = $end;
	$end += $interval;
}
close TMP;

#
#  Now, run gnuplot on the script file.
#
system "gnuplot < stats.script > /dev/null 2>&1" || 
	die "gnuplot croaked: exit=" . $? . "\n";

if (!$opt_S) {
	unlink "stats.script";
	unlink "stats.drift";
	unlink "stats.off";
	unlink "stats.comp";
	unlink "stats.clk";
}

;# Process single-character switches with switch clustering.  Pass one argument
;# which is a string containing all switches that take an argument.  For each
;# switch found, sets $opt_x (where x is the switch name) to the value of the
;# argument, or 1 if no argument.  Switches which take an argument don't care
;# whether there is a space between the switch and the argument.

;# Usage:
;#	do Getopt('oDI');  # -o, -D & -I take arg.  Sets opt_* as a side effect.

sub Getopt {
    local($argumentative) = @_;
    local($_,$first,$rest);

    while (($_ = $ARGV[0]) =~ /^-(.)(.*)/) {
	($first,$rest) = ($1,$2);
	if (index($argumentative,$first) >= $[) {
	    if ($rest ne '') {
		shift;
	    }
	    else {
		shift;
		$rest = shift;
	    }
	    eval "\$opt_$first = \$rest;";
	}
	else {
	    eval "\$opt_$first = 1;";
	    if ($rest ne '') {
		$ARGV[0] = "-$rest";
	    }
	    else {
		shift;
	    }
	}
    }
}

#
# Local Variables:
# mode: text
# End:
