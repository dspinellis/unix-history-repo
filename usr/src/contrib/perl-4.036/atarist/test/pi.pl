# ---------------------------------------------------------------------------
# pi.perl  computes pi (3.14...) about 5120 Digits
#
# W. Kebsch, July-1988  {uunet!mcvax}!unido!nixpbe!kebsch

$my_name = $0;
$version = $my_name . "-1.2";

# some working parameter

$smax =  5120;          # max digits
$lmax =     4;          # digits per one array element
$hmax = 10000;          # one array element contains: 0..9999
$smin = $lmax;          # min digits
$mag  =     7;          # magic number

# subroutines

sub mul_tm              # multiply the tm array with a long value
{
    $cb = pop(@_);      # elements(array)
    $x  = pop(@_);      # value

    $c = 0;
    for($i = 1; $i <= $cb; $i++)
    {
	$z      = $tm[$i] * $x + $c;
	$c      = int($z / $hmax);
	$tm[$i] = $z - $c * $hmax;
    }
}

sub mul_pm              # multiply the pm array with a long value
{
    $cb = pop(@_);      # elements(array)
    $x  = pop(@_);      # value

    $c = 0;
    for($i = 1; $i <= $cb; $i++)
    {
	$z      = $pm[$i] * $x + $c;
	$c      = int($z / $hmax);
	$pm[$i] = $z - $c * $hmax;
    }
}

sub divide              # divide the tm array by a long value
{
    $cb = pop(@_);      # elements(array)
    $x  = pop(@_);      # value

    $c = 0;
    for($i = $cb; $i >= 1; $i--)
    {
	$z      = $tm[$i] + $c;
	$q      = int($z / $x);
	$tm[$i] = $q;
	$c      = ($z - $q * $x) * $hmax;
    }
}

sub add                 # add tm array to pm array
{
    $cb = pop(@_);      # elements(array)

    $c = 0;
    for($i = 1; $i <= $cb; $i++)
    {
	$z = $pm[$i] + $tm[$i] + $c;
	if($z >= $hmax)
	{
	    $pm[$i] = $z - $hmax;
	    $c      = 1;
	}
	else
	{
	    $pm[$i] = $z;
	    $c      = 0;
	}
    }
}

$m0 = 0; $m1 = 0; $m2 = 0;

sub check_xb            # reduce current no. of elements (speed up!)
{
    $cb = pop(@_);      # current no. of elements

    if(($pm[$cb] == $m0) && ($pm[$cb - 1] == $m1) && ($pm[$cb - 2] == $m2))
    {
	$cb--;
    }
    $m0 = $pm[$cb];
    $m1 = $pm[$cb - 1];
    $m2 = $pm[$cb - 2];
    $cb;
}

sub display             # show the result
{
    $cb = pop(@_);      # elements(array);

    printf("\n%3d.", $pm[$cb]);
    $j = $mag - $lmax;
    for($i = $cb - 1; $i >= $j; $i--)
    {
	printf(" %04d", $pm[$i]);
    }
    print "\n";
}

sub the_job             # let's do the job
{
    $s = pop(@_);       # no. of digits

    $s  = int(($s + $lmax - 1) / $lmax) * $lmax;
    $b  = int($s / $lmax) + $mag - $lmax;
    $xb = $b;
    $t  = int($s * 5 / 3);

    for($i = 1; $i <= $b; $i++)         # init arrays
    {
	$pm[$i] = 0;
	$tm[$i] = 0;
    }
    $pm[$b - 1] = $hmax / 2;
    $tm[$b - 1] = $hmax / 2;

    printf("digits:%5d, terms:%5d, elements:%5d\n", $s, $t, $b);
    for($n = 1; $n <= $t; $n++)
    {
	printf("\r\t\t\t  term:%5d", $n);
	if($n < 200)
	{
	    do mul_tm((4 * ($n * $n - $n) + 1), $xb);
	}
	else
	{
	    do mul_tm((2 * $n - 1), $xb);
	    do mul_tm((2 * $n - 1), $xb);
	}
	if($n < 100)
	{
	    do divide(($n * (16 * $n + 8)), $xb);
	}
	else
	{
	    do divide((8 * $n), $xb);
	    do divide((2 * $n + 1), $xb);
	}
	do add($xb);
	if($xb > $mag)
	{
	    $xb = do check_xb($xb);
	}
    }
    do mul_pm(6, $b);
    do display($b);
    ($user,$sys,$cuser,$csys) = times;
    printf("\n[u=%g  s=%g  cu=%g  cs=%g]\n",$user, $sys, $cuser, $csys);
}

# main block ----------------------------------------------------------------

$no_of_args = $#ARGV + 1;
print("$version, ");
die("usage: $my_name <no. of digits>") unless($no_of_args == 1);
$digits = int($ARGV[0]);
die("no. of digits out of range [$smin\..$smax]")
			    unless(($digits >= $smin) && ($digits <= $smax));
do the_job($digits);
exit 0;

# That's all ----------------------------------------------------------------
