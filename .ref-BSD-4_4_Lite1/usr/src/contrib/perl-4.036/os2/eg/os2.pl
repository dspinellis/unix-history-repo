extproc C:\binp\misc\perl.exe -S
#!perl

# os2.pl:  Demonstrates the OS/2 system calls and shows off some of the
# features in common with the UNIX version.

do "syscalls.pl" || die "Cannot load syscalls.pl ($!)";

# OS/2 version number.

	$version = "  "; syscall($OS2_GetVersion,$version); 
	($minor, $major) = unpack("CC", $version);
	print "You are using OS/2 version ", int($major/10), 
			".", int($minor/10), "\n\n";
 
# Process ID.
	print "This process ID is $$ and its parent's ID is ", 
		getppid(), "\n\n";

# Priority.

	printf "Current priority is %x\n", getpriority(0,0);
	print "Changing priority by +5\n";
	print "Failed!\n" unless setpriority(0,0,+5);
	printf "Priority is now %x\n\n", getpriority(0,0);

# Beep.
	print "Here is an A440.\n\n";
	syscall($OS2_Beep,440,50);

# Pipes.  Unlike MS-DOS, OS/2 supports true asynchronous pipes.
	open(ROT13, '|perl -pe y/a-zA-Z/n-za-mN-ZA-M/') || die;
	select(ROT13); $|=1; select(STDOUT);
	print "Type two lines of stuff, and I'll ROT13 it while you wait.\n".
	      "If you type fast, you might be able to type both of your\n".
	      "lines before I get a chance to translate the first line.\n";
	$_ = <STDIN>; print ROT13 $_;
	$_ = <STDIN>; print ROT13 $_;
	close(ROT13);
	print "Thanks.\n\n";

# Inspecting the disks.
	print "Let's look at the disks you have installed...\n\n";

	$x = "\0\0";
	syscall($OS2_Config, $x, 2);
	print "You have ", unpack("S", $x), " floppy disks,\n";

	$x = "  ";
	syscall($OS2_PhysicalDisk, 1, $x, 2, 0, 0);
	($numdisks) = unpack("S", $x);

	print "and $numdisks partitionable disks.\n\n";
	for ($i = 1; $i <= $numdisks; $i++) {
		$disk = $i . ":";
		$handle = "  ";
		syscall($OS2_PhysicalDisk, 2, $handle, 2, $disk, 3);
		($numhandle) = unpack("S", $handle);
		$zero = pack("C", 0);
		$parmblock = " " x 16;
		syscall($OS2_IOCtl, $parmblock, $zero, 0x63, 9, $numhandle);
		($x, $cylinders, $heads, $sect) = unpack("SSSS", $parmblock);
		print "Hard drive #$i:\n";
		print "   cylinders: $cylinders\n";
		print "       heads: $heads\n";
		print "    sect/trk: $sect\n";
		syscall($OS2_PhysicalDisk, 3, 0, 0, $handle, 2);
	}

# I won't bother with the other stuff.  You get the idea.

