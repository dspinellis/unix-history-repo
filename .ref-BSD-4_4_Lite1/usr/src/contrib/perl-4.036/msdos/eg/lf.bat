@REM=("
@perl %0.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
@end ") if 0 ;

# Convert all the files in the current directory from MS-DOS to unix
# line ending conventions.
#
# By Diomidis Spinellis
#
open(FILES, 'find . -print |');
while ($file = <FILES>) {
	$file =^ s/[\n\r]//;
	if (-f $file) {
		if (-B $file) {
			print STDERR "Skipping binary file $file\n";
			next;
		}
		($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime, $mtime, $ctime,
 $blksize, $blocks) = stat($file);
		open(IFILE, "$file");
		open(OFILE, ">xl$$");
		binmode OFILE || die "binmode xl$$: $!\n";
		while (<IFILE>) {
			print OFILE;
		}
		close(OFILE) || die "close xl$$: $!\n";
		close(IFILE) || die "close $file: $!\n";
		unlink($file) || die "unlink $file: $!\n";
		rename("xl$$", $file) || die "rename(xl$$, $file): $!\n";
		chmod($mode, $file) || die "chmod($mode, $file: $!\n";
		utime($atime, $mtime, $file) || die "utime($atime, $mtime, $file): $!\n";
	}
}
