@REM=("
@perl %0.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
@end ") if 0 ;

#
# Test the ioctl function for MS-DOS.  Provide a list of drives and their
# characteristics.
#
# By Diomidis Spinellis.
#

@fdnum = ("STDIN", "STDOUT", "STDERR");
$maxdrives = 15;
for ($i = 3; $i < $maxdrives; $i++) {
	open("FD$i", "nul");
	@fdnum[$i - 1] = "FD$i";
}
@mediatype = (
	"320/360 k floppy drive",
	"1.2M floppy",
	"720K floppy",
	"8'' single density floppy",
	"8'' double density floppy",
	"fixed disk",
	"tape drive",
	"1.44M floppy",
	"other"
);
print "The system has the following drives:\n";
for ($i = 1; $i < $maxdrives; $i++) {
	if ($ret = ioctl(@fdnum[$i], 8, 0)) {
		$type = ($ret == 0) ? "removable" : "fixed";
		$ret = ioctl(@fdnum[$i], 9, 0);
		$location = ($ret & 0x800) ? "local" : "remote";
		ioctl(@fdnum[$i], 0x860d, $param);
		@par = unpack("CCSSSC31S", $param);
		$lock = (@par[2] & 2) ? "supporting door lock" : "not supporting door lock";
		printf "%c:$type $location @mediatype[@par[1]] @par[3] cylinders @par[6]
 sectors/track $lock\n", ord('A') + $i - 1;
	}
}
