.NH 3
/sys/vaxmba
.PP
The following bug fixes and modifications have
been applied to the MASSBUS device drivers:
.IP \fBhp.c\fP 10
a large number of disk drives attached to second vendor disk
controllers are now automatically
recognized at boot time by probing the holding register and
using disk geometry information to decide what kind of drive
is present; the hpSDIST/hpRDIST parameters that control seek
and search operations on controllers with multiple drives have
been made a per-drive parameter; a bug where the sector number
reported on a hard error was off by one has been fixed; the
error recovery code now searches the bad sector table when a
header CRC error occurs; the error recovery code now
handles bad sectors on tracks which also have skip sectors; 
a bug in the handling of ECC errors has been fixed; many 
separate driver data structures have been consolidated into
the software carrier structure;
the driver handles the ML-11 solid-state disk
.IP \fBmba.c\fP 10
now autoconfigures MASSBUS tapes and disks which ``come on-line''
after the initial boot
