.SH
.ce
\s+2Extracting the tape\s0
.PP
This tape has been written using the new
.I tar
tape achiver program.
If you don't have
.I tar,
you can extract a binary of the pogram from the tape by giving the following command:
.DS
% dd if=/dev/mt0 bs=1b skip=1 count=49 of=tar
% chmod 755 tar
.DE
You can then extract the tape with the (single) command
.DS
% tar x
.DE
This will use ``mkdir'' to make directories as needed.
A manual page for
.I tar
is attached.
