.\" Copyright (c) 1983, 1993, 1994
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" Redistribution and use in source and binary forms, with or without
.\" modification, are permitted provided that the following conditions
.\" are met:
.\" 1. Redistributions of source code must retain the above copyright
.\"    notice, this list of conditions and the following disclaimer.
.\" 2. Redistributions in binary form must reproduce the above copyright
.\"    notice, this list of conditions and the following disclaimer in the
.\"    documentation and/or other materials provided with the distribution.
.\" 3. All advertising materials mentioning features or use of this software
.\"    must display the following acknowledgement:
.\"	This product includes software developed by the University of
.\"	California, Berkeley and its contributors.
.\" 4. Neither the name of the University nor the names of its contributors
.\"    may be used to endorse or promote products derived from this software
.\"    without specific prior written permission.
.\"
.\" THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
.\" ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
.\" IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
.\" ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
.\" FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
.\" DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
.\" OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
.\" HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.\" LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
.\" OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
.\" SUCH DAMAGE.
.\"
.\"	@(#)2.1.t	8.4 (Berkeley) 5/26/94
.\"
.Sh 2 "Generic operations
.PP
Many system abstractions support the
.Fn read ,
.Fn write ,
and
.Fn ioctl
operations.
We describe the basics of these common primitives here.
Similarly, the mechanisms whereby normally synchronous operations
may occur in a non-blocking or asynchronous fashion are
common to all system-defined abstractions and are described here.
.Sh 3 "Read and write
.PP
The
.Fn read
and
.Fn write
system calls can be applied to communications channels,
files, terminals and devices.
They have the form:
.DS
.Fd read 3 "read input
cc = read(fd, buf, nbytes);
result ssize_t cc; int fd; result void *buf; size_t nbytes;
.DE
.DS
.Fd write 3 "write output
cc = write(fd, buf, nbytes);
result ssize_t cc; int fd; void *buf; size_t nbytes;
.DE
The
.Fn read
call transfers as much data as possible from the
object defined by \fIfd\fP to the buffer at address \fIbuf\fP of
size \fInbytes\fP.  The number of bytes transferred is
returned in \fIcc\fP, which is \-1 if a return occurred before
any data was transferred because of an error or use of non-blocking
operations.
A return value of 0 is used to indicate an end-of-file condition.
.PP
The
.Fn write
call transfers data from the buffer to the
object defined by \fIfd\fP.  Depending on the type of \fIfd\fP,
it is possible that the
.Fn write
call will accept only a portion of the provided bytes;
the user should resubmit the other bytes in a later request.
Error returns because of interrupted or otherwise incomplete operations
are possible, in which case no data will have been transferred.
.PP
Scattering of data on input, or gathering of data for output
is also possible using an array of input/output vector descriptors.
The type for the descriptors is defined in \fI<sys/uio.h>\fP as:
.DS
.TS
l s s s
l l l l.
struct iovec {
	char	*iov_base;	/* base of a component */
	size_t	iov_len;	/* length of a component */
};
.TE
.DE
.LP
The \fIiov_base\fP field should be treated as if its type were
``void *'' as POSIX and other versions of the structure may use
that type.
Thus, pointer arithmetic should not use this value without a cast.
.LP
The calls using an array of \fIiovec\fP structures are:
.DS
.Fd readv 3 "read gathered input
cc = readv(fd, iov, iovlen);
result ssize_t cc; int fd; struct iovec *iov; int iovlen;
.DE
.DS
.Fd writev 3 "write scattered output
cc = writev(fd, iov, iovlen);
result ssize_t cc; int fd; struct iovec *iov; int iovlen;
.DE
Here \fIiovlen\fP is the count of elements in the \fIiov\fP array.
.Sh 3 "Input/output control
.LP
Control operations on an object are performed by the
.Fn ioctl
operation:
.DS
.Fd ioctl 3 "control device
ioctl(fd, request, buffer);
int fd; u_long request; caddr_t buffer;
.DE
This operation causes the specified \fIrequest\fP to be performed
on the object \fIfd\fP.  The \fIrequest\fP parameter specifies
whether the argument buffer is to be read, written, read and written,
or is not used, and also the size of the buffer, as well as the
request.
Different descriptor types and subtypes within descriptor types
may use distinct
.Fn ioctl
requests. For example,
operations on terminals control flushing of input and output
queues and setting of terminal parameters; operations on
disks cause formatting operations to occur; operations on tapes
control tape positioning.
The names for basic control operations are defined by \fI<sys/ioctl.h>\fP,
or more specifically by files it includes.
.Sh 3 "Non-blocking and asynchronous operations
.PP
A process that wishes to do non-blocking operations on one of
its descriptors sets the descriptor in non-blocking mode as
described in section
.Xr 1.5.4 .
Thereafter the
.Fn read
call will return a specific EWOULDBLOCK error indication
if there is no data to be
.Fn read .
The process may
.Fn select
the associated descriptor to determine when a read is possible.
.PP
Output attempted when a descriptor can accept less than is requested
will either accept some of the provided data, returning a shorter than normal
length, or return an error indicating that the operation would block.
More output can be performed as soon as a
.Fn select
call indicates
the object is writable.
.PP
Operations other than data input or output
may be performed on a descriptor in a non-blocking fashion.
These operations will return with a characteristic error indicating
that they are in progress
if they cannot complete immediately.  The descriptor
may then be
.Fn select 'ed
for
.Fn write
to find out when the operation has been completed.
When
.Fn select
indicates the descriptor is writable, the operation has completed.
Depending on the nature of the descriptor and the operation,
additional activity may be started or the new state may be tested.
