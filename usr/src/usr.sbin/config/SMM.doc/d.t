.\" Copyright (c) 1983 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)d.t	6.1 (Berkeley) %G%
.\"
.ds LH "Building Systems With Config
.ds RH "Data Structure Sizing Rules
.ds CF July 27, 1983
.LP
.nr H1 1
.ds CH "
.bp
.ds CH "\(hy \\n(PN \(hy
.LG
.B
.ce
APPENDIX D. VAX KERNEL DATA STRUCTURE SIZING RULES
.sp 2
.R
.NL
.PP
Certain system data structures are sized at compile time
according to the maximum number of simultaneous users expected,
while others are calculated at boot time based on the
physical resources present; e.g. memory.  This appendix lists
both sets of rules and also includes some hints on changing
built-in limitations on certain data structures.
.SH
Compile time rules
.PP
The file \fI/sys/conf/param.c\fP contains the definitions of
almost all data structures sized at compile time.  This file
is copied into the directory of each configured system to allow
configuration-dependent rules and values to be maintained.
The rules implied by its contents are summarized below (here
MAXUSERS refers to the value defined in the configuration file
in the ``maxusers'' rule).
.IP \fBnproc\fP
.br
The maximum number of processes which may be running at
any time.  It is defined to be 20 + 8 * MAXUSERS and referred
to in other calculations as NPROC.
.IP \fBntext\fP
.br
The maximum number of active shared text segments.  Defined as
24 + MAXUSERS + NETSLOP, where NETSLOP is 20 when the Internet
protocols are configured in the system and 0 otherwise.  The
added size for supporting the network is to take into account
the numerous server processes which are likely to exist.
.IP \fBninode\fP
.br
The maximum number of files in the file system which may be
active at any time.  This includes files in use by users, as 
well as directory files being read or written by the system
and files associated with bound sockets in the UNIX ipc domain.
This is defined as (NPROC + 16 + MAXUSERS) + 32.
.IP \fBnfile\fP
.br
The number of ``file table'' structures.  One file
table structure is used for each open, unshared, file descriptor.
Multiple file descriptors may reference a single file table
entry when they are created through a \fIdup\fP call, or as the
result of a \fIfork\fP.  This is defined to be
.DS
16 * (NPROC + 16 + MAXUSERS) / 10 + 32 + 2 * NETSLOP
.DE
where NETSLOP is defined as for \fBntext\fP.
.IP \fBncallout\fP
.br
The number of ``callout'' structures.  One callout
structure is used per internal system event handled with
a timeout.  Timeouts are used for terminal delays,
watchdog routines in device drivers, protocol timeout processing, etc.
This is defined as 16 + NPROC.
.IP \fBnclist\fP
.br
The number of ``c-list'' structures.  C-list structures are
used in terminal i/o.  This is defined as 100 + 16 * MAXUSERS.
.IP \fBnmbclusters\fP
.br
The maximum number of pages which may be allocated by the network.  
This is defined as 256 (a quarter megabyte of memory) in /sys/h/mbuf.h.
In practice, the network rarely uses this much memory.  It starts off
by allocating 64 kilobytes of memory, then requesting more as 
required.  This value represents an upper bound.
.IP \fBnquota\fP
.br
The number of ``quota'' structures allocated.  Quota structures
are present only when disc quotas are configured in the system.  One
quota structure is kept per user.  This is defined to be
(MAXUSERS * 9) / 7 + 3.
.IP \fBndquot\fP
.br
The number of ``dquot'' structures allocated.  Dquot structures
are present only when disc quotas are configured in the system.
One dquot structure is required per user, per active file system quota.
That is, when a user manipulates a file on a file system on which
quotas are enabled, the information regarding the user's quotas on
that file system must be in-core.  This information is cached, so
that not all information must be present in-core all the time.
This is defined as (MAXUSERS * NMOUNT) / 4 + NPROC, where NMOUNT
is the maximum number of mountable file systems.
.LP
In addition to the above values, the system page tables (used to
map virtual memory in the kernel's address space) are sized at
compile time by the SYSPTSIZE definition in the file /sys/vax/vmparam.h.
This is defined to be 20 + MAXUSERS pages of page tables. 
Its definition affects
the size of many data structures allocated at boot time because
it constrains the amount of virtual memory which may be addressed
by the running system.  This is often the limiting factor
in the size of the buffer cache.
.SH
Run-time calculations
.PP
The most important data structures sized at run-time are those used in
the buffer cache.  Allocation is done by swiping physical memory
(and the associated virtual memory) immediately after the system
has been started up; look in the file /sys/vax/machdep.c.
The amount of physical memory which may be allocated to the buffer
cache is constrained by the size of the system page tables, among
other things.  While the system may calculate
a large amount of memory to be allocated to the buffer cache,
if the system page
table is too small to map this physical
memory into the virtual address space
of the system, only as much as can be mapped will be used.
.PP
The buffer cache is comprised of a number of ``buffer headers''
and a pool of pages attached to these headers.  Buffer headers
are divided into two categories: those used for swapping and
paging, and those used for normal file i/o.  The system tries
to allocate 10% of available physical memory for the buffer
cache (where \fIavailable\fP does not count that space occupied by
the system's text and data segments).  If this results in fewer
than 16 pages of memory allocated, then 16 pages are allocated.
This value is kept in the initialized variable \fIbufpages\fP
so that it may be patched in the binary image (to allow tuning
without recompiling the system).  A sufficient number of
file i/o buffer headers are then allocated to allow each to hold
2 pages each, and half as many swap i/o buffer headers are then
allocated.  The number of swap i/o buffer headers is constrained
to be no more than 256.
.SH
System size limitations
.PP
As distributed, the sum of the virtual sizes of the core-resident
processes is limited to 64M bytes.  The size of the text, and data
segments of a single process are currently limited to 6M bytes each, and
the stack segment size is limited to 512K bytes as a soft, user-changeable
limit, and may be increased to 6M with the \fIsetrlimit\fP\|(2) system call.
If these are insufficient, they
can be increased by changing the constants MAXTSIZ, MAXDSIZ and MAXSSIZ
in the file
/sys/vax/vmparam.h.
The size of the swap maps for these objects must also be increased;
for text, the parameters are NXDAD (/sys/h/text.h)
and DMTEXT (/sys/vax/autoconfig.c).
The maps for data and swap are limited by NDMAP (/sys/h/dmap.h)
and DMMAX (/sys/vax/autoconfig.c).
You must be careful in doing this that you have adequate paging space.
As normally configured , the system has only 16M bytes per paging area.
The best way to get more space is to provide multiple, thereby
interleaved, paging areas.
.PP
To increase the amount of resident virtual space possible,
you can alter the constant USRPTSIZE (in
/sys/vax/vmparam.h).
To allow 128 megabytes of resident virtual space one would
change the 8 to a 16.
.PP
Because the file system block numbers are stored in
page table \fIpg_blkno\fP
entries, the maximum size of a file system is limited to
2^19 1024 byte blocks.  Thus no file system can be larger than 512M bytes.
.PP
The count of mountable file systems is limited to 15.  This should
be sufficient.  If you have many disks it makes sense to make some of
them single file systems, and the paging areas don't count in this total.
To increase this it will be necessary to change the core-map
/sys/h/cmap.h since there is a 4 bit field used here.  The size
of the core-map will then expand to 16 bytes per 1024 byte page.
(Don't forget to change MSWAPX and NMOUNT in /sys/h/param.h also.)
.PP
The maximum value NOFILE (open files per process limit)
can be raised to
is 30 because of a bit field in the page table entry in
/sys/machine/pte.h.
.PP
The amount of physical memory is currently limited to 8 Mb
by the size of the index fields in the core-map (/sys/h/cmap.h).
This limit is also found in /sys/vax/locore.s.
