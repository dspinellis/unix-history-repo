.SH
Summary
.PP
The current 
.UX
kernel implementation is quite inflexible,
closing off what might be interesting design choices.
Lightweight processes both in the kernel and in user processes could
be used to good advantage.
The kernel is not properly structured to allow easy use of different
facilities together.
Streams may be a decent first step in this direction.
.PP
Stub generators, message passing and
RPC transport protocols all
need substantial work as 
.UX
moves into the distributed world.
Using these protocols without stub generators is like a day without sunshine.
.PP
Resource location, authentication and naming are issues
.UX
has not faced in the distributed
environment.
Cascaded services present another level of issues which need to be faced
in their design.
.PP
.UX
has ascii terminals ingrained into its very nature.
It will take much more work to smooth the rough edges emerging from
the forced marriage of workstation displays with 
.UX .
.PP
If a system resource is in short supply (as file descriptors are),
the correct solution is to lift the limit entirely.
Doubling or tripling a limit on a resource only delays the day of
reckoning,
while still preventing those design strategies that found them in
short supply originally.
.PP
Shared memory should allow sharing of memory between processes,
between the kernel and a process, and between a process and hardware.
Shared libraries would open up design opportunities.
.PP
More work needs to be done on performance of some of the new kernel
facilities.
The X server uses select more heavily than any other system call,
accounting for the largest single component of CPU time used,
though select is not the limit in absolute performance.

