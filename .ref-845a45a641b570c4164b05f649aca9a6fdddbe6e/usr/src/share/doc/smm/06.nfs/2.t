.\"	@(#)2.t	5.1 (Berkeley) %G%
.\"
.\" troff -ms
.\" Here is half a paper on the cache consistency protocol. It won't
.\" make much sense unless you have read the [Nelson88] or [Srinivasan89]
.\" and [Gray89] references.
.TL
PRELIMINARY DRAFT: Not Quite NFS, Crash Tolerant Cache Consistency for NFS
.AU
Rick Macklem
rick@snowhite.cis.uoguelph.ca
.AI
University of Guelph
.AB
.PP
Not Quite NFS (NQNFS) is an NFS like protocol designed to maintain full cache
consistency between clients in a crash tolerant manner. It is an adaptation
of the NFS protocol such that the server supports both NFS
and NQNFS clients while maintaining full consistency between the server and
NQNFS clients.
It borrows heavily from work done on Spritely-NFS [Srinivasan89], but uses
Leases [Gray89] to avoid the need to recover server state information
after a crash.
.AE
.SH
1. Overview
.PP
The protocol maintains cache consistency by using a somewhat Sprite [Nelson88] like
protocol, but is based on short term leases\** instead of hard state information
about open files.
.FS
A lease is a ticket permitting an activity that is valid until some expiry time.
.FE
The basic principal is that the protocol will disable client caching of a
file whenever that file is write shared\**.
.FS
Write sharing occurs when at least one client is modifying a file while
other client(s) are reading the file.
.FE
Whenever a client wishes to cache data for a file it must hold a valid lease.
There are three types of leases: read caching, write caching and non-caching.
The latter type requires that all file operations be done synchronously with
the server via. RPCs.
A read caching lease allows for client data caching, but no file modifications
may be done.
A write caching lease allows for client caching of writes,
but requires that all writes be pushed to the server when the lease expires.
If a client has dirty buffers\**
.FS
cached write data not yet pushed (written) to the
server
.FE
when a write cache lease has almost expired, it will attempt to
extend the lease but is required to push the dirty buffers if extension fails.
A client gets leases by either doing a \fBGetLease RPC\fR or by piggybacking
a \fBGetLease Request\fR onto another RPC. Piggybacking is supported for the
frequent RPCs Getattr, Setattr, Lookup, Readlink, Read, Write and Readdir
in an effort to minimize the number of \fBGetLease RPCs\fR required.
All leases are at the granularity of a file, since all NFS RPCs operate on
individual files and NFS has no intrinsic notion of a file hierarchy.
Directories, symbolic links and file attributes may be read cached but are not write cached.
The exception here is the attribute file_size, which is updated during cached
writing on the client to reflect a growing file.
.PP
It is the server's responsibility to ensure that consistency is maintained
among the NQNFS clients by disabling client caching whenever a server file
operation would cause inconsistencies.
This occurs whenever a client has a write caching lease and any other client\**
.FS
including local operations on the server
.FE
tries to access the file or when
a modify operation is attempted on a file being read cached by client(s).
At this time, the server sends an \fBeviction notice\fR to all clients holding
the lease and then waits for lease termination.
Lease termination occurs when a \fBvacated the premises\fR message has been
received from all the clients that have signed the lease or when the lease
expires via. timeout.
The message pair \fBeviction notice\fR and \fBvacated the premises\fR roughly
correspond to a Sprite server\(->client callback, but are not implemented as an
actual RPC, to avoid the server waiting indefinitely for a reply from a dead
client.
.PP
Server consistency checking can be viewed as issuing intrinsic leases for a
file operation for the duration of the operation only. For example, the
\fBCreate RPC\fR will get an intrinsic write lease on the directory in which
the file is being created, disabling client read caches for that directory.
.PP
By relegating this responsibility to the server, consistency between the
server and NQNFS clients is maintained when NFS clients are modifying the
file system as well.\**
.FS
The NFS clients will continue to be \fIapproximately\fR consistent with
the server.
.FE
.PP
The leases are issued as time intervals to avoid the requirement of time of day
clock synchronization. There are three important time constants known to
the server. The \fBmaximum_lease_term\fR sets an upper bound on lease duration.
The \fBclock_skew\fR is added to all lease terms on the server to correct for
differing clock speeds between the client and server and \fBwrite_slack\fR is
the number of seconds the server is willing to wait for a client with an expired
write caching lease to push dirty writes.
.PP
The server maintains a \fBmodify_revision\fR number for each file. It is
defined as a unsigned quadword integer that is never zero and that must
increase whenever the corresponding file is modified on the server.
It is used
by the client to determine whether or not cached data for the file is
stale.
Generating this value is easier said than done. The current implementation
uses the following technique, which I believe to be adequate.
The high order longword is stored in the ufs inode and is initialized to one
when an inode is first allocated.
The low order longword is stored in main memory only and is initialized to
zero when an inode is read in from disk.
When the file is modified for the first time within a given second of wall clock
time, the high order longword is incremented by one and the low order longword
reset to zero. For subsequent modifications within the same second of wall clock
time, the low order longword is incremented. If the low order longword wraps
around to zero, the high order longword is incremented again.
Since the high order longword only increments once per second and the inode
is pushed to disk frequently during file modification, this implies
0 \(<= Current\(miDisk \(<= 5.
When the inode is read in from disk, 10
is added to the high order longword, which ensures that the quadword
is greater than any value it could have had before a crash.
This introduces apparent modifications every time the inode falls out of
the LRU inode cache, but this should only reduce the client caching performance
by a (hopefully) small margin.
.SH
2. Crash Recovery and other Failure Scenarios
.PP
The server must maintain the state of all the current leases held by clients.
The nice thing about short term leases is that maximum_lease_term seconds
after the server stops issuing leases, there are no current leases left.
As such, server crash recovery does not require any state recovery. After
rebooting, the server refuses to service any RPCs except for writes until
write_slack seconds after the last lease would have expired\**.
.FS
The last lease expiry time may be safely estimated as
"boottime+maximum_lease_term+clock_skew" for machines that cannot store
it in nonvolatile RAM.
.FE
By then, the server would not have any outstanding leases to recover the
state of and the clients have had at least write_slack seconds to push dirty
writes to the server and get the server sync'd up to date. After this, the
server simply services requests in a manner similar to NFS.
In an effort to minimize the effect of "recovery storms" [Baker91], the server replies
\fBtry_again_later\fR to the RPCs it is not
yet ready to service.
.PP
After a client crashes, the server may have to wait for a lease to timeout
before servicing a request if write sharing of a file with a cachable lease
on the client is about to occur.
As for the client, it simply starts up getting any leases it now needs. Any
outstanding leases for that client on the server prior to the crash will either be renewed or expire
via timeout.
.PP
Certain network partitioning failures are more problematic. If a client to
server network connection is severed just before a write caching lease expires,
the client cannot push the dirty writes to the server. After the lease expires
on the server, the server permits other clients to access the file with the
potential of getting stale data. Unfortunately I believe this failure scenario
is intrinsic in any delay write caching scheme unless the server is required to
wait \fBforever\fR for a client to regain contact\**.
.FS
Gray and Cheriton avoid this problem by using a \fBwrite through\fR policy.
.FE
Since the write caching lease has expired on the client, it will sync up with the
server as soon as the network connection has been re-established.
.PP
There is another failure condition that can occur when the server is congested.
The worst case scenario would have the client pushing dirty writes to the server
but a large request queue on the server delays these writes for more than
\fBwrite_slack\fR seconds. It is hoped that a congestion control scheme using
the \fBtry_again_later\fR RPC reply can minimize the risk of this occurrence.
.SH
3. Server Disk Full
.PP
There is a serious unresolved problem for delayed write caching with respect to
server disk space allocation.
When the disk on the file server is full, delayed write RPCs can fail
due to "out of space".
For NFS, this occurrence results in an error return from the close system
call on the file, since the dirty blocks are pushed on close.
Processes writing important files can check for this error return
to ensure that the file was written successfully.
For NQNFS, the dirty blocks are not pushed on close and as such the client
may not attempt the write RPC until after the process has done the close
which implies no error return from the close.
For the current prototype, the only solution is to modify programs writing important
file(s) to call fsync and check for an error return from it instead of close.
.SH
4. Protocol Details
.PP
The protocol specification is identical to that of NFS [RFC1094] except for
the following changes.
.IP \(bu
Get Lease RPC
.sp
.nf
        struct getleaseargs {
                fhandle file;
                cachetype readwrite;
                unsigned duration;
        };

        union getleaseres switch (stat status) {
        case NFS_OK:
                boolean cachable;
                unsigned duration;
                modifyrev rev;
                fattr attributes;
        default:
                void;
        };

        getleaseres
        NQNFSPROC_GETLEASE(getleaseargs) = 18;

.fi
Gets a lease for "file" valid for "duration" seconds from when the lease
was issued on the server\**. The lease permits client caching if "cachable"
.FS
To be safe, the client may only assume that the lease is valid for "duration"
seconds from when the RPC request was sent to the server.
.FE
is true. The modify revision level and attributes for the file are also
returned.
.IP \(bu
Eviction Message
.sp
.nf
        void
        NQNFSPROC_EVICTION (fhandle) = 19;

.fi
This message is sent from the server to the client. When the client receives
the message, it should flush data associated with the file represented by
"fhandle" from its caches and then send the \fBVacated Message\fR back to
the server. Flushing includes pushing any dirty writes via. write RPCs.
.IP \(bu
Vacated Message
.sp
.nf
        void
        NQNFSPROC_VACATED (fhandle) = 20;

.fi
This message is sent from the client to the server in response to the
\fBEviction Message\fR. See above.
.IP \(bu
Piggybacked Get Lease Request
.sp
The piggybacked get lease request is functionally equivalent to the Get Lease
RPC except that is attached to one of the other NQNFS RPC requests as follows.
A getleaserequest is prepended to all of the NFS request arguments for NQNFS
and a getleaserequestres is inserted in all NFS result structures just after
the "stat" field only if "stat == NFS_OK".
.sp
.nf
        union getleaserequest switch (cachetype type) {
        case NQNFS_READCACHE:
        case NQNFS_WRITECACHE:
                unsigned duration;
        default:
                void;
        };

        union getleaserequestres switch (cachetype type) {
        case NQNFS_READCACHE:
        case NQNFS_WRITECACHE:
                boolean cachable;
                unsigned duration;
                modifyrev rev;
        default:
                void;
        };

.fi
The get lease request applies to the file that the attached RPC operates on
and the file attributes remain in the same location as for the NFS RPC reply
structure.
.IP \(bu
The Write and Setattr RPCs return a modified "attrstat" with a "modifyrev"
added
.sp
.nf
        union modattrstat switch (stat status) {
        case NFS_OK:
                union getleaserequestres piggy;
                fattr attributes;
                modifyrev rev;
        default:
                void;
        };

NB: Note that I have included the "getleaserequestres" union in the above
    as it is positioned in all NQNFS RPC replies.
    Also, the modifyrev in "piggy" will not be the same as "rev", since
    any piggybacked lease is acquired before the write operation.
.fi
.IP \(bu
An additional "stat" value
.sp
An additional value has been added to the enumerated type "stat"
NQNFS_TRYAGAINLATER=501.
This value is returned by the server when it wishes the client to retry the
RPC request after a short delay. It is used during crash recovery (Section 2)
and may also be useful for server congestion control.
.SH
4. Data Types
.IP \(bu
cachetype
.sp
.nf
        enum cachetype {
                NQLNONE = 0,
                NQLREAD = 1,
                NQLWRITE = 2
        };

.fi
Type of lease requested. NQLNONE is used to indicate no piggybacked lease
request.
.IP \(bu
modifyrev
.sp
.nf
        typedef unsigned hyper modifyrev;

.fi
The "modifyrev" is a unsigned quadword integer value that is never zero
and increases every time the corresponding file is modified on the server.
.SH
References
.IP [Baker91]
Mary Baker and John Ousterhout, Availability in the Sprite Distributed
File System, In \fIOperating System Review\fR, (25)2, pg. 95-98,
April 1991.
.IP [Baker91a]
Mary Baker, Private Email Communication, May 1991.
.IP [Burrows88]
Michael Burrows, Efficient Data Sharing, Technical Report #153,
Computer Laboratory, University of Cambridge, Dec. 1988.
.IP [Gray89]
Cary G. Gray and David R. Cheriton, Leases: An Efficient Fault-Tolerant
Mechanism for Distributed File Cache Consistency, In \fIProc. of the
Twelfth ACM Symposium on Operating Systems Principals\fR, Litchfield Park,
AZ, Dec. 1989.
.IP [Howard88]
John H. Howard, Michael L. Kazar, Sherri G. Menees, David A. Nichols,
M. Satyanarayanan, Robert N. Sidebotham and Michael J. West,
Scale and Performance in a Distributed File System, \fIACM Trans. on
Computer Systems\fR, (6)1, pg 51-81, Feb. 1988.
.IP [Juszczak89]
Chet Juszczak, Improving the Performance and Correctness of an NFS Server,
In \fIProc. Winter 1989 USENIX Conference,\fR pg. 53-63, San Diego, CA, January 1989.
.IP [Kent87]
Christopher. A. Kent, \fICache Coherence in Distributed Systems\fR, Research Report 87/4,
Digital Equipment Corporation Western Research Laboratory, April 1987.
.IP [Nelson88]
Michael N. Nelson, Brent B. Welch, and John K. Ousterhout, Caching in the
Sprite Network File System, \fIACM Transactions on Computer Systems\fR (6)1
pg. 134-154, February 1988.
.IP [Ousterhout90]
John K. Ousterhout, Why Aren't Operating Systems Getting Faster As Fast as
Hardware? In \fIProc. Summer 1990 USENIX Conference\fR, pg. 247-256, Anaheim,
CA, June 1990.
.IP [Sandberg85]
Russel Sandberg, David Goldberg, Steve Kleiman, Dan Walsh, and Bob Lyon,
Design and Implementation of the Sun Network filesystem, In \fIProc. Summer
1985 USENIX Conference\fR, pages 119-130, Portland, OR, June 1985.
.IP [Schroeder85]
Michael D. Schroeder, David K. Gifford and Roger M. Needham, A Caching
File System For A Programmer's Workstation, In \fIProc. of the Tenth
ACM Symposium on Operating Systems Principals\fR, pg. 25-34, Orcas Island,
WA, Dec. 1985.
.IP [Srinivasan89]
V. Srinivasan and Jeffrey. C. Mogul, \fISpritely NFS: Implementation and
Performance of Cache-Consistency Protocols\fR, Research Report 89/5,
Digital Equipment Corporation Western Research Laboratory, May 1989.
.IP [RFC1094]
Sun Microsystems Inc., \fINFS: Network File System Protocol Specification\fR,
ARPANET Working Group Requests for Comment, DDN Network Information Center,
SRI International, Menlo Park, CA, March 1989, RFC-1094.
