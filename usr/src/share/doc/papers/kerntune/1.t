.\"	@(#)1.t	1.1	(Copyright 1984 M. K. McKusick)	84/05/21
.ds RH Introduction
.NH 1
Introduction
.PP
The purpose of this paper is to describe the tools and techniques 
that are available for improving the performance of the the kernel.
The primary tool used to measure the kernel is the hierarchical
profiler \fIgprof\fP.
The profiler enables the user to measure the cost of
the abstractions that the kernel provides to the user.
Once the expensive abstractions are identified,
optimizations are postulated to help improve their performance.
These optimizations are each individually
verified to insure that they are producing a measurable improvement.
