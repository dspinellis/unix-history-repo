# Copyright (c) 1979 Regents of the University of California
#
# sccsid[] = "@(#)ashdr.s 1.1 10/29/80";
#
# Global constants
#
	.set	R6,0x040
	.set	R7,0x080
	.set	R8,0x100
	.set	R9,0x200
	.set	R10,0x400
	.set	R11,0x800
	.set	PSW,4
	.set	AP,8
	.set	FP,12
	.set	PC,16
#
# error codes
#
	.set	EARGV,1
	.set	EASRT,2
	.set	EBADFNUM,3
	.set	EBADINUM,4
	.set	ECASE,5
	.set	ECHR,6
	.set	ECLOSE,7
	.set	ECREATE,8
	.set	ECTLWR,9
	.set	ECTSNG,10
	.set	ECTUPR,11
	.set	EFMTSIZE,12
	.set	EGOTO,13
	.set	EHALT,14
	.set	ELLIMIT,15
	.set	ELN,16
	.set	ENAMESIZE,17
	.set	ENAMRNG,18
	.set	ENARGS,19
	.set	ENILPTR,20
	.set	ENOFILE,21
	.set	ENUMNTFD,22
	.set	EOPEN,23
	.set	EOUTOFMEM,24
	.set	EPACK,25
	.set	EPASTEOF,26
	.set	ERANGE,27
	.set	EREADIT,28
	.set	EREFINAF,29
	.set	EREMOVE,30
	.set	ESEEK,31
	.set	ESQRT,32
	.set	ESTLIM,33
	.set	ESUBSC,34
	.set	EUNPACK,35
	.set	EWRITE,36
	.set	EWRITEIT,37
