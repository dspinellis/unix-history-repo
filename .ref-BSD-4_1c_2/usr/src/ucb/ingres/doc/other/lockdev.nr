.ds HE 'HOW TO INSTALL CONCURRENCY DEVICE''PAGE %'
.so nmacs
.ce
Instructions for installing the INGRES lock device in UNIX

.ce
December 20, 1977
.sp 3
INGRES version 6.1 has a fully implemented concurrency control
mechanism.
The INGRES system sets and clears locks by writing to
a device called "/dev/lock".
If the device cannot be opened for writing, INGRES
runs without any concurrency control.

Initially it is advisable to bring up INGRES without
the concurrency device.
If planned usage of the INGRES system requires concurrency
control then the device "/dev/lock" must be installed
into your UNIX system.

This document describes the procedure for installing
the UNIX driver for
"/dev/lock".

.sh 1
Log in as root.
.sh 1
Compiling the concurrency device
.PS
.dn
Copy the source and header files
.PS
The concurrency device consists of two
files: .../source/support/ildr.c
and .../source/support/ilock.h

Move the source file to /usr/sys/dmr/ildr.c
and move the header file to /usr/sys/ildr.h

.nf
	mv .../source/support/ildr.c /usr/sys/dmr/ildr.c

	mv .../source/support/ildr.h /usr/sys/ildr.h

.fi
.sh
Installation Parameters
.PS
There are several parameters in ildr.h that can be set by
the user.  These include:

.nf
	NLOCKS - number of locks
	PLOCKS - page locks
	RLOCKS - relation locks
	DLOCKS - data base locks

.fi
NLOCKS is the maximum number of locks permissible.
It determines the size of the lock table.
PLOCKS is the maximum number of page, relation and data base
locks.
RLOCKS is the maximum number of relation and data base locks.
DLOCKS is the maximum number of data base locks.  Only
DLOCKS INGRESes can be active at one time.

The following conditions must hold to protect against
deadlock due to lock table overflow:

.nf
	NLOCKS > PLOCKS > RLOCKS > DLOCKS + 6

.fi
The extran number of RLOCKS are required inorder
to be able to run 6-variable queries.
We recommend first setting DLOCKS and then setting the other parameters
as follows:

.nf
	RLOCKS = 2*DLOCKS + 6
	PLOCKS = RLOCKS + 3
	NLOCKS = PLOCKS + 1
.fi

For example, to reduce the size of the lock table, one could set
DLOCKS = 2, RLOCKS = 10, PLOCKS = 13, NLOCKS = 14.
In this case, only 2 people could be running ingres at one time.

As another example, to allow for more concurrent INGRESes, one could set
DLOCKS = 20, RLOCKS = 46, PLOCKS = 49, NLOCKS = 50.
This scenario would allow for twenty simultaneous INGRES users.

The memory required by the lockdriver is 
1008 bytes in the text segment and
36 + 18 * NLOCKS in the data and BSS segments.

.sh
Change directory
.PS
Change directory to where the device drivers
are kept on your system.

.nf
	chdir /usr/sys/dmr

.fi
.sh
Compile and update the UNIX library
.PS
UNIX object code
is divided
into two libraries, lib1 (the system code)
and lib2 (the device drivers).
Compile ildr.c and put it in lib2

.nf
	cc -c -O ildr.c
	ar rv ../lib2 ildr.o
.fi
.sh 1
Installing device into the UNIX file system.
.PS
To install a device into the UNIX system,
the configuration file must be changed.
.dn
Edit /usr/sys/conf/conf.c ( or c.c )
.PS
Edit the configuration file and add the lock
device to the character device list.
On some systems the configuration file
is called conf.c and on others it is
called c.c.
Find the last device under the "cdevsw[]"
table and add the line:

.nf
	&nulldev,  &nulldev,  &nulldev,  &ilwrite,  &nodev,

.fi
The row number for the device become the major device number.
.ul
Remember to count starting at zero.
.sh
Recompile conf.c (or c.c)
.PS
Recompile the configuration file.

.ti +8
chdir /usr/sys/conf
.ti +8
cc -c conf.c
.sh 1
Updating the system exit() routine.
.PS
.dn
Editing /usr/sys/ken/sys1.c
.PS
The system exit call should be changed to call the
concurrency device.
This is needed in cases when INGRES dies
abnormally,
for example when the process is killed or
when a system error occures.
The call "ilrma(q->p_pid)" should be added to
exit() in /usr/sys/ken/sys1.c
before the label "loop".
The exit code will then look like:

.nf
exit()
{
	register int *q, a;
	register struct proc *p;

	p = u.u_procp;
	p->p_flag =& ~STRC;
	p->p_clktim = 0;
	for(q = &u.u_signal[0]; q < &u.u_signal[NSIG];)
		*q++ = 1;
	for(q = &u.u_ofile[0]; q < &u.u_ofile[NOFILE]; q++)
		if(a = *q) {
			*q = NULL;
			closef(a);
		}
	iput(u.u_cdir);
	xfree();
	a = malloc(swapmap, 1);
	if(a == NULL)
		panic("out of swap");
	p = getblk(swapdev, a);
	bcopy(&u, p->b_addr, 256);
	bwrite(p);
	q = u.u_procp;
	mfree(coremap, q->p_size, q->p_addr);
	q->p_addr = a;
	q->p_stat = SZOMB;
	/*
	 * remove outstanding ingres locks for
	 * the dying process
	 */
	ilrma(q->p_pid);

loop:
	for(p = &proc[0]; p < &proc[NPROC]; p++)
	if(q->p_ppid == p->p_pid) {
		wakeup(&proc[1]);
		wakeup(p);
		for(p = &proc[0]; p < &proc[NPROC]; p++)
		if(q->p_pid == p->p_ppid) {
			p->p_ppid  = 1;
			if (p->p_stat == SSTOP)
				setrun(p);
		}
		swtch();
		/* no return */
	}
	q->p_ppid = 1;
	goto loop;
}
.fi

.sh
Updating /usr/sys/lib1
.PS
Now recompile sys1.c and update lib1:

.nf
	chdir /usr/sys/ken
	cc -c -O sys1.c
	ar rv ../lib1 sys1.o

.fi
.sh 1
Reloading UNIX
.PS
.dn
Reloading the system
.PS
The UNIX system is now ready to be reloaded.
Follow the procedures for your particular
installation.
Make sure the files:

.nf
low.o
m45.o ( or m40.o)
conf.o (or c.o)

.fi
are all present or that the load procedure compiles them.
On the Berkeley 11/70 system the shell file to
do this is /usr/sys/ld:
.nf

	chdir /usr/sys
	sh ld 70

.fi
.sh
Preparing to Reboot.
.PS
You are no ready to move the new unix into
/unix.
Since the namelist has changed,
you should be running single user
before doing this.
.sh
Rebooting UNIX
.PS
Move the new UNIX to the directory
where it is normally booted from:

.nf
	mv /usr/sys/a.out /unix
.fi

Now reboot the system by giving a sync command,
and restarting according to your normal boot
procedures.

.sh 1
Add device to /dev
.PS
.dn
Creating the node.
.PS
The concurrency device must be put in "/dev"
after unix has been rebooted.
Execute the command:

.nf
	/etc/mknod  /dev/lock  c  major  0

.fi
where "major" is the major device number.
Once again, the major device number is the
position of the device in the "cdevsw[]" table.
Remember to start counting from zero.
.sh
Change the mode.
.PS
Now change the mode of the device to be writable by
owner and owned by INGRES:

.nf
	chown ingres /dev/lock
	chmod 200 /dev/lock
.fi
.sh 1
Done.
.PS
INGRES will now use the /dev/lock device for its
concurrency control.
A simple way to verify this is to try running
(for example) "purge" on a data base while
anyone is running "ingres" on the same data base.
