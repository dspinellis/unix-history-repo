/*-
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 */

#include <stdio.h>
#include <kvm.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/kinfo.h>
#include <sys/kinfo_proc.h>
#include <machine/vmparam.h>

#ifndef offsetof
#define	offsetof(s, f) ((int)&((s *)0)->f)
#endif

static void
shift_page(fd, off, ssize)
	register int fd;
	register off_t off;
	register int ssize;
{
	char buffer[NBPG];

	(void)lseek(fd, -NBPG, SEEK_END);
	for (; ssize > 0; ssize -= NBPG) {
		read(fd, buffer, NBPG);
		write(fd, buffer, NBPG);
		(void)lseek(fd, -2 * NBPG, SEEK_CUR);
	}
}

/*
 * Fix up the core image for the sparc.  We need to flush any register
 * windows that are cached in the pcb out to the user stack.
 * Also, we need to get the trap frame and possible floating point state
 * from the top of the kernel stack and store it in the pcb.
 */
int
md_core(kd, fd, ki)
	kvm_t *kd;
	int fd;
	struct kinfo_proc *ki;
{
	register struct rwindow *rw;
	register int nsaved, cc, ssize;
	register off_t off, s;
	register u_long sp;
	struct pcb pcb;
	struct trapframe tf;

	/*
	 * Before anything else read the trapframe.  Synchronizing here
	 * is impossible if the process is running.
	 */
	cc = kvm_read(kd, (u_long)ki->kp_proc.p_md.md_tf,
		      /* XXX */
		      (void *)&tf, sizeof(tf));
	if (cc < 0)
		error("kvm_read: %s (reading kernel trapframe)",
		      kvm_geterr(kd));
	if (cc != sizeof(tf))
		error("cannot read kernel trapframe");

	/*
	 * Write out the real trap frame.
	 */
	off = offsetof(struct user, u_md);
	off += offsetof(struct md_coredump, md_tf);
	if (lseek(fd, off, SEEK_SET) == -1) {
		perror("lseek");
		exit(1);
	}
	(void)write(fd, (char *)&tf, sizeof(tf));

	if (ki->kp_proc.p_md.md_fpstate != 0) {
		/*
		 * If floating point state is present, write it out too.
		 * It comes right after the trapframe so we don't need to seek.
		 */
		struct fpstate fs;
		cc = kvm_read(kd, (u_long)ki->kp_proc.p_md.md_fpstate,
			      (void *)&fs, sizeof(fs));
		if (cc < 0)
			error("kvm_read: %s (fpu state)", kvm_geterr(kd));
		if (cc != sizeof(fs))
			error("cannot read fpu state");
		(void)write(fd, (char *)&fs, sizeof(fs));
	}
	/*
	 * Read pcb.
	 */
	if (lseek(fd, offsetof(struct user, u_pcb), SEEK_SET) == -1) {
		perror("lseek");
		exit(1);
	}
	cc = read(fd, (char *)&pcb, sizeof(pcb));
	if (cc < 0) {
		perror("read");
		exit(1);
	}
	if (cc != sizeof(pcb))
		error("couldn't read pcb from core file");

	/*
	 * Write any unsaved windows to the appropriate stack locations.
	 */
	nsaved = pcb.pcb_nsaved;
	if (nsaved == 0)
		return (0);

	rw = &pcb.pcb_rw[0];
	off = ctob(UPAGES + ki->kp_eproc.e_vm.vm_dsize);
	ssize = ctob(ki->kp_eproc.e_vm.vm_ssize);
	sp = tf.tf_out[6];
	for (; --nsaved >= 0; ++rw) {
		/*
		 * Copy register window into appropriate stack location.
		 */
		s = ssize - (USRSTACK - sp);
		if (s < 0) {
			if (s < -NBPG)
				error("cannot copy pcb windows to stack");
			/*
			 * It's possible to be missing the bottomost
			 * page because a stack page hasn't been allocated
			 * for the register save area.  Shift over
			 * the stack segment by a page, and update 
			 * the u-area to reflect the new stack size.  YECH!
			 */
			shift_page(fd, off, ssize);
			ssize += NBPG;
			s += NBPG;
			++ki->kp_eproc.e_vm.vm_ssize;
			(void)lseek(fd, offsetof(struct user,
						 u_kproc.kp_eproc.e_vm),
				    SEEK_SET);
			(void)write(fd, (char *)&ki->kp_eproc.e_vm, 
				    sizeof(ki->kp_eproc.e_vm));
		}
		if (lseek(fd, off + s, SEEK_SET) == -1)
			error("cannot copy pcb windows to stack");

		write(fd, (char *)rw, sizeof(*rw));
		sp = rw->rw_in[6];
	}
	return (0);
}

