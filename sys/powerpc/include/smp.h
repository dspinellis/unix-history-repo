/*-
 * Copyright (c) 2008 Marcel Moolenaar
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $FreeBSD$
 */

#ifndef _MACHINE_SMP_H_
#define _MACHINE_SMP_H_

#ifdef _KERNEL

#define	IPI_AST			0
#define	IPI_PREEMPT		1
#define	IPI_RENDEZVOUS		2
#define	IPI_STOP		3

#define	IPI_PPC_TEST		4

#ifndef LOCORE

void	ipi_all(int ipi);
void	ipi_all_but_self(int ipi);
void	ipi_selected(cpumask_t cpus, int ipi);
void	ipi_self(int ipi);

struct cpuref {
	uintptr_t	cr_hwref;
	u_int		cr_cpuid;
};

int	powerpc_smp_first_cpu(struct cpuref *);
int	powerpc_smp_get_bsp(struct cpuref *);
int	powerpc_smp_next_cpu(struct cpuref *);
int	powerpc_smp_start_cpu(struct pcpu *);

void	pmap_cpu_bootstrap(volatile uint32_t *, int);
uint32_t cpudep_ap_bootstrap(volatile uint32_t *);
void	machdep_ap_bootstrap(volatile uint32_t *);

#endif /* !LOCORE */
#endif /* _KERNEL */
#endif /* !_MACHINE_SMP_H */
