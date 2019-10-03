/*
 * System call switch table.
 *
 * DO NOT EDIT-- this file is automatically @generated.
 * $FreeBSD$
 */

#include <sys/sysent.h>
#include <sys/sysproto.h>
#include <contrib/cloudabi/cloudabi32_types.h>
#include <compat/cloudabi32/cloudabi32_proto.h>

#define AS(name) (sizeof(struct name) / sizeof(register_t))

/* The casts are bogus but will do for now. */
struct sysent cloudabi32_sysent[] = {
	{ AS(cloudabi_sys_clock_res_get_args), (sy_call_t *)cloudabi_sys_clock_res_get, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 0 = cloudabi_sys_clock_res_get */
	{ AS(cloudabi_sys_clock_time_get_args), (sy_call_t *)cloudabi_sys_clock_time_get, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 1 = cloudabi_sys_clock_time_get */
	{ AS(cloudabi_sys_condvar_signal_args), (sy_call_t *)cloudabi_sys_condvar_signal, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 2 = cloudabi_sys_condvar_signal */
	{ AS(cloudabi_sys_fd_close_args), (sy_call_t *)cloudabi_sys_fd_close, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 3 = cloudabi_sys_fd_close */
	{ AS(cloudabi_sys_fd_create1_args), (sy_call_t *)cloudabi_sys_fd_create1, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 4 = cloudabi_sys_fd_create1 */
	{ AS(cloudabi_sys_fd_create2_args), (sy_call_t *)cloudabi_sys_fd_create2, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 5 = cloudabi_sys_fd_create2 */
	{ AS(cloudabi_sys_fd_datasync_args), (sy_call_t *)cloudabi_sys_fd_datasync, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 6 = cloudabi_sys_fd_datasync */
	{ AS(cloudabi_sys_fd_dup_args), (sy_call_t *)cloudabi_sys_fd_dup, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 7 = cloudabi_sys_fd_dup */
	{ AS(cloudabi32_sys_fd_pread_args), (sy_call_t *)cloudabi32_sys_fd_pread, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 8 = cloudabi32_sys_fd_pread */
	{ AS(cloudabi32_sys_fd_pwrite_args), (sy_call_t *)cloudabi32_sys_fd_pwrite, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 9 = cloudabi32_sys_fd_pwrite */
	{ AS(cloudabi32_sys_fd_read_args), (sy_call_t *)cloudabi32_sys_fd_read, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 10 = cloudabi32_sys_fd_read */
	{ AS(cloudabi_sys_fd_replace_args), (sy_call_t *)cloudabi_sys_fd_replace, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 11 = cloudabi_sys_fd_replace */
	{ AS(cloudabi_sys_fd_seek_args), (sy_call_t *)cloudabi_sys_fd_seek, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 12 = cloudabi_sys_fd_seek */
	{ AS(cloudabi_sys_fd_stat_get_args), (sy_call_t *)cloudabi_sys_fd_stat_get, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 13 = cloudabi_sys_fd_stat_get */
	{ AS(cloudabi_sys_fd_stat_put_args), (sy_call_t *)cloudabi_sys_fd_stat_put, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 14 = cloudabi_sys_fd_stat_put */
	{ AS(cloudabi_sys_fd_sync_args), (sy_call_t *)cloudabi_sys_fd_sync, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 15 = cloudabi_sys_fd_sync */
	{ AS(cloudabi32_sys_fd_write_args), (sy_call_t *)cloudabi32_sys_fd_write, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 16 = cloudabi32_sys_fd_write */
	{ AS(cloudabi_sys_file_advise_args), (sy_call_t *)cloudabi_sys_file_advise, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 17 = cloudabi_sys_file_advise */
	{ AS(cloudabi_sys_file_allocate_args), (sy_call_t *)cloudabi_sys_file_allocate, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 18 = cloudabi_sys_file_allocate */
	{ AS(cloudabi_sys_file_create_args), (sy_call_t *)cloudabi_sys_file_create, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 19 = cloudabi_sys_file_create */
	{ AS(cloudabi_sys_file_link_args), (sy_call_t *)cloudabi_sys_file_link, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 20 = cloudabi_sys_file_link */
	{ AS(cloudabi_sys_file_open_args), (sy_call_t *)cloudabi_sys_file_open, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 21 = cloudabi_sys_file_open */
	{ AS(cloudabi_sys_file_readdir_args), (sy_call_t *)cloudabi_sys_file_readdir, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 22 = cloudabi_sys_file_readdir */
	{ AS(cloudabi_sys_file_readlink_args), (sy_call_t *)cloudabi_sys_file_readlink, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 23 = cloudabi_sys_file_readlink */
	{ AS(cloudabi_sys_file_rename_args), (sy_call_t *)cloudabi_sys_file_rename, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 24 = cloudabi_sys_file_rename */
	{ AS(cloudabi_sys_file_stat_fget_args), (sy_call_t *)cloudabi_sys_file_stat_fget, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 25 = cloudabi_sys_file_stat_fget */
	{ AS(cloudabi_sys_file_stat_fput_args), (sy_call_t *)cloudabi_sys_file_stat_fput, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 26 = cloudabi_sys_file_stat_fput */
	{ AS(cloudabi_sys_file_stat_get_args), (sy_call_t *)cloudabi_sys_file_stat_get, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 27 = cloudabi_sys_file_stat_get */
	{ AS(cloudabi_sys_file_stat_put_args), (sy_call_t *)cloudabi_sys_file_stat_put, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 28 = cloudabi_sys_file_stat_put */
	{ AS(cloudabi_sys_file_symlink_args), (sy_call_t *)cloudabi_sys_file_symlink, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 29 = cloudabi_sys_file_symlink */
	{ AS(cloudabi_sys_file_unlink_args), (sy_call_t *)cloudabi_sys_file_unlink, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 30 = cloudabi_sys_file_unlink */
	{ AS(cloudabi_sys_lock_unlock_args), (sy_call_t *)cloudabi_sys_lock_unlock, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 31 = cloudabi_sys_lock_unlock */
	{ AS(cloudabi_sys_mem_advise_args), (sy_call_t *)cloudabi_sys_mem_advise, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 32 = cloudabi_sys_mem_advise */
	{ AS(cloudabi_sys_mem_map_args), (sy_call_t *)cloudabi_sys_mem_map, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 33 = cloudabi_sys_mem_map */
	{ AS(cloudabi_sys_mem_protect_args), (sy_call_t *)cloudabi_sys_mem_protect, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 34 = cloudabi_sys_mem_protect */
	{ AS(cloudabi_sys_mem_sync_args), (sy_call_t *)cloudabi_sys_mem_sync, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 35 = cloudabi_sys_mem_sync */
	{ AS(cloudabi_sys_mem_unmap_args), (sy_call_t *)cloudabi_sys_mem_unmap, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 36 = cloudabi_sys_mem_unmap */
	{ AS(cloudabi32_sys_poll_args), (sy_call_t *)cloudabi32_sys_poll, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 37 = cloudabi32_sys_poll */
	{ AS(cloudabi_sys_proc_exec_args), (sy_call_t *)cloudabi_sys_proc_exec, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 38 = cloudabi_sys_proc_exec */
	{ AS(cloudabi_sys_proc_exit_args), (sy_call_t *)cloudabi_sys_proc_exit, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 39 = cloudabi_sys_proc_exit */
	{ 0, (sy_call_t *)cloudabi_sys_proc_fork, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 40 = cloudabi_sys_proc_fork */
	{ AS(cloudabi_sys_proc_raise_args), (sy_call_t *)cloudabi_sys_proc_raise, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 41 = cloudabi_sys_proc_raise */
	{ AS(cloudabi_sys_random_get_args), (sy_call_t *)cloudabi_sys_random_get, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 42 = cloudabi_sys_random_get */
	{ AS(cloudabi32_sys_sock_recv_args), (sy_call_t *)cloudabi32_sys_sock_recv, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 43 = cloudabi32_sys_sock_recv */
	{ AS(cloudabi32_sys_sock_send_args), (sy_call_t *)cloudabi32_sys_sock_send, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 44 = cloudabi32_sys_sock_send */
	{ AS(cloudabi_sys_sock_shutdown_args), (sy_call_t *)cloudabi_sys_sock_shutdown, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 45 = cloudabi_sys_sock_shutdown */
	{ AS(cloudabi32_sys_thread_create_args), (sy_call_t *)cloudabi32_sys_thread_create, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 46 = cloudabi32_sys_thread_create */
	{ AS(cloudabi_sys_thread_exit_args), (sy_call_t *)cloudabi_sys_thread_exit, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 47 = cloudabi_sys_thread_exit */
	{ 0, (sy_call_t *)cloudabi_sys_thread_yield, AUE_NULL, NULL, 0, 0, SYF_CAPENABLED, SY_THR_STATIC },	/* 48 = cloudabi_sys_thread_yield */
};
