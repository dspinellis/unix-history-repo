rp03:
	/etc/mknod rp0 b 1 1
	/etc/mknod swap b 1 2
	/etc/mknod rp3 b 1 3
	/etc/mknod rrp0 c 11 1
	/etc/mknod rrp3 c 11 3
	chmod go-w rp0 swap rp3 rrp0 rrp3

rp04 rp05:
	/etc/mknod rp0 b 6 0
	/etc/mknod swap b 6 1
	/etc/mknod rp3 b 6 6
	/etc/mknod rrp0 c 14 0
	/etc/mknod rrp3 c 14 6
	chmod go-w rp0 swap rp3 rrp0 rrp3

rp06:
	/etc/mknod rp0 b 6 0
	/etc/mknod swap b 6 1
	/etc/mknod rp3 b 6 7
	/etc/mknod rrp0 c 14 0
	/etc/mknod rrp3 c 14 7
	chmod go-w rp0 swap rp3 rrp0 rrp3

tm:
	/etc/mknod mt0 b 3 0
	/etc/mknod rmt0 c 12 0
	/etc/mknod nrmt0 c 12 128
	chmod go+w mt0 rmt0 nrmt0

ht:
	/etc/mknod mt0 b 7 64
	/etc/mknod mt1 b 7 0
	/etc/mknod rmt0 c 15 64
	/etc/mknod rmt1 c 15 0
	/etc/mknod nrmt0 c 15 192
	/etc/mknod nrmt1 c 15 128
	chmod go+w mt0 mt1 rmt0 rmt1 nrmt0 nrmt1
