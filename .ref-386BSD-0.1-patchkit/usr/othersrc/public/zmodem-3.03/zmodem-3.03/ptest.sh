#a short test for sz and rz using a named pipe - no modem used.
/etc/mknod fifo p
sz <fifo /etc/motd |rz >fifo
rm fifo
