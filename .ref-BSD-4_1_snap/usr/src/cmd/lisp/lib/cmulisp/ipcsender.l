

(dv ipcsenderfns (IPCSender))

(def IPCSender
  (lambda nil
    (setq message (IPCMsgAlloc))
    (setq ls (IPCLSAlloc))
    (setq descr (array descr fixnum 100))
    (setq data (array data fixnum 100))
    (setq localport (IPCAllocatePort 1))
    (setq remoteport (IPCLocate 'larswe))
    (cond (remoteport (print '"Located listener.") (terpri)))
    (setq local_port_type 'PT)
    (setq remote_port_type 'PT)
    (setq i 0)
    (prog nil
     loop (setq i (|1+| i))
          (princ 'Send>)
          (setq input (read))
          (princ (concat '"Sending message " i '"."))
          (terpri)
          (IPCSExprConstruct input
                             localport
                             local_port_type
                             remoteport
                             remote_port_type
                             i
                             'NORMALMSG
                             message
                             ls
                             descr
                             data)
          (IPCSend message 0 'WAIT)
          (go loop))))
