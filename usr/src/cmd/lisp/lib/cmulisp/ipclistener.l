

(dv ipclistenerfns (IPCListener))

(def IPCListener
  (lambda nil
    (setq message (IPCMsgAlloc))
    (setq receive_port (IPCAllocatePort 1))
    (IPCAssertName 'larswe receive_port)
    (setq descr (array descr fixnum 100))
    (setq data (array data fixnum 100))
    (setq ls (IPCLSAlloc))
    (prog nil
     loop (IPCInitLSMsg receive_port
                        'PT
                        0
                        'PT
                        -10
                        'NORMALMSG
                        message
                        ls
                        descr
                        data)
          (setq received (IPCReceive -1 message 10000))
          (cond ((eq received t)
                 (princ (concat '"[" (IPCGetID message) '"]"))
                 (princ (IPCSExprExtract message))
                 (terpri))
                (t (princ '"Timeout. ") (princ received) (terpri)))
          (go loop))))
