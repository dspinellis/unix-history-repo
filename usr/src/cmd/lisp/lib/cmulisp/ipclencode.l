

(dv ipclencodefns
    ((declare (special descr_index data_index))
     GetType
     IPCGetDNumElts
     IPCGetDOffset
     IPCGetDType
     IPCInitLSMsg
     IPCPutDNumElts
     IPCPutDOffset
     IPCPutDType
     IPCPutLSDescriptor
     IPCSExprConstruct
     IPCSExprDecode
     IPCSExprDecode1
     IPCSExprEncode
     IPCSExprExtract
     PutType))

(declare (special descr_index data_index))

(def GetType
  (lambda (index array)
    (cdr
     ((lambda (type_atom)
              (cond ((not type_atom)
                     (err '|Bad type code encountered by GetType|))
                    (type_atom)))
      (assq (GetInteger index array)
            '((0 . UNSTRUCTURED)
              (1 . LINEARSTRUCTURE)
              (2 . INTEGER)
              (3 . PTOWNERSHIP)
              (4 . PTRECEIVERIGHTS)
              (5 . PTALLRIGHTS)
              (6 . PT)
              (7 . DESCRIPTOR)
              (8 . CHAR)
              (9 . LIST)
              (10 . NIL)
	      (11 . REAL]

(def IPCGetDNumElts
  (lambda (index array)
    (GetInteger (|1+| (times 3 index)) array)))

(def IPCGetDOffset
  (lambda (index array)
    (GetInteger (plus 2 (times 3 index)) array)))

(def IPCGetDType
  (lambda (index array)
    (GetType (times 3 index) array)))

(def IPCInitLSMsg
  (lambda (locpt ltyp rempt rtyp id mtyp msg ls descr data)
    (IPCSetLocalPort locpt msg)
    (IPCSetLPType ltyp msg)
    (IPCSetRemotePort rempt msg)
    (IPCSetRPType rtyp msg)
    (IPCSetID id msg)
    (IPCSetNumElts 1 msg)
    (IPCSetMsgType mtyp msg)
    (IPCSetType 'LINEARSTRUCTURE msg)
    (IPCSetDataPtr ls msg)
    (IPCSetLSPtr descr ls)
    (IPCSetLSDataPtr data ls)
    (IPCSetLSSize (times (getlength descr) (getdelta descr)) ls)
    (IPCSetLSDataSize (times (getlength data) (getdelta data)) ls)))

(def IPCPutDNumElts
  (lambda (index num_elts array)
    (PutInteger (|1+| (times 3 index)) num_elts array)))

(def IPCPutDOffset
  (lambda (index offset array)
    (PutInteger (plus 2 (times 3 index)) offset array)))

(def IPCPutDType
  (lambda (index type array)
    (PutType (times 3 index) type array)))

(def IPCPutLSDescriptor
  (lambda (index dtype dnumelts doffset array)
    (IPCPutDType index dtype array)
    (IPCPutDNumElts index dnumelts array)
    (IPCPutDOffset index doffset array)
    t))

(def IPCSExprConstruct
  (lexpr (nargs)
    ((lambda (x locpt ltyp rempt rtyp id mtyp msg ls descr data port_atom)
             (IPCInitLSMsg locpt ltyp rempt rtyp id mtyp msg ls descr data)
             (cond (port_atom (IPCSExprEncode x descr data 0 0 port_atom))
                   ((IPCSExprEncode x descr data 0 0))))
     (arg 1)
     (arg 2)
     (arg 3)
     (arg 4)
     (arg 5)
     (arg 6)
     (arg 7)
     (arg 8)
     (arg 9)
     (arg 10)
     (arg 11)
     (cond ((greaterp nargs 11) (arg 12))))))

(def IPCSExprDecode
  (lambda (descr data descr_index data_index)
    (IPCSExprDecode1 descr data)))

(def IPCSExprDecode1
  (lambda (descr data)
    ((lambda (data_type)
             (cond ((member data_type
                            '(REAL INTEGER
                                   CHAR
                                   NIL
                                   PT
                                   PTOWNERSHIP
                                   PTRECEIVERIGHTS
                                   PTALLRIGHTS))
                    (cond ((eq data_type 'REAL)
                           (prog1 (GetReal data_index data)
                                  (setq data_index (plus 2 data_index))
                                  (setq descr_index (|1+| descr_index))))
                          ((member data_type
                                   '(INTEGER PT
                                             PTOWNERSHIP
                                             PTRECEIVERIGHTS
                                             PTALLRIGHTS))
                           (prog1 (GetInteger data_index data)
                                  (setq data_index (add1 data_index))
                                  (setq descr_index (|1+| descr_index))))
                          ((eq data_type 'NIL)
                           (setq descr_index (|1+| descr_index))
                           nil)
                          ((eq data_type 'CHAR)
                           (prog1 (GetAtom1 (times 4 data_index) data)
                                  (setq data_index
                                        (plus data_index
                                              (car
                                               (Divide (plus (IPCGetDNumElts descr_index
                                                                             descr)
                                                             3)
                                                       4))))
                                  (setq descr_index (|1+| descr_index))))))
                   ((eq data_type 'LIST)
                    (prog (result temp num_elts)
                          (setq num_elts (IPCGetDNumElts descr_index descr))
                          (setq descr_index (|1+| descr_index))
                          (setq result
                                (cons (IPCSExprDecode1 descr data) nil))
                          (setq temp result)
                          (setq num_elts (|1-| num_elts))
                     loop (cond ((eq num_elts 1)
                                 (rplacd temp (IPCSExprDecode1 descr data))
                                 (return result))
                                (t (rplacd temp
                                           (cons (IPCSExprDecode1 descr
                                                                  data)
                                                 nil))
                                   (setq temp (cdr temp))
                                   (setq num_elts (|1-| num_elts))
                                   (go loop)))))))
     (IPCGetDType descr_index descr))))

(def IPCSExprEncode
  (lexpr (args)
    ((lambda (x descr data descr_index data_index port_atom)
             (cond ((or (atom x) (and (car x) (equal (car x) port_atom)))
                    (cond ((floatp x)
                           (IPCPutLSDescriptor descr_index
                                               'REAL
                                               1
                                               data_index
                                               descr)
                           (PutReal data_index x data)
                           (cons (|1+| descr_index) (plus 2 data_index)))
                          ((dtpr x)
                           (IPCPutLSDescriptor descr_index
                                               (cadr x)
                                               1
                                               data_index
                                               descr)
                           (PutInteger data_index (caddr x) data)
                           (cons (|1+| descr_index) (|1+| data_index)))
                          ((fixp x)
                           (IPCPutLSDescriptor descr_index
                                               'INTEGER
                                               1
                                               data_index
                                               descr)
                           (PutInteger data_index x data)
                           (cons (|1+| descr_index) (|1+| data_index)))
                          ((not x)
                           (IPCPutLSDescriptor descr_index
                                               'NIL
                                               0
                                               data_index
                                               descr)
                           (cons (|1+| descr_index) data_index))
                          ((symbolp x)
                           ((lambda (atom_length)
                                    (IPCPutLSDescriptor descr_index
                                                        'CHAR
                                                        atom_length
                                                        data_index
                                                        descr)
                                    (cons (|1+| descr_index)
                                          (plus (car
                                                 (Divide (plus atom_length
                                                               3)
                                                         4))
                                                data_index)))
                            (PutAtom1 (times 4 data_index) x data)))
                          (t
                           (err
                            (concat '|Unknown type to IPCSExprEncode -- |
                                    (type x))))))
                   ((dtpr x)
                    (IPCPutLSDescriptor descr_index
                                        'LIST
                                        (do ((num_elts 0 (|1+| num_elts))
                                             (list x (cdr list)))
                                            ((atom list) (|1+| num_elts)))
                                        data_index
                                        descr)
                    (prog (index_list)
                          (setq index_list
                                (cons (|1+| descr_index) data_index))
                     loop (cond
                           ((atom x)
                            (return
                             (IPCSExprEncode x
                                             descr
                                             data
                                             (car index_list)
                                             (cdr index_list)
                                             port_atom))))
                          (setq index_list
                                (IPCSExprEncode (car x)
                                                descr
                                                data
                                                (car index_list)
                                                (cdr index_list)
                                                port_atom))
                          (setq x (cdr x))
                          (go loop)))
                   (t
                    (err
                     (concat '|Unknown type to IPCSExprEncode -- |
                             (type x))))))
     (arg 1)
     (arg 2)
     (arg 3)
     (arg 4)
     (arg 5)
     (cond ((greaterp args 5) (arg 6))))))

(def IPCSExprExtract
  (lambda (message)
    ((lambda (LS) (IPCSExprDecode (IPCGetLSPtr LS) (IPCGetLSDataPtr LS) 0 0))
     (IPCGetDataPtr message))))

(def PutType
  (lambda (index type array)
    ((lambda (type_integer)
             (cond ((not type_integer) (err '|Bad type atom to PutType|))
                   ((PutInteger index type_integer array))))
     (cdr
      (assq type
            '((UNSTRUCTURED . 0)
              (LINEARSTRUCTURE . 1)
              (INTEGER . 2)
              (PTOWNERSHIP . 3)
              (PTRECEIVERIGHTS . 4)
              (PTALLRIGHTS . 5)
              (PT . 6)
	      (DESCRIPTOR . 7)
              (CHAR . 8)
              (LIST . 9)
              (NIL . 10)
              (REAL . 11)))))))
