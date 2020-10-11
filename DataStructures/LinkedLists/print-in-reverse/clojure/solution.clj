;;; solution

(definterface ISinglyLinkedListNode
  (getdata [])
  (getnext [])
  (setdata [node-data])
  (setnext [next-ptr]))

(deftype SinglyLinkedListNode [^:volatile-mutable data ^:volatile-mutable next]

  ISinglyLinkedListNode
  (getdata [_] data)
  (getnext [_] next)
  (setdata [_ node-data] (set! data node-data))
  (setnext [_ next-ptr] (set! next next-ptr)))

(definterface ISinglyLinkedList
  (gethead [])
  (gettail [])
  (sethead [list-head])
  (settail [list-tail])
  (insertnode [node-data]))

(deftype SinglyLinkedList [^:volatile-mutable head ^:volatile-mutable tail]
  ISinglyLinkedList

  (gethead [_] head)
  (gettail [_] tail)
  (sethead [_ list-head] (set! head list-head))
  (settail [_ list-tail] (set! tail list-tail))

  (insertnode [this node-data] [
                                (def node (SinglyLinkedListNode. node-data nil))
                                (if head [(.setnext (.gettail this) node)] (.sethead this node))
                                (.settail this node)
                                ])
  )

(defn print-singly-linked-list [node sep]
  (when node
    [
     (print (.getdata node))

     (when (.getnext node) (print sep))

     (print-singly-linked-list (.getnext node) sep)
     ]
    )
  )

                                        ; Complete the reversePrint function below.


                                        ;
                                        ; For your reference:
                                        ;
                                        ; SinglyLinkedListNode [
                                        ;     ^:volatile-mutable data
                                        ;     ^:volatile-mutable next
                                        ; ]
                                        ;
                                        ;
(defn reversePrint [head]
  (when (.getnext head)
    (reversePrint (.getnext head)))
  (println (str (.getdata head)))
  )


;;; I have wrapped Hackerrank code with commented MY INSERTIONS
;;; ran M-x cider-jack-in and in repl did (load-file "./solution.clj")
(binding [*in*  (clojure.java.io/reader "./input0.txt") ] ;;; MY INSERTIONS start

  (def tests (Integer/parseInt (clojure.string/trim (read-line))))

  (doseq [tests-itr (range tests)]
    (def llist-count (Integer/parseInt (clojure.string/trim (read-line))))

    (def llist (SinglyLinkedList. nil nil))

    (doseq [_ (range llist-count)]
      (.insertnode llist (Integer/parseInt (read-line)))
      )

    (reversePrint (.gethead llist))
    )


  ) ;;; MY INSERTIONS end
