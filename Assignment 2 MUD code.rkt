#lang racket

;;Scheme Request for Implementation
(require srfi/1) ;; This srfi works with pairs and lists
(require srfi/13) ;; This srfi works with String Libraries
(require srfi/48) ;; Intermediate Format Strings

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (command) ;; To execute the function
  (let* ((input (read-line)) ;; Allows user input
         (string-tokens (string-tokenize input)) ;; converting an entire string into a list of strings
         (tokens (map string->symbol string-tokens)) ;; map strings into a list
         (cmd (car tokens))) ;; Read the first string token
    (cond
      ;; explain the commands
      ((eq? cmd 'help)
       (format #t "Usage:\nsearch <term>\nquit\n"))
      ;; break the loop
      ((eq? cmd 'quit)
       (exit ))
      ;; do something
      ((eq? cmd 'search)
       (format #t "Searching for ~a ...\n" (cadr tokens)))
      ;; handle unknown input
      (else
       (format #t "Huh ?\n"))))
  (command)) ;; To execute the command

;; Creating the objects
(define objects '((1 "a silver dagger")
                  (1 "a gold coin")
                  (2 "a silver key")
                  (3 "a garage key")
                  (3 "a shoe")
                  (4 "a screwdriver")
                  (4 "a torch")
                  (5 "a racket")
                  (8 "a notebook")
                  (10 "a basement key")))

(define rooms '((1 "(lobby)")
                (2 "(hallway)")
                (3 "(swamp)")
                (4 "(garage)")
                (5 "(courtyard)")
                (6 "(garden)")
                (7 "(basement)")
                (8 "(bedroom)")
                (9 "(kitchen)")
                (10 "(livingroom)")))

;; Describe the room 'descriptions' within as association list
(define description '((1 "You are in the lobby.")
                      (2 "You are in the hallway.")
                      (3 "You are in a swamp.")
                      (4 "You are in the garage.")
                      (5 "You are in the courtyard.")
                      (6 "You are in the garden.")
                      (7 "You are in the basement.")
                      (8 "You are in the bedroom.")
                      (9 "You are in the kitchen.")
                      (10 "You are in the livingroom.")))

;; Define some actions which will include into our decisiontable structure
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit) ((end) quit)))
(define pick '(((get) pick) ((pickup) pick) ((retrieve) pick) ((pick) pick) ((obtain) pick)))
(define drop '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop) ((release) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory) ((items) inventory)))

;; Quasiquote is used to apply special properties
;; Unquote-splicing is used because we need to remove the extra list that would be generated had we just used unquote
(define actions `(,@look ,@quit ,@pick ,@drop ,@inventory))

(define decisiontable `((1 ((north) 2) ((north west) 8) ((north east) 9) ,@actions)
                        (2 ((south) 1) ((north) 7) ((west) 10) ,@actions)
                        (3 ((east) 6) ((north) 5) ,@actions)
                        (4 ((south west) 5) ((west) 7) ,@actions)
                        (5 ((south) 3) ((north east) 4) ((west) 6) ,@actions)
                        (6 ((west) 3) ((east) 5) ,@actions)
                        (7 ((south) 2) ((west) 4) ,@actions)
                        (8 ((south east) 1) ((north) 10) ((east) 9) ,@actions)
                        (9 ((south west) 1) ((west) 8) ((east) 10) ,@actions)
                        (10 ((south) (8) ((west) 9) ((east) 2) ,@actions))))


;; a number of hash tables are defined in order to support the mutable data structure
;; that will contain our reference to the objects available in the rooms and the user inventory
(define objectdb (make-hash))
(define inventorydb (make-hash))

(define (add-object db id object)
  ;; checks if hash key exists
  (if (hash-has-key? db id)
      ;; if exist,
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

;; use the for-each function to add every single item in the assoc list to the objectdb using
;; the add-object function.
;; (first r) returns the index of the oibject assoc list room
;; (second r) returns the item of the objects assoc list room
(define (add-objects db)
  (for-each
   (λ (r)
     (add-object db (first r) (second r))) objects))

;; populate objectdb with the items provided in the object
(add-objects objectdb)

;; Displays the objects that are in the room and/or in our inventory
(define (display-objects db id)
  (when (hash-has-key? db id)
    ;; join string if multiple items
    (let* ((record (hash-ref db id))
           (output (string-join record " and " )))
      ;; depending on the parameters, provide different answer
      (when (not (equal? output "" ))
        (if (eq? id 'inventory)
            (printf "You are carrying ~a.\n" output)
            (printf "You can see ~a.\n" output))))))




;; This function examines the data for a single room and looks for direction entries, e.g ((north) 2)
;; It does this by checking for a number at the second position of each list
;; The outputted result is not user friendly at the moment. The list needs to be converted back into strings
(define (get-directions id)
  ;; List decisiontable assigns the id of the decisiontable searched to record
  (let* ((record (assq id decisiontable))
         (assign (assv id rooms)))
    (let* ((result (filter (λ (n) (number? (second n))) (cdr record)))
           (n (length result)))
      ;; Conditions to check the directions
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits. \n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* (;;(losym (map (λ (x) (car x)) result))
                   ;; (lojnt (map (λ (x) (being (cons (car x) ) result
                   ;; (lonum (map (λ (x) (second x)) result))
                    (losym (map (λ (x) (begin (cons (car x) (cdr (assv (second x) rooms))))) result)) 
                    (lostr (map (λ (x)
                                  (string-join (list (symbol->string (caar x))
                                                     (second x) ""))) losym)))   
                    ;;(lostr (map (λ (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr "and "))))))))
;;(for-each (λ (direction) (printf "~a" (first direction))) result))
;;(printf "\n")))

;; Maps the paramter to the list of atoms then joins it
(define (slist->string l)
  (string-join (map symbol->string l)))

(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))

(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))

(define (display-inventory)
  (display-objects inventorydb 'bag))

(define (remove-object-from-room db id str )
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           ;; result is the list of items in the room
           (result (remove (λ (x) (string-suffix-ci? str x)) record))
           ;; item is the difference with the previous list e.g. the item collected
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "I don’t see that item in the room!\n"))
            (else
             (printf "Added ~a to your bag.\n" (first item))
             (add-object inventorydb 'bag (first item))
             ;; we are the original databse with the result
             (hash-set! db id result))))))

(define (remove-object-from-inventory db id str)
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           (result (remove (λ (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "You are not carrying that item !\n"))
            (else
             (printf "Removed ~a from your bag.\n" (first item))
             (add-object objectdb id (first item))
             (hash-set! db 'bag result))))))

(define responses
  '((1 "Hello how are you")
    (2 "Want me to cheer you up?")
    (3 "Do you want to watch a movie?")
    (4 "What genre would you like to watch?")
    (5 "Do you not like movies?")
    (6 "Good choice, want me to look for films?")
    (7 "Shall I recommend a gory film for you?")
    (8 "Shall I recommend a non-gory scary film for you?")))

;; Returns a string based on the given id
(define (get-response id)
  (car (assq-ref responses id))) ;; provides the string associated with the number you provide

;; Generates a keyword list based on the id given
(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id))) ;; lets the assq-ref of the id we choose in decision table hold into 'keys'
    (map
     (λ (key) (car key)) keys))) ;; maps the car of the keys into key. The car of ((comedy) 20) is (comedy).


;; A wrapper function which allows us to supply an id and list of tokens
;; Returns the matching id
(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id)) ;; Let the record hold the tokens in decisiontable with a id you provide
         (keylist (get-keywords id)) ;; keylist holds the keywords from given id
         ;; holds the index largest number from list-of-lengths into index
         (index (index-of-largest-number (list-of-lengths keylist tokens)))) 
    (if index
        ;; lists the cadr of the id of the decisiontable and its index given.
        (cadr (list-ref record index))
        #f)))

;; Returning the position of the largest value in our list
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
        #f
        (list-index (λ (x) (eq? x n)) list-of-numbers))))

;; Accepts a keyword list and does a match against a list of tokens
;; Outputs the list in the form: (0 0 0 2 0)
(define (list-of-lengths keylist tokens)
  (map
   (λ (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (get-description id)
  (car (assq-ref description id)))

(define (display-description id)
  (printf "~a\n" (get-description id)))

(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (when description
      (display-description id)
      (display-objects objectdb id))
    (printf "> ")
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (printf "huh? I didn't understand that!\n")
               (loop id #f))
              ((eq? response 'look)
               (get-directions id)
               (loop id #t))
              ((eq? response 'pick)
               (pick-item id input)
               (loop id #f))
              ((eq? response 'drop)
               (put-item id input)
               (loop id #f))
              ((eq? response 'inventory)
               (display-inventory)
               (loop id #f))              
              ((eq? response 'quit)
               (printf "So Long, and Thanks for All the Fish...\n")
               (exit)))))))

(define (recommend initial-id)
  (let loop ((id initial-id))
    (format #t "~a \n > " (get-response id))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((eq? #f response)
               (format #t "huh? I didn't understand that! ")
               (loop id))
              ((eq? 'gory response )
               (format #t "Searching for gory horror films ....\n ")
               (exit ))
              ((eq? 'non-gory response )
               (format #t "Searching for non-gory scary films ....\n ")
               (exit ))
              ((zero? response )
               (format #t "Okay bye 👋 ...\n")
               (exit))
              (else
               (loop response)))))))
