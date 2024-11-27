#lang racket/gui
#|
Author: Jay Yang - M01006234
Course: BSc CompSci Year 1 '24-'25
Module: Programming

Project Details: 
|#



(require web-server/servlet
         web-server/servlet-env)



#|--------------------------------------------------------------------- Start: Build Database From Files --------------------------------------------------------------------------------|#
#|
Initialize Program - Load Database
Returns a List of Structs. Structs are appropriate for each relation.
This function (and program) was written in a way such that "(" and ")" mark the beginning and end of names containing spaces. 
I'll be honest, I knew what I was doing, and the function does what I intended line-by-line. But I also bruteforced my way through it.
There is probably a better way to do this with  maybe some built-in racket functions. But I could not find documentation, so I just through up
of my own way to do it.

Data in text files are expected to have followed all data type/string patterns. 
|#
(define import-data (λ (copy type)
                      (define buildList '()) ; list to be returned
                      (define buildString "")
                      (for ((line copy))
                        (set! buildString "") ; holds the string of each column
                        (define a "") ; holds first column
                        (define b "") ; holds second column
                        (define c "") ; and so on.
                        (define d "")
                        (define e "")
                        (define hold "") ; holds the string within "(" and ")"
                        (define help "") ; holds the column AFTER a ")"
                        (define count 0) ; counts occurences of \space to track columns
                        (define countSpaceinParen 0) ;; Needed this to count the spaces in () of text file.
                        (for ((i (string-length line)))
                          (cond

                            ((equal? (string-ref line i) #\()
                             (define getPosition 0)
                             (for ((char (substring line (add1 i) (sub1 (string-length line)))))
                               #:final (equal? char #\))
                               (set! getPosition (add1 getPosition))
                               (cond
                                 ((not (equal? char #\)))
                                  (cond
                                    ((equal? char #\space) (set! countSpaceinParen (add1 countSpaceinParen))) ;; need to count spaces for the other space counter below (lines up correct columns)
                                    )
                                  (set! hold (string-append hold (string char)))
                                  )
                                 ((and (equal? char #\)) (equal? type "event"))
                                  (for ((char2 (substring line (+ getPosition i 2) (string-length line)))) ; it just kind of magically worked. I suspect it has to be written this way because I did (add1 i) on line 30. 
                                    #:break (equal? char2 #\space)
                                    (set! help (string-append help (string char2)))
                                       )
                                  )
                                 )
                               )
                             )   
                            
                            ((equal? (string-ref line i) #\space)
                             (set! count (add1 count))
                             (cond
                               ((= count 1)
                                (set! a buildString))
                               ((= count 2)
                                (set! b buildString))
                               ((= count 3)
                                (set! c buildString))
                               ((= count 4)
                                (set! d buildString))
                               ((= count (+ 5 countSpaceinParen))
                                (set! e buildString))
                               )
                             (set! buildString "")
                             )
                            
                            ((= i (sub1 (string-length line))) ;; when loop has iterated through whole line
                             
                             (cond
                               ((equal? type "fan")
                                (set! e (string-append buildString (string (string-ref line i))))
                                (set! buildList (append buildList
                                                        (list
                                                         (fanAccount a b c d e)
                                                         )
                                                        )
                                      )
                                )
                               ((equal? type "band")
                                (set! buildList (append buildList
                                                        (list
                                                         (bandAccount a b c hold)
                                                         )
                                                        )
                                      )
                                )
                               ((equal? type "event")
                                (define more (string-append buildString (string (string-ref line i))))
                                (set! buildList (append buildList
                                                        (list
                                                         (event a b hold help e more)
                                                         )
                                                        )
                                      )
                                )
                               ((equal? type "fansave")
                                (set! e (string-append buildString (string (string-ref line i))))
                                (set! buildList (append buildList
                                                        (list
                                                         (fanSave a e)
                                                         )
                                                        )
                                      )
                                )
                               )
                             (set! buildString "")
                             )

                            (else
                             (set! buildString (string-append buildString (string (string-ref line i))))
                             )
                            )
                          )
                        )
                      buildList
                      )
  )

#| Define Relations |#
(struct fanAccount (username password email name surname) #:mutable #:transparent)
(struct bandAccount (username password email bandname) #:mutable #:transparent)
(struct event (eventid bandusername venue date time price) #:mutable #:transparent)
(struct fanSave (fanusername eventid) #:mutable #:transparent)

#| Build Relation: Fan |#
(define reader (open-input-file "fans.txt" #:mode 'binary))
(port-count-lines! reader)
(define copyData (port->lines reader #:line-mode 'any))
(close-input-port reader)
(define fanList (import-data copyData "fan"))

#| Build Relation: Band |#
(set! reader (open-input-file "bands.txt" #:mode 'binary))
(port-count-lines! reader)
(set! copyData (port->lines reader #:line-mode 'any))
(close-input-port reader)
(define bandList (import-data copyData "band"))

#| Build Relation: Event |#
(set! reader (open-input-file "events.txt" #:mode 'binary))
(port-count-lines! reader)
(set! copyData (port->lines reader #:line-mode 'any))
(close-input-port reader)
(define eventList (import-data copyData "event"))

#| Build Relation: Fan Save |#
(set! reader (open-input-file "fansaves.txt" #:mode 'binary))
(port-count-lines! reader)
(set! copyData (port->lines reader #:line-mode 'any))
(close-input-port reader)
(define fanSaved (import-data copyData "fansave"))

(define testWriteAppend (λ ()
                          (define writer (open-output-file "bands.txt" #:mode 'text #:exists 'append)) ;;maybe use update and just refill with entire database
                          (display "\rcoolerband password band2@email.com (Cooler Band)" writer)
                          (close-output-port writer)
                          )
  )

#|--------------------------------------------------------------------- End: Build Database From Files --------------------------------------------------------------------------------|#


#|-------------------------------------------------------------------- Start: Various Program Functions  ------------------------------------------------------------------------------|#




#|--------------------------------------------------------------------- End: Various Program Functions  ------------------------------------------------------------------------------|#




#|-------------------------------------------------------------------------------- Start: GUI Elements  ------------------------------------------------------------------------------------|#




#|-------------------------------------------------------------------------------- End: GUI Elements  ------------------------------------------------------------------------------------|#