#lang racket/gui
#|
Author: Jay Yang - M01006234
Course: BSc CompSci Year 1 '24-'25
Module: Programming

Listen, I'm going a little wild with this so I might not have comments for everything.

Project Details: 
|#


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
                        (define help "") ; holds the next column AFTER () strings ^
                        (define count 0) ; counts occurences of #\space to track columns (minus name strings in ())
                        (define countSpaceinParen 0) ;; Needed this to count the spaces in () strings. (Because previous counter still counts the spaces in parenthesis.)
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
                               ((equal? type "photo")
                                (set! e (string-append buildString (string (string-ref line i))))
                                (set! buildList (append buildList
                                                        (list
                                                         (eventPhoto a hold)
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

; NGL, I was too lazy too add more fields to relations because it meant I would have to add more to the algoritm above
(struct eventPhoto (username path) #:mutable #:transparent)
(struct profilePhotos (username path))

#| Build Relation: Fan |#
(define reader (open-input-file "DB/fans.txt" #:mode 'binary))
(port-count-lines! reader)
(define copyData (port->lines reader #:line-mode 'any))
(close-input-port reader)
(define fanList (import-data copyData "fan"))

#| Build Relation: Band |#
(set! reader (open-input-file "DB/bands.txt" #:mode 'binary))
(port-count-lines! reader)
(set! copyData (port->lines reader #:line-mode 'any))
(close-input-port reader)
(define bandList(import-data copyData "band"))

#| Build Relation: Event |#
(set! reader (open-input-file "DB/events.txt" #:mode 'binary))
(port-count-lines! reader)
(set! copyData (port->lines reader #:line-mode 'any))
(close-input-port reader)
(define eventList (import-data copyData "event"))

#| Build Relation: Fan Save |#
(set! reader (open-input-file "DB/fansaves.txt" #:mode 'binary))
(port-count-lines! reader)
(set! copyData (port->lines reader #:line-mode 'any))
(close-input-port reader)
(define fanSaved (import-data copyData "fansave"))


(set! reader (open-input-file "DB/eventphotos.txt" #:mode 'binary))
(port-count-lines! reader)
(set! copyData (port->lines reader #:line-mode 'any))
(close-input-port reader)
(define eventPaths (list->vector (import-data copyData "photo")))

#|--------------------------------------------------------------------- End: Build Database From Files --------------------------------------------------------------------------------|#

#|-------------------------------------------------------------------- Start: Various Program Functions  ------------------------------------------------------------------------------|#

(define testWriteAppend (λ ()
                          (define writer (open-output-file "bands.txt" #:mode 'text #:exists 'append)) ;;maybe use update and just refill with entire database
                          (display "\rcoolerband password band2@email.com (Cooler Band)" writer)
                          (close-output-port writer)
                          )
  )

(define randBannerNum (λ ()
                        (cond
                          ((< (vector-length eventPaths) 5)
                           (first (shuffle (range 0 (vector-length eventPaths)))))
                          (else (first (shuffle (range 0 5))))
                          )
                        )
  )

;; while loop hygeinic macro
(define-syntax-rule (my-while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

;; Created a copy of button class so I can store/send more data for the callback
;; No need for type-testing because I know what it's supposed to be
;; And the user need not concern themselves with what it is
(define updatingButton% (class button%
                          (super-new)
                          (init-field (extra ""))
                          (define/public set-extra (λ (un)
                                                     (set! extra un)
                                                     )
                            )
                          (define/public get-extra (λ ()
                                                     extra
                                                     )
                            )
                          )
  )

;; I really disliked having to create message objects over and over if I wanted a multiline text thingy.
;; So, I made a textbox class that will create multi-line messages where each line's character length can be specified through a field. 
(define messageBox% (class vertical-panel%
                      (super-new)
                      (init-field (message ""))
                      (init-field (char 0))
                      (inherit get-children)
                      (inherit delete-child)
                      
                      (define getList (λ ()
                                        (define str message)
                                        (define newStr "")
                                        (define mlist `())
                                        (my-while (not (equal? str ""))
                                                  (for ((i char))
                                                    #:final (> i (string-length str))
                                                    (cond
                                                      ((= i (sub1 char))
                                                       (set! mlist (append mlist `(,newStr)))
                                                       (set! str (substring str i (string-length str)))
                                                       (set! newStr "")
                                                       )
                                                      ((> i (sub1 (string-length str)))
                                                       (set! newStr str)
                                                       (set! mlist (append mlist `(,newStr)))
                                                       (set! str "")
                                                       (set! newStr "")
                                                       )
                                                      (else
                                                       (set! newStr (string-append newStr (string (string-ref str i))))
                                                       )
                                                      )
                                                    )
                                                  )
                                        mlist
                                        )
                        )

                      (define mList (getList))

                      (define makeMessages (λ (list)
                                             (define objectList `())
                                             (for ((m list))
                                               (append objectList
                                                       `(
                                                         ,(new message%
                                                               [parent this]
                                                               [label m]
                                                               [horiz-margin 0]
                                                               [vert-margin 0]
                                                               [auto-resize #t]
                                                               )
                                                         )
                                                       )
                                               )
                                             )
                        )
                      
                      (define/public set-message (λ (new)
                                                   (set! message new)
                                                   (set! mList (getList))
                                                   (makeMessages mList)
                                                   )
                        )

                      (define/public clearChildren (λ ()
                                                     (for ((i (send this get-children)))
                                                       (send this delete-child i)
                                                       )
                                                     )
                        )
                      
                      (makeMessages mList)
                      )
  )

(define loggedin? (list #f "" "" #f))

#|--------------------------------------------------------------------- End: Various Program Functions  ------------------------------------------------------------------------------|#

#|--------------------------------------------------------------------- Start: Various Button Callbacks --------------------------------------------------------------------------------|#

(define clearMain (λ ()
                    (for ((i (send main get-children)))
                      (send main delete-child i)
                      )
                    )
  )

(define changeLSP (λ ()
                    (cond
                      ((boolean=? (first loggedin?) #t)
                       (send buttonHolder delete-child loginbut)
                       (send buttonHolder delete-child sigupbut)
                       (send buttonHolder add-child profilebut)
                       )
                      )
                    )
  )
                      

(define toHome (λ ()
                 (clearMain)
                 (send main add-child homePanel)
                 (send statePage set-label "Home")
                 
                 )
  )

(define toLogin (λ ()
                  (send loginMessenger set-label "")
                  (send loginUsername set-value "")
                  (send loginPassword set-value "")
                  (clearMain)
                  (send main add-child loginPanel)
                  (send statePage set-label "Log In")
                  )
  )


(define verifyLogin (λ ()
                      (define username (send loginUsername get-value))
                      (define password (send loginPassword get-value))
                      (define check '(#f "" #f))
                      (cond
                        ((or (not (empty? (regexp-match* #rx"(?!_)[^a-zA-Z0-9]+" username))) (not (empty? (regexp-match* #rx"__+" username))))
                         (send loginMessenger set-label "Sorry, but this sort of username isn't allowed in our system. Check again.")(send loginMessenger set-color "red"))
                        (else 
                         (for ((i fanList))
                           #:final (and (equal? (fanAccount-username i) username) (equal? (fanAccount-password i) password))
                           (cond
                             ((and (equal? (fanAccount-username i) username) (equal? (fanAccount-password i) password))
                              (set! check `(#t "fan" #t))
                              )
                             (else (set! check '(#f "" #t)))
                             )
                           )
                         (cond
                           ((boolean=? (first check) #f)
                            (for ((i bandList))
                              #:final (and (equal? (bandAccount-username i) username) (equal? (bandAccount-password i) password))
                              (cond
                                ((and (equal? (bandAccount-username i) username) (equal? (bandAccount-password i) password))
                                 (set! check '(#t "band" #t))
                                 )
                                (else (set! check '(#f "" #t)))
                                )
                              )
                            )
                           )
                         )
                        )
                      (cond
                        ((boolean=? (first (reverse check)) #t)
                         (cond
                           ((boolean=? (first check) #t)
                            (set! loggedin? (list #t username (first (rest check))))
                            (send loginMessenger set-color "black")
                            (changeLSP)
                            (toHome)
                            )
                           (else (send loginMessenger set-label "Incorrect username/password. Try again.")(send loginMessenger set-color '"red"))
                           )
                         )
                        )
                      )
  )

(define toSignUp (λ ()
                   (for ((i (send stayLeft get-children)))
                     (send stayLeft delete-child i)
                     )
                   (send stayLeft add-child pickRegi)
                   (send regiMessenger set-label "")
                   (send regiFanUsername set-value "")
                   (send regiFanPassword set-value "")
                   (clearMain)
                   (send main add-child regiPanel)
                   (send statePage set-label "Sign Up")
                   )
  )
                   
(define registerFan (λ ()
                      (for ((i (send regiIn get-children)))
                        (send regiIn delete-child i)
                        )
                      (send stayLeft delete-child pickRegi)
                      (send stayLeft add-child regiSide)
                      (send regiIn add-child regiFan)
                      (send attemptRegi set-extra "fan")
                      )
  )

(define registerBand (λ ()
                      (for ((i (send regiIn get-children)))
                        (send regiIn delete-child i)
                        )
                      (send stayLeft delete-child pickRegi)
                      (send stayLeft add-child regiSide)
                      (send regiIn add-child regiBand)
                      (send attemptRegi set-extra "band")
                      )
  )

(define resetMessageBox (λ ()
                          (send regiMessenger set-label "")
                          (send regiMessenger set-color #f)
                          )
  )

(define alreadyUsername? (λ (input)
                           (define result #f)
                           (for ((i fanList))
                             #:final (equal? (fanAccount-username i) input)
                             (cond
                               ((equal? (fanAccount-username i) input) (set! result #t))
                              )
                             )
                           (cond
                             ((equal? result #f)
                              (for ((i bandList))
                                #:final (equal? (bandAccount-username i) input)
                                (cond
                                  ((equal? (bandAccount-username i) input) (set! result #t))
                                  )
                                )
                              )
                             )
                           result
                           )
  )

(define verifyRegi (λ ()
                     (define copy "")
                     (define beforeAt "")
                     (define afterAt "")
                     (define @counter 0)

                     (define split (λ ()
                                     (for ((i (string-length copy)))
                                       #:final (equal? (string (string-ref copy i)) "@")
                                       (cond
                                         ((equal? (string (string-ref copy i)) "@")
                                          (set! @counter (add1 @counter))
                                          (set! copy (substring copy (add1 i) (string-length copy))))
                                         (else
                                          (set! beforeAt (string-append beforeAt (string (string-ref copy i)))))
                                         )
                                       )
                                     
                                     
                                     (for ((t (string-length copy)))
                                       (set! afterAt (string-append afterAt (string (string-ref copy t))))
                                       (cond
                                         ((equal? (string (string-ref copy t)) "@")
                                          (set! @counter (add1 @counter)))
                                         )
                                       )
                                     
                                     )
                       )
                     
                     (cond
                       ((equal? (send attemptRegi get-extra) "fan")
                        (set! copy (send regiFanEmail get-value))
                            
                        (split)
                    
                        
                        (cond
                          ((equal? (send regiFanUsername get-value) "")
                           (send regiMessenger set-label "Please enter a username.")(send regiMessenger set-color "red"))
                          ((equal? (send regiFanName get-value) "")
                           (send regiMessenger set-label "Please enter your name.")(send regiMessenger set-color "red"))
                          ((equal? (send regiFanSurname get-value) "")
                           (send regiMessenger set-label "Please enter your surname .")(send regiMessenger set-color "red"))
                          ((equal? (send regiFanEmail get-value) "")
                           (send regiMessenger set-label "Please enter an email.")(send regiMessenger set-color "red"))
                          ((equal? (send regiFanPassword get-value) "")
                           (send regiMessenger set-label "Please enter a password.")(send regiMessenger set-color "red"))
                                 
                          ((not (empty? (regexp-match* #rx"(?!(_))[^a-zA-Z0-9]+" (send regiFanUsername get-value))))
                           (send regiMessenger set-label "Sorry, usernames can only contain letters, numbers and underscores. Try again.")(send regiMessenger set-color "red"))
                          ((not (empty? (regexp-match* #rx"(__)+" (send regiFanUsername get-value))))
                           (send regiMessenger set-label "Sorry, an underscore cannot be followed by another underscore. Try again.")(send regiMessenger set-color "red"))
                                 
                          ((not (empty? (regexp-match* #rx"[^a-zA-Z]+" (send regiFanName get-value))))
                           (send regiMessenger set-label "Invalid characters found in name. Try again.")(send regiMessenger set-color "red"))
                                 
                          ((not (empty? (regexp-match* #rx"(?!( ))[^a-zA-Z]" (send regiFanSurname get-value))))
                           (send regiMessenger set-label "Invalid characters found in surname. Try again.")(send regiMessenger set-color "red"))
                                 
                          ((equal? beforeAt "")
                           (send regiMessenger set-label "Text before @ empty")(send regiMessenger set-color "red"))
                          ((equal? afterAt "")
                           (send regiMessenger set-label "Text after @ empty")(send regiMessenger set-color "red"))
                          ((= @counter 0)
                           (send regiMessenger set-label "No @ found in email.")(send regiMessenger set-color "red"))
                          ((> @counter 1)
                           (send regiMessenger set-label "Too many @'s found in email.")(send regiMessenger set-color "red"))
                          ((not (empty? (regexp-match* #rx"(?!(.))[^a-zA-Z0-9]+" beforeAt)))
                           (send regiMessenger set-label "Emails can only contain letters, numbers, and periods.")(send regiMessenger set-color "red"))
                          ((not (empty? (regexp-match* #rx"(?!(.))[^a-zA-Z0-9]+" afterAt)))
                           (send regiMessenger set-label "Emails can only contain letters, numbers, and periods.")(send regiMessenger set-color "red"))
                          ((or (not (empty? (regexp-match* #rx"(\\.\\.)+" beforeAt))) (not (empty? (regexp-match* #rx"(\\.\\.)+" afterAt))))
                           (send regiMessenger set-label "No consecutive periods.")(send regiMessenger set-color "red"))
                          ((not (empty? (regexp-match* #rx"(\\.@\\.)+" (send regiFanEmail get-value))))
                           (send regiMessenger set-label "No periods before or after @.")(send regiMessenger set-color "red"))

                          ((alreadyUsername? (send regiFanUsername get-value))
                           (send regiMessenger set-label "Username is taken.")(send regiMessenger set-color "red"))

                          (else
                           (set! fanList (append fanList `(
                                                           ,(fanAccount
                                                             (send regiFanUsername get-value)
                                                             (send regiFanPassword get-value)
                                                             (send regiFanEmail get-value)
                                                             (string-titlecase (send regiFanName get-value))
                                                             (string-titlecase (send regiFanSurname get-value))
                                                             )
                                                           )
                                                 )
                                 )
                           (define writer (open-output-file "DB/fans.txt" #:mode 'text #:exists 'append)) ;;maybe use update and just refill with entire database
                           (display (string-join `(,(send regiFanUsername get-value) ,(send regiFanPassword get-value) ,(send regiFanEmail get-value) ,(string-titlecase (send regiFanName get-value)) ,(string-titlecase (send regiFanSurname get-value))) " " #:before-first "\r") writer)
                           (close-output-port writer)
                           (toLogin)
                           )
                          
                          )
                        )
                       ((equal? (send attemptRegi get-extra) "band")
                        (set! copy (send regiBandEmail get-value))

                        (split)

                        (cond
                          ((equal? (send regiBandUsername get-value) "")
                           (send regiMessenger set-label "Please enter a username.")(send regiMessenger set-color "red"))
                          ((equal? (send regiBandName get-value) "")
                           (send regiMessenger set-label "Please enter your name.")(send regiMessenger set-color "red"))
                          ((equal? (send regiBandEmail get-value) "")
                           (send regiMessenger set-label "Please enter an email.")(send regiMessenger set-color "red"))
                          ((equal? (send regiBandPassword get-value) "")
                           (send regiMessenger set-label "Please enter a password.")(send regiMessenger set-color "red"))

                          ((not (empty? (regexp-match* #rx"(?!(_))[^a-zA-Z0-9]+" (send regiBandUsername get-value))))
                           (send regiMessenger set-label "Sorry, usernames can only contain letters, numbers and underscores. Try again.")(send regiMessenger set-color "red"))
                          ((not (empty? (regexp-match* #rx"(__)+" (send regiBandUsername get-value))))
                           (send regiMessenger set-label "Sorry, an underscore cannot be followed by another underscore. Try again.")(send regiMessenger set-color "red"))

                          ((not (empty? (regexp-match* #rx"(?!( ))[^a-zA-Z]+" (send regiBandName get-value))))
                           (send regiMessenger set-label "Invalid characters found in band name. Try again.")(send regiMessenger set-color "red"))
                          ((not (empty? (regexp-match* #rx"(  )+" (send regiBandName get-value))))
                           (send regiMessenger set-label "Consecutive spaces in band name. Try again.")(send regiMessenger set-color "red"))

                          
                          ((equal? beforeAt "")
                           (send regiMessenger set-label "Invalid email.")(send regiMessenger set-color "red"))
                          ((equal? afterAt "")
                           (send regiMessenger set-label "Invalid email.")(send regiMessenger set-color "red"))
                          ((> @counter 1)
                           (send regiMessenger set-label "Too many @'s found in email.")(send regiMessenger set-color "red"))
                          ((not (empty? (regexp-match* #rx"(?!(.))[^a-zA-Z0-9]+" beforeAt)))
                           (send regiMessenger set-label "Emails can only contain letters, numbers, and periods.")(send regiMessenger set-color "red"))
                          ((not (empty? (regexp-match* #rx"(?!(.))[^a-zA-Z0-9]+" afterAt)))
                           (send regiMessenger set-label "Emails can only contain letters, numbers, and periods.")(send regiMessenger set-color "red"))
                          ((or (not (empty? (regexp-match* #rx"(\\.\\.)+" beforeAt))) (not (empty? (regexp-match* #rx"(\\.\\.)+" afterAt))))
                           (send regiMessenger set-label "No consecutive periods.")(send regiMessenger set-color "red"))
                          ((not (empty? (regexp-match* #rx"(\\.@\\.)+" (send regiFanEmail get-value))))
                           (send regiMessenger set-label "No periods before or after @.")(send regiMessenger set-color "red"))

                          ((alreadyUsername? (send regiBandUsername get-value))
                           (send regiMessenger set-label "Username is taken.")(send regiMessenger set-color "red"))

                          (else
                           (set! bandList (append bandList `(
                                                           ,(bandAccount
                                                             (send regiBandUsername get-value)
                                                             (send regiBandPassword get-value)
                                                             (send regiBandEmail get-value)
                                                             (send regiBandName get-value)
                                                             )
                                                           )
                                                 )
                                 )
                           (define writer (open-output-file "DB/bands.txt" #:mode 'text #:exists 'append)) ;;maybe use update and just refill with entire database
                           (display (string-join `(,(send regiBandUsername get-value) ,(send regiBandPassword get-value) ,(send regiBandEmail get-value) ,(string-titlecase (send regiBandName get-value))) " " #:before-first "\r") writer)
                           (close-output-port writer)
                           (toLogin)
                           )
                          
                          )
                        )
                       )

                     )
  )


(define do-to-Search (λ ()
                       (clearMain)
                       (send main add-child searchMain)
                       )
  )


                   



#|--------------------------------------------------------------------- End: Various Button Callbacks  -----------------------------------------------------------------------------|#

#|--------------------------------------------------------------------- Start: Export Database to Files --------------------------------------------------------------------------------|#





#|--------------------------------------------------------------------- End: Export Database From Files -----------------------------------------------------------------------------|#

#|-------------------------------------------------------------------------------- Start: GUI Elements  ------------------------------------------------------------------------------------|#
#| I originally thought of creating a web server for this, but customizing wep pages through racket is way too different.
I came to the conclusion that, if I aim to do this in web form, I might as well just use HTML as is instead of racket.
I don't have the time to learn more about web applications beyond the basics I already know.|#

#|-------------------------------------------------------------------------|#
(define app (new frame%
                 (label "Bandifan!")
                 [width 1380]
                 [height 1000]
                 [style '(no-resize-border no-system-menu)]
                 )
  )


(define alwaysActiveHeader (new vertical-panel%
                                [parent app]
                                [horiz-margin 0]
                                )
  )

(define brandBox (new horizontal-panel%
                      [parent alwaysActiveHeader]
                      [alignment '(left center)]
                      [horiz-margin 2]
                      [vert-margin 0]
                      )
  )

(define exitApp (new button%
                     [parent brandBox]
                     [label (read-bitmap "GUI Images/main-gui/exit-button.png")]
                     [callback
                      (λ (o e)
                        (exit)
                        )
                      ]
                     [horiz-margin 2]
                     [vert-margin 0]
                     )
  )

(define iconize (new button%
                     [parent brandBox]
                     [label (read-bitmap "GUI Images/main-gui/iconize-button.png")]
                     [callback
                      (λ (o e)
                        (send app iconize #t)
                        )
                      ]
                     [horiz-margin 2]
                     [vert-margin 0]
                     )
  )

(define logo(new button%
                 [parent brandBox]
                 [horiz-margin 2]
                 [vert-margin 0]
                 [label (read-bitmap "GUI Images/main-gui/logo-button.png")]
                 [callback
                  (λ (o e)
                    (toHome)
                    )
                  ]
                     
                 )
  )

(define statePage (new message%
                       [parent brandBox]
                       [label "Home"]
                       [horiz-margin 10]
                       [vert-margin 0]
                       [min-width 400]
                       )
  )

(define buttonHolder (new horizontal-panel%
                          [parent brandBox]
                          [alignment '(right center)]
                          [horiz-margin 0]
                          [vert-margin 0]
                          )
  )

(define loginbut (new button%
                      [parent buttonHolder]
                      [label (read-bitmap "GUI Images/main-gui/login-button.png")]
                      [horiz-margin 2]
                      [vert-margin 0]
                      [callback
                       (λ (o e)
                         (toLogin)
                         )
                       ]
                      )
  )
(define sigupbut (new button%
                      [parent buttonHolder]
                      [label (read-bitmap "GUI Images/main-gui/signin-button.png")]
                      [horiz-margin 2]
                      [vert-margin 0]
                      [callback
                       (λ (o e)
                         (toSignUp)
                         )
                       ]
                      )
  )

(define profilebut (new button%
                        [parent buttonHolder]
                        [label "Profile"]
                        [horiz-margin 2]
                        [vert-margin 0]
                        [min-width 80]
                        [min-height 25]
                        [style '(deleted)]
                        )
  )

(define naviSearch (new horizontal-panel%
                        [parent alwaysActiveHeader]
                        [alignment '(center center)]
                        [horiz-margin 5] ;+1 for the border?
                        [vert-margin 0]
                        [style '(border)]
                        )
  )

(define naviButtons (new horizontal-panel%
                         [parent naviSearch]
                         [alignment '(left center)]
                         [horiz-margin 0]
                         [vert-margin 0]
                         )
  )
(define homeButton (new button%
                        [parent naviButtons]
                        [label (read-bitmap "GUI Images/main-gui/home-button.png")]
                        [callback
                         (λ (o e)
                           (toHome)
                           )
                         ]
                        [horiz-margin 2]
                        [vert-margin 0]
                        )
  )


(define savedButton (new button%
                         [parent naviButtons]
                         [label (read-bitmap "GUI Images/main-gui/myevents-button.png")]
                         [horiz-margin 2]
                         [vert-margin 0]
                         )
  )



(define searchBox (new horizontal-panel%
                       [parent naviSearch]
                       [alignment '(right center)]
                       [horiz-margin 2]
                       [vert-margin 0]
                       )
  )

(define searchAlways (new text-field%
                          [parent searchBox]
                          [label ""]
                          [horiz-margin 2]
                          [vert-margin 0]
                          )
  )

(define filterDrop (new choice%
                        [parent searchBox]
                        [label ""]
                        [min-height 25]
                        [choices '("--Filter--" "Band" "Date" "Venue")]
                        [horiz-margin 2]
                        [vert-margin 0]
                        )
  )

(define searchButton (new button%
                          [parent searchBox]
                          [label "Search"]
                          [min-height 25]
                          [horiz-margin 1]
                          [vert-margin 0]
                          [callback
                           (λ (o e)
                             (do-to-Search)
                             )
                           ]
                          )
  )

(define main (new vertical-panel%
                  [parent app]
                  [min-height 886]
                  [alignment '(left center)]
                  [horiz-margin 0]
                  [vert-margin 0]
                  )
  )

(define homePanel (new vertical-panel%
                       [parent main]
                       [horiz-margin 0]
                       [vert-margin 0]
                       [alignment '(left top)]
                       )
  )

#|--------Start Home-----------|#
(define messageHolder1 (new panel%
                            [parent homePanel]
                            [alignment '(left center)]
                            [horiz-margin 8]
                            [vert-margin 0]
                            )
  )

(define topMessage (new message%
                        [parent messageHolder1]
                        [label "Upcoming Events"]
                        [horiz-margin 0]
                        [vert-margin 0]
                        )
  )

(define bannerScroll (new horizontal-panel%
                          [parent homePanel]
                          [alignment '(center center)]
                          [horiz-margin 0]
                          [vert-margin 0]
                          )
  )

(define bannerButton (new updatingButton%
                          [parent bannerScroll]
                          [label (read-bitmap (eventPhoto-path (vector-ref eventPaths 0)))] ;init
                          [extra (eventPhoto-username (vector-ref eventPaths 0))] ;init
                          [horiz-margin 0]
                          [vert-margin 0]
                          )
  )

#|--------------------------Auto-Update Banner on Home---------------------------|#
(define last 0) ;ew
(define next 0)
(define showcaseTimer (new timer%
                           [notify-callback
                            (λ ()
                              (set! next (randBannerNum))
                              (my-while (= next last)
                                        (set! next (randBannerNum)))
                              (send bannerButton set-label (read-bitmap (eventPhoto-path (vector-ref eventPaths next))))
                              (send bannerButton set-extra (eventPhoto-username (vector-ref eventPaths next)))
                              (set! last next) 
                              )
                            ]
                           [interval 4000]
                           )
  )
#|-----------------------------------------------------|#

(define messageHolder2 (new panel%
                            [parent homePanel]
                            [alignment '(left top)]
                            [horiz-margin 8]
                            [vert-margin 0]
                            )
  )

(define nextMessage (new message%
                         [parent messageHolder2]
                         [label "Check out these bands!"]
                         [horiz-margin 0]
                         [vert-margin 0]
                         )
  )
                           
(define bandAds (new horizontal-panel%
                     [parent homePanel]
                     [alignment '(center top)]
                     [min-height 430]
                     [horiz-margin 0]
                     [vert-margin 0]
                     )
  )

(define bandAd1 (new vertical-panel%
                     [parent bandAds]
                     [alignment '(center center)]
                     [style '(border)]
                     [horiz-margin 6]
                     [vert-margin 6]
                     )
  )

(define bandAd2 (new vertical-panel%
                     [parent bandAds]
                     [alignment '(center center)]
                     [style '(border)]
                     [horiz-margin 6]
                     [vert-margin 6]
                     )
  )

(define bandAd3 (new vertical-panel%
                     [parent bandAds]
                     [alignment '(center center)]
                     [style '(border)]
                     [horiz-margin 6]
                     [vert-margin 6]
                     )
  )
#|--------End Home-----------|#

#|------------Start Login Page-----------|#

(define loginPanel (new vertical-panel%
                        [parent main]
                        [style '(deleted border)]
                        [alignment '(left top)]
                        [horiz-margin 0]
                        [vert-margin 0]
                        )
  )

(define loginSection (new horizontal-panel%
                          [parent loginPanel]
                          [alignment '(left top)]
                          [horiz-margin 0]
                          [vert-margin 0]
                          )
  )

(define moreInfo (new vertical-panel%
                      [parent loginSection]
                      [alignment '(left top)]
                      [horiz-margin 0]
                      [vert-margin 0]
                      [min-width 700]
                      )
  )

(define loginSide (new vertical-panel%
                       [parent loginSection]
                       [alignment '(left top)]
                       [horiz-margin 0]
                       [vert-margin 0]
                       [min-width 500]
                       )
  )

(define signSpacer (new panel%
                        [parent loginSide]
                        [alignment '(left top)]
                        [horiz-margin 0]
                        [vert-margin 0]
                        [min-height 100]
                        )
  )

(define bigLogin (new message%
                      [parent loginSide]
                      [label "Log In"]
                      [horiz-margin 50]
                      [vert-margin 40]
                      )
  )

(define loginVert (new vertical-panel%
                       [parent loginSide]
                       [alignment '(left top)]
                       [horiz-margin 0]
                       [vert-margin 0]
                       [min-height 500]
                       )
  )

(define loginUsername (new text-field%
                           [parent loginVert]
                           [init-value ""]
                           [style '(single vertical-label)]
                           [label "Username: "]
                           [horiz-margin 50]
                           [vert-margin 25]
                           )
  )

(define loginPassword (new text-field%
                           [parent loginVert]
                           [init-value ""]
                           [style '(single vertical-label password)]
                           [label "Password: "]
                           [horiz-margin 50]
                           [vert-margin 10]
                           )
  )

(define logbuttonfix (new horizontal-panel%
                          [parent loginVert]
                          [horiz-margin 50]
                          [vert-margin 0]
                          [alignment '(right top)]
                          )
  )

(define toLeft (new panel%
                     [parent logbuttonfix]
                     [alignment '(left top)]
                     )
  )

(define cancelLogin (new button%
                         [parent toLeft]
                         [label "Cancel"]
                         [vert-margin 60]
                         [callback
                          (λ (o e)
                            (toHome)
                            )
                          ]
                         )
  )

(define noAccount? (new button%
                        [parent logbuttonfix]
                        [label "Don't have an account?"]
                        [vert-margin 60]
                        [callback
                          (λ (o e)
                            (toSignUp)
                            )
                          ]
                        )
  )

(define attemptLogin (new button%
                          [parent logbuttonfix]
                          [label "Login"]
                          [vert-margin 60]
                          [callback
                           (λ (o e)
                             (verifyLogin)
                             )
                           ]
                          )
  )

(define loginMessagerBox (new vertical-panel%
                              [parent loginVert]
                              [alignment '(right top)]
                              )
  )

(define loginMessenger (new message%
                            [parent loginMessagerBox]
                            [label ""]
                            [auto-resize #t]
                            [horiz-margin 50]
                            )
  )


#|-------------End Login Page-----------|#

#|------------- Start Register Page-----------|#

(define regiPanel (new vertical-panel%
                       [parent main]
                       [style '(deleted border)]
                       [alignment '(left top)]
                       [horiz-margin 0]
                       [vert-margin 0]
                       )
  )

(define regiSection (new horizontal-panel%
                         [parent regiPanel]
                         [alignment '(left top)]
                         [horiz-margin 0]
                         [vert-margin 0]
                         )
  )

(define stayLeft (new vertical-panel%
                      [parent regiSection]
                      [alignment '(left top)]
                      [horiz-margin 0]
                      [vert-margin 0]
                      )
  )

#|----------------------------|#
(define pickRegi (new vertical-panel%
                      [parent stayLeft]
                      [alignment '(left top)]
                      [horiz-margin 0]
                      [vert-margin 0]
                      [min-width 500]
                      )
  )

(define spacer (new panel%
                    [parent pickRegi]
                    [min-height 20]
                    )
  )

(define which (new message%
                   [parent pickRegi]
                   [label "Which type of account are you registering?"]
                   [horiz-margin 50]
                   [vert-margin 50]
                   )
  )

(define regiButtons (new horizontal-panel%
                         [parent pickRegi]
                         [alignment '(center center)]
                         [horiz-margin 0]
                         [vert-margin 0]
                         )
  )

(define fanButton (new button%
                       [parent regiButtons]
                       [label "Fan"]
                       [horiz-margin 80]
                       [callback
                        (λ (o e)
                          (send typeInfo clearChildren)
                          (send typeInfo set-alignment 'left 'center)
                          (send typeInfo set-message "Do you have some favorite bands that you want to keep up with? Make an Fan account to save and easilly recall upcoming events from your favorite bands!")
                          (send continuebut set-extra "fan")
                          (send attemptRegi set-extra "fan")
                          )
                        ]
                       )
  )
(define bandButton (new button%
                        [parent regiButtons]
                        [label "Band"]
                        [horiz-margin 80]
                        [callback
                        (λ (o e)
                          (send typeInfo clearChildren)
                          (send typeInfo set-alignment 'right 'center)
                          (send typeInfo set-message "All bands want to make their concerts as accessible as possible. With our service, you can have a place to show all your upcoming events to fans, soon-to-be fans, and even onlookers!")
                          (send continuebut set-extra "band")
                          (send attemptRegi set-extra "band")
                          )
                        ]
                        )
  )

(define static (new panel%
                    [parent pickRegi]
                    [alignment '(left center)]
                    [min-height 120]
                    )
  )

(define typeInfo (new messageBox%
                      [parent static]
                      [alignment '(left center)]
                      [message ""]
                      [char 50]
                      [horiz-margin 50]
                      [vert-margin 0]
                      )
  )





(define buttonBox (new horizontal-panel%
                       [parent pickRegi]
                       [alignment '(center top)]
                       )
  )

(define cancelSignUp (new button%
                          [parent buttonBox]
                          [label "Cancel"]
                          [horiz-margin 10]
                          [callback
                           (λ (o e)
                             (toHome)
                             )
                           ]
                          )
  )

(define continuebut (new updatingButton%
                          [parent buttonBox]
                          [label "Continue"]
                          [extra ""]
                          [horiz-margin 10]
                          [callback
                           (λ (o e)
                             (cond
                               ((equal? (send continuebut get-extra) "fan") (registerFan))
                               ((equal? (send continuebut get-extra) "band") (registerBand))
                               )
                             )
                           ]
                               
                          )
  )

#|----------------------------|#
(define regiSide (new vertical-panel%
                      [parent stayLeft]
                      [alignment '(left top)]
                      [horiz-margin 0]
                      [vert-margin 0]
                      [style '(deleted)]
                      [min-width 500]
                      )
  )

(define signSpacer2 (new panel%
                         [parent regiSide]
                         [alignment '(left top)]
                         [horiz-margin 0]
                         [vert-margin 0]
                         [min-height 100]
                         )
  )

(define bigRegi (new message%
                     [parent regiSide]
                     [label "Sign Up"]
                     [horiz-margin 50]
                     [vert-margin 40]
                     )
  )

(define regiIn (new vertical-panel%
                      [parent regiSide]
                      [alignment '(right top)]
                      [horiz-margin 0]
                      [vert-margin 0]
                      )
  )

#|----------------------------|#
(define regiFan (new vertical-panel%
                     [parent regiIn]
                     [alignment '(right top)]
                     [horiz-margin 0]
                     [vert-margin 0]
                     [style '(deleted)]
                     )
  )

(define regiFanUsername (new text-field%
                          [parent regiFan]
                          [init-value ""]
                          [style '(single vertical-label)]
                          [label "Username: "]
                          [horiz-margin 50]
                          [vert-margin 5]
                          [callback (λ (o e)
                                      (resetMessageBox)
                                      )
                                    ]
                          )
  )

(define regiFanName (new text-field%
                          [parent regiFan]
                          [init-value ""]
                          [style '(single vertical-label)]
                          [label "Name: "]
                          [horiz-margin 50]
                          [vert-margin 5]
                          [callback (λ (o e)
                                      (resetMessageBox)
                                      )
                                    ]
                          )
  )

(define regiFanSurname (new text-field%
                          [parent regiFan]
                          [init-value ""]
                          [style '(single vertical-label)]
                          [label "Surname: "]
                          [horiz-margin 50]
                          [vert-margin 5]
                          [callback (λ (o e)
                                      (resetMessageBox)
                                      )
                                    ]
                          )
  )


(define regiFanEmail (new text-field%
                          [parent regiFan]
                          [init-value ""]
                          [style '(single vertical-label)]
                          [label "Email: "]
                          [horiz-margin 50]
                          [vert-margin 5]
                          [callback (λ (o e)
                                      (resetMessageBox)
                                      )
                                    ]
                          )
  )

(define regiFanPassword (new text-field%
                          [parent regiFan]
                          [init-value ""]
                          [style '(single vertical-label password)]
                          [label "Password: "]
                          [horiz-margin 50]
                          [vert-margin 5]
                          [callback (λ (o e)
                                      (resetMessageBox)
                                      )
                                    ]
                          )
  )

#|----------------------------|#

#|----------------------------|#

(define regiBand (new vertical-panel%
                     [parent regiIn]
                     [alignment '(right top)]
                     [horiz-margin 0]
                     [vert-margin 0]
                     [style '(deleted)]
                     )
  )

(define regiBandUsername (new text-field%
                          [parent regiBand]
                          [init-value ""]
                          [style '(single vertical-label)]
                          [label "Username: "]
                          [horiz-margin 50]
                          [vert-margin 5]
                          [callback (λ (o e)
                                      (resetMessageBox)
                                      )
                                    ]
                          )
  )

(define regiBandName (new text-field%
                          [parent regiBand]
                          [init-value ""]
                          [style '(single vertical-label)]
                          [label "Band Name: "]
                          [horiz-margin 50]
                          [vert-margin 5]
                          [callback (λ (o e)
                                      (resetMessageBox)
                                      )
                                    ]
                          )
  )

(define regiBandEmail (new text-field%
                          [parent regiBand]
                          [init-value ""]
                          [style '(single vertical-label)]
                          [label "Email: "]
                          [horiz-margin 50]
                          [vert-margin 5]
                          [callback (λ (o e)
                                      (resetMessageBox)
                                      )
                                    ]
                          )
  )

(define regiBandPassword (new text-field%
                          [parent regiBand]
                          [init-value ""]
                          [style '(single vertical-label password)]
                          [label "Password: "]
                          [horiz-margin 50]
                          [vert-margin 5]
                          [callback (λ (o e)
                                      (resetMessageBox)
                                      )
                                    ]
                          )
  )

#|----------------------------|#

(define regbuttonfix (new panel%
                          [parent regiSide]
                          [horiz-margin 0]
                          [vert-margin 0]
                          [alignment '(left top)]
                          [min-height 280]
                          )
  )

(define attemptRegi (new updatingButton%
                         [parent regbuttonfix]
                         [label "Register"]
                         [horiz-margin 50]
                         [vert-margin 25]
                         [extra ""]
                         [callback
                          (λ (o e)
                            (verifyRegi)
                            )
                          ]
                         )
  )

(define regiMessagerBox (new vertical-panel%
                             [parent regiSide]
                             [alignment '(left top)]
                             )
  )

(define regiMessenger (new message%
                           [parent regiMessagerBox]
                           [label ""]
                           [auto-resize #t]
                           [horiz-margin 30]
                           )
  )

(define moreInfo2 (new vertical-panel%
                       [parent regiSection]
                       [alignment '(right top)]
                       [horiz-margin 0]
                       [vert-margin 0]
                       [min-width 700]
                       )
  )

#|--------------End Register Page------------|#

#|--------------Start: Search Page------------|#

(define searchMain (new vertical-panel%
                        [parent main]
                        [alignment '(left top)]
                        [horiz-margin 0]
                        [vert-margin 0]
                        [style '(deleted)]
                        )
  )

(define sTopSection (new horizontal-panel%
                         [parent searchMain]
                         [alignment '(left center)]
                         )
  )




#|--------------End: Search Page------------|#


(send app show #t)
(send app center 'both)

#|-------------------------------------------------------------------------------- End: GUI Elements  ------------------------------------------------------------------------------------|#
