#lang racket/gui
#|
Author: Jay Yang - M01006234
Course: BSc CompSci Year 1 '24-'25
Module: Programming

Listen, I'm going a little wild with this so I might not have comments for everything. Ask me for details and I will explain.

Project Details: 
|#


#|--------------------------------------------------------------------- Start: Build Database From Files --------------------------------------------------------------------------------|#
#|
Initialize Program - Load Database
Returns a List of Structs. Structs are appropriate for each relation.
This function (and program) was written in a way such that "(" and ")" mark the beginning and end of names containing spaces; like "The Band Name"
I'll be honest, I knew what I was doing, and the function does what I intended line-by-line. But I also kinda feel like I bruteforced my way through it.
There is probably a better way to do this with maybe some built-in racket functions. But I could not find documentation, so I just thought
of my own way to do it.

Other than that, every line's text is separated by spaces.

This program will export data to txt files in the correct formats for re-reading. 
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
                        (define f "")
                        (define g "")
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
                               ((= count (+ 6 countSpaceinParen))
                                (set! f buildString))
                               ((= count (+ 7 countSpaceinParen))
                                (set! g buildString))
                               )
                             (set! buildString "")
                             )
                            
                            ((= i (sub1 (string-length line))) ;; when loop has iterated through whole line
                             
                             (cond
                               ((equal? type "fan")
                                (set! e buildString)
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

                                (cond
                                  ((equal? more "a")
                                   (set! more "Available"))
                                  ((equal? more "b")
                                   (set! more "Booked"))
                                  ((equal? more "c")
                                   (set! more "Cancelled"))
                                  )
                                (set! buildList (append buildList
                                                        (list
                                                         (event a b hold help e f more)
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
(struct event (eventid bandusername venue date time price status) #:mutable #:transparent)
(struct fanSave (fanusername eventid) #:mutable #:transparent)

; honestly, I was too lazy and have too little time to add more fields to relations because it meant I would have to add more to the parsing algoritm above
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

; probably a poor way to store photos
(set! reader (open-input-file "DB/eventphotos.txt" #:mode 'binary))
(port-count-lines! reader)
(set! copyData (port->lines reader #:line-mode 'any))
(close-input-port reader)
(define eventPaths (list->vector (import-data copyData "photo")))

#|--------------------------------------------------------------------- End: Build Database From Files --------------------------------------------------------------------------------|#

#|-------------------------------------------------------------------- Start: Various Program Functions  ------------------------------------------------------------------------------|#

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





(define loggedin? (list #f "" ""))

(define getNameStruct (λ ()
                        (cond
                          ((equal? (first (reverse loggedin?)) "fan")
                           (first (filter (λ (elem) (equal? (first (rest loggedin?)) (fanAccount-username elem))) fanList)))
                          ((equal? (first (reverse loggedin?)) "band")
                           (first (filter (λ (elem) (equal? (first (rest loggedin?)) (bandAccount-username elem))) bandList)))
                          )
                        )
  )

(define getProfilePhoto (λ ()
                          (cond
                            ((fanAccount? (getNameStruct))
                             (cond
                               ((file-exists? (string-join `("GUI Images/profile-pics/" ,(fanAccount-username (getNameStruct)) ".png") ""))
                                (read-bitmap (string-join `("GUI Images/profile-pics/" ,(fanAccount-username (getNameStruct)) ".png") ""))
                                )
                               (else (read-bitmap "GUI Images/profile-pics/default_profile.png"))
                               )
                             )
                            ((bandAccount? (getNameStruct))
                             (cond
                               ((file-exists? (string-join `("GUI Images/profile-pics/" ,(bandAccount-username (getNameStruct)) ".png") ""))
                                (read-bitmap (string-join `("GUI Images/profile-pics/" ,(bandAccount-username (getNameStruct)) ".png") ""))
                                )
                               (else (read-bitmap "GUI Images/profile-pics/default_profile.png"))
                               )
                             )
                            )
                          )
  )

#|----- Fan Profile Things-----|#

(define alreadyAdded? (λ (fanUsername eventID)
                        (define result #f)
                        (for ((save fanSaved))
                          #:final (and (equal? fanUsername (fanSave-fanusername save)) (equal? eventID (fanSave-eventid save)))
                          (cond
                            ((and (equal? fanUsername (fanSave-fanusername save)) (equal? eventID (fanSave-eventid save)))
                             (set! result #t))
                            )
                          )
                        result
                        )            
  )

(define getFanEvents (λ ()
                       (define events '())
                       (define filteredList (filter (λ (elem) (equal? (first (rest loggedin?)) (fanSave-fanusername elem))) fanSaved))
                       (define ids '())
                       (for ((id filteredList))
                         (set! ids (append ids `(,(fanSave-eventid id))))
                         )

                       (define userEventList '())
                       (for ((i ids))
                         (set! events (filter (λ (elem) (equal? i (event-eventid elem))) eventList))
                         (set! userEventList (append userEventList events))
                         )
                       userEventList
                       )
  )

(define getBandEvents (λ ()
                        (filter (λ (elem) (equal? (first (rest loggedin?)) (event-bandusername elem))) eventList) 
                        )
  )

(define getBandName (λ ()
                      (bandAccount-bandname (first (filter (λ (elem) (equal? (first (rest loggedin?)) (bandAccount-username elem))) bandList)))
                      )
  )

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
                       (send buttonHolder add-child logoutbut)
                       (send buttonHolder add-child profilebut)
                       )
                      (else
                       (send buttonHolder delete-child logoutbut)
                       (send buttonHolder delete-child profilebut)
                       (send buttonHolder add-child loginbut)
                       (send buttonHolder add-child sigupbut)
                       )
                      )
                    )
  )

(define clearSearch (λ ()
                      (send filterDrop set-selection 0)
                      (send searchAlways set-value "")
                      )
  )

(define toHome (λ ()
                 (clearMain)
                 (send main add-child homePanel)
                 (send statePage set-label "Home")
                 (clearSearch)
                 )
  )

(define toLogin (λ ()
                  (send loginMessenger set-label "")
                  (send loginUsername set-value "")
                  (send loginPassword set-value "")
                  (clearMain)
                  (clearSearch)
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
                            (cond
                              ((equal? (first (rest check)) "band")
                               (send naviButtons delete-child savedButton)
                               (send naviButtons add-child concertButton))
                              )
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
                   (clearSearch)
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
                           (define writer (open-output-file "DB/fans.txt" #:mode 'text #:exists 'append)) 
                           (display (string-join `(,(send regiFanUsername get-value) ,(send regiFanPassword get-value) ,(send regiFanEmail get-value) ,(string-titlecase (send regiFanName get-value)) ,(string-join `("(" ,(string-titlecase (send regiFanSurname get-value)) ")") "")) " " #:before-first "\r") writer)
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
                           (display (string-join `(,(send regiBandUsername get-value) ,(send regiBandPassword get-value) ,(send regiBandEmail get-value) ,(string-join `("(" ,(string-titlecase (send regiBandName get-value)) ")") "")) " " #:before-first "\r") writer)
                           (close-output-port writer)
                           (toLogin)
                           )
                          
                          )
                        )
                       )

                     )
  )

(define verifyAdd (λ ()
                    (send inputErrMsg set-color #f)
                    (send inputErrMsg set-label "")
                    (define day "")
                    (define month "")
                    (define year "")
                    (define leap? 0)
                    (define hr 0)
                    (define min 0)

                    ;; Stops going through the conditions for some reason
                    (cond
                      ((equal? (send locationInput get-value) "")
                       (send inputErrMsg set-label "Please enter location.")(send inputErrMsg set-color "red"))
                      ((equal? (send dateInput get-value) "")
                       (send inputErrMsg set-label "Please enter date.")(send inputErrMsg set-color "red"))
                      ((equal? (send priceInput get-value) "")
                       (send inputErrMsg set-label "Please enter price.")(send inputErrMsg set-color "red"))
                      ((equal? (send timeInputHr get-value) "")
                       (send inputErrMsg set-label "Please enter hour.")(send inputErrMsg set-color "red"))
                      ((equal? (send timeInputMin get-value) "")
                       (send inputErrMsg set-label "Please enter minutes.")(send inputErrMsg set-color "red"))

                      
                      ((empty? (regexp-match* #rx"[a-zA-Z]+" (send locationInput get-value)))
                       (send inputErrMsg set-label "Venue input contains no letters. Please ensure you've entered it correctly.")(send inputErrMsg set-color "red"))
                      ((empty? (regexp-match* #rx"[0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]" (send dateInput get-value)))
                       (send inputErrMsg set-label "Date not in correct format.")(send inputErrMsg set-color "red"))
                      
                      ((not (empty? (regexp-match* #rx"[0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]" (send dateInput get-value))))
                       
                       (for ((i (send dateInput get-value)) (j (string-length (send dateInput get-value))))
                         (cond
                           ((not (equal? (string i) "-"))
                            (cond
                              ((< j 2)
                               (set! day (string-append day (string i))))
                              ((< j 5)
                               (set! month (string-append month (string i))))
                              (else (set! year (string-append year (string i))))
                              )
                            )
                           )
                         )
                       
                       (set! day (string->number day))
                       (set! month (string->number month))
                       (set! year (string->number year))

                       (cond
                         ((= (modulo year 100) 0)
                          (cond
                            ((= (modulo year 400) 0)
                             (set! leap? 29))
                            )
                          )

                         ((= (modulo year 4) 0)
                          (set! leap? 29))
                         (else (set! leap? 28))
                         )

                       (cond
                         ((> month 12)
                          (send inputErrMsg set-label "Inputted month too high.")(send inputErrMsg set-color "red"))
                         ((> day 31)
                          (send inputErrMsg set-label "Inputted day too high.")(send inputErrMsg set-color "red"))
                         ((< day 0)
                          (send inputErrMsg set-label "Inputted day too low.")(send inputErrMsg set-color "red"))
                         ((> day 31)
                          (send inputErrMsg set-label "Inputted day too high.")(send inputErrMsg set-color "red"))
                         ((and (or (= month 4) (= month 6) (= month 9) (= month 11)) (> day 30))
                          (send inputErrMsg set-label "Inputted day too high.")(send inputErrMsg set-color "red"))
                         ((and (= month 2) (> day leap?))
                          (send inputErrMsg set-label "Inputted day too high.")(send inputErrMsg set-color "red"))
                         ((< year 2024)
                          (send inputErrMsg set-label "Year too old.")(send inputErrMsg set-color "red"))
                         ((or (> (string-length (send timeInputHr get-value)) 2) (> (string-length (send timeInputMin get-value)) 2))
                          (send inputErrMsg set-label "Too many digits in time input.")(send inputErrMsg set-color "red"))
                         ((not (empty? (regexp-match* #rx"[^0-9]+" (send timeInputHr get-value))))
                          (send inputErrMsg set-label "Non-numerical characters in time input.")(send inputErrMsg set-color "red"))
                         ((not (empty? (regexp-match* #rx"[^0-9]+" (send timeInputMin get-value))))
                          (send inputErrMsg set-label "Non-numerical characters in time input.")(send inputErrMsg set-color "red"))
                         ((and (empty? (regexp-match* #rx"[^0-9]+" (send timeInputHr get-value))) (empty? (regexp-match* #rx"[^0-9]+" (send timeInputMin get-value))))
                          (set! hr (string->number (send timeInputHr get-value)))
                          (set! min (string->number (send timeInputMin get-value)))

                          (cond
                            ((> hr 23)
                             (send inputErrMsg set-label "Hour exceeds 24-hr limit.")(send inputErrMsg set-color "red"))
                            ((> min 59)
                             (send inputErrMsg set-label "Minutes exceed 60-sec limit.")(send inputErrMsg set-color "red"))
                      


                            ((not (empty? (regexp-match* #rx"(?!\\.)[^0-9]+" (send priceInput get-value))))
                             (send inputErrMsg set-label "Bad value in price input.")(send inputErrMsg set-color "red"))
                            ((empty? (regexp-match* #rx"[0-9]+\\.[0-9][0-9]" (send priceInput get-value)))
                             (send inputErrMsg set-label "Incorrect pricing format.")(send inputErrMsg set-color "red"))
                            ((> (length (regexp-match* #rx"[0-9]+\\.[0-9][0-9]" (send priceInput get-value))) 1)
                             (send inputErrMsg set-label "Incorrect pricing format.")(send inputErrMsg set-color "red"))
                            ((not (equal? (string (string-ref (send priceInput get-value) (- (string-length (send priceInput get-value)) 3))) "."))
                             (send inputErrMsg set-label "Incorrect pricing format.")(send inputErrMsg set-color "red"))
                         
                            (else
                             (define nextID (add1 (string->number (event-eventid (first (reverse eventList))))))

                             (cond
                               ((= (string-length (number->string nextID)) 1)
                                (set! nextID (string-append "000" (number->string nextID))))
                               ((= (string-length (number->string nextID)) 2)
                                (set! nextID (string-append "00" (number->string nextID))))
                               ((= (string-length (number->string nextID)) 3)
                                (set! nextID (string-append "0" (number->string nextID))))
                               (else
                                (set! nextID (number->string nextID)))
                               )
                             
                             (set! eventList
                                   (append eventList
                                           `(
                                             ,(event
                                               nextID
                                               (first (rest loggedin?))
                                               (send locationInput get-value)
                                               (send dateInput get-value)
                                               (string-join `(,(send timeInputHr get-value) ":" ,(send timeInputMin get-value)) "")
                                               (send priceInput get-value)
                                               "Available"
                                               )
                                             )
                                           )
                                   )

                             (define writer (open-output-file "DB/events.txt" #:mode 'text #:exists 'append)) 
                             (display (string-join `(,nextID ,(first (rest loggedin?)) ,(string-join `("(" ,(string-titlecase (send locationInput get-value)) ")") "") ,(send dateInput get-value) ,(string-join `(,(send timeInputHr get-value) ":" ,(send timeInputMin get-value)) "") ,(send priceInput get-value) "a") " " #:before-first "\r") writer)
                             (close-output-port writer)

                             (toBandConcerts)
                             )
                            )
                          )
                         )
                       )
                      )
                    )
  )
                       
                      
                       

(define checkHeight ;; Responsive height in comparison to amount of results found. (Spacing)
  (λ (a b)
    (cond
      ((= (length (send a get-children)) 0)
       (send b min-height 1))
      ((<= (length (send a get-children)) 3)
       (send b min-height (abs (round (* (quotient (send a get-height) 3) 2)))))
      ((and (> (length (send a get-children)) 3) (<= (length (send a get-children)) 6))
       (send b min-height (abs (round (quotient(send a get-height) 3)))))
      (else
       (send b min-height 0))
      )
    )                            
  )


(define do-to-Search (λ ()
                       (clearMain)
                       (send statePage set-label "Searching for events...")
                       (send main add-child searchMain)
                       (send attemptSearch set-countUp 0)
                       (send attemptSearch set-filterStatus (send filterDrop get-selection))
                       (send attemptSearch set-value (send searchAlways get-value))
                       (send attemptSearch do)
                       (send attemptSearch makeBoxes)
                       (send attemptSearch add-child boxSpaceSearch)
                       (checkHeight attemptSearch boxSpaceSearch)
                       )
  )



(define toFanProfile (λ ()
                       (clearMain)
                       (send statePage set-label "My Profile")
                       (send main add-child fanOPmain)
                       (send pfp set-label (getProfilePhoto))
                       (send fanNameDisplay set-label (string-join `(,(fanAccount-name (getNameStruct)) ,(fanAccount-surname (getNameStruct))) " "))
                       (send fanUNDisplay set-label (string-join `("@" ,(fanAccount-username (getNameStruct))) ""))
                       )
  )

(define loggingOut (λ ()
                     (cond
                       ((equal? (first (reverse loggedin?)) "band")
                        (send naviButtons delete-child concertButton)
                        (send naviButtons add-child savedButton)
                        )
                       )
                     (set! loggedin? '(#f "" ""))
                     (toHome)
                     (changeLSP)
                     (send pfp set-label (make-bitmap 192 192 #t))
                     )
  )

(define toSavedList (λ ()
                      (cond
                        ((and (equal? (first loggedin?) #t) (equal? (first (reverse loggedin?)) "fan"))
                         (clearMain)
                         (send statePage set-label "My Saved Events")
                         (send main add-child fanSaveListMain)
                         (send fanSaveList refreshList)
                         (send fanSaveList makeList)
                         (send fanSaveList add-child pushups)
                         (checkHeight fanSaveList pushups)
                         )
                        (else (toLogin))
                        )

                      )
  )

(define toBandConcerts (λ ()
                         (clearMain)
                         (send statePage set-label "My Listed Concerts")
                         (send main add-child bandConcertsMain)
                         (send bandMadeList refreshList)
                         (send bandMadeList makeList)
                         (send bandMadeList add-child pushupper)
                         (checkHeight bandMadeList pushupper)
                         )
  )

(define updateEventFile (λ ()
                          (define letter "")
                          (define writer (open-output-file "DB/events.txt" #:mode 'text #:exists 'truncate))
                          (close-output-port writer)
                          
                          (define writing (open-output-file "DB/events.txt" #:mode 'text #:exists 'append))
                          (for ((i eventList)(j (length eventList)))
                            (cond
                              ((equal? (event-status i) "Available")
                               (set! letter "a"))
                              ((equal? (event-status i) "Booked")
                               (set! letter "b"))
                              ((equal? (event-status i) "Cancelled")
                               (set! letter "c"))
                              )
                               
                            (cond
                              ((equal? j 0)
                               (display (string-join `(,(event-eventid i) ,(event-bandusername i) ,(string-join `("(" ,(event-venue i) ")") "") ,(event-date i) ,(event-time i) ,(event-price i) ,letter) " ") writing))
                              (else
                               (display (string-join `(,(event-eventid i) ,(event-bandusername i) ,(string-join `("(" ,(event-venue i) ")") "") ,(event-date i) ,(event-time i) ,(event-price i) ,letter) " " #:before-first "\r") writing))
                              )
                            )
                          (close-output-port writing)
                          )
  )

(define setAvailable (λ (struct)
                       (for ((i eventList))
                         (cond
                           ((equal? i struct)
                            (set-event-status! i "Available")
                            (updateEventFile)
                            (toBandConcerts)
                            )
                          )
                         )
                       )
  )

(define setBooked (λ (struct)
                       (for ((i eventList))
                         (cond
                           ((equal? i struct)
                            (set-event-status! i "Booked")
                            (updateEventFile)
                            (toBandConcerts)
                            )
                          )
                         )
                       )
  )

(define setCancelled (λ (struct)
                       (for ((i eventList))
                         (cond
                           ((equal? i struct)
                            (set-event-status! i "Cancelled")
                            (updateEventFile)
                            (toBandConcerts)
                            )
                          )
                         )
                       )
  )

(define toAddNew (λ ()
                   (clearMain)
                   (send main add-child createEventMain)
                   (send bandNameInput set-value (getBandName))
                   (send locationInput set-value "")
                   (send dateInput set-value "")
                   (send timeInputHr set-value "")
                   (send timeInputMin set-value "")
                   (send priceInput set-value "")
                   )
  )

#|--------------------------------------------------------------------- End: Various Program Functions  -----------------------------------------------------------------------------|#

#|-------------------------------------------------------------------------------------- Start: Custom Classes ------------------------------------------------------------------------------------|#

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
                                               (set! objectList (append objectList
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
                                             objectList
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

(define searchResultBox% (class vertical-panel%
                           (super-new)

                           (init-field filterStatus)
                           (init-field value)
                           (inherit get-children)
                           (inherit delete-child)

                           (define holdResults '())
                           (define holdObjects '())
                           (define holdids '())
                           (define holdBandAccount '())
                           (define name "")
                           (define setup '())
                           (define countUp 0)
                           (define messagesList '())

                           (define/public do (λ ()
                                               (set! holdResults '())
                                               (set! holdObjects '())
                                               (set! name "")
                                               (set! setup '())
                                               (cond  
                                                 ((= filterStatus 1)
                                                  (for ((i bandList))
                                                    (cond
                                                      ((equal? (bandAccount-bandname i) (string-titlecase value))
                                                       (set! name (bandAccount-bandname i))
                                                       (set! holdResults (filter (λ (elem) (equal? (event-bandusername elem) (bandAccount-username i))) eventList))
                                                       )
                                                      )
                                                    )
                                                  )
                                                 ((= filterStatus 2)
                                                  
                                                  (set! holdResults (filter (λ (elem) (equal? (event-date elem) value)) eventList))
                                                  )
                                                 )
                                               )
                             )

                           (define/public makeBoxes (λ ()
                                                      (for ((i (send this get-children)))
                                                        (send this delete-child i)
                                                        )
                                                      (send searchError set-color #f)
                                                      (send searchError set-label "")

                                                      (cond
                                                        ((or (boolean=? (first loggedin?) #f) (equal? (first (reverse loggedin?)) "band"))
                                                         (cond
                                                           ((not (empty? holdResults))
                                                            #|-------------------------------|#
                                                            (for ((i holdResults))
                                                              
                                                              (set! countUp (add1 countUp))
                                                              
                                                              (new message%
                                                                   [parent this]
                                                                   [label (string-append "Result " (number->string countUp))]
                                                                   [horiz-margin 20]
                                                                   [vert-margin 5]
                                                                   )
                                                              
                                                              (set! holdObjects (append holdObjects `(
                                                                                                      ,(new vertical-panel%
                                                                                                            [parent this]
                                                                                                            [alignment '(left top)]
                                                                                                            [style '(border)]
                                                                                                            [horiz-margin 20]
                                                                                                            [vert-margin 10]
                                                                                                            )
                                                                                                      )
                                                                                        )
                                                                    )
                                                              )

                                                            (send searchError set-label (string-append (string-append "Found " (number->string (length holdObjects))) " matches."))
                                                            #|-------------------------------|#
                                                            (for ((i holdObjects)(j holdResults))
                                                              (set! messagesList (append messagesList `((
                                                                                                         ,(new message%
                                                                                                               [parent i]
                                                                                                               [label  (string-append "Band: " name)]
                                                                                                               [horiz-margin 4]
                                                                                                               [vert-margin 4]
                                                                                                               )
                                                                                                         ,(new message%
                                                                                                               [parent i]
                                                                                                               [label (string-append "Location: " (event-venue j))]
                                                                                                               [horiz-margin 4]
                                                                                                               [vert-margin 4]
                                                                                                               )
                                                                                                         ,(new message%
                                                                                                               [parent i]
                                                                                                               [label (string-append "Date: " (event-date j))]
                                                                                                               [horiz-margin 4]
                                                                                                               [vert-margin 4]
                                                                                                               )
                                                                                                         ,(new message%
                                                                                                               [parent i]
                                                                                                               [label (string-append "@ " (event-time j))]
                                                                                                               [horiz-margin 4]
                                                                                                               [vert-margin 4]
                                                                                                               )
                                                                                                         ,(new message%
                                                                                                               [parent i]
                                                                                                               [label (string-append "Price: £" (event-price j))]
                                                                                                               [horiz-margin 4]
                                                                                                               [vert-margin 4]
                                                                                                               )
                                                                                                         ,(new message%
                                                                                                               [parent i]
                                                                                                               [label (event-status j)]
                                                                                                               [horiz-margin 4]
                                                                                                               [vert-margin 4]
                                                                                                               )
                                                                                                         ) )
                                                                                         
                                                                                         )
                                                                    )
                                                              )
                                                            setup
                                                            )
                                                           (else (send searchError set-label "No results found or band may not have any events. Remember to filter your query.") (send searchError set-color "red")
                                                                 )
                                                           )
                                                         )
                                                        ((and (boolean=? (first loggedin?) #t) (equal? (first (reverse loggedin?)) "fan"))
                                                         (cond
                                                           ((not (empty? holdResults))
                                                            #|-------------------------------|#
                                                            (for ((i holdResults))
                                                              (set! countUp (add1 countUp))
                                                              
                                                              (new message%
                                                                   [parent this]
                                                                   [label (string-append "Result " (number->string countUp))]
                                                                   [horiz-margin 20]
                                                                   [vert-margin 5]
                                                                   )
                                                              
                                                              (set! holdObjects (append holdObjects `(
                                                                                                      ,(new vertical-panel%
                                                                                                            [parent this]
                                                                                                            [alignment '(left top)]
                                                                                                            [style '(border)]
                                                                                                            [horiz-margin 20]
                                                                                                            [vert-margin 10]
                                                                                                            )
                                                                                                      )
                                                                                        )
                                                                    )
                                                              )
                                                            #|-------------------------------|#

                                                            (define saved? #f)
                                                              
                                                            (for ((i holdObjects)(j holdResults))
                                                              
                                                              (set! saved? (alreadyAdded? (first (rest loggedin?)) (event-eventid j)))
                                                              
                                                              (set! messagesList (append messagesList `((
                                                                                                         ,(new message%
                                                                                                               [parent i]
                                                                                                               [label (string-append "Band: " name)]
                                                                                                               [horiz-margin 4]
                                                                                                               [vert-margin 4]
                                                                                                               )
                                                                                                         ,(new message%
                                                                                                               [parent i]
                                                                                                               [label (string-append "Location: " (event-venue j))]
                                                                                                               [horiz-margin 4]
                                                                                                               [vert-margin 4]
                                                                                                               )
                                                                                                         ,(new message%
                                                                                                               [parent i]
                                                                                                               [label (string-append "Date: " (event-date j))]
                                                                                                               [horiz-margin 4]
                                                                                                               [vert-margin 4]
                                                                                                               )
                                                                                                         ,(new message%
                                                                                                               [parent i]
                                                                                                               [label (string-append "@ " (event-time j))]
                                                                                                               [horiz-margin 4]
                                                                                                               [vert-margin 4]
                                                                                                               )
                                                                                                         ,(new message%
                                                                                                               [parent i]
                                                                                                               [label (string-append "Price: £" (event-price j))]
                                                                                                               [horiz-margin 4]
                                                                                                               [vert-margin 4]
                                                                                                               )
                                                                                                         ,(new message%
                                                                                                               [parent i]
                                                                                                               [label (event-status j)]
                                                                                                               [horiz-margin 4]
                                                                                                               [vert-margin 4]
                                                                                                               )
                                                                                                         ))
                                                                                         
                                                                                         )
                                                                    )
                                                              (cond
                                                                ((and (equal? (first (reverse loggedin?)) "fan") saved?)
                                                                 (new message%
                                                                      [parent i]
                                                                      [label "Already saved to your list"]
                                                                      [color "red"]
                                                                      [horiz-margin 4]
                                                                      [vert-margin 4]
                                                                      )
                                                                 )
                                                                                                            
                                                                ((and (equal? (first (reverse loggedin?)) "fan") (not saved?))
                                                                 (new updatingButton%
                                                                      [parent i]
                                                                      [label "Save"]
                                                                      [horiz-margin 100]
                                                                      [vert-margin 10]
                                                                      [extra (event-eventid j)]
                                                                      [callback (λ (o e)
                                                                                  (set! fanSaved (append fanSaved `(,(fanSave (first (rest loggedin?)) (event-eventid j)))))
                                                                                  (define writer (open-output-file "DB/fansaves.txt" #:mode 'text #:exists 'append))
                                                                                  (display (string-join `(,(first (rest loggedin?)) ,(event-eventid j)) " " #:before-first "\r") writer)
                                                                                  (close-output-port writer)
                                                                                  (do-to-Search)
                                                                                  )
                                                                                ]
                                                                      )
                                                                 )
                                                                                                            
                                                                )
                                                              )
                                                            
                                                            setup ;;?? I thought I would need to return the list of objects but uh, guess not. it works so i won't bother optimizing haha
                                                            )
                                                           (else (send searchError set-label "No results found or band may not have any events. Remember to filter your query.") (send searchError set-color "red")
                                                                 )
                                                           )
                                                         )
                                                        )
                                                      )
                             )

                           (define/public set-filterStatus (λ (filt)
                                                             (set! filterStatus filt)
                                                             )
                             )

                           (define/public set-value (λ (val)
                                                      (set! value val)
                                                      )
                             )

                           (define/public set-countUp (λ (n)
                                                        (set! countUp n)
                                                        )
                             )
                                                    
                           )
  )


(define eventDisplay% (class vertical-panel%
                        (super-new)
                        (define boxObjects '())
                        (define countUp 0)
                        (define messagesList '())
                        (define name "")
                        
                        (define userEvents '())

                        (define/public refreshList (λ ()
                                                     (set! userEvents '())
                                                     (cond
                                                       ((equal? (first loggedin?) #t)
                                                        (set! userEvents (append userEvents (getFanEvents)))
                                                        )
                                                       )
                                                     )
                          )

                        (define/public makeList (λ ()
                                                  (for ((i (send this get-children)))
                                                    (send this delete-child i)
                                                    )

                                                  (set! boxObjects '())
                                                  (set! countUp 0)
                                                  (set! messagesList '())
                                                  (set! name "")
                                                  
                                                  (cond
                                                    ((empty? userEvents)
                                                     (send nothinghere set-label "No events saved yet.")
                                                     (send nothinghere set-color "red")
                                                     )

                                                    (else
                                                     (send nothinghere set-color #f)
                                                     (send nothinghere set-label "")
                                                     
                                                     (for ((i userEvents))
                                                       (set! countUp (add1 countUp))

                                                       
                                                              
                                                       (new message%
                                                            [parent this]
                                                            [label (string-append "Save " (number->string countUp))]
                                                            [horiz-margin 20]
                                                            [vert-margin 5]
                                                            )
                                                              
                                                       (set! boxObjects (append boxObjects `(
                                                                                             ,(new vertical-panel%
                                                                                                   [parent this]
                                                                                                   [alignment '(left top)]
                                                                                                   [style '(border)]
                                                                                                   [horiz-margin 20]
                                                                                                   [vert-margin 10]
                                                                                                   )
                                                                                             )
                                                                                )
                                                             )
                                                       )
                                                     #|-------------------------------|#

                                                     (define saved? #f)
                                                              
                                                     (for ((i boxObjects)(j userEvents))
                                                       (set! name (bandAccount-bandname (first (filter (λ (elem) (equal? (event-bandusername j) (bandAccount-username elem))) bandList))))
                                                              
                                                       (set! messagesList (append messagesList `((
                                                                                                  ,(new message%
                                                                                                        [parent i]
                                                                                                        [label (string-append "Band: " name)]
                                                                                                        [horiz-margin 10]
                                                                                                        [vert-margin 10]
                                                                                                        )
                                                                                                  ,(new message%
                                                                                                        [parent i]
                                                                                                        [label (string-append "Location: " (event-venue j))]
                                                                                                        [horiz-margin 10]
                                                                                                        [vert-margin 10]
                                                                                                        )
                                                                                                  ,(new message%
                                                                                                        [parent i]
                                                                                                        [label (string-append "Date: " (event-date j))]
                                                                                                        [horiz-margin 10]
                                                                                                        [vert-margin 10]
                                                                                                        )
                                                                                                  ,(new message%
                                                                                                        [parent i]
                                                                                                        [label (string-append "@ " (event-time j))]
                                                                                                        [horiz-margin 10]
                                                                                                        [vert-margin 10]
                                                                                                        )
                                                                                                  ,(new message%
                                                                                                        [parent i]
                                                                                                        [label (string-append "Price: £" (event-price j))]
                                                                                                        [horiz-margin 10]
                                                                                                        [vert-margin 10]
                                                                                                        )
                                                                                                  ,(new message%
                                                                                                        [parent i]
                                                                                                        [label (event-status j)]
                                                                                                        [horiz-margin 10]
                                                                                                        [vert-margin 10]
                                                                                                        )
                                                                                                  ,(new button%
                                                                                                        [parent i]
                                                                                                        [label "Remove from List"]
                                                                                                        [horiz-margin 10]
                                                                                                        [vert-margin 10]
                                                                                                        [callback
                                                                                                         (λ (o e)
                                                                                                           (define newList '())
                                                                                                           (define push '())

                                                                                                           (for ((i fanSaved))
                                                                                                             (cond
                                                                                                               ((not (and (equal? (first (rest loggedin?)) (fanSave-fanusername i))(equal? (event-eventid j) (fanSave-eventid i))))
                                                                                                                (set! newList (append newList (list i)))
                                                                                                                )
                                                                                                               (else (set! push i))
                                                                                                               )
                                                                                                             )
                                                                                                           (set! fanSaved newList)
                                                                                                           (define writer (open-output-file "DB/fansaves.txt" #:mode 'text #:exists 'truncate))
                                                                                                           (close-output-port writer)
                                                                                                           (define writing (open-output-file "DB/fansaves.txt" #:mode 'text #:exists 'append))
                                                                                                           (define counter 0)
                                                                                                           (for ((i fanSaved))
                                                                                                             (cond
                                                                                                               ((= counter 0)
                                                                                                                (display (string-join `(,(fanSave-fanusername i) ,(fanSave-eventid i)) " ") writing)
                                                                                                                (set! counter (add1 counter))
                                                                                                                )
                                                                                                               (else
                                                                                                                (display (string-join `(,(fanSave-fanusername i) ,(fanSave-eventid i)) " " #:before-first "\r") writing)
                                                                                                                )
                                                                                                               )
                                                                                                             )
                                                                                                           (close-output-port writing)
                                                                                                           (toSavedList)
                                                                                                           )
                                                                                                         ]
                                                                                                             
                                                                                                        )
                                                                                                  ))
                                                                                         
                                                                                  )
                                                             )
                                                       )
                                                     )
                        
                                                    )
                                                  )
                          )
                        )
  )


(define concertDisplay% (class vertical-panel%
                          (super-new)
                          (define boxObjects '())
                          (define countUp 0)
                          (define messagesList '())
                          (define name "")
                        
                          (define bandEvents '())

                          (define/public refreshList (λ ()
                                                       (set! bandEvents (getBandEvents))
                                                       )
                            )

                          (define/public makeList (λ ()
                                                    (for ((i (send this get-children)))
                                                      (send this delete-child i)
                                                      )

                                                    (set! boxObjects '())
                                                    (set! countUp 0)
                                                    (set! messagesList '())
                                                    (set! name (bandAccount-bandname (first (filter (λ (elem) (equal? (bandAccount-username elem) (first (rest loggedin?)))) bandList))))
                                                  
                                                    (cond
                                                      ((empty? bandEvents)
                                                       (send ifempty set-label "No events saved yet.")
                                                       (send ifempty set-color "red")
                                                       )

                                                      (else
                                                       (send ifempty set-color #f)
                                                       (send ifempty set-label "")
                                                     
                                                       (for ((i bandEvents))
                                                         (set! countUp (add1 countUp))

                                                       
                                                              
                                                         (new message%
                                                              [parent this]
                                                              [label (string-append "Concert " (number->string countUp))]
                                                              [horiz-margin 20]
                                                              [vert-margin 5]
                                                              )
                                                              
                                                         (set! boxObjects (append boxObjects `(
                                                                                               ,(new vertical-panel%
                                                                                                     [parent this]
                                                                                                     [alignment '(left top)]
                                                                                                     [style '(border)]
                                                                                                     [horiz-margin 20]
                                                                                                     [vert-margin 10]
                                                                                                     )
                                                                                               )
                                                                                  )
                                                               )
                                                         )
                                                       #|-------------------------------|#

                                                       (define saved? #f)
                                                              
                                                       (for ((i boxObjects)(j bandEvents))
                                                       
                                                              
                                                         (set! messagesList (append messagesList `((
                                                                                                    ,(new message%
                                                                                                          [parent i]
                                                                                                          [label (string-append "Band: " name)]
                                                                                                          [horiz-margin 4]
                                                                                                          [vert-margin 4]
                                                                                                          )
                                                                                                    ,(new message%
                                                                                                          [parent i]
                                                                                                          [label (string-append "Location: " (event-venue j))]
                                                                                                          [horiz-margin 4]
                                                                                                          [vert-margin 4]
                                                                                                          )
                                                                                                    ,(new message%
                                                                                                          [parent i]
                                                                                                          [label (string-append "Date: " (event-date j))]
                                                                                                          [horiz-margin 4]
                                                                                                          [vert-margin 4]
                                                                                                          )
                                                                                                    ,(new message%
                                                                                                          [parent i]
                                                                                                          [label (string-append "@ " (event-time j))]
                                                                                                          [horiz-margin 4]
                                                                                                          [vert-margin 4]
                                                                                                          )
                                                                                                    ,(new message%
                                                                                                          [parent i]
                                                                                                          [label (string-append "Price: £" (event-price j))]
                                                                                                          [horiz-margin 4]
                                                                                                          [vert-margin 4]
                                                                                                          )
                                                                                                    ,(new message%
                                                                                                          [parent i]
                                                                                                          [label (event-status j)]
                                                                                                          [horiz-margin 4]
                                                                                                          [vert-margin 4]
                                                                                                          )
                                                                                                    ,(new horizontal-panel%
                                                                                                          [parent i]
                                                                                                          [alignment '(center top)]
                                                                                                          [horiz-margin 4]
                                                                                                          [vert-margin 4]   
                                                                                                          )
                                                                                                 
                                                                                                    ))
                                                                                         
                                                                                    )
                                                               )
                                                         `,(new button%
                                                                [parent (first (reverse (first (reverse messagesList))))]
                                                                [label "Edit This Concert"]
                                                                [horiz-margin 10]
                                                                [vert-margin 10]
                                                                )
                                                         (cond
                                                           ((equal? (event-status j) "Available")
                                                            `,(new button%
                                                                   [parent (first (reverse (first (reverse messagesList))))]
                                                                   [label "Set to Booked"]
                                                                   [horiz-margin 10]
                                                                   [vert-margin 10]
                                                                   [callback (λ (o e)
                                                                               (setBooked j)
                                                                               )
                                                                             ]
                                                                   )
                                                            `,(new button%
                                                                   [parent (first (reverse (first (reverse messagesList))))]
                                                                   [label "Set to Cancelled"]
                                                                   [horiz-margin 10]
                                                                   [vert-margin 10]
                                                                   [callback (λ (o e)
                                                                               (setCancelled j)
                                                                               )
                                                                             ]
                                                                   )
                                                            )
                                                           ((equal? (event-status j) "Booked")
                                                            `,(new button%
                                                                   [parent (first (reverse (first (reverse messagesList))))]
                                                                   [label "Set to Available"]
                                                                   [horiz-margin 10]
                                                                   [vert-margin 10]
                                                                   [callback (λ (o e)
                                                                               (setAvailable j)
                                                                               )
                                                                             ]
                                                                   )
                                                            `,(new button%
                                                                   [parent (first (reverse (first (reverse messagesList))))]
                                                                   [label "Set to Cancelled"]
                                                                   [horiz-margin 10]
                                                                   [vert-margin 10]
                                                                   [callback (λ (o e)
                                                                               (setCancelled j)
                                                                               )
                                                                             ]
                                                                   )
                                                            )
                                                           ((equal? (event-status j) "Cancelled")
                                                            `,(new button%
                                                                   [parent (first (reverse (first (reverse messagesList))))]
                                                                   [label "Set to Available"]
                                                                   [horiz-margin 10]
                                                                   [vert-margin 10]
                                                                   [callback (λ (o e)
                                                                               (setAvailable j)
                                                                               )
                                                                             ]
                                                                   )
                                                            `,(new button%
                                                                   [parent (first (reverse (first (reverse messagesList))))]
                                                                   [label "Set to Booked"]
                                                                   [horiz-margin 10]
                                                                   [vert-margin 10]
                                                                   [callback (λ (o e)
                                                                               (setBooked j)
                                                                               )
                                                                             ]
                                                                   )
                                                            )
                                                           )
                                                         )
                                                     
                                                       )
                        
                                                      )
                                                    )
                            )
                          )
  )

#|-------------------------------------------------------------------------------------- End: Custom Classes ------------------------------------------------------------------------------------|#

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

(define logoutbut (new button%
                       [parent buttonHolder]
                       [label "Logout"]
                       [horiz-margin 2]
                       [vert-margin 0]
                       [min-width 80]
                       [min-height 25]
                       [style '(deleted)]
                       [callback
                        (λ (o e)
                          (loggingOut)
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
                        [callback (λ (o e)
                                    (cond
                                      ((equal? (first (reverse loggedin?)) "fan")
                                       (toFanProfile)
                                       )
                                      )
                                    )
                                  ]
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
                         [callback (λ (o e)
                                     (toSavedList)
                                     )
                                   ]
                         )
  )

(define concertButton (new button%
                           [parent naviButtons]
                           [label "My Concerts"]
                           [horiz-margin 2]
                           [vert-margin 0]
                           [style '(deleted)]
                           [callback
                            (λ (o e)
                              (toBandConcerts)
                              )
                            ]
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
                        [choices '("--Filter--" "Band" "Date")]
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

(define regbuttonfix (new horizontal-panel%
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

(define cancelButtonNumberSomething (new button%
                                         [parent regbuttonfix]
                                         [label "Cancel"]
                                         [vert-margin 25]
                                         [callback
                                          (λ (o e)
                                            (toHome)
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
                         [alignment '(left top)]
                         [style '(border)]
                         [horiz-margin 5]
                         [vert-margin 5]
                         )
  )

(define boxbox (new panel%
                    [parent sTopSection]
                    [min-height 20]
                    [min-width 20]
                    [style '(border)]
                    [horiz-margin 5]
                    [vert-margin 5]
                    )
  )


(define searchHeading (new vertical-panel%
                           [parent sTopSection]
                           [min-width 1180]
                           [alignment '(left center)]
                           
                           )
  )

(define searchHeadTxt (new message%
                           [parent searchHeading]
                           [label "Search"]
                           )
  )

(define searchHeadPssg (new message%
                            [parent searchHeading]
                            [label "Lookup events by date (DD-MM-YY) or by a band's name."]
                            )
  )




(define searchArea (new vertical-panel%
                        [parent searchMain]
                        [alignment '(left top)]
                        [min-height 700]
                        [horiz-margin 5]
                        )
  )

(define attemptSearch (new searchResultBox%
                           [parent searchArea]
                           [value (send searchAlways get-value)]
                           [filterStatus (send filterDrop get-selection)]
                           [alignment '(left top)]
                           [style '(vscroll)]
                           [min-height 600]
                           ;; its slow and it flickers but it works
                           )
  )



(define boxSpaceSearch (new horizontal-panel%
                            [parent attemptSearch]
                            [min-height 0]
                            [style '(deleted)]
                            )
  )

(define boxSpace (new horizontal-panel%
                      [parent searchArea]
                      [horiz-margin 5]
                      [alignment '(center center)]
                      )
  )

(define searchError (new message%
                         [parent boxSpace]
                         [color "red"]
                         [label ""]
                         [auto-resize #t]
                         )
  )

#|--------------End: Search Page------------|#

#|-------------Start: Fan Profile Page-----------|#

(define fanOPmain (new vertical-panel%
                       [parent main]
                       [alignment '(left top)]
                       [horiz-margin 0]
                       [vert-margin 0]
                       [style '(deleted)]
                       )
  )
                       
(define fanDetails (new horizontal-panel%
                        [parent fanOPmain]
                        [alignment '(center center)]
                        [style '(border)]
                        [horiz-margin 5]
                        [vert-margin 5]
                        )
  )

(define asdf (new panel%
                  [parent fanDetails]
                  [horiz-margin 20]
                  )
  )

(define pfp (new message%
                 [parent fanDetails]
                 [label (make-bitmap 192 192 #t)]
                 [auto-resize #t]
                 [horiz-margin 20]
                 [vert-margin 20]
                 )
  )

(define detailsTXT (new vertical-panel%
                        [parent fanDetails]
                        [alignment '(center center)]
                        [horiz-margin 20]
                        )
  )

(define fanNameDisplay (new message%
                            [parent detailsTXT]
                            [label ""]
                            [auto-resize  #t]
                           
                            )
  )

(define fanUNDisplay (new message%
                          [parent detailsTXT]
                          [label ""]
                          [auto-resize #t]
                          )
  )

(define ghjk (new panel%
                  [parent fanDetails]
                  [horiz-margin 20]
                  )
  )

(define fanArea (new horizontal-panel%
                     [parent fanOPmain]
                     [min-height 550]
                     [alignment '(left top)]
                     [style '(border)]
                     )
  )

;; I want to actually make a proper profile page but I definitely don't have enough time. 

#|-------------End: Fan Profile Page------------|#

#|-------------Start: Fan Saved List Page------------|#

(define fanSaveListMain (new vertical-panel%
                             [parent main]
                             [alignment '(left top)]
                             [horiz-margin 0]
                             [vert-margin 0]
                             [style '(deleted)]
                             )
  )

(define listHeader (new vertical-panel%
                        [parent fanSaveListMain]
                        [alignment '(center center)]
                        [style '(border)]
                        [horiz-margin 5]
                        [vert-margin 5]
                        )
  )

(define whereyouare (new message%
                         [parent listHeader]
                         [label "Your Saved Events"]
                         )
  )

(define nothinghere (new message%
                         [parent listHeader]
                         [label ""]
                         [color #f]
                         [auto-resize #t]
                         )
  )

(define listArea (new vertical-panel%
                      [parent fanSaveListMain]
                      [alignment '(left top)]
                      [min-height 800]
                      [horiz-margin 5]
                      )
  )

(define fanSaveList (new eventDisplay%
                         [parent listArea]
                         [alignment '(left top)]
                         [horiz-margin 0]
                         [vert-margin 0]
                         [style '(vscroll)]
                         [min-height 700]
                         )
  )

(define pushups (new horizontal-panel%
                     [parent fanSaveList]
                     [min-height 0]
                     [style '(deleted)]
                     )
  )


#|-------------End: Fan Saved List Page------------|#

#|--------------- Start: Band Concerts Page ------------------|#

(define bandConcertsMain (new vertical-panel%
                              [parent main]
                              [alignment '(left top)]
                              [horiz-margin 0]
                              [vert-margin 0]
                              [style '(deleted)]
                              )
  )

(define concertsHeader (new vertical-panel%
                            [parent bandConcertsMain]
                            [alignment '(center center)]
                            [style '(border)]
                            [horiz-margin 5]
                            [vert-margin 5]
                            )
  )

(define titler (new message%
                    [parent concertsHeader]
                    [label "Your Saved Events"]
                    )
  )

(define ifempty (new message%
                     [parent concertsHeader]
                     [label ""]
                     [color #f]
                     [auto-resize #t]
                     )
  )

(define concertsArea (new vertical-panel%
                          [parent bandConcertsMain]
                          [alignment '(left top)]
                          [min-height 800]
                          [horiz-margin 5]
                          )
  )

(define bandMadeList (new concertDisplay%
                         [parent concertsArea]
                         [alignment '(left top)]
                         [horiz-margin 0]
                         [vert-margin 0]
                         [style '(vscroll)]
                         [min-height 700]
                         )
  )

(define pushupper (new horizontal-panel%
                     [parent bandMadeList]
                     [min-height 0]
                     [style '(deleted)]
                     )
  )

(define addAtBottom (new horizontal-panel%
                         [parent concertsArea]
                         [alignment '(center center)]
                         [horiz-margin 5]
                         [vert-margin 2]
                         )
  )

(define addconbut (new button%
                       [parent addAtBottom]
                       [label "Create New Listing"]
                       [callback
                        (λ (o e)
                          (toAddNew)
                          )
                        ]
                       )
  )

#|------------------End: Band Concerts Page-------------------|#

#|------------------Start: Add Concerts Page-------------------|#

(define createEventMain (new vertical-panel%
                          [parent main]
                          [style '(deleted)]
                          [vert-margin 5]
                          )
  )

(define createEventHeader (new horizontal-panel%
                               [parent createEventMain]
                               [horiz-margin 5]
                               [style '(border)]
                               [alignment '(center center)]
                               )
  )

(define addTitle (new message%
                      [label "Create Concert Listing"]
                      [parent createEventHeader]
                      )
  )

(define createConcertArea (new horizontal-panel%
                               [parent createEventMain]
                               [horiz-margin 5]
                               [vert-margin 5]
                               [alignment '(center center)]
                               [min-height 800]
                               )
  )

(define spacer1 (new panel%
                     [parent createConcertArea]
                     )
  )

(define inputArea (new vertical-panel%
                       [parent createConcertArea]
                       [alignment '(center center)]
                       [style '(border)]
                       [min-width 600]
                       )
  )

(define changeAlign (new panel%
                         [parent inputArea]
                         [alignment '(left center)]
                         [horiz-margin 80]
                         [vert-margin 40]
                         )
  )

(define moretitling (new message%
                         [parent changeAlign]
                         [label "Enter new concert details"]
                         )
  )

(define bandNameInput (new text-field%
                             [parent inputArea]
                             [init-value ""]
                             [style '(single vertical-label)]
                             [label "Band Name: "]
                             [horiz-margin 80]
                             [vert-margin 5]
                             [enabled #f]
                             )
  )

(define locationInput (new text-field%
                             [parent inputArea]
                             [init-value ""]
                             [style '(single vertical-label)]
                             [label "Location Venue: "]
                             [horiz-margin 80]
                             [vert-margin 5]
                             )
  )

(define dateInput (new text-field%
                             [parent inputArea]
                             [init-value ""]
                             [style '(single vertical-label)]
                             [label "Date (DD-MM-YY): "]
                             [horiz-margin 80]
                             [vert-margin 5]
                             )
  )
(define priceInput (new text-field%
                             [parent inputArea]
                             [init-value ""]
                             [style '(single vertical-label)]
                             [label "Price (£): "]
                             [horiz-margin 80]
                             [vert-margin 5]
                             )
  )
(define timeMixer (new horizontal-panel%
                       [parent inputArea]
                       [alignment '(left top)]
                       [horiz-margin 80]
                       [vert-margin 5]
                       )
  )

(define timeInputHr (new text-field%
                             [parent timeMixer]
                             [init-value ""]
                             [style '(single vertical-label)]
                             [label "Time (23:59): "]
                             
                             )
  )


(define timeInputMin (new text-field%
                             [parent timeMixer]
                             [init-value ""]
                             [style '(single )]
                             [label ":"]
                             [vert-margin 20]
                             )
  )

(define buttonInputs (new horizontal-panel%
                          [parent inputArea]
                          [alignment '(center top)]
                          [horiz-margin 100]
                          [vert-margin 20]
                          )
  )

(define cancelAdd (new button%
                       [parent buttonInputs]
                       [label "Cancel"]
                       [horiz-margin 20]
                       [callback
                        (λ (o e)
                          (toBandConcerts)
                          )
                        ]
                       )
  )

(define attemptCreate (new button%
                           [parent buttonInputs]
                           [label "Create"]
                           [horiz-margin 20]
                           [callback (λ (o e)
                                       (verifyAdd)
                                       )
                                     ]
                           )
  )

(define whySpace (new vertical-panel%
                      [parent inputArea]
                      [min-height 100]
                      [alignment '(center top)]
                      [horiz-margin 80]
                      [vert-margin 10]
                      )
  )

(define inputErrMsg (new message%
                         [parent whySpace]
                         [label ""]
                         [auto-resize #t]
                         )
  )

(define spacer2 (new panel%
                     [parent createConcertArea]
                     )
  )

#|------------------End: Add Concerts Page-------------------|#


#|-------------------------------------------------------------------------------- End: GUI Elements  ------------------------------------------------------------------------------------|#


(send app show #t)
(send app center 'both)
