;;==========================================================================
;;
;; STARTER FILE FOR CSC 4240/5240 PROGRAM #1: Eliza
;;==========================================================================

;;----------------------------------------------------------------------------
;; eliza: top-level function which, when given a sentence (no
;; punctuation, please!), comes back with a response like you would.

( defun eliza ( sentence )
  ( respond ( change-pros sentence ) database ) )

;;----------------------------------------------------------------------------
;; change-pros: changes the pronouns of the sentence so that Eliza can
;; come back with the appropriately switched first and second person
;; references.

( defun change-pros ( sentence )
  ( cond 
    ( ( null sentence ) nil )
    ( ( equal ( car sentence ) 'you )
      ( cons 'I ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'I )
      ( cons 'you ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'my )
      ( cons 'your ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'your )
      ( cons 'I ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'we )
      ( cons 'you ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'our )
      ( cons 'your ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'yourself )
      ( cons 'myself ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'myself )
      ( cons 'yourself ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'mine )
      ( cons 'yours ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'yours )
      ( cons 'mine ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'am )
      ( cons 'are ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'is )
      ( cons 'are ( change-pros ( cdr sentence ) ) ) )


    ( t ( cons ( car sentence ) ( change-pros ( cdr sentence ) ) ) ) ) )

;;----------------------------------------------------------------------------
;; respond: given a sentence, looks through the database in search of
;; a matching pattern and the response; given the database response,
;; uses 'instantiate' to fill in the blanks, and returns the completed
;; response

( defun respond ( sentence db )
    ( cond
        ;; end of DB, return random catch-all
        ;; NOTE: This originally returned a nil value, but was appropriated
        ( ( null db )
            (catchall_resp (random 2))
        )

        ;; if the result of matching the sentence against the current
        ;; pattern is a success, produce this response
        ( ( success ( setq result ( match sentence ( first ( car db ) ) ) ) )
          ( instantiate result ( second ( car db ) ) ) )
        
        ;; otherwise, keep looking through the DB
        ( t ( respond sentence ( cdr db ) ) )
    )
)

;;----------------------------------------------------------------------------
;; match: if there is not a match between this pattern and this data,
;; returns 'fail;' otherwise, returns the sentence in partitioned
;; format

( defun match ( data pattern )
  ( cond
    ;; end of both data and pattern; a match
    ( ( and ( null data ) ( null pattern ) ) nil )

    ;; end of pattern, but not end of data; no match
    ( ( null pattern ) fail )

    ;; end of data, but not end of pattern; if the pattern starts with
    ;; a variable, eat it and try and match the rest of the pattern to
    ;; the null sentence (will only work if all variables); otherwise,
    ;; fail
    ( ( null data ) 
      ( cond
	( ( variablep ( car pattern ) )
	  ( if ( success ( setq result ( match data ( cdr pattern ) ) ) )
	      result
	    fail ) )
	( t fail ) ) )


    ;; first item of data and pattern are identical; if the rest of it
    ;; matched, return the first item cons'ed with the rest of the
    ;; partitioned sentence; otherwise, fail
    ( ( equal ( car data ) ( car pattern ) )
      ( if ( success ( setq result ( match ( cdr data ) ( cdr pattern ) ) ) )
	  ( cons ( list ( car data ) ) result )
	fail ) )

    ;; first item of pattern is a variable; if the rest of the data
    ;; (minus the first word, matched to the variable) is a match with
    ;; all of the pattern, return the appropriate stuff; if all of the
    ;; data (variable eats nothing) matches the rest of the pattern,
    ;; return appropriate stuff; else, fail.
    ( ( variablep ( car pattern ) ) 
      ( cond
	;; variable eats nothing;  () is put in partitioned sentence
	( ( success ( setq result ( match data ( cdr pattern ) ) ) )
	  ( cons () result ) )
	;; variable eats one word; word is cons'ed into the first
	;; element of the partitioned sentence, assuming that the step
	;; before an actual match word would be a ()
	( ( success ( setq result ( match ( cdr data ) pattern ) ) )
	  ( cons ( cons ( car data ) ( car result ) ) ( cdr result ) ) )
	;; otherwise, fail
	( t fail ) ) )

    ( t fail ) ) )

;;----------------------------------------------------------------------------
;; instantiate: takes a partitioned sentence and the response it has
;; been matched to and generates the appropriated completed response

( defun instantiate ( partitioned response )
  ( cond
    ( ( null response ) nil )
    ;; numbers indicate what part of the partitioned sentence to
    ;; insert into the response
    ( ( numberp ( car response ) )
      ( setq index ( - ( car response ) 1 ) )
      ( append ( nth index partitioned )
	     ( instantiate partitioned ( cdr response ) ) ) )
    ( t ( cons ( car response )
	     ( instantiate partitioned ( cdr response ) ) ) ) ) )

;;---------------------------------------------------------------------------
;;
;;  			     helping functions
;;
;;---------------------------------------------------------------------------

( setq fail '-1 )

( defun success ( result )
  ( not ( equal result fail ) ) )

( defun variablep ( word )
  ( equal word '0 ) )


;;---------------------------------------------------------------------------
;;
;;  			         database
;;
;;---------------------------------------------------------------------------

;; CHANGE THIS: add more to this database so that the interaction is
;; more interesting and communicative and so that Eliza sounds like you 
;; would sound in the same conversation!
;;---------------------------------------------------------------------------

( setq database
    '(
        ;; example greetings/farewells -- change them to sound like you
        ( (Hello 0)
          (How's it going?) )
        ( (0 how are I 0)
          (I'm doing pretty well and thanks for asking))
        ( (0 have you ever 0)
          (I cant say that I have ever 5 ))
        ( (0 you got 0)
          (Thats pretty cool Ive always wanted 4 ))
        ( (0 can I 0)
          (That would be pretty tough Im stuck behind this computer all day))
        ( (0 you came here because 0)
	      (That's fine What seems to be up))
        ( (0 Goodbye 0)
	      (Take it easy!) )

        ;; feelings
        ( (0 you think 0)
          (Why do you think 4 ?) )
        ( (0 you feel 0)
          (Where do you think this feeling of being 6 comes from?) )
        ( (0 you are 0)
          (Why are you 4 ?) )
        ( (0 you dont know why 0)
          (Why dont you know why 6) )
    )
)


( defun catchall_resp ( val )
    ( cond
        ((equal val '0)
            '(Im not quite sure I understand))
        ((equal val '1)
            '(Could you maybe rephrase that))
    )
)
