(use-modules
 (demo assets)
 (hoot eval)                                        
 (hoot ffi)                                         	
 (hoot hashtables)                                  	
 ((hoot error-handling) #:select (format-exception))
 (ice-9 match)                                      
 (repl-environment)                                 
 (srfi srfi-11)
 (guile)
 (repl)
 (ren-sexp scene)
 (ren-sexp rssl)
 (ren-sexp bg)
 (ren-sexp music)
 (ren-sexp sprites)
 (ren-sexp core))

(define WELCOME
  (make-scene
   #:bg (black-screen)
   #:text "Hello and Welcome to this new super visual novel engine!  Press <SPACE> to launch the game."))

(define music:curious_critters
  (make-music (audio:curious_critters) 100))

(define nastya:tired
  (make-sprite (image:nastya-tired) 1000 'left 'nastya 'tired 'casual))

(define nastya:normal
  (make-sprite (image:nastya-normal) 1000 'left 'nastya 'normal 'casual))

(define nastya:normal-ice
  (make-sprite (image:nastya-normal-ice) 1000 'left 'nastya 'normal-ice 'casual))

(define masha:happy
  (make-sprite (image:masha-happy) 1000 'right 'masha 'happy 'casual))

(define masha:troubled
  (make-sprite (image:masha-troubled) 1000 'right 'masha 'troubled 'casual))

(define masha:normal
  (make-sprite (image:masha-normal) 1000 'right 'masha 'normal 'casual))

(define bg:kitchen (%make-bg (image:kitchen) 1000))

(define END
  (make-scene #:bg (make-bg (image:end))))

(define script
  (->> (list WELCOME)
       (CLEAN)
       (BGM	music:curious_critters)
       (BG	bg:kitchen)
       (PAUSE	50)
       (JOIN	(list masha:normal nastya:tired))
       (UPDATE	masha:normal masha:happy)
       (TXT	"And here the water is ready.")

       (UPDATE	nastya:tired nastya:normal)
       (+TXT	"My patience has ended!")

       (UPDATE	masha:happy masha:troubled)
       (+TXT	"Wait a little bit longer, please.")
       
       (UPDATE	nastya:normal nastya:tired)
       (+TXT     "I don't want to wait, pour it faster.")

       (UPDATE	masha:troubled masha:normal)
       (+TXT     "No, a tea ceremony must be performed according to the rules.")

       (UPDATE	nastya:tired nastya:normal)
       (TXT     "These strange procedures of yours.. these bowls with the size of a kitten's paw.. and it watery sour tea... What is the point of all these bourgeois trinkets?! Our forefathers drank a tea from kettles in samovars!")

       (UPDATE	masha:normal masha:happy)
       (TXT     "Nobody ever drank a tea like that, as far as I know. A tea was brought to the Moscow Tsardom from China in the middle of the 17th century and was initially available only to an upper classes... And a date of creating the first samovar in the Russian Empire is marked as 1740. It was made, by the way, not far away, just three hours drive from there.")

       (UPDATE	nastya:normal nastya:tired)
       (TXT     "That's all bourgeois science. I believe only in my master – The Cold. The Cold is harsh but fair. At night, he whispers to me The Entire Truth of this world... If I forget to close a window properly.")

       (UPDATE	masha:happy masha:normal)
       (TXT     "I sincerely believe he will help you pass our exam.")

       (UPDATE	nastya:tired nastya:normal-ice)
       (TXT     "He is needed for prophecies. You're for exams. The world will plunge into darkness and servants of the Lord will smash everything living apart that stands in their way...")

       (UPDATE	nastya:normal-ice nastya:tired)
       (TXT     "...If we disturb this fragile balance.")

       (UPDATE	masha:normal masha:troubled)
       (TXT     "...You seem very tired.")

       (UPDATE	masha:troubled masha:happy)
       (TXT     "Fine. There will be a samovar. In a week, lets gather at an interesting place near the Yeltsin Center. For now... The water is exactly the right temperature. Today we will try my raspberry tea.")
       
       (JUST	END)))

(define-foreign document-body
  "document" "body"
  -> (ref null extern))
(define-foreign get-element-by-id
  "document" "getElementById"
  (ref string) -> (ref null extern))
(define-foreign make-text-node
  "document" "createTextNode"
  (ref string) -> (ref null extern))
(define-foreign make-element
  "document" "createElement"
  (ref string) -> (ref null extern))
(define-foreign make-tree-walker
  "document" "createTreeWalker"
  (ref null extern) -> (ref null extern))

(define-foreign prevent-default!
  "event" "preventDefault"
  (ref null extern) -> none)
(define-foreign keyboard-event-key
  "event" "keyboardKey"
  (ref null extern) -> (ref string))
(define-foreign %keyboard-event-shift?
  "event" "keyboardShiftKey"
  (ref null extern) -> i32)
(define (keyboard-event-shift? elem)
  (= (%keyboard-event-shift? elem) 1))

(define-foreign element-value
  "element" "value"
  (ref null extern) -> (ref string))
(define-foreign set-element-value!
  "element" "setValue"
  (ref null extern) (ref string) -> none)
(define-foreign %element-checked?
  "element" "checked"
  (ref null extern) -> i32)
(define (element-checked? elem)
  (= (%element-checked? elem) 1))
(define-foreign set-element-checked!
  "element" "setChecked"
  (ref null extern) i32 -> none)
(define-foreign scroll-height
  "element" "scrollHeight"
  (ref null extern) -> f64)
(define-foreign set-scroll-top!
  "element" "setScrollTop"
  (ref null extern) f64 -> none)
(define-foreign append-child!
  "element" "appendChild"
  (ref null extern) (ref null extern) -> (ref null extern))
(define-foreign remove!
  "element" "remove"
  (ref null extern) -> none)
(define-foreign replace-with!
  "element" "replaceWith"
  (ref null extern) (ref null extern) -> none)
(define-foreign set-attribute!
  "element" "setAttribute"
  (ref null extern) (ref string) (ref string) -> none)
(define-foreign remove-attribute!
  "element" "removeAttribute"
  (ref null extern) (ref string) -> none)
(define-foreign add-event-listener!
  "element" "addEventListener"
  (ref null extern) (ref string) (ref null extern) -> none)
(define-foreign remove-event-listener!
  "element" "removeEventListener"
  (ref null extern) (ref string) (ref null extern) -> none)

(define-foreign current-node
  "treeWalker" "currentNode"
  (ref null extern) -> (ref null extern))
(define-foreign set-current-node!
  "treeWalker" "setCurrentNode"
  (ref null extern) (ref null extern) -> (ref null extern))
(define-foreign next-node!
  "treeWalker" "nextNode"
  (ref null extern) -> (ref null extern))
(define-foreign first-child!
  "treeWalker" "firstChild"
  (ref null extern) -> (ref null extern))
(define-foreign next-sibling!
  "treeWalker" "nextSibling"
  (ref null extern) -> (ref null extern))

(define procedure->external/cached
  (let ((cache (make-weak-key-hashtable)))
    (lambda (proc)
      (or (weak-key-hashtable-ref cache proc)
          (let ((f (procedure->external proc)))
            (weak-key-hashtable-set! cache proc f)
            f)))))

(define (add-event-listener!/wrap elem name proc)
  (add-event-listener! elem name (procedure->external/cached proc)))
(define (remove-event-listener!/wrap elem name proc)
  (remove-event-listener! elem name (procedure->external/cached proc)))

(define (set-attribute!* elem name val)
  (if (string=? name "checked")
      ;; Special case for input 'checked' attribute.  Instead of
      ;; setting an attribute, we set the property.  It's a hack,
      ;; but fine for this little demo.
      (set-element-checked! elem (if val 1 0))
      (set-attribute! elem name val)))

(define (attr-value? x)
  (or (string? x) (boolean? x)))

(define (sxml->dom exp)
  (match exp
    ((? string? str)
     (make-text-node str))
    (((? symbol? tag) . body)
     (let ((elem (make-element (symbol->string tag))))
       (define (add-children children)
         (for-each (lambda (child)
                     (append-child! elem (sxml->dom child)))
                   children))
       (match body
         ((('@ . attrs) . children)
          (for-each (lambda (attr)
                      (match attr
                        (((? symbol? name) (? attr-value? val))
                         (set-attribute!* elem
                                          (symbol->string name)
                                          val))
                        (((? symbol? name) (? procedure? proc))
                         (let ((name* (symbol->string name)))
                           (add-event-listener!/wrap elem name* proc)))))
                    attrs)
          (add-children children))
         (children (add-children children)))
       elem))))

(define (virtual-dom-render root old new)
  (define (attrs+children exp)
    (match exp
      ((('@ . attrs) . children)
       (values attrs children))
      (children
       (values '() children))))
  (define (find-attr attrs name)
    (match attrs
      (() #f)
      ((attr . rest)
       (match attr
         ((name* val)
          (if (eq? name name*)
              val
              (find-attr rest name)))))))
  (define (update-attrs node old-attrs new-attrs)
    (for-each
     (lambda (attr)
       (match attr
         ((name val)
          (let ((name-str (symbol->string name)))
            (match (find-attr old-attrs name)
              ;; No existing attr/listener, add new one.
              (#f
               (match val
                 ((? attr-value?)
                  (set-attribute!* node name-str val))
                 ((? procedure?)
                  (add-event-listener!/wrap node name-str val))))
              ;; Replace old attr or listener with new.
              (old-val
               (match val
                 ((? attr-value?)
                  (unless (equal? old-val val)
                    (set-attribute!* node name-str val)))
                 ((? procedure?)
                  (unless (eq? old-val val)
                    (remove-event-listener!/wrap node name-str old-val)
                    (add-event-listener!/wrap node name-str val))))))))))
     new-attrs)
    ;; Delete old attrs that aren't in new.
    (for-each
     (lambda (attr)
       (match attr
         ((name val)
          (let ((name-str (symbol->string name)))
            (match (find-attr new-attrs name)
              (#f
               (match val
                 ((? attr-value?)
                  (remove-attribute! node name-str))
                 ((? procedure?)
                  (remove-event-listener! node name-str val))))
              (_ #t))))))
     old-attrs))
  (let ((walker (make-tree-walker root)))
    (first-child! walker)
    (let loop ((parent root)
               (old old)
               (new new))
      (match old
        (#f
         ;; It's the first render, so clear out whatever might be
         ;; in the actual DOM and render the entire tree.  No
         ;; diffing necessary.
         (let loop ((node (current-node walker)))
           (unless (external-null? node)
             (let ((next (next-sibling! walker)))
               (remove! node)
               (loop next))))
         (append-child! parent (sxml->dom new)))
        ((? string?)
         ;; Replace text node with either a new text node if the
         ;; text has changed, or an element subtree if the text
         ;; has been replaced by an element.
         (unless (and (string? new) (string=? old new))
           (let ((new-node (sxml->dom new)))
             (replace-with! (current-node walker) new-node)
             (set-current-node! walker new-node))))
        (((? symbol? old-tag) . old-rest)
         (let-values (((old-attrs old-children)
                       (attrs+children old-rest)))
           (match new
             ((? string?)
              ;; Old node was an element, but the new node is a
              ;; string, so replace the element subtree with a
              ;; text node.
              (let ((new-text (make-text-node new)))
                (replace-with! (current-node walker) new-text)
                (set-current-node! walker new-text)))
             (((? symbol? new-tag) . new-rest)
              (let-values (((new-attrs new-children)
                            (attrs+children new-rest)))
                (cond
                 ;; The element tag is the same, so modify the
                 ;; inner contents of the element if necessary.
                 ((eq? old-tag new-tag)
                  (let ((parent (current-node walker)))
                    (update-attrs parent old-attrs new-attrs)
                    (first-child! walker)
                    (let child-loop ((old old-children)
                                     (new new-children))
                      (match old
                        (()
                         ;; The old child list is empty, so
                         ;; diffing stops here.  All remaining
                         ;; children in the new list are fresh
                         ;; elements that need to be added.
                         (for-each
                          (lambda (new)
                            (append-child! parent (sxml->dom new)))
                          new))
                        ((old-child . old-rest)
                         (match new
                           ;; The new child list is empty, so any
                           ;; remaining children in the old child
                           ;; list need to be removed, including
                           ;; the current one.
                           (()
                            (let rem-loop ((node (current-node walker)))
                              (unless (external-null? node)
                                (let ((next (next-sibling! walker)))
                                  (remove! node)
                                  (rem-loop next)))))
                           ;; Recursively diff old and new child
                           ;; elements.
                           ((new-child . new-rest)
                            (loop parent old-child new-child)
                            (next-sibling! walker)
                            (child-loop old-rest new-rest))))))
                    (set-current-node! walker parent)))
                 ;; New element tag is different than the old
                 ;; one, so replace the entire element subtree.
                 (else
                  (replace-with! (current-node walker)
                                 (sxml->dom new)))))))))))))

(define *current-vdom* #f)
(define (refresh!)
  (pk 'refresh)
  (let ((new-vdom (render)))
    (virtual-dom-render (document-body) *current-vdom* new-vdom)
    (set! *current-vdom* new-vdom)))

(define *log*
  '("Welcome to the Hoot REPL!
"))

(define (log-append! . lines)
  (set! *log* (append *log* lines)))

(define *unspecified* (if #f #f))
(define (unspecified? x)
  (eq? x *unspecified*))

(define prompt "> ")

(define (call-with-error-handling thunk)
  (with-exception-handler (lambda (exn)
                            (format-exception exn (current-output-port)))
    thunk
    #:unwind? #t))

(define env (repl-environment))

(define *prev-input* #f)

(define %invalid (cons 'invalid 'expression))
(define (read* port)
  (with-exception-handler (lambda (exn) %invalid)
    (lambda () (read port))
    #:unwind? #t))

(define (eval! str)
  ;; Parse user input.
  (let ((exp (read* (open-input-string str)))
        ;; Open output port.
        (output (open-output-string)))
    ;; Redirect all output to our output port.
    (parameterize ((current-output-port output))
      ;; Echo the prompt and user code.
      (display prompt)
      (display str)
      (cond
       ((eq? exp %invalid)
        (display "invalid Scheme expression\n"))
          ;; Invoke the interpreter.
       (else
        (set! *prev-input* str)
        (call-with-values (lambda ()
                            (call-with-error-handling
                             (lambda ()
                               (eval exp env))))
          ;; Display each returned value on its own line.
          (lambda vals
            (if (null? vals)
                (display "\n")
                (for-each (lambda (val)
                            (unless (unspecified? val)
                              (display "=> ")
                              (write val))
                            (newline))
                          vals)))))))
    ;; Append output to log.
    (log-append! (get-output-string output))))

(define (scroll-to-bottom!)
  (let ((repl (get-element-by-id "repl")))
    (set-scroll-top! repl (scroll-height repl))))

(define (maybe-eval event)
  ;; Get the event's key.
  (let ((key (keyboard-event-key event)))
    ;; Evaluate user code when Enter is pressed, but not when
    ;; Shift is being held so the user can edit across multiple
    ;; lines.
    (cond
     ((and (string=? key "Enter")
           (not (keyboard-event-shift? event)))
      ;; Get the text within the expression textarea.
      (let* ((input (get-element-by-id "expression"))
             (exp (element-value input)))
        ;; If the textarea is empty, do nothing.
        (unless (string=? exp "")
          ;; Clear the textarea.
          (set-element-value! input "")
          ;; Evaluate and append output to log.
          (eval! exp)
          ;; Update UI.
          (refresh!)
          ;; Scroll the log to show the next output.
          (scroll-to-bottom!))))
     ((string=? key "ArrowUp")
      (match *prev-input*
        (#f (values))
        (str (set-element-value! (get-element-by-id "expression") str)))))))

(define (render)
  `(div (@ (id "all"))
	(div (@ (id "stage"))
	     (canvas (@ (id "all-canvas")))
	     (canvas (@ (id "gray-canvas")))
	     (canvas (@ (id "text-canvas")))
	     (canvas (@ (id "carret-canvas")))
	     (canvas (@ (id "old-text-canvas")))
	     (canvas (@ (id "menu-canvas")))
	     (canvas (@ (id "debug-canvas"))))
	(div (@ (class "container"))
	     (div (@ (id "repl")
		     (class "repl repl-text"))
		  (div (@ (class "log")) ,@*log*)
		  (div (@ (class "prompt"))
                       ,prompt
                       (textarea (@ (id "expression")
				    (class "repl-text")
				    (rows "5")
				    (keyup ,maybe-eval))))))))

(refresh!)

(init (reverse script))
