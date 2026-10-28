;; util.scm -- pure helpers for sap: parsing `git diff` output and turning it
;; into what the editor draws or what `git apply` takes. No helix requires,
;; so `steel tests/util-test.scm` runs it directly.

(provide split-lines
         parse-diff
         hunk-contains?
         hunk-at
         render-plan
         hunk-patch
         diff-location)

(define (split-lines text)
  (let ([lines (split-many text "\n")])
    ;; a trailing newline leaves an empty last element; drop it
    (if (and (not (null? lines)) (equal? (car (reverse lines)) ""))
        (reverse (cdr (reverse lines)))
        lines)))

;; "-12,3" / "+7" -> (start . count); a missing count means 1.
(define (parse-range s)
  (let* ([body (substring s 1 (string-length s))]
         [parts (split-many body ",")])
    (cons (string->int (car parts))
          (if (null? (cdr parts)) 1 (string->int (cadr parts))))))

;; `git diff` output for one file -> (preamble . hunks). The preamble is the
;; header lines before the first "@@" (diff --git, index, ---, +++); each
;; hunk is a hash with 'old-start 'old-count 'new-start 'new-count (git's
;; 1-based numbers), 'header (the "@@" line) and 'lines (the body lines).
(define (parse-diff text)
  (define (finish hunk) (hash-insert hunk 'lines (reverse (hash-ref hunk 'lines))))
  (let loop ([lines (split-lines text)] [preamble '()] [hunks '()] [cur #f])
    (cond
      [(null? lines)
       (cons (reverse preamble) (reverse (if cur (cons (finish cur) hunks) hunks)))]
      [(starts-with? (car lines) "@@ ")
       (let* ([parts (split-many (car lines) " ")]
              [old (parse-range (cadr parts))]
              [new (parse-range (caddr parts))])
         (loop (cdr lines)
               preamble
               (if cur (cons (finish cur) hunks) hunks)
               (hash 'old-start (car old) 'old-count (cdr old)
                     'new-start (car new) 'new-count (cdr new)
                     'header (car lines) 'lines '())))]
      [(not cur) (loop (cdr lines) (cons (car lines) preamble) hunks cur)]
      [else (loop (cdr lines) preamble hunks (hash-insert cur 'lines (cons (car lines) (hash-ref cur 'lines))))])))

;; Does the hunk cover new-file line `line` (1-based)? A pure deletion
;; (new-count 0) sits after its new-start line, so it covers that line (or
;; line 1 at the top of the file).
(define (hunk-contains? hunk line)
  (let ([start (hash-ref hunk 'new-start)] [count (hash-ref hunk 'new-count)])
    (if (= count 0)
        (= line (max 1 start))
        (and (>= line start) (< line (+ start count))))))

(define (hunk-at hunks line)
  (cond
    [(null? hunks) #f]
    [(hunk-contains? (car hunks) line) (car hunks)]
    [else (hunk-at (cdr hunks) line)]))

;; What to draw for -U0 hunks in a buffer holding the new file, 0-based:
;;   (list 'highlight line)           an added or changed line
;;   (list 'rows anchor texts)        removed lines, as rows below `anchor`
;; Removed lines go just above where they were: below the line before the
;; hunk (or below the first line when the hunk starts the file).
(define (render-plan hunks)
  (apply append
         (map (lambda (hunk)
                (let* ([ns (hash-ref hunk 'new-start)]
                       [nc (hash-ref hunk 'new-count)]
                       [removed (map (lambda (l) (substring l 1 (string-length l)))
                                     (filter (lambda (l) (starts-with? l "-")) (hash-ref hunk 'lines)))]
                       [anchor (max 0 (if (= nc 0) (- ns 1) (- ns 2)))]
                       [highlights (let loop ([i 0] [acc '()])
                                     (if (>= i nc) (reverse acc) (loop (+ i 1) (cons (list 'highlight (+ (- ns 1) i)) acc))))])
                  (if (null? removed)
                      highlights
                      (cons (list 'rows anchor removed) highlights))))
              hunks)))

;; A patch with just this hunk, for `git apply --unidiff-zero`.
(define (hunk-patch preamble hunk)
  (string-append (string-join (append preamble (list (hash-ref hunk 'header)) (hash-ref hunk 'lines)) "\n")
                 "\n"))

;; In a full `git diff` (with context), the file and new-file line that line
;; `idx` (0-based) of the output points at: (path . line) or #f. Removed
;; lines point at the new line they would have been next to.
(define (diff-location lines idx)
  (let loop ([i idx] [offset 0] [header #f])
    (cond
      [(< i 0) #f]
      [else
       (let ([l (list-ref lines i)])
         (cond
           [(and (not header) (starts-with? l "@@ "))
            (loop (- i 1) offset (car (parse-range (caddr (split-many l " ")))))]
           [(and (not header) (or (starts-with? l "+") (starts-with? l " ")))
            (loop (- i 1) (+ offset (if (= i idx) 0 1)) header)]
           [(not header) (loop (- i 1) offset header)]
           [(starts-with? l "+++ b/") (cons (substring l 6 (string-length l)) (+ header offset))]
           [(starts-with? l "diff --git") #f]
           [else (loop (- i 1) offset header)]))])))
