;; Run with `sh tests/run-all.sh` or `steel tests/util-test.scm` from the repo root.
(require "../util.scm")

(define failures 0)
(define (check name got expected)
  (if (equal? got expected)
      (displayln "ok   " name)
      (begin
        (set! failures (+ failures 1))
        (displayln "FAIL " name ": got " got ", expected " expected))))

;; git diff -U0 of a file where line 2 changed, line 5 was deleted and two
;; lines were added after line 7.
(define u0 "diff --git a/f b/f\nindex 1..2 100644\n--- a/f\n+++ b/f\n@@ -2 +2 @@\n-two\n+TWO\n@@ -5 +4,0 @@\n-five\n@@ -7,0 +7,2 @@\n+new a\n+new b\n")
(define parsed (parse-diff u0))
(define pre (car parsed))
(define hunks (cdr parsed))

(check "preamble" pre (list "diff --git a/f b/f" "index 1..2 100644" "--- a/f" "+++ b/f"))
(check "hunk count" (length hunks) 3)
(check "change range" (list (hash-ref (car hunks) 'old-start) (hash-ref (car hunks) 'new-start) (hash-ref (car hunks) 'new-count)) (list 2 2 1))
(check "deletion range" (list (hash-ref (cadr hunks) 'new-start) (hash-ref (cadr hunks) 'new-count)) (list 4 0))
(check "hunk lines" (hash-ref (caddr hunks) 'lines) (list "+new a" "+new b"))

(check "contains change" (hunk-contains? (car hunks) 2) #t)
(check "not contains" (hunk-contains? (car hunks) 3) #f)
(check "deletion covers its line" (hunk-contains? (cadr hunks) 4) #t)
(check "addition range" (map (lambda (l) (hunk-contains? (caddr hunks) l)) (list 6 7 8 9)) (list #f #t #t #f))
(check "hunk-at" (hash-ref (hunk-at hunks 8) 'header) "@@ -7,0 +7,2 @@")
(check "hunk-at none" (hunk-at hunks 1) #f)

(check "render plan"
       (render-plan hunks)
       (list (list 'rows 0 (list "two")) (list 'highlight 1)
             (list 'rows 3 (list "five"))
             (list 'highlight 6) (list 'highlight 7)))

(check "hunk patch"
       (hunk-patch pre (car hunks))
       "diff --git a/f b/f\nindex 1..2 100644\n--- a/f\n+++ b/f\n@@ -2 +2 @@\n-two\n+TWO\n")

;; full diff with context, for the changes panel
(define full (split-lines "diff --git a/x.nix b/x.nix\n--- a/x.nix\n+++ b/x.nix\n@@ -10,4 +10,4 @@ foo\n a\n-b\n+B\n c\n"))
(check "location of context" (diff-location full 4) (cons "x.nix" 10))
(check "location of added" (diff-location full 6) (cons "x.nix" 11))
(check "location of removed" (diff-location full 5) (cons "x.nix" 11))
(check "location of trailing context" (diff-location full 7) (cons "x.nix" 12))
(check "location of header" (diff-location full 0) #f)

(if (> failures 0)
    (begin (displayln failures " failure(s)") (error! "tests failed"))
    (displayln "all util tests passed"))
