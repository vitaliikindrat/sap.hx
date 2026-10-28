;; sap -- see what changed, in Helix. Named for sapwood: the young, live
;; layer of the trunk, the growth on top of what is settled.
;;
;; * `:sap-changes` opens every change against HEAD in one buffer (git
;;   status + the full diff, `diff` highlighting); enter on a line opens the
;;   file there, with its changes expanded.
;; * `:sap-toggle` expands the focused file's changes in place: removed lines
;;   as red rows where they were, added and changed lines highlighted. It
;;   diffs the buffer, unsaved edits included, against HEAD -- the same base
;;   as Helix's gutter -- and redraws on save.
;; * `:sap-preview` expands only the hunk under the cursor (toggle).
;; * `:sap-stage-hunk`, `:sap-unstage-hunk`, `:sap-reset-hunk` act on the
;;   hunk under the cursor (the file must be saved).
;;
;;   (require "sap/sap.scm")
;;
;; Needs a Steel helix with the virtual-text API (canopy.hx's
;; helix-virtual-text.patch). Theme scopes: ui.virtual.diff.minus (removed
;; rows, falls back to diff.minus) and ui.virtual.diff.plus (added lines).
;; Running git follows hardhus/helix-git's approach.

(require "helix/editor.scm")
(require "helix/misc.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require (only-in "helix/keymaps.scm" keymap *reverse-buffer-map-insert*))
(require-builtin helix/core/text as text.)
(require-builtin steel/process)
(require "util.scm")

(provide sap-changes
         sap-goto
         sap-toggle
         sap-preview
         sap-stage-hunk
         sap-unstage-hunk
         sap-reset-hunk)

;;;; the virtual-text API, looked up so this file loads without it

(define (lookup name)
  (with-handler (lambda (_) #f)
                (eval-string (string-append "(require-builtin helix/core/misc as vt.) vt." name))))
(define vt-add-lines (lookup "add-virtual-lines"))
(define vt-add-highlight (lookup "add-line-highlight"))
(define vt-remove (lookup "remove-virtual-text"))
(define (vt-available?) (and (function? vt-add-lines) (function? vt-add-highlight) (function? vt-remove)))

(define (status msg) (set-status! (string-append "sap: " msg)))

;;;; git

;; Run git in `dir`; (Ok stdout) or (Err message). `input` goes to stdin.
(define (git-run dir args [input #false])
  (let ([cmd (command "git" args)])
    (set-current-dir! cmd dir)
    (with-stdout-piped cmd)
    (with-stderr-piped cmd)
    (when input (with-stdin-piped cmd))
    (let ([spawned (spawn-process cmd)])
      (if (Err? spawned)
          (Err "could not run git (is it on your PATH?)")
          (let ([child (Ok->value spawned)])
            (when input
              (let ([port (child-stdin child)])
                (write-string input port)
                (close-port port)))
            (wait->stdout child))))))

(define (ok-or result default) (if (Ok? result) (Ok->value result) default))

(define (doc-path doc)
  (let ([p (editor-document->path doc)]) (if (string? p) p #f)))
(define (dir-of path) (trim-end-matches path (file-name path)))
(define (toplevel dir)
  (let ([r (git-run dir (list "rev-parse" "--show-toplevel"))])
    (if (Ok? r) (trim (Ok->value r)) #f)))

(define (tmp-dir)
  (let ([d (string-append (with-handler (lambda (_) "/tmp") (env-var "TMPDIR")) "/sap-hx")])
    ;; another hx may create it between the check and the call
    (unless (path-exists? d) (with-handler (lambda (_) void) (create-directory! d)))
    d))

;; Steel's open-output-file will not overwrite, so replace the file.
(define (write-file! path text)
  (when (path-exists? path) (delete-file! path))
  (let ([port (open-output-file path)])
    (write-string text port)
    (close-port port)))

;; -U0 hunks of the buffer text against HEAD (all-added for a file HEAD
;; does not have), or #f when the file is not in a git work tree.
(define (buffer-hunks doc)
  (let* ([path (doc-path doc)]
         [dir (and path (dir-of path))]
         [top (and dir (toplevel dir))])
    (if (not top)
        #f
        (let* ([name (file-name path)]
               [base (git-run dir (list "cat-file" "--filters" (string-append "HEAD:./" name)))]
               [base-path (string-append (tmp-dir) "/base-" (number->string (doc-id->usize doc)))]
               [_ (write-file! base-path (ok-or base ""))]
               [text (text.rope->string (editor->text doc))]
               [out (git-run dir (list "-c" "core.autocrlf=false" "diff" "--no-index" "--histogram"
                                       "-U0" "--no-color" "--no-ext-diff" "--" base-path "-")
                             text)])
          (cdr (parse-diff (ok-or out "")))))))

;;;; inline rendering

(define *inline* (hash)) ; doc-id usize -> list of virtual-text ids

(define (row-json texts)
  (value->jsexpr-string
   (map (lambda (t)
          (hash 'style "ui.virtual.diff.minus"
                'spans (list (hash 'text (string-append "- " (string-replace t "\t" "    "))
                                   'style "diff.minus"))))
        texts)))

;; Draw `hunks` into the focused document; returns the item ids.
(define (draw! hunks)
  (let loop ([plan (render-plan hunks)] [ids '()])
    (if (null? plan)
        ids
        (let* ([item (car plan)]
               [id (if (equal? (car item) 'highlight)
                       (vt-add-highlight (cadr item) "ui.virtual.diff.plus")
                       (vt-add-lines (cadr item) (row-json (caddr item))))])
          (loop (cdr plan) (if id (cons id ids) ids))))))

(define (clear! doc)
  (let ([ids (hash-try-get *inline* (doc-id->usize doc))])
    (when ids
      (for-each (lambda (id) (vt-remove id)) ids)
      (set! *inline* (hash-remove *inline* (doc-id->usize doc))))))

(define (focused) (editor->doc-id (editor-focus)))

;; Redraw the focused document with `hunks`; 'all or a single hunk.
(define (show! doc hunks)
  (clear! doc)
  (set! *inline* (hash-insert *inline* (doc-id->usize doc) (draw! hunks))))

(define (cursor-line) (+ 1 (helix.static.get-current-line-number)))

;;@doc
;; Expand the focused file's changes against HEAD in place (toggle)
(define (sap-toggle)
  (let ([doc (focused)])
    (cond
      [(not (vt-available?)) (status "needs a helix with canopy's virtual-text patch")]
      [(hash-try-get *inline* (doc-id->usize doc)) (clear! doc) (status "changes hidden")]
      [else
       (let ([hunks (buffer-hunks doc)])
         (cond
           [(not hunks) (status "not in a git work tree")]
           [(null? hunks) (status "no changes against HEAD")]
           [else (show! doc hunks)
                 (status (string-append (number->string (length hunks)) " hunk(s) against HEAD"))]))])))

;;@doc
;; Expand only the hunk under the cursor (toggle)
(define (sap-preview)
  (let ([doc (focused)])
    (cond
      [(not (vt-available?)) (status "needs a helix with canopy's virtual-text patch")]
      [(hash-try-get *inline* (doc-id->usize doc)) (clear! doc)]
      [else
       (let* ([hunks (or (buffer-hunks doc) '())]
              [hunk (hunk-at hunks (cursor-line))])
         (if hunk (show! doc (list hunk)) (status "no change on this line")))])))

;; While a file is expanded, keep it in step with what is on disk.
(define (refresh-if-shown! doc)
  (when (and (hash-try-get *inline* (doc-id->usize doc)) (equal? (doc-id->usize doc) (doc-id->usize (focused))))
    (let ([hunks (buffer-hunks doc)])
      (if (and hunks (not (null? hunks))) (show! doc hunks) (clear! doc)))))

;;;; hunk actions

;; Apply the hunk under the cursor from `diff-args` (a git diff of this file)
;; with `apply-args`, then reload the buffer.
(define (act! diff-args apply-args done-msg)
  (let* ([doc (focused)]
         [path (doc-path doc)])
    (cond
      [(not path) (status "the buffer has no file")]
      [(editor-document-dirty? doc) (status "save the file first")]
      [else
       (let* ([dir (dir-of path)]
              [top (toplevel dir)]
              [out (and top (git-run dir (append diff-args (list "--" (string-append "./" (file-name path))))))]
              [parsed (and out (Ok? out) (parse-diff (Ok->value out)))]
              [hunk (and parsed (hunk-at (cdr parsed) (cursor-line)))])
         (cond
           [(not top) (status "not in a git work tree")]
           [(not hunk) (status "no hunk here for that")]
           [else
            (let ([r (git-run top (append (list "apply" "--unidiff-zero") apply-args (list "-"))
                              (hunk-patch (car parsed) hunk))])
              (if (Err? r)
                  (status (string-append "git apply failed: " (to-string (Err->value r))))
                  (begin
                    (editor-document-reload doc)
                    (refresh-if-shown! doc)
                    (status done-msg))))]))])))

;;@doc
;; Stage the hunk under the cursor
(define (sap-stage-hunk)
  (act! (list "diff" "-U0" "--no-color") (list "--cached") "hunk staged"))

;;@doc
;; Unstage the hunk under the cursor
(define (sap-unstage-hunk)
  (act! (list "diff" "--cached" "-U0" "--no-color") (list "--cached" "-R") "hunk unstaged"))

;;@doc
;; Throw away the unstaged hunk under the cursor (back to the index version)
(define (sap-reset-hunk)
  (act! (list "diff" "-U0" "--no-color") (list "-R") "hunk reset"))

;;;; the changes buffer

(define *panel-root* #f)
(define *panel-label* "sap-changes")

;;@doc
;; Open every change against HEAD in one buffer; enter opens the file there
(define (sap-changes)
  (let* ([path (doc-path (focused))]
         [top (toplevel (if path (dir-of path) (helix-find-workspace)))])
    (if (not top)
        (status "not in a git work tree")
        (let* ([st (ok-or (git-run top (list "status" "--short")) "")]
               [diff (ok-or (git-run top (list "diff" "HEAD" "--no-color" "--no-ext-diff")) "")])
          (if (and (equal? (trim st) "") (equal? (trim diff) ""))
              (status "no changes")
              (begin
                (set! *panel-root* top)
                (helix.new)
                (set-scratch-buffer-name! "[sap] changes")
                (helix.static.insert_string
                 (string-append "# " top " against HEAD -- enter opens the file at that line\n"
                                st "\n" diff))
                (helix.set-language "diff")
                (helix.static.goto_file_start)
                (helix.static.collapse_selection)
                (*reverse-buffer-map-insert* (doc-id->usize (focused)) *panel-label*)))))))

;;@doc
;; In the changes buffer: open the file and line under the cursor, expanded
(define (sap-goto)
  (let* ([doc (focused)]
         [lines (split-lines (text.rope->string (editor->text doc)))]
         [idx (helix.static.get-current-line-number)]
         [line (if (< idx (length lines)) (list-ref lines idx) "")]
         ;; a `git status --short` line: "XY path"
         [status-path (and (> (string-length line) 3)
                           (not (starts-with? line "#"))
                           (not (starts-with? line "diff "))
                           (equal? (substring line 2 3) " ")
                           (substring line 3 (string-length line)))]
         [loc (or (diff-location lines idx) (and status-path (cons status-path 1)))])
    (if (not (and loc *panel-root*))
        (status "no file on this line")
        (begin
          (helix.open (string-append *panel-root* "/" (car loc)))
          (helix.goto (number->string (cdr loc)))
          (helix.static.align_view_center)
          (let ([opened (focused)])
            (unless (hash-try-get *inline* (doc-id->usize opened))
              (let ([hunks (buffer-hunks opened)])
                (when (and hunks (not (null? hunks)) (vt-available?)) (show! opened hunks)))))))))

(keymap (buffer *panel-label*) (normal (ret ":sap-goto")))

;;;; hooks

(register-hook 'document-saved (lambda (doc) (refresh-if-shown! doc)))
