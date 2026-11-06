;;; futur-wake-tests.el --- Focused tests for the no-thread wake  -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Tests for the no-thread wake of `futur--funcall' (organa-s0f3).
;;
;; The wake is created when futur loads, so every test loads futur
;; itself and runs in its own fresh batch Emacs, one test per process.
;; The environment selects what is loaded:
;;
;;   FUTUR_WAKE_TESTS_SRC      directory holding the futur.el (or .elc)
;;                             under test; nothing else is loaded from it
;;   FUTUR_WAKE_TESTS_THREADS  "t" or "nil": `futur-use-threads', set
;;                             before the load
;;   FUTUR_WAKE_TESTS_MUTANT   unset, or "drop-kick": the AC1 test drops
;;                             every write to a futur-wake process
;;
;; Only futur is loaded: not futur-tests, not futur-elisp.
;;
;; Why AC1 can tell the fix from the bug: `timer_check' runs timers
;; from a copy of `timer-list' taken once per call, so the timer that
;; `futur--funcall' adds from inside a timer callback is not in the
;; copy.  The wait then selects on the full remaining timeout, unless
;; some process output arrives; reading output makes the wait loop run
;; `timer_check' again, which runs the new timer (Emacs bug#80468).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar futur-use-threads)
(defvar futur--background)
(declare-function futur--funcall "futur" (&rest args))
(declare-function futur-new "futur" (builder))
(declare-function futur-bind "futur" (futur fun &optional error-fun))
(declare-function futur-done "futur" (val))
(declare-function futur-failed "futur" (error-object))
(declare-function futur-abort "futur" (futur reason))
(declare-function futur-timeout "futur" (time))
(declare-function futur-deliver-value "futur" (futur val))
(declare-function futur-blocking-wait-to-get-result "futur"
                  (futur &optional error-fun))

;;; ─── Loading ──────────────────────────────────────────────────────

(defun futur-wake-tests--threads ()
  "Return the backend requested by FUTUR_WAKE_TESTS_THREADS."
  (pcase (getenv "FUTUR_WAKE_TESTS_THREADS")
    ("t" t)
    ("nil" nil)
    (v (error "FUTUR_WAKE_TESTS_THREADS must be t or nil, not %S" v))))

(defun futur-wake-tests--load ()
  "Load the futur under test once, with the requested backend."
  (let ((dir (or (getenv "FUTUR_WAKE_TESTS_SRC")
                 (error "FUTUR_WAKE_TESTS_SRC is unset")))
        (threads (futur-wake-tests--threads)))
    (when (featurep 'futur) (error "futur is already loaded"))
    (set-default 'futur-use-threads threads)
    (load (expand-file-name "futur" dir) nil t)
    (princ (format "S0F3-LOAD file=%s threads=%S emacs=%s\n"
                   (symbol-file 'futur--funcall) threads emacs-version))
    (should (eq (and futur--background t) threads))))

(defun futur-wake-tests--named (prefix)
  "Return the processes whose name starts with PREFIX."
  (cl-remove-if-not (lambda (p) (string-prefix-p prefix (process-name p)))
                    (process-list)))

(defun futur-wake-tests--no-kick-processes ()
  "Check that no wake or fallback process exists."
  (should-not (futur-wake-tests--named "futur-wake"))
  (should-not (futur-wake-tests--named "futur-bug80468")))

;;; ─── AC1/AC2: timer-enqueued continuation ─────────────────────────

(defun futur-wake-tests--timer-chain ()
  "Enqueue a continuation from inside a timer, then wait once.
Return (DELIVERED RAN RETURNED): when the timer called `futur--funcall',
when the continuation ran (nil if not yet), and when the wait returned.
The fixture adds no other timer, no polling and no idle hook."
  (let ((delivered nil) (ran nil))
    (run-with-timer 0 nil
                    (lambda ()
                      (setq delivered (float-time))
                      (futur--funcall (lambda () (setq ran (float-time))))))
    (accept-process-output nil 1)
    (list delivered ran (float-time))))

(defun futur-wake-tests--report (label delivered ran returned)
  "Print the AC1 numbers under LABEL; return the observed shape."
  (let ((shape (cond ((and ran (<= (- ran delivered) 0.05)) 'prompt)
                     ((and (null ran) (>= (- returned delivered) 0.9)) 'late)
                     (t 'other))))
    (princ (format "%s delivered=%.6f ran=%s returned=%.6f ran-delivered=%s returned-delivered=%.6f shape=%s\n"
                   label delivered
                   (if ran (format "%.6f" ran) "nil")
                   returned
                   (if ran (format "%.6f" (- ran delivered)) "nil")
                   (- returned delivered) shape))
    shape))

(ert-deftest futur-wake/ac1-timer-enqueued-continuation ()
  "A continuation enqueued from a timer runs within 50 ms (AC1).
With FUTUR_WAKE_TESTS_MUTANT=drop-kick the wake byte is never written,
so the same assertion must fail (AC2)."
  (futur-wake-tests--load)
  (let* ((mutant (getenv "FUTUR_WAKE_TESTS_MUTANT"))
         (wake (futur-wake-tests--named "futur-wake"))
         (dropped 0)
         (send (symbol-function 'process-send-string))
         (result
          (pcase mutant
            ('nil (futur-wake-tests--timer-chain))
            ("drop-kick"
             (should (= 2 (length wake)))
             (cl-letf (((symbol-function 'process-send-string)
                        (lambda (process string)
                          (if (memq process wake)
                              (cl-incf dropped)
                            (funcall send process string)))))
               (futur-wake-tests--timer-chain)))
            (_ (error "Unknown FUTUR_WAKE_TESTS_MUTANT %S" mutant)))))
    (pcase-let* ((`(,delivered ,ran ,returned) result)
                 (shape (futur-wake-tests--report
                         (format "S0F3-AC1 mutant=%s dropped=%d"
                                 (or mutant "none") dropped)
                         delivered ran returned)))
      (should delivered)
      (when mutant (should (= dropped 1)))
      (should (eq shape 'prompt)))))

;;; ─── AC3/AC5: controls on both backends ───────────────────────────

(defmacro futur-wake-tests--control (name doc &rest body)
  "Define control NAME: load futur, check the kick processes, run BODY."
  (declare (indent 1) (doc-string 2))
  `(ert-deftest ,name ()
     ,doc
     (futur-wake-tests--load)
     (when futur-use-threads (futur-wake-tests--no-kick-processes))
     (let ((start (float-time)))
       (unwind-protect (progn ,@body)
         (princ (format "S0F3-AC3 %s threads=%S elapsed=%.6f\n"
                        ',name futur-use-threads (- (float-time) start)))))))

(futur-wake-tests--control futur-wake/ac3-filter-enqueued
  "A value delivered from a process filter reaches a bound continuation."
  (let* ((f (futur-new
             (lambda (fut)
               (make-process :name "s0f3-echo" :command '("/bin/echo" "x")
                             :connection-type 'pipe :noquery t
                             :filter (lambda (_p s) (futur-deliver-value fut s))
                             :sentinel #'ignore))))
         (chain (futur-bind f (lambda (s) (concat s "y")))))
    (should (equal "x\ny" (futur-blocking-wait-to-get-result chain)))))

(futur-wake-tests--control futur-wake/ac3-sentinel-enqueued
  "A value delivered from a process sentinel reaches a bound continuation."
  (let* ((f (futur-new
             (lambda (fut)
               (make-process :name "s0f3-exit" :command '("/bin/sh" "-c" "exit 3")
                             :connection-type 'pipe :noquery t
                             :filter #'ignore
                             :sentinel (lambda (p _event)
                                         (when (eq 'exit (process-status p))
                                           (futur-deliver-value
                                            fut (process-exit-status p))))))))
         (chain (futur-bind f #'1+)))
    (should (equal 4 (futur-blocking-wait-to-get-result chain)))))

(futur-wake-tests--control futur-wake/ac3-bind-chain
  "Chained binds deliver from a done futur and from a timer futur."
  (should (equal 3 (futur-blocking-wait-to-get-result
                    (futur-bind (futur-bind (futur-done 1) #'1+) #'1+))))
  (should (equal 2 (futur-blocking-wait-to-get-result
                    (futur-bind (futur-bind (futur-timeout 0.01)
                                            (lambda (_) 1))
                                #'1+)))))

(futur-wake-tests--control futur-wake/ac3-error-propagation
  "A failure propagates through a bind, and an error handler catches it."
  (should-error (futur-blocking-wait-to-get-result
                 (futur-bind (futur-failed '(arith-error)) #'1+))
                :type 'arith-error)
  (should (eq 'handled
              (futur-blocking-wait-to-get-result
               (futur-bind (futur-failed '(arith-error)) #'1+
                           (list (cons 'arith-error
                                       (lambda (_err) 'handled))))))))

(futur-wake-tests--control futur-wake/ac3-timeout
  "A timeout futur completes with nil after its delay."
  (let ((t0 (float-time)))
    (should-not (futur-blocking-wait-to-get-result (futur-timeout 0.05)))
    (should (<= 0.05 (- (float-time) t0) 2))))

(futur-wake-tests--control futur-wake/ac3-cancel
  "Aborting a waiting futur fails it and cancels its timer."
  (let* ((before (copy-sequence timer-list))
         (f (futur-timeout 10))
         (added (cl-set-difference timer-list before)))
    (should (= 1 (length added)))
    (futur-abort f "s0f3 cancel")
    (should-error (futur-blocking-wait-to-get-result f) :type 'futur-aborted)
    (should-not (cl-intersection added timer-list))))

;;; ─── AC4: wake shape, setup failure, unload (no-thread backend) ───

(defun futur-wake-tests--load-failing (failing-step)
  "Load futur with FAILING-STEP of the wake setup made to fail.
FAILING-STEP is `listener' or `sender', where `make-network-process'
signals, or `peer', where the real pair is made and only the accepted
receiver's :remote contact is reported as a different endpoint.
Return the warnings emitted, which are recorded instead of displayed."
  (let ((make (symbol-function 'make-network-process))
        (contact (symbol-function 'process-contact))
        (made 0) (faked 0) (warnings nil))
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest args)
                 (if (eq (if (plist-get args :server) 'listener 'sender)
                         failing-step)
                     (signal 'file-error (list "s0f3 forced failure" failing-step))
                   (cl-incf made)
                   (apply make args))))
              ((symbol-function 'process-contact)
               (lambda (process &optional key &rest rest)
                 (if (and (eq failing-step 'peer) (eq key :remote)
                          (string-prefix-p "futur-wake-receiver "
                                           (process-name process)))
                     (progn (cl-incf faked) [127 0 0 1 1])
                   (apply contact process key rest))))
              ((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (futur-wake-tests--load)
      (when (eq failing-step 'peer)
        ;; All three endpoints existed: listener and sender were made,
        ;; and the accepted receiver's peer was consulted.
        (should (= 2 made))
        (should (<= 1 faked)))
      (should-not (futur-wake-tests--named "futur-wake"))
      (should (= 1 (length (futur-wake-tests--named "futur-bug80468"))))
      (futur--funcall #'ignore)
      (futur--funcall #'ignore)
      (accept-process-output nil 0.05))
    (princ (format "S0F3-AC4 failing=%s made=%d faked=%d warnings=%S\n"
                   failing-step made faked warnings))
    warnings))

(ert-deftest futur-wake/ac4-pair-shape ()
  "After a load, exactly two wake processes exist, a verified pair."
  (futur-wake-tests--load)
  (should-not futur-use-threads)
  (let ((sender (car (futur-wake-tests--named "futur-wake-sender")))
        (receiver (car (futur-wake-tests--named "futur-wake-receiver"))))
    (should (= 2 (length (futur-wake-tests--named "futur-wake"))))
    (should-not (futur-wake-tests--named "futur-bug80468"))
    (dolist (p (list sender receiver))
      (should (process-live-p p))
      (should (eq 'network (process-type p)))
      (should (eq main-thread (process-thread p))))
    (should (equal (process-contact receiver :remote)
                   (process-contact sender :local)))))

(ert-deftest futur-wake/ac4-listener-fails ()
  "No listener: no TCP endpoint, one fallback process, one warning."
  (should (= 1 (length (futur-wake-tests--load-failing 'listener)))))

(ert-deftest futur-wake/ac4-sender-fails ()
  "No sender: the listener is deleted, one fallback process, one warning."
  (should (= 1 (length (futur-wake-tests--load-failing 'sender)))))

(ert-deftest futur-wake/ac4-wrong-peer ()
  "A wrong peer is rejected after all three endpoints existed.
None survives, one fallback process and one warning remain, and
unloading restores the pre-load process baseline."
  (let ((baseline (process-list)))
    (should (= 1 (length (futur-wake-tests--load-failing 'peer))))
    (unload-feature 'futur t)
    (should-not (cl-set-exclusive-or baseline (process-list)))))

(ert-deftest futur-wake/ac4-unload-pair ()
  "Unloading futur from the pair state leaves no process behind."
  (let ((baseline (process-list)))
    (futur-wake-tests--load)
    (should (futur-wake-tests--named "futur-wake"))
    (unload-feature 'futur t)
    (should-not (cl-set-exclusive-or baseline (process-list)))))

(ert-deftest futur-wake/ac4-unload-fallback ()
  "Unloading futur from the fallback state leaves no process behind."
  (let ((baseline (process-list)))
    (futur-wake-tests--load-failing 'listener)
    (unload-feature 'futur t)
    (should-not (cl-set-exclusive-or baseline (process-list)))))

(provide 'futur-wake-tests)
;;; futur-wake-tests.el ends here
