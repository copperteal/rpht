;;; Permission to use, copy, modify, and/or distribute this software for any
;;; purpose with or without fee is hereby granted.
;;;
;;; THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
;;; OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defpackage #:rpht
  (:use #:cl)
  (:export #:*readable-hash-tables*))

(in-package #:rpht)

(defvar *readable-hash-tables* t)

(declaim (type boolean *readable-hash-tables*))

(defun sharp-H (s c n)
  (declare (ignore c n))
  (when *read-suppress*
    (read s t nil t)
    (return-from sharp-H))
  (let* ((content (read s t nil t))
         (alist (car content))
         (initargs (cdr content)))
    (let ((ht (apply #'make-hash-table initargs)))
      (mapc #'(lambda (pair)
                (destructuring-bind (a . d) pair
                  (setf (gethash a ht) d)))
            (reverse alist))
      ht)))

(set-dispatch-macro-character #\# #\H #'sharp-H)

(defmethod print-object ((ht hash-table) s)
  (cond
    ((and (integerp *print-level*) (zerop *print-level*))
     (write-char #\# s))
    ((or *readable-hash-tables* *print-readably*)
     (let ((alist nil)
           (initargs (list :test (hash-table-test ht)
                           :size (hash-table-size ht)
                           :rehash-size (hash-table-rehash-size ht)
                           :rehash-threshold (hash-table-rehash-threshold ht))))
       (maphash #'(lambda (k v)
                    (setq alist (acons k v alist)))
                ht)
       (format s "#H~W" (list* alist initargs))))
    (t
     (print-unreadable-object (ht s :type t :identity t)
       (format s ":TEST ~W :COUNT ~W" (hash-table-test ht) (hash-table-count ht))))))
