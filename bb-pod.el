;;; bb-pod.el --- Babashka pod interface library -*- lexical-binding: t -*-

;; Copyright © 2023 Amit Shrestha
;;
;; Author: Amit Shrestha
;; Maintainer: Amit Shrestha
;; URL: https://github.com/rorokimdim/bb-pod
;; Version: 0.0.1
;; Package-Requires: ((emacs "26") (parseedn "20220520.835"))
;; Keywords: babashka

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides functions to interact with a Babashka pod. See https://github.com/babashka/pods.

;;; Installation:

;; bb-pod is available as a package in melpa.org and stable.melpa.org. First, make sure you've
;; enabled one of the repositories in your Emacs config:

;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;;
;; or
;;
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Next, install bb-pod using package-install:

;; M-x package-refresh-contents
;; M-x package-install bb-pod

;;; Usage:

;; Start a pod using bb-pod/start:

;; (setq pod (bb-pod/start "pod-prefix" '("path/to/pod-executable") "pod-namespace"))
;;
;; If the call succeeds, all exposed pod vars will be avaialble as functions with 'pod-prefix/' prefix.

;;; Code:

(defgroup bb-pod nil
  "Babashka pod interface library."
  :prefix "bb-pod-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/rorokimdim/bb-pod")
  :link '(emacs-commentary-link :tag "Commentary" "bb-pod"))

(require 'parseedn)

(define-error 'bb-pod "bb-pod error")
(define-error 'bb-pod-dead-pod "pod is not running. start a new pod with bb-pod/start" 'bb-pod)
(define-error 'bb-pod-pod-exception "pod raised an exception" 'bb-pod)

(setq bb-pod--pod-id-seq 0)

(defun bb-pod/start (pod-name pod-command pod-ns)
  "Starts a pod.

  All pod functions will be made available under POD-NAME/ prefix. For example,
  if POD-NAME is 'a', a function named 'compute' will be made available as a/compute.
  The function will take in the pod (returned from this function) as its first argument followed
  by its expected arguments.

  POD-COMMAND is the command to run the pod (in exec form; i.e as a list of strings). If the pod does
  not require any argumnets, just pass in (list command-path).

  POD-NS is the namespace to load from the pod.

  See https://github.com/babashka/pods
  "
  (let* ((pod-id (bb-pod--generate-pod-id))
         (buffer-name (concat pod-name "-" pod-id))
         (sentinel-fn (lambda (proc event)
                        (when (buffer-live-p (process-buffer proc))
                          (with-current-buffer (process-buffer proc)
                            (goto-char (point-max))
                            (insert ":event " event "\n")))))
         (pod-process (with-environment-variables (("BABASHKA_POD" "true"))
                        (make-process
                         :name pod-name
                         :command pod-command
                         :buffer buffer-name
                         :stderr buffer-name
                         :sentinel sentinel-fn)))
         (pod (bb-pod--construct-pod pod-id pod-name pod-process pod-ns))
         (filter-fn (lambda (proc message)
                      (bb-pod--assoc-pod-state pod :message message)
                      (when (buffer-live-p (process-buffer proc))
                        (with-current-buffer (process-buffer proc)
                          (goto-char (point-max))
                          (insert ":message " message "\n"))))))
    (set-process-filter pod-process filter-fn)
    (bb-pod--load-namespace pod)
    pod))

(defun bb-pod/shutdown (pod)
  "Shutdowns a pod.

  POD is the value returned from bb-pod/start function.
  "
  (let* ((pod-id (bb-pod--select-pod-id pod))
         (proc (bb-pod--select-pod-process pod))
         (proc-buffer (process-buffer proc)))
    (when (process-live-p proc)
      (process-send-string proc (bb-pod--bencode-encode (list :op "shutdown")))
      (kill-process proc)
      (with-current-buffer (process-buffer proc)
        (let ((buffer-modified-p nil)
              (kill-buffer-query-functions nil))
          (kill-buffer))))))


;;
;; The rest is private.
;;

(defun bb-pod--generate-pod-id ()
  (concat (number-to-string (cl-incf bb-pod--pod-id-seq))
          "-"
          (number-to-string (random 1000000))))

(defun bb-pod--construct-pod (pod-id pod-name pod-process pod-ns)
  (list pod-id
        pod-name
        pod-process
        (make-hash-table :test #'equal)
        pod-ns))

(defun bb-pod--select-pod-id (pod)
  (first pod))

(defun bb-pod--select-pod-name (pod)
  (second pod))

(defun bb-pod--select-pod-process (pod)
  (third pod))

(defun bb-pod--select-pod-state (pod)
  (fourth pod))

(defun bb-pod--select-pod-ns (pod)
  (fifth pod))

(defun bb-pod--lookup-pod-state (pod key)
  (let ((state (bb-pod--select-pod-state pod)))
    (gethash key state)))

(defun bb-pod--assoc-pod-state (pod key value)
  (let ((state (bb-pod--select-pod-state pod)))
    (puthash key value state)))

(defun bb-pod--check-error (response)
  (let ((ex-message (plist-get response :ex-message)))
    (when ex-message
      (let* ((ex-data (json-parse-string (plist-get response :ex-data)))
             (ex-type (gethash "type" ex-data)))
        (signal 'bb-pod-pod-exception (list :ex-type ex-type :ex-message ex-message))))))

(defun bb-pod--send-request (pod request &optional timeout-seconds)
  (let* ((pod-id (bb-pod--select-pod-id pod))
         (pod-process (bb-pod--select-pod-process pod))
         (timeout-seconds (or timeout-seconds 5)))
    (bb-pod--assoc-pod-state pod :message nil)
    (process-send-string pod-process (bb-pod--bencode-encode request))
    (accept-process-output pod-process timeout-seconds)
    (let ((message (bb-pod--lookup-pod-state pod :message)))
      (when message
        (bb-pod--assoc-pod-state pod :message nil)
        (let* ((decoded (bb-pod--bencode-decode message)))
          (bb-pod--check-error decoded)
          decoded)))))

(defun bb-pod--ensure-alive (pod)
  (when (not (bb-pod/alive-p pod))
    (signal 'bb-pod-dead-pod pod)))

(defun bb-pod--describe (pod)
  (bb-pod--ensure-alive pod)
  (let* ((pod-id (bb-pod--select-pod-id pod))
         (rid (concat pod-id "/describe/" (number-to-string (random 100000))))
         (request (list :op "describe" :id rid)))
    (bb-pod--send-request pod request)))

(defun bb-pod--invoke (pod name &rest args)
  (bb-pod--ensure-alive pod)
  (let* ((pod-id (bb-pod--select-pod-id pod))
         (pod-ns (bb-pod--select-pod-ns pod))
         (rid (concat pod-id "/" name "/" (number-to-string (random 100000))))
         (var (concat pod-ns "/" name))
         (request (list :op "invoke" :id rid :var var :args (json-encode args)))
         (response (bb-pod--send-request pod request)))
    (when response
      (let ((value (plist-get response :value)))
        (json-parse-string value)))))

(defun bb-pod/clear-pod-buffer (pod)
  (let ((proc (bb-pod--select-pod-process pod)))
    (when (process-live-p proc)
      (with-current-buffer (process-buffer proc)
        (erase-buffer)))))

(defun bb-pod/alive-p (pod)
  (process-live-p (bb-pod--select-pod-process pod)))

(defun bb-pod--load-namespace (pod)
  (let* ((pod-description (bb-pod--describe pod))
         (pod-ns (bb-pod--select-pod-ns pod))
         (pod-name (bb-pod--select-pod-name pod))
         (namespaces (plist-get pod-description :namespaces))
         (namespaces (remove-if-not (lambda (x) (equal (plist-get x :name) pod-ns)) namespaces))
         (vars (plist-get (first namespaces) :vars))
         (load-var (lambda (x)
                     (let* ((name (plist-get x :name))
                            (meta (parseedn-read-str (plist-get x :meta)))
                            (docstring (gethash :doc meta))
                            (arglists (gethash :arglists meta))
                            (args-doc (if (= (length arglists) 1)
                                          (format "must be %s" (first arglists))
                                        (format "must be one of %s" arglists))))
                       (defalias (intern (concat pod-name "/" name))
                         `(lambda (pod &rest args)
                            ,(format "%s\n args: %s" docstring args-doc)
                            (apply #'bb-pod--invoke pod ,name args)))))))
    (mapcar load-var vars)))

;;
;; bencode implementation -- not fast
;;
;; For a better, faster implementation checkout https://github.com/skeeto/emacs-bencode
;;
;; This code is similar but uses recursion to encode/decode so may not be suitable for
;; parsing highly nested data.
;;
;; Also, this code does not worry about keeping the dictionary keys sorted.
;;
(defun bb-pod--plistp (x)
  (and (consp x)
       (keywordp (car x))))

(defun bb-pod--bencode-encode-to-buffer (x)
  (cond ((integerp x) (bb-pod--bencode-encode-int x))
        ((stringp x) (bb-pod--bencode-encode-string x))
        ((keywordp x) (bb-pod--bencode-encode-string
                       (substring (symbol-name x) 1)))
        ((bb-pod--plistp x) (bb-pod--bencode-encode-plist x))
        ((hash-table-p x) (bb-pod--bencode-encode-hash-table x))
        ((listp x) (bb-pod--bencode-encode-list x))
        ((vectorp x) (bb-pod--bencode-encode-list
                      (coerce x 'list)))
        (t (signal 'bencode-unsupported-type x))))

(defun bb-pod--bencode-encode (x)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (bb-pod--bencode-encode-to-buffer x)
    (buffer-string)))

(defun bb-pod--bencode-encode-int (x)
  (insert "i" (number-to-string x) "e"))

(defun bb-pod--bencode-encode-string (x)
  (insert (number-to-string (length x)) ":" x))

(defun bb-pod--map-plist (fn xs)
  (cl-loop for (k v) on xs by 'cddr
           collect (funcall fn k v)))

(defun bb-pod--bencode-encode-plist (xs)
  (unless (evenp (length xs))
    (signal 'bencode-invalid-plist-entries xs))
  (insert "d")
  (bb-pod--map-plist (lambda (k v)
                       (bb-pod--bencode-encode-to-buffer k)
                       (bb-pod--bencode-encode-to-buffer v))
                     xs)
  (insert "e"))

(defun bb-pod--bencode-encode-hash-table (ht)
  (insert "d")
  (maphash (lambda (k v)
             (bb-pod--bencode-encode-to-buffer k)
             (bb-pod--bencode-encode-to-buffer v))
           ht)
  (insert "e"))

(defun bb-pod--bencode-encode-list (xs)
  (insert "l")
  (dolist (x xs)
    (bb-pod--bencode-encode-to-buffer x))
  (insert "e"))

(defun bb-pod--digitp (x)
  (and x (>= x ?0 ) (<= x ?9)))

(defun bb-pod--char-to-string (x)
  (and x (char-to-string x)))

(defun bb-pod--bencode-decode-int ()
  (unless (= (char-after) ?i)
    (signal 'bencode-invalid-integer-encoding (bb-pod--char-to-string (char-after))))

  (forward-char)
  (let ((int-value 0)
        (sign 1)
        (c (char-after)))
    (cond ((= c ?+) (forward-char))
          ((= c ?-) (progn (setf sign -1)
                           (forward-char))))
    (setf c (char-after))
    (while (bb-pod--digitp c)
      (setf int-value (+ (* int-value 10)
                         c
                         (- ?0)))
      (forward-char)
      (setf c (char-after)))
    (unless (and c (= c ?e))
      (signal 'bencode-invalid-integer-encoding (bb-pod--char-to-string c)))
    (forward-char)
    (* sign int-value)))

(defun bb-pod--bencode-decode-string ()
  (let ((n 0)
        (c (char-after)))
    (while (bb-pod--digitp c)
      (setf n (+ (* n 10)
                 c
                 (- ?0)))
      (forward-char)
      (setf c (char-after)))
    (unless (and c (= c ?:))
      (signal 'bencode-invalid-integer-encoding (bb-pod--char-to-string c)))
    (forward-char)
    (cond
     ((= n 0) "")
     ((> (+ (point) n) (point-max)) (signal 'bencode-invalid-string-encoding 'end-of-file))
     (t (prog1 (buffer-substring (point) (+ (point) n))
          (forward-char n))))))

(defun bb-pod--bencode-decode-list ()
  (unless (= (char-after) ?l)
    (signal 'bencode-invalid-list-encoding (bb-pod--char-to-string (char-after))))

  (forward-char)
  (let ((c (char-after))
        (xs ()))
    (while (not (= c ?e))
      (push (bb-pod--bencode-decode-buffer) xs)
      (setf c (char-after)))
    (forward-char)
    (nreverse xs)))

(defun bb-pod--bencode-decode-dict ()
  (unless (= (char-after) ?d)
    (signal 'bencode-invalid-list-encoding (bb-pod--char-to-string (char-after))))

  (forward-char)
  (let ((c (char-after))
        (xs ()))
    (while (not (= c ?e))
      (let ((k (bb-pod--bencode-decode-buffer))
            (v (bb-pod--bencode-decode-buffer)))
        (push (intern (concat ":" k)) xs)
        (push v xs))
      (setf c (char-after)))
    (forward-char)
    (nreverse xs)))

(defun bb-pod--bencode-decode-buffer ()
  (let ((start (char-after)))
    (cond ((= start ?i) (bb-pod--bencode-decode-int))
          ((= start ?d) (bb-pod--bencode-decode-dict))
          ((= start ?l) (bb-pod--bencode-decode-list))
          ((bb-pod--digitp start) (bb-pod--bencode-decode-string))
          (t (signal 'bencode-invalid-encoding (bb-pod--char-to-string start))))))

(defun bb-pod--bencode-decode (x)
  (with-temp-buffer
    (insert x)
    (goto-char (point-min))
    (bb-pod--bencode-decode-buffer)))

(provide 'bb-pod)

;;; bb-pod.el ends here
