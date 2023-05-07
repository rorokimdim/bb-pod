;;; bb-pod.el --- Babashka pod interface library -*- lexical-binding: t -*-

;; Copyright © 2023 Amit Shrestha
;;
;; Author: Amit Shrestha
;; Maintainer: Amit Shrestha
;; URL: https://github.com/rorokimdim/bb-pod
;; Version: 0.0.1
;; Package-Requires: ((emacs "26") (parseedn "20220520.835") (bencode "20190317.2010"))
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

(require 'bencode)
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
      (process-send-string proc (bencode-encode (list :op "shutdown")))
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
    (process-send-string pod-process (bencode-encode request))
    (accept-process-output pod-process timeout-seconds)
    (let ((message (bb-pod--lookup-pod-state pod :message)))
      (when message
        (bb-pod--assoc-pod-state pod :message nil)
        (let* ((decoded (bencode-decode message)))
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

(provide 'bb-pod)

;;; bb-pod.el ends here
