#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
#!r6rs

(import (rnrs (6))
        (ockham core)
        (ockham ops)
        (ockham wasm-emit)
        (only (chezscheme) system get-process-id))

(init-ops!)

(define (print-usage)
  (display "Usage: ockham [input-file] [-o output.wasm]\n" (current-error-port))
  (display "Compiles Ockham IR to WebAssembly binary (.wasm).\n" (current-error-port))
  (display "If input-file is omitted or '-', reads from stdin.\n" (current-error-port))
  (display "If -o option is omitted, outputs WASM binary to stdout.\n" (current-error-port)))

(define (parse-cli-args args)
  (let loop ((rest args) (input-file #f) (output-file #f))
    (cond
      ((null? rest)
       (values input-file output-file))
      ((or (string=? (car rest) "-h") (string=? (car rest) "--help"))
       (print-usage)
       (exit 0))
      ((string=? (car rest) "-o")
       (if (null? (cdr rest))
           (begin
             (display "Error: -o option requires an output filename\n" (current-error-port))
             (exit 1))
           (loop (cddr rest) input-file (cadr rest))))
      ((and (> (string-length (car rest)) 2)
            (string=? (substring (car rest) 0 2) "-o"))
       (loop (cdr rest) input-file (substring (car rest) 2 (string-length (car rest)))))
      ((string=? (car rest) "-")
       (loop (cdr rest) "-" output-file))
      ((char=? (string-ref (car rest) 0) #\-)
       (display (string-append "Error: Unknown option " (car rest) "\n") (current-error-port))
       (print-usage)
       (exit 1))
      (else
       (if (not input-file)
           (loop (cdr rest) (car rest) output-file)
           (loop (cdr rest) input-file output-file))))))

(define (read-input-sexp input-file)
  (if (or (not input-file) (string=? input-file "-"))
      (read (current-input-port))
      (call-with-input-file input-file
        (lambda (port) (read port)))))

(define (find-wat2wasm-cmd)
  (cond
    ((file-exists? "bin/wat2wasm") "bin/wat2wasm")
    ((file-exists? "./bin/wat2wasm") "./bin/wat2wasm")
    (else "wat2wasm")))

(define (main)
  (let-values (((input-file output-file) (parse-cli-args (cdr (command-line)))))
    (let ((sexp (read-input-sexp input-file)))
      (if (eof-object? sexp)
          (begin
            (display "Error: Unexpected end of input\n" (current-error-port))
            (exit 1))
          (let ((mod-obj (if (module? sexp) sexp (module-deserialize sexp))))
            (if (not (module? mod-obj))
                (begin
                  (display "Error: Invalid OKM IR input (failed to deserialize module)\n" (current-error-port))
                  (exit 1))
                (let* ((wat-str (emit-wasm-wat-string mod-obj))
                       (pid (get-process-id))
                       (temp-wat (string-append "/tmp/ockham_" (number->string pid) ".wat"))
                       (temp-wasm (string-append "/tmp/ockham_" (number->string pid) ".wasm"))
                       (wat2wasm (find-wat2wasm-cmd)))
                  ;; Write temporary WAT file
                  (call-with-port (open-file-output-port temp-wat (file-options replace) (buffer-mode block) (make-transcoder (utf-8-codec)))
                    (lambda (p) (put-string p wat-str)))

                  (if output-file
                      ;; Output to file specified by -o
                      (let ((status (system (string-append wat2wasm " " temp-wat " -o " output-file))))
                        (when (file-exists? temp-wat) (delete-file temp-wat))
                        (unless (zero? status)
                          (display "Error: wat2wasm compilation failed\n" (current-error-port))
                          (exit status)))
                      ;; Output to stdout by default
                      (let ((status (system (string-append wat2wasm " " temp-wat " -o " temp-wasm))))
                        (when (file-exists? temp-wat) (delete-file temp-wat))
                        (unless (zero? status)
                          (display "Error: wat2wasm compilation failed\n" (current-error-port))
                          (exit status))
                        (let ((bytes (call-with-port (open-file-input-port temp-wasm) get-bytevector-all)))
                          (when (file-exists? temp-wasm) (delete-file temp-wasm))
                          (put-bytevector (standard-output-port) bytes)
                          (flush-output-port (standard-output-port))))))))))))

(main)
