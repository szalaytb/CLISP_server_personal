;;; ============================================================
;;; Convert a Lisp string to raw bytes (ASCII/UTF-8 safe for headers)
;;; ============================================================
(defun ascii-string-to-bytes (s)
  ;; Create a byte array (unsigned 8-bit integers)
  (let ((bytes (make-array (length s)
                           :element-type '(unsigned-byte 8))))
    ;; Copy each character as its char-code into the byte array
    (loop for i from 0 below (length s)
          do (setf (aref bytes i)
                   (char-code (char s i))))
    bytes))


;;; ============================================================
;;; Write a string to a binary stream (as bytes)
;;; ============================================================
(defun write-ascii-string (s stream)
  ;; Convert string → bytes → write to stream
  (write-sequence (ascii-string-to-bytes s) stream))


;;; ============================================================
;;; Read one HTTP line from a binary stream
;;; ============================================================
(defun read-byte-line (stream)
  ;; HTTP lines end with CRLF (\r\n = 13,10)
  ;; We:
  ;;  - stop at LF (10)
  ;;  - ignore CR (13)
  (with-output-to-string (out)
    (loop for b = (read-byte stream nil nil)
          while (and b (/= b 10)) ; stop at LF
          do (unless (= b 13)     ; skip CR
               (write-char (code-char b) out)))))


;;; ============================================================
;;; Extract file extension (e.g. ".html", ".png")
;;; ============================================================
(defun file-extension (path)
  (let ((dot-pos (position #\. path :from-end t)))
    (when dot-pos
      (string-downcase (subseq path dot-pos)))))


;;; ============================================================
;;; Determine MIME type based on file extension
;;; ============================================================
(defun mime-type (path)
  (let ((ext (file-extension path)))
    (cond
      ;; Text types
      ((string= ext ".html") "text/html; charset=utf-8")
      ((string= ext ".css")  "text/css; charset=utf-8")
      ((string= ext ".js")   "application/javascript; charset=utf-8")

      ;; Image types (binary!)
      ((string= ext ".png")  "image/png")
      ((or (string= ext ".jpg")
           (string= ext ".jpeg")) "image/jpeg")
      ((string= ext ".gif")  "image/gif")
      ((string= ext ".webp") "image/webp")
      ((string= ext ".ico")  "image/x-icon")

      ;; Fallback
      (t "application/octet-stream"))))


;;; ============================================================
;;; Read entire file as raw bytes
;;; ============================================================
(defun read-file-as-bytes (path)
  ;; Open file in binary mode
  (with-open-file (in path
                      :direction :input
                      :element-type '(unsigned-byte 8))
    ;; Determine file size
    (let* ((size (file-length in))
           ;; Allocate byte buffer
           (bytes (make-array size
                              :element-type '(unsigned-byte 8))))
      ;; Read all bytes into buffer
      (read-sequence bytes in)
      bytes)))


;;; ============================================================
;;; Split a string by spaces (used for HTTP request line)
;;; ============================================================
(defun split-spaces (text)
  (let ((result '())
        (start 0))
    (loop for pos = (position #\Space text :start start)
          do (push (subseq text start pos) result)
          while pos
          do (setf start (1+ pos)))
    (remove "" (nreverse result) :test #'string=)))


;;; ============================================================
;;; Basic path security check
;;; ============================================================
(defun safe-path-p (path)
  ;; Prevent directory traversal attacks like:
  ;;   /../../etc/passwd
  (and path
       (not (search ".." path))
       (not (search "\\" path))))


;;; ============================================================
;;; Remove query string from URL
;;; ============================================================
(defun remove-query-string (path)
  ;; "/image.png?cache=123" → "/image.png"
  (let ((qpos (position #\? path)))
    (if qpos
        (subseq path 0 qpos)
        path)))


;;; ============================================================
;;; Map URL path to filesystem path
;;; ============================================================
(defun request-path-to-file (request-path)
  (let* ((path (remove-query-string request-path))
         ;; "/" → "/index.html"
         (clean-path (if (string= path "/")
                         "/index.html"
                         path)))
    ;; prepend "public"
    (concatenate 'string "public" clean-path)))


;;; ============================================================
;;; Send full HTTP response (binary-safe)
;;; ============================================================
(defun send-response (stream status content-type body-bytes)
  ;; Build HTTP headers (TEXT!)
  (let ((headers
          (format nil
                  "HTTP/1.1 ~A~C~C
Content-Type: ~A~C~C
Content-Length: ~A~C~C
Connection: close~C~C
~C~C"
                  status
                  #\Return #\Linefeed
                  content-type
                  #\Return #\Linefeed
                  (length body-bytes) ;; IMPORTANT: byte length!
                  #\Return #\Linefeed
                  #\Return #\Linefeed
                  #\Return #\Linefeed)))

    ;; Send headers as ASCII bytes
    (write-ascii-string headers stream)

    ;; Send raw file bytes (HTML, CSS, images, etc.)
    (write-sequence body-bytes stream)

    ;; Flush output buffer
    (finish-output stream)))


;;; ============================================================
;;; Convenience wrapper for text responses
;;; ============================================================
(defun send-text-response (stream status content-type text)
  (send-response stream
                 status
                 content-type
                 (ascii-string-to-bytes text)))


;;; ============================================================
;;; Handle one client request
;;; ============================================================
(defun handle-client (stream)
  (unwind-protect
       (let ((request-line (read-byte-line stream)))
         
         ;; ----------------------------------------------------
         ;; Read and ignore all HTTP headers
         ;; ----------------------------------------------------
         (loop for line = (read-byte-line stream)
               while (and line (not (string= line ""))))

         ;; Debug output
         (format t "Request: ~A~%" request-line)

         ;; ----------------------------------------------------
         ;; Parse request line
         ;; Example: "GET /index.html HTTP/1.1"
         ;; ----------------------------------------------------
         (let* ((parts (split-spaces request-line))
                (method (first parts))
                (path (second parts)))

           ;; ------------------------------------------------
           ;; Only allow GET requests
           ;; ------------------------------------------------
           (if (and method
                    path
                    (string= method "GET")
                    (safe-path-p path))

               ;; --------------------------------------------
               ;; Serve static file
               ;; --------------------------------------------
               (let ((file-path (request-path-to-file path)))
                 (if (probe-file file-path)
                     (send-response stream
                                    "200 OK"
                                    (mime-type file-path)
                                    (read-file-as-bytes file-path))
                     ;; File not found
                     (send-text-response stream
                                         "404 Not Found"
                                         "text/html; charset=utf-8"
                                         "<h1>404 Not Found</h1>")))

               ;; Invalid request
               (send-text-response stream
                                   "400 Bad Request"
                                   "text/html; charset=utf-8"
                                   "<h1>400 Bad Request</h1>"))))

    ;; Always close client connection
    (close stream)))


;;; ============================================================
;;; Start server loop
;;; ============================================================
(defun start-server (&key (port 8080))
  ;; Create TCP server socket
  (let ((server (socket:socket-server port)))

    (format t "Server running at http://127.0.0.1:~A/~%" port)

    (unwind-protect
         (loop
           ;; Accept client connection as binary stream
           (let ((client-stream
                   (socket:socket-accept server
                                         :element-type '(unsigned-byte 8))))
             ;; Handle request
             (handle-client client-stream)))

      ;; Cleanup
      (socket:socket-server-close server))))
