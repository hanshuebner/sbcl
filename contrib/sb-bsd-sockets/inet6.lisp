(in-package :sb-bsd-sockets)

;;; Socket class and constructor

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass inet6-socket (socket)
    ((family :initform sockint::AF-INET6))
    (:documentation "Class representing TCP and UDP over IPv6 sockets.

Examples:

 (make-instance 'inet-socket6 :type :stream :protocol :tcp)

 (make-instance 'inet-socket6 :type :datagram :protocol :udp)
")))

(defparameter *inet6-address-any* (vector 0 0 0 0))

(defun address-numbers/v6 (address)
  (loop for i from 0 below 8 by 2
     collect (let ((number (+ (* 256 (aref address i))
                              (aref address (1+ i)))))
               (unless (zerop number) number))))

(defun endpoint-string/v6 (address port)
  (format nil "~{~@[~(~X~)~]~^:~}:~A"
          (address-numbers/v6 address) port))

(defmethod socket-namestring ((socket inet6-socket))
  (ignore-errors
    (multiple-value-bind (address port) (socket-name socket)
      (endpoint-string/v6 address port))))

(defmethod socket-peerstring ((socket inet6-socket))
  (ignore-errors
   (multiple-value-bind (address port) (socket-peername socket)
     (endpoint-string/v6 address port))))

;; Binding a socket to an address and port.  Doubt that anyone's
;; actually using this much, to be honest.
(defun make-inet6-address (colon-separated-integers)
  "Return a vector of octets given a TODO. Signals an error if the string is malformed."
  (declare (type string colon-separated-integers))
  (let ((address (make-array 16 :element-type '(unsigned-byte 8)))
        (i 0))
    (dolist (component (split colon-separated-integers 8 '(#\:)))
      (multiple-value-bind (high low)
          (floor (parse-integer component :radix 16) 256)
        (check-type high (unsigned-byte 8) #+maybe octet)
        (check-type low  (unsigned-byte 8) #+maybe octet)
        (setf (aref address i)      high
              (aref address (1+ i)) low)
        (incf i 2)))
    address))

;;; our protocol provides make-sockaddr-for, size-of-sockaddr,
;;; bits-of-sockaddr

(defmethod make-sockaddr-for ((socket inet6-socket) &optional sockaddr
                              &rest address)
  (let ((host (first address))
        (port (second address))
        (sockaddr (or sockaddr (sockint::allocate-sockaddr-in6))))
    (when (and host port)
      (setf host (coerce host '(simple-array (unsigned-byte 8) (16))))
      ;; port and host are represented in C as "network-endian" unsigned
      ;; integers of various lengths.  This is stupid.  The value of the
      ;; integer doesn't matter (and will change depending on your
      ;; machine's endianness); what the bind(2) call is interested in
      ;; is the pattern of bytes within that integer.

      ;; We have no truck with such dreadful type punning.  Octets to
      ;; octets, dust to dust.

      (setf (sockint::sockaddr-in6-family sockaddr) sockint::af-inet6)
      (setf (sb-alien:deref (sockint::sockaddr-in6-port sockaddr) 0) (ldb (byte 8 8) port))
      (setf (sb-alien:deref (sockint::sockaddr-in6-port sockaddr) 1) (ldb (byte 8 0) port))
      (dotimes (i 4)
        (setf (sb-alien:deref (sockint::sockaddr-in6-flowinfo sockaddr) i) 0))

      (dotimes (i 16)
        (setf (sb-alien:deref (sockint::sockaddr-in6-addr sockaddr) i) (elt host i)))
      (dotimes (i 4)
        (setf (sb-alien:deref (sockint::sockaddr-in6-scope-id sockaddr) i) 0)))
    sockaddr))

(defmethod free-sockaddr-for ((socket inet6-socket) sockaddr)
  (sockint::free-sockaddr-in6 sockaddr))

(defmethod size-of-sockaddr ((socket inet6-socket))
  sockint::size-of-sockaddr-in6)

(defmethod bits-of-sockaddr ((socket inet6-socket) sockaddr)
  "Returns address and port of SOCKADDR as multiple values"
  (values
   (coerce (loop for i from 0 below 16
                 collect (sb-alien:deref (sockint::sockaddr-in6-addr sockaddr) i))
           '(vector (unsigned-byte 8) 16))
   (+ (* 256 (sb-alien:deref (sockint::sockaddr-in6-port sockaddr) 0))
      (sb-alien:deref (sockint::sockaddr-in6-port sockaddr) 1))))
