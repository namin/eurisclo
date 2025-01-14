(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(ql:quickload "eurisclo")
(in-package :eurisclo)

(defun hash-seed (seed)
  "Hash a numeric SEED deterministically into a smaller, bounded value."
  (mod (abs (logxor seed (ash seed -3))) most-positive-fixnum))

(defun initialize-seed (seed)
  "Initialize a random-state deterministically based on a hashed SEED."
  (let ((state (make-random-state nil))) ; Fresh, isolated random state
    ;; Progress the random state deterministically
    (dotimes (i (hash-seed seed))
      (random most-positive-fixnum state))
    state))

(defun run-stat (&optional (key 'H24-INTERP2))
  "Retrieve statistics for a specific heuristic."
  (let ((m (remove-if-not #'(lambda (x) (eq key (car x))) (run-stats))))
    (if m
        (car m)
        (list key 0 0 0))))

(defun stat-total (stat)
  (cadddr stat))
(defun stat-success (stat)
  (caddr stat))

(defun continue-criteria (stat)
  (if (= 0 (stat-success stat))
      (< (stat-total stat) 30)
      (< (stat-total stat) 50)))

(defun stop-criteria (stat)
  (not (continue-criteria stat)))

(defun success-criteria (stat)
  (stat-success stat))

(setf *batch-mode* t)

(let ((seed-arg (cadr sb-ext:*posix-argv*)))
  (let ((seed (parse-integer seed-arg)))
    (setf *random-state* (initialize-seed seed)) ; Use our seeded random state
    (format t "Running eurisko with seed ~A~%" seed)
    (eurisko 0 t)
    (start t (lambda () (stop-criteria (run-stat))))
    (format t "Stats: ~A~%" (run-stats))
    (format t "Success metric: ~A~%" (success-criteria (run-stat)))))
