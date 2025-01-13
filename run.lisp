(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(ql:quickload "eurisclo")
(in-package :eurisclo)

(defun initialize-seed (seed)
  "Initialize the random state with the given seed."
  (setf *random-state* (make-random-state (make-random-state t)))
  (let ((seeded-state (make-random-state nil)))
    (dotimes (i seed)
      (random 1 seeded-state))
    (setf *random-state* seeded-state)))

(defun run-stat (&optional (key 'H24-INTERP2))
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
    (initialize-seed seed)
    (format t "Running eurisko with seed ~A~%" seed)
    (eurisko 0 t)
    (start t (lambda () (stop-criteria (run-stat))))
    (format t "Success metric: ~A~%" (success-criteria (run-stat)))))
