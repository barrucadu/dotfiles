(load (merge-pathnames #P".dwim/dwimdefs.lisp" (user-homedir-pathname)))

;; Check if a file is a text file (or empty).
(defun is-text-file-p (path)
  (let ((mime (system-1 "file" (list path))))
    (or (search " text" mime) (search " empty" mime))))

;; Check if a name is in ~/.ssh/config or /etc/hosts.
(defun is-host-name-p (host)
  (let ((grep (lambda (fn) (system-2 "grep" (list (concat "\\b" host "\\b") fn)))))
    (or (equal 0 (funcall grep (merge-pathnames ".ssh/config" (user-homedir-pathname))))
        (equal 0 (funcall grep #P"/etc/hosts")))))

;; Check if a binary is on the $PATH.
(defun has-p (exe)
  (let ((check (lambda (path) (probe-file (merge-pathnames exe (concat path "/"))))))
    (some check (split-on (sb-ext:posix-getenv "PATH") ":"))))

;; Check if mosh is on the $PATH and the hostname is in ~/.ssh/use_mosh.
(defun use-mosh-p (host)
  (and (has-p "mosh")
       (equal 0 (system-2 "grep" (list (concat "^" host "$") (merge-pathnames ".ssh/use_mosh" (user-homedir-pathname)))))))

;; Check if the current hostname matches a string.
(defun current-hostname-p (host)
  (string= host (system-1 "hostname")))

;; Check if a tmux session exists
(defun tmux-p (session)
  (and (has-p "tmux")
       (equal 0 (system-2 "tmux" (list "has" "-t" session)))))

;; Produce a predicate to check if a file has a given extension.
(defun has-filetype-p (ext)
  #'(lambda (path) (string= ext (pathname-type path))))

;; Produce a predicate to check if a path is in a directory.
(defun in-directory-p (dir)
  #'(lambda (path) (equal (pathname-directory dir) (pathname-directory path))))

;; Rule definitions.
(when (eq 1 *argn*)
  (let ((*arg*     (car *argv*))
        (*arg-raw* (car *argv-raw*)))
    (defrule :edit-text-file (is-text-file-p *arg*)     '(("emacs")))
    (defrule :ssh-to-host    (is-host-name-p *arg-raw*) `((,(if (use-mosh-p *arg-raw*) "mosh" "ssh") :args ,*argv-raw*)))
    (defrule :attach-tmux    (tmux-p         *arg-raw*) `(("tmux" :args ("attach-session" "-d" "-t" ,*arg-raw*))))))

(when (current-hostname-p "azathoth")
  (let ((torrent-watch-dir   #P"/home/barrucadu/nfs/torrents/watch/")
        (torrent-seedbox-dir #P"/home/barrucadu/nfs/torrents/on_seedbox/")
        (download-dir        #P"/home/barrucadu/tmp/"))
    (defrule :move-torrent-files (every (has-filetype-p "torrent") *argv*)
      (cond ((every (in-directory-p download-dir)        *argv*) '(("mv" :post-args (torrent-seedbox-dir))))
            ((every (in-directory-p torrent-seedbox-dir) *argv*) '(("mv" :post-args (torrent-watch-dir))))))))

(when (current-hostname-p "nyarlathotep")
  (let ((torrent-watch-dir   #P"/srv/share/torrents/watch/")
        (torrent-seedbox-dir #P"/srv/share/torrents/on_seedbox/"))
    (defrule :move-torrent-files (every (has-filetype-p "torrent") *argv*)
      (cond ((every (in-directory-p torrent-seedbox-dir) *argv*) '(("cp" :post-args ("."))
                                                                   ("mv" :post-args (torrent-watch-dir))))
            ((every (in-directory-p torrent-watch-dir)   *argv*) '(("cp" :post-args ("."))))))))
