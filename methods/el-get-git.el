;;; el-get --- Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010-2011 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get
;; GIT: https://github.com/dimitri/el-get
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install
;;     Please see the README.asciidoc file from the same distribution

(require 'el-get-core)

(defcustom el-get-git-clone-hook nil
  "Hook run after git clone."
  :group 'el-get
  :type 'hook)

(defun el-get-git-executable ()
  "Return git executable to use, or signal an error when not
found."
  (let ((git-executable (if (and (boundp 'magit-git-executable)
				 (file-executable-p magit-git-executable))
			    magit-git-executable
			  (executable-find "git"))))
    (unless (and git-executable (file-executable-p git-executable))
      (error
       (concat "el-get-git-clone requires `magit-git-executable' to be set, "
	       "or the binary `git' to be found in your PATH")))
    git-executable))

(defun el-get-git-clone (package url post-install-fun)
  "Clone the given package following the URL."
  (let* ((git-executable (el-get-executable-find "git"))
	 (pdir   (el-get-package-directory package))
	 (pname  (el-get-as-string package))
	 (name   (format "*git clone %s*" package))
	 (source (el-get-package-def package))
	 (branch (plist-get source :branch))
	 (args   (if branch
		     (list "--no-pager" "clone" "-b" branch url pname)
		   (list "--no-pager" "clone" url pname)))
	 (ok     (format "Package %s installed." package))
	 (ko     (format "Could not install package %s." package)))
    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,git-executable
		      :args ,args
		      :message ,ok
		      :error ,ko)
       (:command-name "*git submodule update*"
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ("--no-pager" "submodule" "update" "--init" "--recursive")
		      :message "git submodule update ok"
		      :error "Could not update git submodules"))
     post-install-fun)))

(defun el-get-git-pull (package url post-update-fun)
  "git pull the package."
  (let* ((git-executable (el-get-executable-find "git"))
	 (pdir (el-get-package-directory package))
	 (name (format "*git pull %s*" package))
	 (ok   (format "Pulled package %s." package))
	 (ko   (format "Could not update package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ( "--no-pager" "pull")
		      :message ,ok
		      :error ,ko)
       (:command-name "*git submodule update*"
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ("--no-pager" "submodule" "update" "--init" "--recursive")
		      :message "git submodule update ok"
		      :error "Could not update git submodules"))
     post-update-fun)))


(defun el-get-git-diff (package)
  "git diff what we have and what the update would be"
  (let* ((git-executable (el-get-executable-find "git"))
         (pdir (el-get-package-directory package))
         (buff (format "*git diff %s*" package))
         (src  (format "--src-prefix=%s" pdir))
         (dst  (format "--dst-prefix=%s" "update: ")))
    (with-current-buffer (get-buffer-create el-get-diff-buffer)
      (unless (boundp 'tmp-diff)
        (diff-mode)
        (set (make-local-variable 'tmp-diff) '()))
      (push (cons package '()) tmp-diff))
    (el-get-start-process-list
     package
     `((:command-name ,(format "*git fetch %s*" package)
                      :buffer-name ,buff
                      :default-directory ,pdir
                      :program ,git-executable
                      :args ( "--no-pager" "fetch")
                      ;; :process-filter ,(function el-get-diff-filter)
                      :message ,(format "git fetch for package %s ok." package)
                      :error ,(format "git fetch package %s failed." package))
       (:command-name ,buff
                      :buffer-name ,buff
                      :default-directory ,pdir
                      :program ,git-executable
                      :args ( "diff-tree" "-p" "--color"
                              ,src ,dst "HEAD" "origin")
                      :process-filter ,(function el-get-diff-filter)
                      :message ,(format "git diff for package %s ok." package)
                      :error ,(format "git diff for package %s failed." package)))
     #'el-get-git-diff-endfun)))

(defun el-get-git-diff-endfun (package)
  (with-current-buffer (get-buffer-create el-get-diff-buffer)
    (let ((diff (cdr (assoc package tmp-diff))))
      (if diff
          (insert
           (propertize (format "Diff for package %s:\n" package) 'face 'isearch)
           (apply #'concat diff))
        (insert
         (propertize (format "Package %s has no updates.\n" package) 'face 'highlight)))))
  (pop-to-buffer el-get-diff-buffer))

(defun el-get-diff-filter (proc str)
  (with-current-buffer (get-buffer-create el-get-diff-buffer)
;;    (message "profi %S" (process-get proc :el-get-package))
    (let* ((p   (process-get proc :el-get-package))
           (tmp (assoc p tmp-diff))
           (old (cdr tmp)))
    (setcdr tmp `(,@old ,(ansi-color-apply str))))))

(assoc 'foo '((foo . "kale")))

(el-get-register-method
 :git #'el-get-git-clone #'el-get-git-pull #'el-get-rmdir
 #'el-get-git-clone-hook nil nil #'el-get-git-diff)

(provide 'el-get-git)
