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

(defvar el-get-diff-buffer "*el-get diff*")

(defun el-get-git-diff (package)
  "git diff what we have and what the update would be"
  (lexical-let* ((git-executable (el-get-executable-find "git"))
                 (pdir (el-get-package-directory package))
                 (name (format "*git diff %s*" package)))
    (el-get-start-process-list
     package
     `((:command-name "*git fetch*"
                      :buffer-name ,name
                      :default-directory ,pdir
                      :program ,git-executable
                      :args ( "--no-pager" "fetch" "-v")
                      :message ,(format "git fetch for package %s ok." package)
                      :error ,(format "git fetch package %s failed." package)
                      :process-filter ,(function el-get-diff-filter))
       (:command-name ,name
                      :buffer-name ,name
                      :default-directory ,pdir
                      :program ,git-executable
                      ;;:args ( "status" )
                      :args ( "diff-tree" "-p" "--color" "origin")
                      ;;:args ( "ls-tree" "master" )
                      :message ,(format "git diff for package %s ok." package)
                      :error ,(format "git diff for package %s failed." package)
                      :process-filter ,(function el-get-diff-filter)))       
     (lambda (&rest ignored) (pop-to-buffer el-get-diff-buffer)
       ))))

(defun el-get-diff-filter (proc str)
  (with-current-buffer
      (get-buffer-create el-get-diff-buffer)
    (insert str)
    (ansi-color-apply-on-region (point-min) (point-max))))

(el-get-register-method
 :git #'el-get-git-clone #'el-get-git-pull #'el-get-rmdir
 #'el-get-git-clone-hook nil nil #'el-get-git-diff)

(provide 'el-get-git)
