((el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :info "." :compile
		("el-get.*\\.el$" "methods/")
		:features el-get :post-init
		(when
		    (memq 'el-get
			  (bound-and-true-p package-activated-list))
		  (message "Deleting melpa bootstrap el-get")
		  (unless package--initialized
		    (package-initialize t))
		  (when
		      (package-installed-p 'el-get)
		    (let
			((feats
			  (delete-dups
			   (el-get-package-features
			    (el-get-elpa-package-directory 'el-get)))))
		      (el-get-elpa-delete-package 'el-get)
		      (dolist
			  (feat feats)
			(unload-feature feat t))))
		  (require 'el-get))))
 (spotify\.el status "installed" recipe
	      (:name spotify\.el :type github :pkgname "danielfm/spotify.el" :description "Control the Spotify app from within Emacs" :url "https://github.com/danielfm/spotify.el" :after
		     (progn
		       (setq spotify-oauth2-client-secret "9d7b65831e3d40148b055ea9e3472d07")
		       (setq spotify-oauth2-client-id "4aeb25c7700947a0b579d3fa5fd34f76")))))
