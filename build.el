(setq user-emacs-directory (expand-file-name "./.emacs.d"))

;; Bootstraping Straight.el so that we can use packages in git repos
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Setting Up Use-Package bootstrapping with straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package ox-publish :ensure nil :straight nil)
(use-package ox-slimhtml :ensure t)
(use-package esxml :ensure t)

(defvar jh:site-title "Jacob's Website" "Title for my website.") ;; very original, I know.
(defvar jh:site-url "https://jhilker.com/" "Website URL")


(defun jh:org-html-head (info)
  "returns the HTML `head' for my site."
  (sxml-to-xml
   `(head ()
          (meta (@ (name "viewport")
                   (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
         (title ,(concat (org-export-data (plist-get info :title) info) " | " jh:site-title))
         (link (@ (href "/css/style.css")
                  (rel "stylesheet")))
         (script (@ (src "https://unpkg.com/alpinejs@3.x.x/dist/cdn.min.js"))
                 "defer"))))

(defun jh:site-header (info)                        
  "Cretaes the header for my site."
  (concat 
   (sxml-to-xml
    `(header (@ (class "z-10 items-center text-gray-800 bg-gray-200 dark:bg-navy-700 dark:text-gray-200 grid-in-header"))
             (div (@ (class "flex items-center justify-between h-[52px] 2xl:h-[62px]"))
                  (nav (@ (class "items-center hidden h-full space-x-3 lg:flex"))
                       (a (@ (href "/")
                             (class "block h-full p-3 2xl:p-4 transition duration-100 hover:bg-gray-400 dark:hover:bg-navy-600 active:border-b-2 active:border-royal-600")) "Home")
                       (a (@ (href "/about.html")
                             (class "block h-full p-3 2xl:p-4 transition duration-100 hover:bg-gray-400 dark:hover:bg-navy-600 active:border-b-2 active:border-royal-600")) "About")))))))

(defun jh:org-html-template (contents info)
  "The base html template for my site."
  (concat
   (sxml-to-xml
     `(html (@ (x-data "{darkMode: false}")
               (x-init "darkMode = JSON.parse(localStorage.getItem('darkMode')); $watch('darkMode', value => JSON.stringify(localStorage.setItem('darkMode', value)))")
               (:class "{'dark': darkMode === true}"))))
   (jh:org-html-head info)
   (sxml-to-xml
    `(body
      (div (@ (class "grid h-screen grid-areas-mobile grid-rows-layout lg:grid-areas-desktop grid-cols-layout"))
           ,(jh:site-header info)
           ,(jh:fixed-sidebar)
           (main (@ (class "px-3 pt-3 overflow-y-scroll grid-in-main !max-w-none markdown-sm 2xl:markdown-lg markdown-royal scrollbar-thin dark:markdown-dark scroll-smooth motion-reduce:scroll-auto")) ,contents)
           )))))
                 


        
(defun jh:fixed-sidebar ()
"Creates Sidebar"
(concat 
 (sxml-to-xml 
  `(aside (@ (class "flex-col items-center hidden bg-blueGray-300 dark:bg-blueGray-700 dark:text-gray-100 grid-in-sidebar lg:flex"))
          (p (@ (class "p-2 font-semibold uppercase")) ,jh:site-title)
          (div (@ (class "mt-3 overflow-hidden rounded-full h-44 w-44 bg-royal-600")))))))

(org-export-define-derived-backend 'jh-html 'slimhtml
   :translate-alist '((template . jh:org-html-template)))


(defun jh-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to pd custom HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'jh-html filename ".html" plist pub-dir))

(setq org-publish-project-alist
      (list
        (list "my-org-site"
          :recursive t
          :base-directory "./org/" 
          :publishing-function 'jh-html-publish-to-html
          :publishing-directory "./public/"
          :with-creator t
          :with-author t
          :with-toc nil
          :section-numbers nil)
        (list "org-assets"
              :recursive t
              :base-directory "./org/"
              :publishing-function 'org-publish-attachment
              :publishing-directory "./public/"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|woff\\|woff2\\|svg")

        (list "site-assets"
              :recursive t
              :base-directory "./static/"
              :publishing-function 'org-publish-attachment
              :publishing-directory "./public/"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|woff\\|woff2\\|svg")))

(org-publish-all t)
