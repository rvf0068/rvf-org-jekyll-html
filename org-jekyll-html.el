;;; code for configuring the org publish project

(require 'ox-publish)
(require 's)

(defcustom org-jekyll-html-base-source-directory "~/Dropbox/Clases/"
       "Base directory for courses source files"
       :type '(string))

(defcustom org-jekyll-html-base-jekyll-directory "~/Dropbox/pages/"
       "Base directory for jekyll sites"
       :type '(string))

(defun org-jekyll-html-add-project (my-pair)
  (let ((code (car my-pair))
	(default-directory (nth 1 my-pair)))
  (setq org-publish-project-alist
	(nconc org-publish-project-alist
	       `((,code . (:components (,(concat code "-clases")
					,(concat code "-inicio")
					,(concat code "-pdf")
					,(concat code "-extra"))))
		 (,(concat code "-clases")
		  :base-directory ,(concat org-jekyll-html-base-source-directory
					   default-directory
					   "/clases")
		  :publishing-directory ,(concat org-jekyll-html-base-jekyll-directory
						 code
						 "/_posts")
		  :publishing-function org-jekyll-html-publish-to-jekyll
		  :body-only t
		  :base-extension "org"
		  :exclude "options.org" )
		 (,(concat code "-inicio")
		  :base-directory ,(concat org-jekyll-html-base-source-directory
					   default-directory
					   "/inicio")
		  :publishing-directory ,(concat org-jekyll-html-base-jekyll-directory
						 code)
		  :publishing-function org-jekyll-html-publish-to-jekyll
		  :body-only t
		  :base-extension "org")
		 (,(concat code "-pdf")
		  :base-directory ,(concat org-jekyll-html-base-source-directory
					   default-directory
					   "/clases")
		  :publishing-directory ,(concat org-jekyll-html-base-jekyll-directory
						 code
						 "/pdfs")
		  :publishing-function org-jekyll-html-publish-to-pdf
		  :base-extension "org"
		  :exclude "options.org")
		 (,(concat code "-extra")
		  :base-directory ,(concat org-jekyll-html-base-source-directory
					   default-directory
					   "/clases")
		  :publishing-directory ,(concat org-jekyll-html-base-jekyll-directory
						 code
						 "/images")
		  :publishing-function org-publish-attachment
		  :base-extension "png\\|jpg\\|jpeg" ))))))


;;; derived jekyll backend

(org-export-define-derived-backend 'jekyll-html 'html
  :translate-alist '((latex-environment . org-jekyll-html-latex-environment)
		     ;; (latex-fragment . org-jekyll-latex-html-fragment)
		     (src-block . org-jekyll-html-src-block)
		     (inner-template . org-jekyll-html-template))
  :options-alist '((:published "PUBLISHED" nil "true")
		   (:layout "LAYOUT" nil "post")))

(defun org-jekyll-html-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into jekyll format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((lang (org-element-property :language src-block))
	(value (org-element-property :value src-block))
	(name (or (org-element-property :name src-block) "")))
    (if (string= lang "sage")
        (format
         "<div class=\"sage\"><script type=\"text/x-sage\">\n%s</script></div>\n"
         value)
      (format
       "{%% highlight %s %%}\n%s{%% endhighlight %%}"
       lang value))))

(defun org-jekyll-html-latex-environment (latex-environment contents info)
  (let* ((value (org-remove-indentation
		 (org-element-property :value latex-environment)))
	 (replaced (replace-regexp-in-string
		    "\\\\begin{displaymath}\\|\\\\end{displaymath}" "$$" value)))
    replaced))

(defun org-jekyll-html-latex-fragment (latex-fragment contents info)
  (let* ((latex (org-element-property :value latex-fragment))
	 (inline-latex (replace-regexp-in-string "\\\\(\\|\\\\)" "$$" latex))
	 (bars-removed (replace-regexp-in-string "|" "\\\\vert " inline-latex)))
    ;; (message "Latex-fragment is %s" latex)
    bars-removed))

;; We have to transform
;; <img src="recta.png" alt="recta" width="400" align="center" />
;; to
;; <img src="{{ site.baseurl}}/images/recta.png" alt="recta" width="400" align="center" />
;; TODO: Deal with character "

(defun org-jekyll-html-template (contents info)
  (let ((title (or (car (plist-get info :title)) ""))
	 (date (or (car (plist-get info :date)) ""))
	 (time "")
	 (keywords (or (plist-get info :keywords) ""))
	 (published (plist-get info :published))
	 (layout (plist-get info :layout))
	 (frontmatter
	  "---\nlayout: %s\ntitle: %s\ndate: %s %s\ncomments: true\npublished: %s\ncategories: %s\n---\n\n"))
    (setq contents
	  (s-with contents
	    (replace-regexp-in-string
	     "<img src=\\(.\\)\\(.*\\)\.\\(png\\|jpg\\|jpeg\\)"
	     "<img src=\\1{{ site.baseurl }}/images/\\2.\\3")
	    (replace-regexp-in-string ":B<sub>corollary</sub>:" "Corolario")
	    (replace-regexp-in-string ":B<sub>theorem</sub>:" "Teorema")
	    (replace-regexp-in-string ":B<sub>lemma</sub>:" "Lema")
	    (replace-regexp-in-string ":BMCOL:" "")
	    (replace-regexp-in-string ":B<sub>block</sub>" "")
	    (replace-regexp-in-string ":B<sub>column</sub><br />" "")
	    (replace-regexp-in-string ":B<sub>ignoreheading</sub>:<br />" "")))
    (concat (format frontmatter layout title date time published keywords) contents)))

(defun org-jekyll-html-export-as-jekyll
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'jekyll-html "*Org JEKYLL-HTML Export*"
    async subtreep visible-only body-only ext-plist (lambda () (html-mode))))

(defun org-jekyll-html-publish-to-jekyll (plist filename pub-dir)
  (org-publish-org-to 'jekyll-html filename ".html" plist pub-dir))

;;; derived beamer backend

(require 'ox-beamer)

(org-export-define-derived-backend 'beamer-math-class 'beamer
  :translate-alist '((inner-template . org-jekyll-html-math-class-template)))

(defun org-jekyll-html-math-class-template (contents info)
  (s-with contents
    (replace-regexp-in-string "\\[:B_corollary:\\]:" "Corolario")
    (replace-regexp-in-string "\\[:B_definition:\\]:" "Definición")
    (replace-regexp-in-string "\\[:B_example:\\]:" "Ejemplo")
    (replace-regexp-in-string "\\[:B_lemma:\\]" "Lema")
    (replace-regexp-in-string "\\[:B_proposition:\\]" "Proposición")
    (replace-regexp-in-string "\\[:B_theorem:\\]" "Teorema")
    (replace-regexp-in-string ":BMCOL:" "")
    (replace-regexp-in-string ":B\\\\(_{\\\\text{block}}\\\\)" "")
    (replace-regexp-in-string ":B<sub>column</sub><br />" "")
    (replace-regexp-in-string ":B<sub>ignoreheading</sub>:<br />" "")))

(defun org-jekyll-html-export-as-beamer
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'beamer-math-class "*Org BEAMER-MATH-CLASS Export*"
    async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

;;; modified from ox-beamer.el

(defun org-jekyll-html-publish-to-latex (plist filename pub-dir)
  "Publish an Org file to a Beamer presentation (LaTeX)."
  (org-publish-org-to 'beamer-math-class filename ".tex" plist pub-dir))

(defun org-jekyll-html-publish-to-pdf (plist filename pub-dir)
  "Publish an Org file to a Beamer presentation (PDF, via LaTeX)."
  (org-publish-attachment
   plist
   (let ((default-directory (file-name-directory filename)))
     (org-latex-compile
      (org-publish-org-to
       'beamer-math-class filename ".tex" plist (file-name-directory filename))))
   pub-dir))

(provide 'org-jekyll-html)
