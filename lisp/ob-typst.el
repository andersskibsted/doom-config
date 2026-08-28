;;; lisp/ob-typst.el -*- lexical-binding: t; -*-

;;; ob-typst.el -*- lexical-binding: t; -*-

;;; Babel-eksekvering
;;; Code:
(defun org-babel-execute:typst (body params)
  "Eksekvér en Typst-kodeblok og returnér sti til det renderede billede."
  (let* ((out-file (or (cdr (assq :file params))
                        (org-babel-temp-file "typst-" ".svg")))
         (in-file (org-babel-temp-file "typst-" ".typ")))
    (with-temp-file in-file
      (insert "#set page(width: auto, height: auto, margin: 4pt, fill: none)\n")
      (insert "#set text(fill: white, size: 14pt)\n")
      (insert body))
    (call-process "typst" nil nil nil "compile" in-file out-file)
    out-file))
(provide 'ob-typst)

;; (defun org-babel-execute:typst (body params)
;;   "Eksekvér en Typst-kodeblok og returnér sti til det renderede billede."
;;   (let* ((out-file (or (cdr (assq :file params))
;;                         (org-babel-temp-file "typst-" ".svg")))
;;          (in-file (org-babel-temp-file "typst-" ".typ"))
;;          (err-buffer (generate-new-buffer " *typst-error*"))
;;          (exit-code nil))
;;     (with-temp-file in-file
;;       (insert "#set page(width: auto, height: auto, margin: 4pt, fill: none)\n")
;;       (insert "#set text(fill: white, size: 14pt)\n")
;;       (insert body))
;;     (setq exit-code
;;           (call-process "typst" nil err-buffer nil "compile" in-file out-file))
;;     (if (zerop exit-code)
;;         out-file
;;       (let ((err-msg (with-current-buffer err-buffer (buffer-string))))
;;         (kill-buffer err-buffer)
;;         (user-error "Typst compile fejlede: %s" err-msg)))))

;; (defun org-babel-execute:typst (body params)
;;   "Eksekvér en Typst-kodeblok og returnér sti til det renderede billede."
;;   (let* ((out-file (or (cdr (assq :file params))
;;                         (org-babel-temp-file "typst-" ".svg")))
;;          (in-file (org-babel-temp-file "typst-" ".typ"))
;;          (err-buffer (generate-new-buffer " *typst-error*"))
;;          (exit-code nil))
;;     (with-temp-file in-file
;;       (insert "#set page(width: auto, height: auto, margin: 4pt, fill: none)\n")
;;       (insert "#set text(fill: white, size: 14pt)\n")
;;       (insert body))
;;     (setq exit-code
;;           (let ((default-directory (file-name-directory (or (buffer-file-name) default-directory))))
;;             (call-process "typst" nil err-buffer nil "compile" in-file out-file)))
;;     (if (zerop exit-code)
;;         out-file
;;       (let ((err-msg (with-current-buffer err-buffer (buffer-string))))
;;         (kill-buffer err-buffer)
;;         (user-error "Typst compile fejlede: %s" err-msg)))))

;; ;;; Overlay-preview (det nye)
;; (defun ob-typst--overlay-block (image-file)
;;   "Læg en overlay hen over den nuværende src-blok, der viser IMAGE-FILE."
;;   (let* ((element (org-element-at-point))
;;          (beg (org-element-property :begin element))
;;          (end (org-element-property :end element))
;;          (ov (make-overlay beg end)))
;;     (overlay-put ov 'display (create-image image-file))
;;     (overlay-put ov 'ob-typst-overlay t)
;;     (overlay-put ov 'evaporate t)))

;; (defun ob-typst--remove-overlays-at-point ()
;;   "Fjern eksisterende ob-typst-overlays ved punkt, før en ny lægges."
;;   (dolist (ov (overlays-at (point)))
;;     (when (overlay-get ov 'ob-typst-overlay)
;;       (delete-overlay ov))))

;; (defun ob-typst-preview-current-block ()
;;   "Kør typst-blokken ved punkt og vis resultatet som overlay over selve blokken."
;;   (interactive)
;;   (ob-typst--remove-overlays-at-point)
;;   (let* ((info (org-babel-get-src-block-info))
;;          (lang (nth 0 info)))
;;     (when (string= lang "typst")
;;       (let ((image-file (org-babel-execute-src-block)))
;;         (when (and image-file (file-exists-p image-file))
;;           (ob-typst--overlay-block image-file))))))

;; (defun ob-typst--maybe-remove-overlay-at-point ()
;;   "Fjern ob-typst-overlay, hvis cursor er flyttet ind i den."
;;   (dolist (ov (overlays-at (point)))
;;     (when (overlay-get ov 'ob-typst-overlay)
;;       (delete-overlay ov))))

;; (add-hook 'post-command-hook #'ob-typst--maybe-remove-overlay-at-point)

;; (provide 'ob-typst)




;;; ob-typst.el ends here
