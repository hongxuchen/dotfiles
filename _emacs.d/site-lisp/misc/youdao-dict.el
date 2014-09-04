(require 'pos-tip)

(defconst search-url "http://dict.youdao.com/search?le=eng&keyfrom=dict.index&xmlDetail=true&doctype=xml&q=%s")
(defconst search-prompt "Youdao for (default %s): ")
(defconst youdao-return-xml "*youdao-xml*")

(defun read-word ()
  "Read word from active region or keyboard"
  (let (word)
    (setq word
          (if mark-active
              (buffer-substring
               (region-beginning) (region-end))
            (read-string
             (format search-prompt (current-word nil t))
             nil nil (current-word nil t))))
    (setq word (when (string-match "\\(^[ \t]*\\|[ \t]*$\\)" word)
                 (replace-match "" nil nil word)))
    (if (string-match "[,<.>/?;:\[{}`~!@#$%^&*()-=+]" word)
        (error "Wrong word!"))
    word))

(defun youdao-dict-parse-xml ()
  "Parse the xml crawled and return the useful content"
  (let (xml-tree dict-tree original-query return-phrase phonetic-symbol
                 translations trans-content word-forms form-content
                 eg-sentences sen-content result)
    (with-current-buffer youdao-return-xml
      (setq xml-tree (xml-parse-region (point-min) (point-max)))
      (insert (format "%s" xml-tree)))
    (setq dict-tree (car xml-tree))
    (setq original-query (car (last (car (xml-get-children
                                          dict-tree 'original-query)))))
    (setq return-phrase (car (last (car (xml-get-children
                                         dict-tree 'return-phrase)))))
    (when (equal return-phrase nil)
      (pos-tip-show "word not found" nil nil nil 1)
      (error "word not found"))

    (setq phonetic-symbol (car (last (car (xml-get-children
                                           dict-tree 'phonetic-symbol)))))
    ;; get translations
    (setq translations (xml-get-children
                        (car (xml-get-children
                              dict-tree 'custom-translation)) 'translation))
    (loop for trans in translations
          do (setq trans-content
                   (concat trans-content
                           (car (last (car (xml-get-children
                                            trans 'content)))) "\n")))
    ;; get word forms
    (setq wordforms (xml-get-children
                     (car (xml-get-children
                           dict-tree 'word-forms)) 'word-form))
    (setq form-content nil)
    (loop for wf in wordforms
          do (setq form-content
                   (concat form-content
                           (car (last (car (xml-get-children wf 'name))))
                           ": "
                           (car (last (car (xml-get-children wf 'value))))
                           "\n")))
    ;; get example eg-sentences
    (setq eg-sentences (xml-get-children
                        (car (xml-get-children
                              dict-tree 'example-sentences)) 'sentence-pair))
    (loop for sen in eg-sentences
          do (setq sen-content
                   (concat sen-content
                           (car (last (car
                                       (xml-get-children sen 'sentence))))
                           "\n"
                           (car (last (car
                                       (xml-get-children
                                        sen 'sentence-translation))))
                           "\n")))

    (setq sen-content (replace-regexp-in-string "\\(?:<\\(?:/?b>\\)\\)" "" sen-content))

    (setq result (concat "word: " return-phrase "\n"
                         "phonetic: [" phonetic-symbol "]\n"
                         trans-content
                         form-content
                         sen-content))))

(defun youdao-dict ()
  "Query a word from youdao online dictionaries and show it."
  (interactive)
  (with-current-buffer (get-buffer-create youdao-return-xml) (erase-buffer))
  (call-process "curl" nil (list youdao-return-xml nil) nil (format search-url (url-hexify-string (read-word))))
  (pos-tip-show (youdao-dict-parse-xml) nil nil nil 0)
  ;; (popup-tip (youdao-dict-parse-xml))
  (unwind-protect
      (push (read-event) unread-command-events)
    (pos-tip-hide))
  )

(provide 'youdao-dict)
