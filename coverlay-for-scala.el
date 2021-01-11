(defun coverlays/find-cobertura-files ()
  "Find cobertura.xml files."
  (--> (projectile-project-root)
       (format "find %s -type f -name \"cobertura.xml\"" it)
       (shell-command-to-string it)
       (s-split "\n" it)
       (-map 's-trim it)
       (--remove (or (null it) (s-blank-p it)) it)))

(defun coverlays/parse-path-lines-from-cobertura (filepath)
  "Parse FILEPATH to a "
  (when (f-file-p filepath)
    (with-temp-buffer
      (insert-file-contents-literally filepath)
      (let* ((xml (libxml-parse-html-region (point-min) (point-max)))
             (path (nth 2 (nth 3 (esxml-query "sources" xml)))) ;; TODO maybe not universal
             (classes (-drop 2 (esxml-query "classes" xml)))
             (make-path-lines-from-class
              (lambda (class)
                `((filepath . ,(s-concat path "/" (alist-get 'filename (nth 1 class))))
                  (lines . ,(--map (--> it
                                        (nth 1 it)
                                        (--filter
                                         (or (eq (car it) 'number)
                                             (eq (car it) 'hits))
                                         it))
                                   (-drop 2 (nth 3 class))))))))

        (-map make-path-lines-from-class classes)))))


(defun coverlays/produce-lcov (filenames-lines)
  "Produce lcov file from FILENAMES-LINES."
  (--> (--reduce-from
        (let ((heading (s-concat "SF:" (alist-get 'filepath it) "\n"))
              (line-numbers
               (--map
                (s-concat "DA:" (alist-get 'number it) "," (alist-get 'hits it) "\n")
                (alist-get 'lines it))))
          (s-concat acc heading (apply 's-concat line-numbers) "end_of_record\n"))
        ""
        filenames-lines)
       ))

(defun coverlays/produce-lcov-from-cobertura ()
  "Produce lcov file from cobertura"
  (interactive)
  (let ((file (s-concat (projectile-project-root) "/lcov.lcov")))
    (delete-file file)
    (--> (coverlays/find-cobertura-files)
         (--each
             it
           (--> (coverlays/parse-path-lines-from-cobertura it)
                (coverlays/produce-lcov it)
                (write-region it nil file t))))))

(defun coverlays/rebuild-cobertura-on-project ()
  "Run sbt tests and produce lcov file."
  (interactive)
  (let ((proj-type (projectile-project-type))
        (directory (projectile-project-root)))
    (when (or (eq 'bloop proj-type) (eq 'sbt proj-type) (eq 'scala proj-type))
      (message "Producing cobertura output...")
      (async-start
       `(lambda () (let ((default-directory ,directory)) (message "starting %s" ,directory) (call-process "sbt" nil nil nil ";clean;coverage;test;coverageReport;") (message "finishing")))
       `(lambda (result)
          (cd ,directory)
          (coverlays/produce-lcov-from-cobertura)
          (message (format "Coverlay mode ready for %s" ,directory)))))))

(add-hook
 'scala-mode-hook
 (lambda ()
   (let ((file (concat (projectile-project-root) "/lcov.lcov")))
     (when (or (not (f-file-p file)
                    (> (time-to-seconds (time-since
	                                 (file-attribute-modification-time
	                                  (file-attributes (file-truename file)))))
                       (* 60 60 2))))  ;; maybe just lcov older than 2 hours?
       (coverlays/rebuild-cobertura-on-project)))))

(add-hook
 'scala-mode-hook
 (lambda ()
   (when (f-file-p (concat (projectile-project-root) "/lcov.lcov"))
     (turn-on-coverlay-mode))))

(add-hook
 'coverlay-mode-hook
 (lambda ()
   (let ((proj-type (projectile-project-type))
         (file (concat (projectile-project-root) "/lcov.lcov")))
     (when
         (and
          (or
           (eq 'bloop proj-type)
           (eq 'sbt proj-type)
           (eq 'scala proj-type))
          (projectile-project-root)
          (f-file-p file))
       (coverlay-load-file file)))))
