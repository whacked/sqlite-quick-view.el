;;; sqlite-quick-view.el --- simple opener for sqlite databases in emacs using tabulated list mode

;; Copyright (C) 2020- whacked

;; Author: whacked <whacked@users.noreply.github.com>
;; Version: 0.0.1
;; Package-Requires: (esqlite s)

;; Keywords: sqlite
;; URL: https://github.com/whacked/sqlite-quick-view.el

;;; Commentary:

;; open a sqlite (read-only) in emacs's native table viewer

;;; Code:

(require 'esqlite)
(require 's)

(defun sqlite--get-read-only-dsn (db-path)
  (format "file:///%s?mode=ro"
          (replace-regexp-in-string "^/" "" db-path)))

(defun sqlite--get-table-columns-for-database (db-path)
  ;; returns (table-name (col1 col2 col3 ...))
  (let* ((first-record
          (first
           (esqlite-read
            (sqlite--get-read-only-dsn db-path)
            "SELECT name, sql FROM sqlite_schema LIMIT 1")))
         (table-name (first first-record))
         (table-def-sql (cadr first-record)))
    (cons
     table-name
     (list
      (mapcar
       (lambda (column-def-expr)
         (intern (car (s-split " " column-def-expr))))
       (s-split
        ", "
        (s-chop-suffix
         ")"
         (cadr (s-split-up-to "(" table-def-sql 1)))))))))

(defun sqlite-quick-view (db-path)
  (interactive
   (list (read-file-name "path to db: ")))
  (let* ((db-name (file-name-nondirectory db-path))
         (table-name-columns (sqlite--get-table-columns-for-database db-path))
         (table-name (first table-name-columns))
         (columns (cadr table-name-columns))
         (records (mapcar (lambda (record)
                            (-interleave columns record))
                          (esqlite-read
                           (sqlite--get-read-only-dsn db-path)
                           (format "SELECT * FROM %s" table-name))))
         (header-max-lengths
          (seq-reduce
           (lambda (lengths record)
             (seq-map-indexed (lambda (cur-length idx)
                                (let* ((col-key (nth idx columns))
                                       (val (format "%s" (plist-get record col-key)))
                                       (val-length (length val)))
                                  (if (< cur-length val-length)
                                      val-length
                                    cur-length)))
                              lengths))
         
           records
           (mapcar (lambda (_record) 0) columns)))
         (header-formatters
          (vconcat
           (seq-map-indexed
            (lambda (col-key idx)
              (list (symbol-name col-key)
                    (+ 2 (nth idx header-max-lengths))
                    t))
            columns))))
  
    (define-derived-mode sqlite-quick-view-mode
      tabulated-list-mode
      "sqlite-quick-view"
      "Quick viewer for sqlite"
      
      ;; need to get the headers first
      (setq tabulated-list-format header-formatters)
      (setq tabulated-list-padding 1)
      ;; not all tables have "id"
      ;; (setq tabulated-list-sort-key (cons "id" t))
      (tabulated-list-init-header))

    (let ((quick-view-buffer-name (format "*sqlite-quick-view-%s*" db-name)))
      (pop-to-buffer quick-view-buffer-name nil)
      (sqlite-quick-view-mode)
      (define-key sqlite-quick-view-mode-map (kbd "j") 'next-line)
      (define-key sqlite-quick-view-mode-map (kbd "k") 'previous-line)
    
      (setq tabulated-list-entries
            (seq-map-indexed
             (lambda (record index)
               (list (+ 1 index)
                     (vconcat
                      (mapcar (lambda (header-spec)
                                (let* ((key (intern (car header-spec)))
                                       (value (plist-get record key)))
                                  (cond ((eq value :null)
                                         "")
                                        (t value))))
                              header-formatters))))
             records))
      (tabulated-list-print t))))
