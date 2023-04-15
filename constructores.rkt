#lang racket

;; TDA System = name (String) x users (String list) x drives (drive list) x current-user (String) x current-drive (Char) x
;; current-status (String)
;; TDA Drive = letter (String list) x name (String) x folders (folder list) x capacity (String)
;; TDA Folder = name (String) x files (file list) x  folder_hijo_izquierdo (list folder) x folder_hermano_derecho (list folder)
;; x patch_actual (String)