#lang racket

(provide system make-system make-drive make-security-atributes make-folder file make-file)

;; TDA System = name (String) x users (list String) x drives (drive list) x current-user (String) x current-drive (Char) x
;; path_actual (String) x Folders (Folder)  x fecha_creacion (String) x fecha_modificación (String)
;; TDA Drive = letter (String list) x name (String) x folders (folder list) x capacity (String)
;; TDA Folder = name (String) x fecha-creación (String) x fecha-modificación (String) x user-creator (String)
;;              x type (String) x secturity-atributes (Security) x path (String)
;; TDA File = name (String) x extension (String) x type (String) x secturity-atributes (Security) x Contenido (String) x path (String)
;; TDA Security = lectura (String) x escritura (String) x oculto (String) x acceso (all / userName) (String)
;; Por simplicidad se omitirá el permiso de ejecución del archivo dado que no se pide en el enunciado.

;; Constructor de TDA System
;; Se inicializa el sistema
(define system (lambda (name) (make-system name '() '() "" "" "" '() (current-seconds))))
;; Se extrae como una función aparte el constructor del sistema ya que será utilizado por otras funciones más.
(define make-system (lambda
                        (name users drives current-user current-drive path folders seconds)
                      (list name users drives current-user current-drive path folders seconds (current-seconds))
                      ))

;; Se construye una unidad para la lista de drives
(define make-drive (lambda
                       (letter name folders capacity) (list letter name folders capacity)))

;; Se construye TDA Security
(define make-security-atributes (lambda
                                    (escritura lectura oculto acceso)
                                  (list escritura lectura oculto acceso)))

;; Se construye una carpeta
(define make-folder (lambda
                        (name user-creator path)
                      (list name (current-seconds) (current-seconds) user-creator (make-security-atributes #\r #\w "" "all") "folder" path)
                      ))

;; Se constuye un archivo
;; TDA File = name (String) x extension (String) x Contenido (String) x secturity-atributes (Security) x type (String) x path (String)
(define file (lambda
                 (name extension content . security-atributes)
               (list security-atributes content extension name)))

;; Se le da forma del TDA
(define make-file (lambda
                      (archivo type path)
                    (reverse (cons path (cons type archivo)))
                    ))
