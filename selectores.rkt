#lang racket

(require "constructores.rkt")
(provide get-user get-current-user get-system-name get-system-usuarios get-system-drive get-system-current-user get-system-current-drive get-system-path get-system-folder get-system-fecha-creacion get-letters-all-drives get-system-folder-drive get-rest-drives get-drive get-name-drive get-capacity-drive get-path get-type-file)

;; Selectores del TDA System
;; Obtener de la lista de usuarios el usuario actual
(define get-user (lambda
                     (lista_usuarios user)
                   (if (eq? (car lista_usuarios) user)
                       (car lista_usuarios)
                       (get-user (cdr lista_usuarios) user)
                       )
                   ))
(define get-current-user (lambda (system user) (get-user cadr user)))

;; Se obtiene el nombre del sistema, usuarios, drives, current-user, current-drive, fecha-creación
(define get-system-name (lambda (sistema) (list-ref sistema 0)))
(define get-system-usuarios (lambda (sistema) (list-ref sistema 1)))
(define get-system-drive (lambda (sistema) (list-ref sistema 2)))
(define get-system-current-user (lambda (sistema) (list-ref sistema 3)))
(define get-system-current-drive (lambda (sistema) (list-ref sistema 4)))
(define get-system-path (lambda (sistema) (list-ref sistema 5)))
(define get-system-folder (lambda (sistema) (list-ref sistema 6)))
(define get-system-fecha-creacion (lambda (sistema) (list-ref sistema 7)))

;; Obtener todas las letras existentes de los drives
(define get-letters-all-drives(
                               lambda (sistema)
                                (define get-letters-all-drives-int (
                                                                    lambda (sistema letras)
                                                                     (if (null? (get-system-drive sistema)) letras
                                                                         (get-letters-all-drives-int
                                                                          (make-system (get-system-name sistema)
                                                                                       (get-system-usuarios sistema)
                                                                                       (cdr (get-system-drive sistema))
                                                                                       (get-system-current-user sistema)
                                                                                       (get-system-current-drive sistema)
                                                                                       (get-system-path sistema)
                                                                                       (get-system-folder sistema)
                                                                                       (get-system-fecha-creacion sistema))
                                                                          (cons (car (car (get-system-drive sistema))) letras)
                                                                          )
                                                                         )
                                                                     ))
                                (get-letters-all-drives-int sistema null)
                                ))

;; Obtiene las carpetas guardadas en el drive con la letra letter (usado en switch drive)
(define get-system-folder-drive (lambda (drives letter)
                                  (if (eq? letter (car (car drives)))
                                      (list-ref (car drives) 2)
                                      (get-system-folder-drive (cdr drives) letter)
                                      )
                                  ))

;; Obtiene la lista de drives sin contar al drive que contiene la letra letter (usado en mkdir)
(define get-rest-drives (lambda (drives rest-drives letter)
                          (if (null? drives)
                              rest-drives
                              (if (eq? letter (car (car drives)))
                                  (get-rest-drives (cdr drives) rest-drives letter)
                                  (get-rest-drives (cdr drives) (car drives) letter)
                                  )
                              )
                          ))

;; Obtiene un drive en especifico en una lista de drives por medio de letter 
(define get-drive (lambda (drives letter)
                    (if (eq? (car (car drives)) letter)
                        (car drives)
                        (get-drive (cdr drives) letter)
                        )
                    ))

;; Obtiene el nombre del drive
(define get-name-drive (lambda (drive)
                         (list-ref drive 1)
                         ))

;; Obtiene la capacidad del drive
(define get-capacity-drive (lambda (drive)
                             (list-ref drive 3)))

;; Obtener path de una carpeta
(define get-path (lambda (folder)
                          (last folder)
                          ))

;; Obtiene que tipo de archivo es
(define get-type-file (lambda (file-list)
                        (cond
                          ((null? (cddr file-list)) (car file-list))
                          (else (get-type-file (cdr file-list)))
                          )
                        ))
