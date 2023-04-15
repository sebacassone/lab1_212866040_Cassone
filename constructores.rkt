#lang racket

;; TDA User = name (String) x permisos (list String)
;; TDA System = name (String) x users (User list) x drives (drive list) x current-user (String) x current-drive (Char) x
;; current-status (String) x fecha_creacion (String)
;; TDA Drive = letter (String list) x name (String) x folders (folder list) x capacity (String)
;; TDA Folder = name (String) x files (file list) x  folder_hijo_izquierdo (list folder) x folder_hermano_derecho (list folder)
;; x patch_actual (String)
;; TDA File = name (String) x Ruta (String) x Contenido (String)

;; Permisos estructura = ( Leer: 1 o 0, Escribir: 1 o 0, root: 1 o 0)
;; Por simplicidad se omitirá el permiso de ejecución del archivo dado que no se pide en el enunciado.
;; El permiso de usuario root permite modificar datos del sistema como por ejemplo crear nuevos
;; usuarios, eliminar usuarios, crear nuevos drives, etc.

;; Constructor de TDA System
;; Se inicializa el sistema
(define system (lambda (name) (make-system name null null "" "" "" (current-seconds))))
;; Se extrae como una función aparte el constructor del sistema ya que será utilizado por otras funciones más.
(define make-system (lambda
                        (name users drives current-user current-drive current-status seconds)
                      (list name users drives current-user current-drive current-status seconds)
                      ))

;; Se construye una unidad para la lista de drives
(define make-drive (lambda
                       (letter name capacity) (list letter name null capacity)))

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

;; Se obtiene el nombre del sistema, usuarios, drives, current-user, current-drive, current-status, fecha-creación
(define get-system-name (lambda (sistema) (list-ref sistema 0)))
(define get-system-usuarios (lambda (sistema) (list-ref sistema 1)))
(define get-system-drive (lambda (sistema) (list-ref sistema 2)))
(define get-system-current-user (lambda (sistema) (list-ref sistema 3)))
(define get-system-current-drive (lambda (sistema) (list-ref sistema 4)))
(define get-system-current-status (lambda (sistema) (list-ref sistema 5)))
(define get-system-fecha-creacion (lambda (sistema) (list-ref sistema 6)))

;; Otras funciones de los TDA
;; Ejecutar una función sobre el sistema (creación de archivos, carpetas, renombrar, copiar, mover, eliminar,
;; verificar permisos y dejar registro de la fecha de modificación

;; Distinguir casos de uso
;; Si es un comando que opera sobre archivo se verificará que el usuario tenga permisos sobre el archivo
;; o carpeta, para esto obtendremos el archivo en cuestión y verificaremos si el usuario tiene permisos.
;; "grep" "add-file" "del" "move" "ren" "encrypt" "decrypt" "restore"
(define tipo-de-comando (lambda (funcion)
                          (if (memq funcion (list copy))
                              1
                              0)
                          ))
;; Se verifican permisos
(define (verificar-permisos patito) 1)
(define copy (lambda (sistema) (list sistema)))

;; Diferencia de inmediato los tipos de comandos para poder verificar los permisos necesarios
(define run
  (lambda
      (sistema funcion)
    ;; Tipo de comando que modifica un archivo
    (cond ((eq? (tipo-de-comando funcion) 1)
           ;; Verifica si el usuario tiene permisos sobre el archivo
           (if (eq? (verificar-permisos 1) 1)
               ;; Caso de que tenga permisos
               (cons (funcion sistema) (cons (current-seconds) "Entró") )
               "No tienes permisos sobre este archivo")
           )
           ;; En el caso de que el tipo de comando sea sobre el sistema se da libertad al usuario
          (else (cons (funcion sistema) (cons (current-seconds) "No entró")))
          )
    ))

;; Se agrega una unidad fisica o lógica a la lista de unidades (drives)
(define add-drive
  (lambda (system)
    (lambda (letter name capacity) ;; Información de la unidad que se creará
      (make-system (get-system-name system)
                   (get-system-usuarios system)
                   (cons (make-drive letter name capacity)
                         (get-system-drive system))
                   (get-system-current-user system)
                   (get-system-current-drive system)
                   (get-system-current-status system)
                   (get-system-fecha-creacion system))
      )
    ))

;; Script
;; Se crea el sistema
(define S0 (system "newSystem"))
S0
;; Se añaden unidades a S0
(define S1 ((run S0 add-drive) #\C "S0" 1000))
S1