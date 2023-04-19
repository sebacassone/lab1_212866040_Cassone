#lang racket

;; TDA System = name (String) x users (list String) x drives (drive list) x current-user (String) x current-drive (Char) x
;; patch_actual (String) x Folders (Folder)  x fecha_creacion (String) x fecha_modificación (String)
;; TDA Drive = letter (String list) x name (String) x folders (folder list) x capacity (String)
;; TDA Folder = name (String) x files (file list) x  folder_hijo_izquierdo (list folder) x folder_hermano_derecho (list folder)
;; x patch (String)
;; TDA File = name (String) x Ruta (String) x Contenido (String)

;; Permisos estructura = ( Leer: 1 o 0, Escribir: 1 o 0, root: 1 o 0)
;; Por simplicidad se omitirá el permiso de ejecución del archivo dado que no se pide en el enunciado.
;; El permiso de usuario root permite modificar datos del sistema como por ejemplo crear nuevos
;; usuarios, eliminar usuarios, crear nuevos drives, etc.

;; Constructor de TDA System
;; Se inicializa el sistema
(define system (lambda (name) (make-system name '() '() "" "" "" '() (current-seconds))))
;; Se extrae como una función aparte el constructor del sistema ya que será utilizado por otras funciones más.
(define make-system (lambda
                        (name users drives current-user current-drive patch folders seconds)
                      (list name users drives current-user current-drive patch folders seconds (current-seconds))
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

;; Se obtiene el nombre del sistema, usuarios, drives, current-user, current-drive, fecha-creación
(define get-system-name (lambda (sistema) (list-ref sistema 0)))
(define get-system-usuarios (lambda (sistema) (list-ref sistema 1)))
(define get-system-drive (lambda (sistema) (list-ref sistema 2)))
(define get-system-current-user (lambda (sistema) (list-ref sistema 3)))
(define get-system-current-drive (lambda (sistema) (list-ref sistema 4)))
(define get-system-patch (lambda (sistema) (list-ref sistema 5)))
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
                                                                                       (get-system-patch sistema)
                                                                                       (get-system-folder sistema)
                                                                                       (get-system-fecha-creacion sistema))
                                                                          (cons (car (car (get-system-drive sistema))) (cons letras null)))
                                                                         )
                                                                     ))
                                (get-letters-all-drives-int sistema '())
                                ))
;; Obtiene las carpetas guardadas en el drive
(define get-system-folder-drive (lambda (drives letter)
                                  (if (eq? letter (car (car drives)))
                                      (list-ref (car drives) 2)
                                      (get-system-folder-drive (cdr drives) letter)
                                      )
                                  ))

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
               (funcion sistema)
               "No tienes permisos sobre este archivo")
           )
          ;; En el caso de que el tipo de comando sea sobre el sistema se da libertad al usuario
          (else (funcion sistema))
          )
    ))

;; Se agrega una unidad fisica o lógica a la lista de unidades (drives)
(define add-drive
  (lambda (system)
    (lambda (letter name capacity) ;; Información de la unidad que se creará
      (if (memq letter (get-letters-all-drives system))
          system
          (make-system (get-system-name system)
                       (get-system-usuarios system)
                       (cons (make-drive letter name capacity)
                             (get-system-drive system))
                       (get-system-current-user system)
                       (get-system-current-drive system)
                       (get-system-patch system)
                       (get-system-folder system)
                       (get-system-fecha-creacion system))
          )
      )))

;; Se crea un nuevo usuario 
(define register (lambda (system)
                   (lambda (userName)
                     (if (memq userName (get-system-usuarios system))
                         system
                         (make-system (get-system-name system)
                                      (cons userName (get-system-usuarios system))
                                      (get-system-drive system)
                                      (get-system-current-user system)
                                      (get-system-current-drive system)
                                      (get-system-patch system)
                                      (get-system-folder system)
                                      (get-system-fecha-creacion system))
                         )
                     )))

;; Se inicia sesión en un usuario solo si este existe.
(define login (lambda (system)
  (lambda (userName)
   (if (and (memq userName (get-system-usuarios system)) (eq? "" (get-system-current-user system)))
                         (make-system (get-system-name system)
                                      (get-system-usuarios system)
                                      (get-system-drive system)
                                      userName
                                      (get-system-current-drive system)
                                      (get-system-patch system)
                                      (get-system-folder system)
                                      (get-system-fecha-creacion system))
                          system
                         )
    )))

;; Se cierra sesión de un usuario solo si este existe
(define logout (lambda (system)
   (if (not (eq? "" (get-system-current-user system)))
                         (make-system (get-system-name system)
                                      (get-system-usuarios system)
                                      (get-system-drive system)
                                      ""
                                      (get-system-current-drive system)
                                      (get-system-patch system)
                                      (get-system-folder system)
                                      (get-system-fecha-creacion system))
                          system
                         )
    ))

;; Cambia de drive solo si hay un usuario logeado
;; Debe tener dos cosas principalmente, en system debe marcar que se seleccionó la letra que se quiere
;; y además debe replicar los archivos que están en el disco dejandolos en system también.
(define switch-drive (lambda (system)
                       (lambda (letter)
                         (if (and
                              (not (eq? "" (get-system-current-user system))) ;; Si esta logeado
                              (memq letter (get-letters-all-drives system)) ;; Si la letra existe
                              )
                             (make-system (get-system-name system)
                                      (get-system-usuarios system)
                                      (get-system-drive system)
                                      (get-system-current-user system)
                                      letter
                                      "/"
                                      (get-system-folder-drive (get-system-drive system) letter) ;; Obtiene los archivos del drive especificado
                                      (get-system-fecha-creacion system))
                          system
                         )
                        )))
                             
                         

;; Script
;; Se crea el sistema
(define S0 (system "newSystem"))
S0
;; Se añaden unidades a S0
;; Añadiendo unidades. Incluye caso S2 que intenta añadir unidad con una letra que ya existe
(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))
S3

;; Añadiendo usuarios. Incluye caso S6 que intenta registrar usuario duplicado
(define S4 ((run S3 register) "user1"))
(define S5 ((run S4 register) "user1"))
(define S6 ((run S5 register) "user2"))
S6

;; iniciando sesión con usuarios. Incluye caso S8 que intenta iniciar sesión con user2 sin antes haber salido con user1
(define S7 ((run S6 login) "user1"))
S7
(define S8 ((run S7 login) "user2"))
S8

;; cerrando sesión user1 e iniciando con user2
(define S9 (run S8 logout))
S9
(define S10 ((run S9 login) "user2"))
S10

;; Cambios de unidad, incluyendo unidad inexistente K
(define S11 ((run S10 switch-drive) #\K))
(define S12 ((run S11 switch-drive) #\C))
S12