#lang racket

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

;; Obtener el tipo 

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
                       (cons (make-drive letter name null capacity)
                             (get-system-drive system))
                       (get-system-current-user system)
                       (get-system-current-drive system)
                       (get-system-path system)
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
                                      (get-system-path system)
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
                                   (get-system-path system)
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
                                  (get-system-path system)
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
                             
;; Crea carpetas
(define verificar-carpetas-duplicadas (lambda (name path folders)
                                        ;; Filtro solo las carpetas de esta ruta
                                        (if (null? folders)
                                            0
                                            (if
                                             (eq?
                                              (car (car (filter (lambda (folder) (eq? (get-type-file folder) "folder")) (filter (lambda (folder) (eq? (get-path folder) path)) folders))))
                                              name)
                                             1
                                             (verificar-carpetas-duplicadas name path (cdr folders))
                                             )
                                            )
                                        ))
                                        

(define md (lambda (system)
                (lambda (name)
                  (if (eq? (verificar-carpetas-duplicadas name (get-system-path system) (get-system-folder system)) 0) 
                      (make-system (get-system-name system)
                                   (get-system-usuarios system)
                                   ;; Se obtienen todo el resto de drives y se crea nuevamente el drive modificado
                                   (cons (get-rest-drives (get-system-drive system) null (get-system-current-drive system))
                                         (cons (make-drive
                                                (get-system-current-drive system)
                                                (get-name-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                (cons (make-folder name (get-system-current-user system) (get-system-path system))
                                                      (get-system-folder system))
                                                (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                ) null)
                                         )
                                   (get-system-current-user system)
                                   (get-system-current-drive system)
                                   (get-system-path system)
                                   ;; Se crea la carpeta nueva
                                   (cons (make-folder name (get-system-current-user system) (get-system-path system))
                                         (get-system-folder system))
                                   (get-system-fecha-creacion system)
                                   )
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
S11
(define S12 ((run S11 switch-drive) #\C))
S12
(define S13 ((run S12 switch-drive) #\D))
S13

;; Crea carpetas
(define S14 ((run S13 md) "Folder"))
S14
(define S15 ((run S14 switch-drive) #\C))
S15
(define S16 ((run S15 switch-drive) #\D))
S16
(define S17 ((run S16 md) "Folder"))
S17
(define S18 ((run S17 md) "Folder2"))
S18
(define S19 ((run S18 md) "Folder2"))
S19
(define S20 ((run S19 switch-drive) #\C))
(define S21 ((run S20 md) "Folder"))
(define S22 ((run S21 md) "Folder2"))
(define S23 ((run S22 md) "Folder2"))
S23