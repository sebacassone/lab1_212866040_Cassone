#lang racket

(require "constructores.rkt")
(require "selectores.rkt")
(provide run add-drive register login logout switch-drive md cd)

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
                                            (if (null? (filter (lambda (folder) (eq? (get-type-file folder) "folder")) (filter (lambda (folder) (eq? (get-path folder) path)) folders)))
                                                0
                                                (if
                                                 (eq?
                                                  (car (car (filter (lambda (folder) (eq? (get-type-file folder) "folder")) (filter (lambda (folder) (eq? (get-path folder) path)) folders))))
                                                  name)
                                                 1
                                                 (verificar-carpetas-duplicadas name path (cdr folders))
                                                 )
                                            )
                                         )
                                        )
  )
                                        

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

;; Todas estas funciones son usadas para hacer el cambio de directorio
;; Cambiar de Directorio en caso de los ..
(define (eliminar-palabra-final path)
  (define (eliminar-palabra-final-int parts remaining-parts)
    (if (null? (cdr parts))
        (string-join (reverse remaining-parts) "/")
        (eliminar-palabra-final-int (cdr parts) (cons (car parts) remaining-parts))))
  
  (eliminar-palabra-final-int (string-split path "/") '()))

;; Esta función hace el cambio de path, luego se tiene que verificar que este path existe
(define change-path (lambda (originalPath newPath)
                      (cond
                        ;; Si ocurre esta condición es por que quedo originalPath en null es por que llegó a la raiz 
                        ((string=? originalPath "")
                             (change-path "/" newPath))
                        ;; Si newPath que es la ruta que quiere ir el usuario queda en null es por que ya llegó a la carpeta
                        ;; deseada, por lo que solo se verifica que originalPath esté correcto, en caso que no lo esté se corrige
                        ((string=? newPath "")
                         (if (not (eq? (string-ref originalPath 0) #\/))
                             (string-append "/" originalPath)
                             originalPath))
                        ;; Este es el caso de los ..
                        ((and (eq? (string-ref newPath 0) #\.) (eq? (string-ref newPath 1) #\.))
                         ;; Si ya está en la raiz no se hace nada
                         (if (eq? originalPath "/")
                             ;; Si queda solo un ".." no se hace nada y se retorna en newPath null
                             (if (string=? newPath "..")
                                 (change-path originalPath "")
                                 ;; Si aún queda ruta por recorrer se quita solo los .. con el /
                                 (change-path originalPath (substring newPath 3 (string-length newPath)))
                                 )
                             ;; Si originalPath no es la raiz, se verifica si queda solo un ".."
                             (if (string=? newPath "..")
                                 ;; Si es así se retorna null en newPath y se elimina la palabra del final despues del último /
                                 (change-path (eliminar-palabra-final originalPath) "")
                                 (change-path (eliminar-palabra-final originalPath) (substring newPath 3 (string-length newPath)))
                                 )
                         ))
                        ;; Este es el caso del punto normal "./" para indicar la ruta actual
                        ((eq? (string-ref newPath 0) #\.)
                         ;; Se quita el . y se vuelve a llamar a la función
                         (change-path originalPath (substring (substring newPath 1) 1))
                         )
                        ;; Este es el caso en que ya se eliminan todos los .. y los .
                        ;; Finalmente se actualiza la ruta uniendo estos dos strings newPath y originalPath
                        (else (if (eq? originalPath "/")
                                  ;; En caso de que originalPath ya esté en la raíz simplemente se unen
                                   (string-append originalPath newPath)
                                   (if (not (eq? (string-ref originalPath 0) #\/))
                                       ;; Si no tiene un "/" originalPath se le agrega y se unen 
                                       (string-append (string-append "/" originalPath "/") newPath)
                                       (string-append (string-append originalPath "/") newPath))
                        ))
                       )
                      ))

;; Esta función se encarga de hacer el cambio de unidad previo a hacer el cambio de path
(define cd (lambda (system)
             (lambda (path)
               ;; Si el path tiene la forma "X:/", verifica si la unidad X existe y si X no es la unidad actual se hace el cambio
               ;; de unidad
               (if (and (eq? (string-ref path 1) #\:) (eq? (string-ref path 2) #\/))
                   ;; Se verifica que la unidad existe, si existe se verifica si es la unidad actual, de lo contario no hace nada
                   (if (memq (string-ref path 0) (get-letters-all-drives system))
                       ;; Se verifica si la unidad pedida es la actual o no
                       (if (eq? (string-ref path 0) (get-system-current-drive system))
                           ((change-directory system) (substring path 3 (string-length path)))
                           ((change-directory ((switch-drive system) (string-ref path 0))) (substring path 3 (string-length path)))
                           )
                       system
                       )
                   ((change-directory system) path)
                   )
               )))
                       
;; Esta función finalmente hace el cambio de path
(define change-directory (lambda (system)
             (lambda (path)
               (cond
                 ((eq? (string-ref path 0) #\/)
                  (make-system (get-system-name system)
                                   (get-system-usuarios system)
                                   (get-system-drive system)
                                   (get-system-current-user system)
                                   (get-system-current-drive system)
                                   (verificar-existencia-path path)
                                   (get-system-folder system)
                                   (get-system-fecha-creacion system)
                                   )
                  )
                 (else (make-system (get-system-name system)
                                   (get-system-usuarios system)
                                   (get-system-drive system)
                                   (get-system-current-user system)
                                   (get-system-current-drive system)
                                   (verificar-existencia-path (change-path (get-system-path system) path))
                                   (get-system-folder system)
                                   (get-system-fecha-creacion system)
                                   ))
                 )
               )
             )
  )