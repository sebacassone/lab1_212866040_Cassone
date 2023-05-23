#lang racket

(require "system_21286604_CassoneGonzalez.rkt")

(provide 
run 
add-drive 
register 
login 
logout
switch-drive
md
cd 
add-file
del
rd
copy
move
ren
dir
format
encrypt
decrypt
plus-one
minus-one
grep
view-trash
restore
)

;; TDA System = name (String) x users (list String) x drives (drive list) x current-user (String) x current-drive (Char) x
;; path_actual (String) x Folders (Folder)  x fecha_creacion (String) x fecha_modificación (String)
;; TDA Drive = letter (String list) x name (String) x folders (folder list) x capacity (String)
;; TDA Folder = name (String) x fecha-creación (String) x fecha-modificación (String) x user-creator (String)
;;              x secturity-atributes (Security) x type (String) x path (String)
;; TDA File = name (String) x extension (String) x Contenido (String) x secturity-atributes (Security) x user-creator (String) x type (String) x path (String)
;; Por simplicidad se omitirá el permiso de ejecución del archivo dado que no se pide en el enunciado.


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
                       (get-system-trash system)
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
                                      (get-system-trash system)
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
                                   (get-system-trash system)
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
                                  (get-system-trash system)
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
                                          (get-system-trash system)
                                          (get-system-fecha-creacion system))
                             system
                             )
                         )))
                                                                  
;; Crea la carpeta
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
                                                (cons (make-folder name "" "" (get-system-current-user system) (get-system-path system))
                                                      (get-system-folder system))
                                                (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                ) null)
                                         )
                                   (get-system-current-user system)
                                   (get-system-current-drive system)
                                   (get-system-path system)
                                   ;; Se crea la carpeta nueva
                                   (cons (make-folder name "" "" (get-system-current-user system) (get-system-path system))
                                         (get-system-folder system))
                                   (get-system-trash system)
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
               ;; Si solo se quiere volver a root
               (if (string=? "/" path)
                   ((change-directory system) "/")
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
                   )
               )))
                 
;; Esta función finalmente hace el cambio de path
(define change-directory (lambda (system)
             (lambda (path)
               (cond
                 ;; En caso de que se quiera cambiar de unidad
                 ((string=? path "")
                  (make-system (get-system-name system)
                                   (get-system-usuarios system)
                                   (get-system-drive system)
                                   (get-system-current-user system)
                                   (get-system-current-drive system)
                                   (if (member path (get-all-existing-paths (get-system-folder system)))
                                       path
                                       (get-system-path system)
                                       )
                                   (get-system-folder system)
                                   (get-system-trash system)
                                   (get-system-fecha-creacion system)
                                   )
                  )
                 ;; En caso de que sea una ruta directa
                 ((eq? (string-ref path 0) #\/)
                  (make-system (get-system-name system)
                                   (get-system-usuarios system)
                                   (get-system-drive system)
                                   (get-system-current-user system)
                                   (get-system-current-drive system)
                                   (if (member path (get-all-existing-paths (get-system-folder system)))
                                       path
                                       (get-system-path system)
                                       )
                                   (get-system-folder system)
                                   (get-system-trash system)
                                   (get-system-fecha-creacion system)
                                   )
                  )
                 ;; En el caso de que no sea una ruta directa e incluya alguna puntación o sea el nombre de la carpeta
                 (else (make-system (get-system-name system)
                                   (get-system-usuarios system)
                                   (get-system-drive system)
                                   (get-system-current-user system)
                                   (get-system-current-drive system)
                                   (if (member (change-path (get-system-path system) path) (get-all-existing-paths (get-system-folder system)))
                                       (change-path (get-system-path system) path)
                                       (get-system-path system)
                                       )
                                   (get-system-folder system)
                                   (get-system-trash system)
                                   (get-system-fecha-creacion system)
                                   ))
                 )
               )
             )
  )

;; Crear un nuevo archivo
(define add-file (lambda (system)
                   (lambda (file)
                     (if (eq? (verificar-carpetas-duplicadas (last file) (get-system-path system) (get-system-folder system)) 0) 
                         (make-system (get-system-name system)
                                      (get-system-usuarios system)
                                      ;; Se obtienen todo el resto de drives y se crea nuevamente el drive modificado
                                      (cons (get-rest-drives (get-system-drive system) null (get-system-current-drive system))
                                            (cons (make-drive
                                                   (get-system-current-drive system)
                                                   (get-name-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   (cons (make-file file "" "" (get-system-current-user system) (get-system-path system))
                                                         (get-system-folder system))
                                                   (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   ) null)
                                            )
                                      (get-system-current-user system)
                                      (get-system-current-drive system)
                                      (get-system-path system)
                                      ;; Se crea la carpeta nueva
                                      (cons (make-file file "" "" (get-system-current-user system) (get-system-path system))
                                            (get-system-folder system))
                                      (get-system-trash system)
                                      (get-system-fecha-creacion system)
                                      )
                         system
                         )
                     )))

;; Comienza a recorrer los subdirectorios a eliminar
(define delete-all-files (lambda (files subdirectories)
                           (if (null? subdirectories)
                               files
                               (delete-all-files
                                ;; Se hace un filtro para obtener todos los archivos menos los que estén dentro de la carpeta
                                      (filter
                                       ;; Se filtra la carpeta
                                       (lambda (carpeta)
                                         (not (string=?
                                               (car subdirectories)
                                               (string-append (get-path carpeta) (get-name-folder carpeta))
                                               )
                                              )
                                         )     
                                       ;; Se filtra a todos los archivos que tengan el path de la carpeta que se quiere eliminar
                                       (filter
                                        (lambda (archivo)
                                          (not (string=?
                                                (car subdirectories)
                                                (get-path archivo)
                                                )))
                                        files
                                        ))
                                      (cdr subdirectories))
                               )
                           ))
                           

;; Se encarga de eliminar una carpeta en un drive
(define delete-folder (lambda (system)
                        (lambda (path)
                          (make-system (get-system-name system)
                                       (get-system-usuarios system)
                                      ;; Se obtienen todo el resto de drives y se crea nuevamente el drive modificado
                                      (cons (get-rest-drives (get-system-drive system) null (get-system-current-drive system))
                                            (cons (make-drive
                                                   (get-system-current-drive system)
                                                   (get-name-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   ;; Se hace un filtro para obtener todos los archivos menos los que estén dentro de la carpeta
                                                   (delete-all-files (get-system-folder system) (get-subdirectory-of-folder path (get-all-existing-paths (get-system-folder system)))) 
                                                   (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   ) null)
                                            )
                                      (get-system-current-user system)
                                      (get-system-current-drive system)
                                      (get-system-path system)
                                      ;; Se crea la carpeta nueva
                                      ;; Se hace un filtro para obtener todos los archivos menos los que estén dentro de la carpeta
                                      (delete-all-files (get-system-folder system) (get-subdirectory-of-folder path (get-all-existing-paths (get-system-folder system))))
                                      (append (get-system-trash system) (filter
                                       (lambda (archivo)
                                         (not (member
                                          (get-system-folder system)
                                          (delete-all-files (get-system-folder system) (get-subdirectory-of-folder path (get-all-existing-paths (get-system-folder system))))
                                          ))
                                         )
                                       (get-system-folder system)
                                       ))
                                          
                                      (get-system-fecha-creacion system)
                                      )
                          )))


;; Se hace función para eliminar
(define delete-file (lambda (system)
                      (lambda (fileName)
                        (if (eq? (existe-archivo-en-directorio (get-system-folder system) (get-system-path system) fileName) #t)
                            (make-system (get-system-name system)
                                       (get-system-usuarios system)
                                      ;; Se obtienen todo el resto de drives y se crea nuevamente el drive modificado
                                      (cons (get-rest-drives (get-system-drive system) null (get-system-current-drive system))
                                            (cons (make-drive
                                                   (get-system-current-drive system)
                                                   (get-name-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   ;; Se hace un filtro para obtener todos los archivos menos los que estén dentro de la carpeta
                                                   (filter
                                                    ;; Se filtra la carpeta
                                                    (lambda (archivo)
                                                      (not (and (string=?
                                                            fileName
                                                            (get-file-name archivo)
                                                            ) (string=?
                                                               (get-path archivo)
                                                               (get-system-path system)
                                                               )
                                                              )
                                                           )
                                                      )     
                                                     (get-system-folder system)
                                                     )
                                                   (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   ) null)
                                            )
                                      (get-system-current-user system)
                                      (get-system-current-drive system)
                                      (get-system-path system)
                                       ;; Se filtra a todos los archivos que tengan el path de la carpeta que se quiere eliminar
                                       (filter
                                        ;; Se filtra la carpeta
                                        (lambda (archivo)
                                                      (not (and (string=?
                                                            fileName
                                                            (get-file-name archivo)
                                                            ) (string=?
                                                               (get-path archivo)
                                                               (get-system-path system)
                                                               )
                                                              )
                                                           )
                                                      )       
                                        (get-system-folder system)
                                        )
                                      (append (get-system-trash system) (filter
                                        ;; Se filtra la carpeta
                                        (lambda (archivo)
                                                      (and (string=?
                                                            fileName
                                                            (get-file-name archivo)
                                                            )
                                                           (string=?
                                                               (get-path archivo)
                                                               (get-system-path system)
                                                               ) 
                                                           )
                                                      )       
                                        (get-system-folder system)
                                        ))
                                      (get-system-fecha-creacion system)
                                      )
                            system
                            )
                        )))

;; Se eliminan los archivos obtenidos del filtro en set-files
(define delete-files-in-set-files
  (lambda (system)
    (lambda (drive filesForDelete)
      (if (null? filesForDelete)
          drive
          ((delete-files-in-set-files system) (filter
                                               ;; Se filtra la carpeta
                                               (lambda (archivo)
                                                 (not (equal?
                                                       (car filesForDelete)
                                                       archivo
                                                       )
                                                      )
                                                 )     
                                               drive
                                               )
                                              (cdr filesForDelete))
          )
      )))

;; Se elimina con el asterisco
(define delete-set-files (lambda (system)
                           (lambda (fileName)
                             (cond
                               ((string=? fileName "*.*")
                                (make-system (get-system-name system)
                                       (get-system-usuarios system)
                                      ;; Se obtienen todo el resto de drives y se crea nuevamente el drive modificado
                                      (cons (get-rest-drives (get-system-drive system) null (get-system-current-drive system))
                                            (cons (make-drive
                                                   (get-system-current-drive system)
                                                   (get-name-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   ;; Se hace un filtro para obtener todos los archivos menos los que estén dentro de la carpeta
                                                   (delete-all-files (get-system-folder system) (get-subdirectory-of-folder (get-system-path system) (get-all-existing-paths (get-system-folder system)))) 
                                                   (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   ) null)
                                            )
                                      (get-system-current-user system)
                                      (get-system-current-drive system)
                                      (get-system-path system)
                                       ;; Se filtra a todos los archivos que tengan el path de la carpeta que se quiere eliminar
                                      (delete-all-files (get-system-folder system) (get-subdirectory-of-folder (get-system-path system) (get-all-existing-paths (get-system-folder system))))
                                      (append (get-system-trash system) (filter
                                       (lambda (archivo)
                                         (not (member
                                          (get-system-folder system)
                                          (delete-all-files (get-system-folder system) (get-subdirectory-of-folder (get-system-path system) (get-all-existing-paths (get-system-folder system))))
                                          ))
                                         )
                                       (get-system-folder system)))
                                      (get-system-fecha-creacion system)
                                      )
                                )
                               ((char=? (string-ref fileName (- (string-length fileName) 1)) #\*)
                                (make-system (get-system-name system)
                                       (get-system-usuarios system)
                                      ;; Se obtienen todo el resto de drives y se crea nuevamente el drive modificado
                                      (cons (get-rest-drives (get-system-drive system) null (get-system-current-drive system))
                                            (cons (make-drive
                                                   (get-system-current-drive system)
                                                   (get-name-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   ;; Se hace un filtro para obtener todos los archivos menos los que estén dentro de la carpeta
                                                   ((delete-files-in-set-files system)
                                                    (get-system-folder system)
                                                   (filter
                                                    (lambda (archivo)
                                                      (and
                                                       (string-contains? (get-name-folder archivo) (substring fileName 0 (- (string-length fileName) 1)))
                                                       (string=? (get-type-file archivo) "file")))
                                                    (filter
                                                     (lambda (archivo) (string=? (get-path archivo) (get-system-path system)))
                                                     (get-system-folder system)))
                                                   )
                                                   (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   ) null)
                                            )
                                      (get-system-current-user system)
                                      (get-system-current-drive system)
                                      (get-system-path system)
                                       ;; Se filtra a todos los archivos que tengan el path de la carpeta que se quiere eliminar
                                      ((delete-files-in-set-files system)
                                                    (get-system-folder system)
                                                   (filter
                                                    (lambda (archivo)
                                                      (and
                                                       (string-contains? (get-name-folder archivo) (substring fileName 0 (- (string-length fileName) 1)))
                                                       (string=? (get-type-file archivo) "file")))
                                                    (filter
                                                     (lambda (archivo) (string=? (get-path archivo) (get-system-path system)))
                                                     (get-system-folder system)))
                                                   )
                                      (append (get-system-trash system) (filter
                                       (lambda (archivo)
                                         (not (member
                                          (get-system-folder system)
                                          ((delete-files-in-set-files system)
                                                    (get-system-folder system)
                                                   (filter
                                                    (lambda (archivo)
                                                      (and
                                                       (string-contains? (get-name-folder archivo) (substring fileName 0 (- (string-length fileName) 1)))
                                                       (string=? (get-type-file archivo) "file")))
                                                    (filter
                                                     (lambda (archivo) (string=? (get-path archivo) (get-system-path system)))
                                                     (get-system-folder system)))
                                                   )
                                          ))
                                         )
                                       (get-system-folder system)))
                                      (get-system-fecha-creacion system)
                                      )
                                )
                               (else (make-system (get-system-name system)
                                       (get-system-usuarios system)
                                      ;; Se obtienen todo el resto de drives y se crea nuevamente el drive modificado
                                      (cons (get-rest-drives (get-system-drive system) null (get-system-current-drive system))
                                            (cons (make-drive
                                                   (get-system-current-drive system)
                                                   (get-name-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   ;; Se hace un filtro para obtener todos los archivos menos los que estén dentro de la carpeta
                                                  ((delete-files-in-set-files system)
                                                    (get-system-folder system)
                                                    (filter
                                                     (lambda (archivo3)
                                                       (string=? (get-extension-file archivo3) (cadr (string-split fileName "*.")))
                                                       )
                                                     (filter
                                                      (lambda (archivo2)
                                                        (and
                                                         (string-contains? (get-name-folder archivo2) (car (string-split fileName "*.")))
                                                         (string=? (get-type-file archivo2) "file")
                                                         ))
                                                      (filter
                                                       (lambda (archivo) (string=? (get-path archivo) (get-system-path system)))
                                                       (get-system-folder system))
                                                      )
                                                     )
                                                   )
                                                   (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   ) null)
                                            )
                                      (get-system-current-user system)
                                      (get-system-current-drive system)
                                      (get-system-path system)
                                       ;; Se filtra a todos los archivos que tengan el path de la carpeta que se quiere eliminar
                                      ((delete-files-in-set-files system)
                                                    (get-system-folder system)
                                                    (filter
                                                     (lambda (archivo3)
                                                       (string=? (get-extension-file archivo3) (cadr (string-split fileName "*.")))
                                                       )
                                                     (filter
                                                      (lambda (archivo2)
                                                        (and
                                                         (string-contains? (get-name-folder archivo2) (car (string-split fileName "*.")))
                                                         (string=? (get-type-file archivo2) "file")
                                                         ))
                                                      (filter
                                                       (lambda (archivo) (string=? (get-path archivo) (get-system-path system)))
                                                       (get-system-folder system))
                                                      )
                                                     )
                                                   )
                                      (append (get-system-trash system) (filter
                                       (lambda (archivo)
                                         (not (member
                                          (get-system-folder system)
                                          ((delete-files-in-set-files system)
                                                    (get-system-folder system)
                                                    (filter
                                                     (lambda (archivo3)
                                                       (string=? (get-extension-file archivo3) (cadr (string-split fileName "*.")))
                                                       )
                                                     (filter
                                                      (lambda (archivo2)
                                                        (and
                                                         (string-contains? (get-name-folder archivo2) (car (string-split fileName "*.")))
                                                         (string=? (get-type-file archivo2) "file")
                                                         ))
                                                      (filter
                                                       (lambda (archivo) (string=? (get-path archivo) (get-system-path system)))
                                                       (get-system-folder system))
                                                      )
                                                     )
                                                   )
                                          ))
                                         )
                                       (get-system-folder system)))
                                      (get-system-fecha-creacion system)
                                      ))
                               )
                             )))

;; Se crea el requerimiento de eliminar un archivo o una carpeta con archivos en un mismo directorio
;; Casos:
;; El primer es que sea una carpeta y elimina todo el contenido en él
;; El segundo caso es que sea un archivo
;; El tercero que sean varios archivos según un patrón
(define del (lambda (system)
              (lambda (fileName)
                ;; Si la carpeta existe y es efectivamente una carpeta
                (if
                 ;; Esto es solo el condicional, se obtiene la ruta del archivo y se verifica que exista como path, osea si es carpeta
                 (member
                 ((get-path-complete system) fileName)
                 (get-all-existing-paths (get-system-folder system)
                                         )
                 )
                 ;; En caso de que sea una carpeta
                 ((delete-folder system) ((get-path-complete system) fileName))
                 ;; Ahora se distingue entre los casos que se elimina solo
                 ;; un archivo o hace uso de * con algún acrónimo para eliminar
                 ;; las palabras que comiencen como f o feo, cosas así.
                 (if (contiene-asterisco fileName)
                     ((delete-set-files system) fileName)
                     ;; ((delete-set-files system) fileName)
                     ((delete-file system) fileName)
                  )
                 )
                )))

;; Se crea requerimiento de remover directorio vacío (simil a rmdir en linux)
(define rd (lambda (system)
             (lambda (folderName)
               (if
                 ;; Esto es solo el condicional, se obtiene la ruta del archivo y se verifica que exista como path, osea si es carpeta
                 (member
                 ((get-path-complete system) folderName)
                 (get-all-existing-paths (get-system-folder system)
                                         )
                 )
                 (if (null?
                      (filter
                       (lambda (archivo)
                         (string=? (get-path archivo) ((get-path-complete system) folderName)))
                       (get-system-folder system))
                      )
                     ((delete-folder system) ((get-path-complete system) folderName))
                     system
                     )
                 system)
               )))

                    
;; Se crea requerimiento de Copy, esta función usará la función de obtener un archivo
;; en un mismo directorio, y también la función de crear un archivo
(define cp (lambda (system)
               (lambda (source targetPath)
                 ;; Se verifica que el archivo exista en la ruta actual
                 (if
                  (null? (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))
                 system
                 ;; Verifica que la letra actual es igual al la del targetPath si corresponde
                 (if (equal? (string-ref targetPath 0) (get-system-current-drive system))
                      ;; Se verifica que targetPath sea una ruta válida
                     (if
                      (member (substring targetPath 2 (string-length targetPath)) (get-all-existing-paths (get-system-folder system)))
                      ;; Si lo es, verifica que al insertar el archivo en en la ruta deseada no sea duplicado
                      (if
                       (eq? (verificar-carpetas-duplicadas source (substring targetPath 2 (string-length targetPath)) (get-system-folder system)) 0)
                       (if
                        (equal? (get-type-file (car (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))) "folder")
                           ((insertar-conjunto-de-carpetas-y-archivos system) (cambiar-path-carpetas
                                                       (get-all-files-in-subdirectories
                                                        (get-system-folder system)
                                                        source
                                                        (get-subdirectory-of-folder
                                                         (string-append (if (eq? (get-system-path system) "/") (get-system-path system) (string-append (get-system-path system) "/")) source)
                                                         (get-all-existing-paths (get-system-folder system)))
                                                        )
                                                       (substring targetPath 2 (string-length targetPath))
                                                       )
                                                      )
                           ((insertar-archivo system) (cambiar-path-archivo
                                                       (car (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))
                                                       (get-system-current-user system)
                                                       (substring targetPath 2 (string-length targetPath))))
                           )
                       system
                       )
                      system
                      )
                     (if
                      (member (substring targetPath 2 (string-length targetPath)) (get-all-existing-paths (get-system-folder ((switch-drive system) (string-ref targetPath 0)))))
                      ;; Si lo es, verifica que al insertar el archivo en en la ruta deseada no sea duplicado
                      (if
                       (eq? (verificar-carpetas-duplicadas source (substring targetPath 2 (string-length targetPath)) (get-system-folder ((switch-drive system) (string-ref targetPath 0)))) 0)
                       (if
                        (equal? (get-type-file (car (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))) "folder")
                           ((switch-drive ((insertar-conjunto-de-carpetas-y-archivos ((switch-drive system) (string-ref targetPath 0)))
                            (cambiar-path-carpetas
                             (get-all-files-in-subdirectories
                              (get-system-folder system)
                              source
                              (get-subdirectory-of-folder
                               (string-append
                                (if (eq? (get-system-path system) "/") (get-system-path system) (string-append (get-system-path system) "/"))
                                source
                                )
                               (get-all-existing-paths (get-system-folder system))
                               )
                              )
                             (substring targetPath 2 (string-length targetPath)))
                            )) (get-system-current-drive system))
                           ((switch-drive ((insertar-archivo ((switch-drive system) (string-ref targetPath 0)))
                            (cambiar-path-archivo
                             (car (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))
                             (get-system-current-user system)
                             (substring targetPath 2 (string-length targetPath)))
                            )) (get-system-current-drive system))
                           )
                       system
                       )
                      system
                      )
                     )
                 )
                 )))

;; Esta función es intermediara entre cp, para verificar si las unidades existen y todo eso
(define copy (lambda (system)
               (lambda (source path)
                 (if (string=? "/" path)
                   ((cp system) source (string-append (list->string (list (get-system-current-drive system))) ":" "/"))
                   ;; Si el path tiene la forma "X:/", verifica si la unidad X existe y si X no es la unidad actual se hace el cambio
                   ;; de unidad
                   (if (and (eq? (string-ref path 1) #\:) (eq? (string-ref path 2) #\/))
                       ;; Se verifica que la unidad existe, si existe se verifica si es la unidad actual, de lo contario no hace nada
                       (if (memq (string-ref path 0) (get-letters-all-drives system))
                           ;; Se verifica si la unidad pedida es la actual o no
                           ((cp system) source path)
                           system
                           )
                       ((cp system) source (string-append (list->string (list (get-system-current-drive system))) ":" path))
                       )
                   )
                 )))

;; Función que hace lo mismo que copy pero elimina el archivo de origen
(define mv (lambda (system)
               (lambda (source targetPath)
                 ;; Se verifica que el archivo exista en la ruta actual
                 (if
                  (null? (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))
                 system
                 ;; Verifica que la letra actual es igual al la del targetPath si corresponde
                 (if (equal? (string-ref targetPath 0) (get-system-current-drive system))
                      ;; Se verifica que targetPath sea una ruta válida
                     (if
                      (member (substring targetPath 2 (string-length targetPath)) (get-all-existing-paths (get-system-folder system)))
                      ;; Si lo es, verifica que al insertar el archivo en en la ruta deseada no sea duplicado
                      (if
                       (eq? (verificar-carpetas-duplicadas source (substring targetPath 2 (string-length targetPath)) (get-system-folder system)) 0)
                       (if
                        (equal? (get-type-file (car (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))) "folder")
                           ((insertar-conjunto-de-carpetas-y-archivos ((del system) source)) (cambiar-path-carpetas
                                                       (get-all-files-in-subdirectories
                                                        (get-system-folder system)
                                                        source
                                                        (get-subdirectory-of-folder
                                                         (string-append (if (eq? (get-system-path system) "/") (get-system-path system) (string-append (get-system-path system) "/")) source)
                                                         (get-all-existing-paths (get-system-folder system)))
                                                        )
                                                       (substring targetPath 2 (string-length targetPath))
                                                       )
                                                      ) 
                           ((insertar-conjunto-de-carpetas-y-archivos ((del system)) source) (cambiar-path-archivo
                                                       (car (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))
                                                       (get-system-current-user system)
                                                       (substring targetPath 2 (string-length targetPath))))
                           )
                       system
                       )
                      system
                      )
                     (if
                      (member (substring targetPath 2 (string-length targetPath)) (get-all-existing-paths (get-system-folder ((switch-drive system) (string-ref targetPath 0)))))
                      ;; Si lo es, verifica que al insertar el archivo en en la ruta deseada no sea duplicado
                      (if
                       (eq? (verificar-carpetas-duplicadas source (substring targetPath 2 (string-length targetPath)) (get-system-folder ((switch-drive system) (string-ref targetPath 0)))) 0)
                       (if
                        (equal? (get-type-file (car (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))) "folder")
                           ((switch-drive ((insertar-conjunto-de-carpetas-y-archivos ((switch-drive ((del system) source)) (string-ref targetPath 0)))
                            (cambiar-path-carpetas
                             (get-all-files-in-subdirectories
                              (get-system-folder system)
                              source
                              (get-subdirectory-of-folder
                               (string-append
                                (if (eq? (get-system-path system) "/") (get-system-path system) (string-append (get-system-path system) "/"))
                                source
                                )
                               (get-all-existing-paths (get-system-folder system))
                               )
                              )
                             (substring targetPath 2 (string-length targetPath)))
                            )) (get-system-current-drive system))
                           ((switch-drive ((insertar-archivo ((switch-drive ((del system) source)) (string-ref targetPath 0)))
                            (cambiar-path-archivo
                             (car (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))
                             (get-system-current-user system)
                             (substring targetPath 2 (string-length targetPath)))
                            )) (get-system-current-drive system))
                           )
                       system
                       )
                      system
                      )
                     )
                 )
                 )))

(define move (lambda (system)
               (lambda (source path)
                 (if (string=? "/" path)
                   ((cp system) source (string-append (list->string (list (get-system-current-drive system))) ":" "/"))
                   ;; Si el path tiene la forma "X:/", verifica si la unidad X existe y si X no es la unidad actual se hace el cambio
                   ;; de unidad
                   (if (and (eq? (string-ref path 1) #\:) (eq? (string-ref path 2) #\/))
                       ;; Se verifica que la unidad existe, si existe se verifica si es la unidad actual, de lo contario no hace nada
                       (if (memq (string-ref path 0) (get-letters-all-drives system))
                           ;; Se verifica si la unidad pedida es la actual o no
                           ((mv system) source path)
                           system
                           )
                       ((mv system) source (string-append (list->string (list (get-system-current-drive system))) ":" path))
                       )
                   )
                 )))


;; Requerimiento de renombrar un archivo o carpeta
(define ren (lambda (system)
               (lambda (source targetName)
                 ;; Se verifica que el archivo exista en la ruta actual
                 (if
                  (null? (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))
                 system
                  ;; Si lo es, verifica que al insertar el archivo en en la ruta deseada no sea duplicado
                  (if
                   (eq? (verificar-carpetas-duplicadas targetName (get-system-path system) (get-system-folder system)) 0)
                   (if
                    (eq? (get-type-file (car (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))) "folder")
                    ((insertar-conjunto-de-carpetas-y-archivos ((del system) source)) (cambiar-nombre-a-carpeta (cambiar-path-renombrar-carpeta
                                                                        (get-all-files-in-subdirectories
                                                                         (get-system-folder system)
                                                                         source
                                                                         (get-subdirectory-of-folder
                                                                          (string-append (if (eq? (get-system-path system) "/") (get-system-path system) (string-append (get-system-path system) "/")) source)
                                                                          (get-all-existing-paths (get-system-folder system)))
                                                                         )
                                                                        source
                                                                        targetName
                                                                        ) source targetName)
                                                                       )
                    ((insertar-archivo ((del system) source)) (cambiar-nombre-a-archivo (cambiar-path-archivo
                                                (car (get-file-in-current-directory (get-system-folder system) source (get-system-path system)))
                                                (get-system-current-user system)
                                                (get-system-path system)) targetName))
                    )
                   system
                   )
                  )
                 )))

;; Requerimiento de DIR
(define dir (lambda (system)
              (lambda (params)
                (define dir-int (lambda (system)
                                  (lambda (files params)
                                    (cond 
                                      ((null? params) files)
                                      ((and (member "/s" params) (member "/a" params))
                                       ((dir-int system) 
                                        (append files
                                                (filter 
                                                 (lambda (archivo) (not (member archivo files)))
                                                 (get-all-files-in-subdirectories
                                                  (get-system-folder system)
                                                  (get-system-path system)
                                                  (if (equal? (get-system-path system) "/")
                                                      (get-all-existing-paths (get-system-folder system))
                                                      (get-subdirectory-of-folder
                                                       (get-system-path system)
                                                       (get-all-existing-paths (get-system-folder system))
                                                       )
                                                      )
                                                  )
                                                 )
                                                )
                                        (cdr (cdr params))
                                        )
                                       )
                                      ((member "/a" params)
                                       ((dir-int system) 
                                        (append files
                                                (filter
                                                 (lambda (archivo)
                                                   (and
                                                    (string=? (get-path archivo) (get-system-path system))
                                                   (not (member archivo files)))
                                                   )
                                                 (get-system-folder system))
                                                 )
                                        (cdr params)
                                        ))
                                      ((member "/s" params)
                                       ((dir-int system) 
                                        (append files
                                                (filter 
                                                 (lambda (archivo)
                                                   (and
                                                    (not (member archivo files))
                                                    (not (member #\h (if (string=? (get-type-file archivo) "file") (get-file-security-atributes archivo) (get-folder-security-atributes archivo))))
                                                    )
                                                   )
                                                 (get-all-files-in-subdirectories
                                                  (get-system-folder system)
                                                  (get-system-path system)
                                                  (if (equal? (get-system-path system) "/")
                                                      (get-all-existing-paths (get-system-folder system))
                                                      (get-subdirectory-of-folder
                                                       (get-system-path system)
                                                       (get-all-existing-paths (get-system-folder system))
                                                       )
                                                      )
                                                  )
                                                 )
                                                )
                                        (cdr params)
                                        )
                                       )
                                      ((and (member "/o" params) (member "D" params))
                                       (sort files (lambda (sub1 sub2) (< (list-ref sub1 1) (list-ref sub2 1))))
                                       )
                                      ((and (member "/o" params) (member "-D" params))
                                       (sort files (lambda (sub1 sub2) (> (list-ref sub1 1) (list-ref sub2 1))))
                                       )
                                      ((and (member "/o" params) (member "N" params))
                                       (sort files (lambda (sub1 sub2) (string<? (list-ref sub1 0) (list-ref sub2 0))))
                                       )
                                     ((and (member "/o" params) (member "-N" params))
                                       (sort files (lambda (sub1 sub2) (string>? (list-ref sub1 0) (list-ref sub2 0))))
                                       )
                                      (else "/s para obtener subcarpetas; /a para obtener archivos ocultos; /o para ordenar, con N para ordenar alfabeticamente y D por fecha de creación, si se acompaña por delante un signo - ordena de manera descendente")
                                    )
                                    )))
                (write
                 ((dir-int system)
                  (filter
                   (lambda (archivo)
                     (and (string=? (get-path archivo) (get-system-path system))
                          (not (member #\h (if (string=? (get-type-file archivo) "file") (get-file-security-atributes archivo) (get-folder-security-atributes archivo))))
                          )
                     )
                     (get-system-folder system))
                  (string-split params " ")))
                )))

;; Format
(define formatear-unidad (lambda (system)
                           (lambda (letter name)
                             (make-system (get-system-name system)
                                          (get-system-usuarios system)
                                          ;; Se obtienen todo el resto de drives y se crea nuevamente el drive modificado
                                          (cons (get-rest-drives (get-system-drive system) null (get-system-current-drive system))
                                                (cons (make-drive
                                                       (get-system-current-drive system)
                                                       name
                                                       '()
                                                       (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                       ) null)
                                                )
                                          (get-system-current-user system)
                                          (get-system-current-drive system)
                                          (get-system-path system)
                                          ;; Se crea la carpeta nueva
                                          '()
                                          (get-system-trash system)
                                          (get-system-fecha-creacion system)
                                          )
                             )
                           )
  )

(define format (lambda (system)
                 (lambda (letter name)
                   (if (memq letter (get-letters-all-drives system))
                       ((switch-drive ((formatear-unidad ((switch-drive system) letter)) letter name)) (get-system-current-drive system)) 
                       system
                       )
                   )
                 )
  )

;; Funciones que tienen que ver con encriptación de archivos
(define plus-one (lambda (palabra)
                   (list->string
                    (map (lambda (caracter)
                           (integer->char (+ 1 (char->integer caracter))))
                         (string->list palabra)))
                   ))

(define minus-one (lambda (palabra)
                   (list->string
                    (map (lambda (caracter)
                           (integer->char (- (char->integer caracter) 1)))
                         (string->list palabra))
                    )
                   ))

(define encrypt (lambda (system)
                  (lambda (encryptFn minus-one password folderName)
                 ;; Se verifica que el archivo exista en la ruta actual
                    (if
                     (null? (get-file-in-current-directory (get-system-folder system) folderName (get-system-path system)))
                     system
                     (if
                      (eq? (get-type-file (car (get-file-in-current-directory (get-system-folder system) folderName (get-system-path system)))) "folder")
                      ((insertar-conjunto-de-carpetas-y-archivos ((del system) folderName)) (encriptar-carpetas
                                                                                         (get-all-files-in-subdirectories
                                                                                          (get-system-folder system)
                                                                                          folderName
                                                                                          (get-subdirectory-of-folder
                                                                                           (string-append (if (eq? (get-system-path system) "/") (get-system-path system) (string-append (get-system-path system) "/")) folderName)
                                                                                           (get-all-existing-paths (get-system-folder system)))
                                                                                          )
                                                                                         encryptFn
                                                                                         minus-one
                                                                                         password
                                                                                         )
                                                                                        )
                      ((insertar-archivo ((del system) folderName)) (encriptar-archivo
                                                                 (car (get-file-in-current-directory (get-system-folder system) folderName (get-system-path system)))
                                                                 encryptFn
                                                                 minus-one
                                                                 password
                                                                 ))               
                      )
                     )
                    )))

;; Desencriptar un archivo
;; Esta función obtiene todas las rutas existentes
(define decrypt (lambda (system)
                  (lambda (password folderName)
                    (if
                     (null? (get-file-in-current-directory (get-system-folder system) (plus-one folderName) (get-system-path system)))
                     system
                         (if
                          (eq? (get-type-file (car (get-file-in-current-directory (get-system-folder system) (plus-one folderName) (get-system-path system)))) "folder")
                          (if (string=? (get-folder-password-encrypt (car (get-file-in-current-directory (get-system-folder system) (plus-one folderName) (get-system-path system)))) password)
                              ((insertar-conjunto-de-carpetas-y-archivos-encrypt ((del system) (plus-one folderName))) (desencriptar-carpetas
                                                                                                     (get-all-files-in-subdirectories
                                                                                                      (get-system-folder system)
                                                                                                      (plus-one folderName)
                                                                                                      (get-subdirectory-of-folder
                                                                                                       (string-append (if (eq? (get-system-path system) "/") (get-system-path system) (string-append (get-system-path system) "/")) folderName)
                                                                                                       (get-all-existing-paths-decrypt (get-system-folder system)))
                                                                                                      )
                                                                                                     ) folderName
                                                                                                    )
                              system
                              )
                          (if (string=? (get-file-password-encrypt (car (get-file-in-current-directory (get-system-folder system) (plus-one folderName) (get-system-path system)))) password)
                              ((insertar-archivo-encrypt ((del system) folderName)) (desencriptar-archivo
                                                                             (car (get-file-in-current-directory (get-system-folder system) (plus-one folderName) (get-system-path system)))
                                                                             )
                                                                            (car (get-file-in-current-directory (get-system-folder system) (plus-one folderName) (get-system-path system)))
                                                                            )
                              system
                              )
                          )
                     )
                     )))
                    
;; Se comienza requermiento de Grep
(define grep (lambda (system)
               (lambda (pattern fileName)
                 (if (not (string=? fileName "."))
                     (if (null? (get-file-in-current-directory (get-system-folder system) fileName (get-system-path system)))
                         "El nombre del archivo no existe en el directorio actual"
                         (if (string-contains? (list-ref (car (get-file-in-current-directory (get-system-folder system) fileName (get-system-path system))) 4) pattern)
                             (write (list pattern (buscar-posicion-subcadena
                                         (list-ref (car (get-file-in-current-directory (get-system-folder system) fileName (get-system-path system))) 4)
                                         pattern)))
                             "No se encontró coincidencias"
                             )
                         )
                     ;; Recorre primero todos los archivos que pertenecen al mismo path y también los archivos que son tipo archivo.
                     ;; Luego con map se recorre estos archivos para obtener coincidencias entre estos archivos. Si no hay tira null.
                     ;; Finalmente se filtran las listas vacias
                     (write (filter
                      (lambda (archivo) (not (null? archivo)))
                      (map
                      (lambda (archivo)
                        (if (null? (get-file-in-current-directory (get-system-folder system) (get-file-name archivo) (get-system-path system)))
                         null
                         (if (string-contains? (list-ref (car (get-file-in-current-directory (get-system-folder system) (get-file-name archivo) (get-system-path system))) 4) pattern)
                             (list pattern (buscar-posicion-subcadena
                                         (list-ref (car (get-file-in-current-directory (get-system-folder system) (get-file-name archivo) (get-system-path system))) 4)
                                         pattern) (get-file-name archivo))
                             null
                             )
                         ))
                      (filter (lambda (archivo) (and (equal? (get-path archivo) (get-system-path system)) (equal? (get-type-file archivo) "file"))) (get-system-folder system))
                      )))
                     ))))
                 
;; Se obtiene la papelera de reciclaje del sistema
(define view-trash (lambda (system)
                     (write (get-system-trash system))))

;; Se obtiene todos los archivos o solo un archivo en concreto
(define restore (lambda (system)
                  (lambda (fileName)
                    (if (equal? fileName "*.*")
                        (view-trash system)
                        (if (null? (filter (lambda (archivo) (string=? (get-name-folder archivo) fileName)) (get-system-trash system)))
                            "Este archivo no se encuentra en la papelera"
                            (write (filter (lambda (archivo) (string=? (get-name-folder archivo) fileName)) (get-system-trash system)))
                            )
                        )
                    )))
                           