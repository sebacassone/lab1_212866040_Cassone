#lang racket

(provide system run add-drive register login logout switch-drive md cd add-file)

;; TDA System = name (String) x users (list String) x drives (drive list) x current-user (String) x current-drive (Char) x
;; path_actual (String) x Folders (Folder)  x fecha_creacion (String) x fecha_modificación (String)
;; TDA Drive = letter (String list) x name (String) x folders (folder list) x capacity (String)
;; TDA Folder = name (String) x fecha-creación (String) x fecha-modificación (String) x user-creator (String)
;;              x secturity-atributes (Security) x type (String) x path (String)
;; TDA File = name (String) x extension (String) x Contenido (String) x secturity-atributes (Security) x user-creator (String) x type (String) x path (String)
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
                        (name user-creator path . securityAtributes)
                      (list name (current-seconds) (current-seconds) securityAtributes user-creator "folder" path)
                      ))

(define make-new-folder (lambda
                        (name user-creator path securityAtributes)
                      (list name (current-seconds) (current-seconds) securityAtributes user-creator "folder" path)
                      ))

;; Se constuye un archivo
;; TDA File = name (String) x extension (String) x Contenido (String) x secturity-atributes (Security) x type (String) x path (String)
(define file (lambda
                 (name extension content . security-atributes)
               (list security-atributes content extension name)))

(define new-file (lambda
                 (name extension content security-atributes)
               (list security-atributes content extension name)))

;; Se le da forma del TDA
(define make-file (lambda
                      (archivo user-creator path)
                    (reverse (cons path (cons "file" (cons user-creator archivo))))
                    ))

;; Selectores de TDA Archivo
(define get-file-name (lambda (archivo)
                        (list-ref archivo 0)))

(define get-file-extension (lambda (archivo)
                             (list-ref archivo 1)))

(define get-file-content (lambda (archivo)
                                (list-ref archivo 2)))

(define get-file-security-atributes (lambda (archivo)
                           (list-ref archivo 3)))

;; Selectores de TDA Carpeta
(define get-folder-security-atributes (lambda (folder)
                           (list-ref folder 3)))

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

;; Obtiene el nombre de una carpeta
(define get-name-folder (lambda (folder)
                          (list-ref folder 0)))

;; Obtener el nombre de usuario en una carpeta
(define get-user-in-folder (lambda (folder)
                             (list-ref folder 4)))

;; Obtener el nombre de usuario en un archivo
(define get-user-in-file (lambda (archivo)
                           (list-ref archivo 4)))
 
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

;; Esta función obtiene todas las rutas existentes
(define get-all-existing-paths (lambda (folders)
                                 (cons "/" (map (lambda (folder) (string-append (get-path folder) (if (eq? (get-path folder) "/") (get-name-folder folder) (string-append "/" (get-name-folder folder))))) (filter (lambda (folder) (eq? (get-type-file folder) "folder")) folders)))
                                 ))

;; Obtiene la extensión de un archivo
(define get-extension-file (lambda (archivo)
                                    (list-ref archivo 1)))
                                 
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
                                            (if (null? (filter (lambda (folder) (equal? (get-path folder) path)) folders))
                                                0
                                                (if
                                                 (eq?
                                                  (car (car (filter (lambda (folder) (equal? (get-path folder) path)) folders)))
                                                  name)
                                                 1
                                                 (verificar-carpetas-duplicadas name path (cdr folders))
                                                 )
                                            )
                                         )
                                        )
  )
                                        
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
                                                   (cons (make-file file (get-system-current-user system) (get-system-path system))
                                                         (get-system-folder system))
                                                   (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                   ) null)
                                            )
                                      (get-system-current-user system)
                                      (get-system-current-drive system)
                                      (get-system-path system)
                                      ;; Se crea la carpeta nueva
                                      (cons (make-file file (get-system-current-user system) (get-system-path system))
                                            (get-system-folder system))
                                      (get-system-fecha-creacion system)
                                      )
                         system
                         )
                     )))

;; Obtiene el path completo de un archivo para verificar que sea una carpeta válida
(define get-path-complete (lambda (system)
                            (lambda (fileName)
                              (if (eq? (string-ref fileName 0) #\/) ;; Si es una ruta
                                  fileName
                                  ;; Si fileName no es una ruta entonces se trata de una carpeta local
                                  (if (eq? (get-system-path system) "/") 
                                      (string-append (get-system-path system) fileName) ;; Si la ruta actual es root se une el nombre del archivo
                                      (string-append (get-system-path system) (string-append "/" fileName)) ;; Si no está en root se une con un /
                                      )
                                  )
                              )))

;; Obtener todos los subdirectorios de una ruta
(define get-subdirectory-of-folder (lambda (path paths)
                                     (cons path (filter (lambda (palabra) (string-contains? palabra (if (string=? path "/") path (string-append path "/")))) paths))
                                     ))

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
                                      (get-system-fecha-creacion system)
                                      )
                          )))

;; Se crea esta función para verificar si contiene asteriscos.
(define contiene-asterisco (lambda (fileName)
                             (string-contains? fileName "*")
                             ))

;; Se comprueba que existe
(define existe-archivo-en-directorio (lambda (files path fileName)
                                       (if 
                                        ;; Se filtra por la ruta primero
                                        ;; y luego se obtiene el nombre
                                        (null? (filter
                                                (lambda (archivo)
                                                  (string=? (get-name-folder archivo)
                                                            fileName)
                                                  )
                                                (filter
                                                 (lambda (archivo)
                                                   (string=?
                                                    (get-path archivo)
                                                    path
                                                    ))
                                                 files)
                                                )
                                               )
                                        #f
                                        #t
                                        )
                                       ))

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
                                                      (not (string=?
                                                            ((get-path-complete system) fileName)
                                                            (string-append (get-path archivo) (if (string=? fileName "/") (get-name-folder archivo) (string-append "/" (get-name-folder archivo))))
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
                                          (not (string=?
                                                ((get-path-complete system) fileName)
                                                (string-append (get-path archivo) (if (string=? fileName "/") (get-name-folder archivo) (string-append "/" (get-name-folder archivo))))
                                                )
                                               )
                                          )     
                                        (get-system-folder system)
                                        )
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

;; Función que obtiene el archivo según su nombre actual
(define get-file-in-current-directory (lambda (drive source path)
                                        (filter
                                         (lambda
                                             (archivo)
                                            (eq? source (get-name-folder archivo))  
                                           )
                                         drive
                                         )
                                        ))

;; Función para modificar el path de un archivo
(define cambiar-path-archivo (lambda (archivo user path)
                               (make-file (new-file (get-file-name archivo) (get-file-extension archivo) (get-file-content archivo) (get-file-security-atributes archivo)) user path)
                               ))
                               

;; Función insertar archivo
(define insertar-archivo (lambda (system)
                           (lambda (folders)
                             (make-system (get-system-name system)
                                          (get-system-usuarios system)
                                          ;; Se obtienen todo el resto de drives y se crea nuevamente el drive modificado
                                          (cons (get-rest-drives (get-system-drive system) null (get-system-current-drive system))
                                                (cons (make-drive
                                                       (get-system-current-drive system)
                                                       (get-name-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                       (cons folders
                                                             (get-system-folder system))
                                                       (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                       ) null)
                                                )
                                          (get-system-current-user system)
                                          (get-system-current-drive system)
                                          (get-system-path system)
                                          ;; Se crea la carpeta nueva
                                          (cons folders (get-system-folder system))
                                          (get-system-fecha-creacion system)
                                          )
                             )
                           )
  )

(define insertar-conjunto-de-carpetas-y-archivos (lambda (system)
                           (lambda (folders)
                             (make-system (get-system-name system)
                                          (get-system-usuarios system)
                                          ;; Se obtienen todo el resto de drives y se crea nuevamente el drive modificado
                                          (cons (get-rest-drives (get-system-drive system) null (get-system-current-drive system))
                                                (cons (make-drive
                                                       (get-system-current-drive system)
                                                       (get-name-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                       (append folders
                                                             (get-system-folder system))
                                                       (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                       ) null)
                                                )
                                          (get-system-current-user system)
                                          (get-system-current-drive system)
                                          (get-system-path system)
                                          ;; Se crea la carpeta nueva
                                          (append folders
                                                             (get-system-folder system))
                                          (get-system-fecha-creacion system)
                                          )
                             )
                           )
  )

;; Obtener todas las carpetas y directorios de una ruta
(define get-all-files-in-subdirectories (lambda (files source subdirectories)
                                          (define get-all-files-in-subdirectories-int (lambda (files source subdirectories endFiles)
                                                      (if (null? subdirectories)
                                                          endFiles
                                                          (get-all-files-in-subdirectories-int
                                                           ;; Se hace un filtro para obtener todos los archivos menos los que estén dentro de la carpeta
                                                           files
                                                           source
                                                           (cdr subdirectories)
                                                           (append 
                                                                    
                                                            ;; Se filtra a todos los archivos que tengan el path de la carpeta que se quiere eliminar
                                                                  (filter
                                                                   (lambda (archivo)
                                                                     (string=?
                                                                      (car subdirectories)
                                                                      (get-path archivo)
                                                                      )
                                                                     )
                                                                   files
                                                                   ) endFiles )
                                                           )
                                                          )
                                                                         ))
                                          ;; Une la carpeta original que se copiara con los subdirectorios
                                          (append (filter (lambda (archivo)
                                                            (string=? (get-name-folder archivo)
                                                                      source)
                                                            ) files) (get-all-files-in-subdirectories-int files source subdirectories null))
                                          )
  )

;; Cambia el path de todo un conjunto de archivos
(define cambiar-path-carpetas (lambda (files path)
                                (if (null? files)
                                    null
                                    (if (string=? (get-type-file (car files)) "folder")
                                        ;; En caso de que sea una carpeta se crea una carpeta
                                        (cons
                                         (make-new-folder
                                          (get-name-folder (car files))
                                          (get-user-in-folder (car files))
                                          (string-append (if (equal? path "/") (substring path 1 (string-length path)) path) (get-path (car files)))
                                          (get-folder-security-atributes (car files))
                                          )
                                         (cambiar-path-carpetas (cdr files) path)
                                         )
                                        ;; En caso de que sea un archivo
                                        (cons
                                         (make-file
                                          (new-file (get-file-name (car files)) (get-file-extension (car files)) (get-file-content (car files)) (get-file-security-atributes (car files)))
                                          (get-user-in-file (car files))
                                          (string-append (if (equal? path "/") (substring path 1 (string-length path)) path) (get-path (car files)))
                                          ) 
                                         (cambiar-path-carpetas (cdr files) path))
                                        )
                                    )
                                ))

                             
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

;; Esta función es intermediara entre 
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
(define S15 ((run S14 switch-drive) #\C))
(define S16 ((run S15 switch-drive) #\D))
(define S17 ((run S16 md) "Folder"))
(define S18 ((run S17 md) "Folder2"))
(define S19 ((run S18 md) "Folder2"))
(define S20 ((run S19 switch-drive) #\C))
(define S21 ((run S20 md) "Folder"))
(define S22 ((run S21 md) "Folder2"))
S22
(define S23 ((run S22 md) "Folder2"))
S23
(define S24 ((run S23 cd) "../Folder2"))
S24
(define S25 ((run S24 md) "Folder2"))
S25
(define S26 ((run S25 cd) "Folder2"))
S26
(define S27 ((run S26 md) "Folder3"))
S27
(define S28 ((run S27 cd) "Folder3"))
S28
(define S29 ((run S28 cd) "D:/Folder"))
S29
(define S30 ((run S29 add-file) (file "foo1.txt" "txt" "hello world 1")))
S30
(define S31 ((run S30 cd) "../"))
S31
(define S32 ((run S31 add-file) (file "Folder" "txt" "hello world 1")))
S32
(define S33 ((run S32 cd) "Folder2"))
S33
(define S34 ((run S33 add-file) (file "foofighters.txt" "txt" "hello world 1")))
S34
(define S35 ((run S34 md) "Folder32"))
S35
(define S36 ((run S35 cd) "../"))
S36
(define S37 ((run S36 del) "/Folder2"))
S37
(define S38 ((run S37 cd) "Folder"))
S38
(define S39 ((run S38 del) "foo1.txt"))
S39
(define S40 ((run S39 del) "foo2.txt"))
S40
(define S41 ((run S40 del) "*.*"))
S41
(define S42 ((run S41 cd) "/"))
S42
(define S43 ((run S42 del) "*.*"))
S43
(define S44 ((run S43 md) "home"))
S44
(define S45 ((run S44 md) "home seba"))
S45
(define S46 ((run S45 add-file) (file "hola.txt" "txt" "hello world 1")))
(define S47 ((run S46 add-file) (file "hola.txz" "txz" "hello world 1")))
S47
(define S48 ((run S47 md) "hola"))
S48
(define S49 ((run S48 del) "hola.*"))
S49
(define S50 ((run S49 rd) "hola"))
S50
(define S51 ((run S50 cd) "home seba"))
S51
(define S52 ((run S51 add-file) (file "foo1.txt" "txt" "hello world 1")))
S52
(define S53 ((run S52 cd) "../"))
S53
(define S54 ((run S53 rd) "home"))
S54
(define S55 ((run S54 cd) "/home seba"))
S55
(define S56 ((run S55 copy) "foo1.txt" "/"))
S56
(define S57 ((run S56 cd) "../"))
S57
(define S58 ((run S57 md) "var"))
S58
(define S59 ((run S58 cd) "var"))
S59
(define S60 ((run S59 md) "backups"))
S60
(define S61 ((run S60 md) "cache"))
S61
(define S62 ((run S61 md) "crash"))
S62
(define S63 ((run S62 md) "lib"))
S63
(define S64 ((run S63 md) "local"))
S64
(define S65 ((run S64 cd) "backups"))
S65
(define S66 ((run S65 md) "alternatives.tar.0"))
S66
(define S67 ((run S66 add-file) (file "alternatives.tar.1.gz" "gz" "hello world 1")))
(define S68 ((run S67 add-file) (file "alternatives.tar.2.gz" "gz" "hello world 1")))
S68
(define S69 ((run S68 cd) "/"))
S69
(define S70 ((run S69 copy) "var" "C:/Folder"))
S70
(define S71 ((run S70 copy) "var" "C:/"))
S71
(define S72 ((run S71 copy) "var" "C:/FOLDER"))
S72
