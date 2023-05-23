#lang racket


(provide 
system 
make-system 
get-system-name 
get-system-usuarios
get-system-drive
get-system-current-user
get-system-current-drive
get-system-path
get-system-folder
get-system-trash
get-system-fecha-creacion
get-letters-all-drives
get-all-existing-paths
get-all-existing-paths-decrypt
get-path-complete
get-subdirectory-of-folder
get-file-in-current-directory
get-all-files-in-subdirectories
insertar-conjunto-de-carpetas-y-archivos
insertar-archivo
insertar-conjunto-de-carpetas-y-archivos-encrypt
insertar-archivo-encrypt
file new-file make-file get-file-name get-file-extension get-file-content get-file-security-atributes get-file-password-encrypt get-file-decrypt-fn get-user-in-file get-type-file get-extension-file cambiar-nombre-a-archivo cambiar-path-archivo contiene-asterisco contiene-punto existe-archivo-en-directorio encriptar-archivo desencriptar-archivo buscar-posicion-subcadena make-folder make-new-folder get-folder-security-atributes get-name-folder get-user-in-folder get-folder-password-encrypt get-folder-decrypt-fn get-path cambiar-path-carpetas cambiar-path-renombrar-carpeta cambiar-nombre-a-carpeta verificar-carpetas-duplicadas encriptar-carpetas desencriptar-carpetas
make-drive get-system-folder-drive get-rest-drives get-drive get-name-drive get-capacity-drive
)

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
;; Constructor de TDA System
;; Se inicializa el sistema
(define system (lambda (name) (make-system name '() '() "" "" "" '() '() (current-seconds))))
;; Se extrae como una función aparte el constructor del sistema ya que será utilizado por otras funciones más.
(define make-system (lambda
                        (name users drives current-user current-drive path folders trash seconds)
                      (list name users drives current-user current-drive path folders trash seconds (current-seconds))
                      ))

; Selectores del TDA System
;; Obtener de la lista de usuarios el usuario actual
;; Se obtiene el nombre del sistema, usuarios, drives, current-user, current-drive, fecha-creación
(define get-system-name (lambda (sistema) (list-ref sistema 0)))
(define get-system-usuarios (lambda (sistema) (list-ref sistema 1)))
(define get-system-drive (lambda (sistema) (list-ref sistema 2)))
(define get-system-current-user (lambda (sistema) (list-ref sistema 3)))
(define get-system-current-drive (lambda (sistema) (list-ref sistema 4)))
(define get-system-path (lambda (sistema) (list-ref sistema 5)))
(define get-system-folder (lambda (sistema) (list-ref sistema 6)))
(define get-system-trash (lambda (sistema) (list-ref sistema 7)))
(define get-system-fecha-creacion (lambda (sistema) (list-ref sistema 8)))

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
                                                                                       (get-system-trash sistema)
                                                                                       (get-system-fecha-creacion sistema))
                                                                          (cons (car (car (get-system-drive sistema))) letras)
                                                                          )
                                                                         )
                                                                     ))
                                (get-letters-all-drives-int sistema null)
                                ))

;; Esta función obtiene todas las rutas existentes
(define get-all-existing-paths (lambda (folders)
                                 (cons "/" (map (lambda (folder) (string-append (get-path folder) (if (eq? (get-path folder) "/") (get-name-folder folder) (string-append "/" (get-name-folder folder))))) (filter (lambda (folder) (eq? (get-type-file folder) "folder")) folders)))
                                 ))

(define get-all-existing-paths-decrypt (lambda (folders)
                                 (cons "/" (map (lambda (folder) (string-append (get-path folder) (if (eq? (get-path folder) "/") (minus-one (get-name-folder folder)) (string-append "/" (minus-one (get-name-folder folder)))))) (filter (lambda (folder) (eq? (get-type-file folder) "folder")) folders)))
                                 ))

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

;; Función que obtiene el archivo según su nombre actual
(define get-file-in-current-directory (lambda (drive source path)
                                        (filter
                                         (lambda
                                             (archivo)
                                            (and (string=? source (get-name-folder archivo)) (string=? path (get-path archivo)))
                                           )
                                         drive
                                         )
                                        ))

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
                                          (get-system-trash system)
                                          (get-system-fecha-creacion system)
                                          )
                             )
                           )
  )

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
                                          (get-system-trash system)
                                          (get-system-fecha-creacion system)
                                          )
                             )
                           )
  )

(define insertar-conjunto-de-carpetas-y-archivos-encrypt (lambda (system)
                           (lambda (folders folderName)
                             (make-system (get-system-name system)
                                          (get-system-usuarios system)
                                          ;; Se obtienen todo el resto de drives y se crea nuevamente el drive modificado
                                          (cons (get-rest-drives (get-system-drive system) null (get-system-current-drive system))
                                                (cons (make-drive
                                                       (get-system-current-drive system)
                                                       (get-name-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                       (filter
                                                        (lambda (archivo) (not (member archivo (get-all-files-in-subdirectories
                                                                                                                (get-system-folder system)
                                                                                                                (plus-one folderName)
                                                                                                                (get-subdirectory-of-folder
                                                                                                                 (string-append (if (eq? (get-system-path system) "/") (get-system-path system) (string-append (get-system-path system) "/")) folderName)
                                                                                                                 (get-all-existing-paths-decrypt (get-system-folder system)))
                                                                                                                ))))
                                                        (append folders (get-system-folder system)))
                                                       (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                       ) null)
                                                )
                                          (get-system-current-user system)
                                          (get-system-current-drive system)
                                          (get-system-path system)
                                          ;; Se crea la carpeta nueva
                                          (filter
                                           (lambda (archivo) (not (member archivo (get-all-files-in-subdirectories
                                                                                                                (get-system-folder system)
                                                                                                                (plus-one folderName)
                                                                                                                (get-subdirectory-of-folder
                                                                                                                 (string-append (if (eq? (get-system-path system) "/") (get-system-path system) (string-append (get-system-path system) "/")) folderName)
                                                                                                                 (get-all-existing-paths-decrypt (get-system-folder system)))
                                                                                                                ))))
                                           (append folders (get-system-folder system)))
                                          (get-system-trash system)
                                          (get-system-fecha-creacion system)
                                          )
                             )
                           )
  )

(define insertar-archivo-encrypt (lambda (system)
                           (lambda (folders folderName)
                             (make-system (get-system-name system)
                                          (get-system-usuarios system)
                                          ;; Se obtienen todo el resto de drives y se crea nuevamente el drive modificado
                                          (cons (get-rest-drives (get-system-drive system) null (get-system-current-drive system))
                                                (cons (make-drive
                                                       (get-system-current-drive system)
                                                       (get-name-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                       (filter (lambda (archivo) (not (equal? archivo folderName)))
                                                        (cons folders (get-system-folder system)))
                                                       (get-capacity-drive (get-drive (get-system-drive system) (get-system-current-drive system)))
                                                       ) null)
                                                )
                                          (get-system-current-user system)
                                          (get-system-current-drive system)
                                          (get-system-path system)
                                          ;; Se crea la carpeta nueva
                                          (filter (lambda (archivo) (not (equal? archivo folderName)))
                                                        (cons folders (get-system-folder system)))
                                          (get-system-trash system)
                                          (get-system-fecha-creacion system)
                                          )
                             )
                           )
  )


;; Se constuye un archivo
(define file (lambda
                 (name extension content . security-atributes)
               (list security-atributes content extension (current-seconds) (current-seconds) name)))

(define new-file (lambda
                 (name extension content security-atributes)
               (list security-atributes content extension (current-seconds) (current-seconds) name)))

;; Se le da forma del TDA
(define make-file (lambda
                      (archivo password decryptFn user-creator path)
                    (reverse (cons path (cons "file" (cons user-creator (cons decryptFn (cons password archivo))))))
                    ))


;; Selectores de TDA Archivo
(define get-file-name (lambda (archivo)
                        (list-ref archivo 0)))

(define get-file-extension (lambda (archivo)
                             (list-ref archivo 3)))

(define get-file-content (lambda (archivo)
                                (list-ref archivo 4)))

(define get-file-security-atributes (lambda (archivo)
                           (list-ref archivo 5)))

(define get-file-password-encrypt (lambda (archivo)
                                    (list-ref archivo 6)))

(define get-file-decrypt-fn (lambda (archivo)
                                    (list-ref archivo 7)))

 ;; Obtener el nombre de usuario en un archivo
(define get-user-in-file (lambda (archivo)
                           (list-ref archivo 8)))

(define get-type-file (lambda (file-list)
                        (cond
                          ((null? (cddr file-list)) (car file-list))
                          (else (get-type-file (cdr file-list)))
                          )
                        ))

;; Obtiene la extensión de un archivo
(define get-extension-file (lambda (archivo)
                                    (list-ref archivo 3)))

;; Cambiar nombre a un archivo, el path ya fue modificado
(define cambiar-nombre-a-archivo (lambda (archivo name)
                                   (make-file
                                    (new-file
                                     name
                                     (if (contiene-punto name) (string-append "." (last (string-split name "."))) (get-file-extension archivo))
                                     (get-file-content archivo)
                                     (get-file-security-atributes archivo)
                                     )
                                    (get-file-password-encrypt archivo)
                                    (get-file-decrypt-fn archivo)
                                    (get-user-in-file archivo)
                                    (get-path archivo)
                                    )
                                   ))

;; Función para modificar el path de un archivo
(define cambiar-path-archivo (lambda (archivo user path)
                               (make-file (new-file (get-file-name archivo) (get-file-extension archivo) (get-file-content archivo) (get-file-security-atributes archivo)) (get-file-password-encrypt archivo) (get-file-decrypt-fn archivo) user path)
                               ))

;; Se crea esta función para verificar si contiene asteriscos.
(define contiene-asterisco (lambda (fileName)
                             (string-contains? fileName "*")
                             ))

;; Se crea esta función para verificar si contiene punto.
(define contiene-punto (lambda (fileName)
                             (string-contains? fileName ".")
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

(define encriptar-archivo (lambda (archivo encryptFn minus-one password) 
                            (make-file
                             (new-file
                              (encryptFn (get-file-name archivo))
                              (encryptFn (get-file-extension archivo))
                              (encryptFn (get-file-content archivo))
                              (get-file-security-atributes archivo)
                              )
                             password
                             minus-one
                             (get-user-in-file archivo)
                             (get-path archivo)
                             )      
                            ))

(define desencriptar-archivo (lambda (archivo) 
                            (make-file
                             (new-file
                              ((get-file-decrypt-fn archivo) (get-file-name archivo))
                              ((get-file-decrypt-fn archivo) (get-file-extension archivo))
                              ((get-file-decrypt-fn archivo) (get-file-content archivo))
                              (get-file-security-atributes archivo)
                              )
                             ""
                             ""
                             (get-user-in-file archivo)
                             (get-path archivo)
                             )      
                            ))

(define buscar-posicion-subcadena (lambda (cadena subcadena)
  (define buscar-posicion-iter (lambda (inicio)
    (cond ((>= inicio (- (string-length cadena) (string-length subcadena)))
           #f)  ; No se encontró coincidencia
          ((string=? subcadena (substring cadena inicio (+ inicio (string-length subcadena)))) inicio)
             ; Se encontró coincidencia en la posición 'inicio'
          (else
           (buscar-posicion-iter (+ inicio 1)))
          )
                                 )) ; Seguir buscando en la siguiente posición

  (buscar-posicion-iter 0)))


;; Se construye una carpeta
(define make-folder (lambda
                        (name password decryptFn user-creator path . securityAtributes)
                      (list name (current-seconds) (current-seconds) securityAtributes password decryptFn user-creator "folder" path)
                      ))

(define make-new-folder (lambda
                        (name password decryptFn user-creator path securityAtributes)
                      (list name (current-seconds) (current-seconds) securityAtributes password decryptFn user-creator "folder" path)
                      ))

;; Selectores de TDA Carpeta
(define get-folder-security-atributes (lambda (folder)
                           (list-ref folder 3)))


;; Obtiene el nombre de una carpeta
(define get-name-folder (lambda (folder)
                          (list-ref folder 0)))

;; Obtener el nombre de usuario en una carpeta
(define get-user-in-folder (lambda (folder)
                             (list-ref folder 6)))

(define get-folder-password-encrypt (lambda (archivo)
                                    (list-ref archivo 4)))

(define get-folder-decrypt-fn (lambda (archivo)
                                    (list-ref archivo 5)))

                                    ;; Obtener path de una carpeta
(define get-path (lambda (folder)
                   (last folder)
                   ))  

;; Cambia el path de todo un conjunto de archivos
(define cambiar-path-carpetas (lambda (files path)
                                (if (null? files)
                                    null
                                    (if (string=? (get-type-file (car files)) "folder")
                                        ;; En caso de que sea una carpeta se crea una carpeta
                                        (cons
                                         (make-new-folder
                                          (get-name-folder (car files))
                                          (get-folder-password-encrypt (car files))
                                          (get-folder-decrypt-fn (car files))
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
                                          (get-file-password-encrypt (car files))
                                          (get-file-decrypt-fn (car files))
                                          (get-user-in-file (car files))
                                          (string-append (if (equal? path "/") (substring path 1 (string-length path)) path) (get-path (car files)))
                                          ) 
                                         (cambiar-path-carpetas (cdr files) path))
                                        )
                                    )
                                ))

;; Cambia el path cambiando solo el nombre de la carpeta
(define cambiar-path-renombrar-carpeta (lambda (files source path)
                                (if (null? files)
                                    null
                                    (if (string=? (get-type-file (car files)) "folder")
                                        ;; En caso de que sea una carpeta se crea una carpeta
                                        (cons
                                         (make-new-folder
                                          (get-name-folder (car files))
                                          (get-folder-password-encrypt (car files))
                                          (get-folder-decrypt-fn (car files))
                                          (get-user-in-folder (car files))
                                          (if
                                           (string-contains? (get-path (car files)) source)
                                           (string-replace (get-path (car files)) source (if (equal? path "/") (substring path 1 (string-length path)) path))
                                           (get-path (car files)))
                                          (get-folder-security-atributes (car files))
                                          )
                                         (cambiar-path-renombrar-carpeta (cdr files) source path)
                                         )
                                        ;; En caso de que sea un archivo
                                        (cons
                                         (make-file
                                          (new-file (get-file-name (car files)) (get-file-extension (car files)) (get-file-content (car files)) (get-file-security-atributes (car files)))
                                          (get-file-password-encrypt (car files))
                                          (get-file-decrypt-fn (car files))
                                          (get-user-in-file (car files))
                                          (if
                                           (string-contains? (get-path (car files)) source)
                                           (string-replace (get-path (car files)) source (if (equal? path "/") (substring path 1 (string-length path)) path))
                                           (get-path (car files)))
                                          ) 
                                         (cambiar-path-renombrar-carpeta (cdr files) source path))
                                        )
                                    )
                                ))     

;; Cambia el nombre a una carpeta al renombrarla, el path ya fue cambiado.
(define cambiar-nombre-a-carpeta (lambda (files oldName name)
                                   (map
                                    (lambda (archivo)
                                      (if (string=? (get-name-folder archivo) oldName)
                                          (make-new-folder
                                           name
                                           (get-folder-password-encrypt archivo)
                                           (get-folder-decrypt-fn archivo)
                                           (get-user-in-folder archivo)
                                           (get-path archivo)
                                           (get-folder-security-atributes archivo)
                                           )
                                          archivo)
                                      )
                                    files)
                                   ))
                                   
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

(define encriptar-carpetas (lambda (files encryptFn minus-one password)
                                   (map
                                    (lambda (archivo)
                                      (if (string=? (get-type-file archivo) "folder")
                                          (make-new-folder
                                           (encryptFn (get-file-name archivo))
                                           password
                                           minus-one
                                           (get-user-in-folder archivo)
                                           (get-path archivo)
                                           (get-folder-security-atributes archivo)
                                           )
                                          (make-file
                                           (new-file
                                            (encryptFn (get-file-name archivo))
                                            (encryptFn (get-file-extension archivo))
                                            (encryptFn (get-file-content archivo))
                                            (get-file-security-atributes archivo)
                                            )
                                           password
                                           minus-one
                                           (get-user-in-file archivo)
                                           (get-path archivo)
                                           )
                                      )
                             )
                             files)
                             ))

(define desencriptar-carpetas (lambda (files)
                                   (map
                                    (lambda (archivo)
                                      (if (string=? (get-type-file archivo) "folder")
                                          (make-new-folder
                                           ((get-folder-decrypt-fn archivo) (get-file-name archivo))
                                           ""
                                           ""
                                           (get-user-in-folder archivo)
                                           (get-path archivo)
                                           (get-folder-security-atributes archivo)
                                           )
                                          (make-file
                                           (new-file
                                            ((get-file-decrypt-fn archivo) (get-file-name archivo))
                                            ((get-file-decrypt-fn archivo) (get-file-extension archivo))
                                            ((get-file-decrypt-fn archivo) (get-file-content archivo))
                                            (get-file-security-atributes archivo)
                                            )
                                           ""
                                           ""
                                           (get-user-in-file archivo)
                                           (get-path archivo)
                                           )
                                      )
                             )
                             files)
                             ))                                

;; Se construye una unidad para la lista de drives
(define make-drive (lambda
                       (letter name folders capacity) (list letter name folders capacity)))

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
