#lang racket

(require "main_21286604_CassoneGonzalez.rkt")
;; (require "funciones_21286604_CassoneGonzalez.rkt")
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
(define S73 ((run S72 move) "var" "C:/"))
S73
(define S74 ((run S73 cd) "/"))
S74
(define S75 ((run S74 ren) "var" "usr"))
S75
(define S76 ((run S75 ren) "foo1.txt" "foo2.txt"))
S76
(define S77 ((run S76 cd) "/home seba"))
S77
(define S78 ((run S77 ren) "foo1.txt" "foo2.txa"))
S78
(define S79 ((run S78 dir) "/s /a"))
S79
(define S80 ((run S78 cd) "/"))
S80
(display ((run S80 dir) "/s /o N"))
(display "\n")
(display ((run S80 dir) "/s"))
(display "\n")
(display ((run S80 dir) "/s /a"))
(display "\n")
(display ((run S80 dir) "/s /a /o N"))
(display "\n")
(display ((run S80 dir) "/?"))

(define S81 ((run S80 format) #\C "perro"))
S81
(define S82 ((run S81 encrypt) plus-one minus-one "1234" "foo2.txt"))
S82
(define S83 ((run S82 encrypt) plus-one minus-one "1234" "usr"))
S83
(define S84 ((run S83 decrypt) "1234" "foo2.txt"))
S84
(define S85 ((run S84 decrypt) "1234" "usr"))
S85
(display ((run S85 grep) "orld" "."))
(display ((run S85 grep) "orld" "foo2.txt"))
(display "\n")
(display (run S85 view-trash))
(display "\n")
(display "\n")
(display ((run S85 restore) "*.*"))
(display "\n")
(display "\n")
(display ((run S85 restore) "foofighters.txt"))
