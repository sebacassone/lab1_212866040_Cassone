#lang racket

(require "main.rkt")

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
(define S26 ((run S25 cd) "Folder2"))
(define S27 ((run S26 md) "Folder3"))
(define S28 ((run S27 cd) "Folder3"))
(define S29 ((run S28 cd) "E:/Folder8/Folder12"))