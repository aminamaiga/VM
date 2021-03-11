;;READ ME
;;Etudiante Aminata MAIGA M1 AIGLE

..........................................Organisation..........................................
Ce projet conforte 3 fichiers 
 1- compiler.lisp => permet de compiler le code lisp en asm qui sera charge dans la VM
    Schémas de compilation
      -Expressions constantes
      -Structures de contrôles
      -Fonctions
      -Compilation d’une déclaration d'une fonction
      -Compilation d’un appel de sous-programme (Paramètres et variables locales)
      -environnement

 2- get-set.lisp => contient les getters et setters des proprites de la VM
    les getters et setters de la sctructure de la VM

 3- vm.lisp => Les instructions de la VM
   -les registres
   -les labels
   -Modes d'adressage
   -Instructions de base
   -Instructions de pile
   -Instructions de saut
   -Instructions diverses

..........................................Installation.........................................
Exemple

(load "compiler.lisp") => charger le code du compilateur
(load "get-set.lisp")  => charger le code des getters and setters
(load "vm.lisp")       => charger le code de la virtual machine

..........................................Excution........................................
Exemple avec la fonction recursive de Fibinacci

(make-vm 'vm) => ;;creation de la vm ;return 0 ou contenu de R0
(compile-load-run 'vm '(defun fibo (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2)))))) => compiler executer la fonction fibo dans la vm ;return 0
(compile-load-run 'vm '(fibo 10)) => appel de fonction de la fonction fibo dans la vm ;return 55

Exemple avec la fonction recursive factorielle
(compile-load-run 'vm '(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) => compiler executer la fonction fact dans la vm
(compile-load-run 'vm '(fact 10)) => appel de fonction de la fonction fact dans la vm


.......................................Compiler........................................
(compiler '(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) => pour compiler ; return asm
