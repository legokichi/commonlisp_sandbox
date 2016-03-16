(defpackage naive-prime-search)
(in-package naive-prime-search)

;; prime-number?:: Num -> List Num -> Bool
(defun prime-number? (n primes)
  (cond
    ((atom primes)               t)
    ((eq 0 (mod n (car primes))) nil)
    (t                           (prime-number? n (cdr primes)))))

;; search-prime:: Num -> List Num -> Num -> List Num
(defun search-prime (n primes max)
  (cond
    ((> n max)                primes)
    ((prime-number? n primes) (search-prime (1+ n) (append primes (list n)) max))
                              ;; appendを使う事でリスト完全コピー。どうみても遅い
                              ;; もしかしたら末尾再帰最適化されないかも
    (t                        (search-prime (1+ n) primes max))))

(time (search-prime 3 '(2) 100000))
;; Real time: 11.719404 sec.
;; Run time: 11.668685 sec.
;; Space: 736128576 Bytes
;; GC: 906, GC time: 4.034319 sec.



(defpackage optimised-naive-prime-search)
(in-package optimised-naive-prime-search)

;; リストを探索したついでに素数を追加すべき末尾ポインタを返す
;; (prime-number? n primes (cons 0 primes)) のような形で pre-primes には適当なリストを用意する
;; prime-number?:: Num -> List Num -> List Num -> (Bool, List Num)
(defun prime-number? (n primes pre-primes)
  (cond
    ((atom primes)               (cons t pre-primes))
    ((eq 0 (mod n (car primes))) (cons nil pre-primes))
    (t                           (prime-number? n (cdr primes) primes))))

;; search-prime:: Num -> List Num -> Num -> List Num
(defun search-prime (n primes max)
  (if (> n max)
    primes ;; 探索範囲を超えたら終了
    (let* ((tmp (prime-number? n primes (cons 0 primes)))
           (prime? (car tmp)) ;; prime?: boolean
           (ptr (cdr tmp)))   ;; ptr: もし素数ならcdrを書き換えるべきコンスセル
      (if prime?
        (setf (cdr ptr) (cons n nil)))    ;; setfによるコンスセル書き換え。コピーは発生しない
      (search-prime (1+ n) primes max)))) ;; clispでそのまま実行しても末尾再帰最適化されない。clisp -c hoge.lisp してバイトコードにコンパイルしておいてから clisp hoge.fas しよう

(time (search-prime 3 '(2) 100000))
;; Real time: 7.283802 sec.
;; Run time: 7.255682 sec.
;; Space: 3353536 Bytes
;; GC: 5, GC time: 0.022078 sec.
