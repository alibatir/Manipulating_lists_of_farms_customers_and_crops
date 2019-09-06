#lang scheme
; 2015400261

(define FARMS '(
(farmA 100 (apricot apple blueberry))
(farmB 90 (broccoli carrot grape))
(farmC 75 (corn grape lemon))
(farmD 75 ())
(farmE 45 (lemon melon olive berry))
(farmF 70 (lemon carrot))
(farmG 50 (olive))
(farmH 150 (olive grape apple))
(farmI 50 (apple))
(farmJ 45 (ginger beans garlic))
(farmK 70 (spinach onion peas))
(farmL 50 ())
(farmM 150 (spinach melon potato tomato))
(farmN 50 (spinach corn))
(farmO 50 (spinach))
))
(define CUSTOMERS '(
(john (farmA farmC) (apricot lemon))
(james (farmB farmC) (grape corn))
(arya (farmB farmD) (grape broccoli))
(elenor () ())
(alan (farmG farmH) (olive apple))
(george (farmF farmE farmG) (lemon melon olive apple))
(cersei (farmE farmF farmH farmI) (lemon olive apple))
(jon (farmA farmB farmC farmD farmE farmF farmG farmH farmI) (apricot apple blueberry broccoli
carrot grape corn lemon melon olive berry))
(sophia (farmK farmN farmO) (spinach onion corn))
(liam (farmM farmN farmO) (spinach melon potato))
(emma (farmD farmH farmK farmA) (apricot grape spinach))
(river (farmG farmB farmI farmA) (apple broccoli olive))
(lucas (farmK farmJ farmA farmD farmM farmN) (spinach onion apple))
(oliver (farmE farmF farmG) (lemon olive))
(zoe () ())
))
(define CROPS '(
(apricot farmA 10)
(apple farmA 12)
(melon farmE 22)
(olive farmE 40)
(berry farmE 10)
(lemon farmF 35)
(carrot farmF 5)
(olive farmG 60)
(olive farmH 30)
(blueberry farmA 15)
(broccoli farmB 8)
(carrot farmB 5)
(grape farmB 10)
(corn farmC 9)
(grape farmC 12)
(lemon farmC 10)
(lemon farmE 12)
(grape farmH 10)
(apple farmH 8)
(apple farmI 8)
(ginger farmJ 10)
(beans farmJ 13)
(garlic farmJ 15)
(spinach farmK 5)
(onion farmK 9)
(peas farmK 8)
(spinach farmM 10)
(melon farmM 15)
(potato farmM 9)
(tomato farmM 9)
(spinach farmN 7)
(corn farmN 9)
(spinach farmO 6)
))

;TRANSPORTATION-COST:
(define (TRANSPORTATION-COST x)
  (find_tc x FARMS))
(define find_tc
  (lambda (x FARMS)
    (if (empty? FARMS) '0 (if (equal? x (caar FARMS)) (cadar FARMS) (find_tc x (cdr FARMS))))))

;AVAILABLE-CROPS:
(define (AVAILABLE-CROPS x)
  (find_ac x FARMS))
(define find_ac
  (lambda (x FARMS)
    (if (empty? FARMS) '() (if (equal? x (caar FARMS)) (caddr (car FARMS)) (find_ac x (cdr FARMS))))))

;INTERESTED-CROPS:
(define (INTERESTED-CROPS x)
  (find_ic x CUSTOMERS))
(define find_ic
  (lambda (x CUSTOMERS)
    (if (empty? CUSTOMERS) '() (if (equal? x (caar CUSTOMERS)) (caddar CUSTOMERS) (find_ic x (cdr CUSTOMERS))))))

;CONTRACT-FARMS:
(define (CONTRACT-FARMS x)
  (find_cf x CUSTOMERS))
(define find_cf
  (lambda (x CUSTOMERS)
    (if (empty? CUSTOMERS) '() (if (equal? x (caar CUSTOMERS)) (cadar CUSTOMERS) (find_cf x (cdr CUSTOMERS))))))

;CONTRACT-WITH-FARM:
(define (CONTRACT-WITH-FARM x) 
  (find_cwf x CUSTOMERS))
(define find_cwf
  (lambda (x CUSTOMERS)
    (cond
    ((empty? CUSTOMERS) '())
   ; ((null? (cadar CUSTOMERS)) '())
    ((equal? #t (member x (cadar CUSTOMERS)))
        (cons (car (car CUSTOMERS))     
              (find_cwf x (cdr CUSTOMERS))))  
      (else (find_cwf x (cdr CUSTOMERS))))
    ))

;find the element in the list or not
(define(member atm a_list)
	(cond
          ((null? a_list) #f)
          ((eq? atm (car a_list)) #t)
          (else (member atm (cdr a_list)))))

;INTERESTED-IN-CROP:
(define (INTERESTED-IN-CROP x)
  (find_iic x CUSTOMERS))
(define find_iic
  (lambda (x CUSTOMERS)
    (cond
    ((empty? CUSTOMERS) '())
    ;((null? (caddar CUSTOMERS)) '())
    ((equal? #t (member x (caddar CUSTOMERS)))
        (cons (car (car CUSTOMERS))     
              (find_iic x (cdr CUSTOMERS))))  
      (else (find_iic x (cdr CUSTOMERS))))
    ))

;MIN-SALE-PRICE:
(define (MIN-SALE-PRICE x)
 (find_min (find_msp x CROPS)))
;get all price
(define find_msp
  (lambda (x CROPS)
     (cond
    ((empty? CROPS) '())
    ((empty? (caar CROPS)) '())
    ((equal? x (caar CROPS))
        (cons (car(cdr(cdr (car CROPS))))     
              (find_msp x (cdr CROPS))))  
      (else (find_msp x (cdr CROPS))))
    ))

;get min price in the all price list
(define (find_min lst)
    
       (cond
         ((null? lst) '0)
         ((null? (cdr lst)) (car lst))
          ((< (car lst) (find_min (cdr lst))) (car lst))
          (else (find_min (cdr lst)))))

;CROPS-BETWEEN:
;x:min price y:max price 
(define (CROPS-BETWEEN x y)
 (remove-duplicates (find_cb x y CROPS)))
(define find_cb
  (lambda (x y CROPS)
     (cond
    ((empty? CROPS) '())
    ((empty? (caar CROPS)) '())
    ((and(>= (caddar CROPS) x) (<= (caddar CROPS) y))
        (cons (caar CROPS)     
              (find_cb x y (cdr CROPS))))  
      (else (find_cb x y (cdr CROPS))))))

;remove duplicates in a list
(define (remove-duplicates l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (remove-duplicates (cdr l)))
        (else
         (cons (car l) (remove-duplicates (cdr l))))))

;BUY-PRICE:
(define (BUY-PRICE customer crop)
  ; FİND_BP ((APPRICOT LEMON) (farmA farmC) APPRICOT CROPS)
   (find_min (find_bp (INTERESTED-CROPS customer) (CONTRACT-FARMS customer) crop CROPS))) 
(define find_bp
  (lambda (fruitList farmList crop CROPS)
    (cond
    ((empty? CROPS) '())
    ((empty? (car (car CROPS))) '())
    ((and(equal? crop (car (car CROPS))) (equal? #t (member (cadar CROPS) farmList)) (not(empty? fruitList )) (equal? #t (member crop fruitList)) (not(empty? (TRANSPORTATION-COST(cadar CROPS)) )))
        (cons (+ (caddar CROPS) (TRANSPORTATION-COST(cadar CROPS)))     
              (find_bp fruitList farmList crop (cdr CROPS))))  
      (else (find_bp fruitList farmList crop (cdr CROPS))))
    ))

;TOTAL-PRICE:
(define (TOTAL-PRICE customer)
   (find_tp (INTERESTED-CROPS customer) (CONTRACT-FARMS customer) customer CROPS))
(define find_tp
  (lambda (fruitList farmList customer CROPS)
    (cond
      ((empty? CROPS) '0)
      ((empty? (car (car CROPS))) '0)
     ((and(not(empty? farmList)) (not(empty? fruitList)) (not(empty? (BUY-PRICE customer (caar CROPS)))) (equal? #t (member (caar CROPS) fruitList)) (equal? #t (member (cadar CROPS) farmList)))
       (+ (BUY-PRICE customer (car(car CROPS)))      
              (find_tp (remove (caar CROPS) fruitList) farmList customer (cdr CROPS))))  
      (else (find_tp (remove (caar CROPS) fruitList) farmList customer (cdr CROPS)))))
    )
;remove the fruit in the fruitList 
(define (remove x ls)
  (if (null? ls)
      '()
      (let ((h (car ls)))
        ((if (eqv? x h)
            (lambda (y) y)
            (lambda (y) (cons h y)))
         (remove x (cdr ls))))))
