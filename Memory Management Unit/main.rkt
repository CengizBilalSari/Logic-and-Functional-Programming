;cengiz bilal sari
;2021400201
;compiling: yes
;complete: yes
#lang racket

(provide (all-defined-out))

; first  function : Convert a binary to a decimal number
(define (binary_to_decimal binary)
  (define (binary-helper lst exponent)
    (if (null? lst) ; base case is empty list
        0
        (+ (* (car lst) (expt 2 exponent)); calculate the value of the current bit and add the result of the rest
           (binary-helper (cdr lst) (- exponent 1)))))
  ; convert the binary string to a list of numbers and call the helper function with the binary list and the initial exponent
  (let ((binary-list (map (lambda (x) (string->number (string x))) (string->list binary))))
    (binary-helper binary-list (- (length binary-list) 1))))

; second  function: Relocate binary addresses within given limit and base
(define (relocator args limit base)
  (define (map-address address)
    (let ((decimal-address (binary_to_decimal address)))
      (if (> decimal-address limit);check if the decimal address exceeds the limit, if so, return -1, else add base
          -1
          (+ base decimal-address))))
  (map map-address args)); map the function to each address in the list

; third  function: Divide a binary address into page number and page offset
(define (divide_address_space num page_size)
    (let* (
         ;calculate the length of the page number
         ;extract the page number and page offset from the address
         (page-number-length (inexact->exact (- (string-length num) (log (* page_size 1024) 2))))
         (page-number (substring num 0 page-number-length))
         (page-offset (substring num page-number-length)))
    (list page-number page-offset))) ; return the page number and page offset as a list

; fourth  function: translate logical addresses to physical addresses using page table and page size
(define (page args page_table  page_size)
  (define (translate-address logical-address)
     (let* (
            ;Divide the logical address into page number and page offset
            (page-split (divide_address_space logical-address page_size))
            (page-number (car page-split))
           (page-offset (cadr page-split))
           ; take the frame number from page table
           (frame-number (list-ref page_table (binary_to_decimal page-number) ))

            )
       ;concatenate the frame number with the page offset
      (string-append  frame-number page-offset))) ;
  ; map the translation function to each address in the list
  (map translate-address args)  ;
  )

; fifth function: Calculate the sine of x using a Taylor series expansion up to num terms
(define (find_sin value num)
   (define (factorial n)
    (if (= n 0)
        1
        (* n (factorial (- n 1)))))

  (define (power base exp)
    (if (= exp 0)
        1
        (* base (power base (- exp 1)))))
  
  (define (degrees-to-radians degrees)
  (* degrees (/ pi 180)))
  
     
  (define (sin-term n)
    (let ((x-radians (degrees-to-radians value)))
    (/ (power x-radians (- (* 2 n) 1)) (factorial (- (* 2 n) 1)))))
  
   (define (taylor-series n)
    (if (= n 0)
        0
        (+ (* (sin-term n) (if (even? n) -1 1)) (taylor-series (- n 1)))))


    (taylor-series num))

; sixth function: hash a binary address
(define (myhash arg table_size)
   (define (sum-of-digits n)
    (if (< n 10)
        n
        (+ (remainder n 10)
           (sum-of-digits (quotient n 10)))))
  (let*(
        (decimal-value (binary_to_decimal arg));convert the binary number to decimal
        (n (+ 1 ( modulo decimal-value 5))) ; calculate n as the remainder of decimal value modulo 5 plus 1
        (sin-value (* 10000000000.0 (find_sin decimal-value n))) ;Find the sin value and multiply it for taking first 10 bits
        (new-sin-value (inexact->exact (floor sin-value)))
        (sum-of-them (sum-of-digits new-sin-value)) ; sum the first 10 bits of sine value
        (modulo-of-them (modulo sum-of-them table_size)

        ))
    modulo-of-them))

;seventh function: translate a hashed logical address to a physical address using a hashed page table
(define (hashed_page arg table_size page_table page_size)
  ; helper function to access an element in a nested list
  (define (access-element list x y)
  (list-ref (car (list-ref list x)) y))
   (let*(
         ;Divide the logical address into page number and page offset
         (page-split (divide_address_space arg page_size))
         (page-number (car page-split))
         (page-offset (cadr page-split))
         ; calculate the hash value
         (hash-value (myhash page-number table_size))
         ; find matching values in the page table
         (matching-values (filter (lambda (pair)
                                  (string=? (car pair) page-number))
                                  (list-ref page_table hash-value))))
     ; concatenate the frame number with the page offset
     (string-append (cadar matching-values) page-offset)))

;  eighth function: split a string of addresses into parts
(define (split_addresses args size)
   (define (split-helper args size result) ; helper function to recursively split the string
    (if (< (string-length args) size)
        (reverse result); since result in the reversed order
        (let ((part (list(substring args 0 size)))
              (rest (substring args size))
              )
          (split-helper rest size (cons part result)))))
  (split-helper args size '()))

; nighth function: map logical addresses to physical addresses using a hashed page table
(define (map_addresses args table_size page_table page_size address_space_size)
    (let*(
        (logical-addresses (split_addresses args address_space_size))
       (hashed-page-helper (lambda (addr); helper method to apply hashed-page to each addresses
                              (hashed_page (car addr) table_size page_table page_size)) 
        ))
    (map hashed-page-helper logical-addresses)));map the hashed-page helper function to each logical address
