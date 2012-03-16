(load "./chapter02.scm")
(load "./test.scm")
(test '(
        ; ex. 2.2
        (=? '(y-point (make-point 1 9)) 9)
        (=? '(x-point (make-point 1 9)) 1)
        (=? '(showpoint (make-point 5 7)) "5,7")
        (=? '(showsegment (make-segment (make-point 0 0)(make-point 2 2)))
            "0,0:2,2")
        (=? '(showpoint (midpoint-segment (make-segment (make-point 0 0)(make-point 2 2))))
            "1.0,1.0")
        (=? '(showpoint (midpoint-segment (make-segment (make-point -2 -4)(make-point 2 2))))
            "0.0,-1.0")

        ; ex. 2.3
        (=? '(len-segment (make-segment (make-point 0 0)(make-point 2 2))) 2)
        (=? '(showsegment (horiz-rect t-rect1)) "0,2:2,2")
        (=? '(showsegment (vert-rect  t-rect1)) "0,2:0,0")
        (=? '(len-rect t-rect1) 2)
        (=? '(hei-rect t-rect1) 2)
        (=? '(hei-rect (make-rect (make-point 1 5)(make-point 5 1))) 4)
        (=?~ '(area-rect (make-rect
                          (make-point 0 2)(make-point 5 0)))
             10.0)
        (=?~  '(perim-rect (make-rect (make-point 0 2)(make-point 5 0))) 14.0)

        ;ex 2.4
        (=? '(kar (kons 1 2)) 1)
        (=? '(kdr (kons 1 2)) 2)

        ;ex 2.5
        (=? '(which-power? 3 3) 1)
        (=? '(which-power? 9 3) 2)
        (=? '(which-power? 27 3) 3)
        (=? '(which-power? 2 2) 1)
        (=? '(which-power? 4 2) 2)
        (=? '(which-power? 32 2) 5)
        (=? '(which-power? 64 2) 6)
        (=? '(qar (qons 1 2)) 1)
        (=? '(qdr (qons 1 2)) 2)
        (=? '(qar (qons 2 3)) 2)
        (=? '(qdr (qons 2 3)) 3)
        (=? '(qar (qons 5 8)) 5)
        (=? '(qdr (qons 5 8)) 8)

        ; ex 2.17
        (=? '(last-pair (list 4 5 6 7)) 7)
        (=? '(last-pair (list 1)) 1)
        (=? '(last-pair '()) '())

        ; ex 2.18
        (=? '(reverse_ (list 1 2 3 4)) '(4 3 2 1))
        (=? '(reverse_ '()) '())
        (=? '(reverse__ (list 1 2 3 4)) '(4 3 2 1))
        (=? '(reverse__ '()) '())

        ; ex 2.19
        (=? '(cc 100 us-coins) 292)
        (=? '(cc 100 us-rev-coins) 292)

        ; ex 2.20
        (=? '(filter_ even? (list 2 4 6 7)) '(2 4 6))
        (=? '(filter_ odd? (list 1 2 4 6 7)) '(1 7))

        (=? '(same-parity 1 2 3 4 5 6 7) '(1 3 5 7))
        (=? '(same-parity 2 3 4 5 6 7) '(2 4 6))

        ; ex 2.21
        (=? '(square-list_ (list 1 2 3 4)) '(1 4 9 16))
        (=? '(square-list__ (list 1 2 3 4)) '(1 4 9 16))

        ; ex 2.22
        (=? '(square-list3-broken (list 1 2 3 4)) '(16 9 4 1))
        (=? '(square-list3-fixed (list 1 2 3 4)) '(1 4 9 16))
        (=? '(square-list3-fixed_ (list 1 2 3 4)) '(1 4 9 16))

        ; ex 2.23
        (=? '(for-each_ (lambda(x) (* x x)) (list 1 2 3)) '())

        ; ex 2.25
        (=? '(expr1 (list 1 2 (list 5 7) 9)) 7)
        (=? '(expr2 (list (list 7))) 7)
        (=? '(expr3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))) 7)

        ; ex 2.26
        (=? '(append x26 y26) (list 1 2 3 4 5 6))
        (=? '(cons x26 y26) (list (list 1 2 3) 4 5 6))
        (=? '(list x26 y26) (list (list 1 2 3) (list 4 5 6)))

        ; ex 2.27
        (=? '(reverse_ x27) (list (list 3 4) (list 1 2)))
        (=? '(deep-reverse x27) (list (list 4 3) (list 2 1)))

        ; ex 2.28
        (=? '(fringe x28) (list 1 2 3 4))
        (=? '(fringe (list x28 x28)) (list 1 2 3 4 1 2 3 4))

        ; ex 2.29

        (=? '(right-branch (make-mobile 1 2)) 2)
        (=? '(left-branch (make-mobile 1 2)) 1)
        (=? '(is-branch? (make-branch 1 (make-mobile (make-branch 1 1)
                                                    (make-branch 1 1)))) #t)
        (=? '(is-branch? '()) #f)
        (=? '(is-branch? (make-mobile (make-branch 1 1) (make-branch 1 1))) #f)

        (=? '(is-structure? '()) #f)
        (=? '(is-structure? (make-mobile (make-branch 1 1) (make-branch 1 1))) #t)
        (=? '(is-structure? (make-branch 1 (make-mobile (make-branch 1 1)
                                                     (make-branch 1 1)))) #f)

        (=? '(total-weight (make-mobile (make-branch 1 3)
                                        (make-branch 5 (make-mobile (make-branch 3 7)
                                                                    (make-branch 6 11))))) 21)

        (=? '(is-balanced? (make-mobile (make-branch 4 8)
                                        (make-branch 16 2)))
            #t)

        (=? '(is-balanced? (make-mobile (make-branch 4 8)
                                        (make-branch 16 1)))
            #f)

        (=? '(is-balanced? (make-mobile (make-branch 5 (make-mobile (make-branch 2 10)
                                                                    (make-branch 2 10)))
                                        (make-branch 10 10)))
            #t)

        (=? '(is-balanced? (make-mobile (make-branch 5 (make-mobile (make-branch 3 10)
                                                                    (make-branch 1 10)))
                                        (make-branch 10 10)))
            #f)

        ; ex 2.30
        (=? '(skuare-tree (list 1
                                (list 2 (list 3 4) 5)
                                (list 6 7)))
            '(1 (4 (9 16) 25) (36 49)))

        (=? '(scuare-tree (list 1
                                (list 2 (list 3 4) 5)
                                (list 6 7)))
            '(1 (4 (9 16) 25) (36 49)))

        ; ex 2.31
        (=? '(m-square-tree (list 1
                                  (list 2 (list 3 4) 5)
                                  (list 6 7)))
            '(1 (4 (9 16) 25) (36 49)))

        ; ex 2.32
        (=? '(subsets '(1 2 3))
            '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))

        ; accumulate (fold) test
        (=? '(accumulate + 0 '(1 2 3 4 5)) 15)
        (=? '(accumulate * 1 '(1 2 3 4 5)) 120)
        (=? '(accumulate cons '() '(1 2 3 4 5)) '(1 2 3 4 5))

        ; ex 2.33
        (=? '(map32 square '(1 2 3)) '(1 4 9))
        (=? '(append32 '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
        (=? '(length32 '(1 2 3)) 3)

        ; ex 2.43
        (=? '(horner-eval 2 '(1 3 0 5 0 1)) 79)

        ; ex 2.35
        (=? '(count-leaves-acc (list (list 1 2) (list 3 4) (list 5 6)))
            6)
        (=? '(count-leaves-acc (list (list 1 (list 11 12)) 2 (list 3 4) (list 5 6)))
            8)

        ; ex 2.36
        (=? '(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
            (list 22 26 30))

        ; ex 2.37
        (=? '(dot-product (list 1 2 3) (list 3 4 5)) 26)

        ; ex 2.38
        (=? '(accumulate + 0 '(1 2 3)) (foldr + 0 '(1 2 3)))
        (=? '(foldl + 0 '(1 2 3)) (foldr + 0 '(1 2 3)))
        (=?~ '(foldr / 1 '(1 2 3)) 1.5)
        (=?~ '(foldl / 1 '(1 2 3)) 0.166)
        (=? '(foldr list '() '(1 2 3)) (list 1 (list 2 (list 3 '()))))
        (=? '(foldl list '() '(1 2 3)) (list (list (list '() 1) 2) 3))

        ; ex 2.39
        (=? '(fr-rev '(1 2 3 4)) '(4 3 2 1))
        (=? '(fl-rev '(1 2 3 4)) '(4 3 2 1))

        ; ex 2.40
        ; needed functions
        (=? '(prime? 2) #t)
        (=? '(prime? 3) #t)
        (=? '(prime? 23) #t)
        (=? '(prime? 24) #f)
        (=? '(flatmap (lambda(list)(map
                                    (lambda(y) (+ y 1))
                                    list))
                             (list (list 1 2)(list 3 4)))
            '(2 3 4 5))
        (=? '(enumerate-interval 0 0) '(0))
        (=? '(enumerate-interval 0 1) '(0 1))
        (=? '(enumerate-interval 0 5) '(0 1 2 3 4 5))
        (=? '(enumerate-interval -2 2) '(-2 -1 0 1 2))
        (=? '(prime-sum? (list 1 2)) #t)
        (=? '(prime-sum? (list 2 2)) #f)
        (=? '(make-pair-sum (list 1 2)) '(1 2 3))
        (=? '(prime-sum-pairs 2) '((2 1 3)))
        (=? '(remove 1 '(1 2 3)) '(2 3))
        (=? '(permutations '(1 2 3)) '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))
        ; 2.40
        (=? '(unique-pairs 3) '((2 1) (3 1) (3 2)))
))