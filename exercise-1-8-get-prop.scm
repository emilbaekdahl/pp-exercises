; Exercise 1.8: The get-prop function for property lists

(define (get-prop key plist)
  (if (= (car plist) key)
      (car (cdr plist))
      (get-prop key (cdr (cdr plist)))))

(define weekday-plist
  (list 'monday 1 'tuesday 2 'wednesday 3 'thursday 4 'friday 5 'saturday 6 'sunday 7))
(get-prop 'wednesday weekday-plist)
