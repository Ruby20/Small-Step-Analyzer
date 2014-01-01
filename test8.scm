((lambda (n $k31)
   ($k31
    (lambda (m $k32)
      ($k32
       (lambda (f $k33)
         ($k33
          (lambda (z $k34)
            (n
             f
             (lambda ($rv36) (m $rv36 (lambda ($rv35) ($rv35 z $k34))))))))))))
 (lambda (f $k37)
   ($k37
    (lambda (z $k38)
      (f
       z
       (lambda ($rv42)
         (f
          $rv42
          (lambda ($rv41)
            (f
             $rv41
             (lambda ($rv40) (f $rv40 (lambda ($rv39) (f $rv39 $k38))))))))))))
 (lambda (qq)
   (qq
    (lambda (f $k29) ($k29 (lambda (z $k30) (f z $k30))))
    (lambda ($rv27) (halt $rv27)))))
