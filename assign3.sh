;
 ; =====================================================================================
 ;
 ;       Filename:  assign3.sh
 ;
 ;    Description:  snake game
 ;
 ;        Version:  2.0
 ;        Created:  18/10/18 00:36:40
 ;       Revision:  none
 ;       Compiler:  racket
 ;
 ;         Author:  Zheng-Yin, Rio (a1691850), a1691850@student.adelaide.edu.au
 ;   Organization:  
 ;
 ; =====================================================================================
;


#lang racket
(require racket/gui)

;定义游戏的边界
(define width 25)
(define height 20)
;定义每个像素块的大小
(define size 16)
;初始化的蛇的长度与位置
(define snake (list (list 2 1) (list 1 1)))
;初始化蛇的方向为向右
(define fangxiang 'r)
; 食物的初始化位置
(define shiwu (list 9 8))
; 吃到了多少次食物
(define timess 0)

; move-block:
; 从列表中删除最后一个元素,并在其开头添加一个新元素
(define (move-block x y) 
  (reverse (append (cdr (reverse snake)) (list(list x y)))))

; find-head: 把列表里面第i个数据拿出来
(define (find-head num lst)
  (list-ref (list-ref lst 0) num))

; draw-block:
; 在给定的绘图对象中绘制一个块,可以是字符串或颜色对象
(define (draw-block screen x y color) 
  (send screen set-brush color 'panel)
  (send screen draw-rectangle (* x size) (* y size) size size))

; move-snake: 
; 移动这个蛇的四个方向 之后会被调用('a, 'd, 'w or 's)

(define (move-snake location)
  (case location
    ['a (set! snake (move-block (- (find-head 0 snake) 1) (find-head 1 snake)))]
    ['d (set! snake (move-block (+ (find-head 0 snake) 1) (find-head 1 snake)))]
    ['w (set! snake (move-block (find-head 0 snake) (- (find-head 1 snake) 1)))]
    ['s (set! snake (move-block (find-head 0 snake) (+ (find-head 1 snake) 1)))]))





; foodinside: 检查食物是不是在蛇的身子里
(define (foodinside snake block [i 0] [g 0]) 
  (if (> (length snake) i)
    (if (and (not (= g i)) (and 
      (eq? (list-ref (list-ref snake i) 0) (list-ref block 0)) ; 检查坐标x y
      (eq? (list-ref (list-ref snake i) 1) (list-ref block 1)))) 
        #t
      (foodinside snake block (+ i 1) g))
    #f))

; cresce-snake: 通过复制蛇列表的最后一个元素使蛇+1,将蛇移动到另一个位置并添加我之前选择的最后一个元素
(define cresce-snake (lambda () 
  (define x (car (reverse snake)))
  (set! shiwu (list (inexact->exact (round (* (random) (- width 10)))) (inexact->exact (round (* (random) (- height 10)))) ))
  (move-snake fangxiang)
  (set! timess (add1 timess))
  (set! snake (append snake (list x)))))

(define restart (lambda()
  (set! fangxiang 'd)
  (set! shiwu (list 6 6))
  (set! snake (list (list 2 1) (list 1 1)))
  (set! timess 0)
))


(define frame (new frame% 
  [label "snake"]
  [width (* width size)]
  [height (* height size)]))



(define (canvas-key frame) (class canvas%
  (define/override (on-char key-event)
    (cond ; todo: in_list? -> set fangxiang
      [(eq? (send key-event get-key-code) 'left) (set! fangxiang 'a)]
      [(eq? (send key-event get-key-code) 'right) (set! fangxiang 'd)]
      [(eq? (send key-event get-key-code) 'up) (set! fangxiang 'w)]
      [(eq? (send key-event get-key-code) 'down) (set! fangxiang 's)]
      [(eq? (send key-event get-key-code) '#\r) (restart)]))
  (super-new [parent frame])))




; update-snake: 用于屏幕更新和蛇运动的功能
(define update-snake (lambda () 
  (draw-block dc (list-ref shiwu 0) (list-ref shiwu 1) "red") ; 画出食物
  (cond [(foodinside snake shiwu) (cresce-snake)] [else (move-snake fangxiang)]) ; 当蛇与食物重合
  (send dc draw-text (number->string timess) (-(* width size) 50) 10)
  (for ([block snake]) (
    if (eq? block (car snake)) 
      (draw-block dc (list-ref block 0) (list-ref block 1) "white") 
      (draw-block dc (list-ref block 0) (list-ref block 1) "white")))))

; lost-the-game: 在输掉比赛时发生的事件
(define lost-the-game (lambda ()
  (send dc draw-text "U Lose" (- (round (/ (* width size) 2)) 30) (- (round (/ (* height size) 2)) 50));第一句话的位置
))

; 画布的例子
(define canvas (
  new (canvas-key frame)))

; 开始画
(define dc (send canvas get-dc))

; 设置字体
(send dc set-font (make-object font% 10 'default))
(send dc set-text-foreground "white")
(send frame show #t)

; 循环框架刷新

(define timer (new timer%
  [notify-callback (lambda()
    (send dc clear)
    (send dc set-brush "black" 'solid)        
    (send dc draw-rectangle 0 0 (* width size) (* height size))                 
    
    (define pengzhuang #f)
    (for ([block snake]
         [j (in-naturals 0)])
      (cond 
            [(or (> (list-ref block 0) width) (> 0 (list-ref block 0))) (set! pengzhuang #t )]
            [(or (> (list-ref block 1) (- height 5)) (> 0 (list-ref block 1))) (set! pengzhuang #t)]
            [(eq? #f pengzhuang) (set! pengzhuang (eq? #t (foodinside snake block 0 j)))]))
    (if pengzhuang (lost-the-game) (update-snake)))]
  [interval #f]))
(instantiate button% () (label "Restart")(parent frame)
     (callback (lambda (button event)(restart)))
     )
(send timer start 100)