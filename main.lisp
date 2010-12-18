;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
(in-package #:outbreak)

(defvar *screen-width* 1024)
(defvar *screen-height* 768)
(defvar *view* (glaw:create-2d-view 0 0 *screen-width* *screen-height*))
(defvar *font* nil)

(defvar *pad-speed* (/ *screen-width* 2.0))
(defvar *ball-speed* 250.0)

(defstruct pad
  x y
  (direction nil)
  shape
  (bbox (glaw:make-bbox)))

(defun create-pad (x y)
  (let ((pad (make-pad :x x :y y)))
    (setf (pad-shape pad) (glaw:create-rectangle-shape
                           (- x 100) (- y 10) (+ x 100) (+ y 10)))
    (glaw:bbox-update/shape (pad-bbox pad) (pad-shape pad))
    pad))

(defun move-pad (pad speed)
  (case (pad-direction pad)
    (:left (decf (pad-x pad) speed)
           (glaw:translate-shape (pad-shape pad) (- speed) 0))
    (:right (incf (pad-x pad) speed)
           (glaw:translate-shape (pad-shape pad) speed 0)))
  (when (pad-direction pad)
    (glaw:bbox-overwrite/shape (pad-bbox pad) (pad-shape pad))))

(glaw:key-handler (it pad) (:left :press)
   (setf (pad-direction it) :left))

(glaw:key-handler (it pad) (:left :release)
   (when (eq (pad-direction it) :left)
     (setf (pad-direction it) nil)))

(glaw:key-handler (it pad) (:right :press)
   (setf (pad-direction it) :right))

(glaw:key-handler (it pad) (:right :release)
   (when (eq (pad-direction it) :right)
     (setf (pad-direction it) nil)))

(defstruct ball
  x y radius
  old-x old-y
  (vx 0.0) (vy 0.0)
  shape
  (bbox (glaw:make-bbox)))

(defun create-ball (x y radius)
  (let ((ball (make-ball :x x :y y :radius radius
                         :old-x x :old-y y)))
    (setf (ball-shape ball) (glaw:create-circle-shape x y radius))
    (glaw:bbox-update/shape (ball-bbox ball) (ball-shape ball))
    ball))

(defun move-ball (ball dt)
  (setf (ball-old-x ball) (ball-x ball)
        (ball-old-y ball) (ball-y ball))
  (incf (ball-x ball) (* (ball-vx ball) dt))
  (incf (ball-y ball) (* (ball-vy ball) dt))
  (glaw:translate-shape (ball-shape ball)
                        (* (ball-vx ball) dt) (* (ball-vy ball) dt))
  (glaw:bbox-overwrite/shape (ball-bbox ball) (ball-shape ball)))

(defstruct brick
  x y
  destroyable
  color
  shape
  (bbox (glaw:make-bbox)))

(defun create-brick (x y w h &key (color (glaw:create-color 0.7 0.7 0.7 1.0))
                                  (destroyable t))
  (let ((brick (make-brick :x x :y y :destroyable destroyable
                           :color color)))
    (setf (brick-shape brick) (glaw:create-rectangle-shape
                               (- x (/ w 2.0)) (- y (/ h 2.0))
                               (+ x (/ w 2.0)) (+ y (/ h 2.0))))
    (glaw:bbox-update/shape (brick-bbox brick) (brick-shape brick))
    brick))

(defstruct level
  pad (balls '()) (bricks '())
  (remaining-bricks 0))

(defun finished-level (level)
  (zerop (level-remaining-bricks level)))

(defun create-level ()
  (let ((level (make-level :pad (create-pad (/ *screen-width* 2.0)
                                            (/ *screen-height* 8.0)))))
    ;; left wall
    (push (create-brick 10 (- (/ *screen-height* 2.0) 10)
                        20 (- *screen-height* 20) :destroyable nil)
          (level-bricks level))
    ;; right wall
    (push (create-brick (- *screen-width* 10) (- (/ *screen-height* 2.0) 10)
                        20 (- *screen-height* 20) :destroyable nil)
          (level-bricks level))
    ;; top wall
    (push (create-brick (/ *screen-width* 2.0) (- *screen-height* 10)
                        *screen-width* 20 :destroyable nil)
          (level-bricks level))
    (glaw:add-input-handler (level-pad level))
    ;; some random bricks
    (loop for x from  100.0 to (- *screen-width* 50.0) by (/ (- *screen-width* 100.0)
                                                            10.0) do
         (loop for y from 200.0 to (- *screen-height* 50.0) by (/ (- *screen-height* 150.0)
                                                                  10.0) do
         (incf (level-remaining-bricks level))
         (push (create-brick x y
                             (/ (- *screen-width* 100.0) 10.0)
                             (/ (- *screen-height* 150.0) 10.0)
                            :color (glaw:create-color (/ (random 100.0) 100.0)
                                                      (/ (random 100.0) 100.0)
                                                      (/ (random 100.0) 100.0)))
               (level-bricks level))))
    level))


(defun fire-ball (level)
  (let* ((pad (level-pad level))
         (ball (create-ball (pad-x pad)
                            (+ (pad-y pad) 40.0)
                            10.0))
         (vx (cond ((eq (pad-direction pad) :right) *ball-speed*)
                   ((eq (pad-direction pad) :left) (- *ball-speed*))
                   (t 0.0))))
    (setf (ball-vx ball) vx)
    (setf (ball-vy ball) *ball-speed*)
    (push ball (level-balls level))))

(defun check-collisions (level)
  (dolist (ball (level-balls level))
    (let ((dx (- (ball-x ball) (ball-old-x ball)))
          (dy (- (ball-y ball) (ball-old-y ball)))
          (sh (ball-bbox ball)))
      ;; ball vs. brick
      (dolist (b (level-bricks level))
        (let ((bsh (brick-bbox b)))
          (when (glaw:bbox-intersect-p (ball-bbox ball) bsh)
            (when (brick-destroyable b)
              (setf (level-bricks level)
                    (remove b (level-bricks level)))
              (incf *score*)
              (decf (level-remaining-bricks level)))
            (cond
             ((glaw:coords-overlap-p
                (glaw:bbox-y-min bsh) (glaw:bbox-y-max bsh)
                (- (glaw:bbox-y-min sh) dy)  (- (glaw:bbox-y-max sh) dy))
               (setf (ball-vx ball) (- (ball-vx ball))))
              ((glaw:coords-overlap-p
                (glaw:bbox-x-min bsh) (glaw:bbox-x-max bsh)
                (- (glaw:bbox-x-min sh) dx)  (- (glaw:bbox-x-max sh) dx))
               (setf (ball-vy ball) (- (ball-vy ball))))))))
      ;; ball vs. pad
      (let ((psh (pad-bbox (level-pad level))))
        (when (glaw:bbox-intersect-p (ball-bbox ball) psh)
          (cond
            ((glaw:coords-overlap-p
                (glaw:bbox-y-min psh) (glaw:bbox-y-max psh)
                (- (glaw:bbox-y-min sh) dy)  (- (glaw:bbox-y-max sh) dy))
               (setf (ball-vx ball) (- (ball-vx ball))))
              ((glaw:coords-overlap-p
                (glaw:bbox-x-min psh) (glaw:bbox-x-max psh)
                (- (glaw:bbox-x-min sh) dx)  (- (glaw:bbox-x-max sh) dx))
               (setf (ball-vy ball) (- (ball-vy ball)))))
          (let ((pad-ctr-dist (- (ball-x ball)
                                 (pad-x (level-pad level)))))
            (case (pad-direction (level-pad level))
              (:left (decf (ball-vx ball) pad-ctr-dist))
              (:right (incf (ball-vx ball) pad-ctr-dist))))))
      ;; ball vs. bottom
      (when (< (ball-y ball) 0.0)
        (decf *score*)
        (setf (level-balls level) (remove ball (level-balls level)))))))

(defun render-level (level)
  (gl:disable :texture-2d)
  (glaw:set-color/rgb 1 1 1 1)
  (glaw:render-shape (pad-shape (level-pad level)))
  (glaw:set-color/rgb 1 1 1 1)
  (dolist (b (level-balls level))
    (glaw:render-shape (ball-shape b)))
  (dolist (b (level-bricks level))
    (glaw:set-color (brick-color b))
    (glaw:render-shape (brick-shape b))
    (glaw:set-color/rgb 1 1 1)
    (glaw:render-bbox (brick-bbox b))))

(defun update-level (level dt)
  (move-pad (level-pad level) (* *pad-speed* dt))
  (dolist (b (level-balls level))
    (move-ball b dt))
  (check-collisions level))

(glaw:key-handler (it level) (:space :press)
   (fire-ball it))

;; Main code

(defvar *level* nil)
(defvar *score* 0)

(glaw:key-handler :global (:space :press)
  (when (finished-level *level*)
    (glaw:remove-input-handler *level*)
    (setf *score* 0)
    (setf *level* (create-level))
    (glaw:add-input-handler *level*)))

(defun init ()
  (glaw:init-content-manager)
  (glaw:load-asset "font.png" :font)
  (setf *font* (glaw:use-resource "font.png"))
  (setf *level* (create-level))
  (glaw:add-input-handler *level*))

(defun shutdown ()
  (glaw:remove-input-handler *level*)
  (setf *level* nil)
  (glaw:dispose-asset "font.png")
  (glaw:shutdown-content-manager))

(defun draw ()
  (glaw:set-view-2d *view*)
  (glaw:begin-draw)
  (render-level *level*)
  (if (finished-level *level*)
      (progn (glaw:set-color/rgb 0 1 0)
             (glaw:format-at (/ *screen-width* 2.0) (/ *screen-height* 2.0)
                             *font* "Finished")
             (glaw:set-color/rgb 1 0 0)
             (glaw:format-at (/ *screen-width* 2.0) (- (/ *screen-height* 2.0) 20)
                             *font* "Score: ~d" *score*)
             (glaw:set-color/rgb 1 0 0)
             (glaw:format-at (/ *screen-width* 2.0) (- (/ *screen-height* 2.0) 40)
                             *font* "Esc to quit. Space to restart"))
      (glaw:format-at 10 10 *font* "Score: ~d" *score*))
  (glaw:end-draw))

(let ((last-update-time (get-internal-real-time)))
  (defun idle ()
    (let* ((elapsed-time (- (get-internal-real-time)
                            last-update-time))
           (dt (/ (* elapsed-time 1.0)
                  internal-time-units-per-second)))
      (setf last-update-time (get-internal-real-time))
      (unless (finished-level *level*)
        (update-level *level* dt)))))

(defmethod glop:on-key (window pressed keycode keysym string)
  (glaw:dispatch-key-event keysym (if pressed :press :release) keycode string)
  (when (eql keysym :escape)
    (glop:push-close-event window)))

(defmethod glop:on-close (window)
  (shutdown))

(defmethod glop:on-button (window pressed button)
  (glaw:dispatch-button-event :mouse (glaw:translate-mouse-button button)
                              (if pressed :press :release)))

(defmethod glop:on-mouse-motion (window x y dx dy)
  (glaw:update-mouse-position x y)
  (glaw:dispatch-motion-event :mouse dx dy))

(defmethod glop:on-draw (window)
  (draw)
  (glop:swap-buffers window))

(defmethod glop:on-resize (window w h)
  (glaw:reshape w h)
  (draw)
  (glop:swap-buffers window))


(defun run ()
  ;; how to get extensions
  (setf cl-opengl-bindings:*gl-get-proc-address* 'glop:gl-get-proc-address)
  (glop:with-window (win "OutBreak" 800 600)
    (glaw:setup-2d-defaults)
    (glaw:reshape 800 600)
    (init)
    (loop while (glop:dispatch-events win :blocking nil) do
         (idle)
         (draw)
         (glop:swap-buffers win))))

