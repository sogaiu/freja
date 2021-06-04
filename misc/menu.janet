(import ./frp3 :as frp)

(def font-size 16)
(def spacing 1)
(def font (default-load-font "assets/fonts/FiraSans-Regular.ttf" font-size))

(defn unit [v]
  (* v 4))

(var still-down false)
(var open-menu nil)
(var menu-rec nil)

(varfn size
  [t]
  (measure-text-ex font
                   t
                   font-size
                   spacing))

(setdyn :layout :vertical)
(var mr @[0 0 1 1])

(var to-draw nil)

(varfn get-rec
  [t [x y]]
  (let [[w h] (measure-text-ex font
                               t
                               font-size
                               spacing)
        anchor (dyn :anchor)]
    [(+ x (anchor 0)) (+ y (anchor 1)) w h]))

(varfn draw-text**
  [t in-pos color]
  (def anchor (dyn :anchor))
  (def pos [(+ (in-pos 0) (anchor 0))
            (+ (in-pos 1) (anchor 1))])
  (draw-text-ex font t pos font-size spacing color)
  (let [[w h] (measure-text-ex font
                               t
                               font-size
                               spacing)]

    (case (dyn :layout)
      :horizontal
      (put anchor 0 (+ w (pos 0)))

      :vertical
      (put anchor 1 (+ h (pos 1))))

    anchor))

(defmacro button
  [text p & body]
  ~(do
     (def mouse-over (check-collision-recs (get-rec ,text ,p) mr))
     (when (and mouse-over (mouse-button-released? 0))
       ,;body)
     (def r (draw-text** ,text ,p (if mouse-over 0xffffffff 0xffffff80)))))

(varfn file-menu
  []
  (draw-rectangle-rec [0 (unit 6) 100 100] 0x3E3E3Eff)

  (with-dyns [:layout :vertical
              :anchor @[(unit 1) (unit 6)]]

    (def p [(unit 4) (unit 0.5)])
    (def mouse-over (check-collision-recs (get-rec "Save" p) mr))
    (when (and mouse-over (mouse-button-released? 0))
      (save-file gb-data (gb-data :path))
      (set open-menu nil))
    (def r (draw-text** "Save" p (if mouse-over 0xffffffff 0xffffff80))))

  (when (mouse-button-released? 0)
    (print "que" still-down)
    (if still-down
      (set still-down false)
      (do (print "wat")
        (set open-menu nil)))))

(def edit-menu-rec @[(unit 12) (unit 6) 120 44])

(varfn edit-menu
  []
  (draw-rectangle-rec edit-menu-rec 0x3E3E3Eff)

  (with-dyns [:layout :vertical
              :anchor @[(+ (edit-menu-rec 0) (unit 1)) (unit 6)]]

    (button "Undo   Ctrl+Z"
            [(unit 4) (unit 0.5)]
            (undo! gb-data)
            (set open-menu nil))

    (button "Redo   Ctrl+R"
            [(unit 4) (unit 0.5)]
            (redo! gb-data)
            (set open-menu nil))

    (when (mouse-button-released? 0)
      (if still-down
        (set still-down false)
        (set open-menu nil)))))

(comment
  (pp (macex '(button "Lule" [(unit 4) (unit 0.5)]
                      (print "hello")
                      (set open-menu nil))))

  (button "Lule" [(unit 4) (unit 0.5)]
          (print "hello")
          (set to-draw "wat")
          (set open-menu nil))

  #
)

(varfn top-bar-focus?
  [self _]
  (let [[_ y _ h] (self :rec)]
    (>= (+ y h) ((get-mouse-position) 1))))

(var top-bar-rec @[0 0 (get-screen-width) (unit 6)])

(varfn draw-top-bar
  []
  (let [[mx my] (get-mouse-position)]
    (put mr 0 mx)
    (put mr 1 my))

  (when to-draw
    (draw-text-ex font to-draw [800 100] 28 1 0x00ff00ff))

  (draw-rectangle-rec top-bar-rec 0x3E3E3Eff)

  (case open-menu
    :file (file-menu)
    :edit (edit-menu))

  (with-dyns [:layout :horizontal
              :anchor @[(unit 1) (unit 0.5)]]

    (def p [(unit 4) (unit 0.5)])

    (def mouse-over (check-collision-recs (get-rec "File" p) mr))
    (when (and mouse-over (mouse-button-down? 0))
      (set open-menu :file)
      (set still-down true)
      (set menu-rec [0 (unit 6) 100 100]))
    (def r (draw-text** "File" p (if mouse-over 0xffffffff 0xffffff80)))

    (def p [(unit 4) (unit 0.5)])
    (def mouse-over (check-collision-recs (get-rec "Edit" p) mr))
    (when (and mouse-over (mouse-button-down? 0))
      (set open-menu :edit)
      (set still-down true)
      (put edit-menu-rec 0
           (+ ((dyn :anchor) 0) (unit 2.5)))
      (set menu-rec edit-menu-rec))
    (def r (draw-text** "Edit" p (if (or (= open-menu :edit) mouse-over) 0xffffffff 0xffffff80)))

    (button "Lule" [(unit 4) (unit 0.5)]
            (print "hello")
            (set to-draw "hello Sogaiu :)")
            (set open-menu nil))))

(def top-bar @{:render draw-top-bar
               :context @{:capture-mouse |
                          (or (in-rec? $ top-bar-rec)
                              (and open-menu
                                   (in-rec? $ menu-rec)))}})


(def rt (load-render-texture (get-screen-width) (get-screen-height)))
#(begin-texture-mode rt)
#(clear-background :blank)
#(end-texture-mode)

(def state-ref @{:ch (ev/chan 1) :data @[]})

(def state
  (->> @[### buttons
         @{:listen [frp/clicks-ref state-ref]
           :hitbox @[810 0 100 70]
           :update (fn [self click state]
                     (draw-rectangle-rec (self :hitbox) :red)
                     (when (in-rec? click (self :hitbox))
                       (frp/swap! frp/callbacks-ref array/push |(print (length state)))))}

         @{:listen [frp/clicks-ref]
           :render (fn [self]
                     (draw-top-bar))
           :update (fn [self click state]
                     (when (in-rec? click (self :hitbox))
                       (frp/swap! frp/callbacks-ref array/push |(print (length state)))))}

         @{:listen [frp/clicks-ref]
           :hitbox @[860 20 100 70]
           :render (fn [self]
                     (draw-rectangle-rec (self :hitbox) :blue))
           :update (fn [self click]
                     (when (in-rec? click (self :hitbox))
                       (print "pushing")
                       (frp/swap! frp/callbacks-ref array/push |(print "blue"))))}

         ### resolving callbacks
         @{:listen [frp/callbacks-ref]
           :update (fn [self cbs]
                     (when-let [cb (last cbs)]
                       (cb)
                       (array/clear cbs)))}]
       (map frp/add-listeners)))

(defn run
  [state]
  (loop [o :in state
         :let [{:listeners listeners
                :listen listen
                :last-res last-res} o]]
    (var any-change false)

    (loop [i :range [0 (length listen)]
           :let [l (listen i)
                 ls (listeners l)]]
      (when-let [v (frp/ev/check ls)]
        (set any-change true)
        (put last-res i v)))

    (begin-texture-mode rt)
    (when any-change
      ((o :update) o ;(o :last-res)))
    (end-texture-mode)))

(frp/swap! state-ref (fn [_] state))

(varfn draw-frame
  [dt]
  (when (mouse-button-pressed? 0)
    (frp/swap! frp/clicks-ref (fn [_] (get-mouse-position))))

  (frp/run (state-ref :data))

  (draw-texture-pro
    (get-render-texture rt)
    [0 0 (get-screen-width) (- (get-screen-height))]
    [0 0 (get-screen-width) (get-screen-height)]
    [0 0]
    0
    :white))