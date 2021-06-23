# (df "misc/frp3.janet")

(use jaylib)
(import ./collision :prefix "")
(import ./../freja/state :prefix "")
(import ./defonce :prefix "")
(import ./../freja/render_new_gap_buffer :prefix "")
(import ./../freja/input :prefix "")
(import ./../top_bar2 :as tb)

123

(def possible-keys
  [(keyword "'")
   (keyword ",")
   :-
   :.
   :/
   :0
   :1
   :2
   :3
   :4
   :5
   :6
   :7
   :8
   :9
   (keyword ";")
   :=
   :a
   :b
   :backslash
   :backspace
   :c
   :caps-lock
   :d
   :delete
   :down
   :e
   :end
   :enter
   :escape
   :f
   :f1
   :f10
   :f11
   :f12
   :f2
   :f3
   :f4
   :f5
   :f6
   :f7
   :f8
   :f9
   :g
   :grave
   :h
   :home
   :i
   :insert
   :j
   :k
   :kb-menu
   :kp-*
   :kp-+
   :kp--
   :kp-.
   :kp-/
   :kp-0
   :kp-1
   :kp-2
   :kp-3
   :kp-4
   :kp-5
   :kp-6
   :kp-7
   :kp-8
   :kp-9
   :kp-=
   :kp-enter
   :l
   :left
   :left-alt
   :left-bracket
   :left-control
   :left-shift
   :left-super
   :m
   :n
   :num-lock
   :o
   :p
   :page-down
   :page-up
   :pause
   :print-screen
   :q
   :r
   :right
   :right-alt
   :right-bracket
   :right-control
   :right-shift
   :right-super
   :s
   :scroll-lock
   :space
   :t
   :tab
   :u
   :up
   :v
   :w
   :x
   :y
   :z])

(defn split
  [ch target-chs]
  @{:fib
    (ev/call
      (fn []
        (forever
          (let [v (ev/take ch)]
            (loop [c :in target-chs]
              (ev/give c v))))))
    :chs target-chs})

(defn listen
  [ref o]
  (unless (get-in o [:listeners ref])
    (def ch (ev/chan 10))
    (update o :listeners |(put (or $ @{}) ref ch))
    
    (if-let [{:fib fib :chs chs} (ref :split)]
      (do #(ev/cancel fib "k")
        (put ref :split (split (ref :ch) (array/push chs ch))))
      (put ref :split (split (ref :ch) @[ch])))
    (update o :last-res |(let [arr (or $ (array 1))]
                           (put arr (dec (length (o :listeners))) (ref :data))))))

(varfn unlisten
  [ref o]
  (def ch (get-in o [:listeners ref]))
  (update-in ref [:split :chs] (fn [chs] (filter |(not= $ ch) chs)))
  (put-in o [:listeners ref] nil)
  (update o :listen (fn [refs] (filter |(not= $ ref) refs)))
  #(pp (ref :split))
  )

(varfn unlisten-all
  [o]
  (loop [l :in (o :listen)]
    (unlisten l o))
  (put o :last-res nil))

(comment
  (loop [o :in state]
    (unlisten-all o))
  #
  )

(defn add-listeners
  [o]
  (loop [l :in (o :listen)]
    (listen l o))
  o)

(varfn run
  [state]

  (var current-res @[])

  (loop [o :in state
         :let [{:listeners listeners
                :listen listen
                :last-res last-res} o]]
    (array/clear current-res)

    (var check-queue true)

    (while check-queue
      (var any-change false)
      (var any-change-in-queues false)
      
      (loop [i :range [0 (length listen)]
             :let [l (listen i)
                   ls (listeners l)]]
        (if-let [v (ev/check ls)]
          (do (set any-change true)
            (unless (l :only-last)
              # if a historised ref was changed
              # we want to rerun the thing to get the next event              
              (set any-change-in-queues true))
            
            (unless (l :no-history)
              (put last-res i v))
            (put current-res i v))
          
          (put current-res i (last-res i))))
      
      (when any-change
        (:update o ;current-res))
      
      (unless any-change-in-queues
        (set check-queue false)))))

(var draw-layout nil)
(var trigger-update-layout nil)

(defn pad-left
  [padding]
  (if (nil? padding)
    0
    (case (length padding)
      1 (first padding)
      2 (padding 1)
      4 (padding 3))))

(defn pad-top
  [padding]
  (if (nil? padding)
    0
    (case (length padding)
      1 (first padding)
      2 (padding 0)
      4 (padding 0))))

(def button {:render (fn [self {:size size :padding padding :color color} children]
                       (draw-rectangle
                         ((dyn :anchor) 0)
                         ((dyn :anchor) 1)
                         (get size 0 0)
                         (get size 1 0)
                         color)

                       (with-dyns [:anchor (array ;(dyn :anchor))]
                         (update (dyn :anchor) 0 + (pad-left padding))
                         (update (dyn :anchor) 1 + (pad-top padding))
                         (when children
                           (map draw-layout children)))

                       (if (= (dyn :layout) :horizontal)
                         (update (dyn :anchor) 0 + (get size 0 0))
                         (update (dyn :anchor) 1 + (get size 1 0))))
             :update (fn [self
                          {:size size
                           :padding padding
                           :on-click on-click
                           :clicks clicks}
                          children]
                       (def [kind pos] clicks)
                       (when (and (or (= kind :drag)
                                      (= kind :press)
                                      (= kind :double-click)
                                      (= kind :triple-click))
                                  (in-rec?
                                    pos
                                    [((dyn :anchor) 0)
                                     ((dyn :anchor) 1)
                                     (get size 0 0)
                                     (get size 1 0)]))
                         (swap! callbacks-ref put kind on-click))

                       (with-dyns [:anchor (array ;(dyn :anchor))]
                         (update (dyn :anchor) 0 + (pad-left padding))
                         (update (dyn :anchor) 1 + (pad-top padding))
                         (when children
                           (map trigger-update-layout children)))

                       (if (= (dyn :layout) :horizontal)
                         (update (dyn :anchor) 0 + (get size 0 0))
                         (update (dyn :anchor) 1 + (get size 1 0))))})

(def horizontal {:render
                 (fn [self
                      {:size size
                       :padding padding
                       :color color}
                      children]

                   (when color
                     (draw-rectangle
                       ((dyn :anchor) 0)
                       ((dyn :anchor) 1)
                       (get size 0 0)
                       (get size 1 0)
                       color))

                   (with-dyns [:anchor (array ;(dyn :anchor))
                               :layout :horizontal]
                     (update (dyn :anchor) 0 + (pad-left padding))
                     (update (dyn :anchor) 1 + (pad-top padding))
                     (when children
                       (map draw-layout children)))

                   (if (= (dyn :layout) :horizontal)
                     (update (dyn :anchor) 0 + (get size 0 0))
                     (update (dyn :anchor) 1 + (get size 1 0))))

                 :update
                 (fn [self
                      {:size size
                       :padding padding}
                      children]

                   (with-dyns [:anchor (array ;(dyn :anchor))
                               :layout :horizontal]
                     (update (dyn :anchor) 0 + (pad-left padding))
                     (update (dyn :anchor) 1 + (pad-top padding))
                     (when children
                       (map trigger-update-layout children)))

                   (if (= (dyn :layout) :horizontal)
                     (update (dyn :anchor) 0 + (get size 0 0))
                     (update (dyn :anchor) 1 + (get size 1 0))))})

(varfn draw-layout
  [c]
  (with-dyns [:anchor (or (dyn :anchor) @[0 0])
              :layout :horizontal]
    (def [f props] c)
    (def children (array/slice c 2))
    (:render f props children)))

(varfn trigger-update-layout
  [c]
  (with-dyns [:anchor (or (dyn :anchor) @[0 0])
              :layout :horizontal]
    (def [f props] c)
    (def children (array/slice c 2))
    (:update f props children)))

(defn noop [_ _ _] nil)

(def p {:render
        (fn [self {:color color} [text]]
          (default color :black)
          (draw-text-ex tb/font text (dyn :anchor) tb/font-size tb/spacing color))
        :update noop})

(def mouse-data (new-mouse-data))

(defn new-text-area
  [id gb-ref]
  @{:listen [gb-ref focus-ref char-ref kb-ref clicks-ref mouse-pos-ref scroll-ref]
    
    :id id    
    
    :render (fn [self]
              (when (or (= (self :id) :main)
                        (= (self :id) ((self :last-res) 1)))
                (def gb ((self :last-res) 0))

                (rl-pop-matrix)

                (end-texture-mode)

                (gb-pre-render gb)
                (begin-texture-mode (rt-ref :data))

                (rl-push-matrix)

                (rl-load-identity)

                (rl-scalef 2 2 1)

                (gb-render-text gb)))    
    
    :update (fn [self gb focus char-to-insert pressed-key clicks mouse-pos scroll]
              #(handle-mouse self (data :mouse))
              #(handle-scroll self)

              (when (= (self :id) focus)
                (when clicks
                  #(swap! callbacks-ref array/push |(print "GB CLICKED"))

                  # handle mouse input here, somewhere
                  )
                (try
                  (do
                    (when (and clicks gb)
                      (handle-mouse-event
                        gb
                        clicks
                        (fn [kind f]
                          (swap! callbacks-ref put kind f)
                          (swap! gb-ref (fn [_] gb))
                          (swap! state-ref identity)))
                      (swap! gb-ref (fn [_] gb))
                      (swap! state-ref identity))
                    
                    (comment
                      (when (and scroll gb)
                        (handle-scroll-event gb scroll)
                        (swap! gb-ref (fn [_] gb))
                        (swap! state-ref identity)))

                    (when pressed-key
                      (do
                        (handle-keyboard2 gb pressed-key)
                        (swap! gb-ref (fn [_] gb))
                        (swap! state-ref identity)))

                    (when char-to-insert
                      (do
                        (handle-keyboard-char gb char-to-insert)
                        (swap! gb-ref (fn [_] gb))
                        (swap! state-ref identity))))

                  ([err fib]
                    (print "kbd")
                    (print (debug/stacktrace fib err))))))})


(def text-area (new-text-area :main gb-ref))
(def search-area (new-text-area :search search-ref))
(def file-open-area (new-text-area :file-open file-open-ref))

(def menu
  @{:listen [clicks-ref]
    :layout (fn [clicks]
              [horizontal
               {:size [1920 30]
                :color 0x3E3E3Eff}
               [button {:size [100 30]
                        :padding [8 12]
                        :on-click (fn [] (print "Hi sogaiu :D"))
                        :clicks clicks
                        :color 0x3E3E3Eff}
                [p {:color 0xffffff80} "File123"]]

               [button {:size [100 30]
                        :padding [8 12]
                        :on-click (fn [] (print "Hi sogaiu :D"))
                        :clicks clicks
                        :color 0x3E3E3Eff}
                [p {:color 0xffffff80} "Edit"]]])
    :render (fn [self]
              (draw-layout ((self :layout) nil)))
    :update (fn [self click]
              (trigger-update-layout ((self :layout) click)))})

(put menu :render nil)

(when (dyn 'state)
  # (print "state exists")
  
  
  (loop [o :in ((dyn 'state) :value)]
    (unlisten-all o))

  )

(def state
  (->> @[### buttons
         text-area
         
         search-area
         
         file-open-area
         
         @{:listen [focus-ref new-frame-ref]
           
           :render (fn [self]
                     (when-let [gb (self :gb)]
                       (render-cursor gb)))

           :on true

           :update (fn [self focus dt]
                     (def old-gb (self :gb))
                     (put self :gb (case focus
                                     :main (gb-ref :data)
                                     :search (search-ref :data)
                                     :file-open (file-open-ref :data)))
                     (def gb (self :gb))

                     (if (not= old-gb gb)
                       (do (put gb :blink 0)
                         (put self :on true)
                         (swap! state-ref identity))
                       (+= (gb :blink) (* 60 dt)))

                     (when (and (> (gb :blink) 30)
                                (self :on))
                       (put self :on false)
                       (swap! state-ref identity))

                     (when (> (gb :blink) 60)
                       (set (gb :blink) 0)
                       (put self :on true)
                       (swap! state-ref identity)))}

         @{:listen [clicks-ref state-ref]
           :hitbox @[810 0 100 70]
           :render (fn [self]
                     (draw-rectangle-rec (self :hitbox) :red))
           :update (fn [self click state]
                     (when (and click
                                (in-rec? click (self :hitbox)))
                       (swap! callbacks-ref put (first click) |(print (length state)))))}

         @{:listen [clicks-ref]
           :hitbox @[860 20 100 70]
           :render (fn [self]
                     (draw-rectangle-rec (self :hitbox) :blue))
           :update (fn [self click]
                     (when (in-rec? click (self :hitbox))
                       (print "pushing")
                       (swap! callbacks-ref put (first click) |(print "blue"))))}

         menu

         ### rendering

         @{:listen [state-ref screen-size-ref rt-ref]

           :id :rendering

           :update (fn [self state screen-size rt]
                     (begin-texture-mode rt)
                     
                     (rl-push-matrix)
                     
                     (rl-load-identity)
                     
                     (rl-scalef 2 2 1)
                     
                     (clear-background :blank)
                     
                     (loop [o :in state
                            :when (o :render)]
                       # (when (o :id) (print "rendering " (o :id)))
                       (:render o))
                     
                     # (draw-rectangle 800 0 100 100 :black)
                     
                     (rl-pop-matrix)
                     
                     (end-texture-mode)
                     
                     (swap! render-queue-ref
                            (fn [_]
                              (fn render
                                []
                                (draw-texture-pro
                                  (get-render-texture rt)
                                  [0
                                   0
                                   (* 2 (get-screen-width))
                                   (* 2 (- (get-screen-height)))]
                                  [0 0 (get-screen-width) (get-screen-height)]
                                  [0 0]
                                  0
                                  :white)))))}

         @{:listen [new-frame-ref render-queue-ref]
           :update (fn [self _ rq]
                     (when rq
                       (rq)))}

         ### resolving callbacks
         @{:listen [callbacks-ref]
           :update (fn [self cb-pairs]
                     (loop [[k cb] :pairs cb-pairs]
                       (when cb (cb))
                       (put cb-pairs k nil)))}]
       (map add-listeners)))

(defn add-obj
  [state-ref o]
  (swap! state-ref
         array/push
         (add-listeners o)))

(comment
  # eval the following sexp to get another "button"
  (add-objeoa
    state-refu
    @{:hitbox @[810 100 100 70]
      :render (fn [self]
                (print "green")
                (draw-rectaungle-rec (self :hitbox) :green))

      :listen [clicks-ref soaetate-ref]
      :update (fn [self click state]
                (when (in-rec? click (self :hitbox))
                  (swap! callbacks-ref
                         put
                         (first click)
                         |(print "green" (length state)))))})
  #
  )

(var delay-left @{})

(defn handle-keys
  [dt]
  (var k (get-char-pressed))

  (while (not= 0 k)
    (swap! char-ref (fn [_] k))
    (set k (get-char-pressed)))

  (loop [[k dl] :pairs delay-left
         :let [left ((update delay-left k - dt) k)]]
    (when (<= left 0)
      # push key
      (swap! kb-ref (fn [_] k))
      (put delay-left k repeat-delay)))

  (loop [k :in possible-keys]
    (when (key-released? k)
      (put delay-left k nil))
    
    (when (key-pressed? k)
      (put delay-left k initial-delay)
      # push key
      (swap! kb-ref (fn [_] k)))))

# initialize state
(swap! state-ref (fn [_] state))

(var last-mouse-pos nil)
(var mouse-data (new-mouse-data))

(import ./../vector_math :as v :fresh true)

(varfn handle-mouse
  [mouse-data]
  (def pos (get-mouse-position))
  (def [x y] pos)

  (put mouse-data :just-double-clicked false)
  (put mouse-data :just-triple-clicked false)

  (when (mouse-button-released? 0)
    (put mouse-data :just-down nil)
    (put mouse-data :recently-double-clicked nil)
    (put mouse-data :recently-triple-clicked nil)
    (put mouse-data :up-pos [x y])

    (swap! clicks-ref (fn [_] [:release pos])))

  (when (mouse-button-pressed? 0)
    (when (and (mouse-data :down-time2)
               # max time to pass for triple click
               (> 0.4 (- (get-time) (mouse-data :down-time2)))
               # max distance to travel for triple click
               (> 200 (v/dist-sqr pos (mouse-data :down-pos))))
      (put mouse-data :just-triple-clicked true)
      (put mouse-data :recently-triple-clicked true))

    (when (and (mouse-data :down-time)
               # max time to pass for double click
               (> 0.25 (- (get-time) (mouse-data :down-time)))
               # max distance to travel for double click
               (> 100 (v/dist-sqr pos (mouse-data :down-pos))))
      (put mouse-data :just-double-clicked true)
      (put mouse-data :recently-double-clicked true)
      (put mouse-data :down-time2 (get-time))))
  
  (cond (mouse-data :just-triple-clicked)
    (swap! clicks-ref (fn [_] [:triple-click pos]))

    (and (mouse-data :just-double-clicked)
         (not (key-down? :left-shift))
         (not (key-down? :right-shift)))
    (swap! clicks-ref (fn [_] [:double-click pos]))
    
    (or (mouse-data :recently-double-clicked)
        (mouse-data :recently-triple-clicked))
    nil # don't start selecting until mouse is released again

    (mouse-button-down? 0)
    (do
      (put mouse-data :down-time (get-time))
      
      (if (= nil (mouse-data :just-down))
        (do (put mouse-data :just-down true)
          (put mouse-data :last-pos pos)
          (put mouse-data :down-pos pos)
          (swap! clicks-ref (fn [_] [:press pos])))
        (do (put mouse-data :just-down false)
          (unless (= pos (mouse-data :last-pos))
            (put mouse-data :last-pos pos)
            (swap! clicks-ref (fn [_] [:drag pos]))))))))

(varfn handle-scroll
  []
  (let [move (get-mouse-wheel-move)]
    (when (not= move 0)
      (swap! scroll-ref (fn [_] (* move 10))))))

(varfn trigger
  [dt]
  (swap! new-frame-ref (fn [_] dt))
  
  #(handle-mouse mouse-data)
  
  #(handle-scroll)
  
  #(handle-keys dt)
  
  (run (state-ref :data)))

(varfn init
  []
  (swap! focus-ref (fn [_] :main))

  (swap! screen-size-ref
         (fn [_]
           [(get-screen-width)
            (get-screen-height)]))

  (swap! rt-ref
         (fn [_]
           (pp (screen-size-ref :data))
           (load-render-texture ;(map |(* 2 $) (screen-size-ref :data)))))
  (tb/init))
