(import spork/test)
(use freja-jaylib)

(import ./dumb :prefix "")
(import ./new_gap_buffer :as gb)
(import ./textfield_api :prefix "")
(import ./text_rendering :as tr)
(import ./find_row_etc :prefix "")
(import ./assets :as a)
(import ./rainbow :as rb)
(import ./highlighting :as hl)

(defn reset-blink
  [props]
  (set (props :blink) 0)
  props)

(defn height
  [{:position position
    :offset offset
    :size size}]

  (def [_ y] position)
  (def [_ oy] offset)
  (def [_ h] size)

  (if (= h :max)
    (- (get-screen-height)
       y
       oy)
    h))

(defn inner-width
  [{:position position
    :offset offset
    :size size}]
  (def [x _] position)
  (def [ox _] offset)
  (def [w _] size)

  (if (= w :max)
    (- (get-screen-width)
       x
       ox)
    w))

(defn width
  [{:position position
    :offset offset
    :size size}]
  (def [x _] position)
  (def [ox _] offset)
  (def [w _] size)

  (if (= w :max)
    (- (get-screen-width)
       x)
    w))

(def warned-chars @{})

(defn get-size
  [sizes c]
  (let [sz (get sizes c)]
    (if-not sz
      (let [sz (first (values sizes))]
        (unless (warned-chars c)
          (eprint "no size for char " c ", using first sizes instead." sz)
          (put warned-chars c true))
        sz)
      sz)))

(defn width-between
  [gb start stop]
  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))
  (def [x-scale y-scale] screen-scale)
  (var acc-w 0)

  (gb/gb-iterate
    gb
    start stop
    i c
    (let [[w h] (get-size sizes c)] (+= acc-w w)))

  acc-w)

(defn index-passing-max-width
  "Returns the index of the char exceeding the max width.
Returns `nil` if the max width is never exceeded."
  [gb start stop max-width]

  # (def {:sizes sizes} gb)

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (var acc-w 0)

  (gb/gb-iterate
    gb
    start stop
    i c
    (let [[w h] (get-size sizes c)]
      (+= acc-w w)
      (when (> acc-w max-width) # we went too far!
        (return stop-gb-iterate i)))))

(defn index-passing-middle-max-width
  "Returns the index of the middle of the char exceeding the max width.
Returns `nil` if the max width is never exceeded."
  [gb start stop max-width]

  (var acc-w 0)
  (var last-w 0)

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (or (gb/gb-iterate
        gb
        start stop
        i c
        (let [[w h] (sizes c)]
          (+= acc-w (+ (* w 0.5) last-w))
          (set last-w (* w 0.5))
          (when (> acc-w max-width) # we went too far!
            (return stop-gb-iterate i))))
      stop))

(comment

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (with-dyns [:debug true]
    (let [gb {:text @"abc"
              :sizes sizes
              :gap-start 0
              :gap-stop 0
              :gap @"123"}]
      (index-passing-max-width gb
                               0 3
                               20))))

(defn line-of-i
  [gb i]
  (def {:lines lines
        :line-flags line-flags
        :stickiness stickiness} gb)

  (let [line-index (->> (binary-search-closest lines |(compare i $))
                        ## binary-search-closest returns length of 
                        ## array when outside length of array
                        (max 0)
                        (min (dec (length lines))))
        line-flag (line-flags line-index)]

    (if (and (= i (lines line-index)) ## end of line
             (= line-flag :word-wrap)
             (= stickiness :down))
      (inc line-index)
      line-index)))

(defn invalidate-cache-hook
  ``
called when cache is invalidated
line-i is the lowest line index that is invalidated
``
  [gb line-i]
  (loop [[k hook] :in (in gb :hooks/invalidate-cache)]
    (hook gb k line-i)))

(defn new-line-hook
  ``
current-line is a buffer that is cleared after each
new-line-hook call, so don't save this
``
  [gb line-i current-line]
  (loop [[k hook] :in (in gb :hooks/new-line)]
    (hook gb k line-i current-line)))

(defn word-wrap-gap-buffer
  [gb

   stop
   y-offset
   y-limit

   &keys {:line-limit line-limit
          :changed changed
          :change-pos change-pos}]

  (def {:lines lines
        :y-poses y-poses
        :line-flags line-flags
        :line-numbers line-numbers} gb)

  (var start-line-i (cond (empty? lines)
                      nil

                      (not changed)
                      (dec (length lines))

                      (not change-pos)
                      nil

                      # else
                      # start from the beginning of the line that was changed
                      (- (line-of-i gb change-pos) 1)))

  (when start-line-i
    # not sure why we need to go one line back
    # but if we don't, we get the scrolling bug
    # maybe this can be removed if I rework the refocus-code
    (-- start-line-i)
    # if we're on a word-wrapped line, we recalculate the whole line
    (while (= :word-wrap (get line-flags start-line-i))
      (-- start-line-i)))

  (set start-line-i (if (neg? start-line-i) nil start-line-i))

  (def width (inner-width gb))
  (def h (* (gb :text/size) (gb :text/line-height)))

  (def start-i (get lines start-line-i 0))

  (default line-limit 999999999999)

  (def old-y-pos (get y-poses start-line-i 0))
  (def old-line-number (get line-numbers start-line-i 1))

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  # remove everything after start
  # since start is where a change (might) have happened

  (let [start-line-i (or start-line-i 0)]
    (array/remove lines start-line-i (length lines))
    (array/remove y-poses start-line-i (length y-poses))
    (array/remove line-flags start-line-i (length line-flags))
    (array/remove line-numbers start-line-i (length line-numbers))
    (invalidate-cache-hook gb start-line-i))

  #(array/clear lines)
  #(array/clear y-poses)
  #(array/clear line-flags)
  #(array/clear line-numbers)

  (var x 0)
  (var y old-y-pos)
  (var w 0)
  (var line-number old-line-number)

  # used for hooks to get the text of the current line
  (def current-line @"")

  (var s (buffer/new 1))
  (var beginning-of-word-i 0)

  (def {:offset offset
        :width-of-last-line-number width-of-last-line-number} gb)
  (def [x-scale _] screen-scale)

  (def treshold (- width (offset 0) (or width-of-last-line-number 0)))

  (put gb :checked-word nil)

  (defn new-line
    [line-i line-number flag line-h]

    (array/push lines line-i)
    (array/push line-numbers line-number)
    (array/push y-poses y)
    (array/push line-flags flag)
    (+= y line-h)

    (new-line-hook gb (dec (length lines)) current-line)

    (buffer/clear current-line))

  (defn check-if-word-is-too-wide
    [old-w i c]

    (put-in gb [:checked-word :fst :treshsh i] (> (+ x old-w) treshold))

    (if (> (+ x old-w) treshold) ## we went outside the max width
      (do ## so rowbreak before the word

        (put-in gb [:checked-word :fst i] beginning-of-word-i)

        (when (and (not= beginning-of-word-i 0)
                   (or (empty? lines)
                       (not= (inc (last lines)) beginning-of-word-i)))
          # debug-info
          # (put-in gb [:checked-word :fst :beginning-of-word-i beginning-of-word-i x] true)
          (new-line beginning-of-word-i line-number :word-wrap h))
        (set x old-w)

        (when (> (+ x old-w) treshold)
          (put-in gb [:checked-word :break-up beginning-of-word-i x] true)

          ## then we need to break up the word
          (var break-i beginning-of-word-i)

          (while (set break-i (-?> (index-passing-max-width
                                     gb
                                     break-i
                                     i
                                     treshold)
                                   dec))
            (new-line break-i line-number :word-wrap h)

            # after breaking up the word
            # set the x to the width of the last line
            (set x (width-between gb (last lines) (inc i)))
            # tbh not sure why I need this
            #(+= x (first (get-size sizes c)))

            (put-in gb [:checked-word :break-up beginning-of-word-i :after-x] x))))

      (+= x (+ old-w (first (get-size sizes c))))))

  (gb/gb-iterate
    gb
    start-i
    stop
    i c

    (buffer/push current-line c)

    (case c gb/newline
      (do (check-if-word-is-too-wide w i c)
        (new-line i line-number :regular h)
        (++ line-number)
        (set x 0)
        (set w 0)
        (set beginning-of-word-i (inc i)))

      (chr "\r") (do)

      gb/space
      (let [old-w w]
        (set w 0)

        (check-if-word-is-too-wide old-w i c)

        (set beginning-of-word-i (inc i)))

      (let [new-w (+ w (first (get-size sizes c)))]
        (set w new-w)))

    # TODO: this seem to be called WAY too much!
    (when (or (> (+ y y-offset) y-limit)
              (>= line-number line-limit))
      (return stop-gb-iterate)))

  (put-in gb [:checked-word :too-far :is-it?] (not (> (+ y y-offset) y-limit)))

  (comment when (gb :id)
           (tracev stop)
           (tracev (gb-length gb))
           (tracev (not (> (tracev (+ y y-offset)) (tracev y-limit)))))

  # this should only be run if bottom of buffer is not visible
  (when (not (> (+ y y-offset) y-limit))
    (let [old-w w]
      (set w 0)

      (comment
        # debug
        (put-in gb [:checked-word :too-far :x-w] (+ x old-w))
        (put-in gb [:checked-word :too-far :threshold]
                treshold)
        (put-in gb [:checked-word :too-far :last-line]
                (last lines))
        (put-in gb [:checked-word :too-far :beg-wo]
                beginning-of-word-i)
        #
)

      (if (> (+ x old-w) treshold) ## we went outside the max width

        (do ## so rowbreak before the word

          (when (and
                  (not= beginning-of-word-i 0)
                  (not= (inc (last lines)) beginning-of-word-i))
            # debug
            # (put-in gb [:checked-word :end-fst beginning-of-word-i x] true)
            (new-line beginning-of-word-i line-number :word-wrap h))

          (set x old-w)

          ## is the word also longer than the line?
          (when (> (+ x old-w) treshold)
            ## then we need to break up the word
            (put-in gb [:checked-word :end-break-up beginning-of-word-i x] true)
            (var break-i beginning-of-word-i)
            (while (set break-i (-?> (index-passing-max-width
                                       gb
                                       break-i
                                       stop
                                       treshold)
                                     dec))
              (new-line break-i line-number :word-wrap h))))

        (do
          (put-in gb [:checked-word :end-fst] nil)
          (put-in gb [:checked-word :end-break-up] nil))))

    # we always add a line at the end -- just to have the last element in lines
    # makes it easier when looping over lines
    (new-line stop line-number :regular h))

  lines)

(var debug nil)
(set debug true)
(set debug false)

(defn in-selection?
  [{:selection selection :caret caret} i]
  (when selection
    (let [start (min selection caret)
          stop (max selection caret)]
      (and (>= i start)
           (< i stop)))))

(defn render-line-bg
  [gb y
   line-start line-stop
   start stop
   color]
  (def {:selection selection
        :colors colors
        :caret caret
        :offset offset
        :width-of-last-line-number width-of-last-line-number
        :highlight highlight} gb)

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))
  (def [x-scale y-scale] screen-scale)

  (when (and (< line-start stop)
             (>= line-stop start))

    (var line-h (* (gb :text/size) (gb :text/line-height)))
    (var start-x nil)
    (var stop-x nil)
    (var acc-w 0)

    (gb/gb-iterate
      gb
      start stop
      i c
      (let [[w h] (get-size sizes c)]
        (when (and (nil? start-x)
                   (>= i line-start))
          (set start-x acc-w))

        (+= acc-w (* x-scale w))

        (when (and start-x
                   (< i line-stop))
          (set stop-x acc-w))

        (set line-h (max line-h h))))

    # when selection moves past end of line
    # render the newline as a space
    (when (and stop-x (< stop line-stop))
      (+= stop-x (first (sizes (chr " ")))))

    (when (and start-x
               stop-x)
      (draw-rectangle-rec
        [(+ (offset 0)
            width-of-last-line-number
            start-x)
         (+ (* y y-scale) (offset 1))
         (- stop-x start-x)
         (* y-scale line-h)]
        color
        #:blue
        #:red
))))

(defn render-selection-box
  [gb start stop y]
  (def {:selection selection
        :colors colors
        :caret caret
        :offset offset
        :width-of-last-line-number width-of-last-line-number
        :highlight highlight} gb)

  (def {:selection selection
        :colors colors
        :caret caret
        :offset offset
        :width-of-last-line-number width-of-last-line-number
        :highlight highlight} gb)

  (when selection
    (let [sel-start (min selection caret)
          sel-stop (max selection caret)]
      (render-line-bg
        gb y
        sel-start
        sel-stop
        start
        stop
        (colors :selected-text-background)))))

(defn abs-x
  [gb x]
  (+ ((gb :position) 0)
     x))

(defn rel-text-x
  [gb x]
  (+ ((gb :offset) 0)
     (gb :width-of-last-line-number)
     x))

(defn abs-text-x
  [gb x]
  (def [x-scale _] screen-scale)
  (+ ((gb :position) 0)
     (/ ((gb :offset) 0) x-scale)
     (/ (gb :width-of-last-line-number)
        x-scale)
     x))

(defn abs-text-y
  [gb y]
  (+ ((gb :position) 1)
     ((gb :offset) 1)
     y))

(defn rel-y
  [gb y]
  (+ ((gb :offset) 1)
     y))

(comment
  (defn measure-text
    [tc text]
    (measure-text-ex (tc :font)
                     text
                     (math/floor (* (tc :mult) (tc :size)))
                     (* (tc :mult) (tc :spacing))))
  #
)

(defn gb-draw-text
  [gb text pos color]
  (def font (a/font (gb :text/font) (gb :text/size)))
  (tr/draw-text*2 font
                  text
                  pos
                  (gb :text/size)
                  (gb :text/spacing)
                  color
                  (screen-scale 0)))

(defn gb-measure-text
  [gb text]
  (def font (a/font (gb :text/font) (gb :text/size)))
  (tr/measure-text*2 font
                     text
                     (gb :text/size)
                     (gb :text/spacing)
                     (screen-scale 0)))

(defn render-styling
  [[start stop styling] gb line-start line-stop y]
  (def {:colors colors} gb)

  (when-let [{:background bg} styling]
    (render-line-bg
      gb
      y
      start stop
      line-start
      line-stop
      bg)))

(defmacro next-styling
  [start stop]
  ~(do
     (def active-stylings @[])

     (while (and (< current-styling-i (length styling))
                 (> ,stop (first (in styling current-styling-i))))
       (def cs (in styling current-styling-i))
       (when (and (< ,start (in cs 1))
                  (>= ,stop (in cs 0)))
         (array/push active-stylings cs))

       (if (> ,stop (in cs 1))
         (++ current-styling-i)
         (break)))
     active-stylings))

(defn render-lines
  ``
  Renders the lines in gap buffer.
  Also renders selection boxes.
  Render lines doesn't modify anything in gb.
  ``
  [sizes gb lines start-index h y-limit]
  (def {:delim-ps delim-ps
        :y-poses y-poses
        :line-numbers line-numbers
        :highlighting highlighting
        :offset offset
        :colors colors
        :styling styling} gb)
  (def [x-scale y-scale] screen-scale)
  (def [x-offset y-offset] offset)

  (def default-text-color (colors :text))

  (var delim-i 0)
  (var hl-i 0)

  (var last-gb-index start-index)

  (var s (buffer/new 1))

  (var x 0)

  (def styling (gb :styling))

  (var current-styling-i (if (empty? styling)
                           nil
                           0))

  # position relative to local top
  # local top = y offset
  # if we scrolled 30px, and y offset is 5px
  # line-start-y is 35px, which means we skip lines above 35px
  (def line-start-y (+ (+ #(offset 1)
                          # we do max here to avoid lines popping in during animation
                          (- (max (gb :render-scroll) # (gb :scroll)
)))))

  # current line
  (var line-i nil)

  ### first skip all lines that are outside of the screen
  ### this would be lines scrolled path

  (loop [i :range [0 (length lines)]
         :let [line-y (y-poses i)]]
    (when (>= line-y (- line-start-y (* h 2))) # subtract h so partial lines will render
      (set last-gb-index (if (= i 0)
                           0
                           (lines i)))
      (set line-i i)
      (break)))

  # just used for debugging
  (var nof-lines 0)

  #(def debug2 (-?>> (gb :path) (string/find "lul")))

  (if (= line-i nil)
    :do-nothing
    (loop [i :range [line-i (length lines)]
           :let [l (lines i)
                 line-y (y-poses i)
                 target-y (rel-y gb (- line-y line-start-y))]
           :until (> target-y y-limit)]

      (++ nof-lines)
      (set x 0)

      (loop [s :in (or (next-styling last-gb-index l) [])]
        (render-styling s gb last-gb-index l (- line-y line-start-y)))

      (render-selection-box gb last-gb-index l (- line-y line-start-y))

      ### render line numbers
      (when (gb :show-line-numbers)
        (let [lns (string/format "%d" (line-numbers i))]
          (gb-draw-text gb
                        lns
                        [0
                         (rel-y gb (* y-scale (- line-y line-start-y)))]
                        :gray)))

      (gb/gb-iterate
        gb
        last-gb-index l
        i c
        (let [[w h] (get-size sizes c)]

          (put s 0 c)

          (do
            (while (and delim-ps
                        (< delim-i (length delim-ps))
                        (< (first (delim-ps delim-i)) i))
              (++ delim-i))

            (while (and highlighting
                        (< hl-i (length highlighting))
                        (< ((highlighting hl-i) 1) i))
              (++ hl-i)))

          (gb-draw-text gb
                        s
                        [(math/floor (rel-text-x gb x))
                         (math/floor (* y-scale target-y))]
                        (cond (in-selection? gb i)
                          :white

                          (and delim-ps
                               (< delim-i (length delim-ps))
                               (= ((delim-ps delim-i) 0) i))
                          (get rb/colors ((delim-ps delim-i) 1) :pink)

                          (and highlighting
                               (< hl-i (length highlighting))
                               (<= ((highlighting hl-i) 0) i))
                          (get colors ((highlighting hl-i) 2) default-text-color)

                          # else
                          (get gb :text/color
                               default-text-color)))

          (+= x (* x-scale w))))

      (set last-gb-index l))))

(defn current-line
  [gb]
  (line-of-i gb (gb :caret)))

(defn current-line-number
  [gb]
  ((gb :line-numbers) (line-of-i gb (gb :caret))))

(defn index-above-cursor-on-line
  [gb line]
  (let [{:lines lines
         :caret-pos caret-pos
         :memory-of-caret-x-pos mocxp} gb]
    (if (<= 0 line)
      (or (index-passing-max-width
            gb
            (get lines (dec line) 0) ## start of prev line
            (lines line) ## end of prev line
            mocxp) ## current x position
          (lines line))
      0)))

(defn page-up!
  [gb]
  (if (zero? (gb :scroll))
    (-> gb
        gb/deselect
        reset-blink
        (gb/put-caret 0))
    (let [target (+ (- (gb :scroll))
                    (* 2 (gb :text/size)))
          line (dec (binary-search-closest (gb :y-poses) |(compare target $)))]
      (-> gb
          gb/deselect
          reset-blink
          (gb/put-caret (max 0 (index-above-cursor-on-line gb line)))
          (update :scroll + (- (height gb) (* 3 (gb :text/size))))
          (update :scroll min 0)
          (put :changed-scroll true)))))

(defn page-down!
  [gb]
  (def lines (gb :lines))
  (def y-poses (gb :y-poses))
  (def line-flags (gb :line-flags))
  (def line-numbers (gb :line-numbers))

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (def target-pos (+ (height gb)
                     (- (gb :scroll))
                     (- (* 2 (gb :text/size)))))

  (let [line (binary-search-closest
               (gb :y-poses)
               |(compare target-pos $))]
    (if (= line (length (gb :lines)))
      (-> gb
          gb/deselect
          reset-blink
          (gb/put-caret (gb/gb-length gb)))
      (-> gb
          gb/deselect
          reset-blink
          (gb/put-caret (index-above-cursor-on-line gb line))
          (update :scroll - (- (height gb) (* 3 (gb :text/size))))
          (update :scroll min 0)
          (put :changed-scroll true)))))

(defn index-above-cursor
  [gb]
  (let [{:lines lines
         :caret-pos caret-pos
         :memory-of-caret-x-pos mocxp} gb
        prev-line (dec (current-line gb))]
    (if (<= 0 prev-line)
      (or (index-passing-max-width
            gb
            (get lines (dec prev-line) 0) ## start of prev line
            (lines prev-line) ## end of prev line
            mocxp) ## current x position
          (lines prev-line))
      0)))

(defn index-below-cursor
  [gb]
  (let [{:lines lines
         :caret-pos caret-pos
         :memory-of-caret-x-pos mocxp} gb
        next-line (inc (current-line gb))]
    (if (< next-line (length lines))
      (or (index-passing-max-width
            gb
            (get lines (dec next-line) 0) ## start of next line
            (lines next-line) ## end of next line
            mocxp) ## current x position
          (lines next-line))
      (gb/gb-length gb))))

(defn index-start-of-line
  [gb]
  (let [{:lines lines
         :line-flags line-flags} gb
        prev-line (dec (current-line gb))]
    (if (<= 0 prev-line)
      (if (= :regular (line-flags prev-line))
        (inc (lines prev-line))
        (lines prev-line))
      0)))

(defn move-to-start-of-line
  [gb]
  (-> gb
      gb/deselect
      (gb/put-caret (index-start-of-line gb))
      (put :stickiness :down)
      (put :changed-x-pos true)))

(defn select-to-start-of-line
  [gb]
  (unless (gb :selection)
    (put gb :selection (gb :caret)))

  (-> gb
      (gb/put-caret (index-start-of-line gb))
      (put :stickiness :down)
      (put :changed-selection true)
      (put :changed-x-pos true)))

(defn move-to-end-of-line
  [gb]
  (-> gb
      gb/deselect
      (gb/put-caret ((gb :lines) (current-line gb)))
      (put :stickiness :right)
      (put :changed-x-pos true)))

(defn select-to-end-of-line
  [gb]
  (unless (gb :selection)
    (put gb :selection (gb :caret)))

  (-> gb
      (gb/put-caret ((gb :lines) (current-line gb)))
      (put :stickiness :right)
      (put :changed-selection true)
      (put :changed-x-pos true)))

(defn focus-pos
  [gb pos]
  (-> gb
      (put :scroll (/ (-> (- (- (pos 1))
                             ((gb :offset) 1)
                             ((gb :position) 1)
                             (- (* 0.5 (- (min (get-screen-height)
                                               (height gb))
                                          ((gb :offset) 1)
                                          ((gb :position) 1)))))
                          (min 0))

                      1 #mult
))
      (put :changed-scroll true)))

(defn focus-caret
  [gb]
  (let [[x y] (gb :caret-pos)
        y y # (+ y
        #  (- ((gb :y-poses) (line-of-i (gb :caret)))
        #     (get (gb :y-poses) (max (length (gb :y-poses))
        #                             (inc (line-of-i (gb :caret)))))))
]
    (focus-pos gb [x y])))

(defn move-up!
  [gb]
  (-> gb
      gb/deselect
      reset-blink
      (gb/put-caret (index-above-cursor gb))))


(defn line-number->line
  [gb line-number]

  (def lines (gb :lines))
  (def y-poses (gb :y-poses))
  (def line-flags (gb :line-flags))
  (def line-numbers (gb :line-numbers))

  (word-wrap-gap-buffer
    gb

    (gb/gb-length gb)
    0
    99999999

    :line-limit line-number)

  (var line (binary-search-closest line-numbers |(compare line-number $)))

  (while (= line-number (get line-numbers (dec line)))
    (-- line))

  line)

(defn goto-line-number
  [gb line-number]
  (let [line (line-number->line gb line-number)
        line (min (dec (length (gb :lines))) line)
        index (if (zero? line)
                0
                ((gb :lines) line))]
    (-> gb
        gb/deselect
        reset-blink
        (gb/put-caret index)
        move-to-start-of-line)))

(defn select-move-up!
  [gb]
  (unless (gb :selection)
    (put gb :selection (gb :caret)))

  (-> gb
      (gb/put-caret (index-above-cursor gb))
      (put :changed-selection true)))

(defn i-at-beginning-of-line?
  [gb i]
  (let [l (dec (line-of-i gb i))
        lf (get-in gb [:line-flags l])]
    (= (if (= :word-wrap lf)
         i
         (dec i))
       (get-in gb [:lines l]))))

(defn move-down!
  [gb]

  (gb/deselect gb)

  ### this part is done to set stickiness to down
  # when the caret is at the far left of a line

  (if (i-at-beginning-of-line? gb (gb :caret))
    (put gb :stickiness :down)
    (put gb :stickiness :right))

  (reset-blink gb)

  (gb/put-caret gb (index-below-cursor gb)))

(defn select-move-down!
  [gb]
  (unless (gb :selection)
    (put gb :selection (gb :caret)))

  (-> gb
      (gb/put-caret (index-below-cursor gb))
      (put :changed-selection true)))

(comment
  (index-above-cursor gb-data)
  (index-below-cursor gb-data)
  (move-up! gb-data)
  (move-down! gb-data))

(defn index->pos!
  "Recalculates position and returns it."
  [gb index]

  (def lines (gb :lines))
  (def y-poses (gb :y-poses))
  (def line-flags (gb :line-flags))
  (def line-numbers (gb :line-numbers))

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (word-wrap-gap-buffer
    gb

    index
    0
    99999999)

  (let [pos [(width-between
               gb
               (get lines (dec (line-of-i gb index)) 0)
               index)
             (last y-poses)]
        sizes (a/glyph-sizes (gb :text/font) (gb :text/size))]

    (word-wrap-gap-buffer
      gb

      (gb/gb-length gb)
      0
      (pos 1))

    [(width-between
       gb
       (get lines (dec (line-of-i gb index)) 0)
       index)

     (get y-poses (line-of-i gb index))]))

(comment
  (index->pos! gb-data 11000)

  (do (put gb-data :scroll
           (+ (- (last (index->pos! gb-data
                                    (gb-data :caret))))
              (* 0.25 (height gb-data))))
    (put gb-data :changed-selection true)
    :ok))

(defn index->pos
  [gb index]
  (def {:lines lines
        :y-poses y-poses} gb)
  (let [# line-index (-> (max 0 (binary-search-closest
        #                         lines
        #                         |(compare index $)))
        #                (min (max 0 (dec (length lines)))))
        line-index (line-of-i gb index)
        x (width-between
            gb
            (get lines (dec line-index) 0)
            index)
        y (y-poses line-index)]

    [x y]))

(defn inner-draw
  [gb]
  (def {:position position
        :offset offset
        :size size
        :colors colors
        :scroll scroll
        :changed changed
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :width-of-last-line-number width-of-last-line-number
        :lines lines
        :y-poses y-poses
        :line-flags line-flags}
    gb)

  (def [x-scale y-scale] screen-scale)

  (def [x y] position)
  (def [ox oy] offset)
  (def [w _] size)

  (def screen-w (* x-scale (get-screen-width)))
  (def screen-h (* y-scale (get-screen-height)))

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (render-lines sizes
                gb
                (gb :lines)
                0
                (* y-scale (* (gb :text/size) (gb :text/line-height)))
                (height gb)))

(defn generate-texture
  [gb]
  (begin-texture-mode (gb :texture))
  (rl-push-matrix)

  (rl-load-identity)

  # you can use blank for slightly thinner text
  (clear-background (or (when (not= (gb :scroll)
                                    (gb :render-scroll)) :blank)
                        (gb :background)
                        (get-in gb [:colors :background])
                        :blue))
  (inner-draw gb)
  (rl-pop-matrix)

  (end-texture-mode))

(defn document-bottom
  [gb y]
  (+ y
     (- (gb :scroll))
     (min (get-screen-height) (height gb))))

(defn ensure-render-texture
  [gb]
  (when-let [t (and (gb :resized)
                    (gb :texture))]
    (unload-render-texture t)
    (put gb :texture nil))

  (unless (gb :texture)
    (let [[x-scale y-scale] screen-scale
          w (math/round (* x-scale (width gb)))
          h (math/round (* y-scale (height gb)))
          rt (load-render-texture w h)]
      (put gb :texture rt))))

(defn word-wrap
  [gb]
  (def {:position position
        :offset offset
        :size size
        :colors colors
        :scroll scroll
        :changed changed
        :lowest-changed-at change-pos
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :changed-styling changed-styling
        :resized resized
        :width-of-last-line-number width-of-last-line-number
        :lines lines
        :y-poses y-poses
        :line-flags line-flags
        :line-numbers line-numbers}
    gb)

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (word-wrap-gap-buffer
    gb

    (gb/gb-length gb)
    0
    (+ 0 (- (height gb) scroll))

    :changed changed
    :change-pos change-pos))

(defn refocus-caret
  [gb]
  (let [caret-y ((gb :caret-pos) 1)
        scroll (* 1 #mult
                  (- (gb :scroll)))]
    (when (< caret-y scroll)
      (focus-caret gb))

    # caret-y is relative to document
    (when (and (> caret-y 0)
               (>= caret-y
                   (document-bottom gb (- (* (gb :text/size) (gb :text/line-height))))))

      ## index->pos! recalculates lines etc
      ## in order to find the "real" position of the caret
      ## this function shouldn't be called needlessly
      (put gb :caret-pos (index->pos! gb (gb :caret)))
      (focus-caret gb))))

(defn gb-pre-render
  [gb]
  (def {:position position
        :offset offset
        :size size
        :colors colors
        :scroll scroll
        :changed changed
        :lowest-changed-at change-pos
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :changed-scroll changed-scroll
        :changed-styling changed-styling
        :resized resized
        :width-of-last-line-number width-of-last-line-number
        :lines lines
        :y-poses y-poses
        :line-flags line-flags
        :line-numbers line-numbers}
    gb)

  (unless (gb :render-scroll)
    (put gb :render-scroll (gb :scroll)))

  (def rs-changed (not= (gb :scroll) (gb :render-scroll)))

  (update gb :render-scroll
          (fn [rs]
            (let [scroll (gb :scroll)
                  org-diff (- scroll rs)
                  snap 1
                  diff (* org-diff 0.5)]
              (if (and (> diff (- snap))
                       (< diff snap))
                scroll
                (+ rs diff)))))

  (def [x y] size)
  (def [x-scale y-scale] screen-scale)

  (ensure-render-texture gb)

  (when (or resized
            changed
            changed-nav
            changed-selection
            changed-scroll
            changed-styling
            rs-changed)

    ## DEBUG TODO: REMOVE

    # (e/put! state/editor-state :force-refresh true)

    (when changed
      (put gb :delim-ps (rb/gb->delim-ps gb)))

    (when (or changed changed-scroll)
      (word-wrap gb))

    (put gb :caret-pos (index->pos gb (gb :caret)))

    (when changed-x-pos
      (put gb :memory-of-caret-x-pos (get-in gb [:caret-pos 0]))
      (if (= 0 (gb :memory-of-caret-x-pos))
        (put gb :stickiness :down)
        (put gb :stickiness :right)))

    (when (and (not (gb :dont-refocus)) (or changed changed-nav))
      # can be changed by refocus-caret
      (put gb :changed-scroll nil)

      (refocus-caret gb))

    # refocusing can cause scroll to change
    # when that happens, we word wrap again,
    # since letters further down might be in view
    (when (gb :changed-scroll)
      (word-wrap gb))

    ### TODO: figure out where to refocus

    (when (or resized
              changed
              changed-selection
              changed-styling
              (get gb :changed-scroll changed-scroll)
              rs-changed)

      (put gb :width-of-last-line-number
           (if (gb :show-line-numbers)
             (first (gb-measure-text gb (string/format "%d" (length lines))))
             0))

      (generate-texture gb))

    (put gb :lowest-changed-at nil)
    (put gb :resized nil)
    (put gb :changed false)
    (put gb :changed-x-pos false)
    (put gb :changed-nav false)
    (put gb :changed-scroll false)
    (put gb :changed-selection false)))

(defn gb-render-text
  [gb]
  (def {:position position
        :offset offset
        #        :sizes sizes
        :size size
        :colors colors
        :scroll scroll
        :changed changed
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :width-of-last-line-number width-of-last-line-number
        :lines lines
        :y-poses y-poses
        :scroll scroll}
    gb)

  (def [x-scale y-scale] screen-scale)

  (def [x y] position)

  (def screen-w (* x-scale (get-screen-width)))
  (def screen-h (* y-scale (get-screen-height)))

  (rl-push-matrix)

  (def h (height gb))
  (def w (width gb))

  #  (rl-mult-matrixf-screen-scale)

  #€   (rl-load-identity)

  (def col (gb :text/color))
  (def text/size (gb :text/size))
  (def text/spacing (gb :text/spacing))

  (let [mag (- (gb :scroll) (gb :render-scroll))]
    (when (> (math/abs mag) 10)

      (put gb :text/color 0x00ff00ff)

      #(rl-translatef (* 0.003 (math/abs mag) (dec (* 2 (math/random))))
      #               (* 0 mag (math/random)) 0)

      (rl-push-matrix)

      #(rl-scalef 0 #(inc (* -0.0001 (math/abs mag))
      #           (inc (* -0.0001 (math/abs mag))) 0)

      (def smear-y (- (min 25 (max -25 (* 0.05 mag)))))

      (rl-translatef 0 smear-y 0)

      (do #comment

        (def stretch-y (inc (* 0.002 (math/abs mag))))

        (def y-diff
          (if (neg? mag)
            (- (* h stretch-y)
               h)
            0))

        (draw-texture-pro
          (get-render-texture-texture2d (gb :texture))
          [0 0 (* x-scale w)
           (* y-scale (- h)) # (- h) #screen-w (- screen-h)
]

          [x
           (- y (* 0.7 y-diff))
           w (* h stretch-y) #(/ screen-w x-scale) (/ screen-h y-scale)
]
          [0 0]
          0
          0x999999955
          #(colors :background)
)

        (draw-texture-pro
          (get-render-texture-texture2d (gb :texture))
          [0 0 (* x-scale w)
           (* y-scale (- h)) # (- h) #screen-w (- screen-h)
]

          [x
           (- y (* 1.1 y-diff))
           w (* h stretch-y) #(/ screen-w x-scale) (/ screen-h y-scale)
]
          [0 0]
          0
          0x99999922
          #(colors :background)
))

      (rl-pop-matrix))

    ### CLEAR BACKGROUND
    #(clear-background (or (gb :background)
    #                      (colors :background)
    #                      :blue))

    (put gb :text/color col)
    (put gb :text/size text/size)
    (put gb :text/spacing text/spacing)

    #  (inner-draw gb)

    (do #comment
      (def ratio (- 1 (math/abs (* mag 0.00005))))
      (def extra-y (if (pos? mag)
                     0
                     (if (not= 0 mag)
                       (* (- 1 ratio) h)
                       (* (- 1 ratio) h))))

      (draw-texture-pro
        (get-render-texture-texture2d (gb :texture))
        [0 0 (* x-scale w)
         (* y-scale (- (* ratio h))) # (- h) #screen-w (- screen-h)
]

        [x
         (+ y extra-y)
         w (* ratio h) #(/ screen-w x-scale) (/ screen-h y-scale)
]
        [0 0]
        0
        :white
        #(colors :background)
))
    #
)

  (rl-pop-matrix))

(defn render-cursor
  [gb]
  (def {:position position
        :offset offset
        #        :sizes sizes
        :size size
        :colors colors
        :scroll scroll
        :changed changed
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :width-of-last-line-number width-of-last-line-number
        :lines lines
        :y-poses y-poses
        :scroll scroll} gb)
  (rl-push-matrix)

  #(rl-load-identity)

  #  (rl-scalef 2 2 1)

  (when :caret-pos
    (unless (gb :render-caret-pos)
      (put gb :render-caret-pos @[;(gb :caret-pos)]))

    (let [[x y] (gb :render-caret-pos)
          [tx ty] (gb :caret-pos)
          org-diff-x (- tx x)
          diff-x (* org-diff-x 0.7)

          org-diff-y (- ty y)
          diff-y (* org-diff-y 0.7)

          new-x (+ x diff-x)
          new-y (+ y diff-y)]
      (put-in gb [:render-caret-pos 0] new-x)
      (put-in gb [:render-caret-pos 1] new-y)

      (let [[x y] (gb :render-caret-pos)
            cx (abs-text-x gb x)
            cy (abs-text-y gb (+ y scroll))
            extra-x (min 10 (max -10
                                 (*
                                   org-diff-x
                                   -0.5)))
            extra-y (* org-diff-y 0.5)]

        (put gb :dbg-y2 scroll)
        (put gb :dbg-y1 y)
        (put gb :dbg-y (abs-text-y gb (+ y scroll)))

        (draw-line-ex
          [(+ cx extra-x) cy]
          [(+ cx (* 1.5 extra-x))
           (+ cy
              (* 1 (math/abs extra-y))
              (- (* (gb :text/size)
                    (gb :text/line-height))
                 1))]
          (min 3 (inc (if (zero? extra-y)
                        (math/abs extra-x)
                        (math/abs extra-y))))
          (or 0x999999ff (gb :caret/color) (get-in gb [:colors :caret])))

        (draw-line-ex
          [(+ cx extra-x) cy]
          [(+ cx (* extra-x 1.5))
           (+ cy
              (- (* (gb :text/size)
                    (gb :text/line-height))
                 1))]

          1
          (or (gb :caret/color) (get-in gb [:colors :caret]))))

      (rl-pop-matrix))))
