(import spork/path)
(import ./file-handling)
(import ./state)
(import ./event/subscribe :as s)
(import ./theme)
(import ./file-handling :as fh)
(import ./render_new_gap_buffer :as rgb)

(varfn checkpoint-date
  []
  (let [{:year year
         :month month
         :month-day month-day} (os/date)]
    (string/format "%04d-%02d-%02d" year month month-day)))

(varfn checkpoint-time
  []
  (let [{:hours hours
         :minutes minutes
         :seconds seconds} (os/date)]
    (string/format "%02d_%02d_%02d" hours minutes seconds)))

(varfn path->checkpoint-dir
  [path]
  (let [path (path/abspath path)
        # need to do this on windows for e.g. `C:`
        path (string/replace-all ":" "_COLON_" path)
        parts (path/parts path)
        root-checkpoints-dir (file-handling/data-path "checkpoints")
        checkpoint-dir
        (path/join root-checkpoints-dir
                   ;(array/slice parts 0 -2)
                   (string ".freja-checkpoint-" (last parts)))]
    checkpoint-dir))

(comment
  #
  (path->checkpoint-dir "aoeu")
  #
)

(varfn save-checkpoint
  [path note]
  # only allow characters that are OK in a path
  # TODO: remove more non-ok characters
  (let [note (string/replace-all path/sep "_SLASH_" note)
        note (string/replace-all ":" "_COLON_" note)
        checkpoint-dir (path->checkpoint-dir path)
        day-dir (string checkpoint-dir path/sep (checkpoint-date))
        checkpoint-path (string day-dir path/sep (checkpoint-time) " " note)]

    (reduce (fn [acc cur]
              (if-not acc
                cur
                (let [new (string acc path/sep cur)]
                  (os/mkdir new)
                  new)))
            nil
            (string/split path/sep day-dir))

    (with [f (file/open checkpoint-path :wn)]
      (with [org-f (file/open path :rn)]
        (def content (file/read org-f :all))
        (file/write f content)))

    (printf "Saved checkpoint: %s" (last (string/split path/sep path)))))

(varfn list-checkpoints
  [path]
  (let [checkpoint-dir (path->checkpoint-dir path)]
    (var days-times @[])
    (loop [dir :in (os/dir checkpoint-dir)
           :let [full-dir (string checkpoint-dir path/sep dir)]]
      (array/push days-times [dir
                              (seq [file :in (os/dir full-dir)]
                                (string full-dir path/sep file))]))
    days-times))


(comment

  (use freja/state)
  (->
    (get-in state/editor-state [:stack 0 1 :editor :gb :path])
    list-checkpoints)
  #
)

(varfn format-filename
  [filename]
  (def peg
    ~{:time (/ (* ':d ':d "_")
               ,(fn [d1 d2] (string d1 d2 ":")))
      :main (* :time :time ':d ':d '(any 1))})
  (string ;(peg/match peg filename)))

(comment
  (format-filename "15_56_56 ueoh")

  #
)


(defn save-file-with-checkpoint
  [props &opt note]
  (def path (props :path))

  (default note "manual save")

  (fh/save-file props)
  (save-checkpoint path note))

(varfn load-file-with-checkpoints
  [props path]
  # this happens on first opening freja, so there might not be a preexisting path
  (when (get-in props [:gb :path])
    (save-file-with-checkpoint (props :gb) (string "before opening " path)))

  (fh/load-file props path)

  # only save a checkpoint if there actually was a file here
  (when (os/stat path)
    (save-checkpoint path "after opening")))

(varfn checkpoint-list
  [props]
  (def {:path path
        :textarea textarea
        :selected selected
        :close close} props)

  [:background {:color (theme/colors :background)}
   [:padding {:all 6}
    [:block {}
     [:padding {:bottom 6}
      [:block {}
       [:clickable {:on-click (fn [_] (close))}
        [:text {:text "Close"
                :color (theme/colors :text)}]]]
      [:text {:text "Checkpoints"
              :color (theme/colors :text)
              :size 28}]]]

    (try
      (let [checkpoints (or (-?> path list-checkpoints) [])]
        [:block {}
         [:block {}
          [:padding {:bottom 12}
           [:text {:text (string
                           "Click on checkpoints below to restore earlier versions of:\n"
                           (path/abspath (tracev path)))
                   :color (theme/colors :text)
                   :size 18}]]]
         ;(seq [[day times] :in (reverse (sort-by first checkpoints))]
            [:padding {:bottom 12}
             [:block {}
              [:text {:size 22
                      :color (theme/colors :text)
                      :text (string day)}]
              ;(seq [fullpath :in (reverse (sort times))]
                 [:clickable {:on-click
                              (fn [_]
                                (when (props :needs-save)
                                  (save-file-with-checkpoint (in textarea :gb) "before moving to checkpoint")
                                  (:put props :needs-save false))
                                (fh/load-file textarea
                                              fullpath)
                                (put-in textarea [:gb :path] path)
                                (:put props :selected fullpath))}
                  [:block {}
                   [:background {:color (when (= selected fullpath)
                                          (theme/colors :text))}
                    [:text {:size 18
                            :color (if (= selected fullpath)
                                     (theme/colors :background)
                                     (theme/colors :text))
                            :text (format-filename (path/basename fullpath))}]]]])]])])
      ([err fib]
        (debug/stacktrace fib err "")
        (if (and (string? err)
                 (peg/find "cannot open directory" err))
          (string err "\n\nthis might be due to no checkpoints existing")
          err)))]])

(defn checkpoint-component
  [props]
  (unless (props :checkpoint-props)
    (let [left-state (get-in state/editor-state [:stack 0 1])
          checkpoint-props
          @{:path (get-in left-state [:editor :gb :path])
            :textarea (left-state :editor)
            :needs-save true
            :close (fn []
                     (put props :checkpoint-props nil)
                     (s/put! state/editor-state :right nil))}]

      (put checkpoint-props :put
           (fn [self k v]
             (s/update! props :checkpoint-props put k v)))

      (put props :checkpoint-props checkpoint-props)))

  [:block {} [checkpoint-list (props :checkpoint-props)]])

(varfn show-checkpoints
  []
  (if-not (= (state/editor-state :right) checkpoint-component)
    (s/put! state/editor-state :right checkpoint-component)
    (do (put state/editor-state :checkpoint-props nil)
      (s/put! state/editor-state :right nil))))

#(save-checkpoint "checkpoint.janet")
(comment
  #
  (do
    (show-checkpoints)
    :ok)
  (list-checkpoints "freja/checkpoint.janet")
  #
)
#(overwrite-checkpoint "checkpoint.janet")

