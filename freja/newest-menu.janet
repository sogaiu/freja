(import freja-layout/sizing/relative :as rs)
(use freja-layout/put-many)

(import ./hiccup :as h)
(import ./event/subscribe :as s)
(import ./state)
(import ./input :as i)
(import ./default-hotkeys :as dh)
(import ./new_gap_buffer :as gb)
(import ./theme)
(import ./find-file)

(use ./defonce)
(use freja-jaylib)

(defonce my-props @{})
(defonce state @{})

(put state :on-event (fn [self {:focus f}]
                       (when (get (f :gb) :open-file)
                         (put self :focused-text-area (f :gb)))))

(def {:label-color label-color
      :hotkey-color hotkey-color
      :damp-color damp-color
      :highlight-color highlight-color
      :bar-bg bar-bg
      :dropdown-bg dropdown-bg}
  theme/comp-cols)

(def kws {:control "Ctrl"})

(defn kw->string
  [kw]
  (get kws
       kw
       (let [s (string kw)]
         (if (one? (length s))
           (string/ascii-upper s)
           s))))

(defn hotkey->string
  [hk]
  (string/join (map kw->string hk) "+"))

(defn menu-row
  [{:f f
    :label label
    :hotkey hotkey}]

  (default hotkey (or
                    (-?> (i/get-hotkey ((state :focused-text-area) :binds) f)
                         hotkey->string)
                    ""))
  (unless hotkey (string "no hotkey for " f))

  [:clickable {:on-click (fn [_]
                           (s/put! my-props :open-menu nil)
                           (f (state :focused-text-area)))}
   [:row {}
    [:align {:horizontal :left
             :weight 1}
     [:padding {:right 40}
      [:text {:color label-color
              :size 22
              :text label}]]]

    [:align {:horizontal :right
             :weight 1}
     [:text {:color hotkey-color
             :size 22
             :text hotkey}]]]])

(defn file-menu
  [props]
  [:shrink {}
   [menu-row
    {:f find-file/find-file-dialog
     :label "Find file"}]
   [menu-row
    {:f dh/open-file-dialog
     :label "Open"}]
   [menu-row
    {:f dh/save-file
     :label "Save"}]
   [menu-row
    {:f dh/quit
     :label "Quit"}]])

(defn edit-menu
  [props]
  [:shrink {}
   [menu-row
    {:f dh/undo!2
     :label "Undo"}]
   [menu-row
    {:f dh/redo!
     :label "Redo"}]
   [menu-row
    {:f dh/cut!
     :label "Cut"}]
   [menu-row
    {:f gb/copy
     :label "Copy"}]
   [menu-row
    {:f dh/paste!
     :label "Paste"}]

   [:padding {:all 8}
    @{:render (fn [{:width w :height h} parent-x parent-y]
                (draw-rectangle 0 0 w (inc h) 0xffffff22))
      :relative-sizing rs/block-sizing
      :children []
      :props {}}]

   [menu-row
    {:f dh/search-dialog
     :label "Search"}]])

(defn hiccup
  [props & children]
  [:event-handler {:on-event
                   (fn [self ev]
                     (when (my-props :open-menu)
                       (when (ev :mouse/release)
                         (s/put! my-props :open-menu nil))))}

   [:padding {:left 0 :top 0}
    [:background {:color bar-bg}
     [:padding {:all 8 :top 4 :bottom 4}
      [:block {}
       [:row {}
        [:padding {:right 8}
         [:clickable {:on-click (fn [_]
                                  (s/put! props :open-menu :file))}
          [:text {:color (if (= (props :open-menu) :file)
                           highlight-color
                           damp-color)
                  :size 22
                  :text "File"}]]]

        [:clickable {:on-click (fn [_]
                                 (s/put! props :open-menu :edit))}
         [:text {:color (if (= (props :open-menu) :edit)
                          highlight-color
                          damp-color)
                 :size 22
                 :text "Edit"}]]]]]]

    (when-let [om (props :open-menu)]
      (case om
        :file
        [:background {:color dropdown-bg}
         [:padding {:all 8
                    :top 3}
          [file-menu props]]]
        :edit
        [:block {}
         [:padding {:right 8}
          [:text {:color 0x00000000 :size 22 :text "File"}]]
         [:background {:color dropdown-bg}
          [:padding {:all 8
                     :top 3}
           [edit-menu props]]]]))]])

(defn init
  []
  (h/new-layer :menu hiccup
               my-props
               :remove-layer-on-error true)

  (s/subscribe! state/focus state))

#
# this will only be true when running load-file inside freja
(when ((curenv) :freja/loading-file)
  (printf "reiniting :)")
  (init))
