(comment

  # example parser/state based on `{:a 1`
  @{:delimiters "{"
    :frames @[@{:args @[]
                :column 0
                :line 1
                :type :root}
              @{:args @[:a 1]
                :column 1
                :line 1
                :type :struct}]}

  )

(defn missing-delims
  [fragment]
  (var missing @"")
  (def p (parser/new))
  (parser/consume p fragment)
  # XXX: in another code base, had a problem w/ parser/eof, but...
  #      parser/eof is necessary for some backtick cases, e.g. not possible
  #      to tell if ``hello`` is complete, as it could be the beginning of
  #      ``hello```!``
  (try
    (parser/eof p)
    ([err]
      (eprintf "parser/eof returned error: %p" err)
      # early return indicating problem
      (break nil)))
  #
  (when-let [state (parser/state p)
             delims (state :delimiters)]
    (when (pos? (length delims))
      (each d (reverse delims)
        (case d
          (chr "(") (buffer/push-string missing ")")
          (chr "[") (buffer/push-string missing "]")
          (chr "{") (buffer/push-string missing "}")
          (chr `"`) (buffer/push-string missing `"`)
          (chr "`") (buffer/push-string missing "`")
          # XXX: should not happen
          (errorf "Unrecognized delimiter character: %s"
                  (string (buffer/push-byte @"" d)))))))
  missing)

(comment

  (missing-delims "(defn a))")
  # => nil

  (missing-delims "(defn a")
  # => @")"

  (missing-delims "{:a 1")
  # => @"}"

  (missing-delims "[:x :y")
  # => @"]"

  (missing-delims
    (string "{:a 1\n"
            " :b"))
  # => @"}"

  (missing-delims
    (string "(defn my-fn\n"
            "  [x]\n"
            "  (+ x 1"))
  # => @"))"

  (missing-delims `"nice string"`)
  # => @""

  (missing-delims `"not quite a string`)
  # => @`"`

  (missing-delims `("what is going on?)`)
  # => @`")`

  (missing-delims "``hello``")
  # => @""

  (missing-delims "``hello```")
  # => @"`"

  (missing-delims "1")
  # => @""

  (missing-delims "")
  # => @""

  (missing-delims
    (string "``\n"
            "  hello"))
  # => @"``"

  )

(defn missing-delims-2
  [fragment]
  (var missing @"")
  (var start-pos nil)
  (var d-type nil)
  #
  (def p (parser/new))
  (parser/consume p fragment)
  # XXX: in another code base, had a problem w/ parser/eof, but...
  #      parser/eof is necessary for some backtick cases, e.g. not possible
  #      to tell if ``hello`` is complete, as it could be the beginning of
  #      ``hello```!``
  (try
    (parser/eof p)
    ([err]
      (eprintf "parser/eof returned error: %p" err)
      # early return indicating problem
      (break nil)))
  #
  (when-let [state (parser/state p)
             delims (state :delimiters)
             last-frame (last (state :frames))]
    (when (pos? (length delims))
      (set start-pos
           [(last-frame :line) (last-frame :column)])
      (set d-type
           (last-frame :type))
      (each d (reverse delims)
        (case d
          (chr "(") (buffer/push-string missing ")")
          (chr "[") (buffer/push-string missing "]")
          (chr "{") (buffer/push-string missing "}")
          (chr `"`) (buffer/push-string missing `"`)
          (chr "`") (buffer/push-string missing "`")
          # XXX: should not happen
          (errorf "Unrecognized delimiter character: %s"
                  (string (buffer/push-byte @"" d)))))))
  [missing start-pos d-type])

(comment

  (missing-delims-2 "(defn a))")
  # => nil

  (missing-delims-2 "(defn a")
  # => [@")" [1 1] :tuple]

  (missing-delims-2 "{:a 1")
  # => [@"}" [1 1] :struct]

  (missing-delims-2 "[:x :y")
  # => [@"]" [1 1] :tuple]

  (missing-delims-2
    (string "{:a 1\n"
            " :b"))
  # => [@"}" [1 1] :struct]

  (missing-delims-2
    (string "(defn my-fn\n"
            "  [x]\n"
            "  (+ x 1"))
  # => [@"))" [3 3] :tuple]

  (missing-delims-2 `"nice string"`)
  # => [@"" nil nil]

  (missing-delims-2 `"not quite a string`)
  # => [@`"` [1 1] :string]

  (missing-delims-2 `("what is going on?)`)
  # => [@`")` [1 2] :string]

  (missing-delims-2 "``hello``")
  # => [@"" nil nil]

  (missing-delims-2 "``hello```")
  # => [@"`" [1 10] :string]

  (missing-delims-2 "@`hello")
  # => [@"`" [1 2] :buffer]

  (missing-delims-2 "1")
  # => [@"" nil nil]

  (missing-delims-2 "")
  # => [@"" nil nil]

  (missing-delims-2
    (string "``\n"
            "  hello"))
  # => [@"``" [1 1] :string]

  )

(defn close-delims
  [maybe-code]
  (def delims (missing-delims maybe-code))
  (string maybe-code delims))

(comment

  (close-delims "(")
  # => "()"

  (close-delims "'(")
  # => "'()"

  (close-delims "~(smile breathe")
  # => "~(smile breathe)"

  (close-delims "{:a 1\n:b 2")
  # => "{:a 1\n:b 2}"

  (close-delims "[1 2 3 5 8")
  # => "[1 2 3 5 8]"

  (close-delims "{:a 1 :b [:x :y")
  # => "{:a 1 :b [:x :y]}"

  (let [maybe-code
        (string "(defn hi\n"
                "  [x]\n"
                "  (+ 3 (* 8\n"
                "          (- 2 1)")]
    (deep=
      #
      (close-delims maybe-code)
      #
      (string maybe-code ")))")))
  # => true

  )
