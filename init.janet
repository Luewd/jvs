(def- [get_ put_ next_ reverse_ reverse!_]
  [get put next reverse reverse!])

(defn init
  ```
  Return the value of key in ds. If the key is not present, it will first be
  initialized with the default value for a nested data structure, which is then
  returned. The default value for a nested data structure is, by default, an
  empty table with no prototype. However, the method :jvs/init can override this.
  If init-fn is provided, instead of initializing the final key with the default
  value, it will be initialized with init-fn.
  ```
  [ds key &opt init-fn]
  (match ds
    {:jvs/init init} (init ds key init-fn)

    {:jvs/get get-fn
     :jvs/put put-fn}
    (let [value (get-fn ds key)]
      (if (nil? value)
        (let [value (if init-fn (init-fn) @{})]
          (put-fn ds key value) value)
        value))

    (let [value (get ds key)]
      (if (nil? value)
        (set (ds key) (if init-fn (init-fn) @{}))
        value))))

(defn init-in
  ```
  Access a value in a nested data structure. Looks into the data structure
  via a sequence of keys, but if any keys are missing, they will be initialized
  with the jvs/init function. If init-fn is provided, instead of initializing
  the final key with jvs/init, it will be initialized with init-fn.
  ```
  [ds ks &opt init-fn]
  (if (empty? ks) (error "expected at least 1 key in ks"))
  (if init-fn
    (let [[ks key] [(tuple/slice ks 0 -2) (last ks)]
          ds (reduce init ds ks)]
      (init ds key init-fn))
    (reduce init ds ks)))

(defn get
  ```
  Return the value of key in ds. If the key is not present, return dflt instead.
  The method :jvs/get may be overriden by ds to customize key retrieval.
  ```
  [ds key &opt dflt]
  (match ds
    {:jvs/get get-fn} (get-fn ds key dflt)
    (get_ ds key dflt)))

(defn get-in
  ```
  Access a value of a nested data structure via a sequence of keys. If any of
  the keys are not present, return dflt instead.
  ```
  [ds ks &opt dflt]
  (def value (reduce get ds ks))
  (if (nil? value) dflt value))

(defn put
  ```
  Set the value of a key within the provided data structure. The method :jvs/put
  may be overriden by ds to customize this process. Returns the modified data
  structure ds.
  ```
  [ds key value]
  (match ds
    {:jvs/put put-fn} (put-fn ds key value)
    (put_ ds key value)))

(defn put-get
  ```
  Set the value of a key within the provided data structure. The method :jvs/put
  may be overriden by ds to customize this process. Returns the new value.
  ```
  [ds key value]
  (put ds key value) value)

(defn put-in
  ```
  Put a value into a nested data structure at a location determined by a sequence
  of keys. If any of the keys are not present, they will be initialized according
  to the rules of the jvs/init function. Returns the modified data structure ds.
  ```
  [ds ks value]
  (case (length ks)
    0 (error "expected at least 1 key in ks")
    1 (put ds ks value)
    (let [[ks key] [(tuple/slice ks 0 -2) (last ks)]]
      (put (init-in ds ks) key value) ds)))

(defn put-get-in
  ```
  Put a value into a nested data structure at a location determined by a sequence
  of keys. If any of the keys are not present, they will be initialized according
  to the rules of the jvs/init function. Returns the new value.
  ```
  [ds ks value]
  (put-in ds ks value) value)

(defn update
  ```
  Invoke func with the value of key in ds. The return value of func becomes the
  key's new value. The methods :jvs/get and :jvs/put may be overriden by ds to
  customize the key retrieval and key setting process. Returns the modified data
  structure ds.
  ```
  [ds key func & args]
  (put ds key (func (get ds key) ;args)))

(defn update-get
  ```
  Invoke func with the value of key in ds. The return value of func becomes the
  key's new value. The methods :jvs/get and :jvs/put may be overriden by ds to
  customize the key retrieval and key setting process. Returns the new value.
  ```
  [ds key func & args]
  (put-get ds key (func (get ds key) ;args)))

(defn update-in
  ```
  Invoke func with a value retrieved from a nested data structure via a sequence
  of keys. The return value of func becomes the new value of this key. Returns the
  modified data structure ds.
  ```
  [ds ks func & args]
  (case (length ks)
    0 (error "expected at least 1 key in ks")
    1 (update ds (in ks 0) func ;args)
    (let [[ks key] [(tuple/slice ks 0 -2) (last ks)]]
      (update (reduce init ds ks) key func ;args) ds)))

(defn update-get-in
  ```
  Invoke func with a value retrieved from a nested data structure via a sequence
  of keys. The return value of func becomes the new value of this key. Returns
  the new value.
  ```
  [ds ks func & args]
  (case (length ks)
    0 (error "expected at least 1 key in ks")
    1 (update-get (in ks 0) func ;args)
    (let [[ks key] [(tuple/slice ks 0 -2) (last ks)]]
      (update-get (reduce init ds ks) key func ;args))))

(defn next
  ```
  Get the next key in a data structure, relative to the provided key. Behavior
  is the same as the builtin next function, unless overriden by the table via
  the :jvs/next method.
  ```
  [ds &opt key]
  (match ds
    {:jvs/next next-fn} (next-fn ds key)
    (next_ ds key)))

(defn length
  ```
  Get the length of a data structure. Behavior is the same as the builtin length
  function unless the :jvs/length or :jvs/next methods are present on the table.
  ```
  [ds]
  (match ds
    {:jvs/length length-fn} (length-fn ds)

    {:jvs/next next-fn}
    (do (var [key i] [(next-fn ds nil) 0])
      (while key (++ i)
        (set key (next-fn ds key)))
      i)

    (length ds)))

(def- immutable-types
  @{:tuple true
    :struct true
    :string true
    :keyword true
    :symbol true})
(defn clone
  ```
  Clone the given value. For immutable data types, this is a no-op. For tables
  with the :jvs/clone method, behavior depends on that method. For tables without
  the :jvs/clone method, the table/clone function is used. For arrays and buffers,
  the array/slice and buffer/slice functions are used, respective to the type.
  ```
  [ds]
  (def t (type ds))
  (cond
    (= t :table) (if (def clone-fn (in ds :jvs/clone))
                   (clone-fn ds) (table/clone ds))
    (= t :array) (array/slice ds)
    (= t :buffer) (buffer/slice ds)
    (in immutable-types t) ds
    (errorf "bad slot #0, expected array|tuple|table|struct|buffer|string|keyword|symbol, found %v" ds)))

(defn reverse
  ```
  Reverse the contents of a data structure. By default this will not work on tables
  as they have no meaningful order, however if :jvs/reverse is present, it will be
  used to perform this reversal.
  ```
  [ds]
  (if (dictionary? ds)
    (if (def reverse-fn (in ds :jvs/reverse))
      (reverse-fn ds)
      (error "expected ds to be an iterable type"))
    (reverse_ ds)))

(defn reverse!
  ```
  Reverse the contents of a data structure in place. By default this will not work
  on tables as they have no meaningful order, however if :jvs/reverse! is present,
  it will be used to perform this reversal.
  ```
  [ds]
  (if (dictionary? ds)
    (if (def reverse-fn (in ds :jvs/reverse!))
      (reverse-fn ds)
      (error "expected ds to be an iterable type"))
    (reverse!_ ds)))

(defn- each-impl
  ```
  Shared functionality used by the jvs/each macros.
  ```
  [x ds body item]
  (def $ds (if (idempotent? ds) ds (gensym)))
  (def [next-fn get-fn]
    [(gensym) (if (not= item :key) (gensym))])
  (def getnext (if get-fn [next-fn get-fn] next-fn))
  (with-syms [key]
    ~(do ,;(if (not= $ds ds) [~(def ,$ds ,ds)] [])
       (def ,getnext
         (if (,dictionary? ,$ds)
           ,(if (not= item :key)
              ~[(,in ,$ds :jvs/next ,next_)
                (,in ,$ds :jvs/get ,get_)]
              ~(,in ,$ds :jvs/next ,next_))
           ,(if (not= item :key)
              ~[,next_ ,get_] next_)))
       (var ,key (,next-fn ,$ds nil))
       (while (,not= nil ,key)
         (def ,x
           ,(case item
              :key key
              :value ~(,get-fn ,$ds ,key)
              :pair ~[,key (,get-fn ,$ds ,key)]))
         ,;body
         (set ,key (,next-fn ,$ds ,key))))))

(defmacro each
  ```
  Evaluate body for each value in ds. Iteration may be customized by a table
  that defines a custom :jvs/next method. Returns nil.
  ```
  [x ds & body]
  (each-impl x ds body :value))

(defmacro eachk
  ```
  Evaluate body for each key in ds. Iteration may be customized by a table that
  defines a custom :jvs/next method. Returns nil.
  ```
  [x ds & body]
  (each-impl x ds body :key))

(defmacro eachp
  ```
  Evaluate body for each [k v] pair in ds. Iteration may be customized by a table
  that defines a custom :jvs/next method. Returns nil.
  ```
  [x ds & body]
  (each-impl x ds body :pair))
