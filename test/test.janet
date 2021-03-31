(import ../ :as jvs)

# This is a simple prototype that implements the jvs interface by storing values
# in the :data field instead of within the primary table itself.
(def MyType
  @{:jvs/get
    (fn [self k &opt v]
      (get-in self [:data k] v))

    :jvs/put
    (fn [self k v]
      (put-in self [:data k] v))

    :jvs/next
    (fn [self &opt k]
      (next (in self :data) k))

    :jvs/length
    (fn [self] (-> (in self :data []) length))

    :jvs/clone
    (fn [self]
      (def clone (table/clone self))
      (update clone :data |(-?> $ table/clone)))})

# Create an instance of a table with the above prototype.
(def custom (table/setproto @{} MyType))

# The custom tables store values within the :data field.
(jvs/put custom :foo "bar")
(jvs/put custom :bar "baz")

# Create a normal table with no special properties.
(def tbl @{})

# When presented with a normal table, jvs/put falls back to the builtin put
# function instead of using a custom implementation.
(jvs/put tbl :foo "bar")

# Confirm that the custom table is structured as expected.
(assert (= 1 (length custom)))
(assert (= 2 (jvs/length custom)))
(assert (= 2 (length (in custom :data))))
(assert (= "bar" (get-in custom [:data :foo])))
(assert (= "baz" (get-in custom [:data :bar])))
(assert (= "bar" (jvs/get custom :foo)))
(assert (= "baz" (jvs/get custom :bar)))

# Confirm that the builtin table is structured as expected.
(assert (= 1 (length tbl)))
(assert (= 1 (jvs/length tbl)))
(assert (= "bar" (in tbl :foo)))
(assert (= "bar" (jvs/get tbl :foo)))

# Cloning the custom table works as expected: the data field is itself cloned
# when would otherwise be assigned to to the new table by reference.
(assert (not= (in custom :data)
              (in (jvs/clone custom) :data)))

# Cloning a builtin table works as expected.
(assert (let [clone (jvs/clone tbl)]
          (and (not= tbl clone)
               (deep= tbl clone))))

# Confirm that the :jvs/next length fallback works.
(put MyType :jvs/length nil)
(assert (= 2 (jvs/length custom)))
