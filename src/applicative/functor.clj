(ns applicative.functor
  "Implement functors in clojure with no macros"
  {:author "Ricardo H. Gomez"
   :date #inst "2015-07-21T23:39:39.341-00:00"}
  (:import (clojure.lang IMeta IObj IDeref)))

(defn- get-fmap
  [v]
  (get (meta v) ::fmap))

(defrecord WrappedObject [object]
  IDeref
  (deref [_]
    object))

(defmethod print-method WrappedObject
  [s w]
  ((get-method print-method (class @s)) @s w))

(defn- unwrap
  [v]
  (if-not (instance? WrappedObject v)
    v
    @v))

(defn- wrap
  [v fmap]
  (if (instance? IObj v)
    (vary-meta v assoc ::fmap fmap)
    (wrap (WrappedObject. v) fmap)))

(defn with-fmap
  "returns the given `in` object that is associated with a given fmap implementation"
  [fmap in]
  (wrap in fmap))

(defn wrap-fmap
  "Takes an implementation of fmap: type f (a -> b) -> f a -> f b
  returns a new fmap function with special metadata and also does validation"
  [fmap*]
  (fn fmap [f in]
    (try (fmap* f in)
         (catch Exception e
           (throw (ex-info "fmap failure"
                           {:type ::fmap
                            :original-function fmap*
                            :this-function fmap
                            :function f
                            :input in}
                           e))))))

(defn fmap
  "Applies a function to a functor wrapped value
  returns a value associated with the fmap"
  [f in]
  (wrap ((get-fmap in) f
         (unwrap in))
        (get-fmap in)))
