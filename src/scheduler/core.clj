(ns scheduler.core
  #^{:author "Craig Ludington",
     :doc "Schedule resources."}
  (use [clj-time.core :as time :only (interval overlaps?)]
       [clj-time.coerce :as coerce :only (from-string to-string)])
  (import org.joda.time.Interval)
  (:gen-class))

(def ^{:doc "Hash of schedules, indexed by resource.  A schedule is a vector of Interval."} schedules (ref {}))

(defn reservation
  ([resource ^String from ^String to]
     (reservation resource (interval (from-string from) (from-string to))))
  ([resource ^Interval interval]
     {:resource resource :interval interval}))

(defn available?
  "True if the reservation is available (no extant reservation overlaps the requested times)."
  [{:keys [resource interval]}]
  (not (some #(overlaps? interval %) (@schedules resource))))

(defn scheduled?
  "True if reservation is scheduled."
  [{:keys [resource interval]}]
  (some #(= % interval) (@schedules resource)))

(defn reserve
  "Add reservation r to schedules."
  [{:keys [resource interval] :as r}]
  (dosync
   (when (available? r)
     (alter schedules assoc resource (conj (@schedules resource) interval)))))

(defn release
  "Release the reservation."
  [{:keys [resource interval] :as r}]
  {:pre  [(scheduled? r)]
   :post [(not (scheduled? r))]}
  (dosync (alter schedules assoc resource (remove #(= interval %) (@schedules resource)))))

