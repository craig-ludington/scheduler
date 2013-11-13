(ns scheduler.core
  #^{:author "Craig Ludington",
     :doc "Schedule resources."}
  (use [clj-time.core :as time :only (interval overlaps?)]
       [clj-time.coerce :as coerce :only (from-string to-string)])
  (import org.joda.time.Interval)
  (:gen-class))

(def ^{:doc "Hash of schedules, indexed by resource.  A schedule is a vector of maps with keys :interval (and possibly others)."} schedules (ref {}))

(defn reservation
  "Return a reservation, a map of :interval and :resource (with any additional options merged in)."
  [resource ^String from ^String to & options]
  {:pre  [(or (not options) (and (seq? options) (even? (count options))))]}
  (into {:resource resource :interval (interval (from-string from) (from-string to))} (apply hash-map options)))

(defn available?
  "True if the reservation is available (no extant reservation overlaps the requested times)."
  [{:keys [resource interval]}]
  (not (some #(overlaps? interval (:interval %)) (@schedules resource))))

(defn scheduled?
  "True if reservation is scheduled."
  [{:keys [resource interval]}]
  (some #(= interval (:interval %)) (@schedules resource)))

(defn reserve
  "Add reservation r to schedules."
  [{:keys [resource interval] :as r}]
  (dosync
   (when (available? r)
     (alter schedules assoc resource (conj (@schedules resource) r)))))

(defn release
  "Release the reservation."
  [{:keys [resource interval] :as r}]
  {:pre  [(scheduled? r)]
   :post [(not (scheduled? r))]}
  (dosync (alter schedules assoc resource (remove #(= interval (:interval %)) (@schedules resource)))))
