(ns scheduler.core
  #^{:author "Craig Ludington",
     :doc "Schedule resources."}
  (use [clj-time.core :as time :only (date-time interval overlaps? plus now millis seconds minutes hours days weeks months years)]
       [clj-time.coerce :as coerce :only (from-string to-string)])
  (import org.joda.time.Interval)
  (:gen-class))

(def ^{:doc "Hash of schedules, indexed by resource.  A schedule is a vector of Interval."} schedules (atom {}))

(defn reservation
  ([resource ^String from ^String to]
     (reservation resource (interval (from-string from) (from-string to))))
  ([resource ^Interval interval]
     {:resource resource :interval interval}))

(defn start [r] (.getStart (:interval r)))
(defn end   [r] (.getEnd   (:interval r)))

(defn available?
  "True if the reservation is available (no extant reservation overlaps the requested times)."
  [{:keys [resource interval]}]
  (not (some #(overlaps? interval %) (@schedules resource))))

(defn scheduled?
  "True if a reservation for this resource at this interval has scheduled."
  [{:keys [resource interval]}]
  (some #(= % interval) (@schedules resource)))

(defn reserve
  "Add reservation r to schedules."
  [{:keys [resource interval] :as r}]
  (when (available? r)
    (swap! schedules #(assoc % resource (conj (@schedules resource) interval)))))

(defn group-schedules
  "A map of true (interval for the reservation) and false (intervals for other reservations)."
  [{:keys [resource interval]}]
  (group-by #(= interval %) (@schedules resource)))

(defn cancel
  "Cancel the reservation."
  [{:keys [resource interval] :as r}]
  {:pre [(scheduled? r)]
   :post [(not (scheduled? r))]}
  (let [m (group-schedules r)
        to-cancel (m true)
        to-keep (m false)]
    (swap! schedules #(assoc % resource to-keep))))

(defn acquire
  "Acquire the reservation's resource."
  [{:keys [resource interval] :as r}]
  {:pre [(scheduled? r)]}
  )

(defn complete
  "Release the reservation's resource."
  [r]
  )

(defn test-setup []
  (do
    (reset! schedules {})
    (reserve (reservation :a "2014-01-01" "2015-12-31"))
    (reserve (reservation :a "2016-01-01" "2017-12-31"))
    ;; gap
    (reserve (reservation :a "2018-01-01" "2019-12-31"))

    (reserve (reservation :b "2014-01-01" "2015-12-31"))
    (reserve (reservation :b "2015-01-01" "2016-12-31"))
    (reserve (reservation :b "2016-01-01" "2017-12-31"))

    (reserve (reservation :c "2014-01-01" "2015-12-31")) ))
