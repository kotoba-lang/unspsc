(ns kotoba.unspsc
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [kotoba.technology :as technology]))

(def registry-resource "kotoba/unspsc/registry.edn")

;; registry.edn is stored as Datomic/Datascript tx-data (a single-entity vector,
;; see schema.edn / scripts/edn-datomize.bb): top-level keys that were already
;; namespaced (:kotoba.registry/*) are unchanged, but the bare :unspsc key
;; (a vector-of-maps, not a Datomic scalar) was pr-str'd into a :kotoba.unspsc/unspsc
;; blob string. Reconstitute the original shape here so downstream callers
;; (segments/by-segment/... below) keep working unchanged against a plain
;; :unspsc vector of un-namespaced segment maps.
(defn registry []
  (let [entity (first (edn/read-string (slurp (io/resource registry-resource))))]
    (-> entity
        (dissoc :db/id :kotoba.unspsc/unspsc)
        (assoc :unspsc (edn/read-string (:kotoba.unspsc/unspsc entity))))))

(defn segments
  ([] (:unspsc (registry)))
  ([reg] (:unspsc reg)))

(defn by-segment
  ([] (by-segment (registry)))
  ([reg] (into {} (map (juxt :segment identity) (segments reg)))))

(defn get-segment
  ([code] (get-segment (registry) code))
  ([reg code] (get (by-segment reg) (str code))))

(defn required-technologies
  ([code] (required-technologies (registry) code))
  ([reg code] (:required-technologies (get-segment reg code))))

(defn optional-technologies
  ([code] (optional-technologies (registry) code))
  ([reg code] (:optional-technologies (get-segment reg code))))

(defn technology-stack
  "Resolve the required technology records for a UNSPSC segment."
  ([code] (technology-stack (registry) code))
  ([reg code]
   (technology/stack (required-technologies reg code))))

(defn readiness
  "Return an execution-readiness summary for a UNSPSC segment and available technology IDs."
  [code available-tech-ids]
  (let [seg (get-segment code)
        required (set (:required-technologies seg))
        available (set available-tech-ids)
        missing (set/difference required available)]
    {:segment (str code)
     :business-id (:business-id seg)
     :ready? (empty? missing)
     :required required
     :available available
     :missing missing
     :operating-states (:operating-states seg)}))

(defn execution-plan
  "Data contract cloud-itonami-unspsc can expose in business state."
  [code]
  (let [seg (get-segment code)
        stack (technology-stack code)]
    {:segment (str code)
     :business-id (:business-id seg)
     :name (:name seg)
     :maturity (:maturity seg)
     :required-technologies (:required-technologies seg)
     :optional-technologies (:optional-technologies seg)
     :operating-states (:operating-states seg)
     :ui-ready? (some :ui? stack)
     :export-ready? (some :export? stack)
     :technology-stack (mapv #(select-keys % [:id :name :layer :capabilities :repos :contracts :ui? :export?])
                             stack)}))

(defn maturity
  "Return the maturity level of a UNSPSC segment entry: :spec (registry
  only), :blueprint (blueprint repo published), or :implemented (source
  actor exists). Defaults to :spec when unset."
  [code]
  (let [seg (get-segment code)]
    (or (:maturity seg)
        (cond
          (:implemented? seg) :implemented
          (:repo seg)         :blueprint
          :else               :spec))))

(defn maturity-summary
  "Aggregate maturity counts across all UNSPSC segment entries."
  []
  (let [segs (segments)]
    {:total       (count segs)
     :spec        (count (filter #(= :spec (maturity (:segment %))) segs))
     :blueprint   (count (filter #(= :blueprint (maturity (:segment %))) segs))
     :implemented (count (filter #(= :implemented (maturity (:segment %))) segs))}))

(defn maturity-roadmap
  "Return the next maturity step for a UNSPSC segment entry:
  :spec->:blueprint->:implemented, with the action required to advance."
  [code]
  (let [seg (get-segment code)
        level (maturity code)
        stack (technology-stack code)
        ui? (some :ui? stack)
        export? (some :export? stack)
        has-repo (boolean (:repo seg))]
    {:segment (str code)
     :maturity level
     :next-step (condp = level
                  :spec        :blueprint
                  :blueprint   :implemented
                  :implemented nil)
     :next-action (condp = level
                    :spec        "publish a blueprint repo (scaffold + blueprint.edn + docs)"
                    :blueprint   "implement the actor (source + tests)"
                    :implemented "at maturity ceiling")
     :ui-ready? ui?
     :export-ready? export?
     :has-repo has-repo}))
