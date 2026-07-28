(ns kotoba.unspsc-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.unspsc :as unspsc]))

(deftest registry-loads
  (let [reg (unspsc/registry)]
    (is (= :kotoba/unspsc (:kotoba.registry/id reg)))
    (is (= 53 (count (unspsc/segments reg))))))

(deftest curated-segments-resolve
  (doseq [seg ["10" "27" "39" "43" "73"]]
    (is (:business-id (unspsc/get-segment seg)))
    (is (seq (unspsc/required-technologies seg)))
    (is (seq (:technology-stack (unspsc/execution-plan seg))))))

(deftest readiness-reports-missing-tech
  (let [r (unspsc/readiness "39" #{:telemetry :forms})]
    (is (false? (:ready? r)))
    (is (contains? (:missing r) :audit-ledger)))
  (is (:ready? (unspsc/readiness "27" #{:robotics :telemetry :optimization :bpmn :audit-ledger}))))

(deftest maturity-tier
  ;; These assert a segment has REACHED a tier, not that it is frozen there.
  ;; A published segment advancing :blueprint -> :implemented is the fleet
  ;; working; segment 27 did exactly that and turned this suite red. The
  ;; original form asserted equality with :blueprint and pinned exact tier
  ;; counts (5 blueprint, 0 implemented), so every promotion was a failure.
  (let [rank {:spec 0 :blueprint 1 :implemented 2}
        at-least (fn [tier id] (>= (rank (unspsc/maturity id) -1) (rank tier)))]
    (testing "published segments have reached at least :blueprint"
      (doseq [id ["10" "27" "39" "43" "73"]]
        (is (at-least :blueprint id) (str "segment " id))))
    (testing "a registry-only segment entry is :spec"
      (is (= :spec (unspsc/maturity "50")))
      (is (= :spec (unspsc/maturity "85")))))
  (testing "maturity-summary counts tiers"
    (let [m (unspsc/maturity-summary)]
      ;; The partition invariant is the real structural check and holds
      ;; regardless of how far the fleet has progressed.
      (is (= (:total m) (+ (:spec m) (:blueprint m) (:implemented m))))
      (is (= 53 (:total m)))
      (is (<= 5 (+ (:blueprint m) (:implemented m)))
          "published segments must not regress below the recorded floor"))))

(deftest maturity-roadmap-next-step
  (is (= :implemented (:next-step (unspsc/maturity-roadmap "10"))))
  (is (= :blueprint (:next-step (unspsc/maturity-roadmap "50")))))
