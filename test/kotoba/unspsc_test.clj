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
  (testing "published blueprint repos are :blueprint"
    (is (= :blueprint (unspsc/maturity "10")))
    (is (= :blueprint (unspsc/maturity "27")))
    (is (= :blueprint (unspsc/maturity "39")))
    (is (= :blueprint (unspsc/maturity "43")))
    (is (= :blueprint (unspsc/maturity "73"))))
  (testing "a registry-only segment entry is :spec"
    (is (= :spec (unspsc/maturity "50")))
    (is (= :spec (unspsc/maturity "85"))))
  (testing "maturity-summary counts tiers"
    (let [m (unspsc/maturity-summary)]
      (is (= (:total m) (+ (:spec m) (:blueprint m) (:implemented m))))
      (is (= 53 (:total m)))
      (is (= 5 (:blueprint m)))
      (is (= 0 (:implemented m))))))

(deftest maturity-roadmap-next-step
  (is (= :implemented (:next-step (unspsc/maturity-roadmap "10"))))
  (is (= :blueprint (:next-step (unspsc/maturity-roadmap "50")))))
