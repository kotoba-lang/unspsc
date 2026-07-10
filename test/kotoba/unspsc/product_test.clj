(ns kotoba.unspsc.product-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.unspsc.product :as prod]))

(deftest catalog-has-sbom-cad-physics-3d
  (let [sum (prod/catalog-summary)]
    (is (>= (:products sum) 11))
    (is (= (:products sum) (:with-sbom sum) (:with-cad sum) (:with-physics sum)
           (:with-full-twin sum)))
    (is (pos? (:with-gltf-ref sum)))
    (is (== 1.0 (:curated-blueprint-coverage sum))
        "all curated open-business segments 10/27/39/43/73 have twins")
    (is (empty? (:curated-blueprint-missing sum)))
    (is (= #{"10" "27" "39" "43" "73"} (set (:curated-blueprint-hits sum))))
    (is (== 1.0 (:seed-traffic-coverage sum))
        "seed-traffic segments 25/26/50/51/53 all have twins")
    (is (empty? (:seed-traffic-missing sum)))
    (is (some #{"43" "25" "26" "27" "10" "39" "73" "50" "51" "53"} (:segments sum)))))

(deftest smartphone-twin-roundtrip-projectors
  (let [p (prod/by-id "prod.smartphone-flagship")]
    (is (= "43191501" (:product/unspsc p)))
    (is (= "43" (:product/unspsc-segment p)))
    (is (= "cloud-itonami-unspsc-43" (:product/open-business-id p)))
    (is (seq (prod/sbom-lines p)))
    (is (some #(= :software (:component/role %)) (prod/sbom-lines p)))
    (let [cdx (prod/sbom-cyclonedx-lite p)]
      (is (= "CycloneDX" (:bomFormat cdx)))
      (is (= "43191501"
             (->> (get-in cdx [:metadata :component :properties])
                  (some #(when (= "unspsc:commodity" (:name %)) (:value %))))))
      (is (seq (:components cdx))))
    (let [feats (prod/cad-features-isekai p)]
      (is (= (count (:product/cad-features p)) (count feats)))
      (is (every? :feat/kind feats))
      (is (every? neg? (map :db/id feats))))
    (let [ir (prod/render-ir p)]
      (is (seq (:instances ir)))
      (is (= "43191501" (get-in ir [:product :unspsc])))
      (is (every? #{:box :cylinder} (map :geo (:instances ir)))))
    (let [gltf (prod/gltf-descriptor p)]
      (is (= "2.0" (get-in gltf [:asset :version])))
      (is (seq (:nodes gltf)))
      (is (= "43191501"
             (get-in gltf [:extensions :KOTOBA_unspsc_product :unspsc]))))
    (let [pp (prod/->product-party-entity p)]
      (is (= "prod.smartphone-flagship" (:product/id pp)))
      (is (= "43191501" (:product/unspsc pp))))
    (let [bom (prod/->plm-bom-props p)]
      (is (seq (:components bom)))
      (is (= "43" (:segment bom))))
    (is (= "cloud-itonami-unspsc-43"
           (get-in (prod/open-business-hints p) [:blueprint-hints :unspsc-blueprint])))))

(deftest by-segment-and-unspsc
  (is (seq (prod/by-segment "43")))
  (is (every? #(= "43" (:product/unspsc-segment %)) (prod/by-segment "43")))
  (is (= "prod.engine-blade-set"
         (:product/id (first (prod/by-unspsc "25101504")))))
  (is (seq (prod/by-unspsc "25"))))

(deftest feature-sbom-summary-matches-cad
  (let [p (prod/by-id "prod.ai-gpu-module")
        s (prod/sbom-feature-summary p)]
    (is (seq s))
    (is (every? :vol s))
    (is (= (count (:product/cad-features p))
           (reduce + 0 (map :qty s))))))

(deftest coverage-entities-carry-brand-owners
  (let [ents (prod/coverage-entities)]
    (is (= (count prod/catalog) (count ents)))
    (is (every? :product/brand-owner ents))
    (is (every? #(re-find #"^org\.corp\.jp\.unspsc-twin-" (:product/brand-owner %)) ents))
    (is (pos? (:product-twins (prod/segment-twin-stats "43"))))
    (is (true? (:has-sbom-twins? (prod/segment-twin-stats "43"))))
    (is (true? (:has-cad-twins? (prod/segment-twin-stats "10"))))))
