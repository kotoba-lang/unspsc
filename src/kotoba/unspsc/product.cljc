(ns kotoba.unspsc.product
  "Portable UNSPSC-keyed product digital twin: SBOM + CAD features + physics + 3D IR.

  Purpose
  -------
  Give trade items a **shared, pure EDN contract** so cloud-itonami (procurement /
  open-business / PLM lanes) and network-isekai (CAD mode, render-IR viewport)
  can consume the same product description without re-deriving geometry or BOM.

  What this is
  ------------
  - **Product-level** data keyed by 8-digit UNSPSC commodity (+ 2-digit segment).
  - **SBOM**: coarse component tree (CycloneDX-shaped maps; not a full SPDX dump).
  - **CAD**: sketch-less feature tree matching network-isekai `isekai.ui.cad`
    (`:feat/kind` extrude|revolve|boss + pose/size) — kami-cad BREP is the documented
    high-fidelity swap behind the same document.
  - **Physics**: mass, bbox, density, material class (CAD/CAE friendly units: mm, kg).
  - **3D**: render-IR instances (box/cylinder proxies) for WebGPU / isekai viewport.

  What this is NOT
  ----------------
  - Not the etzhayyim 18k commodity table (classification SSoT stays there).
  - Not a full STEP/glTF binary store (binaries stay annex/B2; here is the **descriptor**).
  - Not kami-engine ECS — only data + pure projectors."
  (:require [clojure.string :as str]))

;; ───────────────────────── units / helpers ─────────────────────────

(defn unspsc-segment
  "8-digit (or longer) commodity → 2-digit segment string, or nil."
  [code]
  (when-let [d (some-> code str (str/replace #"[^0-9]" "") not-empty)]
    (when (>= (count d) 2) (subs d 0 2))))

(defn- vol-mm3
  "Axis-aligned volume proxy (mm³) from feature dims."
  [{:keys [w h r] :as _f}]
  (if r
    (* Math/PI r r (or h 1.0))
    (* (or w 1.0) (or w 1.0) (or h 1.0))))

;; ───────────────────────── constructors ─────────────────────────

(defn component
  "One SBOM line. Required: :id :name. Optional: :unspsc :qty :unit :mass-kg
  :material :role (:assembly|:part|:material|:packaging|:software)."
  [{:keys [id name unspsc qty unit mass-kg material role supplier]
    :or {qty 1.0 unit :each role :part}}]
  (cond-> {:component/id (str id)
           :component/name (str name)
           :component/qty (double qty)
           :component/unit unit
           :component/role role}
    unspsc (assoc :component/unspsc (str unspsc))
    mass-kg (assoc :component/mass-kg (double mass-kg))
    material (assoc :component/material material)
    supplier (assoc :component/supplier (str supplier))))

(defn cad-feature
  "One CAD feature (isekai.ui.cad-compatible keys, namespaced for pure maps).
  kind ∈ #{:extrude :revolve :boss}."
  [{:keys [id kind order name x y z w h r]
    :or {x 0 y 0 z 0 order 0}}]
  (cond-> {:feat/id (str id)
           :feat/kind kind
           :feat/order (long order)
           :feat/name (str (or name id))
           :feat/x (double x)
           :feat/y (double y)
           :feat/z (double z)}
    w (assoc :feat/w (double w))
    h (assoc :feat/h (double h))
    r (assoc :feat/r (double r))))

(defn make-physics
  "Physical envelope. Lengths mm, mass kg, density g/cm³."
  [{:keys [mass-kg bbox-mm density-g-cm3 center-of-mass-mm material-class]
    :or {bbox-mm [0.0 0.0 0.0] density-g-cm3 1.0 material-class :mixed}}]
  (let [[bx by bz] bbox-mm]
    {:physics/mass-kg (double (or mass-kg 0.0))
     :physics/bbox-mm [(double bx) (double by) (double bz)]
     :physics/density-g-cm3 (double density-g-cm3)
     :physics/material-class material-class
     :physics/center-of-mass-mm (or center-of-mass-mm
                                    [(/ (double bx) 2.0)
                                     (/ (double by) 2.0)
                                     (/ (double bz) 2.0)])}))

(defn product
  "UNSPSC product digital twin.

  Required: :id :name :unspsc
  Optional: :rev :brand :components :features :physics :gltf-ref :notes :sector
            :aliases (seed/GTIN product ids that should resolve as this twin)"
  [{:keys [id name unspsc rev brand components features physics gltf-ref notes sector aliases]
    :or {rev "A" components [] features [] aliases []}}]
  (let [seg (unspsc-segment unspsc)
        comps (mapv #(if (map? %) (if (:component/id %) % (component %)) %) components)
        feats (mapv #(if (map? %) (if (:feat/id %) % (cad-feature %)) %) features)
        phys (cond
               (nil? physics) (make-physics {})
               (:physics/mass-kg physics) physics
               :else (make-physics physics))]
    (cond-> {:product/id (str id)
             :product/name (str name)
             :product/unspsc (str unspsc)
             :product/unspsc-segment seg
             :product/rev (str rev)
             :product/sbom comps
             :product/cad-features (vec (sort-by :feat/order feats))
             :product/physics phys
             :product/open-business-id (when seg (str "cloud-itonami-unspsc-" seg))
             :product/aliases (mapv str aliases)}
      brand (assoc :product/brand (str brand))
      sector (assoc :product/sector sector)
      gltf-ref (assoc :product/gltf-ref gltf-ref)
      notes (assoc :product/notes notes))))

;; ───────────────────────── SBOM projectors ─────────────────────────

(defn sbom-lines
  "Flat SBOM lines from a product twin."
  [prod]
  (vec (:product/sbom prod)))

(defn sbom-by-role
  "Group SBOM by :component/role."
  [prod]
  (->> (sbom-lines prod)
       (group-by :component/role)
       (into (sorted-map))))

(defn sbom-cyclonedx-lite
  "CycloneDX-shaped map (not a full 1.5 serialize — enough for isekai/doctor).
  Components carry bom-ref, type, name, optional purl-ish unspsc property."
  [prod]
  {:bomFormat "CycloneDX"
   :specVersion "1.5"
   :version 1
   :metadata {:component {:type "device"
                          :name (:product/name prod)
                          :version (:product/rev prod)
                          :bom-ref (:product/id prod)
                          :properties [{:name "unspsc:commodity"
                                        :value (:product/unspsc prod)}
                                       {:name "unspsc:segment"
                                        :value (str (:product/unspsc-segment prod))}]}}
   :components
   (mapv (fn [c]
           (cond-> {:type (case (:component/role c)
                            :software "library"
                            :material "material"
                            :packaging "container"
                            "device")
                    :name (:component/name c)
                    :bom-ref (:component/id c)
                    :properties (cond-> [{:name "qty" :value (str (:component/qty c))}]
                                  (:component/unspsc c)
                                  (conj {:name "unspsc:commodity"
                                         :value (:component/unspsc c)})
                                  (:component/mass-kg c)
                                  (conj {:name "mass-kg"
                                         :value (str (:component/mass-kg c))}))}
             (:component/supplier c)
             (assoc :supplier {:name (:component/supplier c)})))
         (sbom-lines prod))})

(defn sbom-feature-summary
  "isekai.ui.cad/sbom-compatible summary over CAD features (kind/qty/vol)."
  [prod]
  (->> (:product/cad-features prod)
       (group-by :feat/kind)
       (map (fn [[kind fs]]
              {:kind kind
               :qty  (count fs)
               :vol  (reduce + 0.0
                             (map (fn [f]
                                    (vol-mm3 {:w (:feat/w f)
                                              :h (:feat/h f)
                                              :r (:feat/r f)}))
                                  fs))}))
       (sort-by :kind)
       vec))

;; ───────────────────────── CAD / 3D projectors ─────────────────────────

(defn cad-features-isekai
  "Feature maps ready for DataScript / isekai.ui.cad (negative temp eids)."
  [prod]
  (mapv (fn [f i]
          (cond-> {:db/id (- (inc i))
                   :feat/kind (:feat/kind f)
                   :feat/order (:feat/order f)
                   :feat/name (:feat/name f)
                   :feat/x (:feat/x f)
                   :feat/y (:feat/y f)
                   :feat/z (:feat/z f)}
            (:feat/w f) (assoc :feat/w (:feat/w f))
            (:feat/h f) (assoc :feat/h (:feat/h f))
            (:feat/r f) (assoc :feat/r (:feat/r f))))
        (:product/cad-features prod)
        (range)))

(defn- feat->inst [f selected-id]
  (let [sel? (and selected-id (= (:feat/id f) selected-id))
        c (if sel? [0.88 0.66 0.23] [0.62 0.66 0.72])
        base {:pos [(:feat/x f) (:feat/z f) (:feat/y f)]
              :color c :metallic 0.35 :roughness 0.4
              :meta {:feat-id (:feat/id f)
                     :name (:feat/name f)
                     :unspsc-segment (:product/unspsc-segment f)}}]
    (case (:feat/kind f)
      :revolve (assoc base :geo :cylinder
                      :size [(or (:feat/r f) 20.0) (or (:feat/h f) 40.0)])
      (assoc base :geo :box
             :size [(or (:feat/w f) 60.0) (or (:feat/h f) 40.0)]))))

(defn render-ir
  "Render-IR for WebGPU / isekai viewport (same shape as isekai.ui.cad/part->ir)."
  ([prod] (render-ir prod nil))
  ([prod selected-feat-id]
   (let [feats (:product/cad-features prod)
         insts (mapv #(feat->inst % selected-feat-id) feats)
         [sx sz] (reduce (fn [[ax az] {:keys [pos]}]
                           [(+ ax (nth pos 0)) (+ az (nth pos 2))])
                         [0.0 0.0] insts)
         n (max 1 (count insts))
         cx (/ sx n) cz (/ sz n)
         bbox (:physics/bbox-mm (:product/physics prod) [100 100 100])
         far (max 6000.0 (* 20.0 (apply max bbox)))]
     {:globals {:sky {:horizon [0.93 0.92 0.86]
                      :sun-dir [-0.4 -0.85 -0.35]
                      :sun [1 0.96 0.85]}
                :eye [(+ cx 130) 150 (+ cz 200)]
                :target [cx 20 cz]
                :fov 48
                :far far}
      :instances insts
      :product {:id (:product/id prod)
                :unspsc (:product/unspsc prod)
                :segment (:product/unspsc-segment prod)
                :gltf-ref (:product/gltf-ref prod)}})))

(defn gltf-descriptor
  "Logical glTF/GLB descriptor (no binary). Consumers resolve :uri via annex/CDN."
  [prod]
  {:asset {:version "2.0" :generator "kotoba.unspsc.product"}
   :extensionsUsed ["KOTOBA_unspsc_product"]
   :extensions
   {:KOTOBA_unspsc_product
    {:productId (:product/id prod)
     :unspsc (:product/unspsc prod)
     :segment (:product/unspsc-segment prod)
     :physics (:product/physics prod)
     :sbomRef (str (:product/id prod) "#sbom")}}
   :nodes (mapv (fn [f]
                  {:name (:feat/name f)
                   :translation [(:feat/x f) (:feat/y f) (:feat/z f)]
                   :extras {:feat/kind (:feat/kind f)
                            :feat/w (:feat/w f)
                            :feat/h (:feat/h f)
                            :feat/r (:feat/r f)}})
                (:product/cad-features prod))
   :uri (:product/gltf-ref prod)})

;; ───────────────────────── cloud-itonami projectors ─────────────────────────

(defn twin-brand-owner-id
  "Kabuto-valid party id for a twin's representative brand-owner
  (`org.corp.<cc>.…` — required by product-party/valid-party-id?)."
  [prod]
  (let [seg (or (:product/unspsc-segment prod) "00")]
    (str "org.corp.jp.unspsc-twin-" seg)))

(defn ->product-party-entity
  "Map twin → product-party / uchiwake-shaped product entity.
  When `with-brand-owner?` (default true), sets `:product/brand-owner` so
  bulk `import-entities` creates a brand-owner edge without interactive gate."
  ([prod] (->product-party-entity prod true))
  ([prod with-brand-owner?]
   (cond-> {:product/id (:product/id prod)
            :product/name (:product/name prod)
            :product/unspsc (:product/unspsc prod)
            :product/sourcing :representative}
     (:product/brand prod) (assoc :product/brand (:product/brand prod))
     (:product/sector prod) (assoc :product/sector (:product/sector prod))
     with-brand-owner?
     (assoc :product/brand-owner (twin-brand-owner-id prod)))))

(defn ->plm-bom-props
  "Map twin → cloud-itonami.smartphone/bom-artifact style props map."
  [prod]
  {:model (:product/id prod)
   :rev (:product/rev prod)
   :unspsc (:product/unspsc prod)
   :segment (:product/unspsc-segment prod)
   :physics (:product/physics prod)
   :components
   (mapv (fn [c]
           {:item (:component/id c)
            :name (:component/name c)
            :qty (:component/qty c)
            :unspsc (:component/unspsc c)
            :ref (:component/id c)})
         (sbom-lines prod))})

(defn open-business-hints
  "Routing hints aligned with cloud-itonami.product-party/open-business-hints shape."
  [prod]
  (let [seg (:product/unspsc-segment prod)]
    {:product (:product/id prod)
     :unspsc (:product/unspsc prod)
     :unspsc-segment seg
     :blueprint-hints
     (cond-> {:unspsc-blueprint (:product/open-business-id prod)
              :has-cad? (boolean (seq (:product/cad-features prod)))
              :has-sbom? (boolean (seq (:product/sbom prod)))
              :has-physics? (boolean (:product/physics prod))}
       seg (assoc :unspsc-segment seg))}))

;; ───────────────────────── catalog (representative twins) ─────────────────────────

(def catalog
  "Representative UNSPSC product twins for open-business + isekai demos.
   Sourcing is :representative — not manufacturer confidential design data."
  [(product
    {:id "prod.smartphone-flagship"
     :name "Flagship smartphone"
     :unspsc "43191501"
     :brand "demo-brand"
     :sector :electronics
     :gltf-ref "asset://unspsc/43/smartphone-flagship.glb"
     :physics {:mass-kg 0.22 :bbox-mm [75 160 8] :density-g-cm3 2.4
               :material-class :electronics}
     :components
     [(component {:id "part.soc" :name "Application SoC" :unspsc "32101604"
                  :qty 1 :mass-kg 0.004 :material :silicon :role :part})
      (component {:id "part.display-oled" :name "OLED display" :unspsc "32121509"
                  :qty 1 :mass-kg 0.03 :material :glass :role :part})
      (component {:id "part.battery" :name "Li-ion pack" :unspsc "26111710"
                  :qty 1 :mass-kg 0.05 :material :li-ion :role :part})
      (component {:id "part.frame" :name "Aluminum frame" :unspsc "30171500"
                  :qty 1 :mass-kg 0.04 :material :aluminum :role :assembly})
      (component {:id "sw.modem-fw" :name "Baseband firmware" :unspsc "43230000"
                  :qty 1 :role :software})]
     :features
     [(cad-feature {:id "body" :kind :extrude :order 0 :name "body"
                    :x 0 :y 0 :z 0 :w 75 :h 8})
      (cad-feature {:id "screen" :kind :boss :order 1 :name "screen"
                    :x 0 :y 0 :z 4 :w 70 :h 2})
      (cad-feature {:id "camera-bump" :kind :revolve :order 2 :name "camera"
                    :x -20 :y 50 :z 8 :r 8 :h 4})]})

   (product
    {:id "prod.ai-gpu-module"
     :name "AI GPU module"
     :unspsc "43201401"
     :brand "demo-silicon"
     :sector :electronics
     :gltf-ref "asset://unspsc/43/ai-gpu-module.glb"
     :physics {:mass-kg 1.4 :bbox-mm [270 110 40] :density-g-cm3 3.1
               :material-class :electronics}
     :components
     [(component {:id "part.gpu-die" :name "GPU die" :unspsc "32101604"
                  :qty 1 :mass-kg 0.02 :material :silicon :role :part
                  :supplier "org.corp.tw.tsmc"})
      (component {:id "part.hbm" :name "HBM stack" :unspsc "32101602"
                  :qty 4 :mass-kg 0.01 :material :silicon :role :part})
      (component {:id "part.pcb" :name "Module PCB" :unspsc "32101500"
                  :qty 1 :mass-kg 0.2 :material :fr4 :role :assembly})
      (component {:id "part.cooler" :name "Vapor chamber" :unspsc "40101800"
                  :qty 1 :mass-kg 0.35 :material :copper :role :part})]
     :features
     [(cad-feature {:id "pcb" :kind :extrude :order 0 :name "pcb"
                    :x 0 :y 0 :z 0 :w 270 :h 3})
      (cad-feature {:id "die" :kind :boss :order 1 :name "die"
                    :x 0 :y 0 :z 3 :w 40 :h 4})
      (cad-feature {:id "cooler" :kind :revolve :order 2 :name "cooler"
                    :x 0 :y 0 :z 8 :r 50 :h 25})]})

   (product
    {:id "prod.engine-blade-set"
     :name "Turbine engine blade set"
     :unspsc "25101504"
     :sector :aerospace
     :gltf-ref "asset://unspsc/25/engine-blade-set.glb"
     :physics {:mass-kg 12.5 :bbox-mm [400 400 600] :density-g-cm3 8.2
               :material-class :superalloy}
     :components
     [(component {:id "part.blade" :name "HPT blade" :unspsc "25101504"
                  :qty 36 :mass-kg 0.28 :material :ni-superalloy :role :part
                  :supplier "sup-aero-blades"})
      (component {:id "part.disk" :name "Turbine disk" :unspsc "25101500"
                  :qty 1 :mass-kg 4.5 :material :ni-superalloy :role :assembly})
      (component {:id "part.coating" :name "TBC coating" :unspsc "31200000"
                  :qty 1 :mass-kg 0.1 :material :ceramic :role :material})]
     :features
     [(cad-feature {:id "disk" :kind :revolve :order 0 :name "disk"
                    :x 0 :y 0 :z 0 :r 180 :h 40})
      (cad-feature {:id "blade-ring" :kind :extrude :order 1 :name "blade-span"
                    :x 0 :y 0 :z 40 :w 360 :h 200})]})

   (product
    {:id "prod.ev-battery-pack"
     :name "EV battery pack"
     :unspsc "26111701"
     :sector :energy-storage
     :gltf-ref "asset://unspsc/26/ev-battery-pack.glb"
     :physics {:mass-kg 450.0 :bbox-mm [2100 1500 150] :density-g-cm3 1.8
               :material-class :energy-storage}
     :components
     [(component {:id "part.module" :name "Cell module" :unspsc "26111710"
                  :qty 16 :mass-kg 22.0 :material :li-ion :role :part})
      (component {:id "part.bms" :name "BMS controller" :unspsc "32151700"
                  :qty 1 :mass-kg 1.2 :material :electronics :role :part})
      (component {:id "part.tray" :name "Aluminum tray" :unspsc "30171500"
                  :qty 1 :mass-kg 40.0 :material :aluminum :role :assembly})]
     :features
     [(cad-feature {:id "tray" :kind :extrude :order 0 :name "tray"
                    :x 0 :y 0 :z 0 :w 2100 :h 40})
      (cad-feature {:id "modules" :kind :boss :order 1 :name "modules"
                    :x 0 :y 0 :z 40 :w 1900 :h 90})]})

   (product
    {:id "prod.service-tool-kit"
     :name "Industrial service tool kit"
     :unspsc "27111700"
     :sector :tools
     :gltf-ref "asset://unspsc/27/service-tool-kit.glb"
     :physics {:mass-kg 3.2 :bbox-mm [400 250 80] :density-g-cm3 2.0
               :material-class :steel}
     :components
     [(component {:id "part.wrench" :name "Combination wrench" :unspsc "27111700"
                  :qty 8 :mass-kg 0.15 :material :steel :role :part})
      (component {:id "part.case" :name "Tool case" :unspsc "24120000"
                  :qty 1 :mass-kg 1.0 :material :polymer :role :packaging})]
     :features
     [(cad-feature {:id "case" :kind :extrude :order 0 :name "case"
                    :x 0 :y 0 :z 0 :w 400 :h 80})
      (cad-feature {:id "handle" :kind :revolve :order 1 :name "handle"
                    :x 0 :y 120 :z 80 :r 12 :h 30})]})

   ;; Curated open-business blueprint segments 10 / 39 / 73 (complete the five)
   (product
    {:id "prod.hive-sensor-array"
     :name "Apiary hive sensor array"
     :unspsc "10151500"
     :sector :agriculture
     :gltf-ref "asset://unspsc/10/hive-sensor-array.glb"
     :physics {:mass-kg 1.8 :bbox-mm [200 200 80] :density-g-cm3 1.2
               :material-class :electronics}
     :components
     [(component {:id "part.th-sensor" :name "Temp/humidity sensor" :unspsc "41111900"
                  :qty 4 :mass-kg 0.02 :material :electronics :role :part})
      (component {:id "part.radio" :name "LoRa radio" :unspsc "43191500"
                  :qty 1 :mass-kg 0.05 :material :electronics :role :part})
      (component {:id "part.housing" :name "Weather housing" :unspsc "24112400"
                  :qty 1 :mass-kg 0.4 :material :polymer :role :assembly})]
     :features
     [(cad-feature {:id "base" :kind :extrude :order 0 :name "base"
                    :x 0 :y 0 :z 0 :w 200 :h 20})
      (cad-feature {:id "mast" :kind :revolve :order 1 :name "mast"
                    :x 0 :y 0 :z 20 :r 15 :h 50})]})

   (product
    {:id "prod.led-panel-driver"
     :name "LED panel driver module"
     :unspsc "39111500"
     :sector :electrical
     :gltf-ref "asset://unspsc/39/led-panel-driver.glb"
     :physics {:mass-kg 0.65 :bbox-mm [180 90 40] :density-g-cm3 2.1
               :material-class :electronics}
     :components
     [(component {:id "part.psu" :name "Constant-current PSU" :unspsc "39121000"
                  :qty 1 :mass-kg 0.25 :material :electronics :role :part})
      (component {:id "part.pcb-drv" :name "Driver PCB" :unspsc "32101500"
                  :qty 1 :mass-kg 0.08 :material :fr4 :role :assembly})
      (component {:id "part.heatsink" :name "Aluminum heatsink" :unspsc "40101800"
                  :qty 1 :mass-kg 0.2 :material :aluminum :role :part})]
     :features
     [(cad-feature {:id "enclosure" :kind :extrude :order 0 :name "enclosure"
                    :x 0 :y 0 :z 0 :w 180 :h 40})
      (cad-feature {:id "heatsink" :kind :boss :order 1 :name "heatsink"
                    :x 0 :y 0 :z 40 :w 160 :h 25})]})

   (product
    {:id "prod.line-cert-kit"
     :name "Manufacturing line certification kit"
     :unspsc "73101500"
     :sector :industrial-services
     :gltf-ref "asset://unspsc/73/line-cert-kit.glb"
     :physics {:mass-kg 4.5 :bbox-mm [500 300 120] :density-g-cm3 1.5
               :material-class :mixed}
     :components
     [(component {:id "part.gauge" :name "Calibrated gauge block" :unspsc "41111600"
                  :qty 6 :mass-kg 0.3 :material :steel :role :part})
      (component {:id "part.logger" :name "Process logger" :unspsc "43211500"
                  :qty 1 :mass-kg 0.5 :material :electronics :role :part})
      (component {:id "part.case-cert" :name "Hard case" :unspsc "24120000"
                  :qty 1 :mass-kg 1.2 :material :polymer :role :packaging})
      (component {:id "sw.cert-fw" :name "Cert workflow firmware" :unspsc "43230000"
                  :qty 1 :role :software})]
     :features
     [(cad-feature {:id "case" :kind :extrude :order 0 :name "case"
                    :x 0 :y 0 :z 0 :w 500 :h 120})
      (cad-feature {:id "logger" :kind :boss :order 1 :name "logger"
                    :x -80 :y 0 :z 120 :w 100 :h 30})]})

   ;; High-traffic :spec segments present in uchiwake seed / fixture graphs
   ;; (food 50, pharma 51, apparel 53) — SBOM/CAD twins so graph twin-segment
   ;; coverage can reach 1.0 without promoting registry maturity tiers.
   (product
    {:id "prod.beverage-can-330"
     :name "Carbonated soft drink 330ml can"
     :unspsc "50202301"
     :brand "demo-cola"
     :sector :food-beverage
     :aliases ["gtin.05449000000996"]
     :gltf-ref "asset://unspsc/50/beverage-can-330.glb"
     :physics {:mass-kg 0.35 :bbox-mm [66 66 115] :density-g-cm3 1.0
               :material-class :aluminum}
     :components
     [(component {:id "part.can-body" :name "Aluminum can body" :unspsc "24121500"
                  :qty 1 :mass-kg 0.013 :material :aluminum :role :packaging})
      (component {:id "part.syrup" :name "Beverage concentrate" :unspsc "50192400"
                  :qty 1 :mass-kg 0.04 :material :agricultural :role :material})
      (component {:id "part.end" :name "Stay-on tab end" :unspsc "24121500"
                  :qty 1 :mass-kg 0.003 :material :aluminum :role :part})]
     :features
     [(cad-feature {:id "body" :kind :revolve :order 0 :name "can-body"
                    :x 0 :y 0 :z 0 :r 33 :h 115})
      (cad-feature {:id "tab" :kind :boss :order 1 :name "tab"
                    :x 0 :y 0 :z 115 :w 20 :h 3})]})

   (product
    {:id "prod.paracetamol-500-twin"
     :name "Paracetamol 500mg tablet pack"
     :unspsc "51142003"
     :sector :pharma
     :aliases ["prod.paracetamol-500"]
     :gltf-ref "asset://unspsc/51/paracetamol-500.glb"
     :physics {:mass-kg 0.02 :bbox-mm [90 50 15] :density-g-cm3 0.8
               :material-class :pharma}
     :components
     [(component {:id "part.api" :name "Paracetamol API" :unspsc "51142000"
                  :qty 20 :mass-kg 0.0005 :material :chemical :role :material})
      (component {:id "part.blister" :name "Blister pack" :unspsc "24120000"
                  :qty 1 :mass-kg 0.008 :material :polymer :role :packaging})
      (component {:id "part.carton" :name "Carton" :unspsc "24121500"
                  :qty 1 :mass-kg 0.01 :material :paper :role :packaging})]
     :features
     [(cad-feature {:id "carton" :kind :extrude :order 0 :name "carton"
                    :x 0 :y 0 :z 0 :w 90 :h 15})
      (cad-feature {:id "blister" :kind :boss :order 1 :name "blister"
                    :x 0 :y 0 :z 8 :w 80 :h 5})]})

   (product
    {:id "prod.cotton-tee-basic"
     :name "Basic cotton T-shirt"
     :unspsc "53101500"
     :sector :apparel
     :gltf-ref "asset://unspsc/53/cotton-tee-basic.glb"
     :physics {:mass-kg 0.18 :bbox-mm [300 200 20] :density-g-cm3 0.3
               :material-class :textile}
     :components
     [(component {:id "part.fabric" :name "Cotton jersey" :unspsc "11151700"
                  :qty 1 :mass-kg 0.15 :material :cotton :role :material})
      (component {:id "part.thread" :name "Sewing thread" :unspsc "11151500"
                  :qty 1 :mass-kg 0.005 :material :cotton :role :material})
      (component {:id "part.label" :name "Care label" :unspsc "14111800"
                  :qty 1 :mass-kg 0.001 :material :paper :role :part})]
     :features
     [(cad-feature {:id "body" :kind :extrude :order 0 :name "body-panel"
                    :x 0 :y 0 :z 0 :w 300 :h 8})
      (cad-feature {:id "sleeve" :kind :boss :order 1 :name "sleeve"
                    :x 160 :y 0 :z 0 :w 80 :h 6})]})

   ;; Commodity-exact twins for seed products that previously only matched by segment
   (product
    {:id "prod.cotton-tshirt"
     :name "Cotton T-shirt (seed commodity)"
     :unspsc "53102516"
     :sector :apparel
     :gltf-ref "asset://unspsc/53/cotton-tshirt.glb"
     :physics {:mass-kg 0.19 :bbox-mm [310 210 20] :density-g-cm3 0.3
               :material-class :textile}
     :components
     [(component {:id "part.fabric-tee" :name "Cotton jersey body" :unspsc "11151700"
                  :qty 1 :mass-kg 0.16 :material :cotton :role :material})
      (component {:id "part.neck" :name "Rib neck band" :unspsc "11151700"
                  :qty 1 :mass-kg 0.01 :material :cotton :role :part})]
     :features
     [(cad-feature {:id "body" :kind :extrude :order 0 :name "body"
                    :x 0 :y 0 :z 0 :w 310 :h 8})
      (cad-feature {:id "collar" :kind :revolve :order 1 :name "collar"
                    :x 0 :y 90 :z 8 :r 40 :h 6})]})

   (product
    {:id "prod.5g-radio"
     :name "5G radio unit"
     :unspsc "43222609"
     :sector :telecom
     :gltf-ref "asset://unspsc/43/5g-radio.glb"
     :physics {:mass-kg 8.5 :bbox-mm [400 300 150] :density-g-cm3 1.8
               :material-class :electronics}
     :components
     [(component {:id "part.rf" :name "RF front-end" :unspsc "32101500"
                  :qty 1 :mass-kg 1.2 :material :electronics :role :part})
      (component {:id "part.antenna" :name "Antenna array" :unspsc "43191500"
                  :qty 1 :mass-kg 2.0 :material :electronics :role :assembly})
      (component {:id "part.psu-radio" :name "Power module" :unspsc "39121000"
                  :qty 1 :mass-kg 1.5 :material :electronics :role :part})]
     :features
     [(cad-feature {:id "chassis" :kind :extrude :order 0 :name "chassis"
                    :x 0 :y 0 :z 0 :w 400 :h 150})
      (cad-feature {:id "antenna" :kind :boss :order 1 :name "antenna"
                    :x 0 :y 0 :z 150 :w 380 :h 40})]})

   (product
    {:id "prod.ev-vehicle"
     :name "Battery-electric passenger vehicle"
     :unspsc "25101503"
     :sector :automotive
     :gltf-ref "asset://unspsc/25/ev-vehicle.glb"
     :physics {:mass-kg 1800.0 :bbox-mm [4500 1800 1500] :density-g-cm3 0.15
               :material-class :mixed}
     :components
     [(component {:id "part.pack" :name "Traction pack" :unspsc "26111701"
                  :qty 1 :mass-kg 450 :material :li-ion :role :assembly})
      (component {:id "part.motor" :name "Drive motor" :unspsc "26101500"
                  :qty 1 :mass-kg 80 :material :steel :role :part})
      (component {:id "part.body-in-white" :name "BIW shell" :unspsc "25171500"
                  :qty 1 :mass-kg 350 :material :aluminum :role :assembly})]
     :features
     [(cad-feature {:id "body" :kind :extrude :order 0 :name "body"
                    :x 0 :y 0 :z 0 :w 4500 :h 1500})
      (cad-feature {:id "wheel" :kind :revolve :order 1 :name "wheel"
                    :x -1500 :y -700 :z 0 :r 350 :h 200})]})

   (product
    {:id "prod.aircraft-narrowbody"
     :name "Narrowbody commercial aircraft"
     :unspsc "25111500"
     :sector :aerospace
     :gltf-ref "asset://unspsc/25/aircraft-narrowbody.glb"
     :physics {:mass-kg 42000.0 :bbox-mm [38000 35000 12000] :density-g-cm3 0.05
               :material-class :superalloy}
     :components
     [(component {:id "part.fuselage" :name "Fuselage sections" :unspsc "25111500"
                  :qty 1 :mass-kg 12000 :material :aluminum :role :assembly})
      (component {:id "part.wing" :name "Wing box" :unspsc "25111500"
                  :qty 2 :mass-kg 4000 :material :composite :role :assembly})
      (component {:id "part.engine" :name "Turbofan" :unspsc "25101504"
                  :qty 2 :mass-kg 2500 :material :ni-superalloy :role :part})]
     :features
     [(cad-feature {:id "fuselage" :kind :revolve :order 0 :name "fuselage"
                    :x 0 :y 0 :z 0 :r 2000 :h 38000})
      (cad-feature {:id "wing" :kind :extrude :order 1 :name "wing"
                    :x 0 :y 0 :z 5000 :w 35000 :h 800})]})

   (product
    {:id "prod.chocolate-spread-jar"
     :name "Chocolate hazelnut spread jar"
     :unspsc "50161900"
     :sector :food-beverage
     :aliases ["gtin.03017620422003" "gtin.07613035044289"]
     :gltf-ref "asset://unspsc/50/chocolate-spread-jar.glb"
     :physics {:mass-kg 0.85 :bbox-mm [90 90 120] :density-g-cm3 1.1
               :material-class :glass}
     :components
     [(component {:id "part.jar" :name "Glass jar" :unspsc "24121500"
                  :qty 1 :mass-kg 0.25 :material :glass :role :packaging})
      (component {:id "part.spread" :name "Spread fill" :unspsc "50192400"
                  :qty 1 :mass-kg 0.55 :material :agricultural :role :material})
      (component {:id "part.lid" :name "Metal lid" :unspsc "24121500"
                  :qty 1 :mass-kg 0.03 :material :steel :role :part})]
     :features
     [(cad-feature {:id "jar" :kind :revolve :order 0 :name "jar"
                    :x 0 :y 0 :z 0 :r 45 :h 110})
      (cad-feature {:id "lid" :kind :boss :order 1 :name "lid"
                    :x 0 :y 0 :z 110 :w 90 :h 10})]})])

(def curated-blueprint-segments
  "Open-business curated UNSPSC segments that should each have ≥1 twin."
  ["10" "27" "39" "43" "73"])

(def seed-traffic-segments
  "High-traffic :spec segments from uchiwake seed/fixture that should have twins
  for graph twin-segment coverage (not registry maturity promotion)."
  ["25" "26" "50" "51" "53"])

(defn coverage-entities
  "Uchiwake-shaped entity vector for bulk import: every twin as a product
  with a representative brand-owner. Lifts brand-owner coverage and
  open-business blueprint routes when imported into product-party."
  []
  (mapv #(->product-party-entity % true) catalog))

(defn by-id
  ([] (into {} (map (juxt :product/id identity) catalog)))
  ([id] (get (by-id) (str id))))

(defn by-segment
  [seg]
  (let [s (unspsc-segment seg)]
    (filterv #(= s (:product/unspsc-segment %)) catalog)))

(defn by-unspsc
  "Products whose commodity equals `code`, or (when `code` is a 2-digit segment)
  all twins in that segment."
  [code]
  (let [digits (some-> code str (str/replace #"[^0-9]" "") not-empty)]
    (cond
      (nil? digits) []
      (= 2 (count digits))
      (by-segment digits)
      :else
      (filterv #(= digits (:product/unspsc %)) catalog))))

(defn- by-alias
  "Find twin that lists `id` in `:product/aliases`."
  [id]
  (when (seq id)
    (first (filter (fn [t] (some #{id} (:product/aliases t))) catalog))))

(defn resolve-twin
  "Best-effort twin for a product map, id string, or commodity code.

  Match order:
  1. exact `:product/id`
  2. `:product/aliases` (seed/GTIN ids mapped onto catalog twins) → conf 1.0
  3. exact 8-digit commodity (`:product/unspsc`) → conf 0.8
  4. 2-digit segment (first twin in catalog for that segment) → conf 0.5

  Returns `{:twin … :match :id|:alias|:commodity|:segment :confidence …}`
  or nil when nothing matches."
  [product-or-id]
  (let [m (cond (map? product-or-id) product-or-id
                (string? product-or-id) {:product/id product-or-id}
                :else nil)
        id (some-> m :product/id str)
        code (or (some-> m :product/unspsc str not-empty)
                 (some-> m :unspsc str not-empty)
                 (when (and (string? product-or-id)
                            (re-matches #"\d{2,}" product-or-id))
                   product-or-id))
        seg (or (unspsc-segment code)
                (some-> m :product/unspsc-segment str))]
    (or (when-let [t (and id (by-id id))]
          {:twin t :match :id :confidence 1.0})
        (when-let [t (and id (by-alias id))]
          {:twin t :match :alias :confidence 1.0})
        (when-let [t (and code (>= (count (str/replace (str code) #"[^0-9]" "")) 8)
                          (first (by-unspsc code)))]
          {:twin t :match :commodity :confidence 0.8})
        (when-let [t (and seg (first (by-segment seg)))]
          {:twin t :match :segment :confidence 0.5}))))

(defn maturity-scorecard
  "Aggregate operator maturity dims into a 0–100 score.

  Inputs may be a graph-routing-coverage map and optional brand-owner coverage
  (0–1). Dimensions are equally weighted soft gates already used by doctor."
  ([uob] (maturity-scorecard uob nil))
  ([uob brand-owner-coverage]
   (let [dims (cond-> {:product-twin (double (:product-twin-coverage uob 0))
                       :high-confidence (double (:product-twin-high-confidence-coverage uob 0))
                       :mean-confidence (double (:product-twin-mean-confidence uob 0))
                       :twin-segment (double (:twin-segment-coverage uob 0))
                       :operator-ready (double (:operator-ready-coverage uob 0))
                       :registry-known (double (:registry-known-coverage uob 0))}
                (number? brand-owner-coverage)
                (assoc :brand-owner (double brand-owner-coverage)))
         n (count dims)
         score (if (pos? n)
                 (* 100.0 (/ (reduce + 0.0 (vals dims)) n))
                 0.0)
         grade (cond (>= score 95) :A
                     (>= score 85) :B
                     (>= score 70) :C
                     (>= score 50) :D
                     :else :F)]
     {:score score
      :grade grade
      :dims dims
      :product-twin-by-match (:product-twin-by-match uob)
      :mean-confidence (:product-twin-mean-confidence uob)})))

(defn catalog-summary
  "Operator-facing maturity of the twin catalog.

  `:curated-blueprint-coverage` is the fraction of open-business curated
  segments (10/27/39/43/73) that have at least one twin with SBOM+CAD+physics."
  []
  (let [segs (mapv :product/unspsc-segment catalog)
        seg-set (set segs)
        curated curated-blueprint-segments
        curated-hits (filterv seg-set curated)
        complete? (fn [p]
                    (and (seq (:product/sbom p))
                         (seq (:product/cad-features p))
                         (:product/physics p)))]
    {:products (count catalog)
     :segments (vec (sort seg-set))
     :with-sbom (count (filter #(seq (:product/sbom %)) catalog))
     :with-cad (count (filter #(seq (:product/cad-features %)) catalog))
     :with-physics (count (filter :product/physics catalog))
     :with-gltf-ref (count (filter :product/gltf-ref catalog))
     :with-full-twin (count (filter complete? catalog))
     :curated-blueprint-segments curated
     :curated-blueprint-hits curated-hits
     :curated-blueprint-missing (filterv (complement seg-set) curated)
     :curated-blueprint-coverage
     (if (seq curated)
       (double (/ (count curated-hits) (count curated)))
       0.0)
     :seed-traffic-segments seed-traffic-segments
     :seed-traffic-hits (filterv seg-set seed-traffic-segments)
     :seed-traffic-missing (filterv (complement seg-set) seed-traffic-segments)
     :seed-traffic-coverage
     (if (seq seed-traffic-segments)
       (double (/ (count (filter seg-set seed-traffic-segments))
                  (count seed-traffic-segments)))
       0.0)
     :commodities (into (sorted-set) (map :product/unspsc catalog))
     :ids (mapv :product/id catalog)}))

(defn segment-twin-stats
  "Stats for one open-business segment entry (product twin maturity)."
  [seg]
  (let [twins (by-segment seg)
        n (count twins)]
    {:product-twins n
     :twin-ids (mapv :product/id twins)
     :has-sbom-twins? (and (pos? n) (every? #(seq (:product/sbom %)) twins))
     :has-cad-twins? (and (pos? n) (every? #(seq (:product/cad-features %)) twins))
     :has-physics-twins? (and (pos? n) (every? :product/physics twins))
     :total-sbom-lines (reduce + 0 (map #(count (:product/sbom %)) twins))
     :total-cad-features (reduce + 0 (map #(count (:product/cad-features %)) twins))}))
