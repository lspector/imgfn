;; gorilla-repl.fileformat = 1

;; @@
;; Lee Spector (lspector@hampshire.edu), December 2016

;; Programmatic image compression with genetic programming.

;; This is experimental code, still under development! The current version
;; might reflect all sorts of temporary experiments, and all of the comments 
;; might be wrong. Don't count on anything!

;; Problem: Given an image, find a concise function that takes x, y coordinates
;; (scaled 0-1) and returns values of red, green, and blue (scaled 0-1) for the 
;; indexed pixel in a given image.

;; This is "programmatic compression" as described here: 
;; http://dces.essex.ac.uk/staff/rpoli/gp-field-guide/1211Compression.html

;; Display and save "best" (by some measure) images during evolution.

;; Initially, use only super-low-rez versions images (maybe 16x16 or even 4x4).

;; Possibly display/save at a higher resolution than is used for error testing.

;; Possibly use only a small subset of pixels for fitness cases, 
;; changing them incrementally over generations. 

;; Possibly use a size-based meta-error to bias selection in favor of smaller 
;; programs.

;; Possibly use "call-controllable soft assignment" instructions for red,
;; green, and blue outputs (see paper on soft assignment by McPhee, Langdon, ...?)
;; (Call-controllability, via a second argument, may be new with this work (?).)

(ns imgfn.core
  (:use clojush.ns clojure.math.numeric-tower)
  (:require [clojure.math.combinatorics :as combo])
  (import [java.awt Color image.BufferedImage]
          [javax.imageio.ImageIO]
          [javax.swing JFrame ImageIcon JLabel])
  (:gen-class))

(use-clojush)

;;; image display/saving/etc code by Julian Oks

(defn to-java-image-buffer
  "Clojure image can be of any size, but must have 3 rgb color channels"
  [clojure-image]
  (let [height (count clojure-image)
        width (count (first clojure-image))
        buffer (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        colors (map (fn [[r g b]] (.getRGB (Color. r g b)))
                    (apply concat clojure-image))]
    (.setDataElements (.getRaster buffer) 0 0 width height (int-array colors))
    buffer))

(defn scale-buffer
  "Returns a new buffer that is the given buffer scaled by a given value."
  [buffer scaling-factor]
  (let [new-width (int (* scaling-factor (.getWidth buffer)))
        new-height (int (* scaling-factor (.getHeight buffer)))
        new-buffer (BufferedImage. new-width new-height BufferedImage/TYPE_INT_RGB)
        g (.createGraphics new-buffer)]
    (.drawImage g buffer 0 0 new-width new-height nil)
    (.dispose g)
    new-buffer))

(defn spit-image
  "Spits an image to a given filename"
  [image-buffer filename]
  (javax.imageio.ImageIO/write image-buffer "png" (java.io.File. filename)))

(defn slurp-into-buffer
  "Slurps an image into a buffer"
  [filename]
  (try (javax.imageio.ImageIO/read (java.io.File. filename))
    (catch Exception e (str "Couldn't open image: " filename))))

(defn slurp-image
  "Slurps an image.  Returns that image as a vector of its rows, each containing pixels [r g b]"
  [filename]
  (let [buffer (slurp-into-buffer filename)
        raster (.getRaster buffer)
        width (.getWidth buffer)
        raw (map #(mod % 256) (.getDataElements raster 0 0 width (.getHeight buffer) nil))]
    (mapv vec (partition width (mapv vec (partition 3 raw))))))

(defn make-frame
  []
  (let [frame (JFrame.)]
    (.setAlwaysOnTop frame true)
    (.setFocusableWindowState frame false)
    frame))

(defn update-frame
  ([frame buffer] (update-frame frame buffer ""))
  ([frame buffer title-text]
   (.setSize frame (.getWidth buffer) (.getHeight buffer))
   (.setContentPane frame (JLabel. (ImageIcon. buffer)))
   (.setTitle frame (str title-text))
   (.setVisible frame true)))

(defn destroy-frame
  [frame]
  (.dispose frame))

;;; end of Julian's code

(def imgfn-frame (make-frame))

(def imgfn-image (slurp-image "rebecca-barcelona-elevator-512.png"))

(defn byte->float01
  [b]
  (mod (/ b 255.0) 1.0))

(defn float01->byte
  [f]
  (mod (int (* f 255.0)) 256))

(defn byteimage->floatimage
  [i]
  (mapv (fn [row] 
          (mapv (fn [col] 
                  (mapv byte->float01 col))
                row))
        i))

(defn floatimage->byteimage
  [i]
  (mapv (fn [row] 
          (mapv (fn [col] 
                  (mapv float01->byte col))
                row))
        i))

(defn shrink
  [i]
  (let [thin (fn [v] (vec (apply concat (partition 1 2 v))))]
    (mapv thin (thin i))))

(defn grow
  [i]
  (let [fatten (fn [v] (apply concat (mapv #(vector % %) v)))]
    (mapv fatten (fatten i))))

(def distilled-image
  (->> imgfn-image
       (shrink)
       (shrink)
       (shrink)
       (shrink)
       (shrink)
       (shrink)
       (shrink)
       (byteimage->floatimage)))

(def flattened-distilled-image (flatten distilled-image))

(def distilled-image-height (count distilled-image))
(def distilled-image-width (count (first distilled-image)))

(defn scale-x [int-x]
  (mod (/ (float int-x) distilled-image-width) 1.0))

(defn scale-y [int-y]
  (mod (/ (float int-y) distilled-image-height) 1.0))

(defn program-result
  [program x y]
  (first (:auxiliary (run-push program
                               (->> (make-push-state)
                                 (push-item {:r 0.5 :g 0.5 :b 0.5} :auxiliary)
                                 (push-item (scale-y y) :input)
                                 (push-item (scale-x x) :input))))))

(defn imgfn-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (println ";;******************************")
  (printf ";; -*- imgfn  report - generation %s\n" generation)(flush)
  (let [interpolation 1 ;; 1 is none, 2 does double, 4 quadruple, etc.
        result-image (vec (for [y (map #(/ % interpolation) 
                                       (range (* interpolation distilled-image-height)))]
                            (vec (for [x (map #(/ % interpolation) 
                                              (range (* interpolation distilled-image-width)))]
                                   (let [result (program-result (:program best) x y)]
                                     [(mod (Math/abs (:r result)) 1.0)
                                      (mod (Math/abs (:g result)) 1.0)
                                      (mod (Math/abs (:b result)) 1.0)])))))
        buff (scale-buffer (to-java-image-buffer (floatimage->byteimage result-image))
                           128)]
    (update-frame imgfn-frame buff)
    (spit-image buff (str "pics/" generation ".png"))
    nil ;; I think this means that the normal best will be reported (total error by default)
    ))

(defn fdiff [float1 float2] (Math/abs (- float1 float2)))

(defn stdev [nums]
  (let [mean (mean nums)]
    (Math/sqrt (/ (apply +' (map #(* (- % mean) (- % mean))
                                 nums))
                  (dec (count nums))))))

(defn covariance [s1 s2] ;; assumes s1 and s2 same length
  (let [s1-mean (mean s1)
        s2-mean (mean s2)]
    (float (/ (reduce + (map * 
                             (map - s1 (repeat s1-mean))
                             (map - s2 (repeat s2-mean))))
              (dec (count s1))))))

; http://ci.columbia.edu/ci/premba_test/c0331/s7/s7_5.html

(defn correlation [s1 s2]
  (let [numerator (covariance s1 s2)]
    (if (zero? numerator)
      0.0
      (/ numerator
         (* (stdev s1) (stdev s2))))))

(defn third [coll] (nth coll 2))

(defn transpose
  [linearized-matrix row-length]
  (vec (apply concat (apply map list (partition row-length linearized-matrix)))))

(defn sq [n] (* n n))

(defn dist 
  [[r1 g1 b1][r2 g2 b2]]
  (Math/sqrt (+ (sq (- r1 r2))
                (sq (- g1 g2))
                (sq (- b1 b2)))))

(defn rotv [v] (vec (concat (rest v) [(first v)])))

(defn blind-combinations
  [s]
  (let [index-pairs (combo/combinations (range (count s)) 2)]
    (map (fn [[i1 i2]]
           (vector (nth s i1) (nth s i2)))
         index-pairs)))

;; INCLUDE both value error and distinctiveness error for each xy pair

(defn imgfn-errors
  [program]
  (let [x-range (range (count distilled-image))
        y-range (range (count (first distilled-image)))
        targets flattened-distilled-image
        results (mapv #(mod % 1.0)
                      (flatten
                        (for [y y-range x x-range] ;; this ordering will match the flattened targets
                          (let [result (program-result program x y)]
                            [(:r result) (:g result) (:b result)]))))
        value-errors (mapv fdiff targets results)
        ;target-distinctivenesses (mapv fdiff targets (repeat (mean targets)))
        ;target-distinctivenesses (mapv fdiff targets (rotv targets))
        ;result-distinctivenesses (mapv fdiff results (repeat (mean results)))
        ;result-distinctivenesses (mapv fdiff results (rotv results))
        ;distinctiveness-errors (mapv fdiff target-distinctivenesses result-distinctivenesses)
        ;target-result-correlation (correlation targets results)
        ;distinctiveness-errors (mapv * target-distinctivenesses result-distinctivenesses)
        ;target-distinctivenesses (mapv fdiff targets (repeat (mean targets)))
        ;result-distinctivenesses (mapv fdiff results (repeat (mean results)))
        ;distinctiveness-errors (mapv - target-distinctivenesses result-distinctivenesses)
        target-distinctivenesses (mapv fdiff targets (repeat (mean targets)))
        result-distinctivenesses (mapv fdiff results (repeat (mean results)))
        distinctiveness-errors (mapv fdiff target-distinctivenesses result-distinctivenesses)
        ]
    ;value-errors
    #_(mapv (fn [ve de] (+ ve (* ve de)))
          value-errors 
          distinctiveness-errors)
    ;(vec (concat value-errors [(/ (- 1 target-result-correlation) 2)]))
    ;(vec (concat value-errors
    ;             (for [[[r1 t1][r2 t2]] (blind-combinations (map vector results targets))]
    ;               (if (if (= t1 t2) (= r1 r2) (not= r1 r2))
    ;                 0.0
    ;                 1.0))))
    ;(vec (concat value-errors
    ;             [(mean (for [[[r1 t1][r2 t2]] (blind-combinations (map vector results targets))]
    ;                      (if (if (= t1 t2) (= r1 r2) (not= r1 r2))
    ;                        0.0
    ;                        1.0)))]))
    (vec (concat value-errors distinctiveness-errors))
    ))

;; call-controllable soft assignment

;(when-not (contains? @instruction-table 'r)
;  (define-registered
;    r
;    ^{:stack-types [:float]}
;    (fn [state]
;      (if (empty? (rest (rest (:float state))))
;        state
;        (push-item (assoc (first (:auxiliary state))
;                     :r
;                     (let [new-value (mod (Math/abs (first (:float state))) 1.0)
;                           old-value (:r (first (:auxiliary state)))
;                           intensity (mod (Math/abs (second (:float state))) 1.0)]
;                       (+ (* new-value intensity)
;                          (* old-value (- 1.0 intensity)))))
;                   :auxiliary
;                   (pop-item :float (pop-item :float (pop-item :auxiliary state))))))))

;(when-not (contains? @instruction-table 'g)
;  (define-registered
;    g
;    ^{:stack-types [:float]}
;    (fn [state]
;      (if (empty? (rest (rest (:float state))))
;        state
;        (push-item (assoc (first (:auxiliary state))
;                     :g
;                     (let [new-value (mod (Math/abs (first (:float state))) 1.0)
;                           old-value (:g (first (:auxiliary state)))
;                           intensity (mod (Math/abs (second (:float state))) 1.0)]
;                       (+ (* new-value intensity)
;                          (* old-value (- 1.0 intensity)))))
;                   :auxiliary
;                   (pop-item :float (pop-item :float (pop-item :auxiliary state))))))))

;(when-not (contains? @instruction-table 'b)
;  (define-registered
;    b
;    ^{:stack-types [:float]}
;    (fn [state]
;      (if (empty? (rest (rest (:float state))))
;        state
;        (push-item (assoc (first (:auxiliary state))
;                     :b
;                     (let [new-value (mod (Math/abs (first (:float state))) 1.0)
;                           old-value (:b (first (:auxiliary state)))
;                           intensity (mod (Math/abs (second (:float state))) 1.0)]
;                       (+ (* new-value intensity)
;                          (* old-value (- 1.0 intensity)))))
;                   :auxiliary
;                   (pop-item :float (pop-item :float (pop-item :auxiliary state))))))))

;; hard assignment
(when-not (contains? @instruction-table 'r)
  (define-registered
    r
    ^{:stack-types [:float]}
    (fn [state]
      (if (empty? (rest (:float state)))
        state
        (push-item (assoc (first (:auxiliary state))
                     :r
                     (mod (Math/abs (first (:float state))) 1.0))
                   :auxiliary
                   (pop-item :float (pop-item :auxiliary state)))))))

(when-not (contains? @instruction-table 'g)
  (define-registered
    g
    ^{:stack-types [:float]}
    (fn [state]
      (if (empty? (rest (:float state)))
        state
        (push-item (assoc (first (:auxiliary state))
                     :g
                     (mod (Math/abs (first (:float state))) 1.0))
                   :auxiliary
                   (pop-item :float (pop-item :auxiliary state)))))))

(when-not (contains? @instruction-table 'b)
  (define-registered
    b
    ^{:stack-types [:float]}
    (fn [state]
      (if (empty? (rest (:float state)))
        state
        (push-item (assoc (first (:auxiliary state))
                     :b
                     (mod (Math/abs (first (:float state))) 1.0))
                   :auxiliary
                   (pop-item :float (pop-item :auxiliary state)))))))

(defn error-deviation
  [i argmap]
  (stdev (:errors i)))

(def argmap
  {:error-function imgfn-errors
   :population-size 1000
   :max-generations 10000
   :max-points 1000
   :max-genome-size-in-initial-program 100
   :evalpush-limit 	1000
   :atom-generators (let [instructions (registered-for-stacks [:integer :boolean :exec :float])]
                      (vec (apply concat (mapv #(vector %1 %2)
                                               instructions
                                               (cycle [(fn [] (- 100 (lrand-int 201)))
                                                       (fn [] (- 1.0 (lrand 2.0)))
                                                       'in1
                                                       'in2
                                                       'r
                                                       'g
                                                       'b])))))
   :parent-selection :epsilon-lexicase
   :epigenetic-markers [:close :silent]
   :use-single-thread false
   :problem-specific-report imgfn-report
   ;:meta-error-categories [error-deviation]
   :genetic-operator-probabilities {:reproduction 0.0
                                    :alternation 0.0
                                    :uniform-mutation 0.0
                                    :uniform-instruction-mutation 0.0
                                    :uniform-integer-mutation 0.0
                                    :uniform-float-mutation 0.0
                                    :uniform-tag-mutation 0.0
                                    :uniform-string-mutation 0.0
                                    :uniform-boolean-mutation 0.0
                                    ; Similar to the old ULTRA operator:
                                    [:alternation :uniform-mutation] 0.5
                                    :uniform-close-mutation 0.1
                                    :uniform-silence-mutation 0.1
                                    :uniform-crossover 0.1
                                    :two-point-crossover 0.0
                                    ; A hill-climbing version of uniform-silence-mutation:
                                    [:make-next-operator-revertable :uniform-silence-mutation] 0.0
                                    :autoconstruction 0.0
                                    :uniform-deletion 0.1
                                    :uniform-addition 0.1
                                    ;; CUSTOM
                                    ;[:alternation :uniform-mutation :uniform-close-mutation :uniform-silence-mutation] 0.45
                                    ;[:uniform-mutation :uniform-close-mutation :uniform-silence-mutation] 0.45
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :uniform-mutation-constant-tweak-rate 0.5
   :uniform-mutation-float-gaussian-standard-deviation 0.01 ;1.0
   :uniform-mutation-int-gaussian-standard-deviation 1
   :uniform-mutation-string-char-change-rate 0.1
   :uniform-mutation-tag-gaussian-standard-deviation 100
   :uniform-close-mutation-rate 0.1
   :close-increment-rate 0.5 ;0.2
   :uniform-deletion-rate 0.01
   :uniform-addition-rate 0.01
   :uniform-silence-mutation-rate 0.1
   })


(defn -main
  [& args]
  (let [buff (scale-buffer (to-java-image-buffer (floatimage->byteimage distilled-image))
                           64)]
    (spit-image buff "target-image.png"))
  (pushgp argmap)
  (destroy-frame imgfn-frame))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;imgfn.core/-main</span>","value":"#'imgfn.core/-main"}
;; <=

;; @@
flattened-distilled-image
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.4588235294117647</span>","value":"0.4588235294117647"},{"type":"html","content":"<span class='clj-double'>0.1607843137254902</span>","value":"0.1607843137254902"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"},{"type":"html","content":"<span class='clj-double'>0.4470588235294118</span>","value":"0.4470588235294118"},{"type":"html","content":"<span class='clj-double'>0.15294117647058825</span>","value":"0.15294117647058825"},{"type":"html","content":"<span class='clj-double'>0.17254901960784313</span>","value":"0.17254901960784313"},{"type":"html","content":"<span class='clj-double'>0.49019607843137253</span>","value":"0.49019607843137253"},{"type":"html","content":"<span class='clj-double'>0.25098039215686274</span>","value":"0.25098039215686274"},{"type":"html","content":"<span class='clj-double'>0.2784313725490196</span>","value":"0.2784313725490196"},{"type":"html","content":"<span class='clj-double'>0.23137254901960785</span>","value":"0.23137254901960785"},{"type":"html","content":"<span class='clj-double'>0.0784313725490196</span>","value":"0.0784313725490196"},{"type":"html","content":"<span class='clj-double'>0.09019607843137255</span>","value":"0.09019607843137255"},{"type":"html","content":"<span class='clj-double'>0.5098039215686274</span>","value":"0.5098039215686274"},{"type":"html","content":"<span class='clj-double'>0.24705882352941178</span>","value":"0.24705882352941178"},{"type":"html","content":"<span class='clj-double'>0.27450980392156865</span>","value":"0.27450980392156865"},{"type":"html","content":"<span class='clj-double'>0.40784313725490196</span>","value":"0.40784313725490196"},{"type":"html","content":"<span class='clj-double'>0.10588235294117647</span>","value":"0.10588235294117647"},{"type":"html","content":"<span class='clj-double'>0.12941176470588237</span>","value":"0.12941176470588237"},{"type":"html","content":"<span class='clj-double'>0.9411764705882353</span>","value":"0.9411764705882353"},{"type":"html","content":"<span class='clj-double'>0.7764705882352941</span>","value":"0.7764705882352941"},{"type":"html","content":"<span class='clj-double'>0.7803921568627451</span>","value":"0.7803921568627451"},{"type":"html","content":"<span class='clj-double'>0.1843137254901961</span>","value":"0.1843137254901961"},{"type":"html","content":"<span class='clj-double'>0.11764705882352941</span>","value":"0.11764705882352941"},{"type":"html","content":"<span class='clj-double'>0.0784313725490196</span>","value":"0.0784313725490196"},{"type":"html","content":"<span class='clj-double'>0.17647058823529413</span>","value":"0.17647058823529413"},{"type":"html","content":"<span class='clj-double'>0.047058823529411764</span>","value":"0.047058823529411764"},{"type":"html","content":"<span class='clj-double'>0.07450980392156863</span>","value":"0.07450980392156863"},{"type":"html","content":"<span class='clj-double'>0.42745098039215684</span>","value":"0.42745098039215684"},{"type":"html","content":"<span class='clj-double'>0.1607843137254902</span>","value":"0.1607843137254902"},{"type":"html","content":"<span class='clj-double'>0.1568627450980392</span>","value":"0.1568627450980392"},{"type":"html","content":"<span class='clj-double'>0.9333333333333333</span>","value":"0.9333333333333333"},{"type":"html","content":"<span class='clj-double'>0.8705882352941177</span>","value":"0.8705882352941177"},{"type":"html","content":"<span class='clj-double'>0.7803921568627451</span>","value":"0.7803921568627451"},{"type":"html","content":"<span class='clj-double'>0.3176470588235294</span>","value":"0.3176470588235294"},{"type":"html","content":"<span class='clj-double'>0.21568627450980393</span>","value":"0.21568627450980393"},{"type":"html","content":"<span class='clj-double'>0.11764705882352941</span>","value":"0.11764705882352941"},{"type":"html","content":"<span class='clj-double'>0.06666666666666667</span>","value":"0.06666666666666667"},{"type":"html","content":"<span class='clj-double'>0.023529411764705882</span>","value":"0.023529411764705882"},{"type":"html","content":"<span class='clj-double'>0.0392156862745098</span>","value":"0.0392156862745098"},{"type":"html","content":"<span class='clj-double'>0.5725490196078431</span>","value":"0.5725490196078431"},{"type":"html","content":"<span class='clj-double'>0.4196078431372549</span>","value":"0.4196078431372549"},{"type":"html","content":"<span class='clj-double'>0.35294117647058826</span>","value":"0.35294117647058826"},{"type":"html","content":"<span class='clj-double'>0.3764705882352941</span>","value":"0.3764705882352941"},{"type":"html","content":"<span class='clj-double'>0.43137254901960786</span>","value":"0.43137254901960786"},{"type":"html","content":"<span class='clj-double'>0.6078431372549019</span>","value":"0.6078431372549019"},{"type":"html","content":"<span class='clj-double'>0.8549019607843137</span>","value":"0.8549019607843137"},{"type":"html","content":"<span class='clj-double'>0.7529411764705882</span>","value":"0.7529411764705882"},{"type":"html","content":"<span class='clj-double'>0.615686274509804</span>","value":"0.615686274509804"}],"value":"(0.4588235294117647 0.1607843137254902 0.2 0.4470588235294118 0.15294117647058825 0.17254901960784313 0.49019607843137253 0.25098039215686274 0.2784313725490196 0.23137254901960785 0.0784313725490196 0.09019607843137255 0.5098039215686274 0.24705882352941178 0.27450980392156865 0.40784313725490196 0.10588235294117647 0.12941176470588237 0.9411764705882353 0.7764705882352941 0.7803921568627451 0.1843137254901961 0.11764705882352941 0.0784313725490196 0.17647058823529413 0.047058823529411764 0.07450980392156863 0.42745098039215684 0.1607843137254902 0.1568627450980392 0.9333333333333333 0.8705882352941177 0.7803921568627451 0.3176470588235294 0.21568627450980393 0.11764705882352941 0.06666666666666667 0.023529411764705882 0.0392156862745098 0.5725490196078431 0.4196078431372549 0.35294117647058826 0.3764705882352941 0.43137254901960786 0.6078431372549019 0.8549019607843137 0.7529411764705882 0.615686274509804)"}
;; <=

;; @@
(apply distinct? flattened-distilled-image)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}
;; <=

;; @@
(count flattened-distilled-image)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>48</span>","value":"48"}
;; <=

;; @@
(count (distinct flattened-distilled-image))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>44</span>","value":"44"}
;; <=

;; @@
(count (blind-combinations (range (* 16 3))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>1128</span>","value":"1128"}
;; <=

;; @@

;; @@
