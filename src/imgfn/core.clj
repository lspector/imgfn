;; gorilla-repl.fileformat = 1

;; @@
;; Lee Spector (lspector@hampshire.edu), December 2016

;; Programmatic image compression with genetic programming.

;; This is experimental code, still under development!

;; Problem: Given an image, find a concise function that takes x, y coordinates
;; (scaled 0-1) and returns values of red, green, and blue (scaled 0-1) for the 
;; indexed pixel in a given image.

;; This is "programmatic compression" as described here: 
;; http://dces.essex.ac.uk/staff/rpoli/gp-field-guide/1211Compression.html

;; Display and save "best" (by some measure) images during evolution.

;; Initially, use only super-low-rez versions images (maybe 16x16 or so).

;; Possibly display/save at a higher resolution than is used for error testing.

;; Possibly use only a small subset of pixels for fitness cases, 
;; changing them incrementally over generations. 

;; Possibly use a size-based meta-error to bias selection in favor of smaller 
;; programs.

(ns imgfn.core
  (:use clojush.ns clojure.math.numeric-tower)
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
                           64)]
    (update-frame imgfn-frame buff)
    (spit-image buff (str "pics/" generation))
    nil ;; I think this means that the normal best will be reported (total error by default)
    ))

(defn fdiff [float1 float2] (Math/abs (- float1 float2)))

(defn stdev [nums]
  (let [mean (mean nums)]
    (Math/sqrt (/ (apply +' (map #(* (- % mean) (- % mean))
                                 nums))
                  (dec (count nums))))))

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

;; INCLUDE both value error and distinctiveness error for each xy pair

(defn imgfn-errors
  [program]
  (let [x-range (range (count distilled-image))
        y-range (range (count (first distilled-image)))
        targets flattened-distilled-image
        results (flatten
                  (for [y y-range x x-range] ;; this ordering will match the flattened targets
                    (let [result (program-result program x y)]
                      [(:r result) (:g result) (:b result)])))
        value-errors (mapv fdiff targets results)
        target-distinctivenesses (mapv fdiff targets (repeat (mean targets)))
        result-distinctivenesses (mapv fdiff results (repeat (mean results)))
        distinctiveness-errors (mapv fdiff target-distinctivenesses result-distinctivenesses)]
    (vec (concat value-errors distinctiveness-errors))))

(when-not (contains? @instruction-table 'r)
  (define-registered
    r
    ^{:stack-types [:float]}
    (fn [state]
      (if (empty? (rest (rest (:float state))))
        state
        (push-item (assoc (first (:auxiliary state))
                     :r
                     (let [new-value (mod (Math/abs (first (:float state))) 1.0)
                           old-value (:r (first (:auxiliary state)))
                           intensity (mod (Math/abs (second (:float state))) 1.0)]
                       (+ (* new-value intensity)
                          (* old-value (- 1.0 intensity)))))
                   :auxiliary
                   (pop-item :float (pop-item :float (pop-item :auxiliary state))))))))

(when-not (contains? @instruction-table 'g)
  (define-registered
    g
    ^{:stack-types [:float]}
    (fn [state]
      (if (empty? (rest (rest (:float state))))
        state
        (push-item (assoc (first (:auxiliary state))
                     :g
                     (let [new-value (mod (Math/abs (first (:float state))) 1.0)
                           old-value (:g (first (:auxiliary state)))
                           intensity (mod (Math/abs (second (:float state))) 1.0)]
                       (+ (* new-value intensity)
                          (* old-value (- 1.0 intensity)))))
                   :auxiliary
                   (pop-item :float (pop-item :float (pop-item :auxiliary state))))))))

(when-not (contains? @instruction-table 'b)
  (define-registered
    b
    ^{:stack-types [:float]}
    (fn [state]
      (if (empty? (rest (rest (:float state))))
        state
        (push-item (assoc (first (:auxiliary state))
                     :b
                     (let [new-value (mod (Math/abs (first (:float state))) 1.0)
                           old-value (:b (first (:auxiliary state)))
                           intensity (mod (Math/abs (second (:float state))) 1.0)]
                       (+ (* new-value intensity)
                          (* old-value (- 1.0 intensity)))))
                   :auxiliary
                   (pop-item :float (pop-item :float (pop-item :auxiliary state))))))))

(defn error-deviation
  [i argmap]
  (stdev (:errors i)))

(def argmap
  {:error-function imgfn-errors
   :population-size 100
   :max-points 1000
   :max-genome-size-in-initial-program 100
   :evalpush-limit 	500
   :alternation-rate 0.01
   :atom-generators (let [instructions (registered-for-stacks [:integer :float :boolean :exec])]
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
                                    :alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-instruction-mutation 0.0
                                    :uniform-integer-mutation 0.0
                                    :uniform-float-mutation 0.0
                                    :uniform-tag-mutation 0.0
                                    :uniform-string-mutation 0.0
                                    :uniform-boolean-mutation 0.0
                                    ; Similar to the old ULTRA operator:
                                    [:alternation :uniform-mutation] 0.0
                                    :uniform-close-mutation 0.2
                                    :uniform-silence-mutation 0.2
                                    :uniform-crossover 0.2
                                    :two-point-crossover 0.0
                                    ; A hill-climbing version of uniform-silence-mutation:
                                    [:make-next-operator-revertable :uniform-silence-mutation] 0.0
                                    :autoconstruction 0.0
                                    :uniform-deletion 0.0
                                    :uniform-addition 0.0
                                    ;; CUSTOM
                                    ;[:alternation :uniform-mutation :uniform-close-mutation :uniform-silence-mutation] 0.45
                                    ;[:uniform-mutation :uniform-close-mutation :uniform-silence-mutation] 0.45
                                    }
   })


(defn -main
  [& args]
  (pushgp argmap)
  (destroy-frame imgfn-frame))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;imgfn.core/-main</span>","value":"#'imgfn.core/-main"}
;; <=

;; @@

;; @@
