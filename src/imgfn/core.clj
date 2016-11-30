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
                                 (push-item {:r 0.0 :g 0.0 :b 0.0} :auxiliary)
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

(defn imgfn-errors
  [program]
  (let [results-targets (for [x (range (count distilled-image)) 
                              y (range (count (first distilled-image)))]
                          [((fn [rgb-map]
                              (mapv #(mod % 1.0)
                                    [(:r rgb-map)(:g rgb-map)(:b rgb-map)]))
                            (program-result program x y))
                           (nth (nth distilled-image y) x)])
        differences (apply concat
                           (mapv (fn [[result-rgb target-rgb]]
                                   (mapv fdiff result-rgb target-rgb))
                                 results-targets))]
    (vec (concat differences
                 [(fdiff (stdev (apply concat (map first results-targets)))
                         (stdev (apply concat (map second results-targets))))]
                 ))))
                 

(when-not (contains? @instruction-table 'r)
  (define-registered
    r
    ^{:stack-types [:float]}
    (fn [state]
      (push-item (assoc (first (:auxiliary state))
                        :r
                        (if (empty? (:float state))
                          0.0
                          (first (:float state))))
                 :auxiliary
                 (pop-item :float (pop-item :auxiliary state))))))

(when-not (contains? @instruction-table 'g)
  (define-registered
    g
    ^{:stack-types [:float]}
    (fn [state]
      (push-item (assoc (first (:auxiliary state))
                        :g
                        (if (empty? (:float state))
                          0.0
                          (first (:float state))))
                 :auxiliary
                 (pop-item :float (pop-item :auxiliary state))))))

(when-not (contains? @instruction-table 'b)
  (define-registered
    b
    ^{:stack-types [:float]}
    (fn [state]
      (push-item (assoc (first (:auxiliary state))
                        :b
                        (if (empty? (:float state))
                          0.0
                          (first (:float state))))
                 :auxiliary
                 (pop-item :float (pop-item :auxiliary state))))))

(defn error-deviation
  [i argmap]
  (stdev (:errors i)))

(def argmap
  {:error-function imgfn-errors
   :population-size 1000
   :max-points 1600
   :max-genome-size-in-initial-program 400
   :evalpush-limit 	1600
   :alternation-rate 0.01
   :atom-generators (let [instructions (registered-for-stacks [:float :boolean :exec])]
                      (apply concat (mapv #(vector %1 %2)
                                          instructions
                                          (cycle [(fn [] (- 1.0 (lrand 2.0)))
                                                  'in1
                                                  'in2
                                                  'r
                                                  'g
                                                  'b]))))
   :parent-selection :epsilon-lexicase
   :use-single-thread false
   :problem-specific-report imgfn-report
   :total-error-method :rmse ;; should just affect what gets reported & sent to screen
   ;:meta-error-categories [error-deviation]
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