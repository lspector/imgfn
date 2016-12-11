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
  (:use clojush.ns clojure.math.numeric-tower clojush.experimental.decimation)
  (:require [clojure.math.combinatorics :as combo])
  (:require [clj-random.core :as random])
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
        ;target-distinctivenesses (mapv fdiff targets (repeat (mean targets)))
        ;result-distinctivenesses (mapv fdiff results (repeat (mean results)))
        ;distinctiveness-errors (mapv fdiff target-distinctivenesses result-distinctivenesses)
        ]
    value-errors
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
    ;(vec (concat value-errors distinctiveness-errors))
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

;; whatevs operator: make a small, probably insignificant change

;; can use standard operators, overriding args if we want

;; Q: should params be set to make have n% (50?) chance of changing 
;; genome of median size in pop???

(defn whatevs
  "Returns the individual."
  [ind {;; we will ignore/override genetic op params, and add :dad in the caller
         :keys [dad] 
         :as argmap}]
  ((lrand-nth [uniform-instruction-mutation
              uniform-integer-mutation
              uniform-float-mutation
              ;uniform-tag-mutation
              ;uniform-string-mutation
              uniform-boolean-mutation
              uniform-close-mutation
              uniform-silence-mutation
              uniform-deletion
              uniform-addition
              (partial alternation dad)
              (partial uniform-crossover dad)])
   ind
   argmap))

;; http://ieeexplore.ieee.org/abstract/document/4983114/?section=abstract
;;https://cs.adelaide.edu.au/~frank/papers/cec09.pdf
;;http://link.springer.com/chapter/10.1007%2F978-3-540-87700-4_12#page-1

(defn ramp-breed
  "Returns an individual bred from the given population using the given parameters."
  [agt ;necessary since breed is called using swap! or send, even though not used
   mom
   variation-steps
   location rand-gen population
   {:keys [genetic-operator-probabilities] :as argmap}]
  (random/with-rng 
    rand-gen
    (loop [steps variation-steps
           child mom]
      (if (< steps 1)
        child
        (recur (dec steps)
               (whatevs child argmap))))))

(defn remove-one
  "Returns sequence s without the first instance of item."
  [item s]
  (let [[without-item with-item] (split-with #(not (= item %)) s)]
    (concat without-item (rest with-item))))

(defn selection-sort ;; ignores locations!
  [population argmap]
  (loop [remaining population
         sorted []]
    (if (empty? remaining)
      sorted
      (let [selected (select remaining 0 argmap)] ;; that 0 is a location argument
        (recur (remove-one selected remaining)
               (conj sorted selected))))))

(defn ramp-produce-new-offspring
  [pop-agents child-agents rand-gens
   {:keys [decimation-ratio population-size decimation-tournament-size
           trivial-geography-radius use-single-thread ]
    :as argmap}]
  (let [pop (if (>= decimation-ratio 1)
              (vec (doall (map deref pop-agents)))
              (decimate (vec (doall (map deref pop-agents)))
                        (int (* decimation-ratio population-size))
                        decimation-tournament-size
                        trivial-geography-radius))
        ordered-pop (selection-sort pop argmap)
        mean-genome-length (mean (map count (map :genome ordered-pop)))]
    (dotimes [i population-size]
      ((if use-single-thread swap! send)
       (nth child-agents i)
       ramp-breed
       (nth ordered-pop i)
       (inc i) i (nth rand-gens i) pop 
       (assoc argmap 
         :dad (lrand-nth pop)
         ;:alternation-rate (/ 1.0 mean-genome-length)
         ;:alignment-deviation 10
         ;:uniform-mutation-rate (/ 1.0 mean-genome-length)
         ;:uniform-mutation-constant-tweak-rate 0.5
         ;:uniform-mutation-float-gaussian-standard-deviation 1.0
         ;:uniform-mutation-int-gaussian-standard-deviation 1
         ;:uniform-mutation-string-char-change-rate 0.1
         ;:uniform-mutation-tag-gaussian-standard-deviation 100
         ;:uniform-close-mutation-rate (/ 1.0 mean-genome-length)
         ;:close-increment-rate 0.2
         ;:uniform-deletion-rate (/ 1.0 mean-genome-length)
         ;:uniform-addition-rate (/ 1.0 mean-genome-length)
         ;:uniform-silence-mutation-rate (/ 1.0 mean-genome-length)
         )))
    (when-not use-single-thread (apply await child-agents)))) ;; SYNCHRONIZE

(defn ramp-pushgp ;; this is a cut/paste of pushgp but using ramp-produce-new-offspring, marked ;*** 
  "The top-level routine of pushgp."
  ([] (pushgp '()))
  ([args]
    (reset! timer-atom (System/currentTimeMillis))
    (load-push-argmap args)
    (random/with-rng (random/make-mersennetwister-rng (:random-seed @push-argmap))
      ;; set globals from parameters
      (reset-globals)
      (initial-report @push-argmap) ;; Print the inital report
      (print-params @push-argmap)
      (check-genetic-operator-probabilities-add-to-one @push-argmap)
      (timer @push-argmap :initialization)
      (println "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
      (println "\nGenerating initial population...") (flush)
      (let [pop-agents (make-pop-agents @push-argmap)
            child-agents (make-child-agents @push-argmap)
            {:keys [rand-gens random-seeds]} (make-rng @push-argmap)]
        ;(print "Random seeds: ")
        ;(doseq [seed random-seeds] (print " " seed))
        ;(println)
        ;; Main loop
        (loop [generation 0]
          (println "Processing generation:" generation) (flush)
          (population-translate-plush-to-push pop-agents @push-argmap)
          (timer @push-argmap :reproduction)
          (print "Computing errors... ") (flush)
          (compute-errors pop-agents rand-gens @push-argmap)
          (println "Done computing errors.") (flush)
          (timer @push-argmap :fitness)
          ;; calculate solution rates if necessary for historically-assessed hardness
          (calculate-hah-solution-rates pop-agents @push-argmap)
          ;; create global structure to support elite group lexicase selection
          (when (= (:parent-selection @push-argmap) :elitegroup-lexicase)
            (build-elitegroups pop-agents))
          ;; calculate implicit fitness sharing fitness for population
          (when (= (:total-error-method @push-argmap) :ifs)
            (calculate-implicit-fitness-sharing pop-agents @push-argmap))
          (timer @push-argmap :other)
          ;; report and check for success
          (let [[outcome best] (report-and-check-for-success (vec (doall (map deref pop-agents)))
                                                             generation @push-argmap)]
            (cond (= outcome :failure) (do (printf "\nFAILURE\n")
                                         (if (:return-simplified-on-failure @push-argmap)
                                           (auto-simplify best (:error-function @push-argmap) (:final-report-simplifications @push-argmap) true 500)
                                           (flush)))
                  (= outcome :continue) (do (timer @push-argmap :report)
                                          (println "\nProducing offspring...") (flush)
                                          (ramp-produce-new-offspring pop-agents child-agents rand-gens @push-argmap)
                                          (println "Installing next generation...") (flush)
                                          (install-next-generation pop-agents child-agents @push-argmap)
                                          (recur (inc generation)))
                  :else  (final-report generation best @push-argmap))))))))

(defn -main
  [& args]
  (let [buff (scale-buffer (to-java-image-buffer (floatimage->byteimage distilled-image))
                           64)]
    (spit-image buff "target-image.png"))
  ;(ramp-pushgp argmap)
  (pushgp argmap)
  (destroy-frame imgfn-frame))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;imgfn.core/-main</span>","value":"#'imgfn.core/-main"}
;; <=

;; @@
(def foo (make-individual :genome (random-plush-genome 10 [0])))
(def bar (make-individual :genome (random-plush-genome 10 [1])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;imgfn.core/bar</span>","value":"#'imgfn.core/bar"}
;; <=

;; @@
foo
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:genome</span>","value":":genome"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:instruction</span>","value":":instruction"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:instruction 0]"}],"value":"{:instruction 0}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:instruction</span>","value":":instruction"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:instruction 0]"}],"value":"{:instruction 0}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:instruction</span>","value":":instruction"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:instruction 0]"}],"value":"{:instruction 0}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:instruction</span>","value":":instruction"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:instruction 0]"}],"value":"{:instruction 0}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:instruction</span>","value":":instruction"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:instruction 0]"}],"value":"{:instruction 0}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:instruction</span>","value":":instruction"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:instruction 0]"}],"value":"{:instruction 0}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:instruction</span>","value":":instruction"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:instruction 0]"}],"value":"{:instruction 0}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:instruction</span>","value":":instruction"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:instruction 0]"}],"value":"{:instruction 0}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:instruction</span>","value":":instruction"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:instruction 0]"}],"value":"{:instruction 0}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:instruction</span>","value":":instruction"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:instruction 0]"}],"value":"{:instruction 0}"}],"value":"[{:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0}]"}],"value":"[:genome [{:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0}]]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:program</span>","value":":program"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:program nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:errors</span>","value":":errors"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:errors nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:total-error</span>","value":":total-error"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:total-error nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:normalized-error</span>","value":":normalized-error"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:normalized-error nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:weighted-error</span>","value":":weighted-error"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:weighted-error nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:meta-errors</span>","value":":meta-errors"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:meta-errors nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:history</span>","value":":history"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:history nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:ancestors</span>","value":":ancestors"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:ancestors nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:uuid</span>","value":":uuid"},{"type":"html","content":"<span class='clj-unkown'>#uuid &quot;2736f209-c76b-4b6e-a449-224b9e53e2ad&quot;</span>","value":"#uuid \"2736f209-c76b-4b6e-a449-224b9e53e2ad\""}],"value":"[:uuid #uuid \"2736f209-c76b-4b6e-a449-224b9e53e2ad\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:parent-uuids</span>","value":":parent-uuids"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:parent-uuids nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:genetic-operators</span>","value":":genetic-operators"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:genetic-operators nil]"}],"value":"#clojush.individual.individual{:genome [{:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0}], :program nil, :errors nil, :total-error nil, :normalized-error nil, :weighted-error nil, :meta-errors nil, :history nil, :ancestors nil, :uuid #uuid \"2736f209-c76b-4b6e-a449-224b9e53e2ad\", :parent-uuids nil, :genetic-operators nil}"}
;; <=

;; @@
(whatevs foo (assoc @push-argmap :dad bar))
;; @@
;; ->
;;; #function[clojush.pushgp.genetic-operators/uniform-deletion]
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:genome</span>","value":":genome"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:genome ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:program</span>","value":":program"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:program nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:errors</span>","value":":errors"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:errors nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:total-error</span>","value":":total-error"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:total-error nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:normalized-error</span>","value":":normalized-error"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:normalized-error nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:weighted-error</span>","value":":weighted-error"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:weighted-error nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:meta-errors</span>","value":":meta-errors"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:meta-errors nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:history</span>","value":":history"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:history nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:ancestors</span>","value":":ancestors"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:ancestors nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:uuid</span>","value":":uuid"},{"type":"html","content":"<span class='clj-unkown'>#uuid &quot;cf15062a-7527-4150-98f4-1f37b0ad5271&quot;</span>","value":"#uuid \"cf15062a-7527-4150-98f4-1f37b0ad5271\""}],"value":"[:uuid #uuid \"cf15062a-7527-4150-98f4-1f37b0ad5271\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:parent-uuids</span>","value":":parent-uuids"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:parent-uuids nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:genetic-operators</span>","value":":genetic-operators"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:genetic-operators nil]"}],"value":"#clojush.individual.individual{:genome (), :program nil, :errors nil, :total-error nil, :normalized-error nil, :weighted-error nil, :meta-errors nil, :history nil, :ancestors nil, :uuid #uuid \"cf15062a-7527-4150-98f4-1f37b0ad5271\", :parent-uuids nil, :genetic-operators nil}"}
;; <=

;; @@
(:uniform-deletion-rate @push-argmap)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>0.01</span>","value":"0.01"}
;; <=

;; @@
(uniform-deletion foo @push-argmap)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:genome</span>","value":":genome"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:genome ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:program</span>","value":":program"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:program nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:errors</span>","value":":errors"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:errors nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:total-error</span>","value":":total-error"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:total-error nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:normalized-error</span>","value":":normalized-error"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:normalized-error nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:weighted-error</span>","value":":weighted-error"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:weighted-error nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:meta-errors</span>","value":":meta-errors"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:meta-errors nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:history</span>","value":":history"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:history nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:ancestors</span>","value":":ancestors"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:ancestors nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:uuid</span>","value":":uuid"},{"type":"html","content":"<span class='clj-unkown'>#uuid &quot;78c2fed3-daf4-4470-9e76-61bc65baedc6&quot;</span>","value":"#uuid \"78c2fed3-daf4-4470-9e76-61bc65baedc6\""}],"value":"[:uuid #uuid \"78c2fed3-daf4-4470-9e76-61bc65baedc6\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:parent-uuids</span>","value":":parent-uuids"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:parent-uuids nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:genetic-operators</span>","value":":genetic-operators"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:genetic-operators nil]"}],"value":"#clojush.individual.individual{:genome (), :program nil, :errors nil, :total-error nil, :normalized-error nil, :weighted-error nil, :meta-errors nil, :history nil, :ancestors nil, :uuid #uuid \"78c2fed3-daf4-4470-9e76-61bc65baedc6\", :parent-uuids nil, :genetic-operators nil}"}
;; <=

;; @@
(println 
        (map #(if (< (lrand) 0.01) nil %) 
             [{:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0}]))
;; @@
;; ->
;;; ({:instruction 0} {:instruction 0} {:instruction 0} {:instruction 0})
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(lrand)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>0.4490940570071442</span>","value":"0.4490940570071442"}
;; <=

;; @@

;; @@
