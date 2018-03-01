(ns calculator.core
  (require [clojure.string :as str])
  (:gen-class))
;todo 
; handle overflow of numbers
; handle non existing file path

(def global-operation-dictionary {
                                  :ADD +
                                  :SUBTRACT - 
                                  :MULTIPLY *
                                  })


(defn get-snd-argument
  "returns the name of register from string"
  [string]
  (nth (str/split string #" ") 1))


(defn get-register-name
  "returns the name of register from string"
  [string]
  (nth (str/split string #" ") 0))


(defn is-print-op? [string]
  (and 
    (= (count (str/split string #" ")) 2)
    (= (nth (str/split string #" ") 0) "print")))


(defn parse-number
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (re-find #"^-?\d+\.?\d*$" s)
    (read-string s)))

(defn quit? 
  "returns true if there exists a quit string in list"
  [operations]
  (some? (some #(= "QUIT" (str/upper-case %)) operations)))


(defn apply-operation
  "takes a number and a tuple with operation string and number and evaluates"
  [number op-and-number]
  (let [ 
        operation-string (first op-and-number)
        operation ((keyword (str/upper-case operation-string)) global-operation-dictionary)

        operation-value (nth op-and-number 1)
        ]
    (operation number operation-value)))


(defn is-input-register?
  "True if input is register and false if not"
  [input]
  (= (parse-number input) nil))


(defn filter-on-valid-string
  "Returns a new list without the operations that cant be handled"
  [operations-strings]
  (let 
    [

     is-valid-op? (fn [operation-string] 
                    (and
                      (= (count (str/split operation-string #" ")) 3)
                      (contains? global-operation-dictionary 
                                 (keyword 
                                   (str/upper-case
                                    (get-snd-argument operation-string))))
                      ))
     ]
    (filter is-valid-op? operations-strings)))


(defn evaluate-register
  "evaluates the value of a register"
  [register operations]
  (let [

        ;take a string and if not an atom apply evaluate that register before
        get-value (fn [ register-string] 
                    (let [input (nth (str/split register-string #" ") 2)]
                      (if (is-input-register? input)
                        ;; --------- OBS Recursion---------
                        (evaluate-register input operations)
                        (parse-number input))))
      
        ;remove all values that is not accepted
        valid-operations (filter-on-valid-string operations)

        ;find the operation with same name as input argument
        corresponding-operations (filter 
                                   #(= register (get-register-name %)) 
                                   valid-operations)

        inputs (map get-value corresponding-operations)
        operations (map get-snd-argument corresponding-operations)

        ;this is a trick to get a tuple with operation and value to apply
        information   (map vector operations inputs)
    
        ;need initial value for reduce
        initial-value 0

        ]
    ;will do the +,- or * on list and accumulate
    (reduce apply-operation initial-value information )))



(defn -main
  "Calculator main"
  [& args]
  (println "Hello, type in your command")
  (let [
        operations (atom [])
        ]

  ; check if args is given and assume it's a filepath in that case
  (if args 

    ;input stream from file
    (do 
      (println "reading file: " (first args))
    (let [
        file-name (first args)
        text (str/split-lines (slurp file-name))
        ]
    (doseq [input-string text]
        (if (is-print-op? input-string)
          (println (evaluate-register 
                     (get-snd-argument input-string) 
                                      @operations))
          (reset! operations (concat @operations [input-string] ))))))

    ; manual input
    (while (not (quit? @operations))
      (let [
            input-string (read-line)
            ]
        (if (is-print-op? input-string)
          (println (evaluate-register 
                     (get-snd-argument input-string) 
                     @operations))
          (reset! operations (concat @operations [input-string] ))))))))


